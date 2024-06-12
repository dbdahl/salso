roxido_registration!();
use roxido::*;

use num_traits::cast::ToPrimitive;
use rand::prelude::SliceRandom;
use rand::SeedableRng;
use rand_pcg::Pcg64Mcg;
use rayon::prelude::*;
use std::convert::TryFrom;

#[roxido]
fn bell(n_items: usize) {
    match dahl_bellnumber::bell(n_items).to_f64() {
        Some(x) => x,
        None => f64::INFINITY,
    }
}

#[roxido]
fn lbell(n_items: usize) {
    dahl_bellnumber::lbell(n_items)
}

#[roxido]
fn enumerate_partitions(n_items: usize) {
    let bell_number = dahl_bellnumber::bell(n_items);
    let n_partitions = usize::try_from(bell_number).unwrap();
    let partitions = RMatrix::<i32>::new(n_partitions, n_items, pc);
    let mut phv = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions.slice_mut(),
        n_partitions,
        n_items,
        true,
    );
    for mut p in dahl_partition::Partition::iter(n_items) {
        p.iter_mut().for_each(|x| *x += 1);
        phv.push_slice(&p[..]);
    }
    partitions
}

#[roxido]
fn expected_loss(
    partitions: &mut RMatrix,
    draws: &mut RMatrix,
    psm: &mut RObject,
    loss: i32,
    a: f64,
) {
    let n_partitions = partitions.nrow();
    let n_items = partitions.ncol();
    let partitions = partitions.to_i32_mut(pc);
    let draws = draws.to_i32_mut(pc);
    let psm2: &mut RMatrix<f64>;
    let psm_slice = if !psm.is_null() {
        psm2 = psm.as_matrix_mut().stop().to_f64_mut(pc);
        psm2.slice_mut()
    } else {
        &mut []
    };
    let results = RVector::<f64>::new(n_partitions, pc);
    let partitions = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions.slice_mut(),
        n_partitions,
        n_items,
        true,
    );
    let loss_function = dahl_salso::LossFunction::from_code(loss, a);
    match loss_function {
        Some(dahl_salso::LossFunction::BinderDraws(a)) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice_mut(),
                n_draws,
                n_items,
                true,
            );
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::BinderCMLossComputer::new(a)),
                &partitions,
                &draws,
                results.slice_mut(),
            )
        }
        Some(dahl_salso::LossFunction::BinderPSM) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::binder_multiple(&partitions, &psm, results.slice_mut())
        }
        Some(dahl_salso::LossFunction::OneMinusARI) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice_mut(),
                n_draws,
                n_items,
                true,
            );
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::OMARICMLossComputer::new(n_draws)),
                &partitions,
                &draws,
                results.slice_mut(),
            )
        }
        Some(dahl_salso::LossFunction::OneMinusARIapprox) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::omariapprox_multiple(&partitions, &psm, results.slice_mut())
        }
        Some(dahl_salso::LossFunction::VI(a)) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice_mut(),
                n_draws,
                n_items,
                true,
            );
            let cache = dahl_salso::log2cache::Log2Cache::new(n_items);
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::VICMLossComputer::new(a, &cache)),
                &partitions,
                &draws,
                results.slice_mut(),
            )
        }
        Some(dahl_salso::LossFunction::VIlb) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::vilb_multiple(&partitions, &psm, results.slice_mut())
        }
        Some(dahl_salso::LossFunction::NVI) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice_mut(),
                n_draws,
                n_items,
                true,
            );
            let cache = dahl_salso::log2cache::Log2Cache::new(n_items);
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| {
                    dahl_salso::optimize::GeneralInformationBasedCMLossComputer::new(
                        n_draws,
                        &cache,
                        dahl_salso::optimize::NVIInformationBasedLoss {},
                    )
                }),
                &partitions,
                &draws,
                results.slice_mut(),
            )
        }
        Some(dahl_salso::LossFunction::ID) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice_mut(),
                n_draws,
                n_items,
                true,
            );
            let cache = dahl_salso::log2cache::Log2Cache::new(n_items);
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| {
                    dahl_salso::optimize::GeneralInformationBasedCMLossComputer::new(
                        n_draws,
                        &cache,
                        dahl_salso::optimize::IDInformationBasedLoss {},
                    )
                }),
                &partitions,
                &draws,
                results.slice_mut(),
            )
        }
        Some(dahl_salso::LossFunction::NID) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice_mut(),
                n_draws,
                n_items,
                true,
            );
            let cache = dahl_salso::log2cache::Log2Cache::new(n_items);
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| {
                    dahl_salso::optimize::GeneralInformationBasedCMLossComputer::new(
                        n_draws,
                        &cache,
                        dahl_salso::optimize::NIDInformationBasedLoss {},
                    )
                }),
                &partitions,
                &draws,
                results.slice_mut(),
            )
        }
        None => stop!("Unsupported loss method: {}", loss),
    };
    results
}

#[roxido]
fn psm(partitions: &mut RMatrix, n_cores: usize) {
    if n_cores == 1 {
        let partitions = Partitions::from_r(partitions.to_i32(pc));
        partitions.pairwise_similarity_matrix(pc)
    } else {
        let n_partitions = partitions.nrow();
        let n_items = partitions.ncol();
        let partitions = partitions.to_i32_mut(pc);
        let n_cores = u32::try_from(n_cores).stop();
        let psm = RMatrix::<f64>::new(n_items, n_items, pc);
        let partitions = dahl_partition::PartitionsHolderBorrower::from_slice(
            partitions.slice_mut(),
            n_partitions,
            n_items,
            true,
        );
        let mut psm2 = dahl_partition::SquareMatrixBorrower::from_slice(psm.slice_mut(), n_items);
        dahl_salso::psm::psm_engine(n_partitions, n_items, n_cores, &partitions, &mut psm2);
        psm
    }
}

#[roxido]
fn minimize_by_enumeration(psm: &mut RMatrix, loss: i32, a: f64) {
    let n_items = psm.nrow();
    let psm = psm.to_f64_mut(pc);
    let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm.slice_mut(), n_items);
    let f = match dahl_salso::LossFunction::from_code(loss, a) {
        Some(loss_function) => match loss_function {
            dahl_salso::LossFunction::BinderDraws(_) => stop!("No implementation for binder."),
            dahl_salso::LossFunction::BinderPSM => dahl_salso::loss::binder_single_kernel,
            dahl_salso::LossFunction::OneMinusARI => stop!("No implementation for omARI."),
            dahl_salso::LossFunction::OneMinusARIapprox => dahl_salso::loss::omariapprox_single,
            dahl_salso::LossFunction::VI(_) => stop!("No implementation for VI."),
            dahl_salso::LossFunction::VIlb => dahl_salso::loss::vilb_single_kernel,
            dahl_salso::LossFunction::NVI => stop!("No implementation for NVI."),
            dahl_salso::LossFunction::ID => stop!("No implementation for ID."),
            dahl_salso::LossFunction::NID => stop!("No implementation for NID."),
        },
        None => stop!("Unsupported loss method: code = {}", loss),
    };
    let minimizer = dahl_salso::optimize::minimize_by_enumeration(f, &psm);
    let results = RVector::<i32>::new(n_items, pc);
    let results_slice = results.slice_mut();
    for (i, v) in minimizer.iter().enumerate() {
        results_slice[i] = i32::try_from(*v + 1).unwrap();
    }
    results
}

#[roxido]
fn minimize_by_salso(
    draws: &mut RMatrix,
    psm: &mut RObject,
    loss: i32,
    a: f64,
    max_n_clusters: i32,
    n_runs: usize,
    seconds: f64,
    max_scans: usize,
    max_zealous_attempts: usize,
    prob_sequential_allocation: f64,
    prob_singletons_initialization: f64,
    n_cores: usize,
) {
    let draws = draws.to_i32_mut(pc);
    let n_items;
    let draws2;
    let psm2;
    let psm3;
    let (loss_function, pdi) = match dahl_salso::LossFunction::from_code(loss, a) {
        Some(loss_function) => match loss_function {
            dahl_salso::LossFunction::BinderDraws(_)
            | dahl_salso::LossFunction::OneMinusARI
            | dahl_salso::LossFunction::VI(_)
            | dahl_salso::LossFunction::NVI
            | dahl_salso::LossFunction::ID
            | dahl_salso::LossFunction::NID => {
                n_items = draws.ncol();
                let n_draws = draws.nrow();
                draws2 = dahl_salso::clustering::Clusterings::from_i32_column_major_order(
                    dahl_partition::PartitionsHolderBorrower::from_slice(
                        draws.slice_mut(),
                        n_draws,
                        n_items,
                        true,
                    )
                    .data(),
                    n_items,
                );
                (
                    loss_function,
                    dahl_salso::PartitionDistributionInformation::Draws(&draws2),
                )
            }
            dahl_salso::LossFunction::BinderPSM
            | dahl_salso::LossFunction::OneMinusARIapprox
            | dahl_salso::LossFunction::VIlb => {
                let psm = psm
                    .as_matrix_mut()
                    .stop_str("'psm' is expected to be a matrix.");
                n_items = psm.ncol();
                psm2 = psm.to_f64_mut(pc);
                psm3 = dahl_partition::SquareMatrixBorrower::from_slice(psm2.slice_mut(), n_items);
                (
                    loss_function,
                    dahl_salso::PartitionDistributionInformation::PairwiseSimilarityMatrix(&psm3),
                )
            }
        },
        None => stop!("Unsupported loss method: code = {}", loss),
    };
    let (max_n_clusters_u16, max_n_clusters_as_rf) = if max_n_clusters < 0 {
        (
            dahl_salso::LabelType::try_from(-max_n_clusters).unwrap(),
            true,
        )
    } else {
        (
            dahl_salso::LabelType::try_from(max_n_clusters).unwrap(),
            false,
        )
    };
    let n_runs = u32::try_from(n_runs).unwrap();
    let max_scans = u32::try_from(max_scans).unwrap();
    let max_zealous_updates = u32::try_from(max_zealous_attempts).unwrap();
    let n_cores = u32::try_from(n_cores).unwrap();
    let mut rng = Pcg64Mcg::from_seed(R::random_bytes::<16>());
    let p = dahl_salso::optimize::SALSOParameters {
        n_items,
        max_size: max_n_clusters_u16,
        max_size_as_rf: max_n_clusters_as_rf,
        max_scans,
        max_zealous_updates,
        n_runs,
        prob_sequential_allocation,
        prob_singletons_initialization,
    };
    let results =
        dahl_salso::optimize::minimize_by_salso(pdi, loss_function, &p, seconds, n_cores, &mut rng);
    let info_attr = RList::with_names(
        &[
            "loss",
            "a",
            "maxNClusters",
            "expectedLoss",
            "initMethod",
            "nScans",
            "nZAcc",
            "nZAtt",
            "nRuns",
            "seconds",
        ],
        pc,
    );
    info_attr.set(1, a.to_r(pc)).stop();
    info_attr
        .set(2, i32::from(results.max_size).to_r(pc))
        .stop();
    info_attr.set(3, results.expected_loss.to_r(pc)).stop();
    info_attr
        .set(
            4,
            i32::try_from(results.initialization_method.to_code())
                .unwrap()
                .to_r(pc),
        )
        .stop();
    info_attr
        .set(5, i32::try_from(results.n_scans).unwrap().to_r(pc))
        .stop();
    info_attr
        .set(
            6,
            i32::try_from(results.n_zealous_accepts).unwrap().to_r(pc),
        )
        .stop();
    info_attr
        .set(
            7,
            i32::try_from(results.n_zealous_attempts).unwrap().to_r(pc),
        )
        .stop();
    info_attr
        .set(8, i32::try_from(results.n_runs).unwrap().to_r(pc))
        .stop();
    info_attr.set(9, results.seconds.to_r(pc)).stop();
    let r = RVector::<i32>::new(n_items, pc);
    for (v, rr) in results.clustering.iter().zip(r.slice_mut().iter_mut()) {
        *rr = i32::try_from(*v + 1).unwrap();
    }
    r.set_class(["salso.estimate"].to_r(pc));
    r.set_attribute(
        RSymbol::new(std::ffi::CStr::from_bytes_with_nul(b"info\0").unwrap()),
        info_attr,
    );
    r
}

// Conditionally-allocated High Probability Subset (CHiPS) Partition

pub struct Partitions {
    n_items: usize,
    n_partitions: usize,
    n_clusters: Vec<u8>,
    raw: Vec<u8>,
}

impl Partitions {
    pub fn from_r(partitions: &RMatrix<i32>) -> Self {
        let slice = partitions.slice();
        let mut raw = Vec::with_capacity(slice.len());
        let n_items = partitions.ncol();
        let n_partitions = partitions.nrow();
        let mut n_clusters = Vec::with_capacity(n_partitions);
        let mut map = std::collections::HashMap::new();
        for sample_index in 0..n_partitions {
            map.clear();
            let mut next_key = 0;
            for item_index in 0..partitions.ncol() {
                let label = slice[n_partitions * item_index + sample_index];
                let relabel = map.entry(label).or_insert_with(|| {
                    let temp = next_key;
                    next_key += 1;
                    temp
                });
                raw.push(*relabel);
            }
            n_clusters.push(next_key);
        }
        Self {
            n_items,
            n_partitions,
            n_clusters,
            raw,
        }
    }

    pub fn alloc_state(&self) -> State {
        let active = (0..self.n_partitions).collect();
        let mapper = self
            .n_clusters
            .iter()
            .map(|n_clusters| vec![u8::MAX; usize::from(*n_clusters)])
            .collect();
        State { active, mapper }
    }

    pub fn at(&self, sample_index: usize) -> &[u8] {
        &self.raw[self.n_items * sample_index..self.n_items * (sample_index + 1)]
    }

    pub fn pairwise_similarity_matrix<'a>(&self, pc: &'a Pc) -> &'a RMatrix<f64> {
        let psm = RMatrix::<f64>::new(self.n_items, self.n_items, pc);
        let slice = psm.slice_mut();
        for labels in self.raw.chunks_exact(self.n_items) {
            for (i, &label_i) in labels.iter().enumerate() {
                for (j, &label_j) in labels[..i].iter().enumerate() {
                    if label_i == label_j {
                        slice[self.n_items * i + j] += 1.0;
                        slice[self.n_items * j + i] += 1.0;
                    }
                }
            }
        }
        let n_partitions_f64 = self.n_partitions as f64;
        for x in slice.iter_mut() {
            // Compute relative frequency.
            *x /= n_partitions_f64;
        }
        for i in 0..self.n_items {
            // Set diagonal elements to 1.0.
            slice[self.n_items * i + i] += 1.0;
        }
        psm
    }
}

pub struct State {
    active: Vec<usize>,
    mapper: Vec<Vec<u8>>,
}

impl State {
    pub fn tabulate(
        &self,
        partitions: &Partitions,
        item_index: usize,
        next_label: u8,
    ) -> Vec<(u8, u32)> {
        let mut counter = Vec::with_capacity(usize::from(next_label + 1));
        for label in 0..=next_label {
            counter.push((label, 0));
        }
        for &sample_index in self.active.iter() {
            let original_label = partitions.at(sample_index)[item_index];
            let new_label = self.mapper[sample_index][usize::from(original_label)];
            if new_label == u8::MAX {
                counter[usize::from(next_label)].1 += 1;
            } else {
                counter[usize::from(new_label)].1 += 1;
            }
        }
        counter
    }

    pub fn house_keeping(
        &mut self,
        partitions: &Partitions,
        item_index: usize,
        label: u8,
        next_label: u8,
    ) {
        let mut deactivate_queue = Vec::new();
        for &sample_index in self.active.iter() {
            let original_label = partitions.at(sample_index)[item_index];
            let new_label = self.mapper[sample_index][usize::from(original_label)];
            let new_label = if new_label == u8::MAX {
                self.mapper[sample_index][usize::from(original_label)] = next_label;
                next_label
            } else {
                new_label
            };
            if new_label != label {
                deactivate_queue.push(sample_index);
            }
        }
        for sample_index in deactivate_queue {
            // DBD: It seems this could be more efficient
            if let Some(position) = self.active.iter().position(|x| *x == sample_index) {
                self.active.swap_remove(position);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct PartialPartition {
    active: Vec<usize>,
    labels: Vec<u8>,
}

impl PartialPartition {
    pub fn empty(n_items: usize) -> Self {
        Self {
            active: Vec::new(),
            labels: vec![0; n_items],
        }
    }

    pub fn allocate(&mut self, index: usize, label: u8) {
        self.active.push(index);
        self.labels[index] = label;
    }
}

impl ToRRef<RVector<i32>> for PartialPartition {
    fn to_r<'a>(&self, pc: &'a Pc) -> &'a mut RVector<i32> {
        let result = RVector::from_value(-1, self.labels.len(), pc);
        let slice = result.slice_mut();
        for &item_index in self.active.iter() {
            slice[item_index] = i32::from(self.labels[item_index] + 1);
        }
        result
    }
}

struct PartialPartitionStorage(Vec<(PartialPartition, f64, usize)>);

impl PartialPartitionStorage {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, x: &PartialPartition, probability: f64, n_items_in_subset: usize) {
        self.0.push((x.clone(), probability, n_items_in_subset));
    }
}

impl ToRRef<RList> for PartialPartitionStorage {
    fn to_r<'a>(&self, pc: &'a Pc) -> &'a mut RList {
        if self.0.is_empty() {
            stop!("The vector is empty.")
        }
        let n_partitions = self.0.len();
        let n_items = self.0[0].0.labels.len();
        let partitions = RMatrix::from_value(-1, n_partitions, n_items, pc);
        let slice_partitions = partitions.slice_mut();
        let probabilities = RVector::<f64>::new(n_partitions, pc);
        let slice_probabilities = probabilities.slice_mut();
        let n_items_in_subset = RVector::<i32>::new(n_partitions, pc);
        let slice_n_items_in_subset = n_items_in_subset.slice_mut();
        for (partition_index, (partition, probability, n_dormant_items)) in
            self.0.iter().rev().enumerate()
        {
            slice_probabilities[partition_index] = *probability;
            slice_n_items_in_subset[partition_index] =
                i32::try_from(n_items - *n_dormant_items).unwrap();
            for &item_index in partition.active.iter() {
                slice_partitions[n_partitions * item_index + partition_index] =
                    i32::from(partition.labels[item_index] + 1);
            }
        }
        let result = RList::with_names(&["subset_partition", "n_items", "probability"], pc);
        if n_partitions == 1 {
            let _ = result.set(0, partitions.to_vector_mut());
        } else {
            let _ = result.set(0, partitions);
        }
        let _ = result.set(1, n_items_in_subset);
        let _ = result.set(2, probabilities);
        result
    }
}

#[roxido]
fn chips(
    partitions: &RMatrix,
    threshold: f64,
    n_runs: usize,
    intermediate_results: bool,
    all_candidates: bool,
    _n_cores: usize,
) {
    if n_runs == 0 {
        stop!("'nRuns' must be at least '0'.")
    }
    if n_runs > 1 && intermediate_results {
        stop!("'nRuns' must be '1' when 'intermediateResults' is 'TRUE'.")
    }
    if !(0.0..=1.0).contains(&threshold) {
        stop!("'threshold' should be a probability in [0.0, 1.0].");
    }
    let partitions = Partitions::from_r(partitions.to_i32(pc));
    let n_partitions_f64 = partitions.n_partitions as f64;
    let mut rng = Pcg64Mcg::from_seed(R::random_bytes::<16>());
    let mut rngs = (0..n_runs)
        .map(|_| Pcg64Mcg::from_rng(&mut rng).unwrap())
        .collect::<Vec<_>>();
    let mut candidates: Vec<_> = rngs
        .par_iter_mut()
        .map(|rng| {
            let mut state = partitions.alloc_state();
            let mut partition = PartialPartition::empty(partitions.n_items);
            let mut next_label = 0;
            let mut dormant: Vec<_> = (0..partitions.n_items).collect();
            let mut storage = PartialPartitionStorage::new();
            let mut probability = 1.0;
            while !dormant.is_empty() {
                let mut best_of = dormant
                    .iter()
                    .map(|&item_index| {
                        let mut counts = state.tabulate(&partitions, item_index, next_label);
                        counts.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
                        let max_count = counts.first().unwrap().1;
                        let number_of_ties = counts.iter().take_while(|x| x.1 == max_count).count();
                        let label = counts[..number_of_ties].choose(rng).unwrap().0;
                        (item_index, label, max_count)
                    })
                    .collect::<Vec<_>>();
                best_of.sort_unstable_by(|(_, _, a), (_, _, b)| b.cmp(a));
                let max_count = best_of.first().unwrap().2;
                let number_of_ties = best_of.iter().take_while(|x| x.2 == max_count).count();
                let (item_index, label, _) = best_of[..number_of_ties].choose(rng).unwrap();
                let candidate_probability = max_count as f64 / n_partitions_f64;
                if candidate_probability < threshold {
                    break;
                }
                partition.allocate(*item_index, *label);
                probability = candidate_probability;
                state.house_keeping(&partitions, *item_index, *label, next_label);
                if *label == next_label {
                    next_label += 1;
                }
                if let Some(position) = dormant.iter().position(|x| *x == *item_index) {
                    dormant.swap_remove(position);
                }
                if intermediate_results {
                    storage.push(&partition, probability, dormant.len());
                }
            }
            if probability >= threshold && !intermediate_results {
                storage.push(&partition, probability, dormant.len());
            }
            storage
        })
        .collect();
    if n_runs == 1 {
        let storage = candidates.pop().unwrap();
        return storage.to_r(pc);
    } else {
        if all_candidates {
            let mut storage = PartialPartitionStorage::new();
            candidates.iter_mut().for_each(|candidate| {
                let x = candidate.0.pop().unwrap();
                storage.push(&x.0, x.1, x.2)
            });
            return storage.to_r(pc);
        } else {
            candidates.sort_unstable_by(|x, y| y.0.last().unwrap().2.cmp(&x.0.last().unwrap().2));
            let storage = candidates.pop().unwrap();
            return storage.to_r(pc);
        }
    }
}
