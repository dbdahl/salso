// The 'roxido_registration' macro is called at the start of the 'lib.rs' file.
roxido_registration!();
use dahl_salso::LossFunction;
use roxido::*;

use core::f64;
use ndarray::{Array2, Array3, Axis};
use num_traits::cast::ToPrimitive;
use rand::prelude::{IndexedRandom, SliceRandom};
use rand::{Rng, SeedableRng};
use rand_pcg::Mcg128Xsl64;
use rand_pcg::Pcg64Mcg;
use rayon::prelude::*;
use rayon::{ThreadPool, ThreadPoolBuilder};
use std::cmp::Eq;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::Hash;

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
    draws: &mut RObject,
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
                let Ok(draws) = draws.as_matrix_mut() else {
                    stop!("'draws' should be a matrix.");
                };
                let draws = draws.to_i32_mut(pc);
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
    raw: Array2<u8>,
}

impl Partitions {
    pub fn from_r(partitions: &RMatrix<i32>) -> Self {
        let n_items = partitions.ncol();
        let n_partitions = partitions.nrow();
        let slice = partitions.slice();
        let mut raw = Array2::<u8>::zeros((n_items, n_partitions));
        let mut n_clusters = Vec::with_capacity(n_partitions);
        let mut map = HashMap::new();
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
                raw[(item_index, sample_index)] = *relabel;
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

    pub fn max_n_clusters(&self) -> u8 {
        *self.n_clusters.iter().max().unwrap_or(&0)
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

    pub fn at(&self, sample_index: usize, item_index: usize) -> u8 {
        self.raw[(item_index, sample_index)]
    }

    pub fn pairwise_similarity_matrix<'a>(&self, pc: &'a Pc) -> &'a RMatrix<f64> {
        //let psm = RMatrix::<f64>::new(self.n_items, self.n_items, pc);
        let psm = RMatrix::from_value(0.0, self.n_items, self.n_items, pc);
        let slice = psm.slice_mut();
        for labels in self.raw.axis_iter(Axis(1)) {
            for (i, &label_i) in labels.iter().enumerate() {
                for (j, &label_j) in labels.into_iter().take(i).enumerate() {
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

#[derive(Clone)]
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
            let original_label = partitions.at(sample_index, item_index);
            let new_label = self.mapper[sample_index][usize::from(original_label)];
            if new_label == u8::MAX {
                counter[usize::from(next_label)].1 += 1;
            } else {
                counter[usize::from(new_label)].1 += 1;
            }
        }
        counter
    }

    pub fn count(&self) -> usize {
        self.active.len()
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
            let original_label = partitions.at(sample_index, item_index);
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
    n_items: usize,
    offset: usize,
    indices: Vec<usize>,
    labels: Vec<u8>,
}

impl PartialPartition {
    pub fn empty(n_items: usize) -> Self {
        Self {
            n_items,
            offset: 0,
            indices: (0..n_items).collect(),
            labels: vec![0; n_items],
        }
    }

    pub fn allocate(&mut self, index: usize, label: u8) {
        if let Some(pos) = self.dormant().iter().position(|&x| x == index) {
            self.indices.swap(self.offset, self.offset + pos);
            self.offset += 1;
        } else {
            panic!("Item {index} is already allocated.");
        }
        self.labels[index] = label;
    }

    pub fn active(&self) -> &[usize] {
        &self.indices[..self.offset]
    }

    pub fn dormant(&self) -> &[usize] {
        &self.indices[self.offset..]
    }

    pub fn n_dormant(&self) -> usize {
        self.n_items - self.offset
    }

    pub fn complete(&self) -> bool {
        self.offset == self.n_items
    }
}

impl ToRRef<RVector<i32>> for PartialPartition {
    fn to_r<'a>(&self, pc: &'a Pc) -> &'a mut RVector<i32> {
        canonicalize_cluster_labels_generic(&self.labels, pc)
    }
}

#[derive(Clone)]
struct PartialPartitionStorage(Vec<(PartialPartition, f64, usize)>, Vec<(usize, u8, u32)>);

impl PartialPartitionStorage {
    pub fn new() -> Self {
        Self(Vec::new(), Vec::new())
    }

    pub fn push(&mut self, x: &PartialPartition, probability: f64, n_items_in_subset: usize) {
        self.0.push((x.clone(), probability, n_items_in_subset));
    }

    pub fn to_r<'a>(&self, compute_auc: bool, and_salso: bool, pc: &'a Pc) -> &'a mut RList {
        if self.0.is_empty() {
            stop!("The vector is empty.")
        }
        let unallocated_rval = RMatrix::from_value(0_i32, self.1.len(), 2, pc);
        let dimnames_rval = RList::new(2, pc);
        let _ = dimnames_rval.set(0, RVector::from_value("", self.1.len(), pc));
        let _ = dimnames_rval.set(1, ["item", "count"].to_r(pc));
        if let Err(e) = unallocated_rval.set_dimnames(dimnames_rval) {
            println!("ERROR: {}", e);
        }
        for (i, x) in self.1.iter().enumerate() {
            let _ = unallocated_rval.set(i, 0, i32::try_from(x.0 + 1).unwrap());
            let _ = unallocated_rval.set(i, 1, i32::try_from(x.2).unwrap());
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
            for &item_index in partition.active().iter() {
                slice_partitions[n_partitions * item_index + partition_index] =
                    i32::from(partition.labels[item_index] + 1);
            }
        }
        let result = if compute_auc {
            let n_items_f64 = n_items as f64;
            let auc: f64 = (&self.0[..self.0.len() - 1])
                .iter()
                .map(|x| x.1 / n_items_f64)
                .sum();
            let result = if and_salso {
                RList::with_names(
                    &[
                        "chips_partition",
                        "n_items",
                        "probability",
                        "unallocated",
                        "AUChips",
                        "chips_and_salso_partition",
                    ],
                    pc,
                )
            } else {
                RList::with_names(
                    &[
                        "chips_partition",
                        "n_items",
                        "probability",
                        "unallocated",
                        "AUChips",
                    ],
                    pc,
                )
            };
            let _ = result.set(4, auc.to_r(pc));
            result
        } else if and_salso {
            RList::with_names(
                &[
                    "chips_partition",
                    "n_items",
                    "probability",
                    "unallocated",
                    "chips_and_salso_partition",
                ],
                pc,
            )
        } else {
            RList::with_names(
                &["chips_partition", "n_items", "probability", "unallocated"],
                pc,
            )
        };
        if n_partitions == 1 {
            let _ = result.set(0, partitions.to_vector_mut());
        } else {
            let _ = result.set(0, partitions);
        }
        let _ = result.set(1, n_items_in_subset);
        let _ = result.set(2, probabilities);
        let _ = result.set(3, unallocated_rval);
        result
    }
}

fn canonicalize_cluster_labels_generic<'b, T: Copy + Eq + Hash>(
    x: &[T],
    pc: &'b Pc,
) -> &'b mut RVector<i32> {
    let result = RVector::from_value(-1, x.len(), pc);
    let slice = result.slice_mut();
    let mut label_map = HashMap::new();
    let mut next_label = 1;
    for (&value, receiver) in x.iter().zip(slice.iter_mut()) {
        let label = *label_map.entry(value).or_insert_with(|| {
            let new_label = next_label;
            next_label += 1;
            new_label
        });
        *receiver = label;
    }
    result
}

#[roxido]
fn canonicalize_cluster_labels(x: &RVector) {
    let y = x.to_i32(pc);
    canonicalize_cluster_labels_generic(y.slice(), pc)
}

#[roxido]
fn chips(
    partitions: &RMatrix,
    threshold: f64,
    n_runs: usize,
    intermediate_results: bool,
    all_candidates: bool,
    and_salso: bool,
    loss: i32,
    a: f64,
    max_n_clusters: u8,
    initial_partition: &RVector,
    n_cores: usize,
) {
    if !(0.0..=1.0).contains(&threshold) && !threshold.is_nan() {
        stop!("'threshold' should be a probability in [0.0, 1.0].");
    }
    if n_runs == 0 {
        stop!("'nRuns' must be at least '0'.")
    }
    if intermediate_results && all_candidates {
        stop!("At least one of 'intermediateResults' and 'allCandidates' must be FALSE.");
    }
    if and_salso && (intermediate_results || all_candidates) {
        stop!("if 'andSALSO', both 'intermediateResults' and 'allCandidates' must be FALSE.");
    }
    let partitions = Partitions::from_r(partitions.to_i32(pc));
    if partitions.n_partitions == 0 {
        stop!("'partitions' must have a least one partition (i.e., row).");
    }
    if partitions.n_items == 0 {
        stop!("'partitions' must have a least one item (i.e., column).");
    }
    let n_partitions_f64 = partitions.n_partitions as f64;
    let (loss_function, max_n_clusters) = if !and_salso {
        (dahl_salso::LossFunction::VI(1.0), 0)
    } else {
        let Some(loss_function) = dahl_salso::LossFunction::from_code(loss, a) else {
            stop!("Unrecognized loss function specification: {loss} {a}");
        };
        match loss_function {
            dahl_salso::LossFunction::BinderDraws(_) => {}
            dahl_salso::LossFunction::VI(_) => {}
            _ => {
                stop!("Unsupported loss function when using CHiPS");
            }
        }
        let max_n_clusters = match max_n_clusters {
            0 => partitions.max_n_clusters(),
            _ => max_n_clusters,
        };
        (loss_function, max_n_clusters)
    };
    let (
        partition_after_initial,
        state_after_initial,
        probability_after_initial,
        next_label_after_initial,
    ) = if initial_partition.len() > 0 {
        let initial_partition = initial_partition.to_i32(pc);
        let mut partition = PartialPartition::empty(partitions.n_items);
        let mut state = partitions.alloc_state();
        let mut next_label = 0;
        let mut label_map = HashMap::new();
        for (index, &value) in initial_partition.slice().iter().enumerate() {
            if value != -1 {
                let next_label_old = next_label;
                let label = *label_map.entry(value).or_insert_with(|| {
                    let new_label = next_label;
                    next_label += 1;
                    new_label
                });
                partition.allocate(index, label);
                state.house_keeping(&partitions, index, label, next_label_old);
            }
        }
        let probability = state.count() as f64 / n_partitions_f64;
        (partition, state, probability, next_label)
    } else {
        (
            PartialPartition::empty(partitions.n_items),
            partitions.alloc_state(),
            1.0,
            0,
        )
    };
    let mut storage_after_initial = PartialPartitionStorage::new();
    if intermediate_results {
        storage_after_initial.push(
            &partition_after_initial,
            probability_after_initial,
            partition_after_initial.n_dormant(),
        );
    }
    let mut rng = Pcg64Mcg::from_seed(R::random_bytes::<16>());
    let mut rngs = (0..n_runs)
        .map(|_| Pcg64Mcg::from_rng(&mut rng))
        .collect::<Vec<_>>();
    let pool = ThreadPoolBuilder::default()
        .num_threads(n_cores)
        .build()
        .unwrap();
    let mut candidates: Vec<_> = pool.install(|| {
        rngs.par_iter_mut()
            .map(|rng| {
                let mut state = state_after_initial.clone();
                let mut partition = partition_after_initial.clone();
                let mut next_label = next_label_after_initial;
                let mut probability = probability_after_initial;
                let mut storage = storage_after_initial.clone();
                if !threshold.is_nan() {
                    while !partition.complete() {
                        let mut best_of = partition
                            .dormant()
                            .iter()
                            .map(|&item_index| {
                                let mut counts =
                                    state.tabulate(&partitions, item_index, next_label);
                                counts.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
                                let max_count = counts.first().unwrap().1;
                                let number_of_ties =
                                    counts.iter().take_while(|x| x.1 == max_count).count();
                                let label = counts[..number_of_ties].choose(rng).unwrap().0;
                                (item_index, label, max_count)
                            })
                            .collect::<Vec<_>>();
                        best_of.sort_unstable_by(|(_, _, a), (_, _, b)| b.cmp(a));
                        let max_count = best_of.first().unwrap().2;
                        let number_of_ties =
                            best_of.iter().take_while(|x| x.2 == max_count).count();
                        let (item_index, label, _) = best_of[..number_of_ties].choose(rng).unwrap();
                        let candidate_probability = max_count as f64 / n_partitions_f64;
                        if candidate_probability < threshold {
                            storage.1 = best_of;
                            break;
                        }
                        partition.allocate(*item_index, *label);
                        probability = candidate_probability;
                        state.house_keeping(&partitions, *item_index, *label, next_label);
                        if *label == next_label {
                            next_label += 1;
                        }
                        if intermediate_results {
                            storage.push(&partition, probability, partition.n_dormant());
                        }
                    }
                    if probability >= threshold && !intermediate_results {
                        storage.push(&partition, probability, partition.n_dormant());
                    }
                }
                storage
            })
            .collect()
    });
    let chips_result = if all_candidates {
        let mut storage = PartialPartitionStorage::new();
        candidates.sort_unstable_by(|x, y| y.0.last().unwrap().2.cmp(&x.0.last().unwrap().2));
        candidates.iter_mut().for_each(|candidate| {
            let x = candidate.0.pop().unwrap();
            storage.push(&x.0, x.1, x.2)
        });
        storage.to_r(false, false, pc)
    } else if intermediate_results {
        let mut storage = PartialPartitionStorage::new();
        let mut n_items_in_subset = 0;
        while n_items_in_subset <= partitions.n_items {
            let probabilities: Vec<_> = candidates
                .iter()
                .map(|candidate| match candidate.0.get(n_items_in_subset) {
                    Some(x) => x.1,
                    None => 0.0,
                })
                .collect();
            let index_of_max = probabilities
                .iter()
                .enumerate()
                .max_by(|(_, a), (_, b)| a.total_cmp(b))
                .map(|(index, _)| index)
                .unwrap();
            match candidates[index_of_max].0.get(n_items_in_subset) {
                Some(x) => storage.push(&x.0, x.1, x.2),
                None => {
                    break;
                }
            };
            n_items_in_subset += 1;
        }
        storage.to_r(true, false, pc)
    } else {
        let mut best = if !threshold.is_nan() {
            candidates.sort_unstable_by(|x, y| {
                let x_last = x.0.last().unwrap();
                let y_last = y.0.last().unwrap();
                y_last
                    .2
                    .cmp(&x_last.2)
                    .then(x_last.1.total_cmp(&y_last.1))
            });
            candidates.pop().unwrap()
        } else {
            let mut tmp = PartialPartitionStorage::new();
            tmp.push(
                &PartialPartition::empty(partitions.n_items),
                f64::NAN,
                partitions.n_items,
            );
            tmp
        };
        if and_salso {
            let result = best.to_r(false, true, pc);
            let partition = best.0.swap_remove(0).0;
            let complete_partition = match loss_function {
                dahl_salso::LossFunction::BinderDraws(_) => {
                    let loss_computer = BinderLossComputer::from(loss_function).unwrap();
                    salso_after_chips(
                        partition,
                        &partitions,
                        loss_computer,
                        max_n_clusters,
                        pool,
                        rngs,
                    )
                }
                dahl_salso::LossFunction::VI(_) => {
                    let loss_computer =
                        VILossComputer::from(loss_function, partitions.n_items).unwrap();
                    salso_after_chips(
                        partition,
                        &partitions,
                        loss_computer,
                        max_n_clusters,
                        pool,
                        rngs,
                    )
                }
                _ => {
                    panic!("Unsupported loss function.")
                }
            };
            let rval = complete_partition.to_r(pc);
            let _ = result.set(4, rval);
            result
        } else {
            best.to_r(false, false, pc)
        }
    };
    chips_result
}

type CountType = u32;

fn salso_after_chips<S: LossComputer>(
    state: PartialPartition,
    partitions: &Partitions,
    loss_computer: S,
    max_n_clusters: u8,
    pool: ThreadPool,
    mut rngs: Vec<Pcg64Mcg>,
) -> PartialPartition {
    // Allocate dormant items to a random cluster label to form a complete cluster.
    let engine = |rng: &mut Mcg128Xsl64| {
        // Clone partial partition
        let mut state = state.clone();
        let moveable = &mut state.indices[state.offset..];
        for i in moveable.iter() {
            state.labels[*i] = rng.random_range(0..max_n_clusters);
        }
        state.offset = state.n_items;
        // Counts
        let max_n_clusters_in_state = usize::from(max_n_clusters);
        let max_n_clusters_in_draws = usize::from(partitions.max_n_clusters());
        let mut state_counts: Vec<CountType> = vec![0; max_n_clusters_in_state];
        let mut counts = Array3::<CountType>::zeros((
            max_n_clusters_in_state + 1,
            max_n_clusters_in_draws,
            partitions.n_partitions,
        ));
        for item_index in 0..state.n_items {
            let label_state = usize::from(state.labels[item_index]);
            state_counts[label_state] += 1;
            let label_state_index = label_state + 1;
            for draw_index in 0..partitions.n_partitions {
                let label_draw = usize::from(partitions.at(draw_index, item_index));
                counts[(0, label_draw, draw_index)] += 1;
                counts[(label_state_index, label_draw, draw_index)] += 1;
            }
        }
        // Sweetening scans
        let mut state_changed = true;
        while state_changed {
            state_changed = false;
            moveable.shuffle(rng);
            for &item_index in moveable.iter() {
                let from_label = usize::from(state.labels[item_index]);
                let to_label = {
                    let mut found_empty = false;
                    let mut min_value = f64::INFINITY;
                    let mut min_to_label = 0;
                    for to_label in 0..max_n_clusters_in_state {
                        let state_count = state_counts[to_label];
                        if state_count == 0 || state_count == 1 && from_label == to_label {
                            if found_empty {
                                continue;
                            } else {
                                found_empty = true;
                            }
                        }
                        let delta = loss_computer.aggregate_delta(
                            item_index,
                            to_label,
                            from_label == to_label,
                            &state_counts,
                            &counts,
                            partitions,
                        );
                        if delta < min_value {
                            min_value = delta;
                            min_to_label = to_label;
                        }
                    }
                    min_to_label
                };
                if to_label != from_label {
                    state.labels[item_index] = u8::try_from(to_label).unwrap();
                    state_counts[to_label] += 1;
                    state_counts[from_label] -= 1;
                    let to_label_index = to_label + 1;
                    let from_label_index = from_label + 1;
                    for draw_index in 0..partitions.n_partitions {
                        let label_draw = usize::from(partitions.at(draw_index, item_index));
                        counts[(to_label_index, label_draw, draw_index)] += 1;
                        counts[(from_label_index, label_draw, draw_index)] -= 1;
                    }
                    state_changed = true;
                }
            }
        }
        let result = loss_computer.expected_loss(
            CountType::try_from(state.n_items).unwrap(),
            &state_counts,
            &counts,
        );
        (state, result)
    };
    let mut candidates: Vec<_> = pool.install(|| rngs.par_iter_mut().map(engine).collect());
    candidates.sort_unstable_by(|x, y| {
        x.1.partial_cmp(&y.1)
            .expect("NaN values cannot be compared.")
    });
    let best = candidates.swap_remove(0);
    best.0
}

trait LossComputer: Sync {
    fn a(&self) -> f64;
    fn half_b(&self) -> f64;
    fn kernel(&self, x: f64) -> f64;
    fn delta(&self, x: CountType) -> f64;

    fn expected_loss(
        &self,
        n_items: CountType,
        state_counts: &[CountType],
        counts: &Array3<CountType>,
    ) -> f64 {
        let n = n_items as f64;
        let (_, max_n_clusters_in_draws, n_draws) = counts.dim();
        let mut sum = 0.0;
        let half_a = self.a() / 2.0;
        let half_b = self.half_b();
        for &count in state_counts.iter() {
            sum += half_b * self.kernel(count as f64 / n);
        }
        sum *= n_draws as f64;
        for draw_index in 0..n_draws {
            for label_draw in 0..max_n_clusters_in_draws {
                let mc = counts[(0, label_draw, draw_index)];
                if mc > 0 {
                    sum += half_a * self.kernel(mc as f64 / n);
                    for label_state in 0..state_counts.len() {
                        sum -= self
                            .kernel(counts[(label_state + 1, label_draw, draw_index)] as f64 / n);
                    }
                }
            }
        }
        sum /= n_draws as f64 / 2.0;
        sum
    }

    fn aggregate_delta(
        &self,
        item_index: usize,
        to_label: usize,
        same_label: bool,
        state_counts: &[CountType],
        counts: &Array3<CountType>,
        partitions: &Partitions,
    ) -> f64 {
        let phantom = if same_label { 0 } else { 1 };
        let mut sum = partitions.n_partitions as f64
            * self.half_b()
            * self.delta(state_counts[to_label] + phantom);
        let to_label_index = to_label + 1;
        for draw_index in 0..partitions.n_partitions {
            let label = usize::from(partitions.at(draw_index, item_index));
            sum -= self.delta(counts[(to_label_index, label, draw_index)] + phantom);
        }
        sum
    }
}

struct BinderLossComputer {
    a: f64,
    half_b: f64,
}

impl BinderLossComputer {
    fn from(loss_function: LossFunction) -> Option<Self> {
        match loss_function {
            LossFunction::BinderDraws(a) => Some(BinderLossComputer {
                a,
                half_b: (2.0 - a) / 2.0,
            }),
            _ => None,
        }
    }
}

impl LossComputer for BinderLossComputer {
    fn a(&self) -> f64 {
        self.a
    }

    fn half_b(&self) -> f64 {
        self.half_b
    }

    fn kernel(&self, x: f64) -> f64 {
        x.powi(2)
    }

    fn delta(&self, x: CountType) -> f64 {
        f64::from(x)
    }
}

struct VILossComputer {
    a: f64,
    half_b: f64,
    delta_table: Vec<f64>,
}

impl VILossComputer {
    fn from(loss_function: LossFunction, n_items: usize) -> Option<Self> {
        match loss_function {
            LossFunction::VI(a) => {
                let mut delta_table = Vec::with_capacity(n_items + 1);
                delta_table.push(0.0);
                delta_table.push(0.0);
                let mut tmp1 = 0.0;
                for i in 2..=n_items {
                    let tmp2 = Self::kernel_engine(i as f64);
                    delta_table.push(tmp2 - tmp1);
                    tmp1 = tmp2;
                }
                Some(VILossComputer {
                    a,
                    half_b: (2.0 - a) / 2.0,
                    delta_table,
                })
            }
            _ => None,
        }
    }

    fn kernel_engine(x: f64) -> f64 {
        x * x.log2()
    }
}

impl LossComputer for VILossComputer {
    fn a(&self) -> f64 {
        self.a
    }

    fn half_b(&self) -> f64 {
        self.half_b
    }

    fn kernel(&self, x: f64) -> f64 {
        if x == 0.0 {
            0.0
        } else {
            Self::kernel_engine(x)
        }
    }

    fn delta(&self, x: CountType) -> f64 {
        // Rather than calculating it...
        //    if x <= 1 {
        //        0.0
        //    } else {
        //        let x = f64::from(x);
        //        Self::kernel_engine(x) - Self::kernel_engine(x - 1.0)
        //    }
        // ... just look it up in a cache
        self.delta_table[x as usize]
    }
}
