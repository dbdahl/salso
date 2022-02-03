mod registration;

use num_traits::cast::ToPrimitive;
use rand::SeedableRng;
use rand_pcg::Pcg64Mcg;
use roxido::*;
use std::convert::TryFrom;

#[roxido]
fn bell(n_items: Rval) -> Rval {
    let x = match dahl_bellnumber::bell(n_items.as_usize()).to_f64() {
        Some(x) => x,
        None => f64::INFINITY,
    };
    Rval::new(x, &mut pc)
}

#[roxido]
fn lbell(n_items: Rval) -> Rval {
    let x = dahl_bellnumber::lbell(n_items.as_usize());
    Rval::new(x, &mut pc)
}

#[roxido]
fn enumerate_partitions(n_items: Rval) -> Rval {
    let n_items = n_items.as_usize();
    let bell_number = dahl_bellnumber::bell(n_items);
    let n_partitions = usize::try_from(bell_number).unwrap();
    let (partitions, slice) = Rval::new_matrix_integer(n_partitions, n_items, &mut pc);
    let mut phv =
        dahl_partition::PartitionsHolderBorrower::from_slice(slice, n_partitions, n_items, true);
    for mut p in dahl_partition::Partition::iter(n_items) {
        p.iter_mut().for_each(|x| *x += 1);
        phv.push_slice(&p[..]);
    }
    partitions
}

#[roxido]
fn expected_loss(partitions: Rval, draws: Rval, psm: Rval, loss: Rval, a: Rval) -> Rval {
    let n_partitions = partitions.nrow();
    let n_items = partitions.ncol();
    let (_, partitions_slice) = partitions.coerce_integer(&mut pc).unwrap();
    let (_, draws_slice) = draws.coerce_integer(&mut pc).unwrap();
    let (_, psm_slice) = psm.coerce_double(&mut pc).unwrap();
    let (results, results_slice) = Rval::new_vector_double(n_partitions, &mut pc);
    let loss = loss.as_i32();
    let a = a.as_f64();
    let partitions = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions_slice,
        n_partitions,
        n_items,
        true,
    );
    let loss_function = dahl_salso::LossFunction::from_code(loss, a);
    match loss_function {
        Some(dahl_salso::LossFunction::BinderDraws(a)) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws_slice,
                n_draws,
                n_items,
                true,
            );
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::BinderCMLossComputer::new(a)),
                &partitions,
                &draws,
                results_slice,
            )
        }
        Some(dahl_salso::LossFunction::BinderPSM) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::binder_multiple(&partitions, &psm, results_slice)
        }
        Some(dahl_salso::LossFunction::OneMinusARI) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws_slice,
                n_draws,
                n_items,
                true,
            );
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::OMARICMLossComputer::new(n_draws)),
                &partitions,
                &draws,
                results_slice,
            )
        }
        Some(dahl_salso::LossFunction::OneMinusARIapprox) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::omariapprox_multiple(&partitions, &psm, results_slice)
        }
        Some(dahl_salso::LossFunction::VI(a)) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws_slice,
                n_draws,
                n_items,
                true,
            );
            let cache = dahl_salso::log2cache::Log2Cache::new(n_items);
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::VICMLossComputer::new(a, &cache)),
                &partitions,
                &draws,
                results_slice,
            )
        }
        Some(dahl_salso::LossFunction::VIlb) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::vilb_multiple(&partitions, &psm, results_slice)
        }
        Some(dahl_salso::LossFunction::NVI) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws_slice,
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
                results_slice,
            )
        }
        Some(dahl_salso::LossFunction::ID) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws_slice,
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
                results_slice,
            )
        }
        Some(dahl_salso::LossFunction::NID) => {
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws_slice,
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
                results_slice,
            )
        }
        None => panic!("Unsupported loss method: {}", loss),
    };
    results
}

#[roxido]
fn psm(partitions: Rval, n_cores: Rval) -> Rval {
    let n_partitions = partitions.nrow();
    let n_items = partitions.ncol();
    let (_, partitions_slice) = partitions.coerce_integer(&mut pc).unwrap();
    let n_cores = n_cores.as_usize() as u32;
    let (psm, psm_slice) = Rval::new_matrix_double(n_items, n_items, &mut pc);
    let partitions = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions_slice,
        n_partitions,
        n_items,
        true,
    );
    let mut psm2 = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
    dahl_salso::psm::psm_engine(n_partitions, n_items, n_cores, &partitions, &mut psm2);
    psm
}

#[roxido]
fn minimize_by_enumeration(psm: Rval, loss: Rval, a: Rval) -> Rval {
    let loss = loss.as_i32();
    let a = a.as_f64();
    let n_items = psm.nrow();
    let (_, psm_slice) = psm.coerce_double(&mut pc).unwrap();
    let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
    let f = match dahl_salso::LossFunction::from_code(loss, a) {
        Some(loss_function) => match loss_function {
            dahl_salso::LossFunction::BinderDraws(_) => panic!("No implementation for binder."),
            dahl_salso::LossFunction::BinderPSM => dahl_salso::loss::binder_single_kernel,
            dahl_salso::LossFunction::OneMinusARI => panic!("No implementation for omARI."),
            dahl_salso::LossFunction::OneMinusARIapprox => dahl_salso::loss::omariapprox_single,
            dahl_salso::LossFunction::VI(_) => panic!("No implementation for VI."),
            dahl_salso::LossFunction::VIlb => dahl_salso::loss::vilb_single_kernel,
            dahl_salso::LossFunction::NVI => panic!("No implementation for NVI."),
            dahl_salso::LossFunction::ID => panic!("No implementation for ID."),
            dahl_salso::LossFunction::NID => panic!("No implementation for NID."),
        },
        None => panic!("Unsupported loss method: code = {}", loss),
    };
    let minimizer = dahl_salso::optimize::minimize_by_enumeration(f, &psm);
    let (results, results_slice) = Rval::new_vector_integer(n_items, &mut pc);
    for (i, v) in minimizer.iter().enumerate() {
        results_slice[i] = i32::try_from(*v + 1).unwrap();
    }
    results
}

#[roxido]
fn minimize_by_salso(
    draws: Rval,
    psm: Rval,
    loss: Rval,
    a: Rval,
    max_n_clusters: Rval,
    n_runs: Rval,
    seconds: Rval,
    max_scans: Rval,
    max_zealous_attempts: Rval,
    prob_sequential_allocation: Rval,
    prob_singletons_initialization: Rval,
    n_cores: Rval,
) -> Rval {
    let n_items;
    let draws2;
    let psm2;
    let (loss_function, pdi) = match dahl_salso::LossFunction::from_code(loss.as_i32(), a.as_f64())
    {
        Some(loss_function) => match loss_function {
            dahl_salso::LossFunction::BinderDraws(_)
            | dahl_salso::LossFunction::OneMinusARI
            | dahl_salso::LossFunction::VI(_)
            | dahl_salso::LossFunction::NVI
            | dahl_salso::LossFunction::ID
            | dahl_salso::LossFunction::NID => {
                n_items = draws.ncol();
                let n_draws = draws.nrow();
                let (_, draws_slice) = draws.coerce_integer(&mut pc).unwrap();
                draws2 = dahl_salso::clustering::Clusterings::from_i32_column_major_order(
                    dahl_partition::PartitionsHolderBorrower::from_slice(
                        draws_slice,
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
                n_items = psm.ncol();
                let (_, psm_slice) = psm.coerce_double(&mut pc).unwrap();
                psm2 = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
                (
                    loss_function,
                    dahl_salso::PartitionDistributionInformation::PairwiseSimilarityMatrix(&psm2),
                )
            }
        },
        None => panic!("Unsupported loss method: code = {}", loss.as_i32()),
    };
    let max_n_clusters_i32 = max_n_clusters.as_i32();
    let (max_n_clusters_u16, max_n_clusters_as_rf) = if max_n_clusters_i32 < 0 {
        (
            dahl_salso::LabelType::try_from(-max_n_clusters_i32).unwrap(),
            true,
        )
    } else {
        (
            dahl_salso::LabelType::try_from(max_n_clusters_i32).unwrap(),
            false,
        )
    };
    let n_runs = u32::try_from(n_runs.as_usize()).unwrap();
    let max_scans = u32::try_from(max_scans.as_usize()).unwrap();
    let max_zealous_updates = u32::try_from(max_zealous_attempts.as_usize()).unwrap();
    let prob_sequential_allocation = prob_sequential_allocation.as_f64();
    let prob_singletons_initialization = prob_singletons_initialization.as_f64();
    let n_cores = u32::try_from(n_cores.as_usize()).unwrap();
    let mut rng = Pcg64Mcg::from_seed(r::random_bytes::<16>());
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
    let results = dahl_salso::optimize::minimize_by_salso(
        pdi,
        loss_function,
        &p,
        seconds.as_f64(),
        n_cores,
        &mut rng,
    );
    let info_attr = Rval::new_list(10, &mut pc);
    info_attr.names_gets(Rval::new(
        [
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
        &mut pc,
    ));
    info_attr.set_list_element(1, a);
    info_attr.set_list_element(
        2,
        Rval::new(i32::try_from(results.max_size).unwrap(), &mut pc),
    );
    info_attr.set_list_element(3, Rval::new(results.expected_loss, &mut pc));
    info_attr.set_list_element(
        4,
        Rval::new(
            i32::try_from(results.initialization_method.to_code()).unwrap(),
            &mut pc,
        ),
    );
    info_attr.set_list_element(
        5,
        Rval::new(i32::try_from(results.n_scans).unwrap(), &mut pc),
    );
    info_attr.set_list_element(
        6,
        Rval::new(i32::try_from(results.n_zealous_accepts).unwrap(), &mut pc),
    );
    info_attr.set_list_element(
        7,
        Rval::new(i32::try_from(results.n_zealous_attempts).unwrap(), &mut pc),
    );
    info_attr.set_list_element(
        8,
        Rval::new(i32::try_from(results.n_runs).unwrap(), &mut pc),
    );
    info_attr.set_list_element(9, Rval::new(results.seconds, &mut pc));
    let (r, r_slice) = Rval::new_vector_integer(n_items, &mut pc);
    for (v, rr) in results.clustering.iter().zip(r_slice.iter_mut()) {
        *rr = i32::try_from(*v + 1).unwrap();
    }
    r.class_gets(Rval::new("salso.estimate", &mut pc));
    r.set_attribute("info", info_attr, &mut pc);
    r
}
