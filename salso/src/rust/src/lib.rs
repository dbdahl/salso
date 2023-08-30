mod registration {
    include!(concat!(env!("OUT_DIR"), "/registration.rs"));
}

use num_traits::cast::ToPrimitive;
use rand::SeedableRng;
use rand_pcg::Pcg64Mcg;
use roxido::*;
use std::convert::TryFrom;

#[roxido]
fn bell(n_items: RObject) -> RObject {
    match dahl_bellnumber::bell(n_items.as_usize().stop()).to_f64() {
        Some(x) => x,
        None => f64::INFINITY,
    }
}

#[roxido]
fn lbell(n_items: RObject) -> RObject {
    dahl_bellnumber::lbell(n_items.as_usize().stop())
}

#[roxido]
fn enumerate_partitions(n_items: RObject) -> RObject {
    let n_items = n_items.as_usize().stop();
    let bell_number = dahl_bellnumber::bell(n_items);
    let n_partitions = usize::try_from(bell_number).unwrap();
    let partitions = R::new_matrix_integer(n_partitions, n_items, pc);
    let mut phv = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions.slice(),
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
    partitions: RObject,
    draws: RObject,
    psm: RObject,
    loss: RObject,
    a: RObject,
) -> RObject {
    let partitions = partitions.as_matrix().stop();
    let n_partitions = partitions.nrow();
    let n_items = partitions.ncol();
    let partitions = partitions.to_mode_integer(pc);
    let draws = draws.as_matrix().map(|x| x.to_mode_integer(pc));
    let psm_slice = if !psm.is_null() {
        psm.as_matrix().stop().to_mode_double(pc).slice()
    } else {
        &mut []
    };
    let results = R::new_vector_double(n_partitions, pc);
    let loss = loss.as_i32().stop();
    let a = a.as_f64().stop();
    let partitions = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions.slice(),
        n_partitions,
        n_items,
        true,
    );
    let loss_function = dahl_salso::LossFunction::from_code(loss, a);
    match loss_function {
        Some(dahl_salso::LossFunction::BinderDraws(a)) => {
            let draws = draws.stop();
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice(),
                n_draws,
                n_items,
                true,
            );
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::BinderCMLossComputer::new(a)),
                &partitions,
                &draws,
                results.slice(),
            )
        }
        Some(dahl_salso::LossFunction::BinderPSM) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(
                psm.as_matrix().stop().as_mode_double().stop().slice(),
                n_items,
            );
            dahl_salso::loss::binder_multiple(&partitions, &psm, results.slice())
        }
        Some(dahl_salso::LossFunction::OneMinusARI) => {
            let draws = draws.stop();
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice(),
                n_draws,
                n_items,
                true,
            );
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::OMARICMLossComputer::new(n_draws)),
                &partitions,
                &draws,
                results.slice(),
            )
        }
        Some(dahl_salso::LossFunction::OneMinusARIapprox) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::omariapprox_multiple(&partitions, &psm, results.slice())
        }
        Some(dahl_salso::LossFunction::VI(a)) => {
            let draws = draws.stop();
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice(),
                n_draws,
                n_items,
                true,
            );
            let cache = dahl_salso::log2cache::Log2Cache::new(n_items);
            dahl_salso::loss::compute_loss_multiple(
                Box::new(|| dahl_salso::optimize::VICMLossComputer::new(a, &cache)),
                &partitions,
                &draws,
                results.slice(),
            )
        }
        Some(dahl_salso::LossFunction::VIlb) => {
            let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm_slice, n_items);
            dahl_salso::loss::vilb_multiple(&partitions, &psm, results.slice())
        }
        Some(dahl_salso::LossFunction::NVI) => {
            let draws = draws.stop();
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice(),
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
                results.slice(),
            )
        }
        Some(dahl_salso::LossFunction::ID) => {
            let draws = draws.stop();
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice(),
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
                results.slice(),
            )
        }
        Some(dahl_salso::LossFunction::NID) => {
            let draws = draws.stop();
            let n_draws = draws.nrow();
            let draws = dahl_partition::PartitionsHolderBorrower::from_slice(
                draws.slice(),
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
                results.slice(),
            )
        }
        None => stop!("Unsupported loss method: {}", loss),
    };
    results
}

#[roxido]
fn psm(partitions: RObject, n_cores: RObject) -> RObject {
    let partitions = partitions.as_matrix().stop();
    let n_partitions = partitions.nrow();
    let n_items = partitions.ncol();
    let partitions = partitions.to_mode_integer(pc);
    let n_cores = n_cores.as_usize().stop() as u32;
    let psm = R::new_matrix_double(n_items, n_items, pc);
    let partitions = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions.slice(),
        n_partitions,
        n_items,
        true,
    );
    let mut psm2 = dahl_partition::SquareMatrixBorrower::from_slice(psm.slice(), n_items);
    dahl_salso::psm::psm_engine(n_partitions, n_items, n_cores, &partitions, &mut psm2);
    psm
}

#[roxido]
fn minimize_by_enumeration(psm: RObject, loss: RObject, a: RObject) -> RObject {
    let psm = psm.as_matrix().stop();
    let loss = loss.as_i32().stop();
    let a = a.as_f64().stop();
    let n_items = psm.nrow();
    let psm = psm.to_mode_double(pc);
    let psm = dahl_partition::SquareMatrixBorrower::from_slice(psm.slice(), n_items);
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
    let results = R::new_vector_integer(n_items, pc);
    let results_slice = results.slice();
    for (i, v) in minimizer.iter().enumerate() {
        results_slice[i] = i32::try_from(*v + 1).unwrap();
    }
    results
}

#[roxido]
fn minimize_by_salso(
    draws: RObject,
    psm: RObject,
    loss: RObject,
    a: RObject,
    max_n_clusters: RObject,
    n_runs: RObject,
    seconds: RObject,
    max_scans: RObject,
    max_zealous_attempts: RObject,
    prob_sequential_allocation: RObject,
    prob_singletons_initialization: RObject,
    n_cores: RObject,
) -> RObject {
    let draws = draws.as_matrix().map(|x| x.to_mode_integer(pc));
    let n_items;
    let draws2;
    let psm2;
    let (loss_function, pdi) =
        match dahl_salso::LossFunction::from_code(loss.as_i32().stop(), a.as_f64().stop()) {
            Some(loss_function) => match loss_function {
                dahl_salso::LossFunction::BinderDraws(_)
                | dahl_salso::LossFunction::OneMinusARI
                | dahl_salso::LossFunction::VI(_)
                | dahl_salso::LossFunction::NVI
                | dahl_salso::LossFunction::ID
                | dahl_salso::LossFunction::NID => {
                    let draws = draws.stop();
                    n_items = draws.ncol();
                    let n_draws = draws.nrow();
                    draws2 = dahl_salso::clustering::Clusterings::from_i32_column_major_order(
                        dahl_partition::PartitionsHolderBorrower::from_slice(
                            draws.slice(),
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
                    let psm = psm.as_matrix().stop();
                    n_items = psm.ncol();
                    let psm = psm.to_mode_double(pc);
                    psm2 = dahl_partition::SquareMatrixBorrower::from_slice(psm.slice(), n_items);
                    (
                        loss_function,
                        dahl_salso::PartitionDistributionInformation::PairwiseSimilarityMatrix(
                            &psm2,
                        ),
                    )
                }
            },
            None => stop!("Unsupported loss method: code = {}", loss.as_i32().stop()),
        };
    let max_n_clusters_i32 = max_n_clusters.as_i32().stop();
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
    let n_runs = u32::try_from(n_runs.as_usize().stop()).unwrap();
    let max_scans = u32::try_from(max_scans.as_usize().stop()).unwrap();
    let max_zealous_updates = u32::try_from(max_zealous_attempts.as_usize().stop()).unwrap();
    let prob_sequential_allocation = prob_sequential_allocation.as_f64().stop();
    let prob_singletons_initialization = prob_singletons_initialization.as_f64().stop();
    let n_cores = u32::try_from(n_cores.as_usize().stop()).unwrap();
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
    let results = dahl_salso::optimize::minimize_by_salso(
        pdi,
        loss_function,
        &p,
        seconds.as_f64().stop(),
        n_cores,
        &mut rng,
    );
    let info_attr = R::new_list(10, pc);
    info_attr
        .set_names(
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
            ]
            .to_r(pc),
        )
        .stop();
    info_attr.set(1, &a).stop();
    info_attr
        .set(2, &i32::try_from(results.max_size).unwrap().to_r(pc))
        .stop();
    info_attr.set(3, &results.expected_loss.to_r(pc)).stop();
    info_attr
        .set(
            4,
            &i32::try_from(results.initialization_method.to_code())
                .unwrap()
                .to_r(pc),
        )
        .stop();
    info_attr
        .set(5, &i32::try_from(results.n_scans).unwrap().to_r(pc))
        .stop();
    info_attr
        .set(
            6,
            &i32::try_from(results.n_zealous_accepts).unwrap().to_r(pc),
        )
        .stop();
    info_attr
        .set(
            7,
            &i32::try_from(results.n_zealous_attempts).unwrap().to_r(pc),
        )
        .stop();
    info_attr
        .set(8, &i32::try_from(results.n_runs).unwrap().to_r(pc))
        .stop();
    info_attr.set(9, &results.seconds.to_r(pc)).stop();
    let r = R::new_vector_integer(n_items, pc);
    for (v, rr) in results.clustering.iter().zip(r.slice().iter_mut()) {
        *rr = i32::try_from(*v + 1).unwrap();
    }
    r.set_class(&"salso.estimate".to_r(pc));
    r.set_attribute(&R::new_symbol("info", pc), &info_attr);
    r
}
