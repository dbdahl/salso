mod registration {
    include!(concat!(env!("OUT_DIR"), "/registration.rs"));
}

use num_traits::cast::ToPrimitive;
use rand::SeedableRng;
use rand_pcg::Pcg64Mcg;
use roxido::*;
use std::convert::TryFrom;

#[roxido]
fn bell(n_items: &RObject) -> &RObject {
    match dahl_bellnumber::bell(n_items.usize().stop()).to_f64() {
        Some(x) => x,
        None => f64::INFINITY,
    }
}

#[roxido]
fn lbell(n_items: &RObject) -> &RObject {
    dahl_bellnumber::lbell(n_items.usize().stop())
}

#[roxido]
fn enumerate_partitions(n_items: &RObject) -> &RObject {
    let n_items = n_items.usize().stop();
    let bell_number = dahl_bellnumber::bell(n_items);
    let n_partitions = usize::try_from(bell_number).unwrap();
    let partitions = pc.new_matrix_integer(n_partitions, n_items);
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
    partitions: &mut RObject,
    draws: &mut RObject,
    psm: &mut RObject,
    loss: &RObject,
    a: &RObject,
) -> &RObject {
    let partitions = partitions.matrix_mut().stop();
    let n_partitions = partitions.nrow();
    let n_items = partitions.ncol();
    let partitions = partitions.to_integer_mut(pc);
    let draws = draws.matrix_mut().map(|x| x.to_integer_mut(pc));
    let psm2: &mut RObject<roxido::r::Matrix, f64>;
    let psm_slice = if !psm.is_null() {
        psm2 = psm.matrix_mut().stop().to_double_mut(pc);
        psm2.slice_mut()
    } else {
        &mut []
    };
    let results = pc.new_vector_double(n_partitions);
    let loss = loss.i32().stop();
    let a = a.f64().stop();
    let partitions = dahl_partition::PartitionsHolderBorrower::from_slice(
        partitions.slice_mut(),
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
            let draws = draws.stop();
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
            let draws = draws.stop();
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
            let draws = draws.stop();
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
            let draws = draws.stop();
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
            let draws = draws.stop();
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
fn psm(partitions: &mut RObject, n_cores: &RObject) -> &RObject {
    let partitions = partitions.matrix_mut().stop();
    let n_partitions = partitions.nrow();
    let n_items = partitions.ncol();
    let partitions = partitions.to_integer_mut(pc);
    let n_cores = n_cores.usize().stop() as u32;
    let psm = pc.new_matrix_double(n_items, n_items);
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

#[roxido]
fn minimize_by_enumeration(psm: &mut RObject, loss: &RObject, a: &RObject) -> &RObject {
    let psm = psm.matrix_mut().stop();
    let loss = loss.i32().stop();
    let a = a.f64().stop();
    let n_items = psm.nrow();
    let psm = psm.to_double_mut(pc);
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
    let results = pc.new_vector_integer(n_items);
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
    loss: &RObject,
    a: &RObject,
    max_n_clusters: &RObject,
    n_runs: &RObject,
    seconds: &RObject,
    max_scans: &RObject,
    max_zealous_attempts: &RObject,
    prob_sequential_allocation: &RObject,
    prob_singletons_initialization: &RObject,
    n_cores: &RObject,
) -> &RObject {
    let draws = draws.matrix_mut().map(|x| x.to_integer_mut(pc));
    let n_items;
    let draws2;
    let psm2;
    let psm3;
    let (loss_function, pdi) =
        match dahl_salso::LossFunction::from_code(loss.i32().stop(), a.f64().stop()) {
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
                    let psm = psm.matrix_mut().stop();
                    n_items = psm.ncol();
                    psm2 = psm.to_double_mut(pc);
                    psm3 =
                        dahl_partition::SquareMatrixBorrower::from_slice(psm2.slice_mut(), n_items);
                    (
                        loss_function,
                        dahl_salso::PartitionDistributionInformation::PairwiseSimilarityMatrix(
                            &psm3,
                        ),
                    )
                }
            },
            None => stop!("Unsupported loss method: code = {}", loss.i32().stop()),
        };
    let max_n_clusters_i32 = max_n_clusters.i32().stop();
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
    let n_runs = u32::try_from(n_runs.usize().stop()).unwrap();
    let max_scans = u32::try_from(max_scans.usize().stop()).unwrap();
    let max_zealous_updates = u32::try_from(max_zealous_attempts.usize().stop()).unwrap();
    let prob_sequential_allocation = prob_sequential_allocation.f64().stop();
    let prob_singletons_initialization = prob_singletons_initialization.f64().stop();
    let n_cores = u32::try_from(n_cores.usize().stop()).unwrap();
    let mut rng = Pcg64Mcg::from_seed(Pc::random_bytes::<16>());
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
        seconds.f64().stop(),
        n_cores,
        &mut rng,
    );
    let info_attr = pc.new_list(10);
    info_attr
        .set_names(
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
            ]
            .to_r(pc),
        )
        .stop();
    info_attr.set(1, a).stop();
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
    let r = pc.new_vector_integer(n_items);
    for (v, rr) in results.clustering.iter().zip(r.slice_mut().iter_mut()) {
        *rr = i32::try_from(*v + 1).unwrap();
    }
    r.set_class("salso.estimate".to_r(pc));
    r.set_attribute(pc.new_symbol("info"), info_attr);
    r
}
