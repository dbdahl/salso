#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

void dahl_salso__expected_loss(int32_t n_partitions,
                               int32_t n_items,
                               int32_t *partition_ptr,
                               int32_t n_draws,
                               int32_t *draws_ptr,
                               double *psm_ptr,
                               int32_t loss,
                               double *results_ptr);

void dahl_salso__minimize_by_enumeration(int32_t n_items,
                                         double *psm_ptr,
                                         int32_t loss,
                                         int32_t *results_label_ptr);

void dahl_salso__minimize_by_salso(int32_t n_items,
                                   int32_t n_draws,
                                   int32_t *draws_ptr,
                                   double *psm_ptr,
                                   int32_t loss,
                                   int32_t max_size,
                                   int32_t n_runs,
                                   double seconds,
                                   int32_t max_scans,
                                   int32_t max_zealous_updates,
                                   double prob_sequential_allocation,
                                   double prob_singletons_initialization,
                                   int32_t n_cores,
                                   int32_t *results_labels_ptr,
                                   double *results_expected_loss_ptr,
                                   int32_t *results_n_scans_ptr,
                                   int32_t *results_n_zealous_accepts_ptr,
                                   int32_t *results_n_zealous_attempts_ptr,
                                   int32_t *results_n_runs_ptr,
                                   int32_t *results_max_size_ptr,
                                   int32_t *results_initialization_method_ptr,
                                   const int32_t *seed_ptr);

void dahl_salso__psm(int32_t n_partitions,
                     int32_t n_items,
                     int32_t n_cores,
                     int32_t *partitions_ptr,
                     double *psm_ptr);
