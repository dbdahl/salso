#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Import C headers for rust API
#include "rustlib/dahl-partition.h"
#include "rustlib/dahl-bellnumber.h"
#include "rustlib/dahl-salso.h"

// Actual Wrappers
SEXP psm(SEXP partitions_sexp, SEXP parallel_sexp) {
  int n_partitions = Rf_nrows(partitions_sexp);
  int n_items = Rf_ncols(partitions_sexp);
  partitions_sexp = PROTECT(Rf_coerceVector(partitions_sexp, INTSXP));
  int *partitions = INTEGER(partitions_sexp);
  int parallel = Rf_asLogical(parallel_sexp);
  SEXP counts = PROTECT(Rf_allocVector(REALSXP, n_items*n_items));
  double *xcounts = REAL(counts);
  dahl_salso__psm(n_partitions, n_items, parallel, partitions, xcounts);
  UNPROTECT(2);
  return counts;
}

SEXP expected_loss(SEXP partitions_sexp, SEXP psm_sexp, SEXP use_vilb_sexp) {
  int n_partitions = Rf_nrows(partitions_sexp);
  int n_items = Rf_ncols(partitions_sexp);
  partitions_sexp = PROTECT(Rf_coerceVector(partitions_sexp, INTSXP));
  int *partitions = INTEGER(partitions_sexp);
  psm_sexp = PROTECT(Rf_coerceVector(psm_sexp, REALSXP));
  double *psm = REAL(psm_sexp);
  SEXP results_sexp = PROTECT(Rf_allocVector(REALSXP, n_partitions));
  double *results = REAL(results_sexp);
  int loss = Rf_asLogical(use_vilb_sexp);
  dahl_salso__expected_loss(n_partitions, n_items, partitions, psm, loss, results);
  UNPROTECT(3);
  return results_sexp;
}

SEXP bell(SEXP n_items_sexp) {
  n_items_sexp = PROTECT(Rf_coerceVector(n_items_sexp, INTSXP));
  int n_items = Rf_asInteger(n_items_sexp);
  UNPROTECT(1);
  return Rf_ScalarReal(dahl_bellnumber__bell(n_items));
}

SEXP lbell(SEXP n_items_sexp) {
  n_items_sexp = PROTECT(Rf_coerceVector(n_items_sexp, INTSXP));
  int n_items = Rf_asInteger(n_items_sexp);
  UNPROTECT(1);
  return Rf_ScalarReal(dahl_bellnumber__lbell(n_items));
}

SEXP enumerate_partitions(SEXP n_items_sexp) {
  n_items_sexp = PROTECT(Rf_coerceVector(n_items_sexp, INTSXP));
  int n_items = *(INTEGER(n_items_sexp));
  int n_partitions = (int) dahl_bellnumber__bell(n_items);
  SEXP partitions = PROTECT(Rf_allocVector(INTSXP, n_partitions*n_items));
  int *xpartitions = INTEGER(partitions);
  dahl_partition__enumerated(n_partitions, n_items, xpartitions);
  UNPROTECT(2);
  return partitions;
}

SEXP minimize_by_enumeration(SEXP n_items_sexp, SEXP psm_sexp, SEXP use_vilb_sexp) {
  n_items_sexp = PROTECT(Rf_coerceVector(n_items_sexp, INTSXP));
  int n_items = Rf_asInteger(n_items_sexp);
  psm_sexp = PROTECT(Rf_coerceVector(psm_sexp, REALSXP));
  double *psm = REAL(psm_sexp);
  int loss = Rf_asLogical(use_vilb_sexp);
  SEXP results_labels_sexp = PROTECT(Rf_allocVector(INTSXP, n_items));
  int *results_labels = INTEGER(results_labels_sexp);
  dahl_salso__minimize_by_enumeration(n_items, psm, loss, results_labels);
  UNPROTECT(3);
  return results_labels_sexp;
}

SEXP minimize_by_salso(SEXP n_items_sexp, SEXP psm_sexp, SEXP use_vilb_sexp, SEXP max_size_sexp, SEXP max_scans_sexp, SEXP n_permutations_sexp, SEXP probability_of_exploration_sexp, SEXP seconds_sexp, SEXP parallel_sexp, SEXP seed_sexp) {
  int n_items = Rf_asInteger(n_items_sexp);
  psm_sexp = PROTECT(Rf_coerceVector(psm_sexp, REALSXP));
  double *psm = REAL(psm_sexp);
  int loss = Rf_asLogical(use_vilb_sexp);
  int max_size = Rf_asInteger(max_size_sexp);
  int max_scans = Rf_asInteger(max_scans_sexp);
  int n_permutations = Rf_asInteger(n_permutations_sexp);
  double probability_of_exploration = Rf_asReal(probability_of_exploration_sexp);
  double seconds = Rf_asReal(seconds_sexp);
  int parallel = Rf_asLogical(parallel_sexp);
  SEXP results_labels_sexp = PROTECT(Rf_allocVector(INTSXP, n_items));
  int *results_labels = INTEGER(results_labels_sexp);
  SEXP results_expected_loss_sexp = PROTECT(Rf_allocVector(REALSXP, 1));
  double *results_expected_loss = REAL(results_expected_loss_sexp);
  SEXP results_n_scans_sexp = PROTECT(Rf_allocVector(INTSXP, 1));
  int *results_n_scans = INTEGER(results_n_scans_sexp);
  SEXP results_actual_n_permutations_sexp = PROTECT(Rf_allocVector(INTSXP, 1));
  int *results_actual_n_permutations = INTEGER(results_actual_n_permutations_sexp);
  int *seed = INTEGER(seed_sexp);
  dahl_salso__minimize_by_salso(n_items, psm, loss, max_size, max_scans, n_permutations, probability_of_exploration, seconds, parallel, results_labels, results_expected_loss, results_n_scans, results_actual_n_permutations, seed);
  SEXP results = PROTECT(Rf_allocVector(VECSXP, 5));
  SET_VECTOR_ELT(results, 0, results_labels_sexp);
  SET_VECTOR_ELT(results, 2, results_expected_loss_sexp);
  SET_VECTOR_ELT(results, 3, results_n_scans_sexp);
  SET_VECTOR_ELT(results, 4, results_actual_n_permutations_sexp);
  UNPROTECT(6);
  return results;
}

// Standard R package stuff
static const R_CallMethodDef CallEntries[] = {
  {".psm", (DL_FUNC) &psm, 2},
  {".expected_loss", (DL_FUNC) &expected_loss, 3},
  {".bell", (DL_FUNC) &bell, 1},
  {".lbell", (DL_FUNC) &lbell, 1},
  {".enumerate_partitions", (DL_FUNC) &enumerate_partitions, 1},
  {".minimize_by_enumeration", (DL_FUNC) &minimize_by_enumeration, 3},
  {".minimize_by_salso", (DL_FUNC) &minimize_by_salso, 10},
  {NULL, NULL, 0}
};

void R_init_salso(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
