// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Mord
Rcpp::List Mord(arma::vec item_no, arma::mat& LCprob, arma::vec prior);
RcppExport SEXP _dcm_m2_Mord(SEXP item_noSEXP, SEXP LCprobSEXP, SEXP priorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type item_no(item_noSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type LCprob(LCprobSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type prior(priorSEXP);
    rcpp_result_gen = Rcpp::wrap(Mord(item_no, LCprob, prior));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dcm_m2_Mord", (DL_FUNC) &_dcm_m2_Mord, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_dcm_m2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
