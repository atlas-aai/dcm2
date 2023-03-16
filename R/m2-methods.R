#' Model Fit M2 Calculations
#'
#' Estimate the M2 statistic as described by Liu et al. (2016).
#'
#' @param model An estimated diagnostic classification model.
#' @param ci The confidence interval for the RMSEA.
#' @param ... Unused, for extensibility.
#'
#' @return A data frame containing:
#' * `m2`: The M2 statistic
#' * `df`: Degrees of freedom for the M2 statistic
#' * `pval`: _p_-value for the M2 statistic
#' * `rmsea`: Root mean square error of approximation
#' * `ci_lower`: Lower end of `ci` interval for RMSEA
#' * `ci_upper`: Upper end of `ci` interval for RMSEA
#' * `srmsr`: Standardized root mean square residual
#'
#' @references Liu, Y., Tian, W., & Xin, T. (2016). An application of
#'    \eqn{M_2}{M2} statistic to evaluate the fit of cognitive diagnostic
#'    models. *Journal of Educational and Behavioral Statistics, 41*, 3-26.
#'    \doi{10.3102/1076998615621293}
#' @export
#'
#' @example
#' sample_size <- 1000
#' test_length <- 4
#' prevalence <- 0.5
#' discrimination <- 3
#' association <- 0.5
#' attributes <- 2
#' set.seed(1234)
#'
#' data <- dcm2:::generate_data(sample_size = sample_size,
#'                              test_length = test_length,
#'                              prevalence = prevalence,
#'                              discrimination = discrimination,
#'                              association = association,
#'                              attributes = attributes)
#' possible_prof <- dcm2:::as_binary(attributes)
#'
#' data$data <- data$data %>%
#'                dplyr::ungroup()
#'
#' fit_dat <- data$data %>%
#'              tidyr::pivot_wider(names_from = "item_id",
#'                                 values_from = "score") %>%
#'              dplyr::select(-"resp_id") %>%
#'              as.matrix() %>%
#'              unname()
#' gdina_mod <- GDINA::GDINA(dat = fit_dat,
#'                           Q = data.frame(data$q_matrix),
#'                           model = "logitGDINA",
#'                           control = list(conv.type = "neg2LL")))
#' m2_fit(gdina_mod, ci = 0.9)
m2_fit <- function(model, ci = 0.9, ...) {
  UseMethod("m2_fit")
}

#' @export
m2_fit.GDINA <- function(model, ci = 0.9, ...) {
  mod_type <- unique(model$model)
  if (mod_type == "LOGITGDINA") mod_type <- "LCDM"

  link_func <- unique(c("identity", "logit", "log")[model$options$linkfunc])

  m2 <- calc_m2(data = model$options$dat, struc_params = model$struc.parm,
                pi_matrix = model$LC.prob, qmatrix = model$options$Q,
                ci = ci, link = link_func, model_type = mod_type)

  return(m2)
}
