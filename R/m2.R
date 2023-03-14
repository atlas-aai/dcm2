#' Model Fit M2 Calculations
#'
#' Estimate the M2 statistic as described by Liu et al. (2016).
#'
#' @param data A data frame containing the raw data, where there is one row per
#' respondent and one column per item
#' @param struc_params A vector containing the structural parameters of the
#' estimated model
#' @param pi_matrix An item-by-class matrix containing the probability of a
#' correct response by members of each latent class
#' @param qmatrix A data frame containing the Q-matrix
#' @param num_item_params A vector containing the number of estimated item
#' parameters for each item
#' @param ci The confidence interval for the RMSEA, computed from the M2
#' @param link A character containing the link function.
#' @param model_type A character containing the model type (e.g., `LCDM`)
#' that was estimated.
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
#'    doi:10.3102/1076998615621293
#' @noRd
calc_m2 <- function(data, struc_params, pi_matrix, qmatrix, num_item_params,
                    ci = 0.9, link = "logit",
                    model_type = c("LCDM", "GDINA", "ACDM", "LLM", "RRUM",
                                   "DINO", "DINA", "BUGDINO")) {

  # data checks
  check_data(data, qmatrix)
  check_struc_params(struc_params, qmatrix)
  check_pi_matrix(pi_matrix, qmatrix)
  check_qmatrix(qmatrix, pi_matrix)
  ci <- check_ci(ci)
  model_type <- rlang::arg_match(model_type)

  if(model_type %in% c("DINO", "DINA", "BUGDINO")) {
    exp_num_item_params <- rep(2, nrow(qmatrix))
  } else if(model_type %in% c("LCDM", "GDINA")) {
    exp_num_item_params <- qmatrix %>%
      modelr::model_matrix(stats::as.formula(paste0("~ .^", ncol(.)))) %>%
      dplyr::mutate(total_params = rowSums(.)) %>%
      dplyr::pull("total_params")
  } else if(model_type %in% c("ACDM", "LLM", "RRUM")) {
    exp_num_item_params <- qmatrix %>%
      dplyr::mutate(total_params = rowSums(.) + 1) %>%
      dplyr::pull("total_params")
  }

  check_num_item_params(num_item_params, qmatrix, exp_num_item_params)

  model_type <- ifelse(model_type == "GDINA", "LCDM", model_type)

  num_items <- nrow(qmatrix)
  num_attr <- ncol(qmatrix)

  n <- nrow(data)
  l <- 2 ^ num_attr
  n_par_j <- sum(num_item_params)

  empirical_marginal_probabilities <- calc_emp_marginal_prob(data, n)

  base_rates <- t(as.matrix(struc_params))
  colnames(base_rates) <- att_profile(num_attr)

  model_marginal_probabilities <- calc_mod_marginal_prob(num_items, pi_matrix,
                                                         base_rates)

  cr <- calc_c_r(num_items, num_item_params, pi_matrix, base_rates, l, num_attr,
                 qmatrix, model_type)

  m2_stat <- n * ((t(empirical_marginal_probabilities - model_marginal_probabilities) %*% cr) %*% (empirical_marginal_probabilities - model_marginal_probabilities))

  se <- sqrt(diag(Mord(c(1:num_items), pi_matrix, base_rates)$bi) -
               c(Mord(c(1:num_items), pi_matrix, base_rates)$uni)^2)

  design_matrix <- calc_design_matrix(num_item_params, qmatrix, model_type)

  skills_missing <- skills(base_rates, l, qmatrix)

  patt <- calc_patt(qmatrix, l, skills_missing)

  jacobian <- calc_jacobian_matrix(num_items, num_item_params, pi_matrix,
                                   design_matrix, patt, base_rates, l, num_attr)

  df <- nrow(jacobian) - ncol(jacobian)

  sig <- 1 - stats::pchisq(m2_stat, df)

  rmsea <- rmsea_calc(x2 = m2_stat, df = df, n = n)
  ci <- rmsea_ci(x2 = m2_stat, df = df, n = n,
                 ci_lower = (1 - ci) / 2, ci_upper = ((1 - ci) / 2) + ci)

  difr <- stats::cor(data, use = "pairwise.complete.obs") - (Mord(c(1:num_items), pi_matrix, base_rates)$bi -
                                                               Mord(c(1:num_items), pi_matrix, base_rates)$uni %*%
                                                               t(Mord(c(1:num_items), pi_matrix, base_rates)$uni)) /
    (se %*% t(se))
  srmsr <- sqrt(sum((difr[lower.tri(difr)]) ^ 2 / (num_items * (num_items -
                                                                  1) / 2)))

  ci_lower <- ci[1]
  ci_upper <- ci[2]

  results <- tibble::tibble(m2 = as.numeric(m2_stat),
                            df = df,
                            pval = as.numeric(sig),
                            rmsea = as.numeric(rmsea),
                            ci_lower, ci_upper, srmsr)

  return(results)
}
