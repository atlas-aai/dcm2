#' Calculate the M2
#'
#' @param data A data frame containing the raw data, where there is one row per
#' respondent and one column per item.
#' @param struc_params A vector containing the structural parameters of the
#' estimated model.
#' @param num_item_params A vector containing the number of estimated item
#' parameters for each of the items.
#' @param pi_matrix An item-by-class matrix containing the probability of a
#' correct response by members of each latent class.
#' @param qmatrix A data frame containing the Q-matrix.
#' @param ci The confidence interval for the RMSEA, computed from the M2.
#' @param link A character containing the link function.
#' @param model_type A character containing the type of measurement model
#' (e.g., `LCDM`) that was estimated.
#' @param allowed_profiles A tibble containing all of the allowable profiles.
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
#' @export
#'
#' @examples
#' allowed_profiles <- dcmstan::create_profiles(ncol(sample_data$q_matrix))
#' names(allowed_profiles) <- names(sample_data$q_matrix)
#'
#' fit_dat <- sample_data$data |>
#'              tidyr::pivot_wider(names_from = "item_id",
#'                                 values_from = "score") |>
#'              dplyr::select(-"resp_id") |>
#'              as.matrix() |>
#'              unname()
#' gdina_mod <- GDINA::GDINA(dat = fit_dat,
#'                           Q = data.frame(sample_data$q_matrix),
#'                           model = "logitGDINA",
#'                           control = list(conv.type = "neg2LL"))
#' struc_params <- gdina_mod$struc.parm
#' pi_matrix <- gdina_mod$LC.prob |>
#'                as.matrix() |>
#'                unname()
#' num_item_params <- c(2, 2, 2, 2, 4, 2, 4, 2)
#' calc_m2(data = fit_dat, struc_params, num_item_params, pi_matrix,
#'         qmatrix = data.frame(sample_data$q_matrix), ci = 0.9, link = "logit",
#'         model_type = "LCDM", allowed_profiles = allowed_profiles)
#'
calc_m2 <- function(
  data,
  struc_params,
  num_item_params,
  pi_matrix,
  qmatrix,
  ci = 0.9,
  link = "logit",
  model_type = c("LCDM", "GDINA", "ACDM", "LLM", "RRUM", "DINO", "DINA",
                 "BUGDINO", "CRUM", "NCRUM", "NIDA", "NIDO"),
  allowed_profiles) {

  # To-do
  ## add additional models

  # data checks
  check_data(data, qmatrix)
  check_pi_matrix(pi_matrix, qmatrix)
  check_struc_params(struc_params, pi_matrix)
  check_qmatrix(qmatrix, pi_matrix)
  check_allowed_profiles(allowed_profiles, pi_matrix)
  ci <- check_ci(ci)
  model_type <- rlang::arg_match(model_type)

  model_type <- ifelse(model_type == "GDINA", "LCDM", model_type)
  # model_type <- ifelse(model_type == "NIDA", "RRUM", model_type)
  # model_type <- ifelse(model_type == "NIDO", "LCDM", model_type)

  att_names <- names(qmatrix)

  num_items <- nrow(qmatrix)
  num_attr <- ncol(qmatrix)

  n <- nrow(data)
  l <- ncol(pi_matrix)

  emp_marginal_probabilities <- calc_emp_marginal_prob(data, n)

  hierarchy <- 2^ncol(qmatrix) != nrow(allowed_profiles)

  base_rates <- t(as.matrix(struc_params))
  if (!hierarchy) {
    colnames(base_rates) <- att_profile(num_attr)
  } else {
    colnames(base_rates) <- allowed_profiles |>
      tidyr::unite(col = "prof_name", sep = "") |>
      dplyr::pull(.data$prof_name)
  }

  model_marginal_probabilities <- calc_mod_marginal_prob(num_items, pi_matrix,
                                                         base_rates)

  cr <- calc_c_r(num_items, num_item_params, pi_matrix, base_rates, l, num_attr,
                 qmatrix, model_type, link, hierarchy, allowed_profiles)

  m2_stat <- n * ((t(emp_marginal_probabilities -
                       model_marginal_probabilities) %*% cr) %*%
                    (emp_marginal_probabilities - model_marginal_probabilities))

  se <- sqrt(diag(Mord(c(1:num_items), pi_matrix, base_rates)$bi) -
               c(Mord(c(1:num_items), pi_matrix, base_rates)$uni)^2)

  design_matrix <- calc_design_matrix(num_item_params, qmatrix, model_type,
                                      hierarchy, allowed_profiles)

  skills_missing <- skills(base_rates, l, qmatrix)

  patt <- calc_patt(qmatrix, l, skills_missing, num_item_params)

  jacobian <- calc_jacobian_matrix(num_items, num_item_params, pi_matrix,
                                   design_matrix, patt, base_rates, l, num_attr,
                                   link, model_type)

  df <- nrow(jacobian) - ncol(jacobian)

  sig <- 1 - stats::pchisq(m2_stat, df)

  rmsea <- rmsea_calc(x2 = m2_stat, df = df, n = n)
  ci <- rmsea_ci(x2 = m2_stat, df = df, n = n,
                 ci_lower = (1 - ci) / 2, ci_upper = ((1 - ci) / 2) + ci)

  difr <- stats::cor(data, use = "pairwise.complete.obs") -
    (Mord(c(1:num_items), pi_matrix, base_rates)$bi -
       Mord(c(1:num_items), pi_matrix, base_rates)$uni %*%
         t(Mord(c(1:num_items), pi_matrix, base_rates)$uni)) / (se %*% t(se))
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
