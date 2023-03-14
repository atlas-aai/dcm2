test_that("M2 for LCDM", {


  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(data$q_matrix),
                              model = "logitGDINA",
                              control=list(conv.type="neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dat <- data

  data <- data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-resp_id) %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = data, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(dat$q_matrix),
                     num_item_params = 2 ^ rowSums(data.frame(dat$q_matrix)),
                     ci = .9,
                     link = "logit", model_type = "LCDM")

  expect_equal(dcm2_m2$m2, gdina_m2$M2,
               tolerance = max(.01, floor(dcm2_m2$m2 / 100)))
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .001)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .001)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .001)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .001)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .001)
})

test_that("M2 works - DINA", {
  sample_size <- 1000
  test_length <- 4
  prevalence <- 0.5
  discrimination <- 2
  association <- 0
  attributes <- 2
  set.seed(1106)

  data <- generate_data(sample_size = sample_size,
                        test_length = test_length,
                        prevalence = prevalence,
                        discrimination = discrimination,
                        association = association,
                        attributes = attributes)
  possible_prof <- as_binary(attributes)

  data$data <- data$data %>%
    dplyr::ungroup()

  fit_dat <- as.matrix(data$data %>%
                         tidyr:: pivot_wider(names_from = "item_id",
                                             values_from = "score") %>%
                         dplyr::select(-.data$resp_id))

  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(data$q_matrix),
                              model = "DINA",
                              control=list(conv.type="neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dat <- data

  data <- data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-resp_id) %>%
    as.matrix() %>%
    unname()

  lcdmr_m2 <- m2_fit(data = data, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(dat$q_matrix),
                     num_item_params = rep(2, test_length * attributes),
                     ci = .9, link = "logit", model_type = "DINA")

  expect_equivalent(lcdmr_m2$m2, gdina_m2$M2,
                    tol = max(.01, floor(lcdmr_m2$m2 / 100)))
  expect_equivalent(lcdmr_m2$df, gdina_m2$M2.df)
  expect_equivalent(lcdmr_m2$pval, gdina_m2$M2.pvalue, tol = .001)
  expect_equivalent(lcdmr_m2$rmsea, gdina_m2$RMSEA2, tol = .001)
  expect_equivalent(lcdmr_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tol = .001)
  expect_equivalent(lcdmr_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tol = .001)
  expect_equivalent(lcdmr_m2$srmsr, gdina_m2$SRMSR, tol = .001)
})

test_that("M2 works - DINO", {
  sample_size <- 1000
  test_length <- 4
  prevalence <- 0.5
  discrimination <- 2
  association <- 0
  attributes <- 2
  set.seed(1106)

  data <- generate_data(sample_size = sample_size,
                        test_length = test_length,
                        prevalence = prevalence,
                        discrimination = discrimination,
                        association = association,
                        attributes = attributes)
  possible_prof <- as_binary(attributes)

  data$data <- data$data %>%
    dplyr::ungroup()

  fit_dat <- as.matrix(data$data %>%
                         tidyr::pivot_wider(names_from = "item_id",
                                            values_from = "score") %>%
                         dplyr::select(-.data$resp_id))

  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(data$q_matrix),
                              model = "DINO",
                              control=list(conv.type="neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dat <- data

  data <- data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-resp_id) %>%
    as.matrix() %>%
    unname()

  lcdmr_m2 <- m2_fit(data = data, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(dat$q_matrix),
                     num_item_params = rep(2, test_length * attributes),
                     ci = .9, link = "logit", model_type = "DINO")

  expect_equivalent(lcdmr_m2$m2, gdina_m2$M2,
                    tol = max(.01, floor(lcdmr_m2$m2 / 100)))
  expect_equivalent(lcdmr_m2$df, gdina_m2$M2.df)
  expect_equivalent(lcdmr_m2$pval, gdina_m2$M2.pvalue, tol = .0015)
  expect_equivalent(lcdmr_m2$rmsea, gdina_m2$RMSEA2, tol = .001)
  expect_equivalent(lcdmr_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tol = .001)
  expect_equivalent(lcdmr_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tol = .001)
  expect_equivalent(lcdmr_m2$srmsr, gdina_m2$SRMSR, tol = .001)
})

test_that("M2 works - ACDM", {
  sample_size <- 1000
  test_length <- 4
  prevalence <- 0.5
  discrimination <- 2
  association <- 0
  attributes <- 2
  set.seed(1106)

  data <- generate_data(sample_size = sample_size,
                        test_length = test_length,
                        prevalence = prevalence,
                        discrimination = discrimination,
                        association = association,
                        attributes = attributes)
  possible_prof <- as_binary(attributes)

  data$data <- data$data %>%
    dplyr::ungroup()

  fit_dat <- as.matrix(data$data %>%
                         tidyr::pivot_wider(names_from = "item_id",
                                            values_from = "score") %>%
                         dplyr::select(-.data$resp_id))

  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(data$q_matrix),
                              model = "ACDM",
                              control=list(conv.type="neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dat <- data

  data <- data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-resp_id) %>%
    as.matrix() %>%
    unname()

  lcdmr_m2 <- m2_fit(data = data, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(dat$q_matrix),
                     num_item_params = c(2, 2, 2, 2, 3, 3, 2, 3),
                     ci = .9, link = "logit", model_type = "ACDM")

  expect_equivalent(lcdmr_m2$m2, gdina_m2$M2,
                    tol = max(.01, floor(lcdmr_m2$m2 / 100)))
  expect_equivalent(lcdmr_m2$df, gdina_m2$M2.df)
  expect_equivalent(lcdmr_m2$pval, gdina_m2$M2.pvalue, tol = .01)
  expect_equivalent(lcdmr_m2$rmsea, gdina_m2$RMSEA2, tol = .001)
  expect_equivalent(lcdmr_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tol = .001)
  expect_equivalent(lcdmr_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tol = .001)
  expect_equivalent(lcdmr_m2$srmsr, gdina_m2$SRMSR, tol = .001)
})

test_that("M2 works - LLM", {
  sample_size <- 1000
  test_length <- 4
  prevalence <- 0.5
  discrimination <- 2
  association <- 0
  attributes <- 2
  set.seed(1106)

  data <- generate_data(sample_size = sample_size,
                        test_length = test_length,
                        prevalence = prevalence,
                        discrimination = discrimination,
                        association = association,
                        attributes = attributes)
  possible_prof <- as_binary(attributes)

  data$data <- data$data %>%
    dplyr::ungroup()

  fit_dat <- as.matrix(data$data %>%
                         tidyr::pivot_wider(names_from = "item_id",
                                            values_from = "score") %>%
                         dplyr::select(-.data$resp_id))

  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(data$q_matrix),
                              model = "LLM",
                              control=list(conv.type="neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dat <- data

  data <- data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-resp_id) %>%
    as.matrix() %>%
    unname()

  lcdmr_m2 <- m2_fit(data = data, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(dat$q_matrix),
                     num_item_params = c(2, 2, 2, 2, 3, 3, 2, 3),
                     ci = .9, link = "logit", model_type = "LLM")

  expect_equivalent(lcdmr_m2$m2, gdina_m2$M2,
                    tol = max(.01, floor(lcdmr_m2$m2 / 100)))
  expect_equivalent(lcdmr_m2$df, gdina_m2$M2.df)
  expect_equivalent(lcdmr_m2$pval, gdina_m2$M2.pvalue, tol = .0015)
  expect_equivalent(lcdmr_m2$rmsea, gdina_m2$RMSEA2, tol = .001)
  expect_equivalent(lcdmr_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tol = .001)
  expect_equivalent(lcdmr_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tol = .001)
  expect_equivalent(lcdmr_m2$srmsr, gdina_m2$SRMSR, tol = .001)
})

test_that("M2 works - RRUM", {
  sample_size <- 1000
  test_length <- 4
  prevalence <- 0.5
  discrimination <- 2
  association <- 0
  attributes <- 2
  set.seed(1106)

  data <- generate_data(sample_size = sample_size,
                        test_length = test_length,
                        prevalence = prevalence,
                        discrimination = discrimination,
                        association = association,
                        attributes = attributes)
  possible_prof <- as_binary(attributes)

  data$data <- data$data %>%
    dplyr::ungroup()

  fit_dat <- as.matrix(data$data %>%
                         tidyr::pivot_wider(names_from = "item_id",
                                            values_from = "score") %>%
                         dplyr::select(-.data$resp_id))

  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(data$q_matrix),
                              model = "RRUM",
                              control=list(conv.type="neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dat <- data

  data <- data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-resp_id) %>%
    as.matrix() %>%
    unname()

  lcdmr_m2 <- m2_fit(data = data, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(dat$q_matrix),
                     num_item_params = c(2, 2, 2, 2, 3, 3, 2, 3),
                     ci = .9, link = "logit", model_type = "RRUM")

  expect_equivalent(lcdmr_m2$m2, gdina_m2$M2,
                    tol = max(.01, floor(lcdmr_m2$m2 / 100)))
  expect_equivalent(lcdmr_m2$df, gdina_m2$M2.df)
  expect_equivalent(lcdmr_m2$pval, gdina_m2$M2.pvalue, tol = .01)
  expect_equivalent(lcdmr_m2$rmsea, gdina_m2$RMSEA2, tol = .001)
  expect_equivalent(lcdmr_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tol = .001)
  expect_equivalent(lcdmr_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tol = .001)
  expect_equivalent(lcdmr_m2$srmsr, gdina_m2$SRMSR, tol = .001)
})

test_that("M2 works - BUGDINO", {
  sample_size <- 1000
  test_length <- 4
  prevalence <- 0.5
  discrimination <- 2
  association <- 0
  attributes <- 2
  set.seed(1106)

  data <- generate_data(sample_size = sample_size,
                        test_length = test_length,
                        prevalence = prevalence,
                        discrimination = discrimination,
                        association = association,
                        attributes = attributes)
  possible_prof <- as_binary(attributes)

  data$data <- data$data %>%
    dplyr::ungroup()

  fit_dat <- as.matrix(data$data %>%
                         tidyr::pivot_wider(names_from = "item_id",
                                            values_from = "score") %>%
                         dplyr::select(-.data$resp_id))

  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(data$q_matrix),
                              model = "BUGDINO",
                              control=list(conv.type="neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dat <- data

  data <- data$data %>%
    tidyr::pivot_wider(names_from = "item_id", values_from = "score") %>%
    dplyr::select(-resp_id) %>%
    as.matrix() %>%
    unname()

  lcdmr_m2 <- m2_fit(data = data, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(dat$q_matrix),
                     num_item_params = rep(2, test_length * attributes),
                     ci = .9, link = "logit", model_type = "BUGDINO")

  expect_equivalent(lcdmr_m2$m2, gdina_m2$M2,
                    tol = max(.01, floor(lcdmr_m2$m2 / 100)))
  expect_equivalent(lcdmr_m2$df, gdina_m2$M2.df)
  expect_equivalent(lcdmr_m2$pval, gdina_m2$M2.pvalue, tol = .0015)
  expect_equivalent(lcdmr_m2$rmsea, gdina_m2$RMSEA2, tol = .001)
  expect_equivalent(lcdmr_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tol = .001)
  expect_equivalent(lcdmr_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tol = .001)
  expect_equivalent(lcdmr_m2$srmsr, gdina_m2$SRMSR, tol = .001)
})
