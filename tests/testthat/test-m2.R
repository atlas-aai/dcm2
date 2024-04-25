test_that("M2 for LCDM", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
                                                         Q = q,
                                                         model = "logitGDINA",
                                                         control =
                                                           list(conv.type =
                                                                "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "logit", model_type = "LCDM")

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 works - DINA", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
                                                         Q = q,
                                                         model = "DINA",
                                                         control =
                                                           list(conv.type =
                                                                "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity", model_type = "DINA")

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 works - DINO", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
                                                         Q = q,
                                                         model = "DINO",
                                                         control =
                                                           list(conv.type =
                                                                "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity", model_type = "DINO")

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 works - ACDM", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
                                                         Q = q,
                                                         model = "ACDM",
                                                         control =
                                                           list(conv.type =
                                                                "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity", model_type = "ACDM")

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 works - LLM", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
                                                         Q = q,
                                                         model = "LLM",
                                                         control =
                                                           list(conv.type =
                                                                "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "logit", model_type = "LLM")

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 works - RRUM", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
                                                         Q = q,
                                                         model = "RRUM",
                                                         control =
                                                           list(conv.type =
                                                                "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "log", model_type = "RRUM")

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 works - BUGDINO", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
                                                         Q = q,
                                                         model = "BUGDINO",
                                                         control =
                                                           list(conv.type =
                                                                "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity", model_type = "BUGDINO")

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})
