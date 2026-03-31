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

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- dcm2::calc_m2(data = fit_dat, struc_params = struc_params,
                           num_item_params = c(2, 2, 2, 2, 4, 2, 4, 2),
                           pi_matrix = pi_matrix,
                           qmatrix = data.frame(sample_data$q_matrix),
                           ci = .9,
                           link = "logit",
                           model_type = "LCDM",
                           allowed_profiles = allowed_profiles)

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)

  q <- sample_data$q_matrix |>
    dplyr::mutate(att_3 = c(1, 1, 0, 0, 1, 0, 0, 0)) |>
    as.data.frame()
  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat, Q = q, model = "logitGDINA",
                              rule = "GDINA2",
                              control = list(conv.type = "neg2LL"))
  )

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(3) |>
    dplyr::rename(att_1 = att1, att_2 = att2, att_3 = att3)

  dcm2_m2 <- dcm2::calc_m2(data = fit_dat, struc_params = struc_params,
                           num_item_params = c(4, 4, 2, 2, 7, 2, 4, 2),
                           pi_matrix = pi_matrix,
                           qmatrix = sample_data$q_matrix |>
                             dplyr::mutate(att_3 = c(1, 1, 0, 0, 1, 0, 0, 0)) |>
                             as.data.frame(),
                           ci = .9,
                           link = "logit",
                           model_type = "LCDM",
                           allowed_profiles = allowed_profiles)

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

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     num_item_params = c(2, 2, 2, 2, 4, 2, 4, 2),
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity",
                     model_type = "DINA",
                     allowed_profiles = allowed_profiles)

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

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     num_item_params = c(2, 2, 2, 2, 4, 2, 4, 2),
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity",
                     model_type = "DINO",
                     allowed_profiles = allowed_profiles)

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

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     num_item_params = c(2, 2, 2, 2, 3, 2, 3, 2),
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity",
                     model_type = "ACDM",
                     allowed_profiles = allowed_profiles)

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

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     num_item_params = c(2, 2, 2, 2, 3, 2, 3, 2),
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "logit",
                     model_type = "LLM",
                     allowed_profiles = allowed_profiles)

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

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     num_item_params = c(2, 2, 2, 2, 3, 2, 3, 2),
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "log",
                     model_type = "RRUM",
                     allowed_profiles = allowed_profiles)

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

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     num_item_params = c(2, 2, 2, 2, 4, 2, 4, 2),
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "identity",
                     model_type = "BUGDINO",
                     allowed_profiles = allowed_profiles)

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 for HDCM", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat, Q = q, model = "logitGDINA",
                              att.str = list(c(1, 2)),
                              control = list(conv.type = "neg2LL"))
  )

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::filter(!(att1 == 0 & att2 == 1)) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- calc_m2(data = fit_dat, struc_params = struc_params,
                     num_item_params = c(2, 2, 2, 2, 3, 2, 3, 2),
                     pi_matrix = pi_matrix,
                     qmatrix = data.frame(sample_data$q_matrix),
                     ci = .9,
                     link = "logit",
                     model_type = "LCDM",
                     allowed_profiles = allowed_profiles)

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 for C-RUM", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat, Q = q, model = "ACDM",
                              linkfunc = "logit",
                              control = list(conv.type = "neg2LL"))
  )

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- dcm2::calc_m2(data = fit_dat, struc_params = struc_params,
                           num_item_params = c(2, 2, 2, 2, 3, 2, 3, 2),
                           pi_matrix = pi_matrix,
                           qmatrix = data.frame(sample_data$q_matrix),
                           ci = .9,
                           link = "logit",
                           model_type = "CRUM",
                           allowed_profiles = allowed_profiles)

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

test_that("M2 for NC-RUM", {
  q <- data.frame(sample_data$q_matrix)
  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat, Q = q, model = "ACDM",
                              linkfunc = "log",
                              control = list(conv.type = "neg2LL"))
  )

  gdina_m2 <- GDINA::modelfit(gdina_mod)

  # calculate m2 with dcm2
  struc_params <- gdina_mod$struc.parm

  pi_matrix <- gdina_mod$LC.prob |>
    as.matrix() |>
    unname()

  allowed_profiles <- dcmstan::create_profiles(2) |>
    dplyr::rename(att_1 = att1, att_2 = att2)

  dcm2_m2 <- dcm2::calc_m2(data = fit_dat, struc_params = struc_params,
                           num_item_params = c(2, 2, 2, 2, 3, 2, 3, 2),
                           pi_matrix = pi_matrix,
                           qmatrix = data.frame(sample_data$q_matrix),
                           ci = .9,
                           link = "log",
                           model_type = "NCRUM",
                           allowed_profiles = allowed_profiles)

  expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
  expect_equal(dcm2_m2$df, gdina_m2$M2.df)
  expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
  expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
  expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
  expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
  expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
})

# nolint start
# test_that("M2 for NIDA", {
#   q <- data.frame(sample_data$q_matrix)
#   out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
#                                                          Q = q,
#                                                          model = "RRUM",
#                                                          control =
#                                                            list(conv.type =
#                                                                   "neg2LL")))
#
#   gdina_m2 <- GDINA::modelfit(gdina_mod)
#
#   # calculate m2 with dcm2
#   struc_params <- gdina_mod$struc.parm
#
#   pi_matrix <- gdina_mod$LC.prob |>
#     as.matrix() |>
#     unname()
#
#   allowed_profiles <- dcmstan::create_profiles(2) |>
#     dplyr::rename(att_1 = att1, att_2 = att2)
#
#   dcm2_m2 <- dcm2::calc_m2(data = fit_dat, struc_params = struc_params,
#                            num_item_params = c(2, 2, 2, 2, 4, 2, 4, 2),
#                            pi_matrix = pi_matrix,
#                            qmatrix = data.frame(sample_data$q_matrix),
#                            ci = .9,
#                            link = "logit",
#                            model_type = "NIDA",
#                            allowed_profiles = allowed_profiles)
#
#   expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
#   expect_equal(dcm2_m2$df, gdina_m2$M2.df)
#   expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
#   expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
#   expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
#   expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
#   expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
# })
#
# test_that("M2 for NIDO", {
#   q <- data.frame(sample_data$q_matrix)
#   out <- utils::capture.output(gdina_mod <- GDINA::GDINA(dat = fit_dat,
#                                                          Q = q,
#                                                          model = "RRUM",
#                                                          # linkfunc = "log",
#                                                          control =
#                                                            list(conv.type =
#                                                                   "neg2LL")))
#
#   gdina_m2 <- GDINA::modelfit(gdina_mod)
#
#   # calculate m2 with dcm2
#   struc_params <- gdina_mod$struc.parm
#
#   pi_matrix <- gdina_mod$LC.prob |>
#     as.matrix() |>
#     unname()
#
#   allowed_profiles <- dcmstan::create_profiles(2) |>
#     dplyr::rename(att_1 = att1, att_2 = att2)
#
#   dcm2_m2 <- dcm2::calc_m2(data = fit_dat, struc_params = struc_params,
#                            num_item_params = c(2, 2, 2, 2, 4, 2, 4, 2),
#                            pi_matrix = pi_matrix,
#                            qmatrix = data.frame(sample_data$q_matrix),
#                            ci = .9,
#                            link = "logit",
#                            model_type = "NIDO",
#                            allowed_profiles = allowed_profiles)
#
#   expect_equal(dcm2_m2$m2, gdina_m2$M2, tolerance = .01)
#   expect_equal(dcm2_m2$df, gdina_m2$M2.df)
#   expect_equal(dcm2_m2$pval, gdina_m2$M2.pvalue, tolerance = .01)
#   expect_equal(dcm2_m2$rmsea, gdina_m2$RMSEA2, tolerance = .01)
#   expect_equal(dcm2_m2$ci_lower, gdina_m2$RMSEA2.CI[1], tolerance = .01)
#   expect_equal(dcm2_m2$ci_upper, gdina_m2$RMSEA2.CI[2], tolerance = .01)
#   expect_equal(dcm2_m2$srmsr, gdina_m2$SRMSR, tolerance = .01)
# })
# nolint end
