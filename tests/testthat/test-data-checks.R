test_that("test check_ci", {
  output <- dcm2:::check_ci(.5)
  expect_equal(output, .5)

  err <- rlang::catch_cnd(dcm2:::check_ci("a"))
  expect_match(err$message, "must be a length one numeric vector.")

  err <- rlang::catch_cnd(dcm2:::check_ci(c(.3, .6)))
  expect_match(err$message, "must be a length one numeric vector.")

  err <- rlang::catch_cnd(dcm2:::check_ci(1.25))
  expect_match(err$message, "must be between 0 and 1 and not missing.")

  err <- rlang::catch_cnd(dcm2:::check_ci(-.15))
  expect_match(err$message, "must be between 0 and 1 and not missing.")

  err <- rlang::catch_cnd(dcm2:::check_ci(NaN))
  expect_match(err$message, "must be between 0 and 1 and not missing.")
})

test_that("test check_data", {
  fit_dat <- dcm2::sample_data$data %>%
    tidyr::pivot_wider(names_from = "item_id",
                       values_from = "score") %>%
    dplyr::select(-"resp_id") %>%
    as.matrix() %>%
    unname()

  q_matrix <- dcm2::sample_data$q_matrix

  err <- rlang::catch_cnd(dcm2:::check_data(fit_dat %>%
                                              as.data.frame(),
                                            q_matrix))
  expect_match(err$message, "must be a matrix.")

  err <- rlang::catch_cnd(dcm2:::check_data(tibble() %>%
                                              as.matrix(),
                                            q_matrix))
  expect_match(err$message, "must include data for at least one student.")

  err <- rlang::catch_cnd(dcm2:::check_data(fit_dat[, -8], q_matrix))
  expect_match(err$message, "The number of items in `data`")

  err <- rlang::catch_cnd(dcm2:::check_data(fit_dat %>%
                                              as.double() %>%
                                              matrix(nrow = 1000, ncol = 8),
                                            q_matrix))
  expect_match(err$message, "must be of type integer.")
})

test_that("test check_struc_params", {
  q_matrix <- dcm2::sample_data$q_matrix

  fit_dat <- dcm2::sample_data$data %>%
    tidyr::pivot_wider(names_from = "item_id",
                       values_from = "score") %>%
    dplyr::select(-"resp_id") %>%
    as.matrix() %>%
    unname()

  gdina_mod <- GDINA::GDINA(dat = fit_dat,
                            Q = data.frame(sample_data$q_matrix),
                            model = "logitGDINA",
                            control = list(conv.type = "neg2LL"))

  struc_params <- gdina_mod$struc.parm

  err <- rlang::catch_cnd(dcm2:::check_struc_params(struc_params %>%
                                                      as.character(),
                                                    q_matrix))
  expect_match(err$message, "The class of `struc_params` must be numeric.")

  err <- rlang::catch_cnd(dcm2:::check_struc_params(struc_params[-4], q_matrix))
  expect_match(err$message,
               "The length of `struc_params` does not match the number of")

  err <- rlang::catch_cnd(dcm2:::check_struc_params(struc_params %>%
                                                      as.integer(),
                                                    q_matrix))
  expect_match(err$message, "struc_params` must be of type double")
})

test_that("test check_pi_matrix", {
  q_matrix <- dcm2::sample_data$q_matrix

  fit_dat <- dcm2::sample_data$data %>%
               tidyr::pivot_wider(names_from = "item_id",
                                  values_from = "score") %>%
               dplyr::select(-"resp_id") %>%
               as.matrix() %>%
               unname()
  gdina_mod <- GDINA::GDINA(dat = fit_dat,
                            Q = data.frame(sample_data$q_matrix),
                            model = "logitGDINA",
                            control = list(conv.type = "neg2LL"))

  pi_matrix <- gdina_mod$LC.prob %>%
                 as.matrix() %>%
                 unname()

  err <- rlang::catch_cnd(dcm2:::check_pi_matrix(pi_matrix %>%
                                                   as.data.frame(),
                                                 q_matrix))
  expect_match(err$message, "must be a matrix.")

  err <- rlang::catch_cnd(dcm2:::check_pi_matrix(pi_matrix[-8, ],
                                                 q_matrix))
  expect_match(err$message,
               "The number of items specific by `pi_matrix` and `qmatrix` do")

  err <- rlang::catch_cnd(dcm2:::check_pi_matrix(pi_matrix[, -4],
                                                 q_matrix))
  expect_match(err$message,
               "The number of latent classes specified in `pi_matrix` and")

  err <- rlang::catch_cnd(dcm2:::check_pi_matrix(pi_matrix %>%
                                                   as.character() %>%
                                                   matrix(nrow = 8,
                                                          ncol = 4),
                                                 q_matrix))
  expect_match(err$message, "must be of type double.")
})

test_that("test check_qmatrix", {
  q_matrix <- dcm2::sample_data$q_matrix

  fit_dat <- dcm2::sample_data$data %>%
    tidyr::pivot_wider(names_from = "item_id",
                       values_from = "score") %>%
    dplyr::select(-"resp_id") %>%
    as.matrix() %>%
    unname()

  gdina_mod <- GDINA::GDINA(dat = fit_dat,
                            Q = data.frame(sample_data$q_matrix),
                            model = "logitGDINA",
                            control = list(conv.type = "neg2LL"))

  pi_matrix <- gdina_mod$LC.prob %>%
    as.matrix() %>%
    unname()

  q_matrix <- q_matrix %>%
    as.data.frame()

  err <- rlang::catch_cnd(dcm2:::check_qmatrix(q_matrix %>%
                                                 as.matrix(nrow = 8, ncol = 2),
                                               pi_matrix))
  expect_match(err$message, "must be a data frame.")

  err <- rlang::catch_cnd(dcm2:::check_qmatrix(q_matrix[-8, ], pi_matrix))
  expect_match(err$message,
               "The number of items specific by `pi_matrix` and `qmatrix` do")

  err <- rlang::catch_cnd(dcm2:::check_qmatrix(q_matrix[, -2] %>%
                                                 as.data.frame(),
                                               pi_matrix))
  expect_match(err$message,
               "The number of latent classes specified in `pi_matrix` and")
})
