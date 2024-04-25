sample_size <- 1000
test_length <- 2
prevalence <- .5
discrimination <- 1
association <- 0
attributes <- 1

resp_trait <- stats::rnorm(n = sample_size, mean = 0, sd = 1)
bk0 <- c(association, stats::runif(n = attributes - 1L, min = 0, max = 2))
bk1 <- c(prevalence, stats::runif(n = attributes - 1L, min = 0.2, max = 0.8))
profiles <- purrr::map_dfr(resp_trait,
                           function(theta, bk0, bk1) {
                             purrr::map2_int(bk0, bk1,
                                             function(x, y, theta) {
                                               stats::runif(1) >
                                                 (1 - stats::pnorm(x * (theta -
                                                                          y)))
                                             },
                                             theta = theta) %>%
                               rlang::set_names(glue::glue("att_",
                                                           "{1:length(.)}")) %>%
                               tibble::enframe() %>%
                               tidyr::pivot_wider(names_from = "name",
                                                  values_from = "value")
                           },
                           bk0 = bk0,
                           bk1 = bk1) %>%
  tibble::rowid_to_column(var = "resp_id")

attr_names <- as.vector(glue::glue("dplyr::desc(att_{1:attributes})"))

# generate Q-matrix -----
all_combo <- rep(list(c(0L, 1L)), attributes) %>%
  rlang::set_names(glue::glue("att_{seq_len(attributes)}")) %>%
  expand.grid() %>%
  dplyr::mutate(total = rowSums(.)) %>%
  dplyr::select("total", dplyr::everything()) %>%
  dplyr::arrange(.data$total, !!! rlang::parse_exprs(attr_names)) %>%
  dplyr::filter(dplyr::between(.data$total, 1, 2)) %>%
  dplyr::mutate(prob = dplyr::case_when(.data$total == 1 ~ 0.500,
                                        TRUE ~ 0.5 / (attributes - 1)))
q_matrix <- purrr::map_dfr(seq_len(test_length),
                           function(chunk, all_combo, att) {
                             if (chunk %in% c(1, 2)) {
                               ret_chk <- diag(x = 1L, nrow = att,
                                               ncol = att) %>%
                                 tibble::as_tibble(.name_repair =
                                                     ~glue::glue("att_{1:att}"))
                               return(ret_chk)
                             }

                             ret_chk <-
                               purrr::map_dfr(glue::glue("att_{1:att}"),
                                              function(var, all_combo) {
                                                dplyr::filter(all_combo,
                                                              !!dplyr::sym(var)
                                                              == 1) %>%
                                                  dplyr::sample_n(size = 1,
                                                                  replace =
                                                                    FALSE,
                                                                  weight =
                                                                    .data$prob)
                                              },
                                              all_combo = all_combo) %>%
                               dplyr::select(dplyr::starts_with("att"))
                             return(ret_chk)
                           },
                           all_combo = all_combo, att = attributes)

# generate item parameters -----
if (attributes == 1) {
  needed_params <-
    modelr::model_matrix(q_matrix, stats::as.formula(paste0("~ .^", 2))) %>%
    tibble::rowid_to_column(var = "item_id") %>%
    dplyr::select(dplyr::where(function(x) !all(x == 0))) %>%
    rlang::set_names(nm = vctrs::vec_as_names(names(.), repair = "universal",
                                              quiet = TRUE)) %>%
    dplyr::rename(intercept = ".Intercept.") %>%
    dplyr::rename_with(~ stringr::str_replace_all(., stringr::fixed("."), "__"))
} else {
  needed_params <-
    modelr::model_matrix(q_matrix, stats::as.formula(paste0("~ .^",
                                                            attributes))) %>%
    tibble::rowid_to_column(var = "item_id") %>%
    dplyr::select(dplyr::where(function(x) !all(x == 0))) %>%
    rlang::set_names(nm = vctrs::vec_as_names(names(.), repair = "universal",
                                              quiet = TRUE)) %>%
    dplyr::rename(intercept = ".Intercept.") %>%
    dplyr::rename_with(~ stringr::str_replace_all(., stringr::fixed("."), "__"))
}
param_names <- colnames(needed_params)
needed_params <- needed_params %>%
  tidyr::pivot_longer(cols = -"item_id", names_to = "param",
                      values_to = "valid") %>%
  dplyr::filter(.data$valid == 1)

intercepts <- needed_params %>%
  dplyr::filter(.data$param == "intercept") %>%
  dplyr::mutate(prob = stats::runif(n = dplyr::n(), min = 0.1, max = 0.35),
                value = vapply(.data$prob, logit, double(1))) %>%
  dplyr::select("item_id", intercept = "value")

effects <- needed_params %>%
  dplyr::filter(.data$param != "intercept") %>%
  tidyr::nest(item_params = -"item_id") %>%
  dplyr::mutate(params =
                  purrr::map(.data$item_params,
                             function(x, dis, att) {
                               if (nrow(x) == 1) {
                                 effect <- x %>%
                                   dplyr::mutate(value = stats::rnorm(1,
                                                                      mean =
                                                                        dis,
                                                                      sd =
                                                                        0.25))
                               } else {
                                 effect <- x %>%
                                   dplyr::mutate(mef = dplyr::case_when(!stringr::str_detect(.data$param, "__") ~ truncnorm::rtruncnorm(
                                                  dplyr::n(),
                                                  a = 0,
                                                  mean = dis / 1.5,
                                                  sd = sqrt(1 / 36))),
                                                 int = dplyr::case_when(stringr::str_detect(.data$param, "__") ~ truncnorm::rtruncnorm(
                                                  dplyr::n(),
                                                  a = -1 * min(.data$mef,
                                                               na.rm =  TRUE),
                                                  mean = dis / 1.5,
                                                  sd = sqrt(1 / 36))),
                                                 value =
                                                   dplyr::coalesce(.data$mef,
                                                                   .data$int)
                                                 ) %>%
                                   dplyr::select("param", "valid", "value")
                               }
                               ret_frame <- effect %>%
                                 dplyr::select("param", "value") %>%
                                 tidyr::pivot_wider(names_from = "param",
                                                    values_from = "value")
                               return(ret_frame)
                             },
                             dis = discrimination, att = attributes)) %>%
  dplyr::select("item_id", "params") %>%
  tidyr::unnest("params")

item_params <- dplyr::full_join(intercepts, effects, by = "item_id") %>%
  dplyr::select({{param_names}})

# generate response data -----
if (attributes == 1) {
  resp_data <- profiles %>%
    dplyr::select(-"resp_id") %>%
    modelr::model_matrix(stats::as.formula(paste0("~ .^", 2))) %>%
    tibble::rowid_to_column(var = "resp_id") %>%
    dplyr::select(dplyr::where(function(x) !all(x == 0))) %>%
    rlang::set_names(nm = vctrs::vec_as_names(names(.), repair = "universal",
                                              quiet = TRUE)) %>%
    dplyr::rename(intercept = ".Intercept.") %>%
    dplyr::rename_with(~ stringr::str_replace_all(., stringr::fixed("."),
                                                  "__")) %>%
    tidyr::pivot_longer(cols = -"resp_id", names_to = "param",
                        values_to = "mastery") %>%
    dplyr::left_join(item_params %>%
                       tidyr::pivot_longer(cols = -"item_id",
                                           names_to = "param",
                                           values_to = "item_param"),
                     by = "param", multiple = "all") %>%
    dplyr::filter(!is.na(.data$item_param)) %>%
    dplyr::select("resp_id", "item_id", "param", "mastery", "item_param") %>%
    dplyr::arrange(.data$resp_id, .data$item_id) %>%
    dplyr::mutate(param_value = .data$mastery * .data$item_param) %>%
    dplyr::group_by(.data$resp_id, .data$item_id) %>%
    dplyr::summarize(log_odds = sum(.data$param_value), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prob = purrr::map_dbl(.data$log_odds, inv_logit),
                  rand = stats::runif(dplyr::n(), min = 0, max = 1),
                  score = dplyr::case_when(.data$rand <= .data$prob ~ 1L,
                                           TRUE ~ 0L)) %>%
    dplyr::select("resp_id", "item_id", "score")
} else {
  resp_data <- profiles %>%
    dplyr::select(-"resp_id") %>%
    modelr::model_matrix(stats::as.formula(paste0("~ .^", attributes))) %>%
    tibble::rowid_to_column(var = "resp_id") %>%
    dplyr::select(dplyr::where(function(x) !all(x == 0))) %>%
    rlang::set_names(nm = vctrs::vec_as_names(names(.), repair = "universal",
                                              quiet = TRUE)) %>%
    dplyr::rename(intercept = ".Intercept.") %>%
    dplyr::rename_with(~ stringr::str_replace_all(., stringr::fixed("."),
                                                  "__")) %>%
    tidyr::pivot_longer(cols = -"resp_id", names_to = "param",
                        values_to = "mastery") %>%
    dplyr::left_join(item_params %>%
                       tidyr::pivot_longer(cols = -"item_id",
                                           names_to = "param",
                                           values_to = "item_param"),
                     by = "param", multiple = "all") %>%
    dplyr::filter(!is.na(.data$item_param)) %>%
    dplyr::select("resp_id", "item_id", "param", "mastery", "item_param") %>%
    dplyr::arrange(.data$resp_id, .data$item_id) %>%
    dplyr::mutate(param_value = .data$mastery * .data$item_param) %>%
    dplyr::group_by(.data$resp_id, .data$item_id) %>%
    dplyr::summarize(log_odds = sum(.data$param_value), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prob = purrr::map_dbl(.data$log_odds, inv_logit),
                  rand = stats::runif(dplyr::n(), min = 0, max = 1),
                  score = dplyr::case_when(.data$rand <= .data$prob ~ 1L,
                                           TRUE ~ 0L)) %>%
    dplyr::select("resp_id", "item_id", "score")
}

data_att1 <- list(resp_profiles = profiles,
                  q_matrix = q_matrix,
                  item_params = item_params,
                  data = resp_data)

usethis::use_data(data_att1, overwrite = TRUE)
