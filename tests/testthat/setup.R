sample_size <- 1000
test_length <- 4
prevalence <- 0.5
discrimination <- 3
association <- 0.5
attributes <- 2
set.seed(1234)

data <- generate_data(sample_size = sample_size,
                      test_length = test_length,
                      prevalence = prevalence,
                      discrimination = discrimination,
                      association = association,
                      attributes = attributes)
possible_prof <- as_binary(attributes)

data$data <- data$data %>%
  dplyr::ungroup()

fit_dat <- data$data %>%
  tidyr::pivot_wider(names_from = "item_id",
                     values_from = "score") %>%
  dplyr::select(-"resp_id") %>%
  as.matrix() %>%
  unname()

data_att1 <- generate_data(sample_size = 1000, test_length = 2, prevalence = .5,
                           discrimination = 1, association = 0, attributes = 1)
