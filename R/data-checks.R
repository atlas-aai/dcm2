check_ci <- function(x) {
  if (length(x) != 1 || !is.numeric(x)) {
    stop("`ci` must be a length one numeric vector.",
         call. = FALSE)
  }

  if (x <= 0 || x >= 1 || is.na(x)) {
    stop("`ci` must be between 0 and 1 and not missing.",
         call. = FALSE)
  } else {
    x
  }
}

check_data <- function(data, qmatrix) {
  if (!is.matrix(data)) {
    stop("`data` must be a matrix.",
         call. = FALSE)
  }

  if (nrow(data) < 1) {
    stop("`data` must include data for at least one student.",
         call. = FALSE)
  }

  if (ncol(data) != nrow(qmatrix)) {
    stop(paste("The number of items in `data` (i.e., the number of columns)",
               "must equal the number of items in the Q-matrix."),
         call. = FALSE)
  }

  if (!is.integer(data)) {
    stop("`data` must be of type integer.",
         call. = FALSE)
  }

  if (missing_data_present) {
    stop("The M2 statistic is unstable when missing data are present.",
         call. = FALSE)
  }

  missing_data_present <- any(is.na(data))
}

check_struc_params <- function(struc_params, pi_matrix) {
  if (!is.numeric(struc_params)) {
    stop("The class of `struc_params` must be numeric.",
         call. = FALSE)
  }

  if (!is.double(struc_params)) {
    stop("`struc_params` must be of type double",
         call. = FALSE)
  }

  if (length(struc_params) != ncol(pi_matrix)) {
    stop(paste("The length of `struc_params` does not match the number of",
               "latent classes indicated by `pi_matrix`."),
         call. = FALSE)
  }
}

check_pi_matrix <- function(pi_matrix, qmatrix) {
  if (!is.matrix(pi_matrix)) {
    stop("`pi_matrix` must be a matrix.",
         call. = FALSE)
  }

  if (!is.double(pi_matrix)) {
    stop("`pi_matrix` must be of type double.",
         call. = FALSE)
  }

  if (nrow(pi_matrix) != nrow(qmatrix)) {
    stop(paste("The number of items specific by `pi_matrix` and `qmatrix` do",
               "not match."),
         call. = FALSE)
  }
}

check_qmatrix <- function(qmatrix, pi_matrix) {
  if (!is.data.frame(qmatrix)) {
    stop("`qmatrix` must be a data frame.",
         call. = FALSE)
  }

  if (nrow(pi_matrix) != nrow(qmatrix)) {
    stop(paste("The number of items specific by `pi_matrix` and `qmatrix`",
               "do not match."),
         call. = FALSE)
  }

  q_matrix_values <- qmatrix |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "att",
                        values_to = "measured") |>
    dplyr::distinct(.data$measured) |>
    dplyr::pull(.data$measured)

  if (any(!(q_matrix_values %in% c(0, 1)))) {
    stop(paste("The entries of `qmatrix` must be 0 or 1."),
         call. = FALSE)
  }
}

check_allowed_profiles <- function(allowed_profiles, pi_matrix) {
  if (nrow(allowed_profiles) != ncol(pi_matrix)) {
    stop(paste("The number of allowable profiles (rows in `allowed_profiles`)",
               "should equal the number of latent classes from the estimated",
               "model (columns in `pi_matrix`)."),
         call. = FALSE)
  }
}
