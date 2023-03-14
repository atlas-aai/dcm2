check_data <- function(x) {
  if (!("data.frame" %in% class(x))) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!tibble::is_tibble(x)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

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
  if (any(class(data) != c("matrix", "array"))) {
    stop("`data` must be a matrix.",
         call. = FALSE)
  }

  if (nrow(data) < 1) {
    stop("`data` must include data for at least one student.",
         call. = FALSE)
  }

  if (ncol(data) != nrow(qmatrix)) {
    stop("The number of items in `data` (i.e., the number of columns) must equal the number of items in the Q-matrix.",
         call. = FALSE)
  }

  if (typeof(data) != "integer") {
    stop("`data` must be of type integer.",
         call. = FALSE)
  }
}

check_struc_params <- function(struc_params, qmatrix) {
  if (class(struc_params) != "numeric") {
    stop("The class of `struc_params` must be numeric.",
         call. = FALSE)
  }

  if (length(struc_params) != 2^ncol(qmatrix)) {
    stop("The length of `struc_params` does not match the number of latent classes indicated by `qmatrix`.",
         call. = FALSE)
  }

  if (typeof(struc_params) != "double") {
    stop("`struc_params` must be of type double",
         call. = FALSE)
  }
}

check_pi_matrix <- function(pi_matrix, qmatrix) {
  if (any(class(pi_matrix) != c("matrix", "array"))) {
    stop("`pi_matrix` must be a matrix.",
         call. = FALSE)
  }

  if (nrow(pi_matrix) != nrow(qmatrix)) {
    stop("The number of items specific by `pi_matrix` and `qmatrix` do not match.",
         call. = FALSE)
  }

  if (ncol(pi_matrix) != 2^ncol(qmatrix)) {
    stop("The number of latent classes specified in `pi_matrix` and `qmatrix` do not match.",
         call. = FALSE)
  }

  if (typeof(pi_matrix) != "double") {
    stop("`pi_matrix` must be of type double.",
         call. = FALSE)
  }
}

check_qmatrix <- function(qmatrix, pi_matrix) {
  if (class(qmatrix) != "data.frame") {
    stop("`pi_matrix` must be a data frame.",
         call. = FALSE)
  }

  if (nrow(pi_matrix) != nrow(qmatrix)) {
    stop("The number of items specific by `pi_matrix` and `qmatrix` do not match.",
         call. = FALSE)
  }

  if (ncol(pi_matrix) != 2^ncol(qmatrix)) {
    stop("The number of latent classes specified in `pi_matrix` and `qmatrix` do not match.",
         call. = FALSE)
  }
}

check_num_item_params <- function(num_item_params, qmatrix,
                                  exp_num_item_params) {
  if (class(num_item_params) != "numeric") {
    stop("`num_item_params` must be numeric.",
         call. = FALSE)
  }

  if (length(num_item_params) != nrow(qmatrix)) {
    stop("The number of items specific by `num_item_params` and `qmatrix` do not match.",
         call. = FALSE)
  }

  if (!is.vector(num_item_params)) {
    stop("`num_item_params` must be a vector.",
         call. = FALSE)
  }

  if (any(num_item_params != exp_num_item_params)) {
    stop("The number of item parameters specified in `num_item_params`, `qmatrix`, and `model_type` do not match.",
         call. = FALSE)
  }
}
