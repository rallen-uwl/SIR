#' Combine Populations
#'
#' This function combines the data.frames provided in the list, while adding
#' a "path" column to distinguish the data.frame data.  This can be used to
#' combine simulation runs to generate a 3D plot with multiple paths.
#'
#' @param solution_list list of data.frames to combine into a single data.frame
#' @return combined data.frame
#' @export
combine_populations <- function(solution = NULL, type = "reinfection") {
  stopifnot(!is.null(solution))

  if (type == "basic") {
    solution <- dplyr::mutate(solution, S = S1 + S2, I = Is + Ia)
  } else if (type == "complex") {
    solution <- dplyr::mutate(solution, S = S1 + S2, I = A1 + A2 + Is)
  } else {
    solution <- dplyr::mutate(solution, S = S1 + S2, I = A1 + A2 + I1 + I2, R = R1 + R2)
  }
  
  return (s)
}

#' Check Distance from Equilibrium
#'
#' This function computes how close the provided data is to an equilibrium
#'
#' @param parameters system parameters
#' @param data value of each population
#' @return data.frame of maximum error of each derivative in the reinfection
#'         model, each derivative evaluated at the data, and the derivative of
#'         the total population
#' @export
check_equilibrium <- function(parameters, data) {
  end_state <- as.numeric(unlist(reinfection_model(NULL, parameters, data)))

  c(
    L2_error = sqrt(sum((end_state)^2)), dS1 = end_state[1],
    dS2 = end_state[2], dA1 = end_state[3], dA2 = end_state[4],
    dI1 = end_state[5], dI2 = end_state[6], dR1 = end_state[7],
    dR2 = end_state[8], dN = sum(end_state)
  )
}

#' Compute Equilibrium Error
#'
#' This function computes the max error of an point to being an equilibrium
#'
#' @param parameters system parameters
#' @param data value of each population
#' @return maximum of absolute value of each derivative evaluated at data
#' @export
compute_equilibrium_error <- function(parameters, data) {
  check_equilibrium(parameters, data)[1]
}

#' Generate Initial Conditions
#'
#' This function generates an initial condition of the form
#' (S1, S2, I1, I2, A1, A2, R1=0, R2=0), where S1, S2, I1, I2, A1, and A2 are
#' each selected from a uniform distribution on [0,100] with the constraint
#' that the sum is 100.
#'
#' @return initial condition as a list
#' @export
generate_initial_conditions <- function() {
  S <- runif(n = 1, min = 0, max = 100)
  I <- 100 - S

  s_values_raw <- runif(n = 2) * S
  s_values <- s_values_raw * S / sum(s_values_raw)

  i_values_raw <- runif(n = 4) * I
  i_values <- i_values_raw * I / sum(i_values_raw)

  ic <- c(s_values, i_values, rep(0, 2))
  names(ic) <- paste0(rep(c("S", "A", "I", "R"), each = 2), seq_len(2))

  return(ic)
}
