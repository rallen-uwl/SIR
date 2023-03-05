#' Generate Reinfection Model
#'
#' This function generates the reinfection model for use with deSolve package
#'
#' @param time unused parameter, needed by deSolve
#' @param state state variable values, in model order (see return)
#' @param parameters model parameters
#' @return list of values of all derivatives of model
#' @export
reinfection_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S1 + S2 + A1 + A2 + I1 + I2 + R1 + R2
    I <- A1 + A2 + I1 + I2
    s1 <- S1 / N
    s2 <- S2 / N
    dS1 <- r * R1 + a2 * s2 - a1 * s1 - b1 * s1 * I
    dS2 <- r * R2 + a1 * s1 - a2 * s2 - b2 * s2 * I
    dA1 <- (1 - l) * b1 * s1 * I + a2 * A2 - (a1 + g + k) * A1
    dA2 <- (1 - l) * b2 * s2 * I + a1 * A1 - (a2 + g + k) * A2
    dI1 <- l * b1 * s1 * I + g * A1 - k * I1
    dI2 <- l * b2 * s2 * I + g * A2 - k * I2
    dR1 <- k * A1 + k * I1 - r * R1
    dR2 <- k * A2 + k * I2 - r * R2
    return(list(c(dS1, dS2, dA1, dA2, dI1, dI2, dR1, dR2)))
  })
}

#' Generate Complex Model
#'
#' This function generates the complex model for use with deSolve package
#'
#' @param time unused parameter, needed by deSolve
#' @param state state variable values, in model order (see return)
#' @param parameters model parameters
#' @return list of values of all derivatives of model
#' @export
complex_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S1 + S2 + A1 + A2 + Is + R
    I <- A1 + A2 + Is
    s1 <- S1 / N
    s2 <- S2 / N
    dS1 <- a2 * s2 - (a1 + b1 * I) * s1
    dS2 <- a1 * s1 - (a2 + b2 * I) * s2
    dA1 <- (1 - l) * b1 * I * s1 + a2 * A2 - (a1 + g + k) * A1
    dA2 <- (1 - l) * b2 * I * s2 + a1 * A1 - (a2 + g + k) * A2
    dIs <- l * (b1 * s1 + b2 * s2) * I + g * (A1 + A2) - k * Is
    dR <- k * I
    return(list(c(dS1, dS2, dA1, dA2, dIs, dR)))
  })
}

#' Generate Basic Model
#'
#' This function generates the basic model for use with deSolve package
#'
#' @param time unused parameter, needed by deSolve
#' @param state state variable values, in model order (see return)
#' @param parameters model parameters
#' @return list of values of all derivatives of model
#' @export
basic_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S1 + S2 + Is + Ia + R
    I <- Ia + Is
    s1 <- S1 / N
    s2 <- S2 / N
    dS1 <- -b1 * s1 * I
    dS2 <- -b2 * s2 * I
    dIs <- l * (b1 * s1 + b2 * s2) * I + g * Ia - k * Is
    dIa <- (1 - l) * (b1 * s1 + b2 * s2) * I - (g + k) * Ia
    dR <- k * I
    return(list(c(dS1, dS2, dIs, dIa, dR)))
  })
}

#' Solve Disease Model
#'
#' This function utilizes the deSolve package to solve the systems of
#' differential equations specified in reiteration_model, complex_model, and
#' basic_model
#'
#' @param type model to solve; default reinfection, supported values
#'             reinfection, complex, or basic
#' @param parameters system parameters
#' @param initial_conditions initial condition of the system
#' @param timesteps sequence of timesteps to sove the system over
#' @param path_num optional identification number to identify this path
#' @return data.frame of time solutions of the system
#' @export
solve_system <- function(type = "reinfection",
                         parameters = NULL,
                         initial_conditions = NULL,
                         timesteps = NULL,
                         path_num = NULL) {
  stopifnot(!is.null(parameters) && !is.null(initial_conditions) &&
    !is.null(timesteps))

  if (type == "basic") {
    solution <- as.data.frame(
      ode(
        y = initial_conditions, func = basic_model,
        parms = parameters, times = timesteps
      )
    )
  } else if (type == "complex") {
    solution <- as.data.frame(
      ode(
        y = initial_conditions, func = complex_model,
        parms = parameters, times = timesteps
      )
    )
  } else {
    solution <- as.data.frame(
      ode(
        y = initial_conditions, func = reinfection_model,
        parms = parameters, times = timesteps
      )
    )
  }

  if (!is.null(path_num)) {
    solution <- solution %>% mutate(path = path_num)
  }

  return(solution)
}
