#' Color-blind friendly color palette
cbPalette <- c(
  "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7", "#999999", "#E69F00"
)

#' Get Color Palette
#'
#' This function returns the color-blind friendly color palette.
#'
#' @return list of colors in color-blind friendly color palette
#' @export
get_color_palette <- function() {
  return (cbPalette)
}

#' Generate Base Plot
#'
#' This function generates an empty plotly line plot.
#'
#' @param data data source for visualization (if not null)
#' @return An empty plotly line plot object
#' @export
generate_base_plot <- function(data = NULL) {
  if (is.null(data)) {
    plot <- plotly::plot_ly(type = "scatter", mode = "lines", colors = cbPalette)
  } else {
    plot <- plotly::plot_ly(data = data, type = "scatter", mode = "lines")
  }
  
  plot <- plot %>%
    plotly::layout(xaxis = list(showline = TRUE), yaxis = list(showline = TRUE))
}

#' Generate Population Plot
#'
#' This function is a wrapper around the generate_base_plot function which 
#' defaults the y_limits parameter to display between 0 and 100.
#'
#' @param data data source for visualization (if not null)
#' @return An empty plotly line plot object
#' @export
generate_population_plot <- function(data = NULL) {
  plot <- generate_base_plot(data) %>%
    plotly::layout(yaxis = list(range = c(0,100)))
}

#' Generate 3D Plot
#'
#' This function creates a 3D plotly plot object.
#'
#' @return An 3D plot object
#' @export
generate_3D_plot <- function() {
  plotly::plot_ly(type = "scatter3d", mode = "lines+markers", colors = cbPalette, marker = list(size = 1.5, showscale = FALSE))
}

#' Generate Tenery Plot
#'
#' This function creates a tenery plotly plot object.
#'
#' @return An tenery plot object
#' @export
generate_tenery_plot <- function() {
  plotly::plot_ly(type = "scatterternary", mode = "scatter", colors = cbPalette)
}

