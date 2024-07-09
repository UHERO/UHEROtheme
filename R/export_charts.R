#' Preview chart for forecast layout
#'
#' This modifies the size of the view port window to more accurately reflect the size
#' of the figure including the placement of elements like data labels that would be used in a forecast layout.
#'
#' @param plot Plot object
#' @param w Width of the view port in inches, defaults to 4.5
#' @param h Height of the view port in inches, defaults to 2.45
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' draw_fcast_layout(plot)
draw_fcast_layout <- function(plot, w = 4.5, h = 2.45) {

  grid::grid.newpage()

  grid::rectGrob(gp = grid::gpar(fill = "white")) |>
    grid::grid.draw()

  grid::viewport(width  = ggplot2::unit(w, "in"),
                 height = ggplot2::unit(h, "in")) |>
    grid::pushViewport()

  ggplot2::ggplot_build(plot) |>
    ggplot2::ggplot_gtable() |>
    grid::grid.draw()
}

#' Preview chart for report layout
#'
#' This modifies the size of the view port window to more accurately reflect the size
#' of the figure including the placement of elements like data labels that would be used in a UHERO report layout.
#' Sometimes charts may need to be a different size, so the function does accept parameters to change the width and height.
#'
#' @param plot Plot object
#' @param w Width of the view port in inches, defaults to 5.6931
#' @param h Height of the view port in inches, defaults to 4
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' draw_report_layout(plot)
draw_report_layout <- function(plot, w = 5.6931, h = 4) {

  grid::grid.newpage()

  grid::rectGrob(gp = grid::gpar(fill = "white")) |>
    grid::grid.draw()

  grid::viewport(width  = ggplot2::unit(w, "in"),
                 height = ggplot2::unit(h, "in")) |>
    grid::pushViewport()

  ggplot2::ggplot_build(plot) |>
    ggplot2::ggplot_gtable() |>
    grid::grid.draw()
}


#' Export chart for forecast report layout
#'
#' Uses \code{ggplot2::ggsave} to save a copy of the chart. By default, charts are sized at 4.5 x 2.45 inches.
#' Sometimes charts may need to be larger, so the export function does accept parameters to change the width and height.
#' Please try not to exceed 5 inches for the width. If the exported file is intended for use in a forecast layout, please use a `.svg`, `.pdf`, or `.eps` extension.
#'
#' @param file_name A string for the file name, including the extension.
#' @param forecast_plot Ggplot plot object.
#' @param w Integer - width of the exported image, defaults to 4.5
#' @param h Integer - height of the exported image, defaults to 2.45
#' @param u A string for the units, defaults to "in" for inches
#' @param ... Additional parameters that can be passed to ggplot2::ggsave
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' export_fcast_layout('plot.svg', plot)
export_fcast_layout <- function(file_name, forecast_plot, w = 4.5, h = 2.45, u = "in", ...) {
   ggsave(filename = file_name, plot = forecast_plot, width = w, height = h, units = u, ...)
}

#' Export chart for UHERO report layout
#'
#' Uses \code{ggplot2::ggsave} to save a copy of the chart. By default, charts are sized at 5.6931 x 4 inches.
#' Sometimes charts may need to be larger, so the export function does accept parameters to change the width and height.
#' Please use a `.svg`, `.pdf`, or `.eps` extension when exporting for a report layout.
#'
#' @param file_name A string for the file name, including the extension.
#' @param plot Ggplot plot object.
#' @param w Integer - width of the exported image, defaults to 5.6931
#' @param h Integer - height of the exported image, defaults to 4
#' @param u A string for the units, defaults to "in" for inches
#' @param ... Additional parameters that can be passed to ggplot2::ggsave
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' export_report_layout('plot.svg', plot)
export_report_layout <- function(file_name, plot, w = 5.6931, h = 4, u = "in", ...) {
  ggsave(filename = file_name, plot = plot, width = w, height = h, units = u, ...)
}

#' Export plot
#'
#' Uses \code{ggplot2::ggsave} to save a copy of the chart. By default the exports are sized at 1920 x 1080 pixels.
#'
#' @param file_name A string for the file name, including the extension.
#' @param plot Ggplot plot object.
#' @param w Integer - width of the exported image, defaults to 1920
#' @param h Integer - height of the exported image, defaults to 1080
#' @param u A string for the units, defaults to "px" for pixels
#' @param ... Additional parameters that can be passed to ggplot2::ggsave
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' export_report_layout('plot.png', plot)
export_plot <- function(file_name, plot, w = 1920, h = 1080, u = "px", ...) {
  ggsave(filename = file_name, plot = plot, width = w, height = h, units = u, ...)
}
