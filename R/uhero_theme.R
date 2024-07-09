font_add("opensans", regular = "./font/OpenSans-Regular.ttf", bold = "./font/OpenSans-Bold.ttf")
font_add("opensans-semibold", regular = "./font/OpenSans-SemiBold.ttf")
showtext_auto()

#' Adds a UHERO theme for Ggplot charts.
#'
#' Modifies the ggplot minimal theme to fit the style used for UHERO reports/presentations.
#'
#' @param layout A boolean to indicate whether or not the theme is being applied to a plot that is to be used in a UHERO report layout.
#' This defaults to true. If layout is TRUE, then the base size is set to 9 and the font family is set to open sans. If the parameter is FALSE, then
#' the base size is set to 30 and the font family is set to opensans-semibold. Set layout to FALSE if the plot is being used for a powerpoint or keynote presentation.
#'
#'
#' @export
#'
#' @examples
#' plot <- ggplot2::ggplot(ggplot2::mpg) + uhero_theme()
uhero_theme <- function(layout = TRUE) {
  font_size <- if (layout) 9 else 30
  font_family <- if (layout) "opensans" else "opensans-semibold"
  theme_minimal(
    base_size = font_size,
    base_family = font_family
  ) %+replace%
    theme(
      #text = element_text(family = "opensans", color = "#1D667F", size = 9),
      # Remove axis title and lines
      axis.title = element_blank(),
      axis.line = element_blank(),
      #axis.text = element_text(family = "opensans", color = "#505050", size = 9),
      # Remove axis tick lines
      axis.ticks = element_blank(),
      legend.key = element_blank(),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      # Remove background panel
      panel.background = element_blank(),
    )
}


#' Format tick labels for ggplot chart axes with continuous scales
#'
#' This can be passed in to the labels parameter of ggplot scales like scale_x_continous() or scale_y_continuous()
#' If the max value of the scale is larger than 1000, the tick labels will be scaled down with a suffix added to the maximum value.
#' For max values of at least 10^3 but less than 10^6, tick labels will be divided by 10^3 with "K" added to the label for the maximum value.
#' For max values of at least 10^6 but less than 10^9, tick labels will be divided by 10^6 with "M" added to the label for the maximum value.
#' For max values of at least 10^9 but less than 10^12, tick labels will be divided by 10^9 with "B" added to the label for the maximum value.
#'
#' @param x Passed in from labels function.
#' @param scale_limit Defaults to the max value of the scale.
#' @param prefix This is optional and defaults to an empty string. This can be used to add things like currency labels, (i.e. "$100").
#' @param percent Boolean that defaults to false. Set the value to true to add a "\%" to the maximum value tick label.
#' @param ... Additional arguments that can be passed to R's format().
#'
#' @return Returns a formatted string for the tick label.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() +
#' ggplot2::scale_y_continuous(labels = function(x) uhero_scale_nums(x, percent = TRUE)) +
#' ggplot2::scale_x_continuous(labels = function(x) uhero_scale_nums(x))
uhero_scale_nums <- function(x, scale_limit = max(x, na.rm = TRUE), prefix = '', percent = FALSE, ...) {
  thou <- (scale_limit >= 10^3 && scale_limit < 10^6)
  mil <- (scale_limit >= 10^6 && scale_limit < 10^9)
  bil <- (scale_limit >= 10^9 && scale_limit < 10^12)
  suffix <- if (thou) {
    'K'
  } else if (mil) {
    'M'
  } else if (bil) {
    'B'
  } else {
    ''
  }
  if (percent) {
    suffix <- paste(suffix, "%", sep="")
  }
  n <- if (thou) {
    10^3
  } else if (mil) {
    10^6
  } else if (bil) {
    10^9
  } else {
    1
  }
  ifelse(x == scale_limit, format(paste0(prefix, x / n, suffix), ...), format(paste0(x / n), ...))
}
