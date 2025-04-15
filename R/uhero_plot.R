set_custom_limits <- function(limits) {
  if (length(limits) != 3 || !is.vector(limits) || !is.numeric(limits)) {
    stop("Error: limits must be a numeric vector of length 3.")
  }

  if (limits[2] <= limits[1]) {
    stop("Error: limits[2] must be greater than limits[1].")
  }

  if (limits[3] >= limits[2] - limits[1]) {
    stop("Error: limits[3] must be less than (limits[2] - limits[1]).")
  }
  limit_min <- limits[1]
  limit_max <- limits[2]
  list(
    min = limit_min,
    max = limit_max
  )
}

set_y_max <- function(limits, series) {
  max <- ifelse(
    is.null(limits),
    max(series),
    set_custom_limits(limits)$max
  )

  max
}

set_y_min <- function(limits, series) {
  min <- ifelse(
    is.null(limits),
    min(series),
    set_custom_limits(limits)$min
  )

  min
}

set_breaks <- function(limits) {
  breaks <- NULL
  if (!is.null(limits)) {
    breaks <- seq(limits[1], limits[2], by = limits[3])
  }

  breaks
}

#' Set up transformation and scaling functions for charts with two y axes.
#'
#'
#' @param data Dataset to be used. See sampleData.xlsx in the sampledata directory of the package for an example
#' @param y1_series A vector of string names (i.e. c("col_name")) corresponding to the columns in the data that should be plotted on the primary/left axis.
#' @param y2_series A vector of string names (i.e. c("col_name")) corresponding to the columns in the data that should be plotted on the secondary/right axis.
#' @param y1_limits A vector of numbers c(min, max, step) indicating the min, max, and steps between axis labels on the left axis. This is optional and defaults to NULL if not specified.
#' @param y2_limits A vector of numbers c(min, max, step) indicating the min, max, and steps between axis labels on the right axis. This is optional and defaults to NULL if not specified.
#'
#' @returns Returns a list of two functions: rescale and transform.
#' Rescale is used to scale the data that is to be plotted on the secondary/right axis
#' to the data that is on the primary/left axis.
#' Transform is used to transform the secondary/right axis.
#' @export
#'
#' @examples
dual_y_axis_transform <- function(
    data,
    y1_series,
    y2_series,
    y1_limits = NULL,
    y2_limits = NULL
) {

  y1_series <- data %>% select(all_of(y1_series))
  y2_series <- data %>% select(all_of(y2_series))

  left_max <- set_y_max(y1_limits, y1_series)
  left_min <- set_y_min(y1_limits, y1_series)
  right_max <- set_y_max(y2_limits, y2_series)
  right_min <- set_y_min(y2_limits, y2_series)

  scale_factor <- (left_max - left_min) / (right_max - right_min)

  shift <- left_min - (scale_factor * right_min)

  rescale <- function(x) {
    scale_factor * x + shift
  }

  axis_transform <- function(x) {
    (x - shift) / scale_factor
  }

  list(
    rescale = rescale,
    transform = axis_transform
  )
}

add_geom_line <- function(plot, plot_data, series_list) {
  plot <- plot +
    geom_line(data = plot_data %>% filter(.data$name %in% series_list),
              aes(y = .data$value, group = .data$name, color = .data$name))

  plot
}

add_geom_col <- function(plot, plot_data, series_list) {
  plot <- plot +
    geom_col(data = plot_data %>% filter(.data$name %in% series_list),
             aes(y = .data$value, fill = .data$name))

  plot
}

#' Generate a plot with ggplot2 with 2 y axes.
#'
#' @param data Dataset to be used. See sampleData.xlsx in the sampledata directory of the package for an example
#' @param y1_series A vector of string names (i.e. c("col_name")) corresponding to the columns in the data that should be plotted on the primary/left axis.
#' @param y2_series A vector of string names (i.e. c("col_name")) corresponding to the columns in the data that should be plotted on the secondary/right axis.
#' @param y1_limits A vector of numbers c(min, max, step) indicating the min, max, and steps between axis labels on the left axis. This is optional and defaults to NULL if not specified.
#' @param y2_limits A vector of numbers c(min, max, step) indicating the min, max, and steps between axis labels on the right axis. This is optional and defaults to NULL if not specified.
#' @param x_var A string corresponding to the column that is to be used for the x axis
#' @param y1_chart_type A string, either "line" or "col" indicating the chart type to be used for series on the left axis.
#' @param y2_chart_type A string, either "line" or "col" indicating the chart type to be used for series on the right axis.
#' @param y1_percent A boolean indicating if the maximum left axis label should be marked with a "\%" sign. Defaults to FALSE.
#' @param y2_percent A boolean indicating if the maximum right axis label should be marked with a "\%" sign. Defaults to FALSE.
#' @param y1_unit_prefix A string to define any symbols that should be in front of the maximum left axis label (i.e. "$"). Defaults to an empty string.
#' @param y2_unit_prefix A string to define any symbols that should be in front of the maximum right axis label (i.e. "$"). Defaults to an empty string.
#'
#' @returns Returns a ggplot object
#' @export
#'
#' @examples
uhero_draw_dual_y_chart <- function(
    data,
    y1_series,
    y2_series,
    y1_limits = NULL,
    y2_limits = NULL,
    x_var,
    y1_chart_type = "line",
    y2_chart_type = "line",
    y1_percent = FALSE,
    y2_percent = FALSE,
    y1_unit_prefix = '',
    y2_unit_prefix = ''
  ) {
  transformation_fns <- dual_y_axis_transform(
    data,
    y1_series,
    y2_series,
    y1_limits,
    y2_limits
  )

  rescaled_data <- data %>% mutate(across(all_of(y2_series), transformation_fns$rescale))

  rescaled_data_long <-
    rescaled_data %>%
    pivot_longer(-all_of(x_var), names_to = "name", values_to = "value") %>%
    mutate(label = if_else(!!sym(x_var) == max(!!sym(x_var)), as.character(.data$name), NA_character_))

  y1_breaks <- set_breaks(y1_limits)

  y2_breaks <- set_breaks(y2_limits)

  x_var <- sym(x_var)

  plot <- rescaled_data_long %>% ggplot(aes(x = !!x_var, label = .data$name))

  if (y2_chart_type == "col") {
    plot <- add_geom_col(plot, rescaled_data_long, y2_series)
  }

  if (y1_chart_type == "col") {
    plot <- add_geom_col(plot, rescaled_data_long, y1_series)
  }

  if (y1_chart_type == "line") {
    plot <- add_geom_line(plot, rescaled_data_long, y1_series)
  }

  if (y2_chart_type == "line") {
    plot <- add_geom_line(plot, rescaled_data_long, y2_series)
  }

  plot <- plot +
    geom_text_repel(
      aes(label = .data$label, y = .data$value, color = .data$name, family = "Open Sans"),
      nudge_x = 0.5,
      na.rm = TRUE,
      size = 3,
      direction = "y",
      show.legend = FALSE,
      hjust = 0,
      segment.size = 0,
      segment.color = NA,
      segment.alpha = 0
    ) +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, prefix = y1_unit_prefix, percent = y1_percent),
      breaks = if (is.null(y1_breaks)) waiver() else y1_breaks,
      limits = if (is.null(y1_limits)) NULL else c(y1_limits[1], y1_limits[2]),
      sec.axis = sec_axis(
        transform = transformation_fns$transform,
        labels = function(x) uhero_scale_nums(x, prefix = y2_unit_prefix, percent = y2_percent),
        breaks = if (is.null(y2_breaks)) waiver() else y2_breaks,
      ),
    ) +
    uhero_scale_color() +
    uhero_scale_fill("secondary")

  plot
}

# uhero_draw_dual_y_chart(
#   transactions,
#   c("Condominium Transactions", "Single-family Transactions"),
#   c("Interest Rate"),
#   y1_limits = NULL,
#   y2_limits = NULL,
#   "year",
#   "line",
#   "line",
#   FALSE,
#   TRUE
# )
#
# uhero_draw_dual_y_chart(
#   transactions,
#   c("Condominium Transactions", "Single-family Transactions"),
#   c("Interest Rate"),
#   y1_limits = NULL,
#   y2_limits = NULL,
#   "year",
#   "line",
#   "line",
#   FALSE,
#   TRUE
# ) + uhero_theme()
