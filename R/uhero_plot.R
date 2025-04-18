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
#' df <- data.frame(
#'   x = c(2010, 2011, 2012, 2013, 2014, 2015),
#'   y = c(5, 3, 10, 15, 4, 7),
#'   z = c(55, 70, 62, 58, 60, 68)
#' )
#' dual_y_axis_transform(
#'   df,
#'   y1_series = c("y"),
#'   y2_series = c("z"),
#'   y1_limits = c(0, 15, 5),
#'   y2_limits = c(0, 70, 10)
#' )
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

add_geom_bar <- function(plot, plot_data, series_list, bar_position) {
  position <- if (is.null(bar_position)) {
    "stack"
  } else {
    bar_position
  }
  plot <- plot +
    geom_bar(data = plot_data %>% filter(.data$name %in% series_list),
             aes(y = .data$value, fill = .data$name), stat = "identity", position = position)

  plot
}

add_geom_bar_dodge <- function(plot, plot_data, series_list) {
  plot <- plot +
    geom_bar(data = plot_data %>% filter(.data$name %in% series_list),
             aes(y = .data$value, fill = .data$name), stat = "identity", position = "dodge")

  plot
}

#' Generate a plot with ggplot2 with the UHERO theme with 2 y axes.
#'
#' @param data Dataset to be used. See sampleData.xlsx in the sampledata directory of the package for an example.
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
#' df <- data.frame(
#'   x = c(2010, 2011, 2012, 2013, 2014, 2015),
#'   y = c(5, 3, 10, 15, 4, 7),
#'   z = c(55, 70, 62, 58, 60, 68)
#' )
#' \dontrun{uhero_draw_dual_y_chart(
#'   df,
#'   y1_series = c("y"),
#'   y2_series = c("z"),
#'   y1_limits = NULL,
#'   y2_limits = NULL,
#'   x_var = "x",
#'   y1_chart_type = "line",
#'   y2_chart_type = "line",
#'   y1_percent = FALSE,
#'   y2_percent = FALSE,
#'   y1_unit_prefix = '',
#'   y2_unit_prefix = ''
#' )}
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
    pivot_longer(-all_of(x_var), names_to = "name", values_to = "value")

  legend_position <- dynamic_legend_position(rescaled_data_long, x_var = x_var, y_var = "value")

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
    uhero_scale_fill("secondary") +
    uhero_theme() +
    theme(
      legend.position = legend_position$pos,
      legend.justification = legend_position$just,
      legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
      legend.title = element_blank()
    )

  plot
}

dynamic_legend_position <- function(data, x_var, y_var) {
  x_raw <- data[[x_var]]
  y <- data[[y_var]]

  # Check if x is categorical (factor or character)
  if (is.factor(x_raw) || is.character(x_raw)) {
    x <- as.numeric(factor(x_raw))  # Position indices
    is_categorical <- TRUE
  } else {
    x <- x_raw
    is_categorical <- FALSE
  }

  x_range <- range(x, na.rm = TRUE)
  y_range <- range(y, na.rm = TRUE)

  # Define corners
  corners <- list(
    "top-left" = list(
      xlim = c(x_range[1], x_range[1] + diff(x_range) * 0.3),
      ylim = c(y_range[2] - diff(y_range) * 0.3, y_range[2]),
      pos = c(0.05, 0.95), just = c(0, 1)
    ),
    "top-right" = list(
      xlim = c(x_range[2] - diff(x_range) * 0.3, x_range[2]),
      ylim = c(y_range[2] - diff(y_range) * 0.3, y_range[2]),
      pos = c(0.95, 0.95), just = c(1, 1)
    ),
    "bottom-left" = list(
      xlim = c(x_range[1], x_range[1] + diff(x_range) * 0.3),
      ylim = c(y_range[1], y_range[1] + diff(y_range) * 0.3),
      pos = c(0.05, 0.05), just = c(0, 0)
    ),
    "bottom-right" = list(
      xlim = c(x_range[2] - diff(x_range) * 0.3, x_range[2]),
      ylim = c(y_range[1], y_range[1] + diff(y_range) * 0.3),
      pos = c(0.95, 0.05), just = c(1, 0)
    )
  )

  # Count points in each corner region
  counts <- sapply(corners, function(corner) {
    sum(
      x >= corner$xlim[1] & x <= corner$xlim[2] &
        y >= corner$ylim[1] & y <= corner$ylim[2],
      na.rm = TRUE
    )
  })

  best_corner <- names(which.min(counts))
  return(corners[[best_corner]][c("pos", "just")])
}

#' Generates a ggplot2 chart with the UHERO theme.
#'
#' @param data Dataset to be used. See sampleData.xlsx in the sampledata directory of the package for an example.
#' @param series A vector of string names (i.e. c("col_name")) corresponding to the columns in the data that should be plotted on the primary/left axis.
#' @param x_var A string corresponding to the column that is to be used for the x axis
#' @param y_limits A vector of numbers c(min, max, step) indicating the min, max, and steps between axis labels on the left axis. This is optional and defaults to NULL if not specified.
#' @param chart_type A string, either "line", "col", or "bar" indicating the chart type to be used for the series.
#' @param bar_position A string to indicate the position of the bars ("stack", "fill", "dodge", "dodge2")
#' if the chart type specified is set to "bar". Defaults to NULL resulting in a stacked bar chart.
#' @param percent A boolean indicating if the maximum y axis label should be marked with a "\%" sign. Defaults to FALSE.
#' @param unit_prefix A string to define any symbols that should be in front of the maximum y axis label (i.e. "$"). Defaults to an empty string.
#'
#' @returns Returns a ggplot object
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(2010, 2011, 2012, 2013, 2014, 2015),
#'   y = c(5, 3, 10, 15, 4, 7),
#'   z = c(55, 70, 62, 58, 60, 68)
#' )
#' \dontrun{uhero_draw_ggplot(
#'   data = df,
#'   series = c("y"),
#'   x_var = "x",
#'   y_limits = NULL,
#'   chart_type = "line",
#'   bar_position = NULL,
#'   percent = FALSE,
#'   unit_prefix = '',
#' )}
uhero_draw_ggplot <- function(
    data,
    series,
    x_var,
    y_limits = NULL,
    chart_type = "line",
    bar_position = NULL,
    percent = FALSE,
    unit_prefix = ""
) {
  filtered_data <- data %>% select(c(x_var, series))

  data_long <-
    filtered_data %>%
    pivot_longer(-all_of(x_var), names_to = "name", values_to = "value") %>%
    mutate(label = if_else(!!sym(x_var) == max(!!sym(x_var)), as.character(.data$name), NA_character_))

  x_var <- sym(x_var)

  y_breaks <- set_breaks(y_limits)

  legend_position <- dynamic_legend_position(data_long, x_var = x_var, y_var = "value")

  plot <- data_long %>% ggplot(aes(x = !!x_var, label = .data$name))

  if (chart_type == "line") {
    plot <- add_geom_line(plot, data_long, series)
  }

  if (chart_type == "col") {
    plot <- add_geom_bar(plot, data_long, series)
  }

  if (chart_type == "bar") {
    plot <- add_geom_bar(plot, data_long, series, bar_position)
  }

  plot <- plot +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, prefix = unit_prefix, percent = percent),
      breaks = if (is.null(y_breaks)) waiver() else y_breaks,
      limits = if (is.null(y_limits)) NULL else c(y_limits[1], y_limits[2])
    ) +
    uhero_scale_color() +
    uhero_scale_fill() +
    uhero_theme() +
    theme(
      legend.position = legend_position$pos,
      legend.justification = legend_position$just,
      legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
      legend.title = element_blank()
    )

  plot
}

#' Adds a gray shaded region to a ggplot chart to indicate forecasted data.
#'
#' @param plot A ggplot object, such as one generated by `uhero_draw_dual_y_chart`
#' @param min_x The starting point along the x-axis.
#' @param max_x The ending point along the x-axis
#'
#' @returns Returns a ggplot object with a rectangular annotation layer for
#' the forecasted data.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(2010, 2011, 2012, 2013, 2014, 2015),
#'   y = c(5, 3, 10, 15, 4, 7),
#'   z = c(55, 70, 62, 58, 60, 68)
#' )
#' \dontrun{
#' chart <- uhero_draw_dual_y_chart(
#'   df,
#'   y1_series = c("y"),
#'   y2_series = c("z"),
#'   y1_limits = NULL,
#'   y2_limits = NULL,
#'   x_var = "x",
#'   y1_chart_type = "line",
#'   y2_chart_type = "line",
#'   y1_percent = FALSE,
#'   y2_percent = FALSE,
#'   y1_unit_prefix = '',
#'   y2_unit_prefix = ''
#' )
#' add_forecast_shading(chart, 2013, 2015)
#'}
add_forecast_shading <- function(plot, min_x, max_x) {
  plot <- plot +
    annotate(
      "rect",
      xmin = min_x,
      xmax = max_x,
      ymin = -Inf,
      ymax = Inf,
      alpha = .1,
      fill = uhero_colors('gray')
    )

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
