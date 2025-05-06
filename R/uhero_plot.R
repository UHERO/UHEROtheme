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

format_data_long <- function (data, x_var, series) {
  filtered_data <- data %>% select(c(x_var, series))

  formatted_data <- filtered_data %>%
    pivot_longer(-all_of(x_var), names_to = "name", values_to = "value") %>%
    mutate(label = if_else(!!sym(x_var) == max(!!sym(x_var)), as.character(.data$name), NA_character_))

  formatted_data
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

normalize_chart_type <- function(chart_types, series) {
  if (is.null(names(chart_types))) {
    setNames(rep(chart_types, length(series)), series)
  } else {
    chart_types
  }
}

add_geom_line <- function(plot, series_data, ...) {
  plot <- plot +
    geom_line(
      data = series_data,
      aes(y = .data$value, group = .data$name, color = .data$name), ...)

  plot
}

add_geom_col <- function(plot, series_data, ...) {
  plot <- plot +
    geom_col(
      data = series_data,
      aes(y = .data$value, fill = .data$name), ...)

  plot
}

add_geom_bar <- function(plot, series_data, ...) {
  plot <- plot +
    geom_bar(
      data = series_data,
      aes(fill = .data$name), ...)

  plot
}

geom_function_map <- list(
  line = function (plot, data, ...) add_geom_line(plot, data, ...),
  col = function (plot, data, ...) add_geom_col(plot, data, ...),
  bar = function (plot, data, ...) add_geom_bar(plot, data, ...)
)

add_chart_geom <- function(series, chart_type, data, plot, ...) {
  grouped_series <- split(series, chart_type[series])  # group series by type

  for (geom_type in names(grouped_series)) {
    s <- grouped_series[[geom_type]]
    geom_func <- geom_function_map[[geom_type]]
    series_data <- data %>% filter(.data$name %in% s)
    plot <- geom_func(plot, series_data, ...)
  }

  plot
}

validate_chart_types <- function(chart_types) {
  invalid <- setdiff(chart_types, names(geom_function_map))
  if (length(invalid) > 0) {
    stop("Invalid chart_type(s): ", paste(invalid, collapse = ", "))
  }
}

#' Generate a plot with ggplot2 with the UHERO theme with 2 y axes.
#'
#' @param data Dataset to be used. See sampleData.xlsx in the sampledata directory of the package for an example.
#' @param x_var A string corresponding to the column that is to be used for the x axis
#' @param y1 A list specifying the series to be plotted on the primary/left axis. Items in the list are:
#' `series` A vector of string names (i.e. c("col_name")) corresponding to the columns in the data. Defaults to NULL
#' `chart_type` A string, either "line" or "col", or a vector
#' (i.e., c("col_name1" = line", "col_name2" = "bar")) to indicate how the series should be drawn.
#' Defaults to NULL. If not specified the plot will default to a line chart.
#' `limits` A vector of numbers c(min, max, step) indicating the min, max, and steps between axis
#' labels. This is optional and defaults to NULL if not specified.
#' `percent` A boolean indicating if the axis is in percentages. Defaults to FALSE.
#' `unit_prefix` A string to define any symbols that should be in front of the
#' maximum axis label (i.e. "$"). Defaults to an empty string.
#' @param y2 A list specifying the series to be plotted on the secondary/right axis. Items in the list
#' and their defaults are the same as the `y1` parameter
#' @param ... Additional optional parameters that can be used by ggplot geoms. For example, `position = "dodge2"` for a bar chart.
#'
#' @returns A structure with the plot and scaled data used in the plot
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(2010, 2011, 2012, 2013, 2014, 2015),
#'   y = c(5, 3, 10, 15, 4, 7),
#'   z = c(55, 70, 62, 58, 60, 68)
#' )
#' \dontrun{
#' uhero_draw_dual_y_ggplot(
#'   df,
#'   x_var = "x",
#'   y1 = list(
#'     series = c("y")
#'   ),
#'   y2 = list(
#'     series = c("z")
#'   )
#' )
#' }
uhero_draw_dual_y_ggplot <- function (
    data,
    x_var,
    y1 = list(
      series = NULL, chart_type = NULL, limits = NULL, percent = NULL, unit_prefix = NULL
    ),
    y2 = list(
      series = NULL, chart_type = NULL, limits = NULL, percent = NULL, unit_prefix = NULL
    ),
    ...
) {

  y1_series <- y1$series
  y2_series <- y2$series
  y1_limits <- y1$limits
  y2_limits <- y2$limits

  transformation_fns <- dual_y_axis_transform(
    data,
    y1_series,
    y2_series,
    y1_limits,
    y2_limits
  )

  # Handle chart_type as named vector (per-series) or single type
  y1_chart_type <- normalize_chart_type(y1$chart_type %||% "line", y1_series)
  y2_chart_type <- normalize_chart_type(y2$chart_type %||% "line", y2_series)

  # Check for valid chart type(s)
  validate_chart_types(c(unname(y1_chart_type), unname(y2_chart_type)))

  rescaled_data <- data %>%
    mutate(across(all_of(y2_series), transformation_fns$rescale))
  rescaled_data_long <-
    rescaled_data %>%
    pivot_longer(-all_of(x_var), names_to = "name", values_to = "value") %>%
    mutate(label = if_else(!!sym(x_var) == max(!!sym(x_var)), as.character(.data$name), NA_character_))

  legend_position <- dynamic_legend_position(rescaled_data_long, x_var = x_var, y_var = "value")
  y1_breaks <- set_breaks(y1_limits)
  y2_breaks <- set_breaks(y2_limits)
  x_sym <- sym(x_var)

  series_colors <- get_series_colors(unique(rescaled_data_long$name), palette = "all")

  # Init Chart
  plot <- rescaled_data_long %>% ggplot(aes(x = !!x_sym, label = .data$name))

  # Add chart types
  plot <- add_chart_geom(y1_series, y1_chart_type, rescaled_data_long, plot, ...)
  plot <- add_chart_geom(y2_series, y2_chart_type, rescaled_data_long, plot, ...)

  # Add scales
  plot <- plot +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, prefix = y1$unit_prefix %||% "", percent = y1$percent %||% FALSE),
      breaks = if (is.null(y1_breaks)) waiver() else y1_breaks,
      limits = if (is.null(y1_limits)) NULL else c(y1_limits[1], y1_limits[2]),
      sec.axis = sec_axis(
        transform = transformation_fns$transform,
        labels = function(x) uhero_scale_nums(x, prefix = y2$unit_prefix %||% "", percent = y2$percent %||% FALSE),
        breaks = if (is.null(y2_breaks)) waiver() else y2_breaks,
      ),
    )

  # Add colors the themes
  plot <- apply_color_scales(plot, c(y1_chart_type, y2_chart_type), series_colors)
  plot <- plot + uhero_theme() +
    legend_theme(legend_position$pos, legend_position$just)

  structure(
    list(
      plot = plot,
      data = rescaled_data_long
    ),
    class = "uhero_dual_y_ggplot"
  )

}

#' Generates a ggplot2 chart with the UHERO theme.
#'
#' @param data Dataset to be used. See sampleData.xlsx in the sampledata directory of the package for an example.
#' @param series A vector of string names (i.e. c("col_name")) corresponding to the columns in the data that should be plotted on the primary/left axis.
#' @param x_var A string corresponding to the column that is to be used for the x axis
#' @param y_limits A vector of numbers c(min, max, step) indicating the min, max, and steps between axis labels on the left axis. This is optional and defaults to NULL if not specified.
#' @param chart_type A string or a vector indicating the chart type for each series. A single string like "bar" can be used to apply the bar chart type to all series.
#' A vector such as c("line", "bar") or c("Col Name" = "line", "Col Name" = "bar") can be used to specify a type for each series. Current accepted chart type values
#' are "line", "bar", and "col". Defaults to NULL. If not specified the plot will default to a line chart.
#' @param percent A boolean indicating if the maximum y axis label should be marked with a "\%" sign. Defaults to FALSE.
#' @param unit_prefix A string to define any symbols that should be in front of the maximum y axis label (i.e. "$"). Defaults to an empty string.
#' @param ... Additional optional parameters that can be used by ggplot geoms. For example, `position = "dodge2"` for a bar chart.
#'
#' @returns A structure with the plot and scaled data used in the plot
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(2010, 2011, 2012, 2013, 2014, 2015),
#'   y = c(5, 3, 10, 15, 4, 7),
#'   z = c(55, 70, 62, 58, 60, 68)
#' )
#' \dontrun{
#' uhero_draw_ggplot(
#'   data = df,
#'   series = c("y"),
#'   x_var = "x",
#'   y_limits = NULL,
#'   chart_type = "line",
#'   percent = FALSE,
#'   unit_prefix = '',
#' )
#' }
uhero_draw_ggplot <- function(
    data,
    series,
    x_var,
    y_limits = NULL,
    chart_type = NULL,
    percent = NULL,
    unit_prefix = NULL,
    ...
) {
  # Handle chart_type as named vector (per-series) or single type
  chart_type <- normalize_chart_type(chart_type %||% "line", series)

  # Check for valid chart type(s)
  validate_chart_types(chart_type)

  # Prepare data
  data_long <-format_data_long(data, x_var, series)
  x_sym <- sym(x_var)
  y_breaks <- set_breaks(y_limits)
  legend_position <- dynamic_legend_position(data_long, x_var = x_var, y_var = "value")

  series_colors <- get_series_colors(unique(data_long$name), palette = "all")

  # Init chart
  plot <- data_long %>% ggplot(aes(x = !!x_sym, label = .data$name))

  # Add series to chart
  plot <- add_chart_geom(series, chart_type, data_long, plot, ...)

  # Scales and theme
  plot <- plot +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, prefix = unit_prefix %||% "", percent = percent %||% FALSE),
      breaks = if (is.null(y_breaks)) waiver() else y_breaks,
      limits = if (is.null(y_limits)) NULL else c(y_limits[1], y_limits[2])
    )
  plot <- apply_color_scales(plot, chart_type, series_colors)
  plot <- plot +  uhero_theme() +
    legend_theme(legend_position$pos, legend_position$just)

  structure(
    list(
      plot = plot,
      data = data_long
    ),
    class = "uhero_ggplot"
  )
}

dynamic_legend_position <- function(data, x_var, y_var, buffer = 0.3) {
  x_raw <- data[[x_var]]
  y <- data[[y_var]]

  # Check if x is categorical (factor or character)
  is_categorical <- is.factor(x_raw) || is.character(x_raw)
  x <- if (is_categorical) as.numeric(factor(x_raw)) else x_raw

  x_range <- range(x, na.rm = TRUE)
  y_range <- range(y, na.rm = TRUE)
  diff_x <- diff(x_range)
  diff_y <- diff(y_range)

  create_corner <- function(x_min, x_max, y_min, y_max, pos, just) {
    list(
      xlim = c(x_range[1] + diff_x * x_min, x_range[1] + diff_x * x_max),
      ylim = c(y_range[1] + diff_y * y_min, y_range[1] + diff_y * y_max),
      pos = pos,
      just = just
    )
  }

  corners <- list(
    top_left = create_corner(0, buffer, 1 - buffer, 1, c(0.05, 0.95), c(0,1)),
    top_right = create_corner(1 - buffer, 1, 1 - buffer, 1, c(0.95, 0.95), c(1, 1)),
    bottom_left = create_corner(0, buffer, 0, buffer, c(0.05, 0.05), c(0, 0)),
    bottom_right = create_corner(1 - buffer, 1, 0, buffer, c(0.95, 0.05), c(1, 0))
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

legend_theme <- function(pos, just) {
  theme(
    legend.position = pos,
    legend.justification = just,
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.title = element_blank()
  )
}

#' Replace ggplot legend with text labels
#'
#' @param plot A ggplot plot object
#' @param font_size A number for the font size of the labels. Defaults to 3.
#'
#' @returns Returns a ggplot object with text labels.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(2010, 2011, 2012, 2013, 2014, 2015),
#'   y = c(5, 3, 10, 15, 4, 7),
#'   z = c(55, 70, 62, 58, 60, 68)
#' )
#' \dontrun{
#' example_plot <- uhero_draw_ggplot(
#'   data = df,
#'   series = c("y"),
#'   x_var = "x",
#'   y_limits = NULL,
#'   chart_type = "line",
#'   percent = FALSE,
#'   unit_prefix = '',
#' )
#' add_text_labels(exanple_plot)
#' }
add_text_labels <- function(plot, font_size = 3) {
  plot <- plot +
    geom_text_repel(
      aes(color = .data$name, label = .data$label, y = .data$value),
      direction = "y",
      size = font_size
    ) +
    theme(
      legend.position = "none"
    )

  plot
}
