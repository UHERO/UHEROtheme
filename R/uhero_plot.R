parse_limits <- function(limits, series) {
  if (is.null(limits)) {
    return(list(
      min = min(series, na.rm = TRUE),
      max = max(series, na.rm = TRUE),
      step = NULL
    ))
  }

  stopifnot(
    is.numeric(limits),
    length(limits) == 3,
    limits[2] > limits[1],
    limits[3] < (limits[2] - limits[1])
  )

  list(
    min = limits[1],
    max = limits[2],
    step = limits[3]
  )
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

  y1_vals <- data %>% select(all_of(y1_series)) %>% unlist(use.names = FALSE)
  y2_vals <- data %>% select(all_of(y2_series)) %>% unlist(use.names = FALSE)

  # left_max <- set_y_max(y1_limits, y1_series)
  # left_min <- set_y_min(y1_limits, y1_series)
  # right_max <- set_y_max(y2_limits, y2_series)
  # right_min <- set_y_min(y2_limits, y2_series)
  lim1 <- parse_limits(y1_limits, y1_vals)
  lim2 <- parse_limits(y2_limits, y2_vals)

  #scale_factor <- (left_max - left_min) / (right_max - right_min)
  scale_factor <- (lim1$max - lim1$min) / (lim2$max - lim2$min)

  #shift <- left_min - (scale_factor * right_min)
  shift <- lim1$min - scale_factor * lim2$min

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

# Normalize point_size per-series for scatter/bubble plots
normalize_point_size <- function(point_size, series, default = 3) {

  # Default: apply constant size to all series
  if (is.null(point_size)) {
    point_size <- default
  }

  # Case 1: single value (numeric or character) — recycle for all series
  if (length(point_size) == 1 && is.null(names(point_size))) {
    point_size <- rep(point_size, length(series))
    names(point_size) <- series
    return(point_size)
  }

  # Case 2: named vector — reorder/align with series
  if (!is.null(names(point_size))) {
    out <- rep(NA, length(series))
    names(out) <- series
    out[names(point_size)] <- point_size
    return(out)
  }

  # Case 3: unnamed vector of same length
  if (length(point_size) == length(series)) {
    names(point_size) <- series
    return(point_size)
  }

  stop("Invalid point_size: must be numeric, character, or named vector matching series.")
}

as_numeric_x <- function(x) {
  if (is.factor(x)) {
    return(as.numeric(x))
  }

  if (is.character(x)) {
    return(as.numeric(factor(x, levels = unique(x))))
  }

  if (is.numeric(x) || inherits(x, c("Date", "POSIXct"))) {
    # Keep numeric and Date/Datetime as-is
    return(x)
  }

  rlang::abort(
    "Unsupported x_var type in add_geom_col_rect()",
    class = "add_geom_col_rect_invalid_x_type"
  )
}


# uses geom_rect so that columns are drawn starting at 0 when a col type is
# used for series plotted on a second, right axis
add_geom_col_rect <- function(plot, series_data, x_var, baseline = 0, ...) {

  x_raw <- series_data[[x_var]]
  x_num <- as_numeric_x(x_raw)

  # Compute spacing-based width
  x_unique <- sort(unique(x_num))

  width <- if (length(x_unique) > 1) {
    min(diff(x_unique), na.rm = TRUE) * 0.9
  } else {
    0.9
  }

  rect_data <- series_data %>%
    mutate(
      xmin = x_num - width / 2,
      xmax = x_num + width / 2,
      ymin = pmin(.data$value, baseline),
      ymax = pmax(.data$value, baseline)
    )

  plot +
    geom_blank(data = series_data, aes(x = .data[[x_var]])) +
    geom_rect(
      data = rect_data,
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$name
      ),
      inherit.aes = FALSE,
      ...
    )
}

# used for normal col type charts (i.e. single axis)
add_geom_col_regular <- function(plot, series_data, ...) {
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

add_geom_point <- function(plot, series_data, point_size, show_size_legend, ...) {
  if (is.character(point_size)) { #bubble chart
    plot <- plot + geom_point(
      data = series_data,
      aes(y = .data$value, color = .data$name, size = .data$size_value),
      show.legend = show_size_legend,
      ...
    )
  } else {
    plot <- plot + geom_point(
      data = series_data,
      aes(y = .data$value, color = .data$name),
      size = point_size,
      ...
    )
  }

  plot
}

apply_bubble_sizes <- function(long_data, data, x_var, point_size) {
  # Identify bubble series
  bubble_series <- names(point_size)[is.character(point_size)]
  if (!length(bubble_series)) return(long_data)

  # Prepare a lookup table for all bubble series
  bubble_lookup <- lapply(bubble_series, function(series_name) {
    bubble_var <- point_size[[series_name]]

    # Skip if bubble_var not in data
    if (!bubble_var %in% names(data)) return(NULL)

    data %>%
      select(all_of(c(x_var, bubble_var))) %>%
      rename(size_value = all_of(bubble_var)) %>%
      mutate(name = series_name)
  })

  bubble_lookup <- bind_rows(bubble_lookup)

  # Join long_data with bubble_lookup on x_var and name
  long_data <- long_data %>%
    left_join(bubble_lookup, by = c(x_var, "name")) %>%
    mutate(size_value = scales::rescale(.data$size_value, to = c(0, 1)))

  long_data
}

geom_dispatch <- function(type, dual = FALSE) {
  switch(type,
    line = add_geom_line,
    bar = add_geom_bar,
    col = if (dual) add_geom_col_rect else add_geom_col_regular,
    scatter = add_geom_point,
    stop("Unknown geom type:", type)
  )
}

add_chart_geom <- function(
    series,
    chart_type,
    data,
    plot,
    x_var = NULL,
    baseline = NULL,
    dual = FALSE,
    point_size = NULL,
    show_size_legend = TRUE,
    ...
  ) {
  # Use factor levels to determine draw order
  draw_order <- series

  # Group by geom type while preserving order
  geom_groups <- split(draw_order, chart_type[draw_order])

  for (geom_type in names(geom_groups)) {
    s <- geom_groups[[geom_type]]
    geom_func <- geom_dispatch(geom_type, dual)
    series_data <- data %>% filter(.data$name %in% s)

    if (geom_type == "col" && dual) {
      plot <- geom_func(plot, series_data, x_var = x_var, baseline = baseline, ...)
    } else if (geom_type == "scatter") {
      for (sname in s) {
        this_data <- series_data %>% filter(.data$name == sname)

        plot <- geom_func(
          plot,
          this_data,
          point_size = point_size[[sname]],
          show_size_legend = show_size_legend,
          ...
        )
      }
    } else {
      # do not pass x_var and baseline for other geoms like line, bar, or single axis cols
      plot <- geom_func(plot, series_data, ...)
    }
  }

  plot
}

validate_chart_types <- function(chart_types) {
  valid_types <- c('line', 'bar', 'col', 'scatter')
  invalid <- setdiff(chart_types, valid_types)
  if (length(invalid) > 0) {
    stop('Invalid chart_type(s):', paste(invalid, collapse = ', '))
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
#' `point_size` A number or string specifying the point size for a scatter/bubble plot. Defaults to 3. Use a number to set a fixed size
#' for points. Or use a data column name to create a bubble chart.
#' @param y2 A list specifying the series to be plotted on the secondary/right axis. Items in the list
#' and their defaults are the same as the `y1` parameter
#' @param bubble_legend Parameter used for bubble plots to indicate if plot should display a size legend, defaults to TRUE.
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
      series = NULL, chart_type = NULL, limits = NULL, percent = NULL, unit_prefix = NULL, point_size = NULL
    ),
    y2 = list(
      series = NULL, chart_type = NULL, limits = NULL, percent = NULL, unit_prefix = NULL, point_size = NULL
    ),
    bubble_legend = TRUE,
    ...
) {

  y1_series <- y1$series
  y2_series <- y2$series
  y1_limits <- y1$limits
  y2_limits <- y2$limits
  y1$chart_type <- y1$chart_type %||% "line"
  y2$chart_type <- y2$chart_type %||% "line"
  y1$percent <- ifelse(is.null(y1$percent), FALSE, y1$percent)
  y2$percent <- ifelse(is.null(y2$percent), FALSE, y2$percent)
  y1$unit_prefix <- y1$unit_prefix %||% ""
  y2$unit_prefix <- y2$unit_prefix %||% ""

  transformation_fns <- dual_y_axis_transform(
    data,
    y1_series,
    y2_series,
    y1_limits,
    y2_limits
  )

  # Handle chart_type as named vector (per-series) or single type
  y1_chart_type <- normalize_chart_type(y1$chart_type, y1_series)
  y2_chart_type <- normalize_chart_type(y2$chart_type, y2_series)

  all_series <- c(y1_series, y2_series)
  all_chart_type <- c(y1_chart_type, y2_chart_type)

  # Check for valid chart type(s)
  validate_chart_types(c(unname(y1_chart_type), unname(y2_chart_type)))

  rescaled_data <- data %>%
    mutate(across(all_of(y2_series), transformation_fns$rescale))
  rescaled_data_long <-
    rescaled_data %>%
    pivot_longer(-all_of(x_var), names_to = "name", values_to = "value") %>%
    mutate(label = if_else(!!sym(x_var) == max(!!sym(x_var)), as.character(.data$name), NA_character_))

  rescale_y2 <- transformation_fns$rescale

  # Use the series order as defined by the input order of y1$series and y2$series
  draw_order <- all_series
  rescaled_data_long$name <- factor(rescaled_data_long$name, levels = draw_order)

  legend_position <- dynamic_legend_position(rescaled_data_long, x_var = x_var, y_var = "value")
  y1_breaks <- set_breaks(y1_limits)
  y2_breaks <- set_breaks(y2_limits)
  x_sym <- sym(x_var)

  series_colors <- get_series_colors(levels(rescaled_data_long$name), palette = "all")

  y1$point_size <- normalize_point_size(y1$point_size, y1_series, default = 3)
  y2$point_size <- normalize_point_size(y2$point_size, y2_series, default = 3)

  point_size <- c(y1$point_size, y2$point_size)

  rescaled_data_long <- apply_bubble_sizes(rescaled_data_long, data, x_var, point_size)

  # Initialize ggplot
  plot <- ggplot(rescaled_data_long, aes(x = !!x_sym))

  plot <- add_chart_geom(
    series = draw_order,
    chart_type = all_chart_type,
    data = rescaled_data_long,
    plot = plot,
    x_var = x_var,
    baseline = rescale_y2(0),
    dual = TRUE,
    point_size = point_size,
    show_size_legend = bubble_legend %||% TRUE,
    ...
  )

  if (any(is.character(point_size))) {
    plot <- plot + scale_size_continuous(range = c(2, 12), guide = if (bubble_legend) "legend" else "none")
  }

  # Add scales
  plot <- plot +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, prefix = y1$unit_prefix, percent = y1$percent),
      breaks = if (is.null(y1_breaks)) waiver() else y1_breaks,
      limits = if (is.null(y1_limits)) NULL else c(y1_limits[1], y1_limits[2]),
      sec.axis = sec_axis(
        transform = transformation_fns$transform,
        labels = function(x) uhero_scale_nums(x, prefix = y2$unit_prefix, percent = y2$percent),
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
#' @param point_size A number or string specifying the point size for a scatter/bubble plot. Defaults to 3. Use a number to set a fixed size
#' for points. Or use a data column name to create a bubble chart.
#' @param bubble_legend Parameter used for bubble plots to indicate if plot should display a size legend, defaults to TRUE.
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
    point_size = 3,
    bubble_legend = TRUE,
    ...
) {
  unit_prefix <- unit_prefix %||% ""
  percent <- ifelse(is.null(percent), FALSE, percent)
  # Handle chart_type as named vector (per-series) or single type
  chart_type <- normalize_chart_type(chart_type %||% "line", series)

  # Check for valid chart type(s)
  validate_chart_types(chart_type)

  # Prepare data
  data_long <- format_data_long(data, x_var, series)
  data_long$name <- factor(data_long$name, levels = series)

  x_sym <- sym(x_var)
  y_breaks <- set_breaks(y_limits)
  legend_position <- dynamic_legend_position(data_long, x_var = x_var, y_var = "value")

  series_colors <- get_series_colors(unique(data_long$name), palette = "all")

  point_size <- normalize_point_size(point_size, series, default = 3)

  data_long <- apply_bubble_sizes(data_long, data, x_var, point_size)

  # Init chart
  plot <- data_long %>% ggplot(aes(x = !!x_sym))

  # Add series to chart
  plot <- add_chart_geom(series,
                         chart_type,
                         data_long,
                         plot,
                         x_var = x_sym,
                         dual = FALSE,
                         point_size = point_size,
                         show_size_legend = bubble_legend %||% TRUE,
                         ...)

  if (any(is.character(point_size))) {
    plot <- plot + scale_size_continuous(range = c(2, 12), guide = if (bubble_legend) "legend" else "none")
  }

  # Scales and theme
  plot <- plot +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, prefix = unit_prefix, percent = percent),
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
  plot +
    annotate(
      "rect",
      xmin = as.Date(min_x),
      xmax = as.Date(max_x),
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.1,
      fill = uhero_colors('gray')
    )
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
