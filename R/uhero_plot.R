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

dual_y_axis_transform <- function(
    data,
    y1_series,
    y2_series,
    y1_limits = NULL,
    y2_limits = NULL
) {

  left_series <- data %>% select(all_of(y1_series))
  right_series <- data %>% select(all_of(y2_series))

  left_max <- set_y_max(y1_limits, left_series)
  left_min <- set_y_min(y1_limits, left_series)
  right_max <- set_y_max(y2_limits, right_series)
  right_min <- set_y_min(y2_limits, right_series)

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

add_geom_line <- function(plot, series_list) {
  plot <- plot +
    geom_line(data = . %>% filter(name %in% series_list), aes(y = value, group = name, color = name))

  plot
}

add_geom_col <- function(plot, series_list) {
  plot <- plot +
    geom_col(data = . %>% filter(name %in% series_list), aes(y = value, fill = name))
}

uhero_draw_dual_y_chart <- function(
    data,
    left_series,
    right_series,
    left_limits = NULL,
    right_limits = NULL,
    x_var,
    left_chart_type = "line",
    right_chart_type = "line",
    left_percent = FALSE,
    right_percent = FALSE
  ) {
  transformation_fns <- dual_y_axis_transform(
    data,
    left_series,
    right_series,
    left_limits,
    right_limits
  )

  rescaled_data <- data %>% mutate(across(all_of(right_series), transformation_fns$rescale))

  rescaled_data_long <- rescaled_data %>% pivot_longer(-x_var, names_to = "name", values_to = "value")

  y1_breaks <- set_breaks(left_limits)

  y2_breaks <- set_breaks(right_limits)

  x_var <- sym(x_var)

  plot <- rescaled_data_long %>% ggplot(aes(x = !!x_var))

  if (right_chart_type == "col") {
    plot <- add_geom_col(plot, right_series)
  }

  if (left_chart_type == "col") {
    plot <- add_geom_col(plot, left_series)
  }

  if (left_chart_type == "line") {
    plot <- add_geom_line(plot, left_series)
  }

  if (right_chart_type == "line") {
    plot <- add_geom_line(plot, right_series)
  }

  plot <- plot +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, percent = left_percent),
      breaks = if (is.null(y1_breaks)) waiver() else y1_breaks,
      limits = if (is.null(left_limits)) NULL else c(left_limits[1], left_limits[2]),
      sec.axis = sec_axis(
        transform = transformation_fns$transform,
        labels = function(x) uhero_scale_nums(x, percent = right_percent),
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
#   left_limits = NULL,
#   right_limits = NULL,
#   "year",
#   "line",
#   "line",
#   FALSE,
#   TRUE
# )

