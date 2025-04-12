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
    mutate(label = if_else(!!sym(x_var) == max(!!sym(x_var)), as.character(name), NA_character_))

  y1_breaks <- set_breaks(y1_limits)

  y2_breaks <- set_breaks(y2_limits)

  x_var <- sym(x_var)

  plot <- rescaled_data_long %>% ggplot(aes(x = !!x_var, label = name))

  if (y2_chart_type == "col") {
    plot <- add_geom_col(plot, y2_series)
  }

  if (y1_chart_type == "col") {
    plot <- add_geom_col(plot, y1_series)
  }

  if (y1_chart_type == "line") {
    plot <- add_geom_line(plot, y1_series)
  }

  if (y2_chart_type == "line") {
    plot <- add_geom_line(plot, y2_series)
  }

  plot <- plot +
    geom_text_repel(aes(label = label, y = value, color = name), nudge_x = 1, na.rm = TRUE) +
    scale_y_continuous(
      labels = function(x) uhero_scale_nums(x, prefix = y1_unit_prefix, percent = y1_percent),
      breaks = if (is.null(y1_breaks)) waiver() else y1_breaks,
      limits = if (is.null(y1_limits)) NULL else c(y1_limits[1], y1_limits[2]),
      sec.axis = sec_axis(
        transform = transformation_fns$transform,
        labels = function(x) uhero_scale_nums(x, y2_unit_prefix, percent = y2_percent),
        breaks = if (is.null(y2_breaks)) waiver() else y2_breaks,
      ),
    ) +
    uhero_scale_color() +
    uhero_scale_fill("secondary")

  plot
}

uhero_draw_dual_y_chart(
  transactions,
  c("Condominium Transactions", "Single-family Transactions"),
  c("Interest Rate"),
  y1_limits = NULL,
  y2_limits = NULL,
  "year",
  "line",
  "line",
  FALSE,
  TRUE
)

# svglite("plot.svg")
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
# ) + theme_set(theme_minimal(base_family = "Lora"))
# dev.off()
