uhero_color_list <- c(
  `blue` = "#1D667F",
  `orange` = "#F6A01B",
  `green` = "#9BBB59",
  `purple` = "#8064A2",
  `cyan` = "#7EC4CA",
  `gray` = "#505050",
  `red` = "red",
  `light blue` = "#6DA2BC",
  `light orange` = "#FFC593",
  `light green` = "#BADA7C",
  `light purple` = "#B69CD9",
  `light cyan` = "#9CE0E6",
  `light gray` = "#C5C5C5",
  `light red` = "#FF9191"
)

#' Hex codes for a given color in the UHERO colors
#'
#' @param ... Names of the UHERO colors. There are 14 total:
#' "blue", "orange", "green", "purple", "cyan", "gray", "red", "light blue", "light orange", "light green", "light purple",
#' "light cyan", "light gray", and "light red".
#'
#' @return A list of colors with their HEX codes.
#' @export
#' @examples
#' uhero_colors("blue")
#' uhero_colors("blue", "light orange")
uhero_colors <- function(...) {
  colors <- c(...)

  if (is.null(colors)) {
    return (uhero_color_list)
  }

  uhero_color_list[colors]
}

uhero_palettes <- list(
  `primary` = uhero_colors("blue", "orange", "green", "purple", "cyan", "gray", "red"),
  `secondary` = uhero_colors("light blue", "light orange", "light green", "light purple", "light cyan", "light gray", "light red"),
  `all` = uhero_colors()
)

#' A palette generator for UHERO colors.
#'
#' @param palette A string that defaults to "primary". This can also be set to either "secondary" or "all".
#' @param discrete A boolean that defaults to TRUE. Set to FALSE for a continuous scale.
#' @param reverse A boolean that defaults to FALSE. Set to TRUE to reverse the color scale.
#' @param ... Additional parameters that can be passed to \code{colorRampPalette}
#'
#' @export
#'
#' @examples
#' uhero_pal()
#' uhero_pal("secondary", discrete = TRUE, reverse = TRUE)
uhero_pal <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uhero_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  if (discrete) {
    discrete_palette(pal)
  } else {
    colorRampPalette(pal, ...)
  }
}

discrete_palette <- function(pal) {
  function(n) {
    if (n > 14) {
      warning("The UHERO color palette has a maximum of 14 colors.")
    } else {
      palette_colors <- pal
      palette_colors <- unname(unlist(palette_colors))
      color_list <- palette_colors[1:n]
    }
  }
}

get_series_colors <- function(series_names, palette = "all") {
  pal <- uhero_palettes[[palette]]
  if (length(series_names) > length(pal)) {
    warning("More series than colors in the selected palette.")
  }
  setNames(pal[seq_along(series_names)], series_names)
}

apply_color_scales <- function(plot, chart_type, series_colors) {
  use_fill <- any(chart_type %in% c("col", 'bar', "area"))
  use_color <- any(chart_type %in% c("line", "point", "step"))

  if (use_color) {
    plot <- plot + scale_color_manual(values = series_colors)
  }
  if (use_fill) {
    plot <- plot + scale_fill_manual(values = series_colors)
    # Add this line to sync text colors:
    plot <- plot + scale_color_manual(values = series_colors)
  }

  plot
}

#' Applies the UHERO color palette to ggplot's color aesthetic.
#'
#' Uses \code{ggplot2::discrete_scale} for the color aesthetic for discrete scales and \code{ggplot2::scale_colour_gradientn}
#' for the color aesthetic on continuous scales. \code{uhero_scale_color} is available as an alias.
#'
#' @param palette A string that defaults to "primary". This can also be set to either "secondary" or "all".
#' @param discrete A boolean that defaults to TRUE. Set to FALSE for a continuous scale.
#' @param reverse A boolean that defaults to FALSE. Set to TRUE to reverse the color scale.
#' @param ... Additional parameters that can be passed to \code{ggplot2::discrete_scale} or \code{ggplot2::scale_fill_gradientn}
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(hwy, class, color = class)) +
#' ggplot2::geom_point(show.legend = FALSE) +
#' uhero_scale_colour()
uhero_scale_colour <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uhero_pal(palette = palette, discrete = discrete, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", "uhero", palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' Applies the UHERO color palette to ggplot's fill aesthetic.
#'
#' Uses \code{ggplot2::discrete_scale} for the fill aesthetic for discrete scales and \code{ggplot2::scale_fill_gradientn}
#' for the fill aesthetic on continuous scales.
#'
#' @param palette A string that defaults to "primary". This can also be set to either "secondary" or "all".
#' @param discrete A boolean that defaults to TRUE. Set to FALSE for a continuous scale.
#' @param reverse A boolean that defaults to FALSE. Set to TRUE to reverse the color scale.
#' @param ... Additional parameters that can be passed to \code{ggplot2::discrete_scale} or \code{ggplot2::scale_fill_gradientn}
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(hwy, fill = class)) +
#' ggplot2::geom_bar(show.legend = FALSE) +
#' uhero_scale_fill(palette = 'secondary')
uhero_scale_fill <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uhero_pal(palette = palette, discrete = discrete, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", "uhero", palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' Applies the UHERO color palette to ggplot's diverging fill aesthetic.
#'
#' Uses \code{ggplot2::scale_fill_gradient2}
#'
#' @param high A string that defaults to "blue". Can accept any of the other colors in \code{uhero_color_list}.
#' @param low A string that defaults to "orange". Can accept any of the other colors in \code{uhero_color_list}.
#' @param ... Any other parameters that can be passed to \code{ggplot2::scale_fill_gradient2}
#'
#' @export
#'
uhero_scale_fill_diverge <- function(high = "blue", low = "orange", ...) {
  ggplot2::scale_fill_gradient2(high = uhero_colors(high), low = uhero_colors(low), ...)
}

#' Applies the UHERO color palette to ggplot's diverging color aesthetic.
#'
#' Uses \code{ggplot2::scale_color_gradient2}. \code{uhero_scale_color_diverge} is available as an alias.
#'
#' @param high A string that defaults to "blue". Can accept any of the other colors in \code{uhero_color_list}.
#' @param low A string that defaults to "orange". Can accept any of the other colors in \code{uhero_color_list}.
#' @param ... Any other parameters that can be passed to \code{ggplot2::scale_color_gradient2}
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'  x = runif(100),
#'  y = runif(100),
#'  z1 = rnorm(100),
#'  z2 = abs(rnorm(100))
#' )
#' ggplot2::ggplot(df, ggplot2::aes(x, y)) +
#' ggplot2::geom_point(ggplot2::aes(colour = z1)) +
#' uhero_scale_colour_diverge()
uhero_scale_colour_diverge <- function(high = "blue", low = "orange", ...) {
  ggplot2::scale_colour_gradient2(high = uhero_colors(high), low = uhero_colors(low), ...)
}

uhero_scale_color <- uhero_scale_colour
uhero_scale_color_diverge <- uhero_scale_colour_diverge
