# UHERO Theme

This package currently contains functions to apply the UHERO style guide
to plots made with [ggplot](https://ggplot2.tidyverse.org/) or [highcharter](https://jkunst.com/highcharter/).

To install the package, run the following lines:

```
# install "devtools" package if needed
# install.packages('devtools')
library(devtools)
devtools::install_github('uhero/uherotheme')
```

Examples of charts using functions from the UHERO Theme package
can be found in `sample_data/sample_data_figures.R`. Documentation is also
available in R by using R's help operators (`help` or `?`).

## Formatting and Styling

### `uhero_theme`

The `uhero_theme()` function modifies ggplot's minimal theme to fit the style
used in UHERO reports and presentations. The function takes one optional numerical
parameter, `font_size`, which defaults to `9`. The theme modifies the font face,
font size, and removes axis lines, titles, tick marks, grid lines, and background.
Additional changes can be made by appending ggplot's `theme()` function.

```
nonfarm_payrolls_plot <- ggplot(nonfarm_payrolls_long, aes(x = Date, y = value, group = name, color = name)) +
  geom_line() +
  uhero_theme() +
  theme(legend.position = 'none', plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
```

### `uhero_scale_nums`

`uhero_scale_nums` formats tick labels for axes with continuous scales. This can
be passed to the `labels` parameter of ggplot scales like `scale_x_continous` or
`scale_y_continuous`. If the maximum value of the scale is at least 1,000, the
labels will be scaled down with the appropriate suffix added on (i.e., 'K'
for thousands, 'M' for millions, and 'B' for billion). The function takes the
following parameters: `x` which is a vector passed in from the `labels` parameter
of the ggplot scale function. `scale_limit` which defaults to the maximum value
of `x`. `prefix` which is optional and defaults to an empty string (can be used to
add a prefix like "$" to the front of the label). `percent` is a boolean that
defaults to `FALSE`; set it to `TRUE` to multiply values by 100 add a "%" to the end of a label. `...`
which are ... arguments that can be passed to R's `format()`. Any prefix or suffix
will only be applied to the last/max value label.

```
vexp_plot <- ggplot(vexp_long, aes(x = name, y = value, fill = Country)) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = function(x) uhero_scale_nums(x, prefix = "$"),
  limits = c(0, 2000), breaks = seq(0, 2000, by = 200)) +
  uhero_theme() +
  theme(
    legend.position = 'none',
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )
```

### `uhero_draw_ggplot` and `uhero_draw_dual_y_ggplot`

`uhero_draw_ggplot` and `uhero_draw_dual_y_ggplot` are helper functions to draw ggplot2 charts
with the UHERO theme applied. Currently accepted charts types are "line", "bar", "col", and "scatter".
The chart types apply the ggplot geoms `geom_line`, `geom_bar`, `geom_col`, and `geom_point`
- `geom_bar` Calculates bar heights using `stat_count`, i.e. counts the number of cases at each category
- `geom_col` Uses `stat_idenity`, i.e. uses the y values to determine the height of the bar
When choosing the scatter plot type, `uhero_draw_ggplot` and the `y1` and `y2` lists of `uhero_draw_dual_y_ggplot`
accept a `point_size` parameter. If omitted, the plot defaults to using a fixed point size of 3. `point_size` can accept
a number value to change the fixed size or a column name to dynamically scale the points for a bubble plot.
Note: Additional parameters like `stat = "identity"` can be passed to the draw ggplot functions
to allow bar charts to draw the same as a col. See code block example below or run
`?uhero_draw_ggplot` / `?uhero_draw_dual_y_ggplot` for more details on the parameters.

```
transactions_plot2 <- uhero_draw_dual_y_ggplot(
  data = transactions,
  x_var = "year",
  y1 = list(
    series = c("Condominium Transactions", "Single-family Transactions"),
    chart_type = "line",
    limits = c(0, 15000, 5000),
    percent = FALSE,
    unit_prefix = "$"
  ),
  y2 = list(
    series = c("Interest Rate"),
    chart_type = "line",
    limits = c(0, .1, .05),
    percent = TRUE
  )
)$plot

vis_market_plot2 <- uhero_draw_ggplot(
  data = vis_market_forecast,
  series = c("US", "JP", "Rest of the World"),
  x_var = "Date",
  chart_type = "line",
  percent = FALSE
)$plot

# Alternative to using 'col' chart_type
uhero_draw_dual_y_ggplot(
  df,
  x_var = "x",
  y1 = list(
    series = c("y")
  ),
  y2 = list(
    chart_type = "bar"
    series = c("z")
  ),
  stat = "identity"
)
```
Additional examples can be found in `sample_data/sample_data_figures.R`. Both functions return
the plot object and the long form of the data generated for the plot.

### `add_text_labels`
By default, `uhero_draw_ggplot` and `uhero_draw_dual_y_ggplot`, will place a lenged in the
least crowded quadrant of the plot. The legend can be replaced in favor of text labels 
by using `add_text_labels`. Currently, this works best for line charts.
`add_text_labels(vis_market_plot2)`

### `add_forecast_shading`
Add a shaded region to indicate forecasted data with `add_forecast_shading`. This accepts the
ggplot object, the start, and the end of the forecast region.
`add_forecast_shading(vis_market_plot2, as.POSIXct("2024-04-01"), as.POSIXct("2028-10-01"))`

### `uhero_colors`

`uhero_colors` can be used to return the HEX codes for the colors in the UHERO palette. There are 14 colors available:
- "blue"
- "orange"
- "green"
- "purple"
- "cyan"
- "gray"
- "red"
- "light blue"
- "light orange"
- "light green"
- "light purple"
- "light cyan"
- "light gray"
- "light red"
```
uhero_colors("blue")
uhero_colors("blue", "light orange")
uhero_colors()
```

### `uhero_palettes`
`uhero_palettes` is a list of the available palettes.
- `primary`: `uhero_colors("blue", "orange", "green", "purple", "cyan", "gray", "red")`
- `secondary`: `uhero_colors("light blue", "light orange", "light green", "light purple", "light cyan", "light gray", "light red")`
- `all`: returns all 14 colors

### `uhero_scale_colour` / `uhero_scale_color`
`uhero_scale_color` applies the UHERO color palette to ggplot's color aesthetic. This function uses ggplot's `discrete_scale` for discrete scales and `scale_colour_gradientn` for continuous scales.
The function accepts the following parameters:
- `palette` A string which defaults to "primary". Other accepted values are "secondary" and "all".
- `discrete` is a boolean which defaults to `TRUE`. Set this to `FALSE` for continuous scales.
- `reverse` is a boolean which defaults to `FALSE`. Set to `TRUE` to reverse the colors in the palette.
- `...` Any other parameters than can be accepted by either ggplot's `discrete_scale` or `scale_colour_gradientn`.
```
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(hwy, class, color = class)) +
ggplot2::geom_point(show.legend = FALSE) +
uhero_scale_colour()
```

### `uhero_scale_fill`
`uhero_scale_fill` applies the UHERO color palette to ggplot's fill aesthetic. This function uses ggplot's `discrete_scale` for discrete scales and `scale_fill_gradientn` for continuous scales.
The function accepts the following parameters:
- `palette` A string which defaults to "primary". Other accepted values are "secondary" and "all".
- `discrete` is a boolean which defaults to `TRUE`. Set this to `FALSE` for continuous scales.
- `reverse` is a boolean which defaults to `FALSE`. Set to `TRUE` to reverse the colors in the palette.
- `...` Any other parameters than can be accepted by either ggplot's `discrete_scale` or `scale_fill_gradientn`.
```
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(hwy, fill = class)) +
ggplot2::geom_bar(show.legend = FALSE) +
uhero_scale_fill(palette = 'secondary')
```

### `uhero_scale_colour_diverge` / `uhero_scale_color_diverge`
Applies the UHERO color palette to ggplot's diverging color aesthetic. The following parameters are accepted:
- `high` A string that accepts one of the 14 colors in the UHERO color list. Defaults to "blue".
- `low` A string that accepts one of the 14 colors in the UHERO color list. Defaults to "orange".
- `...` Any other parameters that can be passed to `scale_color_gradient2`
```
set.seed(1)
df <- data.frame(x = runif(100), y = runif(100), z1 = rnorm(100), z2 = abs(rnorm(100)))
ggplot2::ggplot(df, ggplot2::aes(x, y)) +
ggplot2::geom_point(ggplot2::aes(colour = z1)) +
uhero_scale_colour_diverge()
```

### `uhero_scale_fill_diverge`
Applies the UHERO color palette to ggplot's diverging fill aesthetic. The following parameters are accepted:
- `high` A string that accepts one of the 14 colors in the UHERO color list. Defaults to "blue".
- `low` A string that accepts one of the 14 colors in the UHERO color list. Defaults to "orange".
- `...` Any other parameters that can be passed to `scale_fill_gradient2`

## Exporting

There are 2 helper functions to modify the size of the viewport to preview
charts in R Studio. These are intended to be used to preview placement of elements
like labels and annotations for plots that are being exported for use in a report layout.

### `draw_fcast_layout`

`draw_fcast_layout` modifies the size of the viewport to preview charts in
R Studio. Forecast charts should be exported at a size of 4.5 x 2.45 inches. This
function displays the plot at that size so that labels and annotations can be
placed properly. The function accepts 3 parameters. `plot` which is the ggplot
object that is being drawn. `w` defaults to `4.5`, and `h` defaults to `2.45`. There
may be cases where some charts would need to be a different size, so the `w` and `h`
parameters can be adjusted.

```
df <- data.frame(x = rnorm(10) * 100000, y = seq(0, 1, length.out = 10))
plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
draw_fcast_layout(plot)
```

### `draw_report_layout`

`draw_report_layout` modifies the size of the viewport to preview charts in
R Studio. Report charts should be exported at a size of 5.6931 x 4 inches. This
function displays the plot at that size so that labels and annotations can be
placed properly. The function accepts 3 parameters. `plot` which is the ggplot
object that is being drawn. `w` defaults to `5.6931`, and `h` defaults to `4`. There
may be cases where some charts would need to be a different size, so the `w` and `h`
parameters can be adjusted.

```
df <- data.frame(x = rnorm(10) * 100000, y = seq(0, 1, length.out = 10))
plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
draw_report_layout(plot)
```

### `export_fcast_layout`

`export_fcast_layout` uses ggplot's `ggsave` function to export a copy of the chart.
`export_fcast_layout` takes the following parameters:

- `file_name`: A string for the file name including the extension
- `forecast_plot`: The ggplot plot object
- `w` Width of the exported image, defaults to 4.5
- `h` Height of the exported image, defaults to 2.45
- `u` A string for the units, defaults to "in" for inches
- `...` Additional parameters that can be passed to ggplot2::ggsave
  If exporting for use in a forecast layout, please use either `.svg`, `.pdf`, or `.eps`
  as the file extension. The dimensions should be 4.5 x 2.45 inches, but the `w` and `h`
  parameters can be adjusted if necessary.

```
df <- data.frame(x = rnorm(10) * 100000, y = seq(0, 1, length.out = 10))
plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
export_fcast_layout('plot.svg', plot)
```

### `export_report_layout`

`export_report_layout` uses ggplot's `ggsave` function to export a copy of the chart.
`export_report_layout` takes the following parameters:

- `file_name`: A string for the file name including the extension
- `plot`: The ggplot plot object
- `w` Width of the exported image, defaults to 5.6931
- `h` Height of the exported image, defaults to 4
- `u` A string for the units, defaults to "in" for inches
- `...` Additional parameters that can be passed to ggplot2::ggsave
  If exporting for use in a report layout, please use either `.svg`, `.pdf`, or `.eps`
  as the file extension. The dimensions should be 5.6931 x 4 inches, but the `w` and `h`
  parameters can be adjusted if necessary.

```
df <- data.frame(x = rnorm(10) * 100000, y = seq(0, 1, length.out = 10))
plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
export_report_layout('plot.svg', plot)
```

### `export_plot`

`export_plot` uses ggplot's `ggsave` function to export a copy of the chart.
`export_report_layout` takes the following parameters:

- `file_name`: A string for the file name including the extension
- `plot`: The ggplot plot object
- `w` Width of the exported image, defaults to 1920
- `h` Height of the exported image, defaults to 1080
- `u` A string for the units, defaults to "px" for pixels
- `...` Additional parameters that can be passed to ggplot2::ggsave

```
df <- data.frame(x = rnorm(10) * 100000, y = seq(0, 1, length.out = 10))
plot <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
export_report_layout('plot.png', plot)
```
## Highcharter
### `uhero_hc`

`uhero_hc` can be passed to highcharter's `hc_add_theme()` to apply UHERO styling to plots made with Highcharter. 
Examples applying the theme to highcharter figures can be found in `sample_data/sample_data_figures_HC.R`

## Logos
The UHERO Analytics logo and other variations of it can be found by running `uhero_logos` in the R console. 
The list returns the currently available topic areas and the URL to the logo file. The availble research areas are: 
`analytics`, `economy`, `energy`, `environment`, `forecast`, `health`, `housing`, and `tax`.
