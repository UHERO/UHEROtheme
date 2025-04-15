## THEME FOR HIGHCHARTER
uhero_hc <- hc_theme(
  # colors = c(
  #   "#1D667F",
  #   "#F6A01B",
  #   "#9BBB59",
  #   "#8064A2",
  #   "#7EC4CA",
  #   "#505050",
  #   "red",
  #   "#6DA2BC",
  #   "#FFC593",
  #   "#BADA7C",
  #   "#B69CD9",
  #   "#9CE0E6",
  #   "#C5C5C5",
  #   "#FF9191"
  #   ),
  colors = as.vector(uhero_color_list),
  chart = list(
    backgroundColor = "transparent",
    style = list(
      fontFamily = "Open Sans",
      color = "#505050"
    )
  ),
  title = list(
    align = "center",
    style = list(
      fontWeight = "bold"
    )
  ),
  subtitle = list(
    align = "center"
  ),
  yAxis = list(
    gridLineWidth = 0
  )
)

fcast_line_perc_hc <- function(dataset, forecast_start, forecast_end) {
  highchart() %>%
    hc_add_series(
      dataset,
      "line",
      hcaes(
        x = dataset$Date,
        y = dataset$value,
        group = dataset$name
      ),
      marker = list(
        enabled = FALSE
      )
    ) %>%
    hc_xAxis(
      type = "datetime",
      plotBands = list(
        list(
          from = datetime_to_timestamp(as.Date(forecast_start)),
          to = datetime_to_timestamp(as.Date(forecast_end)),
          color = uhero_color_list[["light gray"]]
        )
      )
    ) %>%
    hc_yAxis(
      labels = list(
        formatter = JS("function() {
          const label = this.axis.defaultLabelFormatter.call(this);
          if (this.axis.max === +label) {
            return `${label}%`;
          }
          return `${label}`;
        }")
      )
    ) %>%
    hc_legend(
      floating = TRUE,
      align = "right",
      verticalAlign = "middle",
      layout = "vertical"
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_tooltip(
      valueDecimals = 2
    ) %>%
    hc_add_theme(uhero_hc)
}
