library(highcharter)
library(readxl)
library(here)
library(tidyr)
library(dplyr)
library(lubridate)

# Load in data - Data taken from 24Q2 Forecast
nonfarm_payrolls <- read_excel(here("./sample_data/SampleData.xlsx"), sheet = 1)
prefire_payrolls <- read_excel(here("./sample_data/SampleData.xlsx"), sheet = 2)
job_open_unemp <- read_excel(here("./sample_data/SampleData.xlsx"), sheet = 3)
mortgages <- read_excel(here("./sample_data/SampleData.xlsx"), sheet = 4)
vis_market_forecast <- read_excel(here("./sample_data/SampleData.xlsx"), sheet = 5)
vexp <- read_excel(here("./sample_data/SampleData.xlsx"), sheet = 6)
transactions <- read_excel(here("./sample_data/SampleData.xlsx"), sheet = 7)

# Convert data into long format
nonfarm_payrolls_long <- nonfarm_payrolls %>%
  pivot_longer(
    `State`:`Maui`
  )

prefire_payrolls_long <- prefire_payrolls %>%
  pivot_longer(
    `Maui County`:`All Other Counties`
  )
prefire_payrolls_long$`Perc Change Pre Fire` <- factor(prefire_payrolls_long$`Perc Change Pre Fire`, levels = unique(prefire_payrolls_long$`Perc Change Pre Fire`))

job_open_unemp_long <- job_open_unemp %>%
  pivot_longer(
    `Job Openings`:`Unemployed Persons`
  )

mortgages_long <- mortgages %>%
  pivot_longer(
    `<=3%`:`>6%`
  )
mortgages_long$name <- factor(mortgages_long$name, levels = c("<=3%", "3.01%-4%", "4.01%-5%", "5.01%-6%", ">6%"))

vis_market_forecast_long <- vis_market_forecast %>%
  pivot_longer(
    `US`:`Rest of the World`
  )

vexp_long <- vexp %>%
  pivot_longer(
    `2023 February`:`2024 February`
  )

transactions_long <- transactions %>%
  pivot_longer(
    `Condominium Transactions`:`Interest Rate`
  )

vexp_long$Country <- factor(vexp_long$Country, labels = c("Other Visitors", "Canada", "Japan", "United States"))
nonfarm_payrolls_long$Date = as.Date(nonfarm_payrolls_long$Date, tz = "GMT")
job_open_unemp_long$Date = as.Date(job_open_unemp_long$Date, tz = "GMT")
mortgages_long$Month = as.Date(mortgages_long$Month, tz = "GMT")
vis_market_forecast_long$Date = as.Date(vis_market_forecast_long$Date, tz = "GMT")
transactions_long$year = as.Date(ymd(transactions_long$year, truncated = 2), tz = "GMT")

# Nonfarm Payrolls
# Remove the hc_exporting() function to remove the exporting menu from the chart.
nonfarm_payrolls_hc <- highchart() %>%
  hc_add_series(
    nonfarm_payrolls_long,
    "line",
    hcaes(
      x = Date,
      y = value,
      group = name
    ),
    marker = list(
      enabled = FALSE
    )
  ) %>%
  hc_xAxis(
    type = "datetime"
  ) %>%
  hc_exporting(
    enabled = TRUE
  ) %>%
  hc_tooltip(
    valueDecimals = 2
  ) %>%
  hc_add_theme(uhero_hc)

# Prefire Payrolls
prefire_payrolls_hc <- highchart() %>%
  hc_add_series(
    prefire_payrolls_long,
    "bar",
    hcaes(
      x = `Perc Change Pre Fire`,
      y = value,
      group = name
    )
  ) %>%
  hc_xAxis(
    type = "category",
    reversed = FALSE
  ) %>%
  hc_yAxis(
    labels = list(
      formatter = JS(
        "function() {
          if (this.isLast) {
            return `${this.value * 100}%`
          }
          return this.value * 100
        }"
      )
    )
  ) %>%
  hc_tooltip(
    valueDecimals = 2
  ) %>%
  hc_add_theme(uhero_hc)

job_open_hc <- highchart() %>%
  hc_add_series(
    job_open_unemp_long,
    "line",
    hcaes(
      x = Date,
      y = value,
      group = name
    )
  ) %>%
  hc_xAxis(
    type = "datetime"
  ) %>%
  hc_yAxis(
    max = 100,
    labels = list(
      formatter = JS(
        "function() {
          if (this.isLast) {
            return `${this.value}K`
          }
          return this.value
        }"
      )
    )
  ) %>%
  hc_tooltip(
    valueDecimals = 2
  ) %>%
  hc_add_theme(uhero_hc)

mortgages_hc <- highchart() %>%
  hc_add_series(
    mortgages_long,
    "column",
    stacking = "percent", # or "normal" ?
    hcaes(
      x = Month,
      y = value,
      group = name,
    )
  ) %>%
  hc_xAxis(
    type = "datetime"
  ) %>%
  hc_yAxis(
    max = 100,
    labels = list(
      formatter = JS(
        "function() {
          if (this.isLast) {
            return `${this.value}%`
          }
          return this.value
        }"
      )
    )
  ) %>%
  hc_add_theme(uhero_hc)

vis_market_forecast_hc <- highchart() %>%
  hc_add_series(
    vis_market_forecast_long,
    "line",
    hcaes(
      x = Date,
      y = value,
      group = name,
    ),
    marker = list(
      enabled = FALSE
    )
  ) %>%
  hc_xAxis(
    type = "datetime"
  ) %>%
  hc_yAxis(
    max = 120
  ) %>%
  hc_add_theme(uhero_hc)

vexp_hc <- highchart() %>%
  hc_add_series(
    vexp_long,
    "column",
    stacking = "normal",
    hcaes(
      x = name,
      y = value,
      group = Country
    )
  ) %>%
  hc_xAxis(
    type = "category"
  ) %>%
  hc_add_theme(uhero_hc)

# The chart below contains an example of adding a logo to the chart.
# Documentation for Highcharts' renderer image can be found at https://api.highcharts.com/class-reference/Highcharts.SVGRenderer#image
# Run uhero_logos in the console to get back a list of all the currently available logos and their URLs.
transactions_hc <- highchart() %>%
  hc_add_series(
    filter(transactions_long, name == "Condominium Transactions" | name == "Single-family Transactions"),
    "line",
    hcaes(
      x = year,
      y = value,
      group = name
    ),
    marker = list(
      enabled = FALSE
    )
  ) %>%
  hc_add_series(
   filter(transactions_long, name == "Interest Rate"),
    "line",
   yAxis = 1,
    hcaes(
      x = year,
      y = value,
      group = name
    ),
   marker = list(
     enabled = FALSE
   )
  ) %>%
  hc_chart(
    events = list(
      load = JS(
        "function() {
          this.renderer.image('https://uhero.hawaii.edu/wp-content/uploads/2024/07/analytics.png', 25, 0, 589/6).add()
        }"
      )
    )
  ) %>%
  hc_exporting(
    enabled = TRUE
  ) %>%
  hc_xAxis(
    type = "datetime"
  ) %>%
  hc_yAxis_multiples(
    list(
      title = "",
      labels = list(
        formatter = JS(
          "function() {
          if (this.isLast && this.value > 1000) {
            return Highcharts.numberFormat(this.value / 1000, 0) + 'K'
          }
          if (this.value > 1000) {
            return Highcharts.numberFormat(this.value / 1000, 0)
          }
          return this.value
        }"
        )
      )
    ),
    list(
      opposite = TRUE,
      title = "",
      labels = list(
        formatter = JS(
          "function() {
          if (this.isLast) {
            return `${this.value}%`
          }
          return this.value
        }"
        )
      )
    )
  ) %>%
  hc_tooltip(
    valueDecimals = 2
  ) %>%
  hc_add_theme(uhero_hc)

