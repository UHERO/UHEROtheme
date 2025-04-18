library(readxl)
library(here)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(dplyr)

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
  mutate(`Maui County` = prefire_payrolls$`Maui County` * 100, `All Other Counties` = prefire_payrolls$`All Other Counties` * 100) %>%
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


# Figures
#For use outside of a report layout, i.e. presentation
nonfarm_payrolls_plot <- ggplot(nonfarm_payrolls_long, aes(x = Date, y = value, group = name, color = name)) +
  geom_line() +
  annotate(
    "text",
    x = as.Date(c("2023-04-01", "2023-10-01", "2020-08-01", "2021-06-01", "2023-09-01")),
    y = c(100, 97, 76, 75, 84),
    label = c("Hawaii", "Honolulu", "State", "Kauai", "Maui"),
    color = c(uhero_colors("blue"), uhero_colors("orange"), uhero_colors("light blue"), uhero_colors("green"), uhero_colors("purple")),
    size = 9 / .pt,
    family = "opensans"
    ) +
  coord_cartesian(clip = 'off') +
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") +
  uhero_theme() +
  uhero_scale_colour() +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

prefire_payrolls_plot <- ggplot(prefire_payrolls_long, aes(x = `Perc Change Pre Fire`, y = value, group = name, fill = name)) +
  geom_col(position = position_dodge2(reverse=TRUE)) +
  scale_y_continuous(labels = function(x) uhero_scale_nums(x, percent = TRUE), limits = c(-20, 20)) +
  annotate(
    "text", y = 10,
    x = c(2, 1),
    label = c("All Other Counties", "Maui County"),
    color = c(uhero_colors("blue"), uhero_colors("orange")),
    hjust = 0,
    family = "opensans",
    size = 9 / .pt) +
  coord_flip(clip = "off") +
  uhero_theme() +
  uhero_scale_colour() +
  uhero_scale_fill() +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

job_open_plot <- ggplot(job_open_unemp_long, aes(x = Date, y = value, group = name, color = name)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(limits = as.Date(c("2015-03-01", "2024-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  annotate(
    "text", x = as.Date(c("2021-01-01", "2017-01-01")),
    y = c(75,45),
    label = c("Unemployed Persons", "Job Openings"),
    color = c(uhero_colors("orange"), uhero_colors("blue")),
    family = "opensans",
    size = 9 / .pt,
    hjust = 0) +
  uhero_scale_colour() +
  uhero_theme() +
  theme(
    legend.position = 'none',
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )

mortgages_plot <- ggplot(mortgages_long, aes(x = Month, y = value, group = name, fill = name)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = function(x) uhero_scale_nums(x, percent = TRUE)) +
  annotate(
    "text",
    x = as.Date(c("2018-01-01", "2018-08-01", "2019-10-01", "2020-12-01", "2022-01-01")),
    y = 105,
    label = c("≤3%", "3.01%-4%", "4.01%-5%", "5.01%-6%", ">6%"),
    color = c(uhero_colors("blue"), uhero_colors("orange"), uhero_colors("green"), uhero_colors("purple"), uhero_colors("light blue")),
    family = "opensans",
    size = 9 / .pt,
    hjust = 0
    ) +
  coord_cartesian(clip = "off") +
  uhero_scale_fill() +
  uhero_theme() +
  theme(
    legend.position = "none",
  )

vis_market_forecast_plot <- ggplot(vis_market_forecast_long, aes(x = Date, y = value, group = name, color = name)) +
  geom_line() +
  # Add shading to indicate the forecast area
  annotate("rect", xmin = as.Date("2024-04-01"), xmax = as.Date("2028-10-01"), ymin = 0, ymax = 120, alpha = .1, fill = uhero_colors('gray')) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  annotate(
    "text", x = as.Date(c("2021-06-01", "2023-01-01", "2023-11-01")),
    y = c(119,95,52),
    label = c("US", "Rest of the World", "Japan"),
    color = c(uhero_colors("green"), uhero_colors("orange"), uhero_colors("blue")),
    family = "opensans",
    size = 9 / .pt
  ) +
  uhero_scale_colour() +
  uhero_theme() +
  theme(
    legend.position = 'none',
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )


colors <- c("United States" = "#1D667F", "Japan" = "#F6A01B", "Canada" = "#9BBB59", "Other Visitors" = "#C5C5C5")
vexp_long <- transform(vexp_long, name_num = ifelse(name == "2023 February", as.numeric(factor(name)) + .25, as.numeric(factor(name)) - .25))
vexp_plot <- ggplot(vexp_long, aes(x = name, y = value, fill = Country)) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = function(x) uhero_scale_nums(x, prefix = "$"), limits = c(0, 2000), breaks = seq(0, 2000, by = 200)) +
  scale_fill_manual(values = colors) +
  geom_line(aes(x = name_num), position = position_stack(), color = "#505050") +
  annotate(
    "text",
    x = c(2.3, 2.3, 2.3, 2.3),
    y = c(550, 400, 250, 100),
    label = c("Other Visitors", "Canada", "Japan", "United States"),
    color = rev(colors),
    family = "opensans",
    size = 9 /.pt,
    hjust = 0
  ) +
  uhero_theme() +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = 'none',
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )

transactions_plot <- ggplot(transactions_long, aes(x = year, group = name, color = name, label = name)) +
  geom_line(data = filter(transactions_long, name == "Condominium Transactions" | name == "Single-family Transactions"), aes(y = value)) +
  geom_line(data = filter(transactions_long, name == "Interest Rate"), aes(y = value / 0.0007), linetype="dashed") +
  scale_y_continuous(
    limits = c(0, 15000),
    breaks = seq(0, 15000, by = 5000),
    labels = function(x) uhero_scale_nums(x),
    sec.axis = sec_axis(~ . * 0.0007, breaks = seq(0, 10, by = 5), labels = function(x) uhero_scale_nums(x, percent = TRUE))
  ) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  annotate(
    "text",
    x = c(2014.5, 2020, 2013.5),
    y = c(14000, 3500, 4600),
    label = c("Condominim Transactions", "Interest Rates", "Single-family Transactions"),
    color = c(uhero_colors("blue"), uhero_colors("orange"), uhero_colors("green")),
    hjust = 0,
    size = 9 / .pt,
    family = "opensans"
  ) +
  uhero_scale_colour() +
  coord_cartesian(clip = "off") +
  uhero_theme() +
  theme(
    legend.position = 'none',
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )

# Use draw_fcast_layout function to preview how the chart would look in a UHERO Forecast report
draw_fcast_layout(nonfarm_payrolls_plot)
draw_fcast_layout(prefire_payrolls_plot)
draw_fcast_layout(job_open_plot)
draw_fcast_layout(mortgages_plot)
draw_fcast_layout(vis_market_forecast_plot)
draw_fcast_layout(vexp_plot)
draw_fcast_layout(transactions_plot)

# Use export_fcast_layout to export chart for the UHERO Forecast report layout
# Please use a .svg extension if exporting for the report layout
export_fcast_layout('transactions.svg', transactions_plot)
