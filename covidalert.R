# load library
library(ggplot2)
library(scales)
library(ggpubr)

# download data
download.file("https://data.ontario.ca/dataset/06a61019-62c1-48d8-8d4d-2267ae0f1144/resource/b792e734-9c69-47d5-8451-40fc85c2f3c6/download/covid_alert_positive_uploads_ontario.csv",
              "data/covid_alert_positive_uploads_ontario.csv")
download.file("https://data.ontario.ca/dataset/06a61019-62c1-48d8-8d4d-2267ae0f1144/resource/37cfeca2-059e-4a5f-a228-249f6ab1b771/download/covid_alert_downloads_canada.csv",
              "data/covid_alert_downloads_canada.csv")
jsonlite::fromJSON("https://api.opencovid.ca/timeseries?loc=ON&stat=cases") %>%
  jsonlite::write_json("data/ccodwg_on_cases.json")

# load data
ca <- read.csv("data/covid_alert_positive_uploads_ontario.csv") %>%
  dplyr::mutate(date = as.Date(date))
dl <- read.csv("data/covid_alert_downloads_canada.csv") %>%
  dplyr::mutate(date = as.Date(date))
on <- jsonlite::fromJSON("data/ccodwg_on_cases.json")$data$cases %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(date <= max(ca$date) & date >= min(ca$date))

# prepare for plotting
scaling_factor <- max(dl$cumulative_total_downloads_canada) / max(on$value_daily) # for second axis
dl$cumulative_total_downloads_canada <- dl$cumulative_total_downloads_canada / scaling_factor
on$fraction_reported <- ca$daily_positive_otks_uploaded_ontario / on$value_daily

# plot: Use of COVID Alert app in Ontario (total cases reported and cumulative app downloads)
ggplot() +
  geom_line(data = on, aes(x = date, y = value_daily, colour = "Total reported")) +
  geom_line(data = ca, aes(x = date, y = daily_positive_otks_uploaded_ontario, colour = "Reported to COVID Alert")) +
  geom_line(data = dl, aes(x = date, y = cumulative_total_downloads_canada), linetype = "dashed") +
  scale_y_continuous(
    labels = label_comma(),
    sec.axis = sec_axis(trans = ~.*scaling_factor, name = "Cumulative app downloads", labels = label_number(suffix = " M", scale = 1e-6))) +
  scale_colour_manual(values = c("Total reported" = "black", "Reported to COVID Alert" = "blue")) +
  labs(title = "Use of COVID Alert app in Ontario", x = "Date", y = "Cases", colour = "") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))
ggsave("figures/covidalert_on_cases_reported.png", width = 5, height = 5)

# plot: Use of COVID Alert app in Ontario (fraction of cases reported)
ggplot() +
  geom_line(data = on, aes(x = date, y = fraction_reported)) +
  labs(title = "Use of COVID Alert app in Ontario", x = "Date", y = "Fraction of cases reported") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))
ggsave("figures/covidalert_on_fraction_cases_reported.png", width = 5, height = 5)