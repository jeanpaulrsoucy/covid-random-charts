####### RANDOM COVID-19 PLOTS #######

# make directories
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

# load packages
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)

### PHAC - HOSPITALIZATIONS/ICU BY AGE GROUP ###

# read data
# https://www150.statcan.gc.ca/n1/pub/13-26-0003/132600032020001-eng.htm
dat <- read_csv(unz("data/COVID19-eng.zip", "COVID19-eng.csv"))

# process data
dat <- dat %>%
  mutate(
    COV_AGR = case_when(
      COV_AGR == 1 ~ "0-19",
      COV_AGR == 2 ~ "20-29",
      COV_AGR == 3 ~ "30-39",
      COV_AGR == 4 ~ "40-49",
      COV_AGR == 5 ~ "50-59",
      COV_AGR == 6 ~ "60-69",
      COV_AGR == 7 ~ "70-79",
      COV_AGR == 8 ~ "80+",
      COV_AGR == 99 ~ "Not stated"
    ),
    COV_HSP = case_when(
      COV_HSP == 1 ~ "Hospitalized - ICU",
      COV_HSP == 2 ~ "Hospitalized - Non-ICU",
      COV_HSP == 3 ~ "Not Hospitalized",
      COV_HSP == 9 ~ "Not stated/Unknown"
    )
  )

# # filter out unknown episode weeks and sort by date
# dat <- dat %>%
#   filter(COV_EW != 99)

# calculate proper date and outcome variables
dat <- dat %>%
  mutate(
    episode_date = case_when(
      COV_EY == 99 | COV_EW == 99 ~ as.Date(NA_character_),
      COV_EY == 20 ~ as.Date("2019-12-29") + 7 * COV_EW,
      COV_EY == 21 ~ as.Date("2020-12-27") + 7 * COV_EW
    )
  )

# aggregate data and calculate values
hosp <- dat %>%
  select(COV_AGR, COV_HSP) %>%
  count(COV_AGR, COV_HSP) %>%
  pivot_wider(names_from = COV_HSP, values_from = n) %>%
  rowwise() %>%
  mutate(n = sum(c(`Hospitalized - ICU`, `Hospitalized - Non-ICU`, `Not Hospitalized`, `Not stated/Unknown`))) %>%
  mutate(
    `% Hospitalized - ICU` = `Hospitalized - ICU` / n * 100,
    `% Hospitalized - Non-ICU` = `Hospitalized - Non-ICU` / n * 100,
    `% Not Hospitalized` = `Not Hospitalized` / n * 100,
    `% Not stated/Unknown` = `Not stated/Unknown` / n * 100
  )

# write table
write.csv(hosp, paste0("tables/hosp_status_by_age_group_", max(dat$episode_date, na.rm = TRUE), ".csv"), row.names = FALSE)

# clean up
rm(dat, hosp)

### ICES - WEEKLY % POSITIVITY (NON-LTC) BY PHU AND AGE GROUP

# function to read relevant data from all sheets
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  s <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x, skip = 25))
  names(s) <- sheets
  s
}

# load data
# https://www.ices.on.ca/~/media/Files/COVID-19/ICES-COVID19-Testing-Data_PHUxAge-Groups-percent-positivity.ashx?la=en-CA
sheets <- read_excel_allsheets("data/ICES-COVID19-Testing-Data_PHUxAge-Groups-percent-positivity.xlsx")

# drop metadata sheets
sheets <- sheets[!names(sheets) %in% c("Terms of Reference", "Acknowledgement & Disclaimers", "Data Notes")]

# create column to identify PHU
sheets <- lapply(seq_along(sheets), function(x) {
  sheets[[x]]["PHU"] <- names(sheets)[x]
  return(sheets[[x]])
})

# combine sheets into one
sheets <- bind_rows(sheets)

# save max date
max_date <- max(as.Date(sheets$`End date of week`))

# convert variables
sheets <- sheets %>%
  rename(
    age_group = `Age group (years)`,
    week_begin = `Start date of week`,
    week_end = `End date of week`,
    overall_pos = `Overall - % positivity`,
    overall_tested_per_100k = `Overall - number tested per 100,000 population`,
    non_ltc_pos = `NON-LTC - % positivity`,
    non_ltc_tested_per_100k = `NON-LTC - number tested per 100,000 population`) %>%
  mutate(
    week_begin = as.Date(week_begin),
    week_end = as.Date(week_end),
    overall_pos = as.numeric(overall_pos) * 100,
    overall_tested_per_100k = as.integer(overall_tested_per_100k),
    non_ltc_pos = as.numeric(non_ltc_pos) * 100,
    non_ltc_tested_per_100k = as.integer(non_ltc_tested_per_100k),
    PHU = factor(PHU)
  )

# select relevant age groups
sheets <- sheets %>%
  mutate(
    age_group = factor(age_group,
                       levels = c(
                         "<2", "2-3", "4-8 (JK to Grade 3)",
                         "9-13 (Grade 4-8)", "14-17", "18-22",
                         "23-29", "30-39", "40-49", "50-59",
                         "60-69", "70-79", "80-89", "90+"
                       ))) %>%
  filter(!is.na(age_group))

# generate groups
sheets <- sheets %>%
  mutate(grp = paste(PHU, age_group, sep = ", "))

# identify top 5 groups
top_5 <- sheets %>%
  filter(week_end == max(week_end)) %>%
  slice_max(non_ltc_pos, n = 5) %>% pull(grp)
sheets <- sheets %>%
  mutate(top_5 = ifelse(grp %in% all_of(top_5), 1, 0)) # all_of() refers to external vector

# plot weekly % positivity (non-LTC) by PHU and age group (highlighting top 5)
p <- ggplot(data = NULL, aes(x = week_end, y = non_ltc_pos, group = grp)) +
  geom_line(data = sheets, colour = "grey", alpha = 0.4) +
  geom_point(data = sheets %>% filter(week_end == max(week_end) & top_5 == 1), colour = "red", size = 3, shape = 4) +
  geom_label_repel(data = sheets %>% filter(week_end == max(week_end) & top_5 == 1), aes(label = grp), seed = 1, fill = NA, label.size = NA, hjust = "left", force = 200, nudge_x = -2) +
  labs(x = "Week ending", y = "Weekly COVID-19 test positivity (%)", title = "Weekly % positivity (non-LTC), by PHU and age group",
       caption = paste0("Data from ICES up to week ending ", format(max_date, "%B %d, %Y"), ". Data points derived from non-zero counts <6 have been suppressed.")) +
  scale_x_date(expand = expansion(add = c(0, 2))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubclean() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.line = element_line(size = 0.2),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
p # show plot

# save plot
ggsave("figures/ices-percent-positivity-by-phu-age-group.png", p)

# print top 15 groups by % positivity
sheets %>%
  filter(week_end == max(week_end)) %>%
  slice_max(non_ltc_pos, n = 15) %>%
  select(PHU, age_group, non_ltc_pos)

# print top 15 groups under 18 by % positivity
sheets %>%
  filter(week_end == max(week_end)) %>%
  slice_max(non_ltc_pos, n = 15) %>%
  select(PHU, age_group, non_ltc_pos) %>%
  filter(age_group %in% c("<2", "2-3", "4-8 (JK to Grade 3)", "9-13 (Grade 4-8)", "14-17"))

# clean up
rm(p, sheets, max_date, top_5, read_excel_allsheets)

### COVID-19 CASES AND DEATHS - ONTARIO VERSUS FLORIDA ###

# load data - Canada
# https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv
dat <- read_csv("data/covid19-download.csv")

# load data - US
dat1 <- read_csv("data/us-states.csv")

# process data
dat <- dat %>%
  filter(prname == "Ontario") %>%
  rename(loc = prname) %>%
  mutate(
    caserate = numtoday / 14734014 * 100000,
    deathrate = numdeathstoday / 14734014 * 1000000) %>%
  select(date, loc, caserate, deathrate)
dat1 <- dat1 %>%
  filter(state == "Florida") %>%
  {mutate(.,
          numtoday = c(.[["cases"]][1], diff(cases)),
          numdeathstoday = c(.[["deaths"]][1], diff(deaths)))} %>%
  rename(loc = state) %>%
  mutate(
    caserate = numtoday / 21538187 * 100000,
    deathrate = numdeathstoday / 21538187 * 1000000) %>%
  select(date, loc, caserate, deathrate)
dat <- bind_rows(dat, dat1) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "2021-01-01")
dat[dat$caserate < 0, "caserate"] <- NA

# plot case data
p_cases <- ggplot(data = dat, aes(x = date, y = caserate, group = loc, color = loc)) +
  geom_smooth(formula = y ~ x, se = FALSE, method = "loess", span = 0.1) +
  geom_line(alpha = 0.3) +
  labs(x = "Date", y = "COVID-19 cases per 100,000", color = "Location",
       title = "Daily per-capita COVID-19 cases in 2021",
       caption = "Rates calculated from data provided by PHAC and the NYT. Negative values are excluded.") +
  theme_pubclean() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )
p_cases # show plot
ggsave("figures/florida_cases.png", p_cases)

# plot death data
p_deaths <- ggplot(data = dat, aes(x = date, y = deathrate, group = loc, color = loc)) +
  geom_smooth(formula = y ~ x, se = FALSE, method = "loess", span = 0.1) +
  geom_line(alpha = 0.3) +
  labs(x = "Date", y = "COVID-19 deaths per 1,000,000", color = "Location",
       title = "Daily per-capita COVID-19 mortality in 2021",
       caption = "Rates calculated from data provided by PHAC and the NYT.") +
  theme_pubclean() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )
p_deaths # show plot
ggsave("figures/florida_deaths.png", p_deaths)

# clean up
rm(dat, dat1, p_cases, p_deaths)

### NORTHWEST TERRITORIES - SUB-HR DATA ###

# load and process data
# https://github.com/ccodwg/Covid19Canada/tree/master/official_datasets/nt
nt_cases <- read_csv("data/nt_cases_timeseries_subhr.csv") %>%
  mutate(
    date = as.Date(date),
    group = paste(name, sub_region_2))
nt_active <- read_csv("data/nt_active_timeseries_subhr.csv") %>%
  mutate(
    date = as.Date(date),
    name = "active") %>%
  # active cases stopped getting reported by residents and non-residents, so we combine
  group_by(name, province, sub_region_1, sub_region_2, date) %>%
  summarize(value = sum(value), value_daily = sum(value_daily), .groups = "drop")

# plots
p1 <- ggplot(nt_cases, aes(x = date, y = value, linetype = name, color = sub_region_2, group = group)) +
  geom_line() +
  labs(title = "NWT - Cumulative cases")
p1 # show plot
ggsave("figures/nt_cumulative_cases.png", p1)
p2 <- ggplot(nt_active, aes(x = date, y = value, linetype = name, color = sub_region_2, group = sub_region_2)) +
  geom_line() +
  labs(title = "NWT - Active cases")
p2 # show plot
ggsave("figures/nt_active_cases.png", p2)
p3 <- ggplot(nt_cases, aes(x = date, y = value_daily, linetype = name, color = sub_region_2, group = group)) +
  geom_line() +
  labs(title = "NWT - New cases")
p3 # show plot
ggsave("figures/nt_new_cases.png", p3)
p4 <- ggplot(nt_active, aes(x = date, y = value_daily, linetype = name, color = sub_region_2, group = sub_region_2)) +
  geom_line() +
  labs(title = "NWT - Change in active cases")
p4 # show plot
ggsave("figures/nt_active_cases_change.png", p4)

# clean up
rm(nt_active, nt_cases, p1, p2, p3, p4)
