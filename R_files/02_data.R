
## Load latest RDS file

all_cases_collapsed_file <- dir_info("data", type = "file", regexp = "(?i)all_cases_collapsed.*\\.rds$") %>%
  subset(subset = modification_time == max(modification_time), select = path, drop = TRUE)

all_cases_collapsed <- readRDS(all_cases_collapsed_file) %>%
  filter(year > 2007, year != 2025) %>%
  as.data.frame()


##Aggregate cases by Month

cases_per_month <- all_cases_collapsed %>%
  group_by(year_month) %>%
  summarise(num_cases = n(), .groups = "drop") %>%
  mutate(
    year_month = as.Date(paste0(year_month, "-01")),
    year = year(year_month),
    month = month(year_month)
  )

## Convert to tsibble
cases_tsibble <- cases_per_month %>%
  mutate(year_month = yearmonth(year_month)) %>%
  as_tsibble(index = year_month)


# Aggregate cases by week
cases_per_week <- all_cases_collapsed %>%
  filter(year != 2025) %>%
  mutate(
    week_start = floor_date(merged_date_min, "week"),
    week_label = paste0(year(week_start), "-W", sprintf("%02d", isoweek(week_start)))
  ) %>%
  group_by(week_start, week_label) %>%
  summarise(num_cases = n(), .groups = "drop")