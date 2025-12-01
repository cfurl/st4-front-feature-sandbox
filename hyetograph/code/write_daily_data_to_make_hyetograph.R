# First, I need to do is calculate daily averages for each day of the record 2002-2024 for each sub-basin and entire rchg zn.
# Second, I need to make a sheet with percentiles at different month-day combinations that will be used for ribbons.
library(dplyr)
library(readr)
library("aws.s3")
library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")
# Read in your month_day stats by basin:
md<-read_csv("/home/data/basin_stats_month_day_combo_2002_2024.csv")
#md<-read_csv(".\\hyetograph\\data\\basin_stats_month_day_combo_2002_2024.csv")

# grab your most recent data written on your s3 in the dailystat

# keep out of github and docker containers
#readRenviron(".Renviron") 

# some AWS checks
required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

# stats bucket read
bucket_stats <- s3_bucket("stg4-edwards-daily-stats-24hr")
s3_path_stats <- bucket_stats$path("")
stg4_edwards_daily_stats_24hr <- open_dataset(s3_path_stats)

current_utc_date <- as_date(with_tz(Sys.time(), "UTC"))

current_year<-year(current_utc_date)
current_month<-month(current_utc_date)
current_day<-day(current_utc_date)

# collect your .parq/s3 whole enchilada
ytd_edwards_stats <- stg4_edwards_daily_stats_24hr |>
  collect()  |>
  group_by(basin)|>
  filter(year==year(current_utc_date)) |>
  mutate(cumulative_basin_avg_in = cumsum(replace(daily_basin_avg_in,is.na(daily_basin_avg_in),0))) |>
  mutate(month=month(date), day=day(date))|>
  ungroup()|>
  select(basin, date, month, day, cumulative_basin_avg_in)


# join your ytd_edwards_stats that update daily with the hard-wired 2002-2024 statistics
this_year <- year(current_utc_date)

full_calendar <- tibble(
  date = seq(
    as.Date(sprintf("%d-01-01", this_year)),
    as.Date(sprintf("%d-12-31", this_year)),
    by = "day")) %>%
  mutate(month = month(date), day   = day(date))

## 3. Get the set of basins you care about
basins <- md %>% distinct(basin) %>% pull()

## 4. Cross basins with the full calendar
##    (one row per basin per day for the whole year)
template <- crossing(
  basin = basins,
  full_calendar
) %>%
  # make sure month/day types match md (md has <dbl> for both)
  mutate(
    month = as.double(month),
    day   = as.double(day)
  )

## 5. Join in the current-year cumulative rainfall (YTD)
##    This introduces NAs for dates that don't exist yet in ytd_edwards_stats.
current_with_ytd <- template %>%
  left_join(
    ytd_edwards_stats %>%
      select(basin, date, cumulative_basin_avg_in),
    by = c("basin", "date")
  )

## 6. Join in the historical stats by (basin, month, day)
##    You can choose whichever columns you want from md for your ribbon.
current_with_stats <- current_with_ytd %>%
  left_join(
    md %>%
      select(
        basin, month, day, perc10, perc90,
        perc25, perc75,
        min_rain, max_rain, median_rain
      ),
    by = c("basin", "month", "day")
  ) %>%
  arrange(basin, date) |>
  select(month, day, min_rain, max_rain, perc10, perc25, perc75, perc90, median_rain, date, basin, cumulative_basin_avg_in) |>
  mutate(name="Median")

write_csv(current_with_stats, "/home/data/ready_2_hyet.csv", append = FALSE, col_names = TRUE) 

