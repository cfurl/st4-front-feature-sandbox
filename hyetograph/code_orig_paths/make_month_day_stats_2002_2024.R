# I plan to wrap up the output of this script in a docker container.  So, at the beginning of the new year this needs to be
# run to include the last year.  This is all for the ribbons on the hyetograph.
library(tidyverse)
library(lubridate)

# contains max radar bin and average for every day every basin
# this is what was written by 'manually_create_2002_2024_basin_avgs.R'
# takes about 8 hours to comb through the S3 and write this .csv
por_daily_avgs<-read_csv("./hyetograph/output/basin_avgs_2002_2024.csv")

# add month, day, make your cumulative sums by year
month_day_stats <- por_daily_avgs |>
  mutate(month = month(date), day = day(date)) |>
  group_by (basin,year) |>
  mutate(cumulative_basin_avg_in = cumsum(replace(daily_basin_avg_in,is.na(daily_basin_avg_in),0))) |>
  ungroup()

# during the above step i notices some NAs existed in the 'daily_basin_avg_in' column.  These would have been
# introduced or developed during this 8 hour s3 comb and write.  I don't understand why they are there and 
# havent' tracked it down.  I have another parq dataset on my laptop from 2002-2023 that I gave tanya to create
# the hyetographs.  I don't even remember creating it, but i did a quick comparison of totals by year and by basin:

{# This is from the S3 - recently created 24hr stg 4 dataset
# 1) Cumulative basin avg on 12/31 by basin + year
dec31_tbl <- month_day_stats %>%
  filter(month == 12, day == 31) %>%
  select(basin, year, cumulative_basin_avg_in)

# 2) Count NAs in daily_basin_avg_in by basin + year
na_tbl <- month_day_stats %>%
  group_by(basin, year) %>%
  summarise(
    n_na_daily_basin_avg = sum(is.na(daily_basin_avg_in)),
    .groups = "drop"
  )

# 3) Combine them
yearly_summary <- dec31_tbl %>%
  left_join(na_tbl, by = c("basin", "year")) %>%
  arrange(basin, year)


# This is from the parquet that i gave tanya
#### now work off what you gave tanya
library(dplyr)
library(readr)
library(purrr)
library(stringr)

data_dir <- "C:/tanya/stg4_edwards/data/cumulative_precip"

# Grab ALL the daily_averages_2002_2023 files, including USGS
files <- list.files(
  data_dir,
  pattern = "daily_averages_2002_2023.csv$",
  full.names = TRUE
)

all_basins <- map_dfr(files, \(f) {
  fname <- basename(f)
  
  # Remove either " Basin_daily_..." or "_daily_..." depending on the pattern
  basin_name <- fname |>
    str_replace(" Basin_daily_averages_2002_2023.csv", "") |>
    str_replace("_daily_averages_2002_2023.csv", "")
  # For example:
  # "Bexar Basin_daily_averages_2002_2023.csv" -> "Bexar"
  # "usgs_dissolved_daily_averages_2002_2023.csv" -> "usgs_dissolved"
  
  read_csv(f, show_col_types = FALSE) |>
    mutate(basin = basin_name)
})

all_basins |> glimpse()


dec31_tbl_t <- all_basins |>
  filter(month == 12, day == 31) |>
  group_by(basin, year) |>
  summarise(
    cum_avg_in = first(avg_mm_rainfall) / 25.4,  # mm → inches
    .groups = "drop"
  )

na_tbl_t <- all_basins |>
  group_by(basin, year) |>
  summarise(
    n_na_daily_cum_cubic_m = sum(is.na(daily_cum_cubic_m_across_basin)),
    .groups = "drop"
  )


summary_tbl_t <- dec31_tbl_t |>
  left_join(na_tbl_t, by = c("basin", "year")) |>
  arrange(basin, year) |>
  mutate(cum_avg_in = round(cum_avg_in, 2))  # nicer for eyeballing

print(summary_tbl_t, n = nrow(summary_tbl_t))

### compare the two datasets
library(dplyr)

comparison_tbl <- summary_tbl_t %>%
  rename(
    cum_in_arrow_on_c = cum_avg_in,
    n_na_arrow_on_c   = n_na_daily_cum_cubic_m
  ) %>%
  inner_join(
    yearly_summary %>%
      rename(
        cum_in_s3_eaa = cumulative_basin_avg_in,
        n_na_s3_eaa   = n_na_daily_basin_avg
      ),
    by = c("basin", "year")
  ) %>%
  mutate(
    diff_cum_in = cum_in_arrow_on_c - cum_in_s3_eaa   # arrow_on_c minus s3_eaa
  ) %>%
  arrange(basin, year)

print(comparison_tbl, n = 100)

# There are some differences here for sure.  It's more than just rounding a few years have differences
# on the order of inches, but those are very rare.  By in large part it's tenths of an inch. I'm comfortable
# procedding with the s3 for ribbons, but if i ever built a commerical archive for someone, i'd want to test this
# out way more. Focusing on my real-time game!
}



# now that I'm done with comparing things, my goal is to write a tibble containing stats.  This will go in my container and 
# be printed daily as a ribbon.
# get some stats going
basin_stats_2002_2024 <- month_day_stats |> 
  group_by(basin, month, day) |> 
  summarise(
    min_rain = min(cumulative_basin_avg_in, na.rm = TRUE),
    max_rain = max(cumulative_basin_avg_in, na.rm = TRUE),
    perc10 = quantile(cumulative_basin_avg_in, 0.10, na.rm = TRUE),
    perc25 = quantile(cumulative_basin_avg_in, 0.25, na.rm = TRUE),
    perc75 = quantile(cumulative_basin_avg_in, 0.75, na.rm = TRUE),
    perc90 = quantile(cumulative_basin_avg_in, 0.90, na.rm = TRUE),
    median_rain = median(cumulative_basin_avg_in, na.rm = TRUE),
    .groups = "drop"
  ) 

write_csv(basin_stats_2002_2024, "./hyetograph\\output\\basin_stats_month_day_combo_2002_2024.csv", append = FALSE, col_names = TRUE)   # first time: header
