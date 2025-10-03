library("aws.s3")
library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")

ytd_basin_avgs_2025 <- read_csv("./daily_basin_avg_update/basin_avgs_2025_ytd.csv")

stats_fs <- s3_bucket("stg4-edwards-daily-stats-24hr", anonymous = FALSE)
stats_prefix <- stats_fs$path("year=2025")

write_dataset(
  dataset = ytd_basin_avgs_2025,        # your tibble/data.frame
  path    = stats_prefix,   # <-- guarantees it goes to stg4-edwards-daily-stats-24hr
  format  = "parquet"
)

