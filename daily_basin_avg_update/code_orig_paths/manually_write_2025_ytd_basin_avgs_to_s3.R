library("aws.s3")
library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")

ytd_basin_avgs_2025 <- read_csv("./daily_basin_avg_update/output/basin_avgs_2025_ytd.csv")


# keep out of github and docker containers
readRenviron(".Renviron") 

# some AWS checks
required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

bucket_stats <- s3_bucket("stg4-edwards-daily-stats-24hr", region = "us-east-2", anonymous = FALSE)
stats_prefix <- bucket_stats$path("")

ytd_basin_avgs_2025|>
  group_by(year)|>
  write_dataset(
    #dataset = ytd_basin_avgs_2025,        # your tibble/data.frame
    path    = stats_prefix,   # <-- guarantees it goes to stg4-edwards-daily-stats-24hr
    format  = "parquet"
)




### make sure you can read it
# make sure you can connect to your bucket and open SubTreeFileSystem and identify path
# then connect to the .parq files on the s3 storage

# stats bucket read
bucket_stats <- s3_bucket("stg4-edwards-daily-stats-24hr")
s3_path_stats <- bucket_stats$path("")
stg4_edwards_daily_stats_24hr <- open_dataset(s3_path_stats)

# collect your .parq/s3 
daily_edwards_stats <- stg4_edwards_daily_stats_24hr |>
  collect() 

# radar bucket read
bucket_radar <- s3_bucket("stg4-texas-24hr")
s3_path_radar <- bucket_radar$path("")
stg4_24hr_texas_parq <- open_dataset(s3_path_radar)

d <- stg4_24hr_texas_parq |>
  filter (month == 10, day == 8) |>
  collect()


