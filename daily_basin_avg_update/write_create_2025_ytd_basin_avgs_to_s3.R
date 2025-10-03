library("aws.s3")
library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")

ytd_basin_avgs_2025 <- read_csv("./daily_basin_avg_update/basin_avgs_2025_ytd.csv")


# keep out of github and docker containers
readRenviron(".Renviron") 

# some AWS checks
required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

stats_fs <- s3_bucket("stg4-edwards-daily-stats-24hr", region = "us-east-2", anonymous = FALSE)
stats_prefix <- stats_fs$path("year=2025")


write_dataset(
  dataset = ytd_basin_avgs_2025,        # your tibble/data.frame
  path    = stats_prefix,   # <-- guarantees it goes to stg4-edwards-daily-stats-24hr
  format  = "parquet"
)




### make sure you can read it
# make sure you can connect to your bucket and open SubTreeFileSystem and identify path
# then connect to the .parq files on the s3 storage

bucket <- s3_bucket("stg4-edwards-daily-stats-24hr")
s3_path <- bucket$path("")
stg4_edwards_daily_stats_24hr <- open_dataset(s3_path)

# collect your .parq/s3 
daily_edwards_stats <- stg4_edwards_daily_stats_24hr |>
  collect() 



