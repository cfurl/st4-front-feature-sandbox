# This script will write daily:
# 1. daily basin wide averages
# 2. daily max radar bin per basin
# 3. ytd basin wide average

# Workflow:
# Thinking I collect () all data from S3 .parq for 2025
# Pull radar data from S3 parquet for most recent day.
# Do the math for each basin for most recent day.
# Add most recent day's data to tibble for all of 2025
# Remove any duplicate rows from that tibble
# Rewrite entire 2025 .parq to your S3 overwriting previous .parq file.
# 2022 - 2024 files paritioned by year, never touched.

# Keep this clean, columns:
# Basin, dttm, daily_basin_avg, daily_max_bin, ytd_basin_avg 

# first we need to do some manual work and create the file from 2025-01-01 to 2025-10-03

library("aws.s3")
library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")
library("sf")

# for local testing only, keep this out of container, added to .gitignore
readRenviron(".Renviron") 

required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

# make sure you can connect to your bucket and open SubTreeFileSystem and identify path
# then connect to the .parq files on the s3 storage
bucket <- s3_bucket("stg4-texas-24hr")
s3_path <- bucket$path("")
stg4_24hr_texas_parq <- open_dataset(s3_path)

# get current times
current_utc_date_time <- with_tz(Sys.time(), "UTC")
current_central_date_time <- with_tz(Sys.time(), "America/Chicago")
current_utc_time <- format(with_tz(Sys.time(), "UTC"), "%H:%M:%S")
current_utc_date <- as_date(with_tz(Sys.time(), "UTC"))

# establish current time in same format that S3 .parq holds time so you can filter
t1 <- as.POSIXct(paste(Sys.Date() - 0, "12:00:00"), tz = "UTC")  # today 0

daily_rain <- stg4_24hr_texas_parq |>
  filter (time %in% t1) |>
  group_by (grib_id) %>%
  #summarize(
  #  sum_rain = sum(rain_mm, na.rm=TRUE)) %>%
  arrange(desc(rain_mm)) |>
  collect()


# now let's grab a geographical area so we can subset
a = "Bexar.shp"

map <- read_sf(paste0("./gis/hrap/",a)) |>
  st_drop_geometry()

basin_area <- sum(map$bin_area) # basin area in m

map_math <- left_join(map,daily_rain, by = c("grib_id","hrap_x","hrap_y"))|> #this keeps all grib_id's in map
  arrange(desc(rain_mm))|>
  mutate(cubic_m_precip = bin_area * rain_mm * .001) # this give you cubic m of precip for each radar bin

basin_rain_cubic_meter <- sum(map_math$cubic_m_precip) 
basin_rain_meter <- basin_rain_cubic_meter/ basin_area
basin_rain_inch <- basin_rain_meter * 39.37
basin_max_bin <- map_math$rain_mm[1]

##

# attach basin, dttm, basin_rain_in, basin_max_bin to some sheet
# remove any duplicates
# write to .parquet


  

