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


# This script takes reads radar data from your .parquet/S3 and then writes
# a .csv with: basin,date,daily_basin_avg_in,daily_max_bin_in
# presently, i have it to filter on year.  All i have in .parquet/s3 at 
# this point is 2025.  The resulting sheet for a whole year is 
# 10 basins x 365 days = 3650 rows.  You will use this to create basin hyetographs
# and to create the 24-hr basin total tables on your map.  A similar version
# of this script will update daily with .github actions. This script is just
# to populate the historical record.

# load packages
library("aws.s3")
library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")
library("sf")
library("fs")

# keep out of github and docker containers
readRenviron(".Renviron") 

# some AWS checks
required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

# make sure you can connect to your bucket and open SubTreeFileSystem and identify path
# then connect to the .parq files on the s3 storage
bucket_radar <- s3_bucket("stg4-texas-24hr")
s3_path_radar <- bucket_radar$path("")
stg4_24hr_texas_parq <- open_dataset(s3_path_radar)

current_utc_date <- as_date(with_tz(Sys.time(), "UTC"))

current_year<-year(current_utc_date)
current_month<-month(current_utc_date)
current_day<-day(current_utc_date)

# collect your .parq/s3 for a given year, rewrite time column. 
daily_rain_update <- stg4_24hr_texas_parq |>
  filter (year == current_year, month == current_month, day == current_day) |>
  collect() |>
  mutate(time = as.POSIXct(time, tz="UTC"))


aoi <- list.files ("./gis/hrap", pattern = "\\.shp$")

# initilalize empty tibble
  daily_update <- tibble(
    basin           = character(),                 # chr
    date              = as.Date(character()),
    year = integer(),
    daily_basin_avg_in = numeric(),                   # dbl
    daily_max_bin_in   = numeric()                    # dbl
  )
  

# this is your OUTER LOOP
for (a in aoi) {
  
 #a="Bexar.shp"
  # drop the .shp for your table you write  
  basin <- str_replace(basename(a), "\\.[sS][hH][pP]$", "")  # case-insensitive
  
  map <- read_sf(paste0("./gis/hrap/",a)) |>
    st_drop_geometry()
  
  basin_area <- sum(map$bin_area) # basin area in m2
  
  # subset your daily rain by your aoi before you enter the time loop. Big saver.
  subset <- left_join(map,daily_rain_update, by = c("grib_id","hrap_x","hrap_y")) # subset geography before time filter
  
  
  
     daily_rain_filter <- subset |>
      arrange(desc(rain_mm))|>
      mutate(cubic_m_precip = bin_area * rain_mm * .001) # this give you cubic m of precip for each radar bin
    
    basin_rain_cubic_meter <- sum(daily_rain_filter$cubic_m_precip) 
    basin_rain_meter <- basin_rain_cubic_meter/ basin_area
    basin_rain_inch <- basin_rain_meter * 39.37
    basin_max_bin <- daily_rain_filter$rain_mm[1]
    
    daily_update <- rbind(daily_update, 
      
      tibble(
      basin           = basin,                                   # chr vec
      date              = as.Date(daily_rain_filter$time[1]),
      year  = year(date),
      daily_basin_avg_in = basin_rain_inch,                     # dbl vec
      daily_max_bin_in   = basin_max_bin                        # dbl vec
    ))
 
}

  bucket_stats <- s3_bucket("stg4-edwards-daily-stats-24hr")
  s3_path_stats <- bucket_stats$path("")
  stg4_edwards_daily_stats_24hr <- open_dataset(s3_path_stats)
  
# collect your .parq/s3 
  daily_edwards_stats <- stg4_edwards_daily_stats_24hr |>
    collect() 
 
# combine with newest data remove duplicate rows 
 daily_edwards_stats<- rbind(daily_edwards_stats,daily_update) |>
   distinct()
  
 daily_edwards_stats|>
   group_by(year)|>
   write_dataset(
     path    = stats_prefix,   # <-- guarantees it goes to stg4-edwards-daily-stats-24hr
     format  = "parquet"
   )
 
  
  
  

