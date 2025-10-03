library("aws.s3")
library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")
library("sf")

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

daily_rain <- stg4_24hr_texas_parq |>
  filter (year == 2025) |>
  group_by (grib_id) %>%
  arrange(desc(rain_mm)) |>
  collect()


#aoi <- list.files ("./gis/hrap", pattern = "\\.shp$")
# this is your outer loop
#for (a in aoi) {

# now let's grab a geographical area so we can subset
a = "usgs_dissolved.shp"

map <- read_sf(paste0("./gis/hrap/",a)) |>
  st_drop_geometry()

basin_area <- sum(map$bin_area) # basin area in m

# initilalize tibble
basin_avgs_2025_ytd <- tibble(
  basin           = character(),                 # chr
  dttm            = ymd_hms(character(), tz = "UTC"),  # dttm (POSIXct, UTC)
  daily_basin_avg_in = numeric(),                   # dbl
  daily_max_bin_in   = numeric()                    # dbl
)

# inner loop
for (d in unique(daily_rain$time)){


map_math <- left_join(map,daily_rain, by = c("grib_id","hrap_x","hrap_y"))|> #this keeps all grib_id's in map
  arrange(desc(rain_mm))|>
  mutate(cubic_m_precip = bin_area * rain_mm * .001) # this give you cubic m of precip for each radar bin

basin_rain_cubic_meter <- sum(map_math$cubic_m_precip) 
basin_rain_meter <- basin_rain_cubic_meter/ basin_area
basin_rain_inch <- basin_rain_meter * 39.37
basin_max_bin <- map_math$rain_mm[1]

daily_update <- bind_cols (a, map_math$time[1], basin_rain_inch, basin_max_bin)

basin_avgs_2025_ytd <- bind_rows (basin_avgs_2025_ytd, daily_update)

}

#}

write_csv(basin_avgs_2025_ytd,"./daily_basin_avg_update/basin_avgs_2025_ytd.csv")

