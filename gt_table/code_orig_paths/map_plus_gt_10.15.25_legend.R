library("arrow")
library("dplyr")
library("lubridate")
library("tidyr")
library("readr")
library("stringr")
library("aws.s3")
library("sf")
library("ggspatial")
library("ggplot2")
library("prettymapr")
library("cowplot")
library("magick")
library("webshot2")

######################## Some S3 things #####################
# remove this from container setup, this gives your local dev the AWS access
readRenviron(".Renviron") # when it's in gitignore

required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

# radar bucket read
bucket_radar <- s3_bucket("stg4-texas-24hr")
s3_path_radar <- bucket_radar$path("")
stg4_24hr_texas_parq <- open_dataset(s3_path_radar)

############################ time stamps #############
current_utc_date_time <- with_tz(Sys.time(), "UTC")
current_central_date_time <- with_tz(Sys.time(), "America/Chicago")
current_utc_time <- format(with_tz(Sys.time(), "UTC"), "%H:%M:%S")
current_utc_date <- as_date(with_tz(Sys.time(), "UTC"))

# Create exact timestamps (UTC) for noon on yesterday and today
t1 <- as.POSIXct(paste(Sys.Date() - 0, "12:00:00"), tz = "UTC")  # today 0
t2 <- as.POSIXct(paste(Sys.Date() - 1, "12:00:00"), tz = "UTC") # yesterday 1

# This sequence is how you get the most recent data.  It today's data hasn't dropped yet
# it will go query yesterday's data.
time_check <- stg4_24hr_texas_parq |>
  select(time)|>
  filter (time %in% c(t1)) |>
  collect()

if (nrow(time_check) == 0) {
  time_filter<-t2
} else {
  time_filter<-t1
}

# This is where you query the parq files by time (not location yet)
# carrying these commands around for whole state, could clip first
# room for optimization here

rain_24hr <- stg4_24hr_texas_parq |>
  filter (time %in% c(time_filter)) |>
  group_by (grib_id) %>%
  summarize(
    sum_rain = sum(rain_mm, na.rm=TRUE)) %>%
  arrange(desc(sum_rain)) |>
  collect()

current_year <- year(time_filter)

rain_cum <- stg4_24hr_texas_parq |>
  filter (year == current_year) |>
  group_by (grib_id) %>%
  summarize(
    sum_rain = sum(rain_mm, na.rm=TRUE)) %>%
  arrange(desc(sum_rain)) |>
  collect()

# Make local time labels for main title after you've queried which days (yesterday or today) are available.
end_time_local <- with_tz(time_filter, "America/Chicago")
begin_time_local <- end_time_local - days(1)

# call the gis layers you want mapped
gs_basins<-read_sf("./gt_table/gis/usgs_basins.shp")
map <- sf::read_sf("./gt_table/gis/usgs_dissolved.shp") # this is your hrap polygon
#streams <- read_sf("./gt_table/gis/streams_recharge.shp")
lakes <- read_sf("./gt_table/gis/reservoirs.shp")
streams_dense<-read_sf("./gt_table/gis/recharge_zone_streams_med_density.shp")

# this is where you subset the statewide set of bins by your shapefile area of interest. DAILY SECTION
map_rain_24hr <- map|>
  left_join(rain_24hr, by = "grib_id")|>
  mutate(cubic_m_precip = bin_area * sum_rain * 0.001)|>
  mutate(sum_rain_in = sum_rain/25.4)

# --- Static legend settings (always show full range) ---
rain_breaks_24  <- c(0, 0.1, 0.25, 0.5, 1, 2, 3, 4, 6, 8, 10, 12)
rain_limits_24  <- c(0, 12)

cols_24 <- c("#A9E2F8","#2A4FD6","#22FE05","#248418","#F6FB07","#FFC348","#E01E17","#8C302C", "#CC17DA","#AE60B3","#FDF5FF")

lab_fun_24 <- function(x) {
  labs <- as.character(x)
  labs[x == max(x, na.rm = TRUE)] <- "12+"
  labs
}

# this is where you subset the statewide set of bins by your shapefile area of interest. YTD SECTION

map_rain_cum <- map|>
  left_join(rain_cum, by = "grib_id")|>
  mutate(cubic_m_precip = bin_area * sum_rain * 0.001)|>
  mutate(sum_rain_in = sum_rain/25.4)

cols_cum<- c("#FFFFD9", "#F1F9B9", "#D6EFB3", "#ACDEB7", "#75C8BD", "#41B6C4", "#2798C1", "#2372B2", "#264DA0", "#1F2F88", "#081D58")

# Dynamic legend scale function
centered_steps_min0 <- function(x, step = 2, n_side = 5) {
  
  x <- x[is.finite(x)]
  stopifnot(length(x) > 0)
  
  med <- stats::median(x, na.rm = TRUE)
  
  # Bin containing the median, aligned to even numbers
  center_low  <- step * floor(med / step)   # e.g., 25.6 -> 24
  center_high <- center_low + step          # 24–26
  
  # Ideal symmetric window (may go < 0 early in the year)
  lower <- center_low  - step * n_side
  upper <- center_high + step * n_side
  
  # Enforce minimum bin 0–2; if we clipped below 0, push those bins to the top
  if (lower < 0) {
    deficit_bins <- as.integer(ceiling((0 - lower) / step))  # how many 2" bins went < 0
    lower <- 0
    upper <- upper + deficit_bins * step                     # keep 11 bins total
  }
  
  rain_breaks_cum <- seq(lower, upper, by = step)                     # length = 12
  rain_limits_cum <- c(lower, upper)
  
  lab_fun_cum <- function(v) {
    labs <- as.character(v)
    labs[v == min(v, na.rm = TRUE)] <- if (lower == 0) "0" else paste0("< ", lower)
    labs[v == max(v, na.rm = TRUE)] <- paste0(upper,"+")
    labs
  }
  
  list(rain_breaks_cum = rain_breaks_cum, rain_limits_cum = rain_limits_cum, lab_fun_cum=lab_fun_cum)
}

cfg<-centered_steps_min0(map_rain_cum$sum_rain_in)

# Mapping function edited from Tanya's work

plot_bin_map<-function(
    title = 'Edwards Aquifer Recharge Zone',
    subtitle= NA,
    note_title = NA,
    font = "Open Sans",
    map_rain = NA,
    map_streams = NA, 
    map_lakes = NA,
    stream_basin_alpha = NA,
    pal_water='black',
    pal_title='white',
    pal_subtitle='white',
    pal_outline='black',
    pal_bin_outline='black',
    pal_legend_text='white',
    bin_alpha = 0.7,
    map_type='cartodark',
    rain_breaks = rain_breaks_24,
    bin_cols = NA,
    lab_fun = NA,
    rain_limits = rain_limits_24
){
  
  bbox <- st_bbox(c(
    xmin = -100.85,
    ymin = 29.05, #29.0
    xmax = -97.75, 
    ymax = 30.52 #.47
  ), crs = 4326)
  
  coord_sys<-3857
  
  # Convert bbox to an sf object for ggplot compatibility
  bbox_sf <- st_as_sfc(bbox)
  bbox_transformed <- st_transform(bbox_sf, crs = coord_sys)
  
  outline <- map |> summarise(geometry = st_union(geometry)) |> st_cast("MULTILINESTRING")  
  
  title_pos <- st_sfc(st_point(c(-100.88, 30.43)), crs = 4326) |> 
    st_transform(crs = 3857) |> 
    st_coordinates() |> as.data.frame()
  
  subtitle_pos <- st_sfc(st_point(c(-100.88, 30.43 - 0.085)), crs = 4326) |> 
    st_transform(crs = 3857) |> 
    st_coordinates() |> as.data.frame()
  
  note_title_pos <- st_sfc(st_point(c(-100.88, 30.43 - 1.41)), crs = 4326) |> 
    st_transform(crs = 3857) |> 
    st_coordinates() |> as.data.frame()

  # --- Static legend settings (always show full range) ---
  rain_breaks  <- rain_breaks
  #rain_labels  <- rain_labels
  rain_limits  <- rain_limits
  
  # --- Set 0 rainfall to NA for transparency ---
  map_rain <- map_rain |>
    mutate(fill_val = ifelse(sum_rain_in == 0, NA_real_, sum_rain_in))
  
  plot<-ggplot()+
    
   # annotation_map_tile(
  #    type = map_type,  # Use the "Carto Light" basemap
  #    zoom = 9  # Adjust zoom level as needed
   # )+
    
    annotation_map_tile(
      type = map_type,        # same provider you cached
      zoom = 9,                   # same zoom you pre-fetched
      cachedir = "./gt_table/rosm.cache/cartolight",  # <-- path you COPY into the image
      forcedownload = FALSE,      # use cache; don’t fetch
      progress = "none",
      alpha = 1
    ) +

    annotate(geom="text",x= title_pos$X,y=title_pos$Y,label=title,size=7.75,hjust=0, color = pal_title, family=font, fontface='bold')+
    annotate(geom="text",x= subtitle_pos$X,y=subtitle_pos$Y,label=subtitle,size=3.5,hjust=0, color = pal_subtitle, family=font)+
    annotate(geom="text",x=  note_title_pos$X,y= note_title_pos$Y,label=note_title,size=2,hjust=0, color = pal_subtitle, family=font)+
    geom_sf(data = map_rain, mapping = aes(fill = fill_val), color = pal_bin_outline, alpha = bin_alpha, na.rm = FALSE) +
    geom_sf(data = outline|>st_transform(crs = coord_sys), color = pal_outline, linewidth = 0.4) +  
    geom_sf(data=map_lakes|>st_transform(crs = coord_sys), fill= pal_water, color= pal_water, linewidth = 0.2)+
    #geom_sf(data=map_streams|>st_transform(crs = coord_sys), color= pal_water, linewidth = 0.1,inherit.aes=FALSE)+
    #geom_sf(data=map_subbasin|>st_transform(crs = coord_sys), color= "black", linewidth = 0.1, fill = NA, alpha=0,inherit.aes=FALSE)+
    geom_sf(
      data = map_streams |> st_transform(crs = coord_sys),
      mapping = aes(),            # don't inherit aes(fill = ...)
      color   = pal_water,
      linewidth = 0.1,
      fill    = NA,               # keep polygons hollow
      alpha   = stream_basin_alpha,                # belt & suspenders
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    
    # IMPORTANT: values anchors the palette to your breaks
  scale_fill_stepsn(
    colours = bin_cols,
    breaks  = rain_breaks,
    limits  = rain_limits,
    values  = scales::rescale(rain_breaks, from = rain_limits),
    labels  = lab_fun,           # <-- function, not character vector
    oob     = scales::squish,
    name    = "Rainfall (in)",
    na.value = NA
  ) +
    guides(fill = guide_colorsteps(title.position = "top", show.limits = TRUE, title.vjust=0.1))+
       
    coord_sf(
      xlim = c(st_bbox(bbox_transformed)["xmin"], st_bbox(bbox_transformed)["xmax"]),
      ylim = c(st_bbox(bbox_transformed)["ymin"], st_bbox(bbox_transformed)["ymax"])
    ) +
    theme_void()+
    theme(
      text = element_text(family=font),
     # legend.position = "inside",
      legend.position = c(0.70,0.1),  
      legend.direction = "horizontal", 
      legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
      legend.title = element_text(size = 10, face='bold', color=pal_legend_text), 
      legend.text = element_text(size = 9, color=pal_legend_text),  
      legend.key.width = unit(2.5, "cm"), 
      legend.key.height = unit(0.5, "cm")  
    )
  
  return(plot)
}

# Daily plot
p24 <- plot_bin_map(
  title = 'Edwards Aquifer Recharge Zone',
  subtitle = paste("Precipitation from", format(begin_time_local, "%Y-%m-%d %H:%M %Z"), "to", format(end_time_local, "%Y-%m-%d %H:%M %Z")),
  note_title = paste("Produced at", format(current_utc_date_time, "%Y-%m-%d %H:%M %Z"), "-", format(current_central_date_time, "%Y-%m-%d %H:%M %Z")),
  font = "",
  map_rain = map_rain_24hr, map_streams = streams_dense, map_lakes = lakes, stream_basin_alpha = 1,
  pal_water = '#2C6690', pal_title='black', bin_alpha = 0.8,
  pal_subtitle='black', pal_outline="#697984", pal_bin_outline=NA,
  pal_legend_text='black', map_type='cartolight',
  rain_breaks = rain_breaks_24,
  bin_cols = cols_24,
  lab_fun = lab_fun_24,
  rain_limits = rain_limits_24
  
)

##### this one maps well
base_map_24 <- "gt_table/output/edwards_map_base_24.png"
ggsave(base_map_24, p24, device = ragg::agg_png, width = 3840, height = 2160, units = "px")

map_img_24   <- image_read(base_map_24)
table_img_24 <- image_read("./gt_table/output/edwards_basin_table_24.png")

# scale table to ~40% of map width; place at NE with a small inset
map_w_24   <- image_info(map_img_24)$width
table_img_24 <- image_resize(table_img_24, paste0(as.integer(map_w_24 * 0.45))) #.4
final_24 <- image_composite(map_img_24, table_img_24, gravity = "northeast", offset = "+150+185") #offset = "+120+140"  # 120 px left, 140 px down from the top-right

image_write(final_24, paste0("gt_table/output/contrib_zone_w_table_24h_",as.Date(time_filter),".png"))

# local file that image_write() just created
local_png_24h <- sprintf("gt_table/output/contrib_zone_w_table_24h_%s.png", as.Date(time_filter))
# choose an S3 key (prefix/folder optional)
s3_key_24h <- file.path("24h", basename(local_png_24h))  # e.g., maps/contrib_zone_w_table_ytd_2025-10-08.png

ok_24h <- put_object(
  file   = local_png_24h,
  object = s3_key_24h,
  bucket = "stg4-edwards-daily-maps",
  headers = list(`Content-Type` = "image/png"),
  multipart = TRUE
)

if (!isTRUE(ok_24h)) stop("Upload failed: ", local_png_24h)

#YTD plot
pcum <- plot_bin_map(
  title = 'Edwards Aquifer Recharge Zone',
  subtitle = paste0("Precipitation from ", current_year, "-01-01 to ", format(end_time_local, "%Y-%m-%d %H:%M %Z")),
  note_title = paste("Produced at", format(current_utc_date_time, "%Y-%m-%d %H:%M %Z"), "-", format(current_central_date_time, "%Y-%m-%d %H:%M %Z")),
  font = "",
  map_rain = map_rain_cum, map_streams = gs_basins, map_lakes = lakes, stream_basin_alpha = 0,
  pal_water = "black", pal_title='black', bin_alpha = 0.8,
  pal_subtitle='black', pal_outline="#697984", pal_bin_outline=NA,
  pal_legend_text='black', map_type='cartolight',
  bin_cols = cols_cum,
  rain_breaks = cfg$rain_breaks_cum,
  lab_fun = cfg$lab_fun_cum,
  rain_limits = cfg$rain_limits_cum
)

##### this one maps well
base_map_cum <- "gt_table/output/edwards_map_base_cum.png"
ggsave(base_map_cum, pcum, device = ragg::agg_png, width = 3840, height = 2160, units = "px")

map_img_cum   <- image_read(base_map_cum)
table_img_cum <- image_read("./gt_table/output/edwards_basin_table_cum.png")

# scale table to ~40% of map width; place at NE with a small inset
map_w_cum   <- image_info(map_img_cum)$width
table_img_cum <- image_resize(table_img_cum, paste0(as.integer(map_w_cum * 0.45))) #.4
final_cum <- image_composite(map_img_cum, table_img_cum, gravity = "northeast", offset = "+150+185") #offset = "+120+140"  # 120 px left, 140 px down from the top-right

image_write(final_cum, paste0("gt_table/output/contrib_zone_w_table_ytd_",as.Date(time_filter),".png"))

# local file that image_write() just created
local_png_ytd <- sprintf("gt_table/output/contrib_zone_w_table_ytd_%s.png", as.Date(time_filter))
# choose an S3 key (prefix/folder optional)
s3_key_ytd <- file.path("ytd", basename(local_png_ytd))  # e.g., maps/contrib_zone_w_table_ytd_2025-10-08.png

ok_ytd <- put_object(
  file   = local_png_ytd,
  object = s3_key_ytd,
  bucket = "stg4-edwards-daily-maps",
  headers = list(`Content-Type` = "image/png"),
  multipart = TRUE
)

if (!isTRUE(ok_ytd)) stop("Upload failed: ", local_png_ytd)


###### Upload maps to 'latest' area publick bucket

latest_bucket <- "stg4-edwards-latest"
region <- ("us-east-2")

# Overwrite fixed keys with 5-minute browser cache
stopifnot(
  isTRUE(put_object(
    file     = local_png_24h,
    object   = "latest_24h.png",
    bucket   = latest_bucket,
    region   = region,
    multipart = TRUE,
    headers  = list(
      `Content-Type`  = "image/png",
      `Cache-Control` = "public, max-age=300, must-revalidate"
    )
  )),
  isTRUE(put_object(
    file     = local_png_ytd,
    object   = "latest_ytd.png",
    bucket   = latest_bucket,
    region   = region,
    multipart = TRUE,
    headers  = list(
      `Content-Type`  = "image/png",
      `Cache-Control` = "public, max-age=300, must-revalidate"
    )
  ))
)

