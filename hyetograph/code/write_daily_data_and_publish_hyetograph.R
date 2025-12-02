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
library("ggplot2")
library("patchwork")


# NEED A DATA FOLDER IN DOCKER FILE
# Read in your month_day stats by basin:
md<-read_csv("/home/data/basin_stats_month_day_combo_2002_2024.csv")
md<-read_csv(".\\hyetograph\\data\\basin_stats_month_day_combo_2002_2024.csv")

# grab your most recent data written on your s3 in the dailystat

# keep out of github and docker containers
readRenviron(".Renviron") 

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

#write_csv(current_with_stats, "/home/data/ready_2_hyet.csv", append = FALSE, col_names = TRUE) 

#for ribbon charts
#cumulative_subbasin<-read.csv("/home/data/ready_2_hyet.csv")|>
cumulative_subbasin<-current_with_stats|>
  mutate(date = as.Date(date))


# some time things for figure labeling
date_info <- cumulative_subbasin %>%
  summarise(
    first_date = min(date, na.rm = TRUE),
    last_date_with_data = max(date[!is.na(cumulative_basin_avg_in)])
  )

start_date <- date_info$first_date[[1]]
end_date   <- date_info$last_date_with_data[[1]]


basins<-cumulative_subbasin|>filter(basin!='usgs_dissolved')|>distinct(basin)|>pull()


plot_ribbon_facet<-function(
    data, 
    basins, 
    legend = T,
    title = "Cumulative Precpitation by Subbasin 2023",
    subtitle = "Rainfall measured in milimeters",
    caption = "Source: Edwards Aquifer Authority",
    pal_title = "#00859B",
    pal_subtitle = "black", 
    pal_caption = "grey50",
    pal_avg = '#00859B', pal_median = "#747474"){
  
  #sysfonts::font_add_google("Open Sans", family = "Open Sans")
  #showtext::showtext_auto()
  font = "sans"
  
  if (!is.na(title)) {
    plot_title <- element_text(face='bold', color=pal_title)
  }
  else {
    plot_title<-element_blank()
  }
  
  if (!is.na(subtitle)) {
    plot_subtitle <- element_text(color=pal_subtitle)
  }
  else {
    plot_subtitle<-element_blank()
  }
  
  if (!is.na(caption)) {
    plot_caption <- element_text(color=pal_caption, hjust=0)
  }
  else {
    plot_caption<-element_blank()
  }
  
  if (legend) {
    plot_legend_position <- "top"
  }
  else {
    plot_legend_position = "none"
  }
  
  
  df <- data |> filter(basin %in% basins)
  
  df_pivoted <- df |>
    # keep perc25/perc75 so they’re available for the ribbon
    select(
      date,
      basin,
      perc25,
      perc75,
      median_rain,
      cumulative_basin_avg_in
    ) |>
    pivot_longer(
      cols      = c(median_rain, cumulative_basin_avg_in),
      names_to  = "name",
      values_to = "value"
    ) |>
    mutate(
      name = case_when(
        name == "median_rain"             ~ "Median",
        name == "cumulative_basin_avg_in" ~ "Actual",
        TRUE                              ~ name
      )
    )
  #factor make sure Actual line has the higher z-index (plots over Median)
  df_pivoted$name<-factor(df_pivoted$name, levels=c("Median", "Actual"))
  
  plot<-ggplot(data=df_pivoted) +
    geom_ribbon(mapping=aes(x =date, ymin=perc25,ymax=perc75, fill=name), alpha=0.2) +  
    geom_line(mapping=aes(x = date, y = value, color=name), linewidth=0.6)+
    facet_wrap(~basin)+
    scale_x_date(
      date_labels = "%b",   
      #date_breaks = "3 months",
      expand = c(0,0)
    ) +
    scale_y_continuous(
      expand = c(0,0)
    )+
    scale_color_manual(
      values = c("Actual" = pal_avg, "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    scale_fill_manual(
      values = c("Actual" = "transparent", "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    labs(
      color = '',
      fill = '',
      x= '')+
    theme_minimal()+
    theme(
      legend.position = plot_legend_position,
      legend.location = "plot",
      legend.title = element_text(size=9),
      legend.margin = margin(l=0),
      legend.box.margin = margin(rep(0,4)),
      plot.title = plot_title, 
      plot.subtitle = plot_subtitle,
      plot.caption.position = "plot",
      plot.caption = plot_caption,
      plot.margin = margin(t=15, b=15, l=15, r=15),
      plot.title.position = "plot",
      legend.justification = "left",
      panel.spacing = unit(1.5, "lines"),
      axis.title.y = element_blank(),
      axis.line.x = element_line(color='black'),
      text = element_text(family=font),
      strip.text = element_text(hjust=0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  if(!is.na(title)){
    plot<-plot + labs(title =title)
  }
  
  if(!is.na(subtitle)){
    plot<-plot + labs(subtitle = subtitle)
  }
  
  if(!is.na(caption)){
    plot<-plot + labs(caption = caption)
  }
  
  return(plot)
}

plot_ribbon<-function(
    data, 
    select_basin, 
    title = "Cumulative Precpitation by Subbasin 2023",
    subtitle = "Rainfall measured in milimeters",
    caption = "Source: Edwards Aquifer Authority",
    legend = T,
    pal_title = "#00859B",
    pal_subtitle = "black", 
    pal_caption = "grey50",
    pal_avg = '#00859B', pal_median = "#747474"){
  
  #sysfonts::font_add_google("Open Sans", family = "Open Sans")
  #showtext::showtext_auto()
  font = "sans"
  
  if (!is.na(title)) {
    plot_title <- element_text(face='bold', color=pal_title)
  }
  else {
    plot_title<-element_blank()
  }
  
  if (!is.na(subtitle)) {
    plot_subtitle <- element_text(color=pal_subtitle)
  }
  else {
    plot_subtitle<-element_blank()
  }
  
  if (!is.na(caption)) {
    plot_caption <- element_text(color=pal_caption, hjust=0)
  }
  else {
    plot_caption<-element_blank()
  }
  
  if (legend) {
    plot_legend_position <- "top"
  }
  else {
    plot_legend_position = "none"
  }
  
  #get subset of data
  df<-data|>filter(basin == select_basin)
  
  df_pivoted <- df |>
    # keep perc25/perc75 so they’re available for the ribbon
    select(
      date,
      basin,
      perc25,
      perc75,
      median_rain,
      cumulative_basin_avg_in
    ) |>
    pivot_longer(
      cols      = c(median_rain, cumulative_basin_avg_in),
      names_to  = "name",
      values_to = "value"
    ) |>
    mutate(
      name = case_when(
        name == "median_rain"             ~ "Median",
        name == "cumulative_basin_avg_in" ~ "Actual",
        TRUE                              ~ name
      )
    )
  #factor make sure Actual line has the higher z-index (plots over Median)
  df_pivoted$name<-factor(df_pivoted$name, levels=c("Median", "Actual"))
  
  
  plot<-ggplot(data=df_pivoted) +
    geom_ribbon(mapping=aes(x =date, ymin=perc25,ymax=perc75, fill=name), alpha=0.2, show.legend = T) +  
    geom_line(mapping=aes(x = date, y = value, color=name), linewidth=0.6)+
    scale_x_date(
      date_labels = "%b",   
      # date_breaks = "3 months",
      expand = c(0,0)
    ) +
    scale_y_continuous(
      expand = c(0,0)
    )+
    scale_color_manual(
      values = c("Actual" = pal_avg, "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    scale_fill_manual(
      values = c("Actual" = "transparent", "Median" = pal_median),
      breaks = c("Actual", "Median")
    ) +
    labs(
      color = '',
      fill = '',
      x= '')+
    theme_minimal()+
    theme(
      legend.position = plot_legend_position,
      legend.location = "plot",
      legend.title = element_text(size=9),
      legend.margin = margin(l=0),
      legend.box.margin = margin(rep(0,4)),
      plot.caption.position = "plot",
      plot.margin = margin(t=15, b=15, l=15, r=15),
      plot.caption = plot_caption,
      plot.title.position = "plot",
      legend.justification = "left",
      axis.title.y = element_blank(),
      axis.line.x = element_line(color='black'),
      text = element_text(family=font),
      plot.title = plot_title,
      plot.subtitle = plot_subtitle,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  if(!is.na(title)){
    plot<-plot + labs(title =title)
  }
  
  if(!is.na(subtitle)){
    plot<-plot + labs(subtitle = subtitle)
  }
  
  if(!is.na(caption)){
    plot<-plot + labs(caption = caption)
  }
  
  return(plot)
}


#### function to run patchwork and remove subtitles
precip_facet<-plot_ribbon_facet(
  data = cumulative_subbasin, 
  basins = basins, 
  pal_avg = "#E69F00",
  pal_title = "black",
  title = NA,
  subtitle = "Subbasin Level",
  caption = NA,
  legend = T
)

precip_total<-plot_ribbon(
  data = cumulative_subbasin, 
  select_basin = "usgs_dissolved", 
  pal_avg = "#E69F00",
  pal_title = "black",
  title = NA,
  subtitle = "Edwards Aquifer Recharge Zone",
  caption = NA)


fp <- (guide_area() +
         (
           precip_total +
             (precip_facet & theme(axis.text  = element_text(size = 7),
                                   strip.text = element_text(size = 7))) +
             plot_layout(widths = c(1.25, 1.5))
         )
) +
  plot_layout(
    guides  = "collect",
    heights = c(0.04, 0.96)   # was 0.08,0.92 → thinner legend strip
  ) +
  plot_annotation(
    title   = paste0("Basin-Averaged NEXRAD Hyetographs (in) from ",
                     format(start_date, "%Y-%m-%d"), " to ",
                     format(end_date, "%Y-%m-%d")),
    #caption = "Ribbon displays 25ᵗʰ and 75ᵗʰ percentiles"
    caption = expression("Ribbon displays 25"^"th" * " and 75"^"th" * " percentiles")
  ) &
  theme(
    text         = element_text(family = "sans", size = 12),
    plot.title   = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, color = "grey60"),
    
    legend.position      = "top",
    legend.direction     = "horizontal",
    legend.justification = c(0, 0.5),
    
    # tighter box around legend
    legend.margin        = margin(0, 2, 0, 2),
    legend.box.margin    = margin(0, 0, 0, 0),
    legend.spacing.x     = unit(4, "pt"),
    legend.spacing.y     = unit(0, "pt"),
    legend.title         = element_blank(),
    
    plot.margin          = margin(rep(10, 4))
  )


#combo_hyet <- "hyetograph/output/combo_hyet.png"
combo_hyet <- ".//hyetograph//output//combo_hyet.png"
ggsave(combo_hyet, fp, device = ragg::agg_png, width = 3840, height = 2160, units = "px")



# when you get down here look at the map_plus_gt to see how you wrote to multiple buckets.
# Too much for me to do on a Monday before Thanksgiving

# local file you just wrote with ggsave()
local_png_hyet <- combo_hyet

# use the same “end of period” date you’re using elsewhere
hyet_date <- as.Date(end_date)   # or as.Date(time_filter), or Sys.Date()

# S3 object key: in 'hyet/' folder with date in name
s3_key_hyet <- file.path(
  "hyet",
  sprintf("combo_hyet_%s.png", hyet_date)   # combo_hyet_2025-12-01.png
)

ok_hyet <- put_object(
  file    = local_png_hyet,
  object  = s3_key_hyet,
  bucket  = "stg4-edwards-daily-maps",
  headers = list(`Content-Type` = "image/png"),
  multipart = TRUE
)

if (!isTRUE(ok_hyet)) stop("Upload failed: ", local_png_hyet)







###### Upload maps to 'latest' area publick bucket

latest_bucket <- "stg4-edwards-latest"
region <- ("us-east-2")



ok_latest_hyet <- put_object(
  file     = local_png_hyet,
  object   = "latest_hyet.png",
  bucket   = latest_bucket,
  region   = region,
  multipart = TRUE,
  headers  = list(
    `Content-Type`  = "image/png",
    `Cache-Control` = "public, max-age=300, must-revalidate"
  )
)

if (!isTRUE(ok_latest_hyet)) stop("Upload failed: ", local_png_hyet)



