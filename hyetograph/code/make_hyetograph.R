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
library("ggplot")
# Read in your month_day stats by basin:
md<-read_csv("C:\\stg4\\front\\st4-front-feature-sandbox\\hyetograph\\output\\basin_stats_month_day_combo_2002_2024.csv")

# grab your most recent data written on your s3 in the dailystat

# keep out of github and docker containers
readRenviron(".Renviron") 

# some AWS checks
required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

#for ribbon charts
#cumulative_subbasin<-read.csv("./hyetograph/output/cumulative_subbasin.csv")|> #this is from Tanya
cumulative_subbasin<-read.csv("./hyetograph/output/ready_2_hyet.csv")|>
  mutate(date = as.Date(date))

#data<-cumulative_subbasin

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
  #font = "Open Sans"
  
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
  
  #dff<- cumulative_subbasin<-read.csv("./hyetograph/output/ready_2_hyet.csv")|>
   # mutate(date = as.Date(date))
  
  #get subset of data
  df<-data|>filter(basin %in% basins)
  
  df_pivoted<-df|>
    select(date, basin, median_rain, avg_mm_rainfall)|>
    pivot_longer(cols=c(median_rain,avg_mm_rainfall))|>
   
   # dff_pivoted<-dff|> 
   # select(date, basin, median_rain, cumulative_basin_avg_in)|>
  #  pivot_longer(cols=c(median_rain,cumulative_basin_avg_in))|>
    
    mutate(name = case_when(name=='median_rain' ~ 'Median', TRUE ~ 'Actual'))|>
    left_join(
      df|>select(date, basin, name, perc25, perc75), by=c("date"="date", "name"="name", "basin"="basin")
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

plot_ribbon_facet(
  data = cumulative_subbasin, 
  basins = basins, 
  pal_avg = style$pal$orange,
  pal_title = "black",
  title = "Cumulative Precipitation by Subbasin",
  subtitle = "Comparison of average and median rainfall (mm) YTD",
  caption = "Source: Edwards Aquifer Authority",
  legend = T
)






























# 3) Map old basin names -> display names you want on the GT table
name_map <- c(
  "Frio-Dry Frio"     = "Frio",
  "Seco-Hondo"        = "Hondo",
  "Cibolo-Dry Comal"  = "Cibolo",
  "Guadalupe"         = "Guad",
  "usgs_dissolved"    = "Rchg Zn",
  "Nueces"            = "Nueces",
  "Sabinal"           = "Sabinal",
  "Medina"            = "Medina",
  "Bexar"             = "Bexar",
  "Blanco"            = "Blanco"
)

# 5) Build the two rows with renamed display columns
wide_ready <- daily_edwards_stats %>%
  mutate(
    display = recode(basin, !!!name_map),
    avg_fmt = fmt_in(daily_basin_avg_in),
    max_fmt = fmt_in(daily_max_bin_in),
    cum_fmt = fmt_in(cumulative_precip_in)
  )

