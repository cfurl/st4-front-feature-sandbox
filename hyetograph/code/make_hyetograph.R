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
# Read in your month_day stats by basin:

# keep out of github and docker containers
readRenviron(".Renviron") 

# some AWS checks
required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

#for ribbon charts
cumulative_subbasin<-read.csv("./hyetograph/output/ready_2_hyet.csv")|>
  mutate(date = as.Date(date))


# some time things for figure labeling
date_info <- cumulative_subbasin %>%
  summarise(
    first_date = min(date, na.rm = TRUE),
    last_date_with_data = max(date[!is.na(cumulative_basin_avg_in)])
  )



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

precip_facet_pane<-plot_ribbon_facet(
  data = cumulative_subbasin, 
  basins = basins, 
  pal_avg = "#E69F00",        # or "orange" or whatever you like
  pal_title = "black",
  title = "Cumulative Precipitation by Subbasin",
  subtitle = "Comparison of average and median rainfall (mm) YTD",
  caption = "Source: Edwards Aquifer Authority",
  legend = TRUE
)

##### single

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

precip_single_pane<-plot_ribbon(
  data = cumulative_subbasin, 
  select_basin = 'usgs_dissolved', 
  pal_avg = "#E69F00",
  pal_title = "black",
  title = "Cumulative Precipitation 2023",
  subtitle = "Comparison of average and median rainfall (mm) YTD",
  caption = "Source: Edwards Aquifer Authority",
  legend = T
)



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
                     format(date_info[1], "%Y-%m-%d"), " to ",
                     format(date_info[2], "%Y-%m-%d")),
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


combo_hyet <- "hyetograph/output/combo_hyet.png"
ggsave(combo_hyet, fp, device = ragg::agg_png, width = 3840, height = 2160, units = "px")









