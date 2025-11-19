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
library("shiny")
library("tictoc")
#library("ggiraph")

######################## Some S3 things #####################
# remove this from container setup, this gives your local dev the AWS access
readRenviron("../.Renviron") # this is for keys one level up from root directory
#readRenviron(".Renviron") # when it's in gitignore

required <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")
missing  <- required[Sys.getenv(required) == ""]
if (length(missing)) {
  stop("Missing env vars on Connect: ", paste(missing, collapse = ", "))
}

# make sure you can connect to your bucket and open SubTreeFileSystem and identify path
# then connect to the .parq files on the s3 storage
#bucket <- s3_bucket("stg4-texas-24hr-historical")
bucket <- s3_bucket("stg4-edwards-24hr-historical")
s3_path <- bucket$path("")
stg4_24hr_texas_parq <- open_dataset(s3_path)

############################ time stamps #############
current_utc_date_time <- with_tz(Sys.time(), "UTC")
current_central_date_time <- with_tz(Sys.time(), "America/Chicago")

# call the gis layers you want mapped
gs_basins<-read_sf("../gis/usgs_basins.shp")
map <- sf::read_sf("../gis/usgs_dissolved.shp") # this is your hrap polygon
streams <- read_sf("../gis/streams_recharge.shp")
lakes <- read_sf("../gis/reservoirs.shp")

# Mapping function edited from Tanya's work
plot_bin_map<-function(
    title = 'Edwards Aquifer Recharge Zone',
    subtitle= NA,
    note_title = NA,
    font = "Open Sans",
    map_rain = NA,
    map_streams = NA, 
    map_lakes = NA,
    pal_water='black',
    pal_title='white',
    pal_subtitle='white',
    pal_outline='black',
    pal_bin_outline='black',
    pal_legend_text='white',
    bin_alpha = 0.7,
    map_type='cartodark'
){
  
  bbox <- st_bbox(c(
    xmin = -100.85,
    ymin = 29.0, 
    xmax = -97.75, 
    ymax = 30.47
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
  rain_breaks  <- c(0, 0.1, 0.25, 0.5, 1, 2, 3, 4, 6, 8, 10, 12)
  rain_labels  <- c("0","0.1","0.25","0.5","1","2","3","4","6","8","10","12+")
  rain_limits  <- c(0, 12)
  
  # --- Set 0 rainfall to NA for transparency ---
  map_rain <- map_rain |>
    mutate(fill_val = ifelse(sum_rain_in == 0, NA_real_, sum_rain_in))
  
  plot<-ggplot()+
    annotation_map_tile(
      type = map_type,  # Use the "Carto Light" basemap
      zoom = 9  # Adjust zoom level as needed
    )+
    
    annotate(geom="text",x= title_pos$X,y=title_pos$Y,label=title,size=7.75,hjust=0, color = pal_title, family=font, fontface='bold')+
    annotate(geom="text",x= subtitle_pos$X,y=subtitle_pos$Y,label=subtitle,size=3.5,hjust=0, color = pal_subtitle, family=font)+
    annotate(geom="text",x=  note_title_pos$X,y= note_title_pos$Y,label=note_title,size=2,hjust=0, color = pal_subtitle, family=font)+
    geom_sf(data = map_rain, mapping = aes(fill = fill_val), color = pal_bin_outline, alpha = bin_alpha, na.rm = FALSE) +
    geom_sf(data = outline|>st_transform(crs = coord_sys), color = pal_outline, linewidth = 0.4) +  
    #geom_sf(data = gs_basins|>st_transform(crs = coord_sys), color = alpha("black", 0.5), linewidth = 0.2, fill=NA) +  
    geom_sf(data=map_lakes|>st_transform(crs = coord_sys), fill= pal_water, color= pal_water, linewidth = 0.2)+
    geom_sf(data=map_streams|>st_transform(crs = coord_sys), color= pal_water, linewidth = 0.1)+
    
    scale_fill_stepsn(
      colours = c("#82D3F0","#0826A2","#22FE05","#248418",
                  "#F6FB07","#FFC348","#E01E17","#8C302C",
                  "#CC17DA","#AE60B3","#FDF5FF"),
      breaks    = rain_breaks,
      limits    = rain_limits,
      labels    = rain_labels,
      oob       = scales::squish,
      name      = "Rainfall (in)",
      na.value  = NA  # keep transparency for NA (zero rainfall)
    ) +
    guides(
      fill = guide_colorsteps(
        title.position = "top",
        title.vjust = 0.1,
        show.limits = TRUE
      )
    )+
    coord_sf(
      xlim = c(st_bbox(bbox_transformed)["xmin"], st_bbox(bbox_transformed)["xmax"]),
      ylim = c(st_bbox(bbox_transformed)["ymin"], st_bbox(bbox_transformed)["ymax"])
    ) +
    theme_void()+
    theme(
      text = element_text(family=font),
      legend.position = "inside",
      legend.position.inside = c(0.70,0.1),  
      legend.direction = "horizontal", 
      legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
      legend.title = element_text(size = 10, face='bold', color=pal_legend_text), 
      legend.text = element_text(size = 9, color=pal_legend_text),  
      legend.key.width = unit(2.5, "cm"), 
      legend.key.height = unit(0.5, "cm")  
    )
  
  return(plot)
}

# create parquet filter
filter_date_range <- function(ds, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  sy <- lubridate::year(start_date);  sm <- lubridate::month(start_date)
  ey <- lubridate::year(end_date);    em <- lubridate::month(end_date)
  
  # base dataset (Arrow Dataset or arrow_dplyr_query)
  ds_base <- ds
  
  if (sy == ey) {
    # Simple case: everything is inside one year
    ds_sub <- ds_base %>%
      filter(
        year == sy,
        month >= sm,
        month <= em
      )
  } else {
    # Start-year: months >= sm
    ds_start <- ds_base %>%
      filter(
        year == sy,
        month >= sm
      )
    
    # End-year: months <= em
    ds_end <- ds_base %>%
      filter(
        year == ey,
        month <= em
      )
    
    # Middle full years, if any
    if (ey > sy + 1) {
      ds_mid <- ds_base %>%
        filter(
          year > sy,
          year < ey
        )
      ds_sub <- dplyr::union_all(ds_start, ds_mid) %>%
        dplyr::union_all(ds_end)
    } else {
      ds_sub <- dplyr::union_all(ds_start, ds_end)
    }
  }
  
  ds_sub
}

## ---- UI ----
ui <- fluidPage(
  style = "padding:0; margin:0;",
  tags$head(tags$title("Rainfall Map")),
  
  fluidRow(
    # Left control panel
    column(
      width = 2,
      style = "background-color:#f8f9fa; height:100vh; padding:1rem;",
      h4("Filters"),
      dateRangeInput(
        inputId = "date_range",
        label   = "Date range",
        start   = "2003-01-01",          # default start
        end     = "2003-01-07",          # default end
        min     = as.Date("2002-01-01"), # earliest STG4 date (adjust if needed)
        max     = Sys.Date()
      )
    ),
    
    # Right plot area
    column(
      width = 10,
      style = "padding:0;",
      plotOutput("rain_map", width = "100%", height = "100vh")
    )
  )
)

## ---- server ----

server <- function(input, output, session) {
  
  map_rain_reactive <- reactive({
    req(input$date_range)
    
    start_date <- as.Date(input$date_range[1])
    end_date   <- as.Date(input$date_range[2])
    
    # Build UTC timestamp bounds for the *exact* date window
    start_ts <- as.POSIXct(start_date, tz = "UTC")
    end_ts   <- as.POSIXct(end_date + 1, tz = "UTC")  # < end_date+1 = inclusive end_date
    
    # 1) Prune by year/month partitions with Arrow-friendly filter
    ds_window <- stg4_24hr_texas_parq %>%
      filter_date_range(start_date, end_date)
    
    # 2) Within those months, refine by time and aggregate ON THE DATASET
    d <- ds_window %>%
      filter(
        time >= start_ts,
        time <  end_ts
      ) %>%
      group_by(grib_id) %>%
      summarize(
        sum_rain = sum(rain_mm, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      arrange(desc(sum_rain)) %>%
      collect()
    
    # 3) Join to map grid and add derived fields
    map %>%
      left_join(d, by = "grib_id") %>%
      mutate(
        cubic_m_precip = bin_area * sum_rain * 0.001,  # mm -> m * area
        sum_rain_in    = sum_rain / 25.4               # mm -> inches
      )
  })
  
  output$rain_map <- renderPlot({
    req(input$date_range)
    
    start_date <- as.Date(input$date_range[1])
    end_date   <- as.Date(input$date_range[2])
    map_rain   <- map_rain_reactive()
    
    plot_bin_map(
      title = "Edwards Aquifer Recharge Zone",
      subtitle = paste(
        "Precipitation from",
        format(start_date, "%Y-%m-%d"),
        "to",
        format(end_date, "%Y-%m-%d")
      ),
      note_title = paste(
        "Produced at",
        format(current_utc_date_time, "%Y-%m-%d %H:%M %Z"),
        "and",
        format(current_central_date_time, "%Y-%m-%d %H:%M %Z")
      ),
      font = "",
      map_rain    = map_rain,
      map_streams = streams,
      map_lakes   = lakes,
      pal_water       = "#2C6690",
      pal_title       = "black",
      bin_alpha       = 0.9,
      pal_subtitle    = "black",
      pal_outline     = "#697984",
      pal_bin_outline = NA,
      pal_legend_text = "black",
      map_type        = "cartolight"
    )
  }, res = 144)
}

shinyApp(ui, server)

