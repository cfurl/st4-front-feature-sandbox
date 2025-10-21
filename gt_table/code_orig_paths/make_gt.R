
# Packages
library(gt)
library(dplyr)
library(magick)
library(webshot2)
library(arrow)
library(lubridate)
library(tidyr)

readRenviron(".Renviron") # when it's in gitignore

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
daily_edwards_stats <- stg4_edwards_daily_stats_24hr |>
  collect()  |>
  group_by(basin)|>
  mutate(cumulative_precip_in = sum(daily_basin_avg_in)) |>
  filter (date == as.Date(current_utc_date))|>
    ungroup()

# Formatting: "< 0.10" when >0 & <0.10; "" for NA; otherwise 2 decimals
fmt_in <- function(x) {
  ifelse(is.na(x), "",
         ifelse(x > 0 & x < 0.10, "< 0.1", sprintf("%.1f", x)))
}


#working on new gt table.......

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

# 4) Desired left->right column order (deduped);

desired_order <- c(
  "Nueces",
  "Frio",
  "Sabinal",
  "Medina",
  "Hondo",
  "Bexar",
  "Cibolo",
  "Guad",
  "Blanco",
  "Rchg Zn"
) |> unique()


# 5) Build the two rows with renamed display columns
wide_ready <- daily_edwards_stats %>%
  mutate(
    display = recode(basin, !!!name_map),
    avg_fmt = fmt_in(daily_basin_avg_in),
    max_fmt = fmt_in(daily_max_bin_in),
    cum_fmt = fmt_in(cumulative_precip_in)
  )

avg_row <- wide_ready %>%
  select(display, value = avg_fmt) %>%
  mutate(Metric = "Basin Avg") %>%
  pivot_wider(names_from = display, values_from = value)

max_row <- wide_ready %>%
  select(display, value = max_fmt) %>%
  mutate(Metric = "Max Bin") %>%
  pivot_wider(names_from = display, values_from = value)

cum_row <- wide_ready %>%
  select(display, value = cum_fmt) %>%
  mutate(Metric = "Basin Avg") %>%
  pivot_wider(names_from = display, values_from = value)

tbl_basin_24hr <- bind_rows(avg_row, max_row) %>%
  relocate(Metric)

existing_cols <- setdiff(names(tbl_basin_24hr), "Metric")
order_final <- c(desired_order, setdiff(existing_cols, desired_order))
tbl_basin_24hr <- tbl_basin_24hr %>% select(Metric, all_of(order_final))

tbl_basin_cum <- bind_rows(cum_row) %>%
  relocate(Metric)

tbl_basin_cum <- tbl_basin_cum %>% select(Metric, all_of(order_final))

# ---- Column width controls ----
stub_width  <- px(35)
basin_width <- px(30)

# ---- Chroma key to remove later (use a color not used anywhere else) ----
CHROMA <- "white"


basin_cols <- intersect(desired_order, names(tbl_basin_24hr))


gt_basin_24hr <-
  tbl_basin_24hr |>
  gt(rowname_col = "Metric") |>
  cols_align(align = "right", columns = all_of(basin_cols)) |>
  cols_width(stub() ~ stub_width, all_of(basin_cols) ~ basin_width) |>
  tab_header(title = md("**24-hr Rainfall Summary (in)**")) |>
  tab_style(
    style = cell_text(size = px(17), weight = "bold"),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(columns = everything()),
      cells_stub(rows = everything()),
      cells_body(columns = everything())
    )
  ) |>
  tab_options(
    table.background.color         = CHROMA,
    heading.background.color       = CHROMA,
    column_labels.background.color = CHROMA,
    row_group.background.color     = CHROMA,
    table.font.size                = px(12),
    data_row.padding               = px(2),
    table.width                    = pct(100),
    table.border.top.width         = px(0),
    stub.border.style              = "none"
  ) |>
  tab_style(
    style = cell_fill(color = CHROMA),
    locations = list(
      cells_body(columns = everything(), rows = everything()),
      cells_stub(rows = everything())
    )
  )

# ---- Save -> trim -> make CHROMA transparent -> write final ----
out_dir   <- "gt_table/output"
tmp_png_24   <- file.path(out_dir, "edwards_basin_table_tmp_24.png")
final_png_24 <- file.path(out_dir, "edwards_basin_table_24.png")
#if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gtsave(gt_basin_24hr, filename = tmp_png_24)

# read & trim
img_24 <- image_read(tmp_png_24) |> image_trim()

# 1) Remove the CHROMA background so only text/lines remain
img_fg_24 <- image_transparent(img_24, color = CHROMA, fuzz = 0)

# 2) Build a semi-transparent "card" the same size as the trimmed table
#    Adjust the hex alpha to taste: 33≈20%, 66≈40%, 80≈50%, CC≈80%, etc.
card_opacity_hex <- "80"  # ~50% opacity
card_color <- paste0("#FFFFFF", card_opacity_hex)  # translucent white
card_24 <- image_blank(
  width  = image_info(img_fg_24)$width,
  height = image_info(img_fg_24)$height,
  color  = card_color
)

# (Optional) add a soft shadow to the card for legibility
# card <- image_shadow(card, color = "black", opacity = 25, sigma = 6, x = 0, y = 2)

# 3) Composite foreground text/lines over the translucent card
img_final_24 <- image_composite(card_24, img_fg_24, operator = "over")

# write final PNG with baked-in semi-transparency
image_write(img_final_24, path = final_png_24, format = "png")





gt_basin_cum <-
  tbl_basin_cum |>
  gt(rowname_col = "Metric") |>
  cols_align(align = "right", columns = all_of(basin_cols)) |>
  cols_width(stub() ~ stub_width, all_of(basin_cols) ~ basin_width) |>
  tab_header(title = md("**YTD Rainfall Summary (in)**")) |>
  tab_style(
    style = cell_text(size = px(17), weight = "bold"),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(columns = everything()),
      cells_stub(rows = everything()),
      cells_body(columns = everything())
    )
  ) |>
  tab_options(
    table.background.color         = CHROMA,
    heading.background.color       = CHROMA,
    column_labels.background.color = CHROMA,
    row_group.background.color     = CHROMA,
    table.font.size                = px(12),
    data_row.padding               = px(2),
    table.width                    = pct(100),
    table.border.top.width         = px(0),
    stub.border.style              = "none"
  ) |>
  tab_style(
    style = cell_fill(color = CHROMA),
    locations = list(
      cells_body(columns = everything(), rows = everything()),
      cells_stub(rows = everything())
    )
  )


# ---- Save -> trim -> make CHROMA transparent -> write final ----
out_dir   <- "gt_table/output"
tmp_png_cum   <- file.path(out_dir, "edwards_basin_table_tmp_cum.png")
final_png_cum <- file.path(out_dir, "edwards_basin_table_cum.png")
#if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gtsave(gt_basin_cum, filename = tmp_png_cum)

# read & trim
img_cum <- image_read(tmp_png_cum) |> image_trim()

# 1) Remove the CHROMA background so only text/lines remain
img_fg_cum <- image_transparent(img_cum, color = CHROMA, fuzz = 0)

# 2) Build a semi-transparent "card" the same size as the trimmed table
#    Adjust the hex alpha to taste: 33≈20%, 66≈40%, 80≈50%, CC≈80%, etc.
#card_opacity_hex <- "80"  # ~50% opacity
#card_color <- paste0("#FFFFFF", card_opacity_hex)  # translucent white
card_cum <- image_blank(
  width  = image_info(img_fg_cum)$width,
  height = image_info(img_fg_cum)$height,
  color  = card_color
)

# (Optional) add a soft shadow to the card for legibility
# card <- image_shadow(card, color = "black", opacity = 25, sigma = 6, x = 0, y = 2)

# 3) Composite foreground text/lines over the translucent card
img_final_cum <- image_composite(card_cum, img_fg_cum, operator = "over")

# write final PNG with baked-in semi-transparency
image_write(img_final_cum, path = final_png_cum, format = "png")



