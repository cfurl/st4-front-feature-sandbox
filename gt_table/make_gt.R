################################################################ this one crusshing it!

# ---- 2x10 gt table with uniform transparency (header + labels + body) ----
# Packages
library(gt)
library(dplyr)
library(magick)

# ==== Basin names (9 sub-basins + whole area) ====
basins <- c(
  "Bexar","Blanco","Cibolo","Frio","Guad",
  "Medina","Nueces","Sabinal","Hondo","Rchg Zn"
)

# ==== Dummy values (inches) ====
basin_avg <- c(1.23, 0.98, 1.75, 2.10, 0.65, 0.05, 1.40, 1.88, 0.92, 1.31)
max_bin   <- c(3.80, 2.45, 4.10, 5.25, 1.90, 2.75, 3.30, 4.85, 2.10, 3.95)

# ---- Pre-format: "< 0.10" when < 0.1; otherwise 2 decimals ----
avg_fmt <- ifelse(is.na(basin_avg), "",
                  ifelse(basin_avg < 0.1, "< 0.10", sprintf("%.2f", basin_avg)))
max_fmt <- ifelse(is.na(max_bin), "",
                  ifelse(max_bin < 0.1, "< 0.10", sprintf("%.2f", max_bin)))

# ---- Build 2-row data frame ----
tbl_basin_24hr <- tibble(Metric = c("Basin Avg", "Max Bin"))
for (i in seq_along(basins)) {
  tbl_basin_24hr[[ basins[i] ]] <- c(avg_fmt[i], max_fmt[i])
}

# ---- Column width controls ----
stub_width  <- px(35)
basin_width <- px(30)

# ---- Chroma key to remove later (use a color not used anywhere else) ----
CHROMA <- "white"

# ---- Create gt table: ALL TEXT BOLD; make header + labels + stub + body use CHROMA ----
gt_basin_24hr <-
  tbl_basin_24hr |>
  gt(rowname_col = "Metric") |>
  cols_align(align = "right", columns = all_of(basins)) |>
  cols_width(stub() ~ stub_width, all_of(basins) ~ basin_width) |>
  tab_header(title = md("**24-hr Rainfall Summary (in)**")) |>
  # bold everything
  tab_style(
    style = cell_text(size = px(17), weight = "bold"),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(columns = everything()),
      cells_stub(rows = everything()),
      cells_body(columns = everything())
    )
  ) |>
  # fill all table regions with CHROMA so they become transparent later
  tab_options(
    table.background.color         = CHROMA,  # outer area
    heading.background.color       = CHROMA,
    column_labels.background.color = CHROMA,
    row_group.background.color     = CHROMA,
    table.font.size                = px(12),
    data_row.padding               = px(2),
    table.width                    = pct(100)
  ) |>
  # also ensure body cells + stub get CHROMA (so no opaque patches remain)
  tab_style(
    style = cell_fill(color = CHROMA),
    locations = list(
      cells_body(columns = everything(), rows = everything()),
      cells_stub(rows = everything())
    )
  )

# ---- Save -> trim -> make CHROMA transparent -> write final ----
out_dir   <- "gt_table"
tmp_png   <- file.path(out_dir, "edwards_basin_table_tmp.png")
final_png <- file.path(out_dir, "edwards_basin_table.png")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gtsave(gt_basin_24hr, filename = tmp_png)

img <- image_read(tmp_png)
img <- image_trim(img)
img <- image_transparent(img, color = CHROMA, fuzz = 0)  # remove CHROMA everywhere, keep text/lines

image_write(img, path = final_png, format = "png")


################################################################ this is the rework like the size and place

# ---- 2x10 gt table with uniform transparency (header + labels + body) ----
# Packages
library(gt)
library(dplyr)
library(magick)

# ==== Basin names (9 sub-basins + whole area) ====
basins <- c(
  "Bexar","Blanco","Cibolo","Frio","Guad",
  "Medina","Nueces","Sabinal","Hondo","Rchg Zn"
)

# ==== Dummy values (inches) ====
basin_avg <- c(1.23, 0.98, 1.75, 2.10, 0.65, 0.05, 1.40, 1.88, 0.92, 1.31)
max_bin   <- c(3.80, 2.45, 4.10, 5.25, 1.90, 2.75, 3.30, 4.85, 2.10, 3.95)

# ---- Pre-format: "< 0.10" when < 0.1; otherwise 2 decimals ----
avg_fmt <- ifelse(is.na(basin_avg), "",
                  ifelse(basin_avg < 0.1, "< 0.10", sprintf("%.2f", basin_avg)))
max_fmt <- ifelse(is.na(max_bin), "",
                  ifelse(max_bin < 0.1, "< 0.10", sprintf("%.2f", max_bin)))

# ---- Build 2-row data frame ----
tbl_basin_24hr <- tibble(Metric = c("Basin Avg", "Max Bin"))
for (i in seq_along(basins)) {
  tbl_basin_24hr[[ basins[i] ]] <- c(avg_fmt[i], max_fmt[i])
}

# ---- Column width controls ----
stub_width  <- px(35)
basin_width <- px(30)

# ---- Chroma key to remove later (use a color not used anywhere else) ----
CHROMA <- "white"

# ---- Create gt table: ALL TEXT BOLD; make header + labels + stub + body use CHROMA ----
gt_basin_24hr <-
  tbl_basin_24hr |>
  gt(rowname_col = "Metric") |>
  cols_align(align = "right", columns = all_of(basins)) |>
  cols_width(stub() ~ stub_width, all_of(basins) ~ basin_width) |>
  tab_header(title = md("**24-hr Rainfall Summary (in)**")) |>
  # bold everything
  tab_style(
    style = cell_text(size = px(17), weight = "bold"),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(columns = everything()),
      cells_stub(rows = everything()),
      cells_body(columns = everything())
    )
  ) |>
  # fill all table regions with CHROMA so they become transparent later
  tab_options(
    table.background.color         = CHROMA,  # outer area
    heading.background.color       = CHROMA,
    column_labels.background.color = CHROMA,
    row_group.background.color     = CHROMA,
    table.font.size                = px(12),
    data_row.padding               = px(2),
    table.width                    = pct(100),
    table.border.top.width         = px(0),
    stub.border.style              = "none"
  ) |>
  # also ensure body cells + stub get CHROMA (so no opaque patches remain)
  tab_style(
    style = cell_fill(color = CHROMA),
    locations = list(
      cells_body(columns = everything(), rows = everything()),
      cells_stub(rows = everything())
    )
  )

# ---- Save -> trim -> make CHROMA transparent -> write final ----
out_dir   <- "gt_table"
tmp_png   <- file.path(out_dir, "edwards_basin_table_tmp.png")
final_png <- file.path(out_dir, "edwards_basin_table.png")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gtsave(gt_basin_24hr, filename = tmp_png)

img <- image_read(tmp_png)
img <- image_trim(img)
img <- image_transparent(img, color = CHROMA, fuzz = 0)  # remove CHROMA everywhere, keep text/lines

image_write(img, path = final_png, format = "png")










# now working on opaqueness

################################################################ this is the rework like the size and place

# ---- 2x10 gt table with uniform transparency (header + labels + body) ----
# Packages
library(gt)
library(dplyr)
library(magick)

# ==== Basin names (9 sub-basins + whole area) ====
basins <- c(
  "Bexar","Blanco","Cibolo","Frio","Guad",
  "Medina","Nueces","Sabinal","Hondo","Rchg Zn"
)

# ==== Dummy values (inches) ====
basin_avg <- c(1.23, 0.98, 1.75, 2.10, 0.65, 0.05, 1.40, 1.88, 0.92, 1.31)
max_bin   <- c(3.80, 2.45, 4.10, 5.25, 1.90, 2.75, 3.30, 4.85, 2.10, 3.95)

# ---- Pre-format: "< 0.10" when < 0.1; otherwise 2 decimals ----
avg_fmt <- ifelse(is.na(basin_avg), "",
                  ifelse(basin_avg < 0.1, "< 0.10", sprintf("%.2f", basin_avg)))
max_fmt <- ifelse(is.na(max_bin), "",
                  ifelse(max_bin < 0.1, "< 0.10", sprintf("%.2f", max_bin)))

# ---- Build 2-row data frame ----
tbl_basin_24hr <- tibble(Metric = c("Basin Avg", "Max Bin"))
for (i in seq_along(basins)) {
  tbl_basin_24hr[[ basins[i] ]] <- c(avg_fmt[i], max_fmt[i])
}

# ---- Column width controls ----
stub_width  <- px(35)
basin_width <- px(30)

# ---- Chroma key to remove later (use a color not used anywhere else) ----
CHROMA <- "white"

# ---- Create gt table: ALL TEXT BOLD; make header + labels + stub + body use CHROMA ----
gt_basin_24hr <-
  tbl_basin_24hr |>
  gt(rowname_col = "Metric") |>
  cols_align(align = "right", columns = all_of(basins)) |>
  cols_width(stub() ~ stub_width, all_of(basins) ~ basin_width) |>
  tab_header(title = md("**24-hr Rainfall Summary (in)**")) |>
  # bold everything
  tab_style(
    style = cell_text(size = px(17), weight = "bold"),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(columns = everything()),
      cells_stub(rows = everything()),
      cells_body(columns = everything())
    )
  ) |>
  # fill all table regions with CHROMA so they become transparent later
  tab_options(
    table.background.color         = CHROMA,  # outer area
    heading.background.color       = CHROMA,
    column_labels.background.color = CHROMA,
    row_group.background.color     = CHROMA,
    table.font.size                = px(12),
    data_row.padding               = px(2),
    table.width                    = pct(100),
    table.border.top.width         = px(0),
    stub.border.style              = "none"
  ) |>
  # also ensure body cells + stub get CHROMA (so no opaque patches remain)
  tab_style(
    style = cell_fill(color = CHROMA),
    locations = list(
      cells_body(columns = everything(), rows = everything()),
      cells_stub(rows = everything())
    )
  )

# ---- Save -> trim -> make CHROMA transparent -> write final ----
out_dir   <- "gt_table"
tmp_png   <- file.path(out_dir, "edwards_basin_table_tmp.png")
final_png <- file.path(out_dir, "edwards_basin_table.png")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gtsave(gt_basin_24hr, filename = tmp_png)

# read & trim
img <- image_read(tmp_png) |> image_trim()

# 1) Remove the CHROMA background so only text/lines remain
img_fg <- image_transparent(img, color = CHROMA, fuzz = 0)

# 2) Build a semi-transparent "card" the same size as the trimmed table
#    Adjust the hex alpha to taste: 33≈20%, 66≈40%, 80≈50%, CC≈80%, etc.
card_opacity_hex <- "80"  # ~50% opacity
card_color <- paste0("#FFFFFF", card_opacity_hex)  # translucent white
card <- image_blank(
  width  = image_info(img_fg)$width,
  height = image_info(img_fg)$height,
  color  = card_color
)

# (Optional) add a soft shadow to the card for legibility
# card <- image_shadow(card, color = "black", opacity = 25, sigma = 6, x = 0, y = 2)

# 3) Composite foreground text/lines over the translucent card
img_final <- image_composite(card, img_fg, operator = "over")

# write final PNG with baked-in semi-transparency
image_write(img_final, path = final_png, format = "png")