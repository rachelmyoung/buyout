#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
  library(tools)
})

# =========================
# USER SETTINGS
# =========================

# Input geocoded addresses
addr_file <- "/Users/rachelyoung/Dropbox/Princeton/research/buyoutprogram/2026/data/raw/uniqueAddress_20002017_GeoCoded_joinCensusTract2000_ACS2000.csv"

# Directory containing FEMA .rda files, ideally one per state
fema_dir <- "/Users/rachelyoung/Dropbox/Princeton/research/buyoutprogram/2026/data/raw/by_state"

# Output directory
out_dir <- "/Users/rachelyoung/Dropbox/Princeton/research/buyoutprogram/2026/data/intermediate"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Column names in the address file
id_col    <- "address_id"
state_col <- "user_state"        # should match state abbreviation or whatever your FEMA filenames use
lon_col   <- "longitude"
lat_col   <- "latitude"

# Whether to keep all addresses (even those not in SFHA polygons)
keep_all_points <- TRUE


id_col    <- "target_fid"
state_col <- "user_state"
lon_col   <- "x"
lat_col   <- "y"

keep_all_points <- TRUE

fema_keep_cols <- c(
  "fld_zone",
  "zone_subty",
  "sfha_tf",
  "static_bfe",
  "depth",
  "velocity",
  "source_cit"
)

# =========================
# HELPERS
# =========================

message2 <- function(...) cat(..., "\n")

clean_state <- function(x) {
  tolower(trimws(as.character(x)))
}

read_fema_rds <- function(path) {
  obj <- readRDS(path)
  
  if (inherits(obj, c("SpatialPolygonsDataFrame", "SpatialPolygons", "Spatial"))) {
    obj <- st_as_sf(obj)
  }
  
  if (!inherits(obj, "sf")) {
    stop("Object in file is not an sf or Spatial object: ", path)
  }
  
  # remember old geometry column name before renaming
  old_geom <- attr(obj, "sf_column")
  
  # lowercase names
  names(obj) <- tolower(names(obj))
  
  # reset active geometry column to the lowercased version
  st_geometry(obj) <- tolower(old_geom)
  
  obj
}


find_fema_file <- function(state_abbrev, fema_dir) {
  files <- list.files(
    fema_dir,
    pattern = "\\.rds$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    stop("No .rds files found in ", fema_dir)
  }
  
  st <- tolower(trimws(as.character(state_abbrev)))
  target_file <- paste0(st, "_sfha.rds")
  
  hits <- files[tolower(basename(files)) == target_file]
  
  if (length(hits) == 1) {
    return(hits)
  }
  
  if (length(hits) == 0) {
    warning("No FEMA .rds file found for state: ", state_abbrev,
            " (expected filename ", target_file, ")")
    return(NA_character_)
  }
  
  warning("Multiple FEMA files matched for ", state_abbrev, "; using first: ", hits[1])
  hits[1]
}

subset_fema_cols <- function(sf_obj, keep_cols) {
  present <- intersect(keep_cols, names(sf_obj))
  if (length(present) == 0) {
    warning("None of requested FEMA columns found; keeping all FEMA columns")
    return(sf_obj)
  }
  sf_obj[, c(present, attr(sf_obj, "sf_column")), drop = FALSE]
}

# =========================
# GET STATE LIST ONLY
# =========================

message2("Reading state list only...")

state_dt <- fread(
  addr_file,
  select = state_col
)

state_dt[[state_col]] <- clean_state(state_dt[[state_col]])

states <- sort(unique(state_dt[[state_col]]))
states <- intersect(states, tolower(state.abb))

rm(state_dt)
gc()

message2("States to process: ", paste(states, collapse = ", "))

# =========================
# LOOP OVER STATES
# =========================

for (st in states) {
  
  message2("Processing ", st, "...")
  
  addr_state <- fread(
    addr_file,
    select = c(id_col, state_col, lon_col, lat_col, "user_address")
  )
  
  addr_state[[state_col]] <- clean_state(addr_state[[state_col]])
  addr_state <- addr_state[get(state_col) == st]
  
  # remove PO Boxes
  addr_state <- addr_state[
    !grepl(
      "^\\s*(P\\.?O\\.?\\s*BOX|RR\\s*\\d+|R\\s*R\\s*\\d+|RURAL\\s+ROUTE|ROUTE\\s*\\d+)",
      user_address,
      ignore.case = TRUE
    )
  ]
  
  # convert coords
  addr_state[, (lon_col) := as.numeric(get(lon_col))]
  addr_state[, (lat_col) := as.numeric(get(lat_col))]
  
  # remove bad coords
  addr_state <- addr_state[
    !is.na(get(lon_col)) &
      !is.na(get(lat_col)) &
      get(lon_col) != 0 &
      get(lat_col) != 0
  ]
  
  # drop address text afterward
  addr_state <- addr_state[, c(id_col, state_col, lon_col, lat_col), with = FALSE]
  
  message2("  State in loop: ", st)
  message2("  Unique user_state in filtered data:")
  print(unique(addr_state[[state_col]]))
  
  # -------------------------
  # Convert to sf
  # -------------------------
  addr_state <- st_as_sf(
    addr_state,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )
  
  # -------------------------
  # FEMA file
  # -------------------------
  fema_file <- find_fema_file(st, fema_dir)
  
  if (is.na(fema_file)) {
    message2("  No FEMA file for ", st, ", skipping.")
    next
  }
  
  message2("  Reading ", basename(fema_file))
  fema_sf <- read_fema_rds(fema_file)
  
  keep_cols <- intersect(fema_keep_cols, names(fema_sf))
  fema_sf <- fema_sf[, c(keep_cols, attr(fema_sf, "sf_column")), drop = FALSE]
  
  addr_state <- st_transform(addr_state, st_crs(fema_sf))
  fema_sf <- st_crop(fema_sf, st_bbox(addr_state))
  
  joined <- st_join(
    addr_state,
    fema_sf,
    join = st_intersects,
    left = TRUE
  )
  
  joined_out <- joined %>% st_drop_geometry()
  
  out_file <- file.path(out_dir, paste0("addresses_fema_join_", st, ".csv"))
  fwrite(as.data.table(joined_out), out_file)
  
  message2("  Wrote ", out_file)
  message2("  Rows in joined output: ", format(nrow(joined_out), big.mark = ","))
  
  rm(addr_state, fema_sf, joined, joined_out)
  gc()
}

# =========================
# COMBINE OUTPUTS
# =========================

out_files <- list.files(
  out_dir,
  pattern = "^addresses_fema_join_[a-z]{2}\\.csv$",
  full.names = TRUE
)

if (length(out_files) == 0) {
  stop("No state output files were created.")
}

final_dt <- rbindlist(lapply(out_files, fread), fill = TRUE, use.names = TRUE)

combined_file <- file.path(out_dir, "addresses_fema_sfha_all_states.csv")
fwrite(final_dt, combined_file)

message2("Done.")
message2("Combined file: ", combined_file)

# =========================
# COMBINE OUTPUTS
# =========================
addr_dt <- fread(addr_file)

# remove PO Boxes
addr_dt <- addr_dt[
  !grepl(
    "^\\s*(P\\.?O\\.?\\s*BOX|RR\\s*\\d+|R\\s*R\\s*\\d+|RURAL\\s+ROUTE|ROUTE\\s*\\d+)",
    user_address,
    ignore.case = TRUE
  )
]
# remove bad coords
addr_dt <- addr_dt[
  !is.na(get(lon_col)) &
    !is.na(get(lat_col)) &
    get(lon_col) != 0 &
    get(lat_col) != 0
]

merged <- left_join(addr_dt, final_dt, by="target_fid")

combined_file <- file.path(out_dir, "unique_addresses_fema_sfha_census.csv")
fwrite(final_dt, combined_file)

