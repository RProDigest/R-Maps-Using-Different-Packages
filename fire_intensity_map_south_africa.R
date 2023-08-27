####
# Plotting Active Fire Maps with R Using NASA Data
#
# @RProDigest, Inspired by Milos Popovic, Ph.D (@milos-makes-maps)
#
# August 27th, 2023
#
#####

# Load necessary packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,      # For data manipulation and visualization
  data.table,     # For reading fire data
  ggmap,          # For plotting the final map
  gganimate,      # For creating animations
  geodata,        # For downloading geographical data
  sf              # For working with geospatial data
)

# 1. Get Boundary Box for South Africa
#--------------------------------------

# Use 'sf' and 'geodata' packages to retrieve an accurate bounding box for South Africa
southAfrica <- gadm(country = "ZAF", level = 2, path = "ENTER YOUR CURRENT R WORKING DIRECTORY HERE") |>
  st_as_sf()

# Extract coordinates for the bounding box
southafrica <- sf::st_bbox(southAfrica)
xmin <- 16.452
ymin <- -34.835
xmax <- 32.891
ymax <- -22.125

# Define the bounding box area
area_coords <- c(xmin, ymin, xmax, ymax)
area <- paste(area_coords, sep = ",", collapse = ",")

# 2. FIRE DATA
#-------------

# Function to fetch fire data from NASA API
get_fire_data <- function(main_url, map_key, source, area, day_range, date) {
  url <- paste(main_url, map_key, source, area, day_range, date, sep = "/")
  fire_data <- data.table::fread(url)
  return(fire_data)
}

# Define parameters for fire data retrieval
main_url <- "https://firms2.modaps.eosdis.nasa.gov/api/area/csv"
map_key <- "XXXXXXXXXX" # Replace with your own map key from here: https://firms2.modaps.eosdis.nasa.gov/api/map_key/
source <- "VIIRS_SNPP_NRT"
day_range <- 10
date <- Sys.Date() - 11

# Fetch fire data using the defined function
fire_data <- get_fire_data(
  main_url = main_url,
  map_key = map_key,
  source = source,
  area = area,
  day_range = day_range,
  date = date
)

# Display a preview of the fire data
head(fire_data)
nrow(fire_data)

# 3. BACKGROUND
#--------------

# Get a background map layer using 'ggmap' package
bg_layer <- ggmap::get_stamenmap(
  bbox = area_coords,
  zoom = 7,
  maptype = "terrain",
  color = "bw",
  force = TRUE
)

# Convert fire data to spatial data format
fire_data |> 
  select(latitude, longitude, acq_date, bright_ti5) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) -> fire_data_sf

# Join fire data with South Africa boundary data
joined <- st_join(fire_data_sf, southAfrica, left = FALSE)

# Prepare fire data for plotting
fire_data_updated <- joined %>%
  mutate(x = st_coordinates(.)[, "X"],
         y = st_coordinates(.)[, "Y"]) %>%
  select(x, y, bright_ti5, acq_date, -geometry) |> 
  rename(longitude = x, latitude = y)

# Plot the background map
ggmap::ggmap(bg_layer)

# 4. THEME
#----------

# Define a custom theme for the map
theme_for_the_win <- function() {
  theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 12, color = "grey10"),
      legend.text = element_text(size = 11, color = "grey10"),
      plot.title = element_text(size = 16, color = "grey10", hjust = .5),
      plot.subtitle = element_text(face = "bold", size = 24, color = "firebrick", hjust = .5),
      plot.caption = element_text(size = 10, color = "grey30", hjust = .5, vjust = 0),
      plot.margin = unit(c(t = -2, r = 0, b = -5, l = .1), "lines")
    )
}

# 5. MAP
#-------

# Convert date column to proper date format
fire_data_updated$datum <- as.Date(fire_data_updated$acq_date)

# Create the base map with fire data points
p <- ggmap::ggmap(bg_layer) +
  geom_point(
    data = fire_data_updated,
    aes(
      x = longitude,
      y = latitude,
      color = bright_ti5 - 273.15,
      group = datum
    ),
    inherit.aes = FALSE
  ) +
  scale_color_gradientn(
    name = "Brightness\nTemperature\n(°C)",
    colors = rev(hcl.colors(6, "Inferno"))
  ) +
  theme_for_the_win() +
  labs(
    title = "Wildfires In South Africa: 16-25 August 2023",
    caption = "Data: NASA FIRMS, Plotted by @RProDigest",
    subtitle = "{as.Date(frame_time)}"
  )

# 6. ANIMATE
#-----------

# Create an animated map
timelapse_map <- p +
  gganimate::transition_time(time = as.Date(datum)) +
  gganimate::shadow_mark() +
  gganimate::enter_fade() +
  gganimate::exit_fade() +
  gganimate::ease_aes("cubic-in-out", interval = 0.25)

# Generate the animated map
animated_map <- gganimate::animate(
  timelapse_map,
  nframes = 60,
  duration = 12,
  start_pause = 3,
  end_pause = 30,
  width = 7,
  height = 7,
  units = "in",
  res = 300,
  fps = 15,
  renderer = gifski_renderer(loop = TRUE)
)

# Save the animated map as a GIF
gganimate::anim_save("fire-southAfrica_v1.gif", animated_map)
