# Load necessary libraries
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(stringr) # For string manipulation
library(readr) # For reading CSV files
library(tmap)

# Set options for tigris
options(tigris_class = "sf", tigris_use_cache = TRUE)

# Step 1: Fetch Los Angeles city shapefile
la_shapefile <- places(state = "CA", cb = TRUE) %>%
  filter(NAME == "Los Angeles")

# Step 2: Read and combine all CSV files into one data frame
csv_files <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)
csv_data <- do.call(rbind, lapply(csv_files, read.csv))

# Step 3: Replace specific value in "Crm Cd Desc"
csv_data <- csv_data %>%
  mutate(Crm.Cd.Desc = str_replace(
    Crm.Cd.Desc, 
    "VANDALISM - FELONY \\(\\$400 & OVER, ALL CHURCH VANDALISMS\\)", 
    "VANDALISM - FELONY"
  ))

# Step 4: Filter out rows where LON and LAT are 0
filtered_data <- csv_data %>%
  filter(LON != 0, LAT != 0, Vict.Sex == 'M')

# Step 5: Identify top 5 crimes in "Crm Cd Desc"
top_crimes <- filtered_data %>%
  count(Vict.Sex, sort = TRUE) %>%
  pull(Vict.Sex)

# Step 6: Filter data for only the top 5 crimes
filtered_top_crimes <- filtered_data %>%
  filter(Vict.Sex %in% top_crimes)

# Step 7: Convert filtered data to an sf object
points_sf <- st_as_sf(filtered_top_crimes, coords = c("LON", "LAT"), crs = 4326)

# Step 8: Transform coordinate system to match Los Angeles shapefile
points_sf <- st_transform(points_sf, st_crs(la_shapefile))

# Step 9: Plot the map with points in red
ggplot() +
  geom_sf(data = la_shapefile, fill = NA, color = "darkgrey", linewidth = 0.5) +
  geom_sf(data = points_sf, color = "#F9766E", size = 0.1, alpha = 1) +  ##F9766E  619DFF
  ggtitle("Map of Los Angeles with Female Victims") +
  theme_minimal() +
  theme(
    legend.position = "none",          # Remove legend for uniform color
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5) # Centered title
  )

# Step 9: Plot the map with points in red
ggplot() +
  geom_sf(data = la_shapefile, fill = NA, color = "darkgrey", linewidth = 0.5) +
  geom_sf(data = points_sf, color = "#619DFF", size = 0.1, alpha = 1) +  ##F9766E  619DFF
  ggtitle("Map of Los Angeles with Male Victims") +
  theme_minimal() +
  theme(
    legend.position = "none",          # Remove legend for uniform color
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5) # Centered title
  )

