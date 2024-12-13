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

# Step 3: Filter out rows where LON and LAT are 0 and Vict Descent values are not blank or "-"
filtered_data <- csv_data %>%
  filter(LON != 0, LAT != 0, !is.na(Vict.Descent), Vict.Descent != "-")

# Step 4: Identify top 3 victim descents
top_descents <- filtered_data %>%
  count(Vict.Descent, sort = TRUE) %>%
  top_n(3, n) %>%
  pull(Vict.Descent)

# Step 5: Filter data for only the top 3 descents
# Rename Vict Descent values for clarity
filtered_data <- filtered_data %>%
  mutate(Vict.Descent = case_when(
    Vict.Descent == "H" ~ "Hispanic/Latino",
    Vict.Descent == "W" ~ "White",
    Vict.Descent == "B" ~ "Black/African American",
    TRUE ~ Vict.Descent
  ))

top_descents <- filtered_data %>%
  count(Vict.Descent, sort = TRUE) %>%
  top_n(3, n) %>%
  pull(Vict.Descent)

# Step 6: Filter data for only the top 3 descents
filtered_top_descents <- filtered_data %>%
  filter(Vict.Descent %in% top_descents)

# Step 7: Convert filtered data to an sf object
points_sf <- st_as_sf(filtered_top_descents, coords = c("LON", "LAT"), crs = 4326)

# Step 8: Transform coordinate system to match Los Angeles shapefile
points_sf <- st_transform(points_sf, st_crs(la_shapefile))

# Step 9: Plot the map with all points in one color and full descent names
ggplot() +
  geom_sf(data = la_shapefile, fill = NA, color = "darkgrey", linewidth = 0.5) +
  geom_sf(data = points_sf, color = "#3A5B7D", size = 0.1, alpha = 1) +
  facet_wrap(~ Vict.Descent, ncol = 3) +
  ggtitle("Map of Los Angeles by Top 3 Victim Descents") +
  theme_minimal() +
  theme(
    legend.position = "none",          # Remove legend for uniform color
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5) # Centered title
  )
