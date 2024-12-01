##################################################################################################################################

###########   Question 1: What are the most common types of crimes and high-risk areas in Los Angeles?  ##########################

##################################################################################################################################
# Load necessary libraries
# Load necessary libraries
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)

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
  filter(LON != 0, LAT != 0)

# Step 5: Identify top 5 crimes in "Crm Cd Desc"
top_crimes <- filtered_data %>%
  count(Crm.Cd.Desc, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(Crm.Cd.Desc)

# Step 6: Filter data for only the top 5 crimes
filtered_top_crimes <- filtered_data %>%
  filter(Crm.Cd.Desc %in% top_crimes)

# Step 7: Convert filtered data to an sf object
points_sf <- st_as_sf(filtered_top_crimes, coords = c("LON", "LAT"), crs = 4326)

# Step 8: Transform coordinate system to match Los Angeles shapefile
points_sf <- st_transform(points_sf, st_crs(la_shapefile))

# Step 9: Plot the map with points categorized by crime
ggplot() +
  geom_sf(data = la_shapefile, fill = "lightblue", color = "darkblue", linewidth = 0.5) +
  geom_sf(data = points_sf, aes(color = Crm.Cd.Desc), size = 2, alpha = 0.8) +
  scale_color_viridis_d(name = "Crime Type") +
  ggtitle("Map of Los Angeles with Top 5 Crimes") +
  theme_minimal() +
  theme(
    legend.position = "bottom",        # Move legend to the bottom
    legend.title = element_text(size = 10), # Customize legend title size
    legend.text = element_text(size = 9),   # Customize legend text size
    legend.box = "horizontal"         # Arrange legend horizontally
  )