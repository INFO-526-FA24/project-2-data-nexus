# Load necessary libraries
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(stringr) # For string manipulation
library(readr) # For reading CSV files

# Set options for tigris
options(tigris_class = "sf", tigris_use_cache = TRUE)

# Step 1: Read and combine all CSV files into one data frame
csv_files <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)
csv_data <- do.call(rbind, lapply(csv_files, read.csv))

# Step 2: Replace specific value in "Crm Cd Desc"
csv_data <- csv_data %>%
  mutate(Crm.Cd.Desc = str_replace(
    Crm.Cd.Desc, 
    "VANDALISM - FELONY \\(\\$400 & OVER, ALL CHURCH VANDALISMS\\)", 
    "VANDALISM - FELONY"
  ))

# Step 3: Filter out rows where LON and LAT are 0
filtered_data <- csv_data %>%
  filter(LON != 0, LAT != 0)

# Step 4: Identify top 5 crimes in "Crm Cd Desc"
top_crimes <- filtered_data %>%
  count(Crm.Cd.Desc, sort = TRUE) %>%
  top_n(5, n) 

# Step 5: Create a bar plot for the top 5 crimes
ggplot(top_crimes, aes(x = reorder(Crm.Cd.Desc, n), y = n, fill = Crm.Cd.Desc)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flip coordinates to make horizontal bars
  scale_fill_viridis_d(name = "Crime Type") +
  labs(
    title = "Top 5 Crimes in Los Angeles",
    x = "Crime Type",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",        # Hide legend for this plot
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )
