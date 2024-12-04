#install.packages("treemapify")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(treemapify)

# Read the dataset
csv_files <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)
crime_data <- do.call(rbind, lapply(csv_files, read.csv))

# Summarize crimes by area and sort in descending order
area_summary <- crime_data %>%
  group_by(AREA.NAME) %>%
  summarize(Crime_Count = n(), .groups = "drop") %>%
  mutate(Percentage = Crime_Count / sum(Crime_Count) * 100) %>%
  arrange(desc(Crime_Count))

# Determine text color based on Crime_Count (higher values: white, lower values: black)
#area_summary <- area_summary %>%
#  mutate(Text_Color = ifelse(Crime_Count > median(Crime_Count), "white", "black"))

# Create a treemap with sorted data and conditional text color
ggplot(area_summary, aes(
  area = Crime_Count,
  fill = Crime_Count,
  label = paste0(AREA.NAME, "\n", Crime_Count, "\n", "(", round(Percentage, 1), "%)")
)) +
  geom_treemap(color = "white") +  # Set line color to white
  geom_treemap_text(
    aes(colour = "darkslateblue"),
    fontface = "bold",
    place = "centre",
    grow = FALSE,  # Disable font resizing
    size = 8      # Set a consistent font size
  ) +
  scale_colour_identity() +  # Use text color as specified in the data
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    title = "Crimes by Area",
    fill = "Crime Count"
  ) +
  theme_minimal()
