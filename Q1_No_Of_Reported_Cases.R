# Load necessary libraries
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(scales)

# Load the data
# Read the dataset
csv_files <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)
crime_data <- do.call(rbind, lapply(csv_files, read.csv))

# Summarize the total number of crimes for the year 2020
crime_summary <- crime_data %>%
  group_by(Year) %>%
  summarize(Total_Crimes = n())

# Calculate percentages for the pie chart
crime_summary <- crime_summary %>%
  mutate(Percentage = Total_Crimes / sum(Total_Crimes) * 100)

# Define a single-color palette (blue)
color_palette <- c("#6096CF")  # Replace with your preferred shade of blue

# Bar Graph: Total Crimes Reported in 2020
ggplot(crime_summary, aes(x = factor(Year), y = Total_Crimes, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_number(scale = 0.001, suffix = "K")) + # Adjust y-axis labels
  scale_fill_manual(values = rep(color_palette, length(unique(crime_summary$Year)))) +
  labs(
    title = "Total Crimes Reported in 2020 - 2024",
    x = "Year",
    y = "Total Number of Crimes (in thousands)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend

# Define a custom color palette with lighter and darker shades for depth
color_palette <- c("#000080", "#3A5B7D")  # Blue and dark blue for shading effect

# Pie Chart: Total Crimes Reported in 2020 with Depth
ggplot(crime_summary, aes(x = "", y = Total_Crimes, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 1.2, color = "white", show.legend = FALSE, fill = color_palette[1]) +  # Outer layer
  geom_bar(stat = "identity", width = 1.17, color = "white", aes(fill = factor(Year))) +  # Inner layer for "depth"
  coord_polar("y") +
  geom_text(aes(label = paste0("(", Year, ")", "\n", round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  scale_fill_manual(values = rep(color_palette[2], length(unique(crime_summary$Year)))) +
  labs(
    title = "Total Crimes Reported in 2020 - 2024"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"  # Hide legend
  )
