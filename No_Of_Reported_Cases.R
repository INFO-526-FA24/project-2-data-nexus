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

# Define a custom color palette
color_palette <- c(brewer.pal(5, "Blues")) # Assign blue for 2020 (adjust as necessary)

# Bar Graph: Total Crimes Reported in 2020
ggplot(crime_summary, aes(x = factor(Year), y = Total_Crimes, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_number(scale = 0.001, suffix = "K")) + # Adjust y-axis labels
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Total Crimes Reported in 2020 - 2024",
    x = "Year",
    y = "Total Number of Crimes (in thousands)"
  ) +
  theme_minimal()

# Pie Chart: Total Crimes Reported in 2020 with Percentages and Years
ggplot(crime_summary, aes(x = "", y = Total_Crimes, fill = factor(Year))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0("(",Year,")", "\n", round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Total Crimes Reported in 2020 - 2024"
  ) +
  labs(
    fill = "Years"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
