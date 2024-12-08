# Load necessary libraries
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(stringr) # For string manipulation
library(readr) # For reading CSV files

# Step 1: Read and combine all CSV files into one data frame
csv_files <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)
crime_data <- do.call(rbind, lapply(csv_files, read.csv))

cleaned_data <- subset(crime_data, !is.na(Vict.Age) & Vict.Age != 0)

# Define bin size (e.g., 5 years per bin)
bin_size <- 5

# Create a histogram for victim age distribution
ggplot(cleaned_data, aes(x = Vict.Age)) +
  geom_histogram(binwidth = bin_size, fill = "#3A5B7D", alpha = 0.9, color = "black") +
  scale_x_continuous(
    breaks = seq(0, max(cleaned_data$Vict.Age, na.rm = TRUE), by = 20),
    name = "Age of Victims"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x / 1000, "K"),
    name = "Number of Victims (in thousands)"
  ) +
  labs(
    title = "Distribution of Crime Victims by Age"
  ) +
  theme_minimal()
