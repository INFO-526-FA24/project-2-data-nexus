# Load necessary libraries
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(stringr) # For string manipulation
library(readr) # For reading CSV files
library(plotly) # For interactive plots
library(scales) # For number formatting

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
  top_n(5, n) %>%
  mutate(Percentage = n / sum(n) * 100) # Calculate percentage of each crime

# Step 5: Create an interactive bar plot for the top 5 crimes
# Modify theme to increase the font size of x-axis labels
plot <- ggplot(top_crimes, aes(x = reorder(Crm.Cd.Desc, n), y = n, 
                               text = paste("Crime Type:", Crm.Cd.Desc, 
                                            "<br>Incidents:", n, 
                                            "<br>Percentage:", sprintf("%.1f%%", Percentage)))) +
  geom_bar(stat = "identity", fill = "#3A5B7D", color = "black", width = 0.4) +
  geom_text(aes(label = paste0(" ----------- (", sprintf("%.1f%%", Percentage), ")")), 
            hjust = 10, 
            size = 4, 
            color = "black",
            fontface = "bold") +
  coord_flip() + # Flip coordinates to make horizontal bars
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), 
                     expand = expansion(mult = c(0.05, 0.1))) + # Format numbers and adjust padding
  labs(
    title = "Top 5 Crimes in Los Angeles",
    x = "Crime Type",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",        # Hide legend for this plot
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    #axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)), # Adjust spacing between text and bars
    axis.text.y = element_text(size = 19), # Increase font size of y-axis labels
    axis.text.x = element_text(size = 14), # Increase font size of x-axis labels
    plot.title.position = "plot"
  )

# Convert ggplot to an interactive plotly plot
interactive_plot <- ggplotly(plot, tooltip = "text")

# Print the interactive plot
interactive_plot
