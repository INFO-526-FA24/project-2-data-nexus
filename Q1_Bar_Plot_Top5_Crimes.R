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

# Step 5: Create a bar plot for the top 5 crimes with scaled axes
ggplot(top_crimes, aes(x = reorder(Crm.Cd.Desc, n), y = n)) +
  geom_bar(stat = "identity", fill = "#3A5B7D", color = "black", width = 0.4) + # Width set to 0.4
  geom_text(aes(label = n), 
            hjust = -0.3, 
            size = 5, 
            color = "white",
            fontface = "bold") +
  coord_flip() + # Flip coordinates to make horizontal bars
  #scale_x_continuous(expand = expansion(mult = c(0, 0.1))) + # Adjust numerical x-axis (after flip)
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) + # Adjust categorical y-axis (crime types)
  labs(
    title = "Top 5 Crimes in Los Angeles",
    x = "Number of Incidents",
    y = "Crime Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",        # Hide legend for this plot
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)), # Adjust spacing between text and bars
    plot.title.position = "plot"
  )

