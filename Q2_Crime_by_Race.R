# Load necessary library
library(dplyr)

# Read the dataset
csv_files <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)
crime_data <- do.call(rbind, lapply(csv_files, read.csv))

# Define a mapping for race abbreviations
race_mapping <- c(
  "B" = "Black",
  "W" = "White",
  "H" = "Hispanic",
  "A" = "Asian",
  "O" = "Other",
  "X" = "Unknown"
)

# Filter for the specified race categories
valid_races <- names(race_mapping)

crime_data <- crime_data %>%
  filter(Vict.Descent %in% valid_races) %>%
  mutate(Vict_Descent = recode(Vict.Descent, !!!race_mapping))

# Summarize crime counts by race
crime_summary <- crime_data %>%
  group_by(Vict_Descent) %>%
  summarize(Crime_Count = n(), .groups = 'drop') %>%
  arrange(desc(Crime_Count))

# Create a horizontal bar graph
ggplot(crime_summary, aes(x = Crime_Count, y = reorder(Vict_Descent, Crime_Count))) +
  geom_bar(stat = "identity", fill = "#3A5B7D") +
  scale_x_continuous(labels = scales::label_number(scale = 0.001, suffix = "K")) +
  labs(
    title = "Crime Counts by Race",
    x = "Number of Crimes (in thousands)",
    y = "Race"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15),  # Increase font size for x-axis labels
    axis.title.x = element_text(size = 14), # Increase font size for x-axis title
    axis.title.y = element_text(size = 14), # Increase font size for y-axis title
    plot.title = element_text(size = 16, hjust = 0.5), # Center-align and enlarge title
    axis.text.y = element_text(size = 19)
  )
