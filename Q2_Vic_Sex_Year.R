# Load necessary library
library(ggplot2)
library(dplyr)

# Read the dataset
csv_files <- list.files(path = "data/", pattern = "*.csv", full.names = TRUE)
crime_data <- do.call(rbind, lapply(csv_files, read.csv))

# Filter data to exclude rows where Vict.Sex is blank or "-"
filtered_data <- subset(crime_data, Vict.Sex != "" & Vict.Sex != "-" & Vict.Sex != "H")

# Group by Year and Vict.Sex
gender_year_summary <- filtered_data %>%
  group_by(Year, Vict.Sex) %>%
  summarise(Count = n(), .groups = "drop")

# Rename columns for clarity
colnames(gender_year_summary) <- c("Year", "Gender", "Count")

# Replace Vict.Sex values with descriptive labels
gender_year_summary$Gender <- recode(gender_year_summary$Gender,
                                     "M" = "Male",
                                     "F" = "Female",
                                     "X" = "Others")

# Calculate total victims per year
total_per_year <- gender_year_summary %>%
  group_by(Year) %>%
  summarise(Total = sum(Count))

# Merge to calculate percentage
gender_year_summary <- merge(gender_year_summary, total_per_year, by = "Year")
gender_year_summary$Percentage <- (gender_year_summary$Count / gender_year_summary$Total) * 100

# Plot the bar graph with specific colors
ggplot(gender_year_summary, aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ Year, nrow = 1) +
  labs(
    title = "Victim Age By Year",
    x = NULL,
    y = "% Total of Crime Victims",
    fill = "Gender"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) + # Format Y-axis as percentage
  scale_fill_manual(values = c("Male" = "#619DFF", "Female" = "#F9766E", "Others" = "#01BA38")) + # Set colors
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 10, angle = 90),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    #panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "grey", fill = NA, size = 1), # Add grey border for each panel
    axis.line = element_line(color = "grey", size = 1) # Add lines to group years
  )
