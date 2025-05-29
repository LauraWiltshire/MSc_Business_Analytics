# Install and load necessary package
library(readxl)
library(tidyverse)
library(MASS)
library(pscl)
library(visdat)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(viridis)  # for a colorblind-friendly palette
library(patchwork) # for combining plots
library(lubridate)
library(ggalluvial)  # For alluvial plot
library(kableExtra)
library(ggridges)

# Get data
setwd("C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/Historic CQC Ratings")
cqc_data <- read_excel("CQC Ratings for Organisations with B&NES Service Users.xlsx")
glimpse(cqc_data)

# Assigning ranked numbers to CQC Ratings
cqc_data$Rating_Rank <- case_when(
  cqc_data$`Latest Rating` == "Outstanding" ~ 4,
  cqc_data$`Latest Rating` == "Good" ~ 3,
  cqc_data$`Latest Rating` == "Requires improvement" ~ 2,
  cqc_data$`Latest Rating` == "Inadequate" ~ 1,
  cqc_data$`Latest Rating` == "No overall rating" ~ 0,
  cqc_data$`Latest Rating` == "Insufficient evidence to rate" ~ 0,
  TRUE ~ NA_real_  # For any other unknown cases
)

# Create a new column with the year extracted from the Publication Date
overall_ratings <- cqc_data %>%
  filter(Rating_Rank > 0) %>%
  mutate(Year = year(`Publication Date`))

# Calculate the proportion of each Rating Rank for each year and domain
rating_proportions <- overall_ratings %>%
  group_by(Domain, Year, Rating_Rank) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  group_by(Domain, Year) %>%
  mutate(Proportion = Frequency / sum(Frequency))

# Calculate the total count of ratings per year and domain
year_counts <- overall_ratings %>%
  group_by(Domain, Year) %>%
  summarise(count = n(), .groups = 'drop')

# Join the rating counts to the rating_proportions table for labeling
rating_proportions <- rating_proportions %>%
  left_join(year_counts, by = c("Domain", "Year")) %>%
  mutate(Year_Label = paste0(Year, " (", count, " ratings)"))

## Function to create a plot for a specific domain
create_domain_plot <- function(domain_name, data, colour_scale) {
  ggplot(data %>% filter(Domain == domain_name),
         aes(x = as.factor(Rating_Rank), y = Proportion, color = Year_Label, group = Year)) +
    geom_smooth(se = FALSE, method = "loess", size = 1, alpha = 0.8) +
    scale_color_viridis_d(option = colour_scale) +
    labs(title = paste("Proportion of Ratings by Year -", domain_name),
         x = "",
         y = "Proportion") +  # Removed legend title from 'color'
    scale_x_discrete(labels = c("1" = "Inadequate", "2" = "Requires improvement", "3" = "Good", "4" = "Outstanding")) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank(),  # Remove legend title
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# Calculate proportions of each Rating Rank within each year
rating_heatmap_prop_data <- rating_proportions %>%
  group_by(Year) %>%
  mutate(Total_Frequency = sum(Frequency)) %>%
  ungroup() %>%
  mutate(Proportion = Frequency / Total_Frequency)

# Plot the heat map with proportions instead of counts
heatmap_plot_prop <- ggplot(rating_heatmap_prop_data, aes(x = factor(Year), y = factor(Rating_Rank), fill = Proportion)) +
  geom_tile(color = NA) +  # Remove lines between tiles
  geom_vline(xintercept = seq(0.5, length(unique(rating_heatmap_prop_data$Year)), 1), color = "white", size = 1) +  # Add white vertical lines to separate years
  scale_fill_viridis_c(option = "plasma", direction = -1, labels = scales::percent) +  # Viridis color scale with better contrast, proportion as percentage
  labs(title = "Heat Map of Proportion of Overall Ratings by Year",
       fill = "Proportion") +  # Legend title
  xlab("") +  # Remove x-axis title
  ylab("") +  # Remove y-axis title
  scale_y_discrete(labels = c("1" = "Inadequate", "2" = "Requires improvement", "3" = "Good", "4" = "Outstanding")) +  # Rename y-axis labels
  theme_minimal() +
  theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Set x-axis text size to 12
      axis.text.y = element_text(size = 12),  # Set y-axis text size to 12
      panel.grid = element_blank(),  # Remove grid lines
      panel.background = element_blank(),  # Remove panel background
      plot.title = element_text(size = 12),  # Set title text size to 12
      legend.text = element_text(size = 12),  # Set legend text size to 12
      legend.title = element_text(size = 12)  # Set legend title size to 12
  )

# Save the plot
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/heatmap_overall_ratings_proportion_A4.png"
ggsave(output_path, plot = heatmap_plot_prop, width = 8.27, height = 3.9, units = "in", dpi = 300)

# Create individual plots for each domain
plot_overall <- create_domain_plot("Overall", rating_proportions, "plasma")
plot_caring <- create_domain_plot("Caring", rating_proportions, "magma")
plot_effective <- create_domain_plot("Effective", rating_proportions, "cividis")
plot_responsive <- create_domain_plot("Responsive", rating_proportions, "viridis")
plot_safe <- create_domain_plot("Safe", rating_proportions, "inferno")
plot_well_led <- create_domain_plot("Well-led", rating_proportions, "turbo")

# Define A4 size in inches (A4 dimensions: 8.27 x 11.69 inches)
a4_width <- 8.27
a4_height <- 11.69

# Combine all the plots using patchwork
final_plot <- (plot_overall | plot_caring) /
  (plot_effective | plot_responsive) /
  (plot_safe | plot_well_led)
final_plot

# Set the working directory
setwd("C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations")

# Save the combined plot as a PNG image at A4 size
ggsave("CQC_Ratings_Domains_A4.png", plot = final_plot, width = a4_width, height = a4_height, units = "in", dpi = 300)

# Save as a PDF
ggsave("CQC_Ratings_Domains_A4.pdf", plot = final_plot, width = a4_width, height = a4_height, units = "in")

## Time series of Overall ratings
# Filter for "Overall" domain and exclude 0 ratings
overall_ratings_yearly <- cqc_data %>%
  filter(Domain == "Overall", Rating_Rank > 0) %>%
  mutate(Year = year(`Publication Date`))  # Extract year from the publication date

# Group by year and rating, and count occurrences
rating_heatmap_data <- overall_ratings_yearly %>%
  group_by(Year, Rating_Rank) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Plot the heat map with improved color differential and no grid lines
ggplot(rating_heatmap_data, aes(x = factor(Year), y = factor(Rating_Rank), fill = Frequency)) +
  geom_tile(color = NA) +  # Remove lines between tiles
  geom_vline(xintercept = seq(0.5, length(unique(rating_heatmap_data$Year)), 1), color = "white", size = 1) +  # Add white vertical lines to separate years
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Viridis color scale with better contrast
  labs(title = "Heat Map of Overall Ratings by Year",
       fill = "Count") +  # Legend title, without axis titles
  xlab("") +  # Remove x-axis title
  ylab("") +  # Remove y-axis title
  scale_y_discrete(labels = c("1" = "Inadequate", "2" = "Requires improvement", "3" = "Good", "4" = "Outstanding")) +  # Rename y-axis labels
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_blank()  # Remove panel background
  )

# Define the file path
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/heatmap_overall_ratings_1_3_A4.png"

# Adjust theme to ensure text size is 12 points
heatmap_plot <- ggplot(rating_heatmap_data, aes(x = factor(Year), y = factor(Rating_Rank), fill = Frequency)) +
  geom_tile(color = NA) +  # Remove lines between tiles
  geom_vline(xintercept = seq(0.5, length(unique(rating_heatmap_data$Year)), 1), color = "white", size = 1) +  # Add white vertical lines to separate years
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Viridis color scale with better contrast
  labs(title = "Heat Map of Overall Ratings by Year",
       fill = "Count") +  # Legend title, without axis titles
  xlab("") +  # Remove x-axis title
  ylab("") +  # Remove y-axis title
  scale_y_discrete(labels = c("1" = "Inadequate", "2" = "Requires improvement", "3" = "Good", "4" = "Outstanding")) +  # Rename y-axis labels
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Set x-axis text size to 12
    axis.text.y = element_text(size = 12),  # Set y-axis text size to 12
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_blank(),  # Remove panel background
    plot.title = element_text(size = 12),  # Set title text size to 12
    legend.text = element_text(size = 12),  # Set legend text size to 12
    legend.title = element_text(size = 12)  # Set legend title size to 12
  )

# Save the plot with 1/3rd of A4 height (3.9 inches) and text size of 12 points
ggsave(output_path, plot = heatmap_plot, width = 8.27, height = 3.9, units = "in", dpi = 300)



# Explore number of Care Packages
# Filter data for the "Overall" domain
overall_ratings_packages <- cqc_data %>%
  filter(Domain == "Overall", Rating_Rank > 0)  # Only keep rows with Overall domain and valid ratings

# Scatter plot comparing nCarePackages with Overall domain ratings
ggplot(overall_ratings_packages, aes(x = nCarePackages, y = factor(Rating_Rank))) +
  geom_jitter(aes(color = Rating_Rank), width = 0.1, height = 0.2, size = 3, alpha = 0.6) +  # Jittered points to avoid overlap
  scale_y_discrete(labels = c("1" = "Inadequate", "2" = "Requires improvement", "3" = "Good", "4" = "Outstanding")) +  # Label for y-axis
  labs(title = "Number of Care Packages vs. Overall Domain Ratings",
       x = "Number of Care Packages (nCarePackages)",
       y = "Overall Domain Rating",
       color = "Rating Rank") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

# Convert ratings to numeric for correlation purposes
overall_ratings_numeric <- overall_ratings_packages %>%
  mutate(Rating_Numeric = as.numeric(factor(Rating_Rank, 
                                            levels = c("1", "2", "3", "4"),
                                            labels = c(1, 2, 3, 4))))  # Convert ratings to numeric

# Scatter plot with regression line
ggplot(overall_ratings_numeric, aes(x = nCarePackages, y = Rating_Numeric)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add regression line with confidence interval
  labs(title = "Correlation Between Care Packages and Overall Domain Ratings",
       x = "Number of Care Packages (nCarePackages)",
       y = "Overall Domain Rating (Numeric)") +
  scale_y_continuous(breaks = c(1, 2, 3, 4),
                     labels = c("Inadequate", "Requires improvement", "Good", "Outstanding")) +  # Keep original rating labels
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

# Convert Rating_Rank to an ordered factor for ordinal logistic regression
overall_ratings_ordinal <- overall_ratings_packages %>%
  mutate(Rating_Ordinal = factor(Rating_Rank, 
                                 levels = c(1, 2, 3, 4), 
                                 labels = c("Inadequate", "Requires improvement", "Good", "Outstanding"),
                                 ordered = TRUE))

# Fit an ordinal logistic regression model using polr() from the MASS package
model <- polr(Rating_Ordinal ~ nCarePackages, data = overall_ratings_ordinal, Hess = TRUE)

# Display the model summary
summary(model)

# Generate predicted probabilities for each rating
predicted_probs <- predict(model, type = "probs")
head(predicted_probs)

# Create a new data frame with a range of nCarePackages values for predictions
new_data <- data.frame(nCarePackages = seq(min(overall_ratings_packages$nCarePackages), 
                                           max(overall_ratings_packages$nCarePackages), 
                                           length.out = 100))

# Predict probabilities for each level of Rating_Ordinal
predicted_probs <- predict(model, new_data, type = "probs")

# Combine the predicted probabilities with the new_data
predicted_data <- cbind(new_data, predicted_probs)

# Reshape the data for plotting
predicted_data_long <- predicted_data %>%
  pivot_longer(cols = -nCarePackages, names_to = "Rating", values_to = "Probability")

# Plot the predicted probabilities for each rating level
ggplot(predicted_data_long, aes(x = nCarePackages, y = Probability, color = Rating)) +
  geom_line(size = 1) +
  labs(title = "Predicted Probability of Ratings Based on Number of Care Packages",
       x = "Number of Care Packages (nCarePackages)",
       y = "Predicted Probability") +
  theme_minimal()

setwd("C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/Historic CQC Ratings")

vis_miss(cqc_data)

# Data processing and plotting
# Define the file path
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/average_nCarePackages_over_time.png"

# Count unique providers rated each year where nCarePackages >= 1
unique_providers_per_year <- cqc_data %>%
  mutate(Year = year(`Publication Date`)) %>%  # Extract year from publication date
  filter(nCarePackages >= 1) %>%  # Only include rows where nCarePackages >= 1
  group_by(Year) %>%
  summarise(Unique_Providers = n_distinct(`Provider ID`))  # Count unique providers per year

# View the result
print(unique_providers_per_year)

# Adjust theme to ensure text size is 12 points
final_plot <- cqc_data %>%
  mutate(Year = year(`Publication Date`)) %>%
  group_by(Year) %>%
  summarise(Average_nCarePackages = mean(nCarePackages, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Average_nCarePackages)) +
  geom_line(color = "orange", size = 1.2) +  # Thicker line with color
  #geom_point(color = "darkblue", size = 2) +  # Add points to each year for better visualization
  scale_x_continuous(breaks = seq(2017, 2024, by = 1)) +  # Use the range of years in your data
  labs(title = "Average Number of Care Packages per Provider Over Time",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Set title size to 12
    axis.text = element_text(size = 12),  # Set axis text size to 12
    axis.title = element_text(size = 12)  # Set axis titles to size 12
  )

# Save the plot with A4 width and one-third of A4 height (3.9 inches) with font size 12
ggsave(output_path, plot = final_plot, width = 8.27, height = 3.9, units = "in", dpi = 300)

# Calculate the count of unique publication dates per Location ID
location_inspection_counts <- cqc_data %>%
  group_by(`Location ID`) %>%
  summarise(Unique_Publication_Dates = n_distinct(`Publication Date`)) %>%
  ungroup()

# Calculate mean, median, and mode of the number of unique publication dates per Location ID
mean_dates <- mean(location_inspection_counts$Unique_Publication_Dates)
median_dates <- median(location_inspection_counts$Unique_Publication_Dates)

# Function to calculate mode (returns the most common value)
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_dates <- calculate_mode(location_inspection_counts$Unique_Publication_Dates)

# Print the statistics
cat("Mean number of unique publication dates per Location ID:", mean_dates, "\n")
cat("Median number of unique publication dates per Location ID:", median_dates, "\n")
cat("Mode number of unique publication dates per Location ID:", mode_dates, "\n")

# Create a histogram of the number of unique publication dates per Location ID
inspection_histogram <- ggplot(location_inspection_counts, aes(x = as.factor(Unique_Publication_Dates))) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Histogram of Unique Publication Dates per Location ID",
       x = "Number of Unique Publication Dates",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  scale_x_discrete(drop = FALSE)  # Ensures all integer values are shown on the x-axis

# Save the plot
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/histogram_of_inspections_per_location.png"
ggsave(output_path, plot = inspection_histogram, width = 8.27, height = 3.9, units = "in", dpi = 300)

# Calculate the average improvement and standard deviation of improvement for each initial rating level
average_next_rating <- cqc_improvement %>%
  group_by(Rating_Rank) %>%
  summarise(
    Avg_Next_Rating = mean(Next_Rating, na.rm = TRUE),
    Avg_Improvement = mean(Next_Rating - Rating_Rank, na.rm = TRUE),  # Calculate the average improvement
    SD_Improvement = sd(Next_Rating - Rating_Rank, na.rm = TRUE)  # Calculate the standard deviation of the improvement
  )

# Plot the initial rating with arrow to the average next rating
ratings_improvement <- ggplot(average_next_rating, aes(x = factor(Rating_Rank), y = Rating_Rank)) +
  # Initial rating as starting dot
  geom_point(size = 4, color = "blue") +
  
  # Arrow from initial rating to average next rating
  geom_segment(
    aes(xend = factor(Rating_Rank), yend = Avg_Next_Rating),
    arrow = arrow(length = unit(0.25, "inches"), type = "open", angle = 15),  # Arrow with "^" shape
    size = 1,
    color = ifelse(average_next_rating$Avg_Next_Rating > average_next_rating$Rating_Rank, "green", "red")
  ) +
  
  # Average next rating as end dot with improvement label
  geom_point(aes(y = Avg_Next_Rating), size = 4, color = "orange") +
  geom_text(aes(
    y = Avg_Next_Rating,
    label = paste0("Avg Change: ", round(Avg_Improvement, 2), "\n(SD: ", round(SD_Improvement, 2), ")")
  ), vjust = -1.5, color = "orange", size = 3.5) +  # Label with average improvement and SD
  
  labs(
    title = "Improvement in Overall Ratings: Initial to Average Next Rating",
    x = "Initial Overall Rating",
    y = "Rating"
  ) +
  scale_x_discrete(labels = c("1" = "Inadequate", "2" = "Requires Improvement", "3" = "Good", "4" = "Outstanding")) +
  scale_y_continuous(breaks = 1:4, labels = c("Inadequate", "Requires Improvement", "Good", "Outstanding")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Save the plot
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/Ratings_Improvement.png"
ggsave(output_path, plot = ratings_improvement, width = 8.27, height = 3.9, units = "in", dpi = 300)

# Prepare data to count transitions between initial and next ratings
cqc_transition_counts <- cqc_data %>%
  filter(Domain == "Overall", Rating_Rank > 0) %>%  # Filter for Overall domain and valid ratings
  arrange(`Location ID`, `Publication Date`) %>%
  group_by(`Location ID`) %>%
  filter(n() >= 2) %>%  # Only include locations with at least two ratings
  summarise(
    Initial_Rating = first(Rating_Rank),
    Next_Rating = nth(Rating_Rank, 2)  # Get the second rating as the "next" rating
  ) %>%
  ungroup() %>%
  count(Initial_Rating, Next_Rating) %>%  # Count the occurrences of each transition
  mutate(
    Transition_Type = case_when(
      Next_Rating > Initial_Rating ~ "Improvement",
      Next_Rating < Initial_Rating ~ "Worsening",
      Next_Rating == Initial_Rating ~ "No Change"
    )  # Determine improvement, worsening, or no change
  )

# Convert rating rank to descriptive labels and order them with Outstanding at the top
cqc_transition_counts <- cqc_transition_counts %>%
  mutate(
    Initial_Rating = factor(Initial_Rating, levels = c(4, 3, 2, 1), labels = c("Outstanding", "Good", "Requires Improvement", "Inadequate")),
    Next_Rating = factor(Next_Rating, levels = c(4, 3, 2, 1), labels = c("Outstanding", "Good", "Requires Improvement", "Inadequate")),
    Transition_Type = factor(Transition_Type, levels = c("Improvement", "No Change", "Worsening"))  # Order for stacking
  ) %>%
  # Create a unique identifier for each Initial to Next rating transition
  mutate(Alluvium_ID = paste(Initial_Rating, Next_Rating, sep = "_"))

# Prepare the data in lodes form to handle axis distinctions
cqc_transition_lodes <- to_lodes_form(
  data = cqc_transition_counts,
  key = "Axis",
  value = "Rating",
  axes = c("Initial_Rating", "Next_Rating")
)

# Create the alluvial plot with widened boxes and labels positioned beside the boxes
ggplot(cqc_transition_lodes, aes(x = Axis, stratum = Rating, alluvium = Alluvium_ID, y = n)) +
  geom_alluvium(aes(fill = Transition_Type), width = 1/10, alpha = 0.7) +  # Slightly widen the alluvium paths
  geom_stratum(width = 0.3, fill = "grey85", color = "black") +  # Widen the boxes slightly
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4, hjust = c(-0.2, 1.2)) +  # Position labels beside the boxes
  
  # Enhanced color scale to include "No Change"
  scale_fill_manual(values = c("Improvement" = "#4CAF50", "Worsening" = "#E57373", "No Change" = "grey")) +
  
  labs(
    title = "Transitions in Overall Domain Ratings for Each Location ID",
    x = "Rating Transition",
    y = "Number of Locations",
    fill = "Transition Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  scale_x_discrete(labels = c("Initial Rating", "Next Rating"))

# Save the plot to a file with A4 width constraints and half-height
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/Overall_Ratings_Transitions.png"
ggsave(output_path, width = 8.27, height = 5.85, units = "in", dpi = 300)
# Save the plot to a file with A4 width constraints and half-height
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/Overall_Ratings_Transitions.png"
ggsave(output_path, width = 8.27, height = 5.85, units = "in", dpi = 300)

# Prepare data to count transitions between initial and next ratings
cqc_transition_counts <- cqc_data %>%
  filter(Domain == "Overall", Rating_Rank > 0) %>%  # Filter for Overall domain and valid ratings
  arrange(`Location ID`, `Publication Date`) %>%
  group_by(`Location ID`) %>%
  filter(n() >= 2) %>%  # Only include locations with at least two ratings
  summarise(
    Initial_Rating = first(Rating_Rank),
    Next_Rating = nth(Rating_Rank, 2)  # Get the second rating as the "next" rating
  ) %>%
  ungroup() %>%
  count(Initial_Rating, Next_Rating) %>%  # Count the occurrences of each transition
  mutate(
    Transition_Type = case_when(
      Next_Rating > Initial_Rating ~ "Improvement",
      Next_Rating < Initial_Rating ~ "Worsening",
      Next_Rating == Initial_Rating ~ "No Change"
    )  # Determine improvement, worsening, or no change
  )

# Convert rating rank to descriptive labels and order them with Outstanding at the top
cqc_transition_counts <- cqc_transition_counts %>%
  mutate(
    Initial_Rating = factor(Initial_Rating, levels = c(4, 3, 2, 1), labels = c("Outstanding", "Good", "Requires Improvement", "Inadequate")),
    Next_Rating = factor(Next_Rating, levels = c(4, 3, 2, 1), labels = c("Outstanding", "Good", "Requires Improvement", "Inadequate")),
    Transition_Type = factor(Transition_Type, levels = c("Improvement", "No Change", "Worsening"))  # Order for stacking
  ) %>%
  # Create a unique identifier for each Initial to Next rating transition
  mutate(Alluvium_ID = paste(Initial_Rating, Next_Rating, sep = "_"))

# Prepare the data in lodes form to handle axis distinctions
cqc_transition_lodes <- to_lodes_form(
  data = cqc_transition_counts,
  key = "Axis",
  value = "Rating",
  axes = c("Initial_Rating", "Next_Rating")
)

# Create the alluvial plot with widened boxes and labels positioned beside the boxes
alluvial_rating_transition <- ggplot(cqc_transition_lodes, aes(x = Axis, stratum = Rating, alluvium = Alluvium_ID, y = n)) +
  geom_alluvium(aes(fill = Transition_Type), width = 2.5/10, alpha = 0.7) +  # Slightly widen the alluvium paths
  geom_stratum(width = 0.25, color = "black", fill = "white", alpha = 0.5) +  # Widen the boxes slightly
  
  # Add labels to the left for Initial Rating and to the right for Next Rating
  #geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4, hjust = 0, data = subset(cqc_transition_lodes, Axis == "Initial_Rating")) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4, hjust = 0, data = subset(cqc_transition_lodes, Axis == "Next_Rating")) +
  
  # Enhanced color scale to include "No Change"
  scale_fill_manual(values = c("Improvement" = "#4CAF50", "Worsening" = "#E57373", "No Change" = "grey")) +
  
  labs(
    title = "Transitions in Overall Domain Ratings for Each Location ID",
    x = "Rating Transition",
    y = "Number of Locations",
    fill = "Transition Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  scale_x_discrete(labels = c("Initial Rating", "Next Rating"))

# Save the plot to a file with A4 width constraints and half-height
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/alluvial_rating_transition.png"
ggsave(output_path, width = 8.27, height = 5.85, units = "in", dpi = 300)

# Calculate the counts and percentages for each transition type by initial rating
transition_summary <- cqc_transition_counts %>%
  group_by(Initial_Rating, Transition_Type) %>%
  dplyr::summarise(Count = sum(n), .groups = "drop") %>%  # Calculate total counts for each group
  group_by(Initial_Rating) %>%
  dplyr::mutate(
    Total = sum(Count),  # Calculate the total for each initial rating
    Percentage = (Count / Total) * 100  # Calculate percentage
  ) %>%
  ungroup() %>%  # Remove grouping before selection
  dplyr::select(Initial_Rating, Transition_Type, Count, Percentage)  # Select relevant columns for display

# Display the results
transition_summary
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/Transition_Summary.html"
kable(transition_summary, format = "html", digits = 2, col.names = c("Starting Rating", "Transition Type", "Count", "Percentage")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 12) %>%
  save_kable(output_path)


# Prepare the data: count the number of each rating per year, and convert counts to proportions
cqc_yearly_summary <- cqc_data %>%
  filter(Domain == "Overall") %>%  # Filter for Overall ratings only
  mutate(
    Year = year(`Publication Date`),  # Extract year
    Rating_Label = factor(Rating_Rank, levels = c(0, 1, 2, 3, 4), 
                          labels = c("NA", "Inadequate", "Requires Improvement", "Good", "Outstanding"))
  ) %>%
  #filter(!is.na(Rating_Rank)) %>%  # Remove rows where Rating_Rank is NA if needed
  group_by(Year, Rating_Label) %>% 
  summarise(Count = n()) %>%  # Count occurrences of each rating per year
  mutate(Proportion = Count / sum(Count)) %>%  # Convert counts to proportions
  ungroup()

# Create the ridgeline plot for distribution by year with proportions
ridgeline_yearly_cqc <- ggplot(cqc_yearly_summary, aes(x = Rating_Label, y = as.factor(Year), height = Proportion, group = Year, fill = as.factor(Year))) +
  geom_density_ridges(stat = "identity", scale = 4, alpha = 0.7) +  # Increase scale for wider ridgelines
  scale_fill_viridis_d(option = "C", guide = "none") +  # Use discrete Viridis scale for consistent fill per year
  labs(
    title = "Ridgeline Plot of CQC Overall Ratings by Year (Proportions)",
    x = "Rating",
    y = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels diagonally
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Save the plot to a file with A4 width constraints and half-height
output_path <- "C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/EDA Visualisations/ridgeline_cqc_yearly_proportions.png"
ggsave(output_path, plot = ridgeline_yearly_cqc, width = 8.27, height = 6.85, units = "in", dpi = 300)
