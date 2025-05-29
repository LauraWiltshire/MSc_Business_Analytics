
# Set Commissioner data
setwd("C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/Commissioner Quality and Risk Dashboards")
commissioner_data <- read_csv('tidy_commissioner_data.csv')

# Set CQC data
setwd("C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation/Historic CQC Ratings")
cqc_data <- read_csv('combined_ratings_data.csv')

# Check all datasets
glimpse(commissioner_data)
glimpse(cqc_data)

# Load the dplyr library
library(dplyr)

# Perform an inner join on the 'CQC ID' columns
# Load the dplyr package
library(dplyr)

joined_data <- commissioner_data %>%
  inner_join(cqc_data, by = c("CQC Registration Number" = "Location ID"))

# View the resulting dataset
head(joined_data)

# View the resulting data
view(joined_data)
setwd("C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation")

# Save the tidy data to a new file, write as CSV
write.csv(joined_data, "Dataset_for_MSc.csv", row.names = FALSE)

