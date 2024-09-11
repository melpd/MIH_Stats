library(readxl)
library(dplyr)

# List all USHL file paths
ushl_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/USHL_data", pattern = "*.xlsx", full.names = TRUE)

load_ushl_data <- function(file_path) {
  # Extract the file name from the full file path
  file_name <- basename(file_path)  # Extracts "Skaters - United States Hockey League, 2018-2019.xlsx"
  
  # Extract the season using a regular expression to match the year format "YYYY-YYYY"
  season <- sub(".*(\\d{4}-\\d{4}).*", "\\1", file_name)
  
  # Read the Excel file and add the season as a column
  data <- read_excel(file_path) %>%
    mutate(season = season) %>%
    
    # Replace dashes "-" with NA and coerce 'Puck touches' to numeric
    mutate(`Puck touches` = as.numeric(gsub("-", NA, `Puck touches`)))
  
  return(data)
}

# Load and combine all USHL files
ushl_data <- bind_rows(lapply(ushl_files, load_ushl_data))
View(ushl_data)

library(dplyr)

aggregated_data <- ushl_data %>%
  group_by(Player) %>%                          # Group by player name
  summarize(across(where(is.numeric),            # Apply to all numeric columns
                   sum, na.rm = TRUE))           # Sum the numeric columns, ignoring NAs

# View the aggregated data
View(aggregated_data)

# Assuming your data has a column 'Player' for player names
duplicate_names <- aggregated_data %>%
  group_by(Player) %>%        # Group by player name
  summarize(count = n()) %>%   # Count occurrences of each name
  filter(count > 1)            # Keep only names with duplicates

# View the duplicates
View(duplicate_names)

