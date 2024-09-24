
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# List all Junior file paths
ushl_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/USHL_data", pattern = "*.xlsx", full.names = TRUE)
j20_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/J20_data", pattern = "*.xlsx", full.names = TRUE)
bchl_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/BCHL_data", pattern = "*.xlsx", full.names = TRUE)
nahl_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/NAHL_data", pattern = "*.xlsx", full.names = TRUE)
ojhl_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/OJHL_data", pattern = "*.xlsx", full.names = TRUE)
nojhl_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/NOJHL_data", pattern = "*.xlsx", full.names = TRUE)
u20_sm_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/U20_SM_data", pattern = "*.xlsx", full.names = TRUE)
ncaa_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/NCAA_data", pattern = "*.xlsx", full.names = TRUE)
hockey_east_files <- list.files("C:/Users/mpdes/OneDrive/Desktop/MIH/Hockey_East_data", pattern = "*.xlsx", full.names = TRUE)

# Function to extract minutes from MM:SS format
extract_minutes <- function(time_str) {
  #if (is.na(time_str) || time_str == "-") {
  #  return(NA)  # Return NA for missing or invalid time entries
  #}
  parts <- strsplit(time_str, ":")[[1]]
  minutes <- as.numeric(parts[1])
  return(minutes)
}

load_data <- function(file_path) {
  # Extract the file name from the full file path
  file_name <- basename(file_path)
  # Extract the season using a regular expression to match the year format "YYYY-YYYY"
  season <- sub(".*(\\d{4}-\\d{4}).*", "\\1", file_name)
  # Read the Excel file and add the season as a column
  data <- read_excel(file_path) %>%
    mutate(season = season) %>%
    #mutate(`Time on ice` = sapply(`Time on ice`, extract_minutes)) %>%
    mutate(across(-c(`Player`, `season`, `Date of birth`, `CORSI`), ~ gsub("-", "0", .))) %>%
    mutate(`Time on ice` = sapply(`Time on ice`, extract_minutes)) %>%
  return(data)
}

# Load and combine all USHL files
ushl_data <- bind_rows(lapply(ushl_files, load_data))
View(ushl_data)
ushl_data <- ushl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]

convert_percent <- function(data, cols) {
  data <- data %>%
    mutate(across(all_of(cols), ~ as.numeric(gsub("%", "", .))/100))
  return(data)
}

percentage_columns <- c("CORSI for, %", "% shots on goal")

ushl_data <- convert_percent(ushl_data, percentage_columns)

cleaned_ushl_data <- ushl_data %>%
  # Replace "-" with 0 in relevant numeric columns and keep negative values like "-5" intact
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%  # Replace standalone "-" with "0"
  # Convert to numeric, preserving negative values
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  # Replace any remaining NA values with 0
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

#cleaned_ushl_data <- ushl_data %>%
#  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI'), ~ as.numeric(gsub("-", "0", .)))) %>%
#  mutate(`Time on ice` = as.numeric(gsub("-", "0", `Time on ice`))) %>%  # Handle Time on ice
#  mutate(across(where(is.numeric), ~ replace_na(., 0)))  # Replace remaining NA with 0
duplicate_ushl <- aggregated_ushl %>%
  group_by(Player) %>%        # Group by player name
  summarize(count = n()) %>%   # Count occurrences of each name
  filter(count > 1)            # Keep only names with duplicates

# Define non-percentage numeric columns
ushl_numeric_columns <- setdiff(names(cleaned_ushl_data), c("Player", "season", percentage_columns))

# Aggregate the data, summing non-percentage columns and averaging percentage columns
aggregated_ushl <- cleaned_ushl_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(ushl_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )
    #aggregated_ushl <- cleaned_ushl_data %>%
#  group_by(Player) %>%
#  summarize(across(where(is.numeric), sum, na.rm = TRUE))

# View the aggregated data
View(aggregated_ushl)

#Loading J20
j20_data <- bind_rows(lapply(j20_files, load_data))
j20_data <- j20_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
j20_data <- convert_percent(j20_data, percentage_columns)
cleaned_j20_data <- j20_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
j20_numeric_columns <- setdiff(names(cleaned_j20_data), c("Player", "season", percentage_columns))
aggregated_j20 <- cleaned_j20_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(j20_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )

duplicate_j20 <- cleaned_j20_data %>%
  group_by(Player) %>%        # Group by player name
  summarize(count = n()) %>%   # Count occurrences of each name
  filter(count > 1)

#Loading BCHL
bchl_data <- bind_rows(lapply(bchl_files, load_data))
bchl_data <- bchl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
bchl_data <- convert_percent(bchl_data, percentage_columns)
cleaned_bchl_data <- bchl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
bchl_numeric_columns <- setdiff(names(cleaned_bchl_data), c("Player", "season", percentage_columns))
aggregated_bchl <- cleaned_bchl_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(bchl_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )

#Load Nahl
nahl_data <- bind_rows(lapply(nahl_files, load_data))
nahl_data <- nahl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
nahl_data <- convert_percent(nahl_data, percentage_columns)
cleaned_nahl_data <- nahl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
nahl_numeric_columns <- setdiff(names(cleaned_nahl_data), c("Player", "season", percentage_columns))
aggregated_nahl <- cleaned_nahl_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(nahl_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )

# Load OJHL
ojhl_data <- bind_rows(lapply(ojhl_files, load_data))
ojhl_data <- ojhl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
ojhl_data <- convert_percent(ojhl_data, percentage_columns)
cleaned_ojhl_data <- ojhl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
ojhl_numeric_columns <- setdiff(names(cleaned_ojhl_data), c("Player", "season", percentage_columns))
aggregated_ojhl <- cleaned_ojhl_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(ojhl_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )

# Load NOJHL
nojhl_data <- bind_rows(lapply(nojhl_files, load_data))
nojhl_data <- nojhl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
nojhl_data <- convert_percent(nojhl_data, percentage_columns)
cleaned_nojhl_data <- nojhl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
nojhl_numeric_columns <- setdiff(names(cleaned_nojhl_data), c("Player", "season", percentage_columns))
aggregated_nojhl <- cleaned_nojhl__data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(nojhl_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )

# Load U20_SM
u20_sm_data <- bind_rows(lapply(u20_sm_files, load_data))
u20_sm_data <- u20_sm_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
u20_sm_data <- convert_percent(u20_sm_data, percentage_columns)
cleaned_u20_sm_data <- u20_sm_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
u20_sm_numeric_columns <- setdiff(names(cleaned_u20_sm_data), c("Player", "season", percentage_columns))
aggregated_u20_sm <- cleaned_u20_sm_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(u20_sm_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )


#Load NCAA files
ncaa_data <- bind_rows(lapply(ncaa_files, load_data))
ncaa_data <- ncaa_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
ncaa_data <- convert_percent(ncaa_data, percentage_columns)
cleaned_ncaa_data <- ncaa_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
ncaa_numeric_columns <- setdiff(names(cleaned_ncaa_data), c("Player", "season", percentage_columns))
aggregated_ncaa <- cleaned_ncaa_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(ncaa_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )


hockey_east_data <- bind_rows(lapply(hockey_east_files, load_data))
hockey_east_data <- hockey_east_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
hockey_east_data <- convert_percent(hockey_east_data, percentage_columns)
cleaned_he_data <- hockey_east_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ gsub("^-$", "0", .))) %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'Time on ice'), 
                ~ as.numeric(.))) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
he_numeric_columns <- setdiff(names(cleaned_he_data), c("Player", "season", percentage_columns))
aggregated_he <- cleaned_he_data %>%
  group_by(Player) %>%
  summarize(
    across(all_of(he_numeric_columns), sum, na.rm = TRUE),  # Sum non-percentage columns
    across(all_of(percentage_columns), mean, na.rm = TRUE)  # Average percentage columns
  )
