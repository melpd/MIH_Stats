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

load_data <- function(file_path) {
  # Extract the file name from the full file path
  file_name <- basename(file_path)  # Extracts "Skaters - United States Hockey League, 2018-2019.xlsx"
  # Extract the season using a regular expression to match the year format "YYYY-YYYY"
  season <- sub(".*(\\d{4}-\\d{4}).*", "\\1", file_name)
  # Read the Excel file and add the season as a column
  data <- read_excel(file_path) %>%
    mutate(season = season) %>%
    mutate(across(-c(`Player`, `season`), ~ gsub("-", "0", .)))
  return(data)
}

# Load and combine all USHL files
ushl_data <- bind_rows(lapply(ushl_files, load_data))
View(ushl_data)
ushl_data <- ushl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]

convert_percent <- function(data, cols) {
  data <- data %>%
    mutate(across(all_of(cols), ~ as.numeric(gsub("%", "", .)) / 100))
  return(data)
}

percentage_columns <- c("CORSI for, %", "% shots on goal")

ushl_data <- convert_percent(ushl_data, percentage_columns)

cleaned_ushl_data <- ushl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

duplicate_ushl <- aggregated_ushl %>%
  group_by(Player) %>%        # Group by player name
  summarize(count = n()) %>%   # Count occurrences of each name
  filter(count > 1)            # Keep only names with duplicates

# Sum the numeric columns, keeping 'Player' and 'Season' unchanged
aggregated_ushl <- cleaned_ushl_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

# View the aggregated data
View(aggregated_ushl)

#Loading J20
j20_data <- bind_rows(lapply(j20_files, load_data))
j20_data <- j20_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
j20_data <- convert_percent(j20_data, percentage_columns)
cleaned_j20_data <- j20_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
aggregated_j20 <- cleaned_j20_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

duplicate_j20 <- cleaned_j20_data %>%
  group_by(Player) %>%        # Group by player name
  summarize(count = n()) %>%   # Count occurrences of each name
  filter(count > 1)

#Loading BCHL
bchl_data <- bind_rows(lapply(bchl_files, load_data))
bchl_data <- bchl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
bchl_data <- convert_percent(bchl_data, percentage_columns)
cleaned_bchl_data <- bchl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

aggregated_bchl <- cleaned_bchl_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

#Load Nahl
nahl_data <- bind_rows(lapply(nahl_files, load_data))
nahl_data <- nahl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
nahl_data <- convert_percent(nahl_data, percentage_columns)
cleaned_nahl_data <- nahl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

aggregated_nahl <- cleaned_nahl_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

# Load OJHL
ojhl_data <- bind_rows(lapply(ojhl_files, load_data))
ojhl_data <- ojhl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
ojhl_data <- convert_percent(ojhl_data, percentage_columns)
cleaned_ojhl_data <- ojhl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

aggregated_ojhl <- cleaned_ojhl_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

# Load NOJHL
nojhl_data <- bind_rows(lapply(nojhl_files, load_data))
nojhl_data <- nojhl_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
nojhl_data <- convert_percent(nojhl_data, percentage_columns)
cleaned_nojhl_data <- nojhl_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

aggregated_nojhl <- cleaned_nojhl_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

# Load U20_SM
u20_sm_data <- bind_rows(lapply(u20_sm_files, load_data))
u20_sm_data <- u20_sm_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
u20_sm_data <- convert_percent(u20_sm_data, percentage_columns)
cleaned_u20_sm_data <- u20_sm_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

aggregated_u20_sm <- cleaned_u20_sm_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

#Load NCAA files
ncaa_data <- bind_rows(lapply(ncaa_files, load_data))
ncaa_data <- ncaa_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
ncaa_data <- convert_percent(ncaa_data, percentage_columns)
cleaned_ncaa_data <- ncaa_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
aggregated_ncaa <- cleaned_ncaa_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

hockey_east_data <- bind_rows(lapply(hockey_east_files, load_data))
hockey_east_data <- hockey_east_data[c('Player','Points', 'Goals', 'Assists', 'season', 'Games played', 'CORSI', 'CORSI for, %', 'Time on ice', '% shots on goal')]
hockey_east_data <- convert_percent(hockey_east_data, percentage_columns)
cleaned_hockey_east_data <- hockey_east_data %>%
  mutate(across(c('Goals', 'Points', 'Assists', 'Games played', 'CORSI', 'CORSI for, %', '% shots on goal'), ~ as.numeric(gsub("-", NA, .)))) %>%  # Convert non-numeric to NA, then numeric
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
aggregated_hockey_east <- cleaned_hockey_east_data %>%
  group_by(Player) %>%                  # Group by both player name and season
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
