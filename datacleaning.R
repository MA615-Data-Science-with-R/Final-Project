# Load necessary library
library(dplyr)


# Read the dataset 
data <- read.csv("/Users/nikhitamantravadi/Desktop/FinalR/South_Asian_dataset.csv")

# Cleaning the data to retain only the necessary columns: GDP and Population
# Combining rows for each country by averaging GDP and Population over all years
cleaned_data <- data %>%
  group_by(Country) %>%
  summarise(
    Avg_GDP = mean(GDP..current.US.., na.rm = TRUE),
    Avg_Population = mean(Population..total, na.rm = TRUE)
  ) %>%
  arrange(Country)  # Sort alphabetically for better readability


# Saving the cleaned dataset to a new CSV file
write.csv(cleaned_data, "/Users/nikhitamantravadi/Desktop/FinalR/Cleaned_South_Asian_Data.csv", row.names = FALSE)


# CLeaning Tourism Data

# Reading the dataset, skipping initial non-data rows
tourism_data <- read.csv("/Users/nikhitamantravadi/Desktop/FinalR/sri-lanka-tourism-statistics.csv", skip = 8)

# Renaming columns for clarity
colnames(tourism_data) <- c("Date", "GDP_Billions", "Per_Capita", "Annual_Change")

# Removing the 'Annual_Change' column as it is empty
tourism_data <- tourism_data %>%
  select(-Annual_Change)

# Converting `Date` to a proper date format and extract the year
tourism_data <- tourism_data %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Year = format(Date, "%Y")  # Extract year for easier analysis
  )

# Handling missing values in the remaining columns
# Replace missing GDP_Billions and Per_Capita with their column means
tourism_data <- tourism_data %>%
  mutate(
    GDP_Billions = ifelse(is.na(GDP_Billions), mean(GDP_Billions, na.rm = TRUE), GDP_Billions),
    Per_Capita = ifelse(is.na(Per_Capita), mean(Per_Capita, na.rm = TRUE), Per_Capita)
  )

# Save the cleaned dataset to a new file
write.csv(tourism_data, "/Users/nikhitamantravadi/Desktop/FinalR/Cleaned_Tourism_Data.csv", row.names = FALSE)



##3 Suicides

library(tidyverse)

# Define file path
file_path <- "/Users/nikhitamantravadi/Downloads/archive/mode_of_suicides.csv"  # Replace with your file path

# Load the data
suicides_data <- read_csv(file_path)

# Step 1: Select relevant columns (Totals for males and females)
suicides_totals <- suicides_data %>%
  select(`Age group`, contains("Totals_M"), contains("Totals_F"))

# Step 2: Combine male and female totals for each year
for (year in 2014:2017) {  # Adjust range of years as needed
  male_column <- paste0(year, "_Totals_M")
  female_column <- paste0(year, "_Totals_F")
  
  if (male_column %in% colnames(suicides_totals) & female_column %in% colnames(suicides_totals)) {
    combined_column <- paste0(year, "_Total")
    suicides_totals <- suicides_totals %>%
      mutate(!!combined_column := get(male_column) + get(female_column))
  }
}

# Step 3: Keep only Age group and combined total columns
final_columns <- c("Age group", grep("_Total$", colnames(suicides_totals), value = TRUE))
suicides_cleaned <- suicides_totals %>%
  select(all_of(final_columns))

# Step 4: Aggregate total suicides for each year
suicides_summary <- suicides_cleaned %>%
  select(-`Age group`) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Total Suicides") %>%
  mutate(Year = as.numeric(str_extract(Year, "\\d{4}")))



# Save cleaned data to a file
write_csv(suicides_cleaned, "suicides_cleaned.csv")



# Opportunities and Threats


# Define file paths
opportunities_file <- "/Users/nikhitamantravadi/Downloads/archive-3/2023 Yala.csv"  # Replace with the correct path
threats_file <- "/Users/nikhitamantravadi/Downloads/di_report-srilanka.csv"  # Replace with the correct path

# Load the datasets
opportunities_data <- read_csv(opportunities_file)
threats_data <- read_delim(threats_file, delim = "\t")

# Clean the Threats Dataset
threats_cleaned <- threats_data %>%
  # Remove unnecessary columns (e.g., "Unnamed" columns)
  select(-starts_with("Unnamed")) %>%
  # Convert `Date (YMD)` to a proper date format
  mutate(Date = as.Date(`Date (YMD)`, format = "%d/%m/%Y")) %>%
  # Extract year for grouping
  mutate(Year = format(Date, "%Y")) %>%
  # Keep only relevant columns
  select(Year, Event, Deaths, Injured, Missing, `Losses $USD`) %>%
  # Aggregate by Year and Event
  group_by(Year, Event) %>%
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE),
    Total_Injured = sum(Injured, na.rm = TRUE),
    Total_Missing = sum(Missing, na.rm = TRUE),
    Total_Losses = sum(`Losses $USD`, na.rm = TRUE),
    .groups = "drop"
  )


# Clean the Opportunities Dataset
# Extract only District and Total_Production
opportunities_cleaned <- opportunities_data %>%
  # Keep only the necessary columns
  select(District, Total_Production) %>%
  # Convert Total_Production to numeric (if it's not already)
  mutate(Total_Production = as.numeric(Total_Production))

opportunities_cleaned <- opportunities_cleaned %>%
  drop_na(Total_Production)
# View the cleaned opportunities dataset
print("Cleaned Opportunities Dataset:")
print(opportunities_cleaned)

#Furthuer Cleaning of threats dataset
# Load the cleaned data
cleaned_data <- read.csv("cleaned_threats.csv")

# Consolidate all rows by Event
consolidated_threat_data <- cleaned_data %>%
  group_by(Event) %>%
  summarise(
    Total_Deaths = sum(Total_Deaths, na.rm = TRUE),
    Total_Injured = sum(Total_Injured, na.rm = TRUE),
    Total_Missing = sum(Total_Missing, na.rm = TRUE),
    Total_Losses = sum(Total_Losses, na.rm = TRUE),
    .groups = "drop"
  )



# Save cleaned datasets if needed
write_csv(threats_cleaned, "cleaned_threats.csv")
write_csv(opportunities_cleaned, "cleaned_opportunities.csv")
write_csv(consolidated_threat_data, "consolidated_threat_data.csv")

