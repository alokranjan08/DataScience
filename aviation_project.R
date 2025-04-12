library(tidyverse)
library(dplyr)

# --------------------------------- Data Preprocessing ---------------------------------

# Load the datasets
crash_data <- read_csv("C:/PaNDa/CAP_482/Project_DataSets/aviation.csv")

# Missing values 
colSums(is.na(crash_data))
# Remove columns with more than 50% missing values
crash_data <- crash_data %>%
  select(-c(DocketUrl, DocketPublishDate))
View(crash_data)

# Fill Missing Numeric Values with Mean/Median
crash_data <- crash_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
View(crash_data)

# Fill Missing Categorical Values with Mode
crash_data <- crash_data %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
View(crash_data)

# Convert categorical variables to factors
crash_data <- crash_data %>%
  mutate(across(where(is.character), as.factor))
str(crash_data)

# Convert Manufacturer in uppercase 
crash_data <- crash_data %>%
  mutate(Make = toupper(Make))

# Replace empty/blank category "," with "UNKNOWN" in AirCraftCategory
crash_data <- crash_data %>%
  mutate(AirCraftCategory = ifelse(AirCraftCategory == " , ", "UNKNOWN", AirCraftCategory))

# Save the cleaned datasets
write_csv(crash_data, "C:/PaNDa/CAP_482/Project_DataSets/aviation_cleaned.csv")

# ------------------------------------------ Exploratory Data Analysis -----------------------------

# Q1. What is the average  number of fatalities per incident by aircraft  category  
avg_fatalities_by_category <- crash_data %>%
  group_by(AirCraftCategory) %>%
  summarise(AvgFatalities = mean(FatalInjuryCount, na.rm = TRUE)) %>%
  arrange(desc(AvgFatalities))
View(avg_fatalities_by_category)

# Q2. What  percentage of incidents have an official report published?
report_percentage <- crash_data %>%
  summarise(ReportPublished = sum(!is.na(ReportStatus)) / n() * 100)
cat("Percentage of incidents with an official report published: ", report_percentage$ReportPublished, "%\n")

# Q3. What are the most common causes of aviation accidents?
commonn_causes <- crash_data %>%
  group_by(ProbableCause) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  top_n(10, Count)
View(commonn_causes)

#-------------------------------------- Data Extraction and Filtering -----------------------------

# Q4. Which years had the highest aviation accident rates?
accident_rates_by_year <- crash_data %>%
  mutate(Year = as.numeric(substr(EventDate, 1, 4))) %>%
  count(Year) %>%
  arrange(desc(n))
View(accident_rates_by_year)

# Q5. What is the survival rate of aviation incidents?
survival_rate <- crash_data %>%
  mutate(Survival = (1 - (FatalInjuryCount / (FatalInjuryCount + SeriousInjuryCount + MinorInjuryCount ))) * 100)
cat("Survival rate of aviation incidents: ", mean(survival_rate$Survival, na.rm = TRUE), "%\n")

# Q6. Do weather conditions (VMC vs IMC) contribute to more accidents?
weather_accidents <- crash_data %>%
  group_by(WeatherCondition) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
cat("Accidents in VMC: ", weather_accidents$Count[weather_accidents$WeatherCondition == "VMC"], "\n")
cat("Accidents in IMC: ", weather_accidents$Count[weather_accidents$WeatherCondition == "IMC"], "\n")
cat("Accidents in Unknown: ", weather_accidents$Count[weather_accidents$WeatherCondition == "Unknown"], "\n")

# Q7. How many  incidents involve multi-engine aircraft?
multi_engine_incidents <- crash_data %>%
  count(NumberOfEngines) %>%
  summarise(TotalIncidents = sum(n))
cat("Total incidents involving multi-engine aircraft: ", multi_engine_incidents$TotalIncidents, "\n")

#---------------------------------- Grouping and Summarization ---------------------------------

# Q8. Which manufacturer has the highest number of fatal incidents per 100 aircraft registered?
fatal_incidents_per_manufacturer <- crash_data %>%
  group_by(Make) %>%
  summarise(Fatal_Incidents = sum(FatalInjuryCount, na.rm = TRUE)) %>%
  arrange(desc(Fatal_Incidents)) %>%
  head(10)
cat("Top 10 manufacturers with the highest number of fatal incidents:\n")
print(fatal_incidents_per_manufacturer)

# Q9. Which type of flight purpose has the highest accident rate per 1000 flights?
accident_rate_by_purpose <- crash_data %>%
  group_by(PurposeOfFlight) %>%
  summarise(Total_Incidents = n()) %>%
  arrange(desc(Total_Incidents))
cat("Accident rate by purpose of flight:\n")
print(accident_rate_by_purpose)

# Q10. Which type of aircraft is most frequently involved in fatal incidents?
fatal_incidents_by_aircraft <- crash_data %>%
  group_by(AirCraftCategory) %>%
  summarise(Fatal_Incidents = sum(FatalInjuryCount, na.rm = TRUE)) %>%
  arrange(desc(Fatal_Incidents))
cat("Top aircraft categories involved in fatal incidents:\n")
print(fatal_incidents_by_aircraft)


#---------------------------------- Sorting and Ranking ---------------------------------
# Q11. Which state has the highest number of aviation accidents per million people?
accidents_per_state <- crash_data %>%
  group_by(State) %>%
  summarise(Total_Incidents = sum(FatalInjuryCount, na.rm = TRUE)) %>%
  arrange(desc(Total_Incidents)) %>%
  head(10)
cat("Top 10 states with the highest number of aviation accidents:\n")
print(accidents_per_state)

# Q12. Rank the top 5 airline with the least accidents
least_accidents <- crash_data %>%
  group_by(Make) %>%
  summarise(Total_Incidents = n()) %>%
  arrange(Total_Incidents) %>%
  head(5)
cat("Top 5 airlines with the least accidents:\n")
print(least_accidents)

# Q13. Which five years had the deadliest aviation accidents?
deadliest_years <- crash_data %>%
  mutate(Year = as.numeric(substr(EventDate, 1, 4))) %>%
  group_by(Year) %>%
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE)) %>%
  arrange(desc(Total_Fatalities)) %>%
  head(5)
cat("Top 5 deadliest years for aviation accidents:\n")
print(deadliest_years)


#-------------------------------- Feature Engineering ---------------------------------

# Q14. Create a new column for "IncidentSeverity" based on the number of fatalities
crash_data <- crash_data %>%
  mutate(IncidentSeverity = case_when(
    FatalInjuryCount > 0 ~ "Catastrophic",
    SeriousInjuryCount > 0 ~ "Serious",
    MinorInjuryCount > 0 ~ "Minor",
    TRUE ~ "No Injury"
  ))
View(crash_data)

# Q15. Generate a new feature 'FatalityRate' as the ratio of fatalities to total injuries
crash_data <- crash_data %>%
  mutate(FatalityRate = (FatalInjuryCount / (FatalInjuryCount + SeriousInjuryCount + MinorInjuryCount)) * 100)
View(crash_data)

#---------------------------------- Hypothesis Testing and Advanced Insights ---------------------------------

# Q16. Does the time of year affect the number of aviation accidents?
crash_data$EventDate <- as.numeric(format(as.Date(crash_data$EventDate, format="%Y-%m-%d"), "%m"))
accidents_by_month <- crash_data %>%
  group_by(EventDate) %>%
  summarise(Total_Incidents = n()) %>%
  arrange(EventDate)
cat("Accidents by month:\n")
print(accidents_by_month)

# Q17. Are amateur-built aircraft more dangerous than factory-built aircraft?
  # Step 1: Keep only rows with valid TRUE/FALSE values
crash_data$AmateurBuilt <- ifelse(crash_data$AmateurBuilt == "TRUE", TRUE,
                                ifelse(crash_data$AmateurBuilt == "FALSE", FALSE, NA))

aviation_clean <- crash_data %>%
  filter(!is.na(AmateurBuilt))
t.test(FatalInjuryCount ~ AmateurBuilt, data = crash_data)

# Q18. Is there a significant difference in fatal injuries between incidents that occurred in the Northern Hemisphere vs. the Southern Hemisphere?
# Step 1: Classify hemisphere
aviation_geo <- crash_data %>%
  filter(!is.na(Latitude), !is.na(FatalInjuryCount)) %>%
  mutate(Hemisphere = ifelse(Latitude >= 0, "Northern", "Southern"))

# Step 2: Perform t-test
t.test(FatalInjuryCount ~ Hemisphere, data = aviation_geo)

# Q19. Do incidents at airports have a higher fatality rate than those that occur elsewhere?
aviation_airport <- crash_data %>%
  filter(!is.na(AirportID)) %>%
  group_by(AirportID) %>%
  summarise(Total_Fatalities = mean(FatalInjuryCount, na.rm = TRUE)) %>%
  arrange(desc(Total_Fatalities))
cat("Fatality rate at airports:\n")
print(aviation_airport)
