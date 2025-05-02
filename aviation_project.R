library(tidyverse)
library(dplyr)

# --------------------------------- Data Preprocessing ---------------------------------

# Load the datasets
crash_data <- read_csv("C:/PaNDa/CAP_482/Project_DataSets/aviation.csv")
View(crash_data)
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
View(crash_data)

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
crash_data$EventMonth <- as.numeric(format(as.Date(crash_data$EventDate, format="%Y-%m-%d"), "%m"))
accidents_by_month <- crash_data %>%
  group_by(EventMonth) %>%
  summarise(Total_Incidents = n()) %>%
  arrange(EventMonth)
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

#---------------------------------- Visualization ---------------------------------

# Q20. How have aviation incidents changed over the years.
ggplot(accident_rates_by_year, aes(x = Year, y = n)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Trend of Aviation Incidents Over the Years",
       x = "Year", y = "Incident Count") +
  theme_minimal()
View(crash_data)

# Q21. What is the distribution of FatalityRate across AirCraftCategory?
ggplot(crash_data, aes(x = FatalityRate, fill = AirCraftCategory)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Fatality Rate by Aircraft Category",
       x = "Fatality Rate (%)", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Q22. How does the number of incidents vary by state?
ggplot(accidents_per_state, aes(x = reorder(State, -Total_Incidents), y = Total_Incidents)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  coord_flip() +  # Rotate for better readability
  labs(title = "Number of Aviation Incidents by State",
       x = "State", y = "Incident Count") +
  theme_minimal()

# Q23. What are the monthly patterns of aviation incidents?
crash_data$MonthName <- month.name[as.integer(crash_data$EventMonth)]
ggplot(crash_data, aes(x = factor(MonthName, levels = month.name))) +
  geom_bar(fill = "purple") +
  labs(title = "Monthly Distribution of Incidents", x = "Month", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Q24. Compare average FatalityRate by WeatherCondition.
avg_fatality_weather <- crash_data %>%
  group_by(WeatherCondition) %>%
  summarise(AvgFatalityRate = mean(FatalityRate, na.rm = TRUE))
ggplot(avg_fatality_weather, aes(x = WeatherCondition, y = AvgFatalityRate, fill = WeatherCondition)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Fatality Rate by Weather Condition", x = "Weather Condition", y = "Fatality Rate (%)") +
  theme_minimal()

#---------------------------------- Regression-Based Questions ---------------------------------

# Q25. Is there a relationship between NumberOfEngines and FatalityRate?
model1 <- lm(FatalityRate ~ NumberOfEngines, data = crash_data)
summary(model1)
# Visualize Relationship Between NumberOfEngines and FatalityRate
ggplot(crash_data, aes(x = NumberOfEngines, y = FatalityRate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Number of Engines vs. Fatality Rate",
       x = "Number of Engines", y = "Fatality Rate (%)") +
  theme_minimal()

# Q26. Can we predict FatalityRate using WeatherCondition, PurposeOfFlight, and NumberOfEngines?
model3 <- lm(FatalityRate ~ WeatherCondition + PurposeOfFlight + NumberOfEngines, data = crash_data)
summary(model3)

# Q27. Does latitude (location) influence fatality rate?
model4 <- lm(FatalityRate ~ Latitude, data = crash_data)
summary(model4)
# Visualize Relationship Between Latitude and FatalityRate
ggplot(crash_data, aes(x = Latitude, y = FatalityRate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "tomato") +
  labs(title = "Latitude vs. Fatality Rate",
       x = "Latitude", y = "Fatality Rate (%)") +
  theme_minimal()

# Q28. Does the Number of Fatal, Serious, and Minor Injuries Predict the Fatality Rate?
model2 <- lm(FatalityRate ~ FatalInjuryCount + SeriousInjuryCount + MinorInjuryCount, data = crash_data)
summary(model2)
# Visualize Relationship Between FatalInjuryCount and FatalityRate
ggplot(crash_data, aes(x = FatalInjuryCount, y = FatalityRate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Fatal Injury Count vs. Fatality Rate",
       x = "Fatal Injury Count", y = "Fatality Rate (%)") +
  theme_minimal()

# Q29. Can Year (from EventDate) explain variation in FatalInjuryCount?
crash_data$Year <- as.numeric(substr(crash_data$EventDate, 1, 4))
model5 <- lm(FatalInjuryCount ~ Year, data = crash_data) 
summary(model5)
# Visualize Relationship Between Year and FatalInjuryCount
ggplot(crash_data, aes(x = Year, y = FatalInjuryCount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Year vs. Fatal Injury Count",
       x = "Year", y = "Fatal Injury Count") +
  theme_minimal()
str(crash_data)

View(crash_data)

#---------------------------------- ANOVA ---------------------------------

# Q30. Which aircraft category has the highest variation in FatalityRate?
crash_data$AirCraftCategory <- as.factor(crash_data$AirCraftCategory)
anova_model <- aov(FatalityRate ~ AirCraftCategory, data = crash_data)
anova_results <- summary(anova_model)
print(anova_results)
TukeyHSD(anova_model)
# Visualize Variation in FatalityRate by Aircraft Category
ggplot(crash_data, aes(x = AirCraftCategory, y = FatalityRate)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  coord_flip() +  # Rotate for better readability
  labs(title = "Variation of Fatality Rate by Aircraft Category",
       x = "Aircraft Category", y = "Fatality Rate (%)") +
  theme_minimal()

# Q31. Does WeatherCondition significantly impact FatalityRate across multiple weather categories?
anova_model_weather <- aov(FatalityRate ~ WeatherCondition, data = crash_data)
anova_results_weather <- summary(anova_model_weather)
print(anova_results_weather)
TukeyHSD(anova_model_weather)
# Visualize Variation in FatalityRate by Weather Condition
ggplot(crash_data, aes(x = WeatherCondition, y = FatalityRate)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6) +
  coord_flip() +  # Rotate for better readability
  labs(title = "Variation of Fatality Rate by Weather Condition",
       x = "Weather Condition", y = "Fatality Rate (%)") +
  theme_minimal()


# Q32. Does PurposeOfFlight significantly impact FatalityRate across multiple flight purposes?
table(crash_data$PurposeOfFlight)  # Quick overview of categories
sort(table(crash_data$PurposeOfFlight), decreasing = TRUE)  # Sorted by frequency
crash_data <- crash_data %>%
  mutate(PurposeOfFlight_Clean = case_when(
    str_detect(PurposeOfFlight, "PERS") ~ "Personal",
    str_detect(PurposeOfFlight, "BUS") ~ "Business",
    str_detect(PurposeOfFlight, "INST") ~ "Instructional",
    str_detect(PurposeOfFlight, "POSI") ~ "Positioning",
    str_detect(PurposeOfFlight, "FERY") ~ "Ferry",
    str_detect(PurposeOfFlight, "AOBV|ASHO|ADRP|GLDT") ~ "Other",
    str_detect(PurposeOfFlight, "MIL") ~ "Military",
    str_detect(PurposeOfFlight, "PUBF|PUBL|PUBS|PUBU") ~ "Public Service",
    str_detect(PurposeOfFlight, "SKYD") ~ "Skydiving",
    TRUE ~ "Unknown"
  ))
crash_data$PurposeOfFlight_Clean <- as.factor(crash_data$PurposeOfFlight_Clean)
anova_model_purpose <- aov(FatalityRate ~ PurposeOfFlight_Clean, data = crash_data)
anova_results_purpose <- summary(anova_model_purpose)
print(anova_results_purpose)
TukeyHSD(anova_model_purpose)
# Visualize Variation in FatalityRate by Purpose of Flight
ggplot(crash_data, aes(x = PurposeOfFlight_Clean, y = FatalityRate)) +
  geom_boxplot(fill = "lightcoral", alpha = 0.6) +
  coord_flip() +  # Rotate for better readability
  labs(title = "Variation of Fatality Rate by Purpose of Flight",
       x = "Purpose of Flight", y = "Fatality Rate (%)") +
  theme_minimal()

# Q33. Is there a progressive increase in FatalityRate across IncidentSeverity levels?
crash_data$IncidentSeverity <- as.factor(crash_data$IncidentSeverity)
anova_model_severity <- aov(FatalityRate ~ IncidentSeverity, data = crash_data)
anova_results_severity <- summary(anova_model_severity)
print(anova_results_severity)
TukeyHSD(anova_model_severity)
# Visualize Variation in FatalityRate by Incident Severity
ggplot(crash_data, aes(x = IncidentSeverity, y = FatalityRate)) +
  geom_boxplot(fill = "lightyellow", alpha = 0.6) +
  coord_flip() +  # Rotate for better readability
  labs(title = "Variation of Fatality Rate by Incident Severity",
       x = "Incident Severity", y = "Fatality Rate (%)") +
  theme_minimal()

# Q34. Does the severity level of AirCraftDamage correlate with higher FatalityRates?
crash_data$AirCraftDamage <- as.factor(crash_data$AirCraftDamage)
anova_model_damage <- aov(FatalityRate ~ AirCraftDamage, data = crash_data)
anova_results_damage <- summary(anova_model_damage)
print(anova_results_damage)
TukeyHSD(anova_model_damage)
# Visualize Variation in FatalityRate by Aircraft Damage
ggplot(crash_data, aes(x = AirCraftDamage, y = FatalityRate)) +
  geom_boxplot(fill = "lightpink", alpha = 0.6) +
  coord_flip() +  # Rotate for better readability
  labs(title = "Variation of Fatality Rate by Aircraft Damage",
       x = "Aircraft Damage", y = "Fatality Rate (%)") +
  theme_minimal()

