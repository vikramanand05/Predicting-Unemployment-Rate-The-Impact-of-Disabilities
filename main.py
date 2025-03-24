#install.packages("randomForest")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(lubridate)

# Load the dataset
data <- read.csv("C:\\Users\\hp\\Downloads\\updated_unemployment_data.csv")

# Convert the Period column to Date format and ensure Country is a factor
data$Period <- as.Date(data$Period)
data$Year <- year(data$Period)
data$Country <- as.factor(data$Country)

# Feature Engineering: Create lagged unemployment rates (e.g., previous year's rate)
data <- data %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(Lag1 = lag(Rate, 1),
         Lag2 = lag(Rate, 2),
         Moving_Avg = (Lag1 + Lag2) / 2) %>%
  ungroup()

# Remove rows with NA (due to lag features)
data <- na.omit(data)

# Split the data into training and forecasting datasets
train_data <- data %>% filter(Year <= 2020)
future_years <- data.frame(
  Year = 2019:2030,
  Country = rep(unique(data$Country), each = length(2019:2030))
)

# Initialize future data with lag features from the last available year
latest_data <- data %>%
  group_by(Country) %>%
  filter(Year == 2019) %>%
  select(Country, Rate, Lag1 = Rate, Lag2 = Lag1, Moving_Avg = Moving_Avg)

future_data <- future_years %>%
  left_join(latest_data, by = "Country")

# Initialize Rate column with NA for future years
future_data$Rate <- NA

# Train a Random Forest model
rf_model <- randomForest(Rate ~ Year + Country + Lag1 + Lag2 + Moving_Avg, 
                         data = train_data, 
                         ntree = 500)

# Forecast iteratively for future years
for (year in 2019:2030) {
  # Filter data for the current year
  current_year_data <- future_data %>% filter(Year == year)
  
  # Predict unemployment rates
  predictions <- predict(rf_model, newdata = current_year_data)
  
  # Update the future_data DataFrame with predictions
  future_data <- future_data %>%
    mutate(
      Rate = ifelse(Year == year, predictions, Rate),
      Lag1 = ifelse(Year == year, Rate, Lag1),
      Lag2 = ifelse(Year == year, Lag1, Lag2),
      Moving_Avg = ifelse(Year == year, (Lag1 + Lag2) / 2, Moving_Avg)
    )
}


# Combine historical and forecasted data
combined_data <- bind_rows(
  train_data %>% select(Year, Country, Rate),
  future_data %>% select(Year, Country, Rate)
)

# Plot the results
ggplot(combined_data, aes(x = Year, y = Rate, color = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Unemployment Rate Prediction by Country (2020-2030)",
       x = "Year", y = "Unemployment Rate (%)") +
  theme_minimal()

#facet

ggplot(combined_data, aes(x = Year, y = Rate, color = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Country-wise Unemployment Rate Prediction (2020-2030)",
       x = "Year", y = "Unemployment Rate (%)") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

#heatmap

ggplot(combined_data, aes(x = Year, y = Country, fill = Rate)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Heatmap of Unemployment Rates by Country (2020-2030)",
       x = "Year", y = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#actual vs predict dot lines

combined_data <- combined_data %>%
  mutate(Data_Type = ifelse(Year <= 2020, "Actual", "Predicted"))

ggplot(combined_data, aes(x = Year, y = Rate, color = Country, linetype = Data_Type)) +
  geom_line(size = 1) +
  labs(title = "Actual vs Predicted Unemployment Rates (2020-2030)",
       x = "Year", y = "Unemployment Rate (%)") +
  theme_minimal()


#year

combined_data <- combined_data %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(Yearly_Change = Rate - lag(Rate)) %>%
  ungroup()

ggplot(combined_data %>% filter(!is.na(Yearly_Change)), 
       aes(x = Year, y = Yearly_Change, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly Change in Unemployment Rates by Country",
       x = "Year", y = "Change in Unemployment Rate (%)") +
  theme_minimal()

# Moving Averages vs Predictions

ggplot(future_data, aes(x = Year, group = Country)) +
  geom_line(aes(y = Rate, color = "Predicted Rate"), size = 1) +
  geom_line(aes(y = Moving_Avg, color = "Moving Average"), linetype = "dashed", size = 1) +
  facet_wrap(~Country, scales = "free_y") +
  labs(title = "Predictions vs Moving Averages by Country (2020-2030)",
       x = "Year", y = "Rate (%)") +
  scale_color_manual(values = c("Predicted Rate" = "blue", "Moving Average" = "red")) +
  theme_minimal()


#  Confidence Interval Visualization

# Assuming confidence intervals (lower and upper bounds) are available
future_data <- future_data %>%
  mutate(Lower = Rate - 2,  # Placeholder values; replace with actual intervals
         Upper = Rate + 2)

ggplot(future_data, aes(x = Year, y = Rate, color = Country)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Country), alpha = 0.2) +
  labs(title = "Unemployment Rate Predictions with Confidence Intervals",
       x = "Year", y = "Unemployment Rate (%)") +
  theme_minimal()
