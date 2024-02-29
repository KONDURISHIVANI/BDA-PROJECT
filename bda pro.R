

#VISUALISATION


install.packages("tidyverse")  # Install tidyverse package if not already installed
library(tidyverse)

raw_data<-read.csv("C:/Users/Varshini/Desktop/covid_19_data_cleaned(1).csv")

str(raw_data)

raw_data$Date <- as.Date(raw_data$Date, format = "%d-%m-%Y")
summary(raw_data)


# Line plot of Confirmed cases over time
ggplot(raw_data, aes(x = Date, y = Confirmed)) +
  geom_line() +
  labs(title = "Confirmed COVID-19 Cases Over Time",
       x = "Date",
       y = "Confirmed Cases")



# Bar plot of Total Deaths by Country
subset_data <- slice(raw_data, 1:10000)
ggplot(subset_data, aes(x = Country, y = Deaths)) +
  geom_bar(stat = "sum") +
  coord_flip() +
  labs(title = "Total Deaths by Country",
       x = "Country",
       y = "Total Deaths")


# Scatter plot of Confirmed vs Recovered cases

ggplot(raw_data, aes(x = Confirmed, y = Recovered)) +
  geom_point() +
  labs(title = "Confirmed vs Recovered Cases",
       x = "Confirmed Cases",
       y = "Recovered Cases")



#Time Series of Confirmed Cases:
library(dplyr)
library(tidyr)
library(ggplot2)

covid_summary <- raw_data %>%
  group_by(Date) %>%
  summarize(Confirmed = sum(Confirmed))


# Plot time series of confirmed cases
ggplot(covid_summary, aes(x = Date, y = Confirmed)) +
  geom_line() +
  labs(title = "Time Series of Confirmed Cases",
       x = "Date",
       y = "Confirmed Cases")



#Daily New Cases:
# Calculate daily new cases
covid_daily <- covid_summary %>%
  mutate(Daily_New_Cases = c(NA, diff(Confirmed)))



# Plot daily new cases
ggplot(covid_daily, aes(x = Date, y = Daily_New_Cases)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Daily New Confirmed Cases",
       x = "Date",
       y = "Daily New Cases")



#Country-wise Total Confirmed Cases:
country_summary <- raw_data %>%
  group_by(Country) %>%
  summarize(Total_Confirmed = sum(Confirmed)) %>%
  arrange(desc(Total_Confirmed)) %>%
  top_n(10)


# Plot country-wise total confirmed cases
ggplot(country_summary, aes(x = reorder(Country, Total_Confirmed), y = Total_Confirmed)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Top 10 Countries by Total Confirmed Cases",
       x = "Country",
       y = "Total Confirmed Cases")






#PREDICTION

install.packages("rpart")
install.packages("rpart.plot")

# Load the required library
library(rpart)

pre_data <- read.csv("C:/Users/Varshini/Desktop/covid_19_data_cleaned(1).csv")

# Check column names in pre_data
colnames(pre_data)

# Define the formula for the Decision Tree
formula <- Confirmed ~ Deaths + Recovered

# Fit the Decision Tree
tree_model <- rpart(formula, data = pre_data)

# Make predictions
predictions <- predict(tree_model, pre_data)

# Extract the actual values
actual_values <- pre_data$Confirmed  # Assuming "Confirmed" is the actual column


# Create a function to classify predictions
classify_cases <- function(values, threshold) {
  ifelse(values >= threshold, 1, 0)
}


# Apply classification
threshold <- 100  # Example threshold, adjust as needed
actual_class <- classify_cases(actual_values, threshold)
predicted_class <- classify_cases(predictions, threshold)


# Create confusion matrix
conf_matrix <- table(actual_class, predicted_class)

# Print confusion matrix
print(conf_matrix)



# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 2)))


if (length(predictions) == length(actual_values)) {
  # Evaluate model accuracy
  mae <- mean(abs(predictions - actual_values))
  mse <- mean((predictions - actual_values)^2)
  rmse <- sqrt(mean((predictions - actual_values)^2))
  
  # Print error metrics
  cat("Mean Absolute Error:", mae, "\n")
  cat("Mean Squared Error:", mse, "\n")
  cat("Root Mean Squared Error:", rmse, "\n")
} else {
  print("Length of predictions and actual values do not match.")
}


library(rpart.plot)


# Plot the decision tree
rpart.plot(tree_model, digits = 3, fallen.leaves = TRUE, main = "Decision Tree for COVID-19 Prediction")
