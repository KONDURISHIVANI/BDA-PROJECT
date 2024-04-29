# Load the dataset
stock_data <- read.csv("C:/Users/Nikhitha Reddy/OneDrive/Desktop/GOOGLE.csv")
# View the structure of the dataset
str(stock_data)
# Perform linear regression
model <- lm(Close ~ Volume, data = stock_data)
# View summary of the regression model
summary(model)
# Plotting the data and regression line
# Load the ggplot2 library
library(ggplot2)

# Create a scatter plot with a linear regression line
ggplot(stock_data, aes(x = Volume, y = Close)) +
  geom_point() +  # Add scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = "Volume", y = "Close Price", title = "Linear Regression - Volume vs. Close")

# Predictions on existing data
predicted_values <- predict(model, newdata = stock_data)

# Create new data for prediction
new_data <- data.frame(Volume = c(10000, 20000, 30000))  # Example new volume values

# Predictions on new data
new_predictions <- predict(model, newdata = new_data)
print(new_predictions)
# Calculate R-squared
rsquared <- summary(model)$r.squared
print(paste("R-squared:", rsquared))

# Calculate Mean Squared Error (MSE)
mse <- mean((stock_data$Close - predicted_values)^2)
print(paste("Mean Squared Error (MSE):", mse))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))



# Histogram of residuals
hist(residuals(model), breaks = 20, col = "lightblue",
     main = "Histogram of Residuals",
     xlab = "Residuals")


stock_data$Date <- as.Date(stock_data$Date)  # Convert to Date format if needed

# Time series plot
plot(stock_data$Date, stock_data$Close, type = "l",
     xlab = "Date", ylab = "Close Price",
     main = "Close Price Over Time")


#Decision Tree
library(rpart)
# Fit a decision tree model
tree_model <- rpart(Close ~ Volume, data = stock_data)

# Display the summary of the tree model
print(tree_model)
plot(tree_model)
text(tree_model)
# Example new data for prediction
new_data <- data.frame(Volume = c(10000, 20000, 30000))

# Predict using the decision tree model
predicted_values <- predict(tree_model, newdata = new_data)

# Display predicted values
print(predicted_values)
tree_model <- rpart(Close ~ Volume, data = stock_data, minsplit = 10, minbucket = 5)







