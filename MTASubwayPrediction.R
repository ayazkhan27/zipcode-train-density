# Install and load httr package
install.packages("httr")
library(httr)

# Set US Census Bureau API key
Sys.setenv(CENSUS_API_KEY = "d9fff35e556609d8b5e28a21cbcea24ced3c8b78")

# Read in data
data <- read.csv("C:/Users/User/Desktop/R Codes/train_data.csv")

# Function to reverse geocode latitude and longitude to zipcode
reverse_geocode_to_zipcode <- function(lat, lon) {
  # Build request URL
  url <- paste0("https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=", 
                lon, "&y=", lat, 
                "&benchmark=Public_AR_Current&vintage=Current_Current&format=json")
  # Send request and parse response
  res <- httr::GET(url)
  res_json <- httr::content(res)
  
  # Extract zipcode from response
  if (is.null(res_json$result) || is.null(res_json$result$geographies) || 
      length(res_json$result$geographies) == 0 || 
      length(res_json$result$geographies[[1]]$ZCTA5CE10) == 0) {
    # Return NA if result, geographies, or ZCTA5CE10 is NULL or empty
    return(NA)
  } else {
    # Return zipcode as character value
    return(as.character(res_json$result$geographies[[1]]$ZCTA5CE10))
  }
}

# Create zipcode column by applying reverse_geocode_to_zipcode function to Latitude and Longitude columns
data$zipcode <- mapply(reverse_geocode_to_zipcode, data$Latitude, data$Longitude, SIMPLIFY = FALSE)

# Check the structure of the data frame to make sure that the zipcode column was added correctly
str(data)

#Extract data frame with zipcodes
zipcode <- select(data, zipcode)

#Install and load dplyr package
install.packages("dplyr")
library(dplyr)

#Add column for total number of trains
data <- data %>%
  mutate(trains = rowSums(select(., starts_with("Route"))))
# Group data by zipcode and calculate total number of trains for each zipcode
zipcode_trains <- data %>%
  group_by(zipcode) %>%
  summarize(trains = sum(trains))

# Find zipcodes with least number of trains
least_trains <- zipcode_trains[which.min(zipcode_trains$trains), ]$zipcode

# Find zipcodes with most number of trains
most_trains <- zipcode_trains[which.max(zipcode_trains$trains), ]$zipcode

# Print results
cat("Zipcode with most trains:", most_trains)
cat("Zipcode with least trains:", least_trains)

# Select data for most and least common zipcodes
most_trains_data <- subset(data, zipcodes == most_trains)
other_zipcodes_data <- subset(data, zipcodes != most_trains)

# Run t-test to compare means
t.test(most_trains_data$Trains, other_zipcodes_data$Trains)
# Install and load ggplot2 package
install.packages("ggplot2")
library(ggplot2)

# Create bar plot
ggplot(data, aes(x = zipcodes, y = trains)) +
  geom_bar(stat = "identity")
# Install and load caret package
install.packages("caret")
library(caret)

# Split data into training and test sets
set.seed(123)
train_index <- createDataPartition(trains, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit linear regression model on training data
model <- train(Trains ~ ., data = train_data, method = "lm")

# Make predictions on test data
predictions <- predict(model, test_data)
# Calibrate model
calibrated_model <- calibrate(model, test_data)

# Make calibrated predictions
calibrated_predictions <- predict(calibrated_model, test_data)
# Fit generalized linear model with poisson distribution and log link function
model <- glm(Trains ~ ., data = train_data, family = poisson(link = "log"))

# Make predictions on test data
predictions <- predict(model, test_data, type = "response")
# Calculate MSE, RMSE, and MAE
mse <- mean((predictions - test_data$Trains)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - test_data$Trains))

# Print results
cat("MSE:", mse)
cat("RMSE:", rmse)
cat("MAE:", mae)
# Create scatterplot of predicted and actual values
ggplot(test_data, aes(x = Trains, y = predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Actual Number of Trains") +
  ylab("Predicted Number of Trains") +
  ggtitle("Predicted vs. Actual Number of Trains")
# Fit linear regression model
fit <- lm(predictions ~ Trains, data = test_data)

# Create scatterplot with trendline
ggplot(test_data, aes(x = Trains, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Actual Number of Trains") +
  ylab("Predicted Number of Trains") +
  ggtitle("Predicted vs. Actual Number of Trains")
# Create scatterplot with trendline and confidence interval
ggplot(test_data, aes(x = Trains, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  xlab("Actual Number of Trains") +
  ylab("Predicted Number of Trains") +
  ggtitle("Predicted vs. Actual Number of Trains")
# Create diagnostic plot
plot(calibrated_model, main = "Diagnostic Plot", xlab = "Predicted Values", ylab = "Residuals")
# Create diagnostic plot with custom options
plot(calibrated_model, main = "Diagnostic Plot", xlab = "Predicted Values", ylab = "Residuals",
     xlim = c(min(predictions), max(predictions)), col = "blue")
# Set up cross-validation using 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Fit model using cross-validation
model <- train(Trains ~ ., data = data, method = "lm", trControl = train_control)

# Make predictions using cross-validated model
predictions <- predict(model, data)
# Install and load Boruta package
install.packages("Boruta")
library(Boruta)

# Set up Boruta algorithm
boruta_control <- BorutaControl(fitMaxIter = 100, Verbose = 2, maxRun = 5)

# Run Boruta algorithm to identify important features
boruta_output <- Boruta(Trains ~ ., data = data, doTrace = 2, maxRun = 5, getImp = "mean", ctrl = boruta_control)

# Extract important features
important_features <- subset(data, boruta_output$ImpHistory[, "Decision"] == "Confirmed")

# Fit model using only important features
model <- train(Trains ~ ., data = important_features, method = "lm")

# Make predictions using model with important features
predictions <- predict(model, data)
# Set up grid of hyperparameter values to search over
param_grid <- expand.grid(alpha = c(0.1, 0.5, 1), lambda = c(0.1, 0.5, 1))

# Fit model using cross-validation and grid search
model <- train(Trains ~ ., data = data, method = "glmnet", trControl = train_control, tuneGrid = param_grid)

# Make predictions using tuned model
predictions <- predict(model, data)
