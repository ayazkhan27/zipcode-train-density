# First, we need to install and load the necessary packages

install.packages("tidyverse")
install.packages("zip")
library(tidyverse)
library(zip)

# Next, we read in the data from the provided file
train_data <- read_csv("train_data.csv")

# Convert latitude and longitude to zip codes with extension codes

train_data <- train_data %>%
  mutate(zip_ext = lapply(1:nrow(.), function(i) zip(train_data$Latitude[i], train_data$Longitude[i])))

# Now we can use the group_by and summarize functions to find the number of trains that pass through each zip code

train_counts <- train_data %>%
  group_by(zip_ext) %>%
  summarize(num_trains = n())

# We can visualize the results with a barplot
ggplot(train_counts, aes(x = zip_ext, y = num_trains)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trains Passing Through Each Zip Code", x = "Zip Code", y = "Number of Trains")

# To find the locations where the most and least number of trains pass through, we can use the top_n and bottom_n functions

most_trains <- train_counts %>% top_n(1, num_trains)
least_trains <- train_counts %>% bottom_n(1, num_trains)

# Let's print out the results

print(paste("The location with the most trains passing through is", most_trains$zip_ext))
print(paste("The location with the least trains passing through is", least_trains$zip_ext))

# Finally, we can create a linear model to predict the number of trains passing through each zip code based on the other variables in the dataset

model <- lm(num_trains ~ ., data = train_data)

#We can check the model summary to see how well the model fits the data

summary(model)

#We can also visualize the model fit with a residual plot

ggplot(model, aes(x = num_trains, y = .resid)) +
  geom_point() +
  labs(title = "Residual Plot for Train Count Prediction Model", x = "Predicted Number of Trains", y = "Residuals")
