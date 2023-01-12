# Load required packages
library(maps)
library(dplyr)

train_data <- read.csv(file.choose())

# Count the number of non-empty values in each row of the Route columns
train_data <- train_data %>%
  mutate(total_trains = rowSums(select(., starts_with("Route")) != ""))

# Convert latitude and longitude to zipcodes
train_data <- train_data %>%
  mutate(zip_ext = zipcode(train_data[, c("Latitude", "Longitude")]))

# Group by zipcode and sum the total number of trains passing through each zipcode
zip_trains <- train_data %>%
  group_by(zip_ext) %>%
  summarize(total_trains = sum(total_trains))

# Find the zipcodes with the highest and lowest number of trains passing through
most_trains <- zip_trains %>% top_n(1, total_trains)
least_trains <- zip_trains %>% top_n(1, total_trains, order_by = "total_trains")

#Let's print out the results
print(paste("The zipcode with the most trains passing through is", most_trains$zip_ext))
print(paste("The zipcode with the least trains passing through is", least_trains$zip_ext))

#Let's visualize the results
ggplot(zip_predictions, aes(x = zip_ext, y = num_trains)) +
  geom_bar(stat = "identity") +
  labs(x = "Zipcode", y = "Number of Trains") +
  ggtitle("Number of Trains by Zipcode")

#Let's also use a statistical test to see if the difference in number of trains passing through the zipcodes is statistically significant
t.test(zip_predictions$num_trains, zip_predictions$zip_ext)