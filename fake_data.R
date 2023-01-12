# Load required libraries
library(data.table)
library(zoo)

# Set the seed for reproducibility
set.seed(123)

# Generate fake data
fake_data <- data.table(
  date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), length.out = 365),
  value = rnorm(365, mean = 100, sd = 15)
)

# Transform data by adding new columns
fake_data[, month := format(date, "%m")]
fake_data[, month_name := month.abb[as.integer(month)]]
fake_data[, year := format(date, "%Y")]
fake_data[, day_of_week := weekdays(date)]

library(ggplot2)
ggplot(fake_data, aes(x = date, y = value)) + geom_line() + xlab("Date") + ylab("Value") + ggtitle("Fake Data Over Time")

print(ggplot(fake_data, aes(x = date, y = value)) + geom_line() + xlab("Date") + ylab("Value") + ggtitle("Fake Data Over Time"))

# Save plot to file
ggsave("fake_data_plot.png", plot = ggplot(fake_data, aes(x = date, y = value)) + geom_line() + xlab("Date") + ylab("Value") + ggtitle("Fake Data Over Time"), width = 8, height = 6)


# Save data to CSV file
write.csv(fake_data, "fake_data.csv", row.names = FALSE)

