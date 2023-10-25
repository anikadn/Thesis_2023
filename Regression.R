library(ggplot2)
library(dplyr)
actual_data <-
  read.csv("./DATA/S&P500.csv", header = TRUE,sep = ";",dec = ",")
actual_data$Date <- as.Date(actual_data$Date, format = "%Y/%m/%d")
# Provided Data
data <- actual_data[actual_data$Date >= as.Date("2020-02-11") & actual_data$Date <= as.Date("2020-04-02"), ]
data$DateNumeric <- as.numeric(data$Date)
filtered_data2 <- actual_data[actual_data$Date >= as.Date("2019-01-01") & actual_data$Date <= as.Date("2021-01-01"), ]
# Quadratic Model
quadratic_model <- lm(Price ~ poly(DateNumeric, 2, raw=TRUE), data = data)

# Predictions
data$predicted <- predict(quadratic_model, data)

# # Plotting
# plot_data <- ggplot(data, aes(x = Date)) +
#   geom_point(data = actual_data, aes(y = Price), color = "yellow", alpha = 0.6) +  # All actual data points
#   geom_point(aes(y = Price), color = "yellow", alpha = 0.9) +  # Highlight data points from the subset
#   geom_line(aes(y = predicted), color = "red") +  # Predicted line
#   labs(title = "Quadratic Regression of Price over Date",
#        x = "Date",
#        y = "Price") +
#   theme_minimal()
#
# print(plot_data)

# 1. Create a sequence of dates
future_dates <- seq(from = max(data$Date) + 1, by = "day", length.out = 30)

# 2. Convert to numeric representation
future_dates_numeric <- as.numeric(future_dates)

# Create a new data frame for prediction
future_data <- data.frame(Date = future_dates, DateNumeric = future_dates_numeric)

# 3. Predict using the model
future_data$predicted <- predict(quadratic_model, future_data)

# Now you can add these future predictions to your plot:
plot_data <- ggplot(data, aes(x = Date)) +
  geom_point(data = filtered_data2, aes(y = Price, color = "Actual Data"), alpha = 0.6) +  # All actual data points
  geom_point(aes(y = Price, color = "Training Data")) +  # Highlight data points from the subset
  geom_line(size = 1,aes(y = predicted, color = "Original Fit"),) +  # Predicted line for original data
  geom_line(size = 1,data = future_data, aes(y = predicted, color = "Future Prediction"), alpha = 0.7) +  # Predicted line for future
  labs(title = "Quadratic Regression of Price over Date",
       x = "Date",
       y = "Price",
       color = "Legend") +  # Legend title
  scale_color_manual(values = c("Actual Data" = "blue",
                                "Training Data" = "red",
                                "Original Fit" = "black",
                                "Future Prediction" = "green3")) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 14
    ),
    axis.text.y =  element_text(size = 14) ,
    axis.title.x = element_text(size = 16, margin = margin(t = 10)) ,
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(l = 10))
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
labs(color = NULL) +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 13)
  )

print(plot_data)
