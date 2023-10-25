#####################################
#Please run the code line by line
#####################################
library(zoo)
library(ggplot2)
doInit = TRUE
# This is the date from which we start
start_date = "2007-01-01"
# This represents the date we can see and it will be incremented
# Make sure it is after the start date
end_Date = as.Date("2007-06-01")
# This is the final date 
end_end_date = as.Date("2009-01-01")
while (end_Date < end_end_date) {
  if (doInit) {
    actual_data <-
      read.csv("./DATA/S&P500.csv", header = TRUE,sep = ";",dec = ",")
    actual_data$Date <- as.Date(actual_data$Date, format = "%Y/%m/%d")
    un_rate <-
      read.csv("./DATA/Unemploytment Rates.csv", header = TRUE,sep = ";",dec = ",")
    un_rate$Date <- as.Date(un_rate$Date, format = "%Y/%m/%d")
    int_rate <-
      read.csv("./DATA/InterestRates.csv", header = TRUE,sep = ";",dec = ",")
    int_rate$Date <- as.Date(int_rate$Date, format = "%Y/%m/%d")
    possible_significant_dates = NA
    triggers = data.frame(x = as.Date(character(0)), y = numeric(0))
    significant_drops = data.frame(Date = as.Date(character(0)), Rate = numeric(0))
    int_start_date = end_Date - 150
    plt = NULL
    
    significant_drops <- data.frame(Date = as.Date(character(0)),
                                    Price = numeric(0))
    
    decline_threshold <- 3.5
    flag = FALSE
    stopp = FALSE
    doInit = FALSE
  }
  filtered_data <-
    actual_data[actual_data$Date >= as.Date(start_date) &
                  actual_data$Date <= as.Date(end_Date),]
  filtered_data2 <-
    actual_data[actual_data$Date >= as.Date(start_date) &
                  actual_data$Date <= end_end_date,]
  filtered_un_rate <-
    un_rate[un_rate$Date >= as.Date(start_date) &
              un_rate$Date <= as.Date(end_Date),]
  filtered_int_rate <-
    int_rate[int_rate$Date >= as.Date(int_start_date) &
               int_rate$Date <= as.Date(end_Date),]
  filtered_int_rate_start <-
    int_rate[int_rate$Date >= as.Date(start_date) &
               int_rate$Date <= as.Date(end_Date),]
  
  if (nrow(filtered_data) <= 0) {
    print("NO such date in this set")
    return()
  }
  
  
  # Convert the stock price column to a zoo object
  stock_zoo_data <-
    zoo(filtered_data$Price, order.by = filtered_data$Date)
  int_zoo_data <-
    zoo(filtered_int_rate_start$Interest.Rate, order.by = filtered_int_rate_start$Date)
  
  # Calculate the 20-day moving average, aligned to the right
  LTM <- rollmean(stock_zoo_data, k = 40, align = "left")
  rolling_LTM <- data.frame(Date = index(LTM),
                            RollingMean = coredata(LTM))
  STM <- rollmean(stock_zoo_data, k = 10, align = "left")
  rolling_STM <- data.frame(Date = index(STM),
                            RollingMean = coredata(STM))
  
  int_roll <- rollmean(int_zoo_data, k = 10, align = "right")
  rolling_int <- data.frame(Date = index(int_roll),
                            Rate = coredata(int_roll))
  
  # Compute a scaling factor
  scaling_factor <-
    max(filtered_data2$Price) / max(filtered_int_rate_start$Interest.Rate)
  
  
  ###############################################
  
  
  # Calculate mean and std deviation
  mean_rate <- mean(filtered_int_rate$Interest.Rate)
  std_dev <- sd(filtered_int_rate$Interest.Rate)
  
  # Set the threshold to be x std deviations below the mean
  threshold <- mean_rate - 2 * std_dev
  
  # Identify new significant drops
  new_drops <- rolling_int[rolling_int$Rate <= threshold,]
  
  # Append to the existing data frame
  significant_drops <- rbind(significant_drops, new_drops)
  
  # Remove duplicates, keeping the last occurrence
  significant_drops <-
    significant_drops[!duplicated(significant_drops$Date, fromLast = TRUE),]
  
  
  # Filter for drops within 10 days of the last date
  last_date <- max(filtered_int_rate$Date)
  if (length(significant_drops) > 0) {
    recent_significant_drops <-
      significant_drops[significant_drops$Date >= last_date - 10,]
  }
  
  possible_significant_dates = NA
  possible_significant_dates = recent_significant_drops[nrow(recent_significant_drops), ]$Date
  if (length(possible_significant_dates) == 0) {
    possible_significant_dates = NA
  }
  
  current_long_term_avg = rolling_LTM$RollingMean[nrow(rolling_LTM)]
  
  current_short_term_avg = rolling_STM$RollingMean[nrow(rolling_STM)]
  
  # Calculate the percentage decline of short-term average relative to the long-term average
  percentage_decline <-
    (current_long_term_avg - current_short_term_avg) / current_long_term_avg * 100
  
  # Assess the significance of the decline
  # This checks if there are any significant dates, regarding the interest rate, 
  # in the last 10 days that can be a sign for a trigger
  if (!(is.na(possible_significant_dates))) {
    # If we see a significant decline we raise a flag to show that a trigger point
    # is to come. We are also updating the decline threshold.
    if (length(percentage_decline) > 0 &&
        (percentage_decline) > decline_threshold) {
      print(
        paste(
          "Percentage decline: " ,
          round(percentage_decline, 2),
          "is larger than decline threshold: ",
          round(decline_threshold, 2),
          " Saving new decline threshold"
        )
      )
      flag = TRUE
      decline_threshold = percentage_decline
    }
    # Extra check to see if the change in percentage_decline is not due to volatility
    else if (length(percentage_decline) > 0 &&
             percentage_decline > decline_threshold * 0.95) {
      print(
        paste(
          "percentage decline:" ,
          round(percentage_decline, 2),
          " is still larger than 0.95 * decline threshold:  ",
          round(decline_threshold * 0.95, 2)
        )
      )
    }
    # Now we can save the date as a trigger.
    else if (flag == TRUE) {
      # Due to the nature of our data sometimes we reached dates where there
      # was no stock prices recorded. To compensate for this we searched for the
      # closest value to the date
      differences <- filtered_data2$Date - end_Date
      index <- which.min(ifelse(differences > 0, differences, Inf))
      
      next_date <- filtered_data2$Date[index]
      trigger_y <-
        filtered_data2$Price[which(filtered_data2$Date == next_date)]
      new_data <- data.frame(x = as.Date(next_date), y = trigger_y)
      triggers <- rbind(triggers, new_data)
      flag = FALSE
      stopp = TRUE
      print(
        paste(
          "TRIGGER!!!! ",
          "percentage decline:" ,
          round(percentage_decline, 2),
          " decline threshold:  ",
          round(decline_threshold, 2)
        )
      )
      
    }
    else{
      possible_significant_dates = NA
    }
  }
  else{
    decline_threshold = 3.5
  }
  #stock_mean = mean(filtered_data)
  
  
  print(possible_significant_dates)
  print(end_Date)
  
  
  ######################################################################
  
  plt = ggplot(filtered_data2, aes(x = Date, y = Price, color)) +
    geom_line(aes(color = "Stock Prices")) +
    scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") +
    labs(title = "ETF and Interest Rate") +
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
    geom_line(data = rolling_LTM, aes(x = Date, y = RollingMean, color =
                                        "40 day Moving Avg")) +
    geom_line(data = rolling_STM, aes(x = Date, y = RollingMean, color =
                                        "10 day Moving Avg")) +
    geom_line(data = rolling_int,
              aes(
                x = Date,
                y = Rate * scaling_factor,
                color = "Interest Rate"
              )) +
    geom_vline(xintercept = filtered_data$Date[length(filtered_data$Date)], linetype =
                 "dashed") +
    scale_y_continuous(name = "Stock Prices ($)",
                       sec.axis = sec_axis( ~ . / scaling_factor, name = "Rate (%)")) +
    scale_color_manual(
      values = c(
        "Stock Prices" = "black",
        "10 day Moving Avg" = "red",
        "40 day Moving Avg" = "blue",
        "Interest Rate" = "green3"
      )
    ) +
    labs(color = NULL) +
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 16)) +
    theme(
      legend.position = "top",
      legend.background = element_blank(),
      legend.text = element_text(size = 13)
    )
  
  plt = plt + geom_point(data = triggers,
                         aes(x, y),
                         color = "red",
                         size = 3)
  
  
  if (stopp) {
    print(plt)
    print(paste("Stock price Threshold (1.5 std under mean): ", (
      mean(filtered_data$Price) - (sd(filtered_data$Price)) * 1.5
    )))
    print(paste("Trigger point stock price: ", trigger_y))
    readline(prompt = "Press Enter to continue...")
    stopp = FALSE
  }
  # This was used to stop the mechanism after a certain date and have it stop-and
  # -continue for the rest of the time
  # if(end_Date > as.Date("2008-04-01")){
  #   print(plt)
  #   readline("")
  # }
  
  # Increment "our vision" of the the prices and rates 
  end_Date <- end_Date + 1
  int_start_date = int_start_date + 1
}


plt

########################################################
# Log returns
#######################################################

# 1. Add 365 days to the `triggers` date
triggers$ShiftedDate <- as.Date(triggers$x) + 365/4
closest_prices = c()
for (i in 1:length(triggers$ShiftedDate)) {
  print(triggers$ShiftedDate[i])
  tmp = triggers$ShiftedDate[i] - filtered_data2$Date
  closest_prices = c(closest_prices, filtered_data2$Price[which.max(tmp <=
                                                                     0)])
}


# 3. Extract the price corresponding to each closest date


log_returns <- log(closest_prices / triggers$y) * 100

log_returns


# 
# 
# plt_int = ggplot(filtered_data2, aes(x = Date, y = Price, color)) +
#   geom_line(aes(color = "Stock Prices")) +
#   geom_line(data = rolling_int,
#             aes(
#               x = Date,
#               y = Rate * scaling_factor,
#               color = "Interest Rate"
#             )) +
#   scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") +
#   labs(title = "S&P500 and Interest Rate") +
#   theme(
#     axis.text.x = element_text(
#       angle = 45,
#       hjust = 1,
#       size = 14
#     ),
#     axis.text.y =  element_text(size = 14) ,
#     axis.title.x = element_text(size = 16, margin = margin(t = 10)) ,
#     axis.title.y = element_text(size = 16, margin = margin(r = 10)),
#     axis.title.y.right = element_text(margin = margin(l = 10))
#   ) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   scale_y_continuous(name = "Stock Prices ($)" ,
#                      sec.axis = sec_axis( ~ . / scaling_factor, name = "Rate (%)")) +
#   scale_color_manual(
#     values = c(
#       "Stock Prices" = "black",
#       "Rolling Mean 10day" = "red",
#       "Rolling Mean 25day" = "blue",
#       "Interest Rate" = "green3"
#     )
#   ) +
#   labs(color = NULL) +
#   theme(plot.title.position = "plot",
#         plot.title = element_text(hjust = 0.5, size = 16)) +
#   theme(
#     legend.position = "top",
#     legend.background = element_blank(),
#     legend.text = element_text(size = 14)
#   )
# 
# plt_int
# #
# #
# scaling_factor_un <-
#   max(filtered_data2$Price) / max(filtered_un_rate$Rate)
# plt_un = ggplot(filtered_data2, aes(x = Date, y = Price, color)) +
#   geom_line(aes(color = "Stock Prices")) +
#   geom_line(data = filtered_un_rate,
#             aes(
#               x = Date,
#               y = Rate * scaling_factor_un,
#               color = "Unemployment Rate"
#             )) +
#   scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") +
#   labs(title = "S&P500 and Unemployment Rate") +
#   theme(
#     axis.text.x = element_text(
#       angle = 45,
#       hjust = 1,
#       size = 14
#     ),
#     axis.text.y =  element_text(size = 14) ,
#     axis.title.x = element_text(size = 16, margin = margin(t = 10)) ,
#     axis.title.y = element_text(size = 16, margin = margin(r = 10)),
#     axis.title.y.right = element_text(margin = margin(l = 10))
#   ) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   scale_y_continuous(name = "Stock Prices ($)" ,
#                      sec.axis = sec_axis( ~ . / scaling_factor_un, name = "Rate (%)")) +
#   scale_color_manual(values = c(
#     "Stock Prices" = "black",
#     "Unemployment Rate" = "green3"
#   )) +
#   labs(color = NULL) +
#   theme(plot.title.position = "plot",
#         plot.title = element_text(hjust = 0.5, size = 16)) +
#   theme(
#     legend.position = "top",
#     legend.background = element_blank(),
#     legend.text = element_text(size = 14)
#   )
# 
# plt_un