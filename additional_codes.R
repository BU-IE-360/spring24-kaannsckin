

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <-read.csv("/Users/kaan/Desktop/360_dnm/production.csv")

# Convert the date column to Date type
data$date <- as.Date(data$date)

# Filter data to include only the years 2022 and 2023
data_filtered <- data %>%
  filter(format(date, "%Y") %in% c("2022", "2023"))

# Calculate the daily average production
daily_avg <- data_filtered %>%
  group_by(date) %>%
  summarise(avg_production = mean(production, na.rm = TRUE))

# Create the plot as a bar graph
ggplot(daily_avg, aes(x = date, y = avg_production)) +
  geom_col() +
  labs(title = "Daily Average Energy Production (2022-2023)", 
       x = "Date", 
       y = "Average Production (in MwH)") +
  theme_minimal()

# Save the plot as a PNG file
ggsave("daily_average_energy_production_2022_2023.png")


library(readr)



# Convert the date column to Date type
data$date <- as.Date(data$date)

# Filter data to include only the years 2022 and 2023
data_filtered <- data %>%
  filter(format(date, "%Y") %in% c("2022", "2023"))

# Calculate the hourly average production
hourly_avg <- data_filtered %>%
  group_by(hour) %>%
  summarise(avg_production = mean(production, na.rm = TRUE)) %>%
  mutate(avg_production = round(avg_production, 2)) # Round to two decimal places

# Print the result
print(hourly_avg)
install.packages("kableExtra")
# Create a formatted table similar to the example
library(knitr)
library(kableExtra)

kable(hourly_avg, col.names = c("Hour", "Production"), align = 'c', caption = "Average production rates (hourly, at two significance)") %>%
  kable_styling(full_width = F)
########

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)



# Convert the date column to Date type
data$date <- as.Date(data$date)

# Select critical hours (5 AM, 6 AM, 6 PM, 7 PM)
critical_hours <- c(5, 6, 18, 19)

# Filter data to include only the critical hours
data_critical_hours <- data %>%
  filter(hour %in% critical_hours) %>%
  mutate(hour = factor(hour, levels = c(5, 6, 18, 19), labels = c("5 AM", "6 AM", "6 PM", "7 PM")))

# Create the plot
p <- ggplot(data_critical_hours, aes(x = date, y = production, color = hour)) +
  geom_line() +
  labs(title = "Production rates of critical hours", 
       x = "Date", 
       y = "Production") +
  theme_minimal() +
  scale_color_manual(values = c("5 AM" = "lightblue", "6 AM" = "red", "6 PM" = "green", "7 PM" = "brown"))

# Display the plot
print(p)

# Save the plot as a PNG file
ggsave("critical_hours_production.png", plot = p, width = 10, height = 6)

####################


#####################

install.packages("caret")
install.packages("forecast")



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)   # For fill function
library(forecast) # For Acf and Pacf functions



# Convert the date column to Date type
data$date <- as.Date(data$date)

# Ensure all dates and hours are included
all_dates <- seq(min(data$date), max(data$date), by = "day")
data <- data %>%
  complete(date = all_dates, hour = 0:23) %>%
  fill(production, .direction = "downup") # Fill missing production values

# Ensure 'production' is numeric
data$production <- as.numeric(data$production)

# Filter data for critical hours (5 AM, 6 AM, 6 PM, 7 PM)
critical_hours <- c(5, 6, 18, 19)
data_critical_hours <- data %>%
  filter(hour %in% critical_hours) %>%
  mutate(hour = factor(hour, levels = critical_hours, labels = c("5 AM", "6 AM", "6 PM", "7 PM")))

# Create the plot
p <- ggplot(data_critical_hours, aes(x = date, y = production, color = hour)) +
  geom_line() +
  labs(title = "Production rates of critical hours", 
       x = "Date", 
       y = "Production") +
  theme_minimal() +
  scale_color_manual(values = c("5 AM" = "lightblue", "6 AM" = "red", "6 PM" = "green", "7 PM" = "brown"))

# Display the plot
print(p)

# Save the plot as a PNG file
ggsave("critical_hours_production.png", plot = p, width = 10, height = 6)

# Model for Regular Hours (6 AM to 7 PM)
regular_hours <- 6:19
data_regular_hours <- data %>%
  filter(hour %in% regular_hours) %>%
  mutate(hour_factor = as.factor(hour),
         month_factor = as.factor(format(date, "%m")))

# Linear model
model <- lm(production ~ hour_factor + month_factor, data = data_regular_hours)

# Calculate Adjusted R2 score
r2 <- summary(model)$adj.r.squared
cat("Adjusted R2 score: ", r2, "\n")
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(forecast) # For Acf and Pacf functions
library(tidyr)   # For fill function



# Convert the date column to Date type
data$date <- as.Date(data$date)

# Filter data for critical hours (5 AM, 6 AM, 7 PM)
critical_hours <- c(5, 6, 19)
data_critical_hours <- data %>%
  filter(hour %in% critical_hours) %>%
  mutate(hour = factor(hour, levels = critical_hours, labels = c("5 AM", "6 AM", "7 PM")))

# Generate ACF and PACF plots for each critical hour
for (critical_hour in levels(data_critical_hours$hour)) {
  # Filter data for the specific critical hour
  data_specific_hour <- data_critical_hours %>%
    filter(hour == critical_hour)
  
  # Fit a linear model for the specific critical hour
  model <- lm(production ~ date, data = data_specific_hour)
  
  # Calculate residuals
  residuals <- residuals(model)
  
  # Plot ACF
  acf_res <- Acf(residuals, main = paste("ACF of Residuals -", critical_hour))
  # Save ACF plot
  acf_plot_file <- paste0("acf_residuals_", gsub(" ", "_", tolower(critical_hour)), ".png")
  png(acf_plot_file, width = 800, height = 600)
  plot(acf_res, main = paste("ACF of Residuals -", critical_hour))
  dev.off()
  
  # Plot PACF
  pacf_res <- Pacf(residuals, main = paste("PACF of Residuals -", critical_hour))
  # Save PACF plot
  pacf_plot_file <- paste0("pacf_residuals_", gsub(" ", "_", tolower(critical_hour)), ".png")
  png(pacf_plot_file, width = 800, height = 600)
  plot(pacf_res, main = paste("PACF of Residuals -", critical_hour))
  dev.off()
  
  # Display plots in R
  plot(acf_res, main = paste("ACF of Residuals -", critical_hour))
  plot(pacf_res, main = paste("PACF of Residuals -", critical_hour))
}

# Check autocorrelation of residuals
residuals <- residuals(model)
acf_residuals <- forecast::Acf(residuals, main = "ACF of Residuals")
pacf_residuals <- forecast::Pacf(residuals, main = "PACF of Residuals")

# Plot ACF and PACF
par(mfrow = c(2, 1))
plot(acf_residuals, main = "ACF of Residuals")
plot(pacf_residuals, main = "PACF of Residuals")

# Save ACF and PACF plots
ggsave("acf_residuals.png", plot = last_plot(), width = 10, height = 6)
ggsave("pacf_residuals.png", plot = last_plot(), width = 10, height = 6)

