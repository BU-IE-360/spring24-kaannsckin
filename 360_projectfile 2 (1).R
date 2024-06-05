# Import required libraries
install.packages("GGally")
install.packages("matrixStats")

library(forecast)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(GGally)
library(matrixStats)

tday=today("Turkey")
#day_before_tday <- tday - 1
day_before_tday <- tday - 2

data_path = '/Users/ufukozkan/Desktop/360_deneme'
file_weather = file.path(data_path, 'weather_info 2.csv')
file_production = file.path(data_path, 'production 2.csv')

weather_data = fread(file_weather)
production_data = fread(file_production)

# Tarih ve saat sütunlarını düzenle
weather_data[, date := as.Date(date, format="%Y-%m-%d")]
production_data[, date := as.Date(date, format="%Y-%m-%d")]
production_data[, hour := as.numeric(hour)]

# Verileri numeric tipine dönüştür (uyarıyı gidermek için)
numeric_vars <- names(weather_data)[!names(weather_data) %in% c("date", "hour")]
weather_data[, (numeric_vars) := lapply(.SD, as.numeric), .SDcols = numeric_vars]

# Geniş formatlı hale getir
long_weather = melt(weather_data, id.vars = c("date", "hour"), variable.name = "variable", value.name = "value")
hourly_region_averages = dcast(long_weather, date + hour ~ variable, fun.aggregate = mean, value.var = "value")
template_dt = unique(weather_data[, .(date, hour)])
template_dt = merge(template_dt, production_data, by = c('date', 'hour'), all.x = TRUE)
template_dt = template_dt[date <= (tday + 1)]

template_dt_with_weather = merge(template_dt, hourly_region_averages, by = c("date", "hour"), all.x = TRUE)
template_dt_with_weather = template_dt_with_weather[order(date, hour)]


###NA VALUES###
any_na <- anyNA(weather_data)
if (any_na) {
  cat("The dataset contains NA values.\n")
  # Display the count of NAs per column
  print(colSums(is.na(weather_data)))
} else {
  cat("The dataset does not contain any NA values.\n")
}
# Display all rows that have NA values
na_rows <- weather_data[!complete.cases(weather_data), ]
View(na_rows)

# Fill NA values with the average of the surrounding values (linear interpolation)
merged_data_filled <- weather_data %>%
  mutate(across(where(is.numeric), ~ na.approx(.x, na.rm = FALSE)))

# Fill leading NAs with the next available value, upward
merged_data_filled <- merged_data_filled %>%
  mutate(across(where(is.numeric), ~ na.locf(.x, fromLast = TRUE, na.rm = FALSE)))

weather_data<- merged_data_filled
#####

summary(weather_data)
#weather_data <- weather_data %>%
#pivot_wider(names_from = c(lat, lon),
#values_from = c(dswrf_surface, tcdc_low.cloud.layer, 
#tcdc_middle.cloud.layer, tcdc_high.cloud.layer,
#tcdc_entire.atmosphere, uswrf_top_of_atmosphere,
#csnow_surface, dlwrf_surface, uswrf_surface, tmp_surface))


#weather_data <-  weather_data %>%
#arrange(date, hour)
View(weather_data)


#Coordinate aggregation by long to wide format
long_weather <- weather_data
long_weather <- melt(weather_data,id.vars=c(1:4))
hourly_region_averages = dcast(long_weather, date+hour~variable,fun.aggregate=mean)
View(hourly_region_averages)

# Merge with hourly_region_averages
template_dt_with_weather <- merge(template_dt, hourly_region_averages, by = c('date', 'hour'), all.x = TRUE)
View(template_dt_with_weather)
#Order it by date and hour
template_dt_with_weather = template_dt_with_weather[order(date,hour)]


template_dt_with_aggregate <- template_dt_with_weather

template_dt_with_aggregate$hourly_cloud_average <- rowMeans(select(template_dt_with_aggregate, starts_with("tcdc_")), na.rm = TRUE)
template_dt_with_aggregate$hourly_max_t <- rowMaxs(as.matrix(select(template_dt_with_aggregate, starts_with("tmp_"))), na.rm = TRUE)
View(template_dt_with_aggregate)


# Use select to exclude coumns starting with "tcdc_" to focus on average
template_dt_with_aggregate <- template_dt_with_aggregate %>%
  select(-starts_with("tcdc_"))
#-starts_with("tmp_"))


all_data = template_dt_with_aggregate[!is.na(production)]
all_data_daily <- all_data[all_data$date == day_before_tday, ]

available_data = template_dt_with_weather[!is.na(production) & hour >= 4 & hour <= 19,]
#to_be_forecasted = template_dt_with_weather[is.na(production)]
to_be_forecasted <- template_dt_with_weather[is.na(production) & hour >= 4 & hour <= 19, ]

do_not_use = c('date','hour')

# Model oluştur ve tahmin yap
lr_model = lm(production ~ ., data = available_data[, !c('date', 'hour'), with = FALSE])
forecasted = predict(lr_model, to_be_forecasted)
forecasted[forecasted < 0] = 0

forecast_table = to_be_forecasted[, .(date, hour)]
forecast_table[, lr_forecast := forecasted]

# ARIMA modeli
fitted = auto.arima(available_data$production, trace = TRUE)
forecast_horizon = nrow(to_be_forecasted)
forecasted_arima = forecast(fitted, h = forecast_horizon)$mean

forecasted_arima[forecasted_arima < 0] = 0
forecast_table[, arima_forecast := forecasted_arima]

# Yarının saatlik tahminlerini düzenle ve göster
day_ahead_forecast = forecast_table[date == (tday + 1)]
print(day_ahead_forecast)
View(day_ahead_forecast)
