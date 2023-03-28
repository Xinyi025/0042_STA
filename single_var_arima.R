# 目标空间单元的列名
target_unit_colname <- "817198"

# 通过列名获取目标空间单元的索引
target_unit <- match(target_unit_colname, colnames(train_data))

# 提取目标空间单元的单变量时间序列
single_ts <- ts(ts_data_multivar[, target_unit], frequency = 24)

acf(single_ts, lag.max=36, xlab="Lag", ylab="ACF",
    main="Autocorrelation plot of monthly average temperatures")

single_ts_diff <- diff(single_ts, lag=24, differences=1)

acf(single_ts_diff, lag.max=36, xlab="Lag", ylab="ACF",
    main="Autocorrelation plot of monthly average temperatures")

pacf(single_ts, lag.max = 36, xlab="lag", ylab = "PACF", main="Partial
Autocorrelation plot of monthly average temperatures")

pacf(single_ts_diff, lag.max=36,xlab="Lag",ylab="PACF",main="Partial
Autocorrelation plot of monthly average temperatures")


fit.ar <- arima(uk_temp$EA[1:1104],order=c(1,0,2),seasonal=list(order=c(1,1,0)
                                                        ,period=12))

# 对单变量时间序列进行ARIMA建模
auto_arima_model <- auto.arima(single_ts)
summary(auto_arima_model)

# 预测
n_periods <- 24*6 # 预测期数
arima_forecast <- forecast(auto_arima_model, h = n_periods)


# 对比预测结果与实际结果

# 提取列名为"817198"的数据
predict_data <- as.data.frame(st_arima_forecast)
actual_data <- as.data.frame(test_data[,target_unit])



matplot(1:144,cbind(actual_data, predict_data),type="l")

# 为数据框添加时间戳和来源列
comparison_data <- data.frame(
  Timestamp = seq(as.POSIXct("2023-01-26 00:00:00", tz = "UTC"), 
                  by = "hour", length.out = 24 * 6),
  Value = combined_data$'Value',
  Source = c(rep("Actual", length(auctual_data)), 
             rep("Forecast", length(predict_data)))
)

# 绘制折线图
ggplot(comparison_data, aes(x = Timestamp, y = Value, color = Source)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Comparison of Actual and Forecasted Traffic Volume for Station 817198",
       x = "Date",
       y = "Traffic Volume") +
  theme(legend.title = element_blank())