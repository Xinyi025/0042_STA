# 目标空间单元的列名
target_unit_colname <- "817198"

# 通过列名获取目标空间单元的索引
target_unit <- match(target_unit_colname, colnames(train_data))

# 提取目标空间单元的单变量时间序列
single_ts <- ts(train_data[, target_unit], frequency = 24)

# 对单变量时间序列进行ARIMA建模
auto_arima_model <- auto.arima(single_ts)

# 预测
n_periods <- 24*6 # 预测期数
arima_forecast <- forecast(auto_arima_model, h = n_periods)


# 对比预测结果与实际结果

# 提取列名为"817198"的数据
predict_data <- as.data.frame(st_arima_forecast)
actual_data <- as.data.frame(test_data[,target_unit])

# 将列名统一为"Value"
colnames(predict_data) <- "Value"
colnames(actual_data) <- "Value"

# 使用rbind将两个数据框堆叠在一起
combined_data <- rbind(predict_data, actual_data)

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