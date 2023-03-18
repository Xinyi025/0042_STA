library(rgdal)
uk_districts <- readOGR(dsn="Data/uk_districts.shp", layer="uk_districts", 
                        p4s = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")@projargs)

china_temp <- read.csv("Data/Temp_China.csv")

china_temp

# 前三列信息不需要，只需要从第四列开始建数据集：
ch_temp_matrix<-data.matrix(china_temp[,4:ncol(china_temp)])

ch_temp_matrix

# 给每一行命名为站名
rownames(ch_temp_matrix) <- paste("sta",1:nrow(ch_temp_matrix),sep = "")

uk_districts
uk_temp_matrix<-data.matrix(uk_districts@data[,-c(1:3)])
uk_temp_matrix
rownames(uk_temp_matrix) <- uk_districts@data[,"NAME"]

plot(uk_temp_matrix["East Anglia",], ylab="Monthly average temperature", xlab="Time (in months)", type="l")

plot(ch_temp_matrix["sta1",],ylab="Annual average temperature",xlab="Time (in years)",type="l")

lag.plot(uk_temp_matrix["East Anglia",], lags=3, do.lines=FALSE)

lag.plot(uk_temp_matrix["East Anglia",], lags=12, do.lines=FALSE)


# ACF
acf(uk_temp_matrix["East Anglia",], lag.max=36, xlab="Lag", ylab="ACF", main="Autocorrelation plot of monthly average temperatures")
EA.s.diff <- diff(uk_temp_matrix["East Anglia",], lag=12, differences=1)
acf(EA.s.diff, lag.max=36, xlab="Lag", ylab="ACF", main="Differenced autocorrelation plot")


pacf(uk_temp_matrix["East Anglia",], lag.max=36,xlab="Lag",ylab="PACF",main="Partial Autocorrelation plot of monthly average temperatures")
pacf(EA.s.diff, lag.max=36, xlab="Lag", ylab="ACF",main="Partial Autocorrelation plot of monthly average temperatures")


pacf(uk_temp_matrix["East Anglia",], lag.max = 36, xlab = "lag", ylab = "PACF", main = "Partial Autocorrelatuion plot of monthly average temperatures")

# try the model of ARIMA(1,0,2)(2,1,1)12

# 3.4.3 parameter estimation and fitting
# 检验模型效果

fit.ar <- arima(uk_temp_matrix["East Anglia", 1:1104], order=c(1,0,2),seasonal = list(order=c(2,1,1),period = 12))
fit.ar

# 
# fit.ar <- arima(uk_temp_matrix["East Anglia", 1:1104], order=c(1,0,1),seasonal = list(order=c(2,1,1),period = 12))
# fit.ar

# fit.ar <- arima(uk_temp_matrix["East Anglia", 1:1104], order=c(12,0,1))
# fit.ar

# normalized root mean squared error (NRMSE)归一化均方根误差 也是检验模型拟合效果的一个指标
source("Data/starima_package.R")
NRMSE_fit <- NRMSE(res=fit.ar$residuals, obs=uk_temp_matrix["East Anglia",1:1104])

NRMSE_fit

NRMSE

# 3.4.4 Diagnostic Checking
tsdiag(fit.ar)



# 3.4.5 Prediction

pre.ar<-predict(fit.ar, n.ahead=12)
matplot(1:12,cbind(uk_temp_matrix["East Anglia", 1105:1116],pre.ar$pred),type="l",main="", xlab="Month", ylab="Average Temp. degrees Celsius")


# 对比不同参数的拟合效果
# fit.Ar <- arima(uk_temp_matrix["East Anglia",1:1104],order=c(1,0,2),seasonal=list(order=c(2,1,1),period=12))
fit.Ar <- arima(uk_temp_matrix["East Anglia",1:1104],order=c(1,0,3),seasonal=list(order=c(2,1,1),period=12))
pre.Ar <- predict(uk_temp_matrix["East Anglia", 1105:(ncol(uk_temp_matrix))], model=fit.Ar)
matplot(cbind(pre.Ar$fitted, pre.Ar$x), type="l")



