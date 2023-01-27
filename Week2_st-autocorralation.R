# Load the required libraries
library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)

#set working directory accordingly with setwd()

# 读取shp文件
uk_districts <-
  readOGR(
    dsn = "Data/uk_districts.shp",
    layer = "uk_districts",
    p4s = CRS(
      "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
    )@projargs
  )

china_temp <- read.csv("Data/Temp_China.csv")
ch_temp_matrix <- data.matrix(china_temp[, 4:ncol(china_temp)])
uk_temp_matrix <- data.matrix(uk_districts@data[, -c(1:3)])
rownames(uk_temp_matrix) <- uk_districts@data[, "NAME"]


# UK average temperature, January 1910
tm_shape(uk_districts) +
  tm_fill("Jan_1910", style = "jenks", palette = "Purples") +
  tm_borders("white") +
  tm_compass(position = c("left", "top"), type = "4star") +
  tm_scale_bar(position = c("right", "top"), text.size = 12)


# UK average temperature, July 1910
tm_shape(uk_districts) +
  tm_fill("Jul_1910", style = "jenks", palette = "Purples") +
  tm_borders("white") +
  tm_compass(position = c("left", "top")) +
  tm_scale_bar() +
  tm_legend(outside.position = "bottom")


# UK average temperature, January 1910, custom breaks
brks = quantile(as.numeric(unlist(uk_districts@data[, -c(1:2)])), seq(0, 1, 1 /
                                                                        10))

Jan_1910 = tm_shape(uk_districts) +
  tm_fill("Jan_1910",
          style = "fixed",
          palette = "-Spectral",
          breaks = brks) +
  tm_borders("white") +
  # tm_compass(position=c("left","top"))+
  tm_scale_bar()
tmap_options(check.and.fix = TRUE)
# interactive map:
tmap_leaflet(Jan_1910)
tmap_mode("view")

# UK average temperature, July 1910, custom breaks
Jul_1910 = tm_shape(uk_districts) +
  tm_fill("Jul_1910",
          style = "fixed",
          palette = "-Spectral",
          breaks = brks) +
  tm_borders("white") +
  # tm_compass(position=c("left","top"))+
  tm_scale_bar()



# 2.3 the concept of the lagged variables
# 观察一组时间序列上的变量，第一个图是温度的时间序列；
# 第二个图是一个散点图，x 轴是每年的温度，y 轴是前一年的温度。
# 自相关系数的值为 0.73（如图所示），表明随后年份的温度具有很强的相关性。

china_temp
ChMeanTemp <- colMeans(china_temp[, 4:(ncol(china_temp))])
ChMeanTemp
ChLagged <-
  data.frame(year = 1951:2001,
             t = ChMeanTemp[2:(length(ChMeanTemp))],
             t_minus_1 = ChMeanTemp[1:(length(ChMeanTemp) - 1)])
ChLagged
p1 <- ggplot(ChLagged, aes(x = year, y = t)) + geom_line()
p2 <- ggplot(ChLagged, aes(x = t, y = t_minus_1)) +
  geom_point() +
  labs(y = "t-1") +
  geom_smooth(method = "lm") + # Add a regression line to the plot
  ggplot2::annotate("text", 8.5, 10, label = paste("r =", round(cor(
    ChLagged$t, ChLagged$t_minus_1
  ), 3))) # Calculate PMCC

# 将两张图并排显示
grid.arrange(p1, p2, nrow = 1)


tmap_leaflet(Jul_1910)