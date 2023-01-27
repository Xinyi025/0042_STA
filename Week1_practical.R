# Week1 ESTDA
temp <- read.csv("Data/Temp_China.csv")

# conames():define the name of each colomn, and [4:ncol(temp)] means from 4th to the last one
colnames(temp)[4:ncol(temp)] <- as.character(c(1951:2002))

# Usage:paste("a character string you want to concatenate", a number you want to concatenate with the character, separator)
station <- paste("sta", 1:nrow(temp), sep = "")
station


# define the colname derectly, but create the new column of station name and combine the new column and 'temp' table
temp <- cbind(station, temp)
temp

# convert temp dataset to matrix
temp_matrix <- data.matrix(temp[, 5:ncol(temp)])



#  explore their general characteristics: the mean and the standard deviation
mu = mean(temp_matrix)
mu

# standard deviation
sdev = sd(temp_matrix)
sdev


# histogram:
hist(temp_matrix)
abline(v = mu, col = "red")

# QQ plot
qqnorm(temp_matrix)
qqline(temp_matrix, col = "red")

# pairs(): a matrix of scatterplots is produces,
# pairs(formula, data = NULL, ..., subset, na.action = stats::na.pass)
#
pairs( ~ LOG + LAT + ALT + rowMeans(temp_matrix),
       data = temp,
       main = "Simple Scatterplot Matrix")



# install if necessary
library(scatterplot3d)
library(plot3D)
library(rgl)


#1. Using Scatterplot3d
scatterplot3d(x = temp$LAT,
              y = temp$ALT,
              z = rowMeans(temp_matrix))

#2. Using plot3D (x,y and z are first three variables so no need to # explicitly define them).
scatter3D(temp$LAT, temp$ALT, rowMeans(temp_matrix))


# Need to see if this can be interactive in web-version
plot3d(temp$LAT, temp$ALT, rowMeans(temp_matrix))


# 1.3 Examining temporal characteristics
plot(
  colMeans(temp_matrix),
  xlab = "Year",
  ylab = "Temperature",
  type = "l",
  xaxt = "n"
)
axis(1, at = seq(9, 49, 10), labels = seq(1960, 2000, 10))

library(reshape)
# reshape::melt 拆分原数据表，将每一年 每一个站都拆分为一列。id列为1：4列，每一个站的站名、坐标；
# measure.vars 是第5列到最后一列(ncol(data))。
# 还可以设置参数na.rm, 移除空值
newtemp <- melt(temp,
                id.vars = 1:4,
                measure.vars = 5:ncol(temp))
head(newtemp)


# change the colomn name
colnames(newtemp)[5:6] <- c("year", "temperature")


library(lattice)
# use .chosen to extract specific data values from a large data frame or matrix.
station.chosen = c("sta1",
                   "sta2",
                   "sta3",
                   "sta4",
                   "sta5",
                   "sta6",
                   "sta7",
                   "sta8",
                   "sta9",
                   "sta10")

#Create a variable containing just the selected stations, using %in%
a <- newtemp[station %in% station.chosen, ]

# create the scatterplot:
xyplot(
  # 因变量~自变量 | 按station分开
  temperature ~ year | station,
  xlab = "year",
  type = "l",
  # colomn and row numbers of scatterplot
  layout = c(5, 2),
  data = a,
  main = "Temperature in China"
)


#Create the heatmap:
heatmap(
  temp_matrix,
  Rowv = NA,
  Colv = NA,
  col = heat.colors(256),
  scale = "column",
  margins = c(5, 3),
  xlab = "Year",
  ylab = "Station ID",
  cexCol = 1.1,
  y.scale.components.subticks(n = 10)
)


# create a heat map order by altitude
temp_order<-temp[order(temp$ALT, decreasing=TRUE),]
temp_ordermatrix<-data.matrix(temp_order[,5:ncol(temp)])

heatmap(temp_ordermatrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column",margins=c(3,3))

# considering about the latitude
levelplot(t(temp_ordermatrix), aspect="fill",)



# 1.4 Examining spatial characteristics
library(ggplot2)
library(OpenStreetMap)
library(raster)

# Note, you need to have the correct Java version (32 or 64 bit) installed for the following to work
#Get the data from the final year (2002):
data_last <- cbind(temp[1:4], temp$"2002")

data_last

# Change the column name:
colnames(data_last)[5] <- "tempvalue"

data_last
# Make a proportional symbol of the latest data. Convert latitude and longitude to Mercator projection:
data_last[, 2:3] <- projectMercator(data_last$LAT, data_last$LOG)
# Download a map tile:
map <- openmap(c(53.5, 73.6), c(15.7, 134.7), type = 'osm')


autoplot.OpenStreetMap(map) + geom_point (data = data_last, aes(
  x = LOG,
  y = LAT,
  color = tempvalue,
  size = tempvalue
)) + ggtitle("Annual Average Temperature in China, 2002")

hist(data_last$ALT, 20)

hist(log(data_last$ALT), 20)

autoplot.OpenStreetMap(map) +
  geom_point(data = data_last, aes(
    x = LOG,
    y = LAT,
    color = tempvalue,
    size = log(ALT)
  )) + ggtitle("Annual Average Temperature in China, 2002") +
  scale_colour_gradient2(low = "blue", high = "red")

hist(rowMeans(temp_matrix), 20)
temp_matrix


high_temp <- which(rowMeans(temp_matrix) > 15)
data_last <- cbind(data_last, "Low", stringsAsFactors = FALSE)
colnames(data_last)[ncol(data_last)] <- "TempGroup"
data_last[high_temp, "TempGroup"] <- "High"

autoplot.OpenStreetMap(map) + geom_point(data = data_last, aes(
  x = LOG,
  y = LAT,
  color = TempGroup,
  size = log(ALT)
)) + ggtitle("Stations with High/Low means")


newtemp[,2:3] <- projectMercator(newtemp$LAT, newtemp$LOG)
# Select years from 1991 onwards

years <- which(as.numeric(as.character(newtemp$year))>1990)
autoplot.OpenStreetMap(map) + geom_point(data=newtemp[years,], aes(x=LOG,y=LAT, color=temperature, size=log(ALT))) + facet_wrap(facets=~year) +
  scale_colour_gradient2(low="blue", high="red")

# Calculate difference in temperatures from year to year (first year is zero as there is no preceding year)
tempdiff <- cbind(0, temp[,6:ncol(temp)]-temp[,5:(ncol(temp)-1)])
newtemp <- cbind(newtemp, unlist(tempdiff))
colnames(newtemp)[ncol(newtemp)] <- "tempdiff"

autoplot.OpenStreetMap(map) + geom_point(data=newtemp[years,], aes(x=LOG,y=LAT, color=tempdiff, size=log(ALT))) + scale_colour_gradient2(low="blue", mid='white', high="red") + facet_wrap(facets=~year)





