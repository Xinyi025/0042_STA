library(readr)

# 读取txt.gz文件
compressed_file <- "PeMS/d08_text_station_hour_2023_01.txt.gz"
lines <- readr::read_lines(gzfile(compressed_file))

# 将字符向量连接成一个以换行符分隔的字符串
lines_text <- paste(lines, collapse = "\n")

# 使用read_csv()函数将字符串转换为dataframe
dataframe <- read_csv(lines_text, col_names = c("TimeStamp", 
                                                "Station", 
                                                "District",
                                                "Route",
                                                "Direction",
                                                "LaneType",
                                                "StationLength",
                                                "Samples",
                                                "Observed",
                                                "Flow",
                                                "AvgOccupancy",
                                                "AvgSpeed",
                                                "Delay (V_t=35)",
                                                "Delay (V_t=40)",
                                                "Delay (V_t=45)",
                                                "Delay (V_t=50)",
                                                "Delay (V_t=55)",
                                                "Delay (V_t=60)",
                                                "Lane N Flow",
                                                "Lane N Avg Occ",
                                                "Lane N Avg Speed"
))

# 查看前几行数据
dataframe <- dataframe[dataframe$LaneType == "ML",]
head(dataframe)

# 读取metadata数据为dataframe
metadata <- read_delim("PeMS/d08_text_meta_2023_01_01.txt", delim = "\t", na = "")
metadata_cor <- metadata[, c("ID", "Longitude", "Latitude")]

# 左连接（left join）dataframe和metadata
result <- merge(dataframe, metadata_cor, by.x = "Station", by.y = "ID", all.x = TRUE)

# 查看前几行数据
head(result)

# 筛选数据
data  <- result[, c("Station","TimeStamp","Flow","Route","Direction","LaneType","Longitude","Latitude")]

# 查找data中的缺失值
missing_values <- is.na(data)

# 计算每列的缺失值数量
missing_values_count <- colSums(missing_values)

# 显示每列的缺失值数量
missing_values_count



# ST-ARIMA模型
library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)

# 保留唯一的站点ID和对应的经纬度
stations <- distinct(data, Station, Longitude, Latitude)
colnames(stations) <- c("station","longitude","latitude")

# 查看前几行数据
head(stations)


# 创建一个leaflet地图对象，添加OpenStreetMap底图
stations$longitude <- as.numeric(stations$longitude)
stations$latitude <- as.numeric(stations$latitude)

map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = mean(stations$longitude),
          lat = mean(stations$latitude),
          zoom = 10)

# 将站点数据添加到地图上
map <- map %>%
  addCircleMarkers(data = stations,
                   lng = ~longitude,
                   lat = ~latitude,
                   color = "red",
                   radius = 5)
# 显示地图
map

write.csv(stations, "stations.csv", row.names = FALSE)



# 读取路网数据
road_nodes <- read_delim("PeMS/nodes.txt", delim = " ",na = "",col_names = c("nodeID","longitude","latitude"))
road_edges <- read_delim("PeMS/edges.txt", delim = " ",na = "",col_names = c("edgeID","start_node","end_node","distance"))

# 查看前几行数据
head(road_nodes)

