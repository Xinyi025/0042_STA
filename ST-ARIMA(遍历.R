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
road_nodes <- read_delim("PeMS/nodes.txt", delim = "\t",na = "",col_names = c("nodeID","longitude","latitude"))
road_edges <- read_delim("PeMS/edges.txt", delim = "\t",na = "",col_names = c("edgeID","start_node","end_node","distance"))

# 查看前几行数据
head(road_nodes)


# 为减少计算量，提升系统内存，提取一定范围内的路网数据和station数据：
library(sf)


# 将unique_stations数据框转换为简单要素集（sf）对象
point_data <- st_as_sf(stations, coords = c("longitude", "latitude"), crs = 4326)

# 创建表示矩形边界的简单要素集（sf）对象
polygon_wkt <- "POLYGON((-117.5790472696296689 34.0396970074302061, -117.5790472696296689 34.10093960379779077, -117.50803961360503536 34.10093960379779077, -117.50803961360503536 34.0396970074302061, -117.5790472696296689 34.0396970074302061))"
polygon_sf <- st_as_sf(st_as_sfc(polygon_wkt), crs = 4326)

# 使用st_intersects()函数找到位于矩形内的站点的逻辑向量
stations_within_polygon_logical <- apply(st_intersects(point_data, polygon_sf), 1, any)

# 使用逻辑向量筛选出位于矩形内的站点
stations_within_polygon <- point_data[stations_within_polygon_logical, ]

# 将筛选出的站点转换回数据框并保留geometry和station属性
filtered_stations <- as.data.frame(stations_within_polygon[c("geometry", "station")])

# 提取经纬度
filtered_stations <- filtered_stations %>%
  mutate(longitude = st_coordinates(geometry)[, 1],
         latitude = st_coordinates(geometry)[, 2])

# 移除geometry列
filtered_stations <- filtered_stations[, -1]



filtered_stations$longitude <- as.numeric(filtered_stations$longitude)
filtered_stations$latitude <- as.numeric(filtered_stations$latitude)
# 将站点数据添加到地图上
map1 <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = mean(stations$longitude),
          lat = mean(stations$latitude),
          zoom = 10)

map1 <- map1 %>%
  addCircleMarkers(data = filtered_stations,
                   lng = ~longitude,
                   lat = ~latitude,
                   color = "red",
                   radius = 5) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = polygon_sf, fillOpacity = 0.2, fillColor = "blue")
# 显示地图
map1



# 按同样的方法筛选出位于矩形内的路网节点nodes
nodes_data <- st_as_sf(road_nodes, coords = c("longitude", "latitude"), crs = 4326)
nodes_within_polygon_logical <- apply(st_intersects(nodes_data, polygon_sf), 1, any)
nodes_within_polygon <- nodes_data[nodes_within_polygon_logical, ]
filtered_nodes <- as.data.frame(nodes_within_polygon[c("geometry", "nodeID")])

# 提取经纬度
filtered_nodes <- filtered_nodes %>%
  mutate(longitude = st_coordinates(geometry)[, 1],
         latitude = st_coordinates(geometry)[, 2])

# 移除geometry列
filtered_nodes <- filtered_nodes[, -1]


filtered_nodes$longitude <- as.numeric(filtered_nodes$longitude)
filtered_nodes$latitude <- as.numeric(filtered_nodes$latitude)
# 将站点数据添加到地图上
map <- map %>%
  addCircleMarkers(data = filtered_nodes,
                   lng = ~longitude,
                   lat = ~latitude,
                   color = "blue",
                   radius = 5)
# 显示地图
map



# 计算基于路网的station之间的距离
# 首先计算离station最近的路网节点：
filtered_stations_sf <- st_as_sf(filtered_stations, coords = c("longitude", "latitude"), crs = 4326)
road_nodes_sf <- st_as_sf(filtered_nodes, coords = c("longitude", "latitude"), crs = 4326)
# 计算每个节点与最近的路网节点的距离矩阵
distance_matrix <- st_distance(filtered_stations_sf, road_nodes_sf)
# 找到每个站点最近的路网节点索引
nearest_node_indices <- apply(distance_matrix, 1, which.min)
# 使用这些索引为station添加最近的路网节点ID
filtered_stations$nearest_node <- road_nodes$nodeID[nearest_node_indices]


# 创建图形对象
library(igraph)
road_edges <- road_edges[, c("start_node", "end_node", "distance", "edgeID")]
graph <- graph_from_data_frame(d = road_edges, vertices = road_nodes, directed = FALSE)

# 为边缘分配权重
E(graph)$weight <- road_edges$distance

# 为图形中的顶点分配ID
V(graph)$name <- V(graph)
V(graph)$name

# 在stations数据框中为每个站点添加一个顶点ID
filtered_stations$vertex_id <- match(filtered_stations$nearest_node, V(graph)$name)

# 使用Dijkstra算法计算各个站点之间的最短路径距离矩阵
shortest_path_matrix <- distances(graph, v = unique(filtered_stations$vertex_id), to = unique(filtered_stations$vertex_id), weights = E(graph)$weight)
dim(shortest_path_matrix)
print(colnames(shortest_path_matrix))
print(rownames(shortest_path_matrix))

write.csv(shortest_path_matrix, "shortest_path_mtx.csv")

# 用节点之间的最短距离代表station之间的最短距离
# 创建一个 n x n 的零矩阵

n  <- as.numeric(nrow(filtered_stations))
station_distance_matrix <- matrix(0, nrow = n, ncol = n)
rownames(station_distance_matrix) <- filtered_stations$station
colnames(station_distance_matrix) <- filtered_stations$station

# 使用 for 循环填充矩阵
for (i in 1:n) {
  for (j in 1:n) {
    start_node <- filtered_stations$nearest_node[i]
    end_node <- filtered_stations$nearest_node[j]
    distance <- shortest_path_matrix[as.character(start_node), as.character(end_node)]
    if (i == j) {
      distance <- 0
    } else {
      if (distance == 0) {
        distance <- 0.0001
      }
    }
    station_distance_matrix[i, j] <- distance
  }
}
as.matrix(station_distance_matrix)


write.csv(station_distance_matrix, "station_mtx.csv")

# 以station_distance_matrix作为距离矩阵，应用反距离权重法得到空间权重矩阵
library(spdep)
# 将距离矩阵转换为距离对象
# 构建反距离权重矩阵

weights <- 1/station_distance_matrix
weights[!is.finite(weights)] <- 0
weights


# 行标准化
row_sums <- apply(weights, 1, sum)

weights_norm <- t(t(weights) / row_sums)

write.csv(weights_norm, "weights.csv")

# 将权重矩阵标准化
weights_list <- mat2listw(weights_norm, style = "W")


# 合并两个数据框
merged_df <- merge(data, filtered_stations, by.x = "Station", by.y = "station")
merged_df

# 保留需要的列
filtered_df <- merged_df[, c("Station", "TimeStamp", "Flow")]
filtered_df

# 检查是否只保filtered_df留了矩形框内的station
all(filtered_df$Station %in% filtered_stations$station)

# 检查filtered_df中是否有缺失值
missing_values <- is.na(filtered_df)
missing_values_count <- colSums(missing_values)
missing_values_count




# 建立 ST-ARIMA 模型
library(stlplus)
library(forecast)
source("starima_package.R")
library(reshape2)





library(xts)
# 将数据从长格式转换为宽格式
wide_df <- dcast(filtered_df, TimeStamp ~ Station, value.var = "Flow")
# 创建时间序列数据
wide_df$TimeStamp <- as.POSIXct(wide_df$TimeStamp,format = "%m/%d/%Y %H:%M:%S")
ts_data_multivar <- xts(wide_df[, -1], order.by = wide_df$TimeStamp)


# 将 2023 年 1 月 22 日 之前的数据作为训练集，其他数据作为测试集
train_data <- ts_data_multivar[1:(24*25),]
test_data <- ts_data_multivar[(24*25+1):(24*31),]


library(vars)
library(tseries)

predictions_list <- list()

# 遍历每个空间单元
for (i in 1:ncol(train_data)) {
  # 提取单变量时间序列
  single_ts <- ts(train_data[, i], frequency = 24)
  
  # 对单变量时间序列进行ARIMA建模
  arima_model <- auto.arima(single_ts)
  
  # 预测
  n_periods <- 24*6 # 预测期数
  arima_forecast <- forecast(arima_model, h = n_periods)
  
  # 将预测结果存储到列表中
  predictions_list[[i]] <- arima_forecast$mean
  print(i)
  print(arima_forecast$method)
}

# 将预测结果整合为矩阵
predictions_matrix <- do.call(cbind, predictions_list)


# 将空间自回归模型应用于预测结果
st_arima_forecast <- matrix(0, nrow = n_periods, ncol = 1)
for (t in 1:n_periods) {
  st_arima_forecast[t, 1] <- sum(weights_list[["weights"]] * predictions_matrix[t, ])
}


# 对比预测结果与实际结果

# 提取列名为"817198"的数据
st_arima_forecast_817198 <- as.data.frame(st_arima_forecast)
test_data_817198 <- as.data.frame(test_data[,target_unit])

# 将列名统一为"Value"
colnames(st_arima_forecast_817198) <- "Value"
colnames(test_data_817198) <- "Value"

# 使用rbind将两个数据框堆叠在一起
combined_data <- rbind(test_data_817198, st_arima_forecast_817198)

# 为数据框添加时间戳和来源列
comparison_data <- data.frame(
  Timestamp = rep(timestamps, 2),
  Value = combined_data$'Value',
  Source = c(rep("Actual", length(test_data_817198)), 
             rep("Forecast", length(st_arima_forecast_817198)))
)

# 绘制折线图
ggplot(comparison_data, aes(x = Timestamp, y = Value, color = Source)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Comparison of Actual and Forecasted Traffic Volume for Station 817198",
       x = "Date",
       y = "Traffic Volume") +
  theme(legend.title = element_blank())
