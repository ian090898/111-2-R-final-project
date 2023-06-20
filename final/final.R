library(dplyr)
#三峽房價與居住配置之關聯性分析

file_path <- "input.txt"
# 讀取分號分隔的CSV檔案
data <- read.csv(file_path)

colnames(data) <- c("鄉鎮市區", "交易標的","土地區段位置建物區段門牌","土地移轉總面積平方公尺","都市土地使用分區","非都市土地使用分區","非都市土地使用編定","交易年月日","交易筆棟數","移轉層次","總樓層數","建物型態","主要用途","主要建材","建築完成年月","建物移轉總面積平方公尺",
                                   "建物格局-房","建物格局-廳","建物格局-衛","建物現況格局-隔間","有無管理組織","總價元","單價元平方公尺","車位類別","車位移轉總面積平方公尺","車位總價元","備註","編號","主建物面積","附屬建物面積","陽台面積",
                                   "電梯")

rm = c(28,27,26,25,24,22,21,20,16,14,11,9,8,7,6,5,4,3,1)
data_subset <- data[,-rm]

####保留 交易標的 中有"房地"的資料(列)
string_to_keep <- "房地"# 定义要保留的特定字符串
# 使用grepl()函数筛选出包含特定字符串的行
filtered_data <- data_subset[grepl(string_to_keep,
                                   data_subset$交易標的), ]
####

####移除 主要用途 中有"農地, 其他"的列
strings_to_remove <- c("農地", "其他", NA)  # 要移除的特定复数字符串
# 使用grepl()函数查找包含特定复数字符串的行
rows_to_remove <- grepl(paste(strings_to_remove, collapse = "|"),
                        filtered_data$主要用途)
filtered_data <- filtered_data[!rows_to_remove, ]# 移除包含特定复数字符串的行
####
####sum of area

cols_to_sum <- c("主建物面積","附屬建物面積","陽台面積")  # 加总value1和value2列

# 在新列中显示加总结果
filtered_data$總面積 <- rowSums(filtered_data[, cols_to_sum])

keyword <- "車位"  # 关键字为"like"
# 添加新列并根据关键字设置值
filtered_data$車位 <- ifelse(grepl(keyword, filtered_data$交易標的, ignore.case = TRUE), 1, 0)

keyword_ <- "有"  # 关键字为"like"
# 添加新列并根据关键字设置值
filtered_data$電梯 <- ifelse(grepl(keyword_, filtered_data$電梯, ignore.case = TRUE), 1, 0)
####

rm = c(4,10,11,12)
final_data <- filtered_data[,-rm]


###圖一： 電梯有無對房價的關係###

# 创建 A 资料库和 B 资料库，这里使用两个空列表代替真正的数据库
Have_elevator <- c()#1
No_elevator <- c()#0

# 根据数据的值将其存储到相应的数据库
for (i in 1:length(final_data$電梯)) {
  if (final_data$電梯[i] == 1) {
    Have_elevator <- c(Have_elevator, final_data$單價元平方公尺[i])
  } else if (final_data$電梯[i] == 0) {
    No_elevator <- c(No_elevator, final_data$單價元平方公尺[i])
  }
#  print(i)
}

# 计算数据的密度估计
data_1 <- as.numeric(unlist(Have_elevator))
#class(data_1)
#print(data_1)
data_2 <- as.numeric(unlist(No_elevator))
density1 <- density(data_1)
density2 <- density(data_2)

# 绘制第一个密度图
plot(density1, main = "elevator vs. price", xlab = "price", ylab = "Density", ylim = c(0, max(density1$y, density2$y)), col = "blue")

# 在第一个密度图上添加第二个密度图
lines(density2, col = "red")
###                               ###小結有電梯者房價高


###圖二： 房數對房價的關係(有電梯)###

# 创建 A 资料库和 B 资料库，这里使用两个空列表代替真正的数据库
#room <- list()
room_1 <- c()
room_2 <- c()
room_3 <- c()
room_4 <- c()

# 根据数据的值将其存储到相应的数据库
for (i in 1:length(final_data$`建物格局-房`)) {
  if (final_data$電梯[i] == 1) 
  {
    if (final_data$`建物格局-房`[i] == 1) {
      room_1 <- c(room_1, final_data$單價元平方公尺[i])
    } else if (final_data$`建物格局-房`[i] == 2) {
      room_2 <- c(room_2, final_data$單價元平方公尺[i])
    } else if (final_data$`建物格局-房`[i] == 3) {
      room_3 <- c(room_3, final_data$單價元平方公尺[i])
    } else if (final_data$`建物格局-房`[i] == 4) {
      room_4 <- c(room_4, final_data$單價元平方公尺[i])
    }
  }
  #  print(i)
}

# 计算数据的密度估计
data_1 <- as.numeric(unlist(room_1))
data_2 <- as.numeric(unlist(room_2))
data_3 <- as.numeric(unlist(room_3))
data_4 <- as.numeric(unlist(room_4))
#class(data_1)
#print(data_1)
density1 <- density(data_1)
density2 <- density(data_2)
density3 <- density(data_3)
density4 <- density(data_4)

# 绘制第一个密度图
plot(density1, main = "room(has elevator) vs. price", xlab = "price", ylab = "Density", ylim = c(0, max(density1$y, density2$y)), col = "blue")

# 在第一个密度图上添加第二个密度图
lines(density2, col = "red")
lines(density3, col = "green")
lines(density4, col = "brown")
#blue: room_1
#red:  room_2
#green: room_3
#brown: room_4
###                               


###圖三： 房數對房價的關係(無電梯)###
###!!!room = 1, 2 all have elevator###

# 创建 A 资料库和 B 资料库，这里使用两个空列表代替真正的数据库
#room <- list()
room_1 <- c()
room_2 <- c()
room_3 <- c()
room_4 <- c()

# 根据数据的值将其存储到相应的数据库
for (i in 1:length(final_data$`建物格局-房`)) {
  if (final_data$電梯[i] == 0) 
  {
    if (final_data$`建物格局-房`[i] == 1) {
      room_1 <- c(room_1, final_data$單價元平方公尺[i])
    } else if (final_data$`建物格局-房`[i] == 2) {
      room_2 <- c(room_2, final_data$單價元平方公尺[i])
    } else if (final_data$`建物格局-房`[i] == 3) {
      room_3 <- c(room_3, final_data$單價元平方公尺[i])
    } else if (final_data$`建物格局-房`[i] == 4) {
      room_4 <- c(room_4, final_data$單價元平方公尺[i])
    }
  }
  #  print(i)
}

# 计算数据的密度估计
data_1 <- as.numeric(unlist(room_1))
data_2 <- as.numeric(unlist(room_2))
data_3 <- as.numeric(unlist(room_3))
data_4 <- as.numeric(unlist(room_4))
#class(data_1)
#print(data_1)
density1 <- density(data_1)
density2 <- density(data_2)
density3 <- density(data_3)
density4 <- density(data_4)

# 绘制第一个密度图
plot(density1, main = "room(no elevator) vs. price", xlab = "price", ylab = "Density", ylim = c(0, max(density1$y, density2$y)), col = "blue")

# 在第一个密度图上添加第二个密度图
lines(density2, col = "red")
lines(density3, col = "green")
lines(density4, col = "brown")
#blue: room_1
#red:  room_2
#green: room_3
#brown: room_4
###                               ###小結有電梯者房價高

###圖四  建築種類

# 创建示例数据
data <- final_data$建物型態

# 创建一个新的向量用于存储分类结果
category <- rep(NA, length(data))

# 遍历数据并进行分类
for (i in 1:length(data)) {
  if (grepl("透天厝", data[i])) {
    category[i] <- "A"
  } else if (grepl("華廈", data[i])) {
    category[i] <- "B"
  } else if (grepl("公寓", data[i])) {
    category[i] <- "C"
  } else if (grepl("住宅大樓", data[i])) {
    category[i] <- "D"
  }
}

# 打印分类结果
#print(category)

# 创建 A 资料库和 B 资料库，这里使用两个空列表代替真正的数据库
Price_A <- c()#透天
Year_A <- c()
Price_B <- c()#華廈
Year_B <- c()
Price_C <- c()#公寓
Year_C <- c()
Price_D <- c()#住宅大樓
Year_D <- c()


# 根据数据的值将其存储到相应的数据库
for (i in 1:length(category)) {
  if (category[i] == "A") {
    Price_A <- c(Price_A, final_data$單價元平方公尺[i])
    Year_A <- c(Year_A, final_data$建築完成年月[i] / 10000)
    print(i)
    print("A")
  } else if (category[i] == "B") {
    Price_B <- c(Price_B, final_data$單價元平方公尺[i])
    Year_B <- c(Year_B, final_data$建築完成年月[i] / 10000)
  } else if (category[i] == "C") {
    Price_C <- c(Price_C, final_data$單價元平方公尺[i])
    Year_C <- c(Year_C, final_data$建築完成年月[i] / 10000)
  } else if (category[i] == "D") {
    Price_D <- c(Price_D, final_data$單價元平方公尺[i])
    Year_D <- c(Year_D, final_data$建築完成年月[i] / 10000)
  }
  #  print(i)
}
print(length(Price_A))
print(length(Year_A))
print(Year_A)


# 计算数据的密度估计
data_1 <- as.numeric(unlist(Price_A))
data_2 <- as.numeric(unlist(Price_B))
data_3 <- as.numeric(unlist(Price_C))
data_4 <- as.numeric(unlist(Price_D))
density1 <- density(data_1)
density2 <- density(data_2)
density3 <- density(data_3)
density4 <- density(data_4)

# 绘制第一个密度图
plot(density1, main = "", xlab = "price", ylab = "Density", ylim = c(0, max(density1$y, density2$y)), col = "blue")

# 在第一个密度图上添加第二个密度图
lines(density2, col = "red")
lines(density3, col = "green")
lines(density4, col = "brown")
#透天 blue
#華廈red
#公寓green
#住宅大樓brown

######

###圖五  不同建物 建成時間vs.price ###

# 创建示例数据

x <- Year_A
y <- Price_A

# 绘制散点图并禁用坐标轴显示
plot(x, y, main = "Scatter Plot", xlab = "year_A", ylab = "price_A")#, axes = FALSE)

x <- Year_B
y <- Price_B
plot(x, y, main = "Scatter Plot", xlab = "year_B", ylab = "price_B")#, axes = FALSE)

x <- Year_C
y <- Price_C
plot(x, y, main = "Scatter Plot", xlab = "year_C", ylab = "price_C")#, axes = FALSE)

x <- Year_D
y <- Price_D
plot(x, y, main = "Scatter Plot", xlab = "year_D", ylab = "price_D")#, axes = FALSE)

#############






