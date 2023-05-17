colnames(csv_page_0_size_100)
colnames(csv_page_0_size_100) <- c("鄉鎮市區", "交易標的","土地區段位置建物區段門牌","土地移轉總面積平方公尺","都市土地使用分區","非都市土地使用分區","非都市土地使用編定","交易年月日","交易筆棟數","移轉層次","總樓層數","建物型態","主要用途","主要建材","建築完成年月","建物移轉總面積平方公尺",
                                   "建物現況格局-房","建物現況格局-廳","建物現況格局-衛","建物現況格局-隔間","有無管理組織","總價元","單價元平方公尺","車位類別","車位移轉總面積平方公尺","車位總價元","備註","編號","主建物面積","附屬建物面積","陽台面積",
                                  "電梯")
colnames(csv_page_0_size_100)

# 创建一个示例数据框
data <- data.frame(column1 = c(1, 2, 3),
                   column2 = c("A", "B", "C"),
                   column3 = c(TRUE, FALSE, TRUE))

# 删除列2（column2）
data$column2 <- NULL

# 查看更新后的数据框
print(data)

csv_page_0_size_100$主要建材<-NULL
View(csv_page_0_size_100)
csv_page_0_size_100$編號<-NULL
View(csv_page_0_size_100)
