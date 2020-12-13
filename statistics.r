library(ggplot2)
library(gt)

#图表主题配置
myColor = "#FF8C73"
theme_set(theme(
    #plot.background = element_rect(colour = "black", size = 3, linetype = 4, fill = "lightblue"), 
    plot.title = element_text(colour = "black", face = "bold", size = 15, vjust = 6),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
))

#读取数据
data = read.csv("data.csv",encoding="UTF-8")
print(summary(data))
#若干函数的实现

#将数据框按销量排序
sortQuantity = function(df){ 
    seq = rev(order(df[,2]))
    price = df[seq,1]
    sales = df[seq,2]
    df = data.frame(
        价格 = price,
        销量 = sales
    )
    return (df)
}

#加权平均数
weightedMean = function(x,f){
    s = x*f
    return (sum(s) / sum(f))
}

#标准分
sc = function(x){
    return ((x - mean(x)) / sd(x))
}

#标准差系数
cv = function(x){
    return (sd(x) / mean(x))
}

#统计表数据
c8 =c(0,0,0,0,0,0,0,0)
tableData = data.frame(
    产品种类数目 = c8,
    均价 = c8,
    月销售总量 = c8,
    月销售总额 = c8,
    均价以上产品销售额占比 = c8,
    销量前三产品销售额占比 = c8,
    产品价格的标准差系数 = c8,
    销量最高产品价格的标准分 = c8
)
name = noquote(unique(data$店名))

for(i in 1 : length(name)){
    df = data.frame(
        价格 = subset(data, 店名 == name[i])$价格,
        销量 = subset(data, 店名 == name[i])$销量
        )
    df = sortQuantity(df)
    prices = df$价格
    quantity = df$销量
    
    #产品种类数目
    totalQuantity = length(df[[1]])
    #均价
    averagePrice = weightedMean(prices, quantity)
    #月销售总量
    totalSalesVolume = sum(quantity)
    #月销售总额
    totalSales = sum(prices * quantity)
    #均价以上产品销售额占比
    proportionAboveAveragePrice = sum(subset(df,价格>=averagePrice)$价格 * subset(df, 价格 >= averagePrice)$销量) / totalSales
    #销量前三产品销售额占比
    proportionTop3 = sum(prices[1:3] * quantity[1:3]) / totalSales
    #产品价格的标准差系数
    CV = cv(prices)
    #销量最高产品价格的标准分
    SC = sc(prices)[1]

    row = c(totalQuantity, averagePrice, totalSalesVolume, totalSales, proportionAboveAveragePrice, proportionTop3, CV, SC)
    tableData[i,] = row
}
row.names(tableData) = name

#绘制统计表
table = 
tableData[order(-tableData$月销售总额),] %>%
gt(
    rownames_to_stub = TRUE
) %>%
tab_header(
    title = "商品数据",
    subtitle = "距离复旦大学1.5km以内的奶茶店"
)  %>%
tab_stubhead("商家") %>%
cols_align(
    "center"
) %>%
fmt_number(
    columns = vars(均价),
    decimals = 1
) %>%
fmt_number(
    columns = contains("标准"),
    decimals = 3
) %>%
fmt_number(
    columns = contains("月销"),
    drop_trailing_zeros = TRUE,
    sep_mark = ","
) %>%
fmt_percent(
    columns = contains("占比"),
    decimals = 1
) 

#统计图部分

#将初始数据按价格区间分组
grouped = data.frame(
    type = c("~10","10-14","15-19","20-24","25-29","30~"),
    price = c(
        weightedMean(subset(data, 价格 < 10)$价格, subset(data, 价格 < 10)$销量), 
        weightedMean(subset(data, 价格 >= 10 & 价格 < 15)$价格, subset(data, 价格 >= 10 & 价格 < 15)$销量), 
        weightedMean(subset(data, 价格 >= 15 & 价格 < 20)$价格, subset(data, 价格 >= 15 & 价格 < 20)$销量), 
        weightedMean(subset(data, 价格 >= 20 & 价格 < 25)$价格, subset(data, 价格 >= 20 & 价格 < 25)$销量),
        weightedMean(subset(data, 价格 >= 25 & 价格 < 30)$价格, subset(data, 价格 >= 25 & 价格 < 30)$销量),
        weightedMean(subset(data, 价格 > 30)$价格, subset(data, 价格 > 30)$销量)
    ),
    quantity = c(
        sum(subset(data, 价格 < 10)$销量), 
        sum(subset(data, 价格 >= 10 & 价格 < 15)$销量), 
        sum(subset(data, 价格 >= 15 & 价格 < 20)$销量), 
        sum(subset(data, 价格 >= 20 & 价格 < 25)$销量), 
        sum(subset(data, 价格 >= 25 & 价格 < 30)$销量), 
        sum(subset(data, 价格 > 30)$销量)
    )
)

# 不同价格区间的产品销量 
quantityDistributionData = data.frame(
    data = grouped$quantity,
    type = grouped$type
)

quantityDistribution =
ggplot(
    data = quantityDistributionData, 
    mapping = aes(x = type, y = data)
) +
geom_col(fill=myColor) +
labs(
    x = "价格区间(元)",
    y = "销量(件)",
    title = "不同价格区间的产品销量"
) +
scale_y_continuous(labels = scales::comma)

# 不同价格区间的产品销售额 
salesDistributionData = data.frame(
    data = grouped$price * grouped$quantity,
    type = c("~10","10-14","15-19","20-24","25-29","30~")
)

salesDistribution =
ggplot(
    data = salesDistributionData, 
    mapping = aes(x = type, y = data)
) +
geom_col(fill=myColor) +
labs(
    x = "价格区间(元)",
    y = "销售额(元)",
    title = "不同价格区间的产品销售额"
) +
scale_y_continuous(labels = scales::comma)

#不同均价区间的商家分布 

#print(tableData[order(tableData$均价),])
shopDistributionData = data.frame(
    data = c(0,6,9,7,3,0),
    type = c("~10","10-14","15-19","20-24","25-29","30~")
)
myLabel = paste( #图例显示百分比
    shopDistributionData$type, "  (",
    shopDistributionData$data / sum(shopDistributionData$data) * 100,
    "%)", sep=""
)

shopDistribution =
ggplot(
    data = shopDistributionData, 
    mapping = aes(
        x = 'Content', 
        y = data, 
        fill = type
    )
) + 
geom_bar(stat = 'identity', position = 'stack') +
coord_polar(theta = 'y') +
labs(x="", y="", title="不同均价区间的商家分布") +
theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.title = element_blank()
)+
scale_fill_discrete(
    breaks = shopDistributionData$type, 
    labels = myLabel
)   

#图表输出
ggsave("不同价格区间的产品销量.png", quantityDistribution)
ggsave("不同价格区间的产品销售额.png", salesDistribution)
ggsave("不同均价区间的商家分布.png", shopDistribution)
print(table)
