sortMerged = function(df){
    seq = order(df[,1])
    price = unique(sort(df[,1]))
    sales = rep(0,length(price))
    j = 1
    for(i in 1 : length(seq)){
        num = df[seq[i],1]
        if(num != price[j]){ j = j + 1}
        sales[j] = sales[j] + df[seq[i],2] 
    }
    df = data.frame(
        价格 = price,
        销量 = sales
    )
    return (df)
}

theme_set(theme(
    plot.background = element_rect(colour = "black", size = 3, linetype = 4, fill = "lightblue"), 
    plot.title = element_text(colour = "black", face = "bold", size = 30, vjust = 1), 
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"
))