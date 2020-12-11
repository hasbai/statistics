library(ggplot2)
library(gapminder)
library(gt)

gt(data=gapminder)

p = ggplot(
    data = gapminder, 
    mapping = aes(
        x = gdpPercap,
        y = lifeExp,
        color = continent,
        fill = continent
    )
)

figure = p + 
    geom_point() + 
    geom_smooth(method="lm") + 
    scale_x_log10(labels=scales::dollar) +
    labs(
        x = "人均GDP",
        y = "期望寿命（年数）",
        title = "经济增长与期望寿命",
        subtitle = "数据点为每个国家每年",
        caption = "数据来源: gapminder"  
        ) 
    

ggsave("1.png")

