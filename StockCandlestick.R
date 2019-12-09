#install.packages('tidyverse', repos="http://mirrors.tuna.tsinghua.edu.cn/CRAN/")
#install.packages("gridExtra")
#install.packages("quantmod")
#install.packages("zoo")
#install.packages("dplyr")

stockplot <- function(x){

fulldata <- read.csv("DRESSTK.csv", header=TRUE)
data<-fulldata[fulldata$Stkcd == x,3:8]
data<- na.omit(data)
class(data)
data$date<-as.Date(data$date)
class(data$date)

# 为了解决日期作为横坐标会出现休市日的情况，需要以下的变量辅助
row_len <- nrow(data)
breaks <- seq(1, row_len, 30)
labels <- data$date[breaks]

# 作K线图
p1 <- data %>%
  arrange(data$date) %>%
  mutate(ma20 = SMA(close, n = 20, align =  "right"),
         ma50 = SMA(close, n = 50, align = "right"),
         ma200 = SMA(close, n = 200, align = "right"),
         date_axis = row_number()) %>%
  ggplot(aes(x = date_axis)) +
  geom_boxplot(aes(lower = pmin(close, open),
                   middle = close,
                   upper = pmax(close, open),
                   ymin = low,
                   ymax = high,
                   group = date_axis,
                   fill = open > close),
               stat = "identity",
               show.legend = FALSE) +
  geom_line(aes(y = ma20), color = "red") +
  geom_line(aes(y = ma50), color = "yellow") +
  geom_line(aes(y = ma200), color = "blue3") +
  scale_x_continuous(breaks = breaks,
                     labels = NULL,
                     expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(margin = margin(l = 8)))

# 作成交量图
p2 <- data %>%
  arrange(date) %>%
  mutate(vol_ma5 = SMA(volume, n = 5, align =  "right"),
         vol_ma10 = SMA(volume, n = 10, align = "right"),
         date_axis = row_number()) %>%
  ggplot(aes(x = date_axis, y = volume)) +
  geom_bar(stat = "identity",
           aes(fill = open > close),
           show.legend = FALSE) +
  geom_line(aes(y = vol_ma5), color = "blue3") +
  geom_line(aes(y = vol_ma10), color = "red") +
  scale_x_continuous(breaks = breaks,
                     labels = format(labels, "%Y-%m"),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(axis.title = element_blank())

# 组合
grid.arrange(p1, p2, nrow = 2, heights = 2:1)
}
