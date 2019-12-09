
Barchart1 <- function(){
##### figure1
#上涨、不变、下降的股票比例
data1 <- read.csv('bar_figure1.csv',header=T)

data1$trend = factor(data1$trend, levels=c('declining','stay','advancing'))

ggplot(data1,aes(' ',stock,fill=trend))+
  geom_bar(stat="identity",position="stack",width = 0.05)+ #stack:堆积柱状图，width:柱状图宽度
  theme_bw()+
  #scale_fill_brewer(palette='Set1')+ #柱状图颜色，使用RColorBrewer
  scale_fill_manual(values=c("#FF6666","#CCCCCC","#66CC33"))+
  guides(fill=guide_legend(title=NULL))+ #不显示标题
  guides(fill=F)  + #不显示图例
  #不显示各种图标
  theme(panel.grid = element_blank(),panel.border = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank(),
        text = element_blank())+
        #显示数字
        geom_text(aes(y=position,label=paste(stock,'%',sep = '')),size=3,col='white')+
        geom_text(aes(y=position_num,label=number),size=3,
                  col=c('#66CC33','','#FF6666'),
                  vjust=-1.5)+
        geom_text(aes(y=position_trend,label=trend1),size=3,col='black',vjust=-1.5)+
  coord_flip() #将图形横纵坐标互换
}


Barchart2<-function(){
#### figure2
#总发行创下新高/新低的股票比例
data2 <- read.csv('bar_figure2.csv',header=T)
data2$trend = factor(data2$trend, levels=c('low','high'))

ggplot(data2,aes(' ',stock,fill=trend))+
  geom_bar(stat="identity",position="stack",width = 0.05)+ #stack:堆积柱状图，width:柱状图宽度
  theme_bw()+
  scale_fill_manual(values=c("#FF6666","#66CC33"))+
  guides(fill=guide_legend(title=NULL))+ #不显示标题
  guides(fill=F)  + #不显示图例
  #不显示各种图标
  theme(panel.grid = element_blank(),panel.border = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank(),
        text = element_blank())+
  #显示数字
  geom_text(aes(y=position,label=paste(stock,'%',sep = '')),size=3,col='white')+
  geom_text(aes(y=position_num,label=number),size=3,
            col=c('#66CC33','#FF6666'),
            vjust=-1.5)+
  geom_text(aes(y=position_trend,label=trend1),size=3,col='black',vjust=-1.5)+
  coord_flip() #将图形横纵坐标互换
}

Barchart3<-function(){
#### figure3
#日振幅高于/低于60天移动平均的股票数比例
data3 <- read.csv('bar_figure3.csv',header=T)
data3$trend = factor(data3$trend, levels=c('below','above'))

ggplot(data3,aes(' ',stock,fill=trend))+
  geom_bar(stat="identity",position="stack",width = 0.05)+ #stack:堆积柱状图，width:柱状图宽度
  theme_bw()+
  scale_fill_manual(values=c("#FF6666","#66CC33"))+
  guides(fill=guide_legend(title=NULL))+ #不显示标题
  guides(fill=F)  + #不显示图例
  #不显示各种图标
  theme(panel.grid = element_blank(),panel.border = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank(),
        text = element_blank())+
  #显示数字
  geom_text(aes(y=position,label=paste(stock,'%',sep = '')),size=3,col='white')+
  geom_text(aes(y=position_num,label=number),size=3,
            col=c('#66CC33','#FF6666'),
            vjust=-1.5)+
  geom_text(aes(y=position_trend,label=trend1),size=3,col='black',vjust=-1.5)+
  coord_flip() #将图形横纵坐标互换
}


Barchart4<-function(){
#### figure4
#日振幅高于/低于120天移动平均的股票数比例
data4 <- read.csv('bar_figure4.csv',header=T)
data3$trend = factor(data4$trend, levels=c('below','above'))

ggplot(data4,aes(' ',stock,fill=trend))+
  geom_bar(stat="identity",position="stack",width = 0.05)+ #stack:堆积柱状图，width:柱状图宽度
  theme_bw()+
  scale_fill_manual(values=c("#FF6666","#66CC33"))+
  guides(fill=guide_legend(title=NULL))+ #不显示标题
  guides(fill=F)  + #不显示图例
  #不显示各种图标
  theme(panel.grid = element_blank(),panel.border = element_blank(),
        axis.text = element_blank(),axis.ticks = element_blank(),
        text = element_blank())+
  #显示数字
  geom_text(aes(y=position,label=paste(stock,'%',sep = '')),size=3,col='white')+
  geom_text(aes(y=position_num,label=number),size=3,
            col=c('#66CC33','#FF6666'),
            vjust=-1.5)+
  geom_text(aes(y=position_trend,label=trend1),size=3,col='black',vjust=-1.5)+
  coord_flip() #将图形横纵坐标互换
}




