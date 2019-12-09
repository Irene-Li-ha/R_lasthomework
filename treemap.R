#install.packages("treemap")
library(treemap)
#install.packages("treemapify")
library(treemapify)
library(RColorBrewer)
library(ggplot2)
library("xlsx")

############################################################################################
#全行业树状图
TreeMap1<-function(){
        data <- read.xlsx("C.xlsx",1,encoding = "UTF-8")
        data$stcok<- as.integer(data$stcok)
        data$color <- rainbow(nlevels(data$stock))[data$stock] #更改颜色
        color <- c( "#9E3649", "#FF0017","#008B4F", "#00AE51")
        
        palette.HCL.options <- list(hue_start=270, hue_end=360+150)
        
        treemap(data,
                index=c("class", "stock1"),
                vSize="market_cap",
                vColor="percent",
                type="value",
                #palette.HCL.options=palette.HCL.options, #设置颜色
                palette=(values=color),
                #palette="-RdGy",
                format.legend = list(scientific = FALSE, big.mark = " "),
                fontsize.title=0, #去掉标题
                fontsize.legend=0, #去掉图例
                fontcolor.labels="white",fontface.labels=2,fontsize.labels = c(12,20), #标签颜色，加粗
                #fontfamily.labels="STSong", #解决中文乱码
                bg.labels=0, #聚类标签的背景颜色
                align.labels=list(c("left", "top"), c("center", "center")), #改变标签位置，前面为聚类标签，后面为每个stock标签
                overlap.labels=0,  #0-1，确定标签之间重叠程度，0表示如果较高级别的标签重叠，则不打印较低级别的标签 
        )
}

############################################################################################
#各行业树状图
TreeMap2<-function(x){
data <- read.xlsx("C.xlsx",1,encoding = "UTF-8")
data$stcok<- as.integer(data$stcok)
data<-data[data$class==x,]
data$color <- rainbow(nlevels(data$stock))[data$stock] #更改颜色
color <- c( "#9E3649", "#FF0017","#008B4F", "#00AE51")

palette.HCL.options <- list(hue_start=270, hue_end=360+150)

treemap(data,
        index=c("class", "stock1"),
        vSize="market_cap",
        vColor="percent",
        type="value",
        #palette.HCL.options=palette.HCL.options, #设置颜色
        palette=(values=color),
        #palette="-RdGy",
        format.legend = list(scientific = FALSE, big.mark = " "),
        fontsize.title=0, #去掉标题
        fontsize.legend=0, #去掉图例
        fontcolor.labels="white",fontface.labels=2,fontsize.labels = c(12,20), #标签颜色，加粗
        #fontfamily.labels="STSong", #解决中文乱码
        bg.labels=0, #聚类标签的背景颜色
        align.labels=list(c("left", "top"), c("center", "center")), #改变标签位置，前面为聚类标签，后面为每个stock标签
        overlap.labels=0,  #0-1，确定标签之间重叠程度，0表示如果较高级别的标签重叠，则不打印较低级别的标签 
       )
}
