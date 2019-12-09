setwd('/Users/Irene/Desktop/last homework')
library(shiny)
#定义第四个页面，Elite页面
source("density.R")
source("final_barchart.R")
source("treemap.R")
source("StockCandlestick.R")
library(shiny)
library("xlsx")
library(ggplot2)
library(dplyr)
library(purrr)
#install.packages("treemap")
library(treemap)
#install.packages("treemapify")
library(treemapify)
library(RColorBrewer)
library(ggplot2)
#library(tidyverse)
library(gridExtra)
library(quantmod)
library(zoo)
library(dplyr)

ui <- navbarPage('FINVIZ',
                 tabPanel('Home',
                          #定义第一个页面：Home页面
                          fluidPage(
                            h1("上证指数K线图",align="center"),
                            fluidRow(
                              column(12,plotOutput("szindex"))
                            ),
                            h1("横向条形图",align="center"),
                            #条形图
                            h3("图一及图二"),
                            fluidRow(
                              column(6,plotOutput("barchart1")),
                              column(6,plotOutput("barchart2"))),
                            h3("图三及图四"),
                            fluidRow(
                              column(6,plotOutput("barchart3")),
                              column(6,plotOutput("barchart4"))),
                            h3("图五"),
                            fluidRow(
                              column(6,plotOutput("BBplot"))
                            ),
                            #输出表格与树状图
                            h1("上证个股行情表",align="center"),
                            fluidRow(
                              column(12,dataTableOutput("overview"))
                              
                            ),
                            h1("全行业树状图",align="center"),
                            fluidRow(
                              column(12,plotOutput("treeMap"))
                              )
                            )),
                 tabPanel('Maps',
                          #定义第二个页面，树状图页面
                          fluidPage(
                            #树状图
                            p("选择想要查看的行业的树状图"),
                            fluidRow(
                              column(6,selectInput("industryclass", "行业分类", 
                                                    c("农副食品加工业","食品制造业","酒、饮料和精制茶制造业",
                                                      "纺织业","纺织服装、服饰业","家具制造业","造纸及纸制品业",
                                                      "医药制造业")))),
                            h1("各行业树状图",align="center"),
                            fluidRow(
                              column(12,plotOutput("industrytreeMap"))
                            )
                          )),
                 tabPanel('Elite',
                          #定义第三个页面，Elite页面
                          tab4 <- fluidPage(
                            #查找个股信息
                            p("选择想要查看的个股的K线图及财务信息数据"),
                            fluidRow(
                              column(3,textInput("stockcode", "证券代码", "600000")),
                              column(3,actionButton("go", "确定"))
                            ),
                            h1("个股K线图",align="center"),
                            #日线图
                            fluidRow(
                              column(12,plotOutput("stockcandlestick"))
                            ),
                            h1("个股财务信息表格",align="center"),
                            #财务信息表格
                            fluidRow(
                              column(12,tableOutput("stockview"))
                            )
                          ))
)

table1 <- read.xlsx("C:\\Users\\Irene\\Desktop\\last homework\\table1.xlsx",1,encoding = "UTF-8")
table2 <- read.xlsx("C:\\Users\\Irene\\Desktop\\last homework\\table2.xlsx",1,encoding = "UTF-8")
table1<-table1[1:170,]
table2<-table2[1:170,]

server <- function(input, output) {
  
  stockfactor <- eventReactive(input$go, {
    #等待按钮被点击，得到股票代码
    stocktable <- table2[table2[,1]==input$stockcode,]
    stock <- cbind(colnames(stocktable),t(stocktable))
    showstock <- cbind(stock[1:8,],stock[9:16,],stock[17:24,])
  })
  output$overview <- renderDataTable(table1)
  output$stockview <- renderTable(
    stockfactor(),colnames = FALSE
  )
  output$BBplot <- renderPlot(
    densityplot()
  )
  output$barchart1 <- renderPlot(
    Barchart1()
  )
  output$barchart2 <- renderPlot(
    Barchart2()
  )
  output$barchart3 <- renderPlot(
    Barchart3()
  )
  output$barchart4 <- renderPlot(
    Barchart4()
  )
  output$treeMap <- renderPlot(
    TreeMap1()
  )
  Xstock <- eventReactive(input$go, {
    #等待按钮被点击，得到股票代码
    input$stockcode
  })
  output$stockcandlestick <- renderPlot(
    stockplot(Xstock())
  )
  output$szindex <- renderPlot(
    stockplot("600000")
  )
  output$industrytreeMap <-renderPlot(
    TreeMap2(input$industryclass)
  )
}




shinyApp(ui, server)

