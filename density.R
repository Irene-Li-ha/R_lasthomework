densityplot <- function(){

density <- read.csv('density.csv', header=T)

# create some fake data with zero-crossings
a= data.frame(x=seq(1,length(density$price1)),y=density$price1)
b <- do.call("rbind",
             sapply(1:(nrow(a)-1), function(i){
               f <- lm(x~y, a[i:(i+1),])
               if (f$qr$rank < 2) return(NULL)
               r <- predict(f, newdata=data.frame(y=0))
               if(a[i,]$x < r & r < a[i+1,]$x)
                 return(data.frame(x=r,y=0))
               else return(NULL)
             }))
a2 <- rbind(a,b)



mytheme<-theme(panel.grid.major=element_line(),
               axis.ticks = element_blank(),
               axis.text = element_blank(),
               axis.title=element_blank())

ggplot(a2,aes(x,y)) + 
  geom_area(data=subset(a2, y<=0), fill="#FF6666") +
  geom_area(data=subset(a2, y>=0), fill="#66CC33") +
  scale_y_continuous(breaks=NULL)+
  labs(fill="")+
  mytheme+
  ggtitle("Where will Shanghai composite index be in the next 5 minutes?")+
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5))
}







