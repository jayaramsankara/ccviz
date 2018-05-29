library(ggplot2)
library(plyr)
accept_active_time <- read.csv("C:/Users/aymathur/Desktop/charts/accept_active_time.csv" , header = FALSE)
 #accept time

plot_graph <- function(accept_active_time, x , p ,t)
{
 csq <- as.factor(accept_active_time$V6)
 time <- x
 cdat <- ddply(accept_active_time, "csq", summarise, active.mean=mean(time))
 ggplot(accept_active_time, aes(x = time , group =csq , color = csq))+
     geom_density(position ="identity" , size = 1,adjust = 1.5)+
     labs(y = "Distribution",x = p,title = t)+
   scale_colour_brewer(palette="Dark2")
}

 active <- plot_graph(accept_active_time, accept_active_time$V2 ,"Active time"
                      ,"Average active time distribution group by csq")
 accept <- plot_graph(accept_active_time, accept_active_time$V1 ,"Accept time"
                      , "Average accept time distribution group by csq")
 active
 accept
 
 #2
 ggplot(accept_active_time, aes(x = V2))+ 
   geom_density(adjust = 1.5 , alpha =0.5 , col = "blue") +
   xlim(120,max(accept_active_time$V2)) +
  labs(y = "Distribution",x = "Active time",
      title = "Average active time distribution group by csq")+
   facet_grid(V6~.)
 
 #3
 ggplot(accept_active_time, aes(x = V1))+ 
   geom_density(adjust = 1.5 , alpha =0.5 , col = "blue") +
   labs(y = "Distribution",x = "Accept time",
        title = "Average accept time distribution group by csq")+
   facet_grid(V6~.)
 

#qq plot for active time
ggplot(accept_active_time) +geom_qq(aes(sample=V2)) + 
  labs(title = "qq plot for active time")+
  facet_grid(V6~.)

#for accept time
ggplot(accept_active_time) +geom_qq(aes(sample=V1)) + 
  labs(title = "qq plot for accept time")+
  facet_grid(V6~.)
