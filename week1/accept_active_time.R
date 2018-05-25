
library(ggplot2)
library(plyr)
accept_active_time <- read.csv("C:/Users/aymathur/Desktop/charts/accept_active_time.csv" , header = FALSE)
 #accept time
 cs <- accept_active_time$V6
 csq <- as.factor(cs)
 cdat <- ddply(accept_active_time, "csq", summarise, active.mean=mean(V2))
 ggplot(accept_active_time, aes(x = V2 , group =csq , color = csq))+ 
     xlim(120,150) +
     geom_density(position ="identity" , size = 1,adjust = 1.5)+
     labs(y = "Distribution",x = "Active time",
                  title = "Average active time distribution group by csq")+
   scale_colour_brewer(palette="Dark2")+
   geom_vline(data=cdat, aes(xintercept=active.mean,  colour=csq),
              linetype="dashed", size=1)
 
 #2
 ggplot(accept_active_time, aes(x = V2))+ 
   geom_density(adjust = 1.5 , alpha =0.5 , col = "blue") +
   xlim(120,max(accept_active_time$V2)) +
  labs(y = "Distribution",x = "Active time",
      title = "Average active time distribution group by csq")+
   facet_grid(V6~.)
 

#qq plot for active time
ggplot(accept_active_time) +geom_qq(aes(sample=V2)) + 
  labs(title = "qq plot for active time")+
  facet_grid(V6~.)

