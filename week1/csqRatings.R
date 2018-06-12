library(ggplot2)
library(plyr)

csqRating <- read.csv("csqRating.csv",header = FALSE)
csq <- as.factor(csqRating$V4)
rating <- csqRating$V6
type <- csqRating$V5
cdat <- ddply(csqRating, "csq", summarise, rating.mean=mean(rating))

#density plot
 ggplot(csqRating, aes(x = rating , color = csq))+ 
  geom_density(size = 1,adjust = 1.5)+xlim(0,6)+
  labs(y = "Density",x = "Rating",
       title = "Rating distribution per csq group by Chat type")+
  scale_colour_brewer(palette="Dark2")+
  geom_vline(data=cdat, aes(xintercept=rating.mean, colour=csq),
             linetype="dashed", size=1)+
  facet_grid(V5~.)
#2
 ggplot(csqRating, aes(x = rating ))+ 
   geom_density(size = 1,adjust = 1.5)+
   labs(y = "Density",x = "Rating",
        title = "Rating distribution per csq group by Chat type")+
   scale_colour_brewer(palette="Dark2")+
   facet_grid(V5~V4)
 
 #boxplot
 ggplot(csqRating, aes(x=csq, y=rating, fill = csq)) + geom_boxplot() +
   facet_grid(V5~.) 

 