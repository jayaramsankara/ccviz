library("ggplot2")


rdata <- read.csv("chatratings.csv", sep="|", header = FALSE)
ratings<-rdata$V2
avg <- mean(rdata$V2)
sdev <- sd(rdata$V2)

p1 <-ggplot(data=data.frame(x=ratings,y= dnorm(x=ratings, mean=avg, sd = sdev)), aes(x=x, y=y)) + geom_line(color="blue", size=1)
p2 <- p1 + scale_y_continuous(breaks = NULL) + xlab("Chat Ratings") + ylab("Density") + ggtitle("Chat Ratings Distribution")
p2

p5 <- ggplot(data=data.frame(x=ratings), aes(x)) + stat_function(fun=dnorm, args=list(mean=avg,sd =sdev), color="blue", size=1)
p6 <- p5 + scale_y_continuous(breaks = NULL) + xlab("Chat Ratings") + ylab("Density") + ggtitle("Chat Ratings Distribution")
p3 <- p2 + stat_function(mapping=aes(x=x), fun=dnorm, args=list(mean=avg,sd =sdev), color="red", size=1)
p3


hp <- ggplot(data=rdata) + geom_bar(aes(x=V2)) +coord_flip() + ggtitle("Chat Ratings Distribution")
hp

qqp <- ggplot(data=rdata) +geom_qq(aes(sample=V2))
qqp


