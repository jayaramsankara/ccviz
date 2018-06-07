library(ggplot2)
library(dplyr)
library(treemapify)
wrapup <- read.csv("wrapup.csv",header = FALSE)

#By category
all_category <- function(wrapup)
{
cat <- wrapup %>% group_by(wrapup$V3) %>% summarise(count = n())
category = cat$`wrapup$V3`
ggplot(cat, aes(area = count, fill = category,label = category)) +
  geom_treemap() +
  geom_treemap_text(place = "centre", alpha = 0.8, colour = "black") 
}

#By reasons
category_reason <- function(wrapup)
{
temp <- wrapup %>% group_by(wrapup$V3,wrapup$V4) %>% summarise(count = n()) 
categoryy = temp$`wrapup$V3`
reason = temp$`wrapup$V4`
ggplot(temp, aes(area = count, fill = count, label = reason,
                subgroup = categoryy)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", alpha = 0.8, colour = "yellow") +
  geom_treemap_text(colour = "white", place = "topleft")
}

#By 1 category
by_category <- function(wrapup, cat)
{
temp <- wrapup %>% group_by(wrapup$V3,wrapup$V4) %>% summarise(count = n()) 
categoryy = temp$`wrapup$V3`
temp <- subset(temp,categoryy==cat)
categoryy = temp$`wrapup$V3`
reason = temp$`wrapup$V4`
ggplot(temp, aes(area = count, fill = count, label = reason, subgroup = categoryy)) +
  geom_treemap()  +
  geom_treemap_text(colour = "white", place = "topleft")+
  geom_treemap_subgroup_text(place = "centre", alpha = 0.8, colour = "yellow")
}

all_category(wrapup)
category_reason(wrapup)
by_category(wrapup,"Categroy9")
