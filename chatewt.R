library(tidyverse)

errorPlot <-  function(csvFile) {
ewtdata <- read.csv(csvFile, header = FALSE)

fdata <- ewtdata %>% filter(V3 < 86000)


diffdata <- fdata %>% data.frame(diff1 = .$V5-.$V3, diff2 = .$V5 - .$V4, errorType= "M/M/C Error", d="Simple EWT Error")

p <- ggplot(data = diffdata ) + geom_point(aes(x=diff1, y=V5, color=errorType)) + geom_point(aes(x=diff2, y=V5, color =d))

q <- p + xlab("Error Factor") + ylab("Actual Wait Time (sec)") + theme_minimal()

return ( q + ggtitle("Chat EWT Errors") )
}


errorPlot("chatewtawt_1.csv")
