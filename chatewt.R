library(tidyverse)
library(gridExtra)

errorPlot <-  function(csvFile) {
  ewtdata <- read.csv(csvFile, header = FALSE)
  
  fdata <- ewtdata %>% filter(V3 < 86000)
  
  
  diffdata <-
    fdata %>% data.frame(
      diff1 = .$V3 - .$V5,
      diff2 = .$V4 - .$V5,
      errorType = "M/M/C Error",
      d = "Simple EWT Error"
    )
  
  diffdata
  p <-
    ggplot(data = diffdata) + geom_point(aes(x = diff1, y = V5, color = errorType)) + geom_point(aes(x =
                                                                                                       diff2, y = V5, color = d))

  q <-
    p + xlab("Error Factor") + ylab("Actual Wait Time (sec)") + theme_minimal()

  return (q + ggtitle("Chat EWT Errors") + scale_x_continuous(limits = c(-300, 100)) +  scale_y_continuous(limits = c(0, 120)))
}


grid.arrange(errorPlot("chatewtawt_1.csv"),
            errorPlot("chatewtawt_2.csv"),
             ncol = 2)
