#CFA Analysis Workshop
#2025-11-15

library(foreign)
dat <- read.spss("https://stats.idre.ucla.edu/wp-content/uploads/2018/05/SAQ.sav",
                 to.data.frame=TRUE, use.value.labels = FALSE)

round(cor(dat[,1:8]),2)
