library(tidyverse)
library(rstatix)


a = c(1,2,3,4,5,6)
b = c(1,2,3,4,5,6)


data_raw <- list(
  a = c(1,2,3,4,5,6),
  b = c(2,4,6,3,1),
  g = c("a","b","a","a","b","a")
)

data <- as.data.frame(data_raw)

utest <- wilcox.test(data$a, data$b, paired = FALSE)
print(utest)

utest$statistic
utest$p.value


utest <- wilcox.test(a, b, paired = FALSE)
print(utest)

utest$statistic[["W"]]
utest$p.value


z <- qnorm(utest$p.value/2)
r <- abs(z) / sqrt(9)
CLES <- utest$statistic[["W"]] / (6 * 6)


rFromWilcox(utest, 6)

rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2) 
  r<- z/ sqrt(N)
  return(r)
}
