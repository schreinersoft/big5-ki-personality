install.packages("psych")
library(psych)

facets <- read_csv("learn/OCfacets-fake_comma.csv")

facets$O = (facets$O1 + facets$O2 + facets$O3)/3
facets$C = (facets$C1 + facets$C2 + facets$C3)/3

head(facets)

Oalpha_results <- facets %>% select(O1, O2, O3) %>% alpha()
Calpha_results <- facets %>% select(C1, C2, C3) %>% alpha()

Oalpha_results

Calpha_results

facets$Otest = facets$O1*3
facets$Otest2 = facets$O1*2

Oalpha_test <- facets %>% select(O1, Otest, Otest2) %>% alpha()

Oalpha_test

