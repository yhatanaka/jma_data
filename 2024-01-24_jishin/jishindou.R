# install.packages('tidyverse')
library("tidyverse")
library("magrittr")

repNo = 10
jishinTime = c(1:10)
jishinValue = c(rep(1,10))

jishinData = data.frame(jishinTime, jishinValue)
jishinData
ggplot(data= jishinData, mapping = aes(x=jishinTime, y=jishinValue)) + geom_line()

