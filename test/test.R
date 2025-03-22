# install.packages('tidyverse')
# install.packages('magrittr')
# install.packages('R6')



library(tidyverse)
g <- ggplot(data = iris)
# g <- g + theme_set(theme_minimal(base_size = 9, base_family='HiraKakuProN-W3'))
g <- g + geom_point(mapping = aes(x = Species, y = Sepal.Length), colour = '#00abff20', fill = '#00abff10')
# g <- g + geom_boxplot(data = iris, mapping = aes(x = Species, y = Sepal.Length), colour = '#00abff20', fill = '#00abff10')
# +	guides(alpha = "none")

g
