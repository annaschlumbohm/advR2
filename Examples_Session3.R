
library(dplyr)
library(ggplot2)

filter()    ## observations (ROWS)
select()    ## variables (COLUMNS)
%>%         ## pipe: object from left to function on the right
  
iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)

dplyr::mutate(iris, sepal = Sepal.Length + Sepal.Width)

iris %>%
  mutate(sepal = Sepal.Length + Sepal.Width)