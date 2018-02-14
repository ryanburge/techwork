library(fst)
library(tidyverse)
library(car)
library(janitor)
library(extrafont)

gss <- read.fst("C://gss.fst") %>% filter(year == 2014)


gss %>% 
  ggplot(., aes(x=usetech)) + geom_histogram(bins =10)

gss$tech <- as.numeric(cut_number(gss$usetech, 3))

#0-20
#22.5-85
#88-100

occ_fem <- gss %>% 
  filter(educ >=16) %>% 
  filter(sex ==2) %>% 
  group_by(tech) %>% 
  count(occ10) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(tech) %>% 
  arrange(tech, -n) %>% 
  na.omit() 
  