gmean <- gss %>% 
  group_by(newocc, sex) %>% 
  summarise(mean = mean(usetech, na.rm = TRUE),
            sd = sd(usetech, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 

gmean %>% 
  mutate(sex = recode(sex, "1= 'Male'; 2= 'Female'")) %>% 
  filter(newocc != 9997) %>% 
  na.omit() %>% 
  ggplot(., aes(x=sex, y= mean)) + geom_col() +
  facet_grid(.~newocc)


ggsave(file="bar_usetech_facets.png", type = "cairo-png", width = 15, height = 10)
