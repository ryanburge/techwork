com %>% 
  filter(V161495 != -9) %>% 
  ggplot(., aes(x=V161495, y = mean)) + geom_point() + geom_smooth(method = lm) +
  xlim(0, 7) +
  labs(x= "Social Media Usage", y = "UseTech") + facet_grid(.~gender)

ggsave(file="images/usetech_social_gender.png", type = "cairo-png", width = 15, height = 15)
       
       

com %>% 
  filter(V161495 != -9) %>% 
  filter(gender != 99) %>% 
  ggplot(., aes(x=V161495, y =know)) + geom_point() + geom_smooth(method = lm) +
  xlim(0, 7) +
  labs(x= "Social Media Usage", y = "Political Knowledge") + facet_grid(.~gender)

ggsave(file="images/usetech_know_gender.png", type = "cairo-png", width = 15, height = 15)
       

com %>% 
  filter(V161495 >0) %>% 
  group_by(gender) %>% 
  summarise(mean = mean(V161495, na.rm = TRUE))

com %>% 
  filter(V161495 != -9) %>% 
  filter(gender != 99) %>% 
  ggplot(., aes(x=V161008, y =know)) + geom_point() + geom_smooth(method = lm) +
  xlim(0, 7) +
  labs(x= "News Consumption", y = "Political Knowledge") + facet_grid(.~gender)

ggsave(file="images/news_know_gender.png", type = "cairo-png", width = 15, height = 15)


com %>% 
  filter(V161495 != -9) %>% 
  filter(gender != 99) %>% 
  ggplot(., aes(x=V161008, y = mean)) + geom_point() + geom_smooth(method = lm) +
  xlim(0, 7) +
  labs(x= "News Consumption", y = "UseTech") + facet_grid(.~gender)

ggsave(file="images/usetech_news_gender.png", type = "cairo-png", width = 15, height = 15)



