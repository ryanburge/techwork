com <- com %>% 
  mutate(white = recode(V161310a, "1=1; else=0")) %>% 
  mutate(black = recode(V161310b, "1=2; else=0")) %>% 
  mutate(hisp = recode(V161309, "1=3; else=0")) %>% 
  mutate(indian = recode(V161310b, "1=4; else=0")) %>% 
  mutate(asian = recode(V161310b, "1=5; else=0")) %>% 
  mutate(race = white + black + hisp + indian + asian) 


jj <- com %>% 
  filter(gender != 99) %>% 
  filter(jobs != 99) %>% 
  group_by(jobs, white) %>% 
  count(gender) %>% 
  mutate(pct = prop.table(n)) %>% 
  ungroup(white) %>% 
  mutate(white = recode(white, "1= 'White'; 0 = 'Not White'"))

jj %>% 
  ggplot(., aes(x=gender, y=pct, group = white, fill = white)) + geom_col(position = "dodge") +  facet_grid(.~ jobs) + bar_rb()

ggsave(file="occ_bar_facets_race.png", type = "cairo-png", width = 42, height = 10)

com %>% 
  group_by(gender) %>% 
  summarise(mean = mean(mean, na.rm = TRUE))






