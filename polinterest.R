polint <- com %>% 
  group_by(gender) %>%
  mutate(polint = 6 -V161003) %>% 
  count(polint) %>% 
  mutate(pct = prop.table(n))

polint %>% 
  filter(gender !=99) %>% 
  ggplot(., aes(x=polint, y=pct, group = gender, fill = gender)) + 
  geom_col(position = "dodge", color = "black ") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Political Interest", y = "Percent", title = "Pay Attention to Politics?") +
  bar_rb() +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = .5)) +
  scale_x_continuous(limits = c(.5,5.5), breaks = c(1,2,3,4,5), labels = c("Never", "", "Half the Time", "", "Always"))

ggsave(file="polint_dodge.png", type = "cairo-png", width = 15, height = 10)


