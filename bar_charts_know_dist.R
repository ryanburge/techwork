anes16 %>% 
  ggplot(., aes(x=know)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  bar_rb() +
  labs(x = "# of Questions Correct", y = "Percent", title = "Political Knowledge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = round(seq(min(anes16$know), max(anes16$know), by = 1),1)) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = 1))

anes16 %>% 
  group_by(gender) %>% 
  summarise(mean = mean(know, na.rm = TRUE))


anes16 %>% 
  filter(gender !=99) %>% 
  ggplot(., aes(x=know)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  bar_rb() +
  labs(x = "# of Questions Correct", y = "Percent", title = "Political Knowledge", subtitle = "Mean Number Correct for Women: 1.94, For Men: 2.33") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = round(seq(min(anes16$know), max(anes16$know), by = 1),1)) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = 1)) + 
  facet_grid(.~ gender)
  