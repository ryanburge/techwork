anes16 %>% 
  ggplot(., aes(x=know)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  bar_rb() +
  labs(x = "# of Questions Correct", y = "Percent", title = "Political Knowledge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = round(seq(min(anes16$know), max(anes16$know), by = 1),1)) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = 1)) 

anes16 %>% 
  group_by(gender) %>% 
  summarise(mean = mean(know, na.rm = TRUE),
            sd = sd(know, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 

col <- c("brown4", "chartreuse4", "brown4", "brown4", "brown4", "brown4", "brown4", "black", "black", "black", "coral","coral", "coral")

m <- anes16 %>% 
  filter(gender == "Male") %>% 
  count(know) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Male"))


f <- anes16 %>% 
  filter(gender == "Female") %>% 
  count(know) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(gender = c("Female"))

t <- bind_rows(m, f)

t %>% 
  ggplot(., aes(x=know, y = pct, fill = gender)) + geom_col(color = "black", show.legend = FALSE) + 
  bar_rb() +
  labs(x = "Number of Questions Correct", y = "Percent", title = "Political Knowledge", subtitle = "Mean Number Correct for Women: 1.94, For Men: 2.33") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = round(seq(min(anes16$know), max(anes16$know), by = 1),1)) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = 1)) + 
  facet_grid(.~ gender) + 
  scale_fill_manual(values = col)

ggsave(file="gender_know_bar.png", type = "cairo-png", width = 18, height = 10) 

  