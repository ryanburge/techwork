com <- com %>% 
  mutate(news = recode(V161008, "0:4 =0; 5:7 =1"))

fem <- com %>% filter(gender == "Female")
men <- com %>% filter(gender == "Male")

reg1 <- lm(know ~ mean*news, data = fem)
reg2 <- lm(know ~ mean*news, data = men)

f <- prediction(reg1)
m <- prediction(reg2)

x <- bind_rows(m, f)

x %>% 
  filter(gender != 99) %>% 
  mutate(news = recode(news, "1 = 'High Consumption'; 0 = 'Low Consumption'")) %>% 
  mutate(new = paste(news, gender, sep = " - ")) %>% 
  ggplot(., aes(mean, fitted, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=fitted - se.fitted, ymax=fitted + se.fitted, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "", title = "Interaction of News Consumption and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,3.5)) +
  theme(legend.text=element_text(size=22)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))


ggsave(file="margins_plot_four_lines_news3.png", type = "cairo-png", width = 18, height = 10) 
