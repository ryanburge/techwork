library(prediction)

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
  mutate(news = recode(news, "1 = 'High Cons.'; 0 = 'Low Cons.'")) %>% 
  mutate(new = paste(news, gender, sep = " - ")) %>% 
  ggplot(., aes(mean, fitted, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=fitted - se.fitted, ymax=fitted + se.fitted, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "", title = "Interaction of News Consumption and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,4)) +
  theme(legend.text=element_text(size=18)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  ggsave("D://techwork/sscr/friesen_color_fig8.png", width = 14, height = 10, dpi = 300)



x %>% 
  filter(gender != 99) %>% 
  mutate(news = recode(news, "1 = 'High Cons.'; 0 = 'Low Cons.'")) %>% 
  mutate(new = paste(news, gender, sep = " - ")) %>% 
  ggplot(., aes(mean, fitted, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=fitted - se.fitted, ymax=fitted + se.fitted, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "", title = "Interaction of News Consumption and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,4)) +
  theme(legend.text=element_text(size=18)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c("black", "black", "black", "black")) + 
  annotate("text", x=50, y = 3.42, label = "High Cons. - Male", size = 6, family = "Product Sans") +
  annotate("text", x=45, y = 2.75, label = "High Cons. - Female", size = 6, family = "Product Sans") +
  annotate("text", x=50, y = 2.45, label = "Low Cons. - Male", size = 6, family = "Product Sans") +
  annotate("text", x=50, y = 2.1, label = "Low Cons. - Female", size = 6, family = "Product Sans") +
  theme(legend.position = "none") +
  ggsave("D://techwork/sscr/friesen_grayscale_fig8.png", width = 14, height = 10, dpi = 300)


