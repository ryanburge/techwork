
com <- com %>% 
  mutate(polint = 6 - V161003) 

com <- com %>% 
  mutate(int2 = recode(polint, "1:2 = 0; 3:5 =1"))

fem <- com %>% filter(gender == "Female")
reg1 <- lm(know ~ mean*int2, data = fem)
b <- predict(reg1, newdata = fem,  interval =  "confidence")
femc <- cbind(fem, b) %>% mutate(group = c("Female"))


men <- com %>% filter(gender == "Male")
reg1 <- lm(know ~ mean*int2, data = men)
b <- predict(reg1, newdata = men,  interval =  "confidence")
menc <- cbind(men, b) %>% mutate(group = c("Male"))



margin <- bind_rows(femc, menc)


margin %>% 
  mutate(int2 = recode(int2, "1 = 'High Interest'; 0 = 'Low Interest'")) %>% 
  mutate(new = paste(int2, group, sep = " - ")) %>% 
  ggplot(., aes(mean, fit, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "", title = "Interaction of Political Interest and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,3.95)) +
  theme(legend.text=element_text(size=18)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  ggsave("D://techwork/sscr/friesen_color_fig6.png", width = 14, height = 10, dpi = 300)


margin %>% 
  mutate(int2 = recode(int2, "1 = 'High Interest'; 0 = 'Low Interest'")) %>% 
  mutate(new = paste(int2, group, sep = " - ")) %>% 
  ggplot(., aes(mean, fit, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "", title = "Interaction of Political Interest and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,3.95)) +
  theme(legend.text=element_text(size=18)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c("black", "black", "black", "black")) + 
  annotate("text", x=50, y = 3.5, label = "High Interest - Male", size = 6, family = "Product Sans") +
  annotate("text", x=50, y = 3, label = "High Interest - Female", size = 6, family = "Product Sans") +
  annotate("text", x=50, y = 2.5, label = "Low Interest - Male", size = 6, family = "Product Sans") +
  annotate("text", x=50, y = 2.1, label = "Low Interest - Female", size = 6, family = "Product Sans") +
  theme(legend.position = "none") +
  ggsave("D://techwork/sscr/friesen_grayscale_fig6.png", width = 14, height = 10, dpi = 300)
