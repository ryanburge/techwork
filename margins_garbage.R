
com <- com %>% 
  mutate(int2 = recode(polint, "1:2 = 0; 3:5 =1"))

reg1 <- lm(know ~ mean*int2, data = com)

b <- predict(reg1, newdata = com, interval =  "confidence")
c <- cbind(com, b)



c %>% 
  mutate(int2 = recode(int2, "1 = 'High Interest'; 0 = 'Low Interest'")) %>% 
  ggplot(., aes(mean, fit, group=int2, color = as.factor(int2), label = int2)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "Entire Sample", title = "Interaction of Political Interest and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,3.5))

ggsave(file="margins_plot.png", type = "cairo-png", width = 15, height = 10)


com <- com %>% 
  mutate(int2 = recode(polint, "1:2 = 0; 3:5 =1"))

fem <- com %>% filter(gender == "Female")



reg1 <- lm(know ~ mean*int2, data = fem)

b <- predict(reg1, newdata = fem,  interval =  "confidence")
femc <- cbind(fem, b) %>% mutate(group = c("Female"))



c %>% 
  mutate(int2 = recode(int2, "1 = 'High Interest'; 0 = 'Low Interest'")) %>% 
  ggplot(., aes(mean, fit, group=int2, color = as.factor(int2), label = int2)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "Female Sample Only", title = "Interaction of Political Interest and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,3.5))

ggsave(file="margins_plot_female.png", type = "cairo-png", width = 15, height = 10) 
  


com <- com %>% 
  mutate(int2 = recode(polint, "1:2 = 0; 3:5 =1")) %>% 
  mutate(kid_di = recode(kids2, "1:9=1 ; else=0 "))

men <- com %>% filter(gender == "Male")



reg1 <- lm(know ~ mean*int2, data = men)

b <- predict(reg1, newdata = men,  interval =  "confidence")
menc <- cbind(men, b) %>% mutate(group = c("Male"))



c %>% 
  mutate(int2 = recode(int2, "1 = 'High Interest'; 0 = 'Low Interest'")) %>% 
  ggplot(., aes(mean, fit, group=int2, color = as.factor(int2), label = int2)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "Male Sample Only", title = "Interaction of Political Interest and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,3.5))

ggsave(file="margins_plot_male.png", type = "cairo-png", width = 15, height = 10)

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
  scale_y_continuous(limits = c(1,3.5)) +
  theme(legend.text=element_text(size=24)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

ggsave(file="margins_plot_four_lines.png", type = "cairo-png", width = 18, height = 10) 



reg1 <- lm(know ~ mean*kid_di, data = men)
b <- predict(reg1, newdata = men,  interval =  "confidence")
menc <- cbind(men, b) %>% mutate(group = c("Male"))

reg1 <- lm(know ~ mean*kid_di, data = fem)
b <- predict(reg1, newdata = fem,  interval =  "confidence")
femc <- cbind(fem, b) %>% mutate(group = c("Female"))

margin <- bind_rows(femc, menc)


margin %>% 
  mutate(kid_di = recode(kid_di, "1 = 'Have Kids'; 0 = 'No Kids'")) %>% 
  mutate(new = paste(kid_di, group, sep = " - ")) %>% 
  ggplot(., aes(mean, fit, group=new, color = as.factor(new), label = new)) +
  geom_line(size =1) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width = .5)) + 
  labs(x ="Percent of Job That Uses Technology", y = "Prediction of Political Knowledge", subtitle = "", title = "Interaction of Having Children and Tech Usage on Political Knowledge") + 
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 24, vjust =2, face = "bold")) +
  scale_y_continuous(limits = c(1,3.5)) +
  theme(legend.text=element_text(size=24)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

ggsave(file="margins_plot_four_lines_kids.png", type = "cairo-png", width = 18, height = 10) 






