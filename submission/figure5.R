
gss <- gss %>% 
  mutate(newocc = recode(occ10, "0:999 = 'Management';
                         1000:1999 = 'Technology, Engineering and Science';
                         2000:2999 = 'Education, Legal, and Media';
                         3000:3540 = 'Health and Technology';
                         3600:4650 = 'Service';
                         4700:4999 = 'Sales';
                         5000:5999 = 'Office and Admin Support';
                         6000:7630 = 'Construction and Maintenance';
                         7700:9750 = 'Production and Transportation';
                         9800:9830 = 'Military'"))


mean <- gss %>% 
  group_by(newocc) %>% 
  summarise(mean = mean(usetech, na.rm = TRUE)) %>% 
  rename(jobs = newocc)


anes16 <- anes16 %>% 
  mutate(jobs = recode(V161291b, "1:7=1; 8:16=2; 17:29=3; 30:35=4; 36:54=5; 55:59=6; 60:66=7; 67:79=8; 80:95=9; 96:98=10; else = 99"))

anes16 <- anes16 %>% 
  mutate(jobs = recode(jobs, "1 = 'Management';
                              2 = 'Technology, Engineering and Science';
                              3 = 'Education, Legal and Media';
                              4 = 'Health and Technology';
                              5 = 'Service';
                              6 = 'Sales';
                              7 = 'Office and Admin Support';
                              8 = 'Construction and Maintenance';
                              9 = 'Production and Transportation';
                              10 = 'Military'"))

com <- left_join(anes16, mean)


com %>% 
  filter(gender != 99) %>% 
  ggplot(., aes(x=mean, y = know, group = gender, color = gender)) + 
  geom_point() +
  geom_smooth(method = lm) + geom_jitter(width = 2.5) +
  labs(x = "Mean Use of Technology by Job Type", y = "Number of Knowledge Questions Correct", title = "Political Knowledge Gain by Tech Use", caption = "Data: ANES 2016") +
  scale_color_manual(values = c("brown4", "chartreuse4"))+
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 34, vjust =0, face = "bold"))  +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + theme(panel.spacing = unit(2, "lines")) +
  ggsave("D://techwork/sscr/friesen_color_fig5.png", width = 10, height =10, dpi = 300)


com %>% 
  filter(gender != 99) %>% 
  ggplot(., aes(x=mean, y = know, group = gender, color = gender)) + 
  geom_point() +
  geom_smooth(method = lm) + geom_jitter(width = 2.5) +
  labs(x = "Mean Use of Technology by Job Type", y = "Number of Knowledge Questions Correct", title = "Political Knowledge Gain by Tech Use", caption = "Data: ANES 2016") +
  scale_color_grey() +
  long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 34, vjust =0, face = "bold"))  +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + theme(panel.spacing = unit(2, "lines")) +
  ggsave("D://techwork/sscr/friesen_grayscale_fig5.png", width = 10, height =10, dpi = 300)






