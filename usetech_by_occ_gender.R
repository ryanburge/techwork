gmean <- gss %>% 
  group_by(newocc, sex) %>% 
  summarise(mean = mean(usetech, na.rm = TRUE),
            sd = sd(usetech, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 

col <- c("brown4", "chartreuse4", "brown4", "brown4", "brown4", "brown4", "brown4", "black", "black", "black", "coral","coral", "coral")


a <- gmean %>% 
  head(10) %>% 
  mutate(sex = recode(sex, "1= 'Male'; 2= 'Female'")) %>% 
  filter(newocc != 9997) %>% 
  na.omit() %>% 
  ggplot(., aes(x=sex, y= mean, fill = sex)) + geom_col(color = "black") +
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") + 
  labs(x = "", y = "", title = "Use of Technology by Occupation and Gender" ) +
  facet_grid(.~newocc) +
  bar_rb() + theme(legend.position="none") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = .5)) + 
  scale_fill_manual(values = col)

b <- gmean %>% 
  tail(12) %>% 
  mutate(sex = recode(sex, "1= 'Male'; 2= 'Female'")) %>% 
  filter(newocc != 9997) %>% 
  na.omit() %>% 
  ggplot(., aes(x=sex, y= mean, fill = sex)) + geom_col(color = "black") +
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "black") + 
  labs(x = "", y = "", caption = "Data: GSS 2014" ) +
  facet_grid(.~newocc) +
  bar_rb() + theme(legend.position="none") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = .5))+ 
  scale_fill_manual(values = col)

c <- a + b + plot_layout(ncol = 1)

ggsave(file="images/occ_gender_usetech.png", type = "cairo-png", width = 30, height = 15, c)




