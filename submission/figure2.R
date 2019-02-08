
gss <- gss %>% 
  mutate(newocc = recode(occ10, "0:999 = 'Management';
                         1000:1999 = 'Technology, Engineering and Science';
                         2000:2999 = 'Education, Legal and Media';
                         3000:3540 = 'Health and Technology';
                         3600:4650 = 'Service';
                         4700:4999 = 'Sales';
                         5000:5999 = 'Office and Admin Support';
                         6000:7630 = 'Construction and Maintenance';
                         7700:9750 = 'Production and Transportation';
                         9800:9830 = 'Military'"))


mean <- gss %>% 
  group_by(newocc) %>% 
  summarise(mean = mean(usetech, na.rm = TRUE),
            sd = sd(usetech, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) 

mean <- mean %>% 
  mutate(newocc = as.factor(newocc)) %>% na.omit()

mean$class <- c("Low", "High", "High", "High", "Low", "Low", "Low", "Low", "Low", "High")


mean_rb <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       legend.text = element_text(family = "Product Sans", size =34),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.x =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 26, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =24),
       axis.title.y =  element_text(family = "Product Sans", size =24), 
       axis.text.x = element_text(family = "Product Sans", size =26), 
       axis.text.y = element_text(family = "Product Sans", size =26)
)
  
}

mean %>% 
  ggplot(., aes(x= mean, y= fct_reorder(newocc, mean))) + 
  geom_point(shape=21, size =4, aes(fill = factor(class)), show.legend = TRUE) +
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(class)), height=0, size = 1, show.legend = FALSE) + 
  mean_rb() + 
  scale_color_manual(values = c("brown4", "chartreuse4")) +
  scale_fill_manual(values = c("brown4", "chartreuse4")) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "What Percentage of Time Do You Spend on Technology?", y = "Occupational Class", title = "Which Occupational Classes Spend the Most Time on Technology?", subtitle = "High and Low Indicate Classification in Previous Scholarship", caption = "Data: GSS 2014") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  ggsave("D://techwork/sscr/friesen_color_fig2.png", width = 18, height = 6, dpi = 300)


mean %>% 
  ggplot(., aes(x= mean, y= fct_reorder(newocc, mean))) + 
  geom_point(shape=21, size =4, aes(fill = factor(class)), show.legend = TRUE) +
  geom_errorbarh(aes(xmin = lower, xmax=upper, colour = factor(class)), height=0, size = 1, show.legend = FALSE) + 
  mean_rb() + 
  scale_color_grey() +
  scale_fill_grey() +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "What Percentage of Time Do You Spend on Technology?", y = "Occupational Class", title = "Which Occupational Classes Spend the Most Time on Technology?", subtitle = "High and Low Indicate Classification in Previous Scholarship", caption = "Data: GSS 2014") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  ggsave("D://techwork/sscr/friesen_grayscale_fig2.png", width = 18, height = 6, dpi = 300)
