library(fst)
library(tidyverse)
library(car)
library(janitor)
library(extrafont)

gss <- read.fst("C://gss.fst") %>% filter(year == 2014)


gss %>% 
   ggplot(., aes(x=usetech)) + geom_histogram(bins =100)
 
 library(ggbeeswarm)
 
 gss %>% 
   mutate(sex = recode(sex, "1 = 'Male'; 2 = 'Female'")) %>% 
   ggplot(., aes(x= factor(sex), y = usetech, color = factor(sex))) + geom_quasirandom() +
   bar_rb() +
   theme(legend.position="none") +
   scale_color_brewer(palette="Set2") +
   scale_y_continuous(labels = function(x) paste0(x, "%")) +
   theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = .5)) +
   labs(x = "Gender", y = "Percent of Job That Uses Technology", title = "Distribution of Tech Usage", caption = "Data: GSS 2014", subtitle = "Mean Score for Women = 56.1%, Men = 48.6%") +
   theme(plot.title = element_text(size= 44, hjust = 0))
   
 ggsave(file="D://techwork/images/beeswarm_gender_usetech.png", type = "cairo-png", width = 21, height = 15)
 
 
 gss %>% 
   mutate(sex = recode(sex, "1 = 'Male'; 2 = 'Female'")) %>% 
   group_by(sex) %>% 
   summarise(mean = mean(usetech, na.rm = TRUE),
             sd = sd(usetech, na.rm = TRUE), 
             n = n()) %>% 
   mutate(se = sd/sqrt(n),
          lower = mean - qt(1 - (0.05 /2),  n -1) * se,
          upper = mean + qt(1 - (0.05 /2),  n -1) * se)
 
# 
# gss$tech <- as.numeric(cut_number(gss$usetech, 3))
# 
# #0-20
# #22.5-85
# #88-100
# 
# occ_fem <- gss %>% 
#   filter(educ >=16) %>% 
#   filter(sex ==2) %>% 
#   group_by(tech) %>% 
#   count(occ10) %>% 
#   mutate(pct = prop.table(n)) %>% 
#   ungroup(tech) %>% 
#   arrange(tech, -n) %>% 
#   na.omit() 
  

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
       plot.title = element_text(family = "Product Sans", size = 32, face = "bold"),
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
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "What Percentage of Time Do You Spend on Technology?", y = "Occupational Class", title = "Which Occupational Classes Spend the Most Time on Technology?", subtitle = "High and Low Indicate Classification in Previous Scholarship", caption = "Data: GSS 2014") +
  scale_x_continuous(labels = function(x) paste0(x, "%"))


ggsave(file="D://techwork/images/usetech_mean.png", type = "cairo-png", width = 21, height = 15)
