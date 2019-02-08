library(fst)
library(tidyverse)
library(car)
library(janitor)
library(extrafont)
library(ggbeeswarm)
library(patchwork)


gss <- read.fst("C://gss.fst") %>% filter(year == 2014)


bar_theme <- function(base_size = 25, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 54, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =32),
       axis.title.y =  element_text(family = "Product Sans", size =32), 
       axis.text.x = element_text(family = "Product Sans", size =24, angle = 45, hjust = 1), 
       legend.text=element_text(size=36)
)
  
}


gss %>% 
  mutate(sex = recode(sex, "1 = 'Male'; 2 = 'Female'")) %>% 
  ggplot(., aes(x= factor(sex), y = usetech, color = factor(sex))) + geom_quasirandom() +
  theme(legend.position="none") +
  bar_theme() +
  scale_color_manual(values = c("brown4", "chartreuse4"))+
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size =24, angle = 0, hjust = .5)) +
  labs(x = "Gender", y = "Percent of Job That Uses Technology", title = "Distribution of Tech Usage", caption = "Data: GSS 2014", subtitle = "Mean Score for Women = 56.1%, Men = 48.6%") +
  theme(plot.title = element_text(size= 24, hjust = 0)) +
  ggsave("D://techwork/sscr/friesen_color_fig1.png", width = 8, height = 10, dpi = 300)


gss %>% 
  mutate(sex = recode(sex, "1 = 'Male'; 2 = 'Female'")) %>% 
  ggplot(., aes(x= factor(sex), y = usetech, color = factor(sex))) + geom_quasirandom() +
  theme(legend.position="none") +
  bar_theme() +
  scale_color_grey() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size =24, angle = 0, hjust = .5)) +
  labs(x = "Gender", y = "Percent of Job That Uses Technology", title = "Distribution of Tech Usage", caption = "Data: GSS 2014", subtitle = "Mean Score for Women = 56.1%, Men = 48.6%") +
  theme(plot.title = element_text(size= 24, hjust = 0)) +
  ggsave("D://techwork/sscr/friesen_grayscale_fig1.png", width = 8, height = 10, dpi = 300)

