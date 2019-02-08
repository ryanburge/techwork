anes16 <- read.fst("C://anes16.fst")

anes16 <- anes16 %>% 
  mutate(know1 = recode(V162072,  "1=1; 0=0; else =NA")) %>% 
  mutate(know2 = recode(V162073a, "1=1; 0=0; else =NA")) %>% 
  mutate(know3 = recode(V162074a, "1=1; 0=0; else =NA")) %>% 
  mutate(know4 = recode(V162075a, "1=1; 0=0; else =NA")) %>% 
  mutate(know5 = recode(V162076a, "1=1; 0=0; else =NA")) %>% 
  mutate(know = know1 + know2 + know3 + know4 + know5)


anes16 <- anes16 %>% 
  mutate(gender = recode(V161342, "1 = 'Male'; 2 = 'Female'; else = 99"))


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


bar_theme <- function(base_size = 12, base_family = "Product Sans") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 32, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 16, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =16),
       axis.title.y =  element_text(family = "Product Sans", size =16), 
       legend.text=element_text(size=36)
)
  
}

t %>% 
  filter(know != "NA") %>% 
  mutate(pct = round(pct, 3)) %>% 
  ggplot(., aes(x=know, y = pct, fill = gender)) + geom_col(color = "black", show.legend = FALSE) + 
  bar_theme() +
  labs(x = "Number of Questions Correct", y = "Percent", title = "Political Knowledge", subtitle = "Mean Number Correct for Women: 2.74, For Men: 3.16") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = .5)) + 
  facet_grid(.~ gender) + 
  scale_fill_manual(values = col) + 
  geom_text(aes(y = pct + .005, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4, family = "Product Sans") +
  ggsave("D://techwork/sscr/friesen_color_fig4.png",  dpi = 300)


t %>% 
  filter(know != "NA") %>% 
  mutate(pct = round(pct, 3)) %>% 
  ggplot(., aes(x=know, y = pct, fill = gender)) + geom_col(color = "black", show.legend = FALSE) + 
  bar_theme() +
  labs(x = "Number of Questions Correct", y = "Percent", title = "Political Knowledge", subtitle = "Mean Number Correct for Women: 2.74, For Men: 3.16") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 0, hjust = .5)) + 
  facet_grid(.~ gender) + 
  scale_fill_grey() + 
  geom_text(aes(y = pct + .005, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4, family = "Product Sans") +
  ggsave("D://techwork/sscr/friesen_grayscale_fig4.png",  dpi = 300)


  