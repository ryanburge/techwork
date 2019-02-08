library(broom)
library(dotwhisker)


com <- com %>% 
  mutate(income = recode(V161361x, "-5=0; -9=0")) %>% 
  mutate(married = recode(V161268, "1=1; else=0")) %>% 
  mutate(kids = recode(V161324, "-9=0")) %>% 
  mutate(educ = recode(V161270, "-9=0; 90=0; 95=0")) 

com <- com %>% 
  mutate(inc2 = income/28) %>% 
  mutate(kids2 = recode(kids, "1:9=1; else=0")) %>% 
  mutate(educ2 = educ/16) %>% 
  mutate(polint2 = polint/5) %>% 
  mutate(usetech = mean/78.19697)

fem <- com %>% filter(gender == "Female")
men <- com %>% filter(gender == "Male")


reg1 <- lm(know ~ usetech + polint2 + educ2 + kids2 + inc2, data = men)
reg2 <- lm(know ~ usetech + polint2 + educ2 + kids2 + inc2, data = fem)

reg1 <- tidy(reg1) %>% mutate(model = "Male")
reg2 <- tidy(reg2) %>% mutate(model = "Female")

fullreg <- bind_rows(reg1, reg2)


regress_theme <- function(base_size = 25, base_family = "Product Sans") 
{theme(panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "azure3", size = .25, linetype = "dashed"), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       panel.grid.major.x =  element_line(colour = "gray48", size = .25, linetype = "dashed"), 
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "Product Sans", size = 34, vjust =2, hjust = .5, face = "bold"),
       plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1),
       plot.caption = element_text(family = "Product Sans", size =20),
       axis.title.x =  element_text(family = "Product Sans", size =24),
       axis.title.y =  element_text(family = "Product Sans", size =24), 
       axis.text.x = element_text(family = "Product Sans", size =18, angle = 0, hjust  =.5),
       legend.justification=c(0, 0), 
       legend.position=c(.75, .65),
       legend.background = element_rect(colour="grey80"), 
       legend.title = element_blank(), 
       legend.text=element_text(size=36)
       
)
  
}



dwplot(fullreg, dodge_size = .15) %>% 
  relabel_predictors(c(educ2 ="Education", kids2 = "Have Kids", polint2 = "Political Interest", usetech = "Technology at Work", inc2 = "Income")) +
  labs(x="Political Knowledge", y="", title="Which Factors Lead to Greater Political Knowledge? ", caption="Data from CCES 2016") + 
  regress_theme() +
  scale_color_manual(values = c("chartreuse4", "brown4"))+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 1) + guides(colour = guide_legend(reverse = TRUE))  +
  ggsave(file="D://techwork/sscr/friesen_color_fig7.png", type = "cairo-png", width = 18, height = 10, dpi = 300) 


dwplot(fullreg, dodge_size = .15) %>% 
  relabel_predictors(c(educ2 ="Education", kids2 = "Have Kids", polint2 = "Political Interest", usetech = "Technology at Work", inc2 = "Income")) +
  labs(x="Political Knowledge", y="", title="Which Factors Lead to Greater Political Knowledge? ", caption="Data from CCES 2016") + 
  regress_theme() +
  scale_color_grey() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 1) + guides(colour = guide_legend(reverse = TRUE))  +
  ggsave(file="D://techwork/sscr/friesen_grayscale_fig7.png", type = "cairo-png", width = 18, height = 10, dpi = 300) 


reg1 <- lm(know ~ usetech + polint2 + educ2 + kids2 + inc2, data = men)
reg2 <- lm(know ~ usetech + polint2 + educ2 + kids2 + inc2, data = fem)

stargazer(reg1, reg2, type = "text", title = "Regression Model in Figure 7", dep.var.labels = c("Predicting Political Knowledge"),
          covariate.labels = c("Tech Usage", "Political Interest", "Education", "Have Kids", "Income"), column.labels = c("Male", "Female"),
          star.cutoffs = c(0.05), out = "D://techwork/images/regression1.htm")

## Checking In a Non-Split Sample ###

com <- com %>% 
  mutate(male = recode(gender, "'Male'=1; else=0"))

reg1 <- lm(know ~ usetech + polint2 + educ2 + kids2 + inc2 + male, data = com)
