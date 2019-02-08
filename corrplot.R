library(ggcorrplot)

small <- com %>% 
  select(mean, polint, educ, kids, income) %>% 
  rename(`Tech. Usage` = mean, `Pol. Interest` = polint, `Education` = educ, `Have Kids` = kids, `Income` = income) %>% 
  na.omit()

plot <- cor(small)


ggcorrplot(plot, lab = TRUE) + 
  labs(title = "Correlation Coefficients", x = "", y = "") +
  bar_rb() + theme(legend.position = "right") + theme(legend.text=element_text(size=12)) + theme(axis.text.y = element_text(family = "Product Sans", size =24, angle = 0, hjust = 1))




ggsave(file="D://techwork/images/corrplot.png", type = "cairo-png", width = 18, height = 10) 
