com %>% 
  filter(gender != 99) %>% 
  ggplot(., aes(x=mean, y = know)) + geom_point() + geom_smooth(method = lm) + geom_jitter(width = 2.5) +
  labs(x = "Mean Use of Technology by Job Type", y = "Number of Knowledge Questions Correct", title = "Political Knowledge Gain by Tech Use", caption = "Data: ANES 2016") +
  facet_grid(.~ gender) + long_rb() +
  theme(plot.title = element_text(family = "Product Sans", size = 54, vjust =0, face = "bold"))  +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + theme(panel.spacing = unit(2, "lines"))

ggsave(file="images/scatter_facet.png", type = "cairo-png", width = 15, height = 10)

com %>% 
  group_by(gender) %>% 
  na.omit() %>% 
  summarise(cor = cor(mean, know))


com %>% 
  filter(jobs != 99) %>% 
  mutate(attend = as.numeric(know), group  = as.factor(jobs)) %>% 
  ggplot(., aes(x=know, y=jobs, height = ..density.., fill = jobs, alpha =.4)) + geom_density_ridges(stat = "density") +
  mean_rb() +
  scale_x_continuous(limits = c(-1, 6), breaks = c(0, 5),  label = c("Low Knowledge","High Knowledge")) +
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 45, hjust = 1)) + theme(legend.position="none") +
  labs(x = "", y ="", title = "Distribution of Political Knowledge by Occupation", caption = "Data: ANES 2016", subtitle = "") +
  theme(plot.title = element_text(size=24, hjust = -2)) + 
  theme(axis.text.x = element_text(family = "Product Sans", size =16, angle = 0)) +
  scale_fill_brewer(palette="Set3") + theme(axis.text.x = element_text(family = "Product Sans", size =26, hjust = .50))

ggsave(file="images/occ_ridge_new.png", type = "cairo-png", width = 15, height = 10)





