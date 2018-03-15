## All this to create a spreadsheet which was used for figuring out job classes
# library(haven)
# anes16 <- read_dta("D://techwork/anes_timeseries_2016.dta")
# 
# 
# jj <- anes16  %>% 
#   mutate(jobs = to_factor(V161291b)) %>% 
#   tabyl(jobs)
# 
# write.csv(jj, "job_lists_anes16.csv")


## Now, we recode our variables 

# anes16 <- read.fst("C://anes16.fst")

anes16 <- anes16 %>% 
  mutate(jobs = recode(V161291b, "1:7=1; 8:16=2; 17:29=3; 30:35=4; 36:54=5; 55:59=6; 60:66=7; 67:79=8; 80:95=9; 96:98=10; else = 99"))

anes16 <- anes16 %>% 
  mutate(jobs = recode(jobs, "1 = 'Management';
                              2 = 'Technology, Engineering and Science';
                              3 = 'Education, Legal, Media';
                              4 = 'Health and Technology';
                              5 = 'Service';
                              6 = 'Sales';
                              7 = 'Office and Admin Support';
                              8 = 'Construction and Maintenance';
                              9 = 'Production and Transportation';
                              10 = 'Military'"))

anes16 <- anes16 %>% 
  mutate(know1 = recode(V162072,  "1=1; else =0")) %>% 
  mutate(know2 = recode(V162073a, "1=1; else =0")) %>% 
  mutate(know3 = recode(V162074a, "1=1; else =0")) %>% 
  mutate(know4 = recode(V162075a, "1=1; else =0")) %>% 
  mutate(know5 = recode(V162076a, "1=1; else =0")) %>% 
  mutate(know = know1 + know2 + know3 + know4 + know5)

anes16 <- anes16 %>% 
  mutate(polint = 6 - V161003) 
  

anes16 <- anes16 %>% 
  mutate(gender = recode(V161342, "1 = 'Male'; 2 = 'Female'; else = 99"))

anes16 <- anes16 %>% 
  mutate(income = recode(V161361x, "-5=0; -9=0")) %>% 
  mutate(married = recode(V161268, "1=1; else=0")) %>% 
  mutate(kids = recode(V161324, "-9=0")) %>% 
  mutate(educ = recode(V161270, "-9=0; 90=0; 95=0"))
  
  

### Take our usetech variable and find the mean, then join

gss <- gss %>% 
  mutate(newocc = recode(occ10, "0:999 = 'Management';
                         1000:1999 = 'Technology, Engineering and Science';
                         2000:2999 = 'Education, Legal, Media';
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

com <- left_join(anes16, mean)




