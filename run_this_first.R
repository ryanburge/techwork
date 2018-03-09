library(fst)
library(tidyverse)
library(car)
library(janitor)
library(extrafont)

cces08 <- read.fst("C://cces08.fst")
cces10 <- read.fst("C://cces10.fst")
cces12 <- read.fst("C://cces12.fst")
cces14 <- read.fst("C://cces14.fst")
cces16 <- read.fst("C://cces16.fst")

gss <- read.fst("C://gss.fst")


anes <- read.fst("C://anes.fst")

anes16 <- read.fst("C://anes16.fst")

anes16 <- anes16 %>% 
  mutate(newocc = recode())

