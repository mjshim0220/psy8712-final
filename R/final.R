#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

#Load the dataframe
safe<-read_csv("/Users/mj/IN PROGRESS Research/R projects/psy8712-final/data/DATA_2020_6th_KWCS_eng_220210.csv")

safe_use<-safe %>% 
  select(country, emp_fptime, edu, job1, emp_place,
         emp_type,
         satisfaction, 
         sleep1, sleep2, sleep3,
         weng1, weng2, weng3,
         wsituation1, wsituation2, wsituation11)

#filtering the employee only
data<-safe_use %>% 
  filter(emp_type==3)

#remove the response asnwering "no opinion" or "refuse" or "N/A")
data <- subset(data, !(satisfaction%in% c(8, 9)))
data <- subset(data, !(edu%in% 9))
data <- subset(data, !(country%in% c(8, 9)))
data <- subset(data, !(job1%in% c(8,9)))
data <- subset(data, !(emp_fptime%in% c(8,9)))
data <- subset(data, !(emp_place%in% c(8,9)))
data <- data[!(data$sleep1 %in% c(8, 9) | data$sleep2 %in% c(8, 9) | data$sleep3 %in% c(8, 9) | data$weng1 %in% c(8, 9) | data$weng2 %in% c(8, 9) | data$weng3 %in% c(8, 9)), ]
data <- subset(data, !(wsituation1%in% c(7,8,9)))
data <- subset(data, !(wsituation2%in% c(7,8,9)))
data <- subset(data, !(wsituation11%in% c(7,8,9)))

data <- data %>%
  rowwise() %>%
  mutate(engagement = mean(c_across(starts_with("weng")))) %>% 
  mutate(sleep_q = mean(c_across(starts_with("sleep"))))

data <- data %>% 
  rename(p_support = wsituation1) %>% 
  rename(m_support = wsituation2) %>% 
  rename(justica = wsituation11)
