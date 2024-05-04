#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)
library(broom)
library(caret)
library(parallel)
library(doParallel)

#Load the dataframe
safe<-read_csv("/Users/mj/IN PROGRESS Research/R projects/psy8712-final/data/DATA_2020_6th_KWCS_eng_220210.csv")

safe_use <- safe %>% 
  select(wtime_length5, emp_fptime, edu, job1, emp_place,
         emp_type,
         satisfaction, 
         sleep1, sleep2, sleep3,
         weng1, weng2, weng3,
         wsituation1, wsituation2, wsituation11)

#filtering the employee only
data <- safe_use %>% 
  filter(emp_type==3)

#remove the response asnwering "no opinion" or "refuse" or "N/A")
data <- subset(data, !(satisfaction%in% c(8, 9)))
data <- subset(data, !(edu%in% 9))
data <- subset(data, !(wtime_length5%in% c(8, 9)))
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
  rename(justice = wsituation11)

# 
# data <- data %>% 
#   mutate(shift = factor(wtime_length5, levels = c(1, 2), labels = c("shift", "fixed"))) %>% 
#   mutate(format = emp_fptime, levels = c(1, 2), labels = c("full time", "part time")) %>% 
#   mutate(edu = factor(edu, labels = c("lower than primary education", "Primary education", "Lower secondary education", "Upper secondary education", "Community college", "Bachelor", "Master/PhD"))) %>% 
#   mutate(sidejob = factor(job1, levels = c("1","2","3"), labels = c("Single job holder", "Multiple job holder", "Multiple job holder"))) %>% 
#   mutate(remote = factor(emp_place, levels = c("1","2"), labels = c("Remote workers", "Office workers")))
# str(data)

# Remote workers vs. office workers engagment
# Convert 'remote_worker' to a factor
data$remote <- factor(data$emp_place, labels = c("Yes", "No"))

ggplot(data, aes(y=engagement, x = remote, fill = remote)) +
  geom_boxplot() +
  theme_classic()

model1<- t.test(engagement ~ remote, data=data)
model1


#Descriptive statistic table
data_use <- data %>% 
  select(p_support, m_support, justice, sleep_q, satisfaction, engagement)
mean <- colMeans(data_use)
sds<-apply(data_use, 2, sd)
table_ds <- data.frame(
  Variable = c("Peer Support", "Manager Support", "Perceived Justice", "Sleep Quality", "Satisfaction", "Engagement"),
  Mean = mean,
  SD = sds,
  row.names = TRUE
)
print(table_ds)

#Chart1
hist_satisfaction <- ggplot(data, aes(x = engagement)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Histogram of Engagement",
       x = "Engagement",
       y = "Frequency")
print(hist_satisfaction)

#correlation between support from sleep quality and engagement
plot1 <- ggplot(data = data_use, aes(x = sleep_q, y = engagement)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sleep Quality", y = "Engagement") +
  ggtitle("The Relationship between Sleep Quality and Engagement")
print(plot1)

#correation between perceived justice within the organization and engagement
plot2 <- ggplot(data = data_use, aes(x = justice, y = engagement)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Perceived Justice", y = "Engagement") +
  ggtitle("The Relationship between Perceived Justice within Organization and Engagement")
print(plot2)

# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
#Analysis
set.seed(0220)
train_spilit <- createDataPartition(data$engagement, 
                                    p = 0.75, 
                                    list = FALSE) #spilt the data into 75%/25%
train <- data[train_spilit, ] #save 75% of df for ML
test <- data[-train_spilit, ] #save 25% of df for test 

ctrl<-trainControl(method = "cv",
                   number = 10)   # get estimates of both 10-fold CV and holdout CV

#OLS regression model
ols_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                   data = train, 
                   method = "lm",
                   trControl = ctrl,
                   preProcess = "center",
                   na.action=na.pass)

# Elastic Net model
elastic_net_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                           data = train, 
                           method = "glmnet",
                           trControl = ctrl,
                           preProcess = "center",
                           na.action = na.pass)
# Random Forest model
rf_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                  data = train, 
                  method = "ranger",
                  trControl = ctrl,
                  preProcess = "center",
                  na.action = na.pass)

#eXtreme Gradient Boosting model
xgb_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                   data = train, 
                   method = "xgbLinear",
                   trControl = ctrl,
                   preProcess = "center",
                   na.action = na.pass)

#CV prediction
ols_pred <- predict(ols_model, test)
net_pred <- predict(elastic_net_model, test)
rf_pred <- predict(rf_model, test)
xgb_pred <- predict(xgb_model, test)

# Evaluate holdout CV
ols_rmse<-cor(test$engagement, ols_pred)^2
net_rmse<-cor(test$engagement, net_pred)^2
rf_rmse<-cor(test$engagement, rf_pred)^2
xgb_rmse<-cor(test$engagement, xgb_pred)^2

#Publication
table2_tbl<-tibble(
  algo = c( "OLS regression", "Elastic net", "Random forest", "eXtreme Gradient Boosting"),
  cv_rsq= c(
    round(ols_model$results$Rsquared[1],2),
    round(elastic_net_model$results$Rsquared[1],2),
    round(rf_model$results$Rsquared[1],2),
    round(xgb_model$results$Rsquared[1],2)
  ),
  ho_rsq=c(
    round(ols_rmse,2),
    round(net_rmse,2),
    round(rf_rmse,2),
    round(xgb_rmse,2)
  )
)

table2_tbl



