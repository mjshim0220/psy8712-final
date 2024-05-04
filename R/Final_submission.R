# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)
library(broom)
library(caret)
library(parallel)
library(doParallel)


##Load the dataframe
###Richard, I used the Korean panel data. Since this data is accessible for those who have the approve from research institute through web, I disclose the raw data. Please let me know if you need the rawdata I used for this project. Sorry for this inconvenience.
# safe<-read_csv("/Users/mj/IN PROGRESS Research/R projects/psy8712-final/data/DATA_2020_6th_KWCS_eng_220210.csv") #load the dataframe
# 
# safe_use <- safe %>% 
#   select(wtime_length5, emp_fptime, edu, job1, emp_place,
#          emp_type,
#          satisfaction, 
#          sleep1, sleep2, sleep3,
#          weng1, weng2, weng3,
#          wsituation1, wsituation2, wsituation11) #select the variables I will use in this project
# write.csv(safe_use, "../data/safe_use.csv")
safe_use<-read_csv("../data/safe_use.csv")
##Data Cleansing
data <- safe_use %>% 
  filter(emp_type==3) #filtering only the employees.
filter_89_cols <- c("satisfaction", "wtime_length5", "job1", "emp_fptime", "emp_place", "sleep1", "sleep2", "sleep3", "weng1", "weng2", "weng3") #Filter out responses where individuals answered 8 or 9 for these items.
data <- subset(data, !apply(data[filter_89_cols], 1, function(x) any(x %in% c(8, 9))))
filter_789_cols <- c("wsituation1", "wsituation2", "wsituation11")
data <- data[!apply(data[filter_789_cols], 1, function(x) any(x %in% c(7, 8, 9))), ] #Filter out responses where individuals answered 7, 8 or 9 for these items.

##Create variables
data <- data %>%
  rowwise() %>%
  mutate(engagement = mean(c_across(starts_with("weng")))) %>% #calculate the average value of weng1,2,3
  mutate(sleep_q = mean(c_across(starts_with("sleep"))))

data <- data %>% 
  rename(p_support = wsituation1) %>% #Rename the item code to a proper variable name.
  rename(m_support = wsituation2) %>% 
  rename(justice = wsituation11) 

#Reverse coding
data <- data %>% 
  mutate(engagement = 6-engagement) %>% 
  mutate(sleep_q = 6 - sleep_q) %>% 
  mutate(p_support = 6-p_support) %>% 
  mutate(m_support = 6-m_support) %>% 
  mutate(justice = 6-justice)

# Analysis
###1. Descriptive statistic
data_use <- data %>% 
  select(p_support, m_support, justice, sleep_q, satisfaction, engagement) 

mean <- colMeans(data_use) #calculate the mean of each variable (column-wise)

sds<-apply(data_use, 2, sd) #calculates the standard deviation of each column 

table_ds <- data.frame(
  Variable = c("Peer Support", "Manager Support", "Perceived Justice", "Sleep Quality", "Satisfaction", "Engagement"),
  Mean = mean,
  SD = sds,
  row.names = TRUE
) #create a descriptive statistic result
print(table_ds) 

###2. Histogram of DV
hist_eng <- ggplot(data, aes(x = engagement)) +
  geom_histogram(bins=10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Engagement",
       x = "Engagement",
       y = "Frequency")
print(hist_eng)

###3. H1
data$remote <- factor(data$emp_place, labels = c("Yes", "No")) #Convert the values into factors with 1:yes, 2:no

fig2 <- ggplot(data, aes(y=engagement, x = remote, fill = remote)) +
  labs(x = "Work mode",y = "Engagement", fill= "Do you work remotely?")+
  geom_boxplot() +
  theme_classic()

fig2

###4. H1 - t-test
model1<- t.test(engagement ~ remote, data=data) #t-test
model1

###5. H2. Scatterplot
fig3 <- ggplot(data = data_use, aes(x = sleep_q, y = engagement)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sleep Quality", y = "Engagement")
print(fig3)

###6. H2 - Correlation analysis
cor.test(data_use$sleep_q, data_use$engagement, method=c("pearson"))

###7. H3 model building
cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl) # Set up parallel processing
set.seed(0220) # set the seed for reproducibility
train_spilit <- createDataPartition(data$engagement, 
                                    p = 0.75, 
                                    list = FALSE) #spilt the data into 75%/25%
train <- data[train_spilit, ] #save 75% of df for ML
test <- data[-train_spilit, ] #save 25% of df for test 

ctrl<-trainControl(method = "cv", #cv for cross validation
                   number = 10)   # get estimates of both 10-fold CV and holdout CV

####OLS regression model
ols_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                   data = train, 
                   method = "lm",
                   trControl = ctrl,
                   preProcess = "center") #since this secondary data uses the likert scale, centering approach is most viable
                   

####Elastic Net model
elastic_net_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                           data = train, 
                           method = "glmnet",
                           trControl = ctrl,
                           preProcess = "center")
####Random Forest model
rf_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                  data = train, 
                  method = "ranger",
                  trControl = ctrl,
                  preProcess = "center")

####eXtreme Gradient Boosting model
xgb_model <- train(engagement ~ p_support + m_support + sleep_q + justice,
                   data = train, 
                   method = "xgbLinear",
                   trControl = ctrl,
                   preProcess = "center")

####CV prediction
ols_pred <- predict(ols_model, test)
net_pred <- predict(elastic_net_model, test)
rf_pred <- predict(rf_model, test)
xgb_pred <- predict(xgb_model, test)

####Evaluate holdout CV
ols_rmse<-cor(test$engagement, ols_pred)^2
net_rmse<-cor(test$engagement, net_pred)^2
rf_rmse<-cor(test$engagement, rf_pred)^2
xgb_rmse<-cor(test$engagement, xgb_pred)^2

####Result table
table2_tbl<-tibble(
  Algorithm = c( "OLS regression", "Elastic net", "Random forest", "eXtreme Gradient Boosting"),
  cv_rsq= c(
    round(ols_model$results$Rsquared[1],3), #three decimals
    round(elastic_net_model$results$Rsquared[1],3),
    round(rf_model$results$Rsquared[1],3),
    round(xgb_model$results$Rsquared[1],3)
  ),
  ho_rsq=c(
    round(ols_rmse,3),
    round(net_rmse,3),
    round(rf_rmse,3),
    round(xgb_rmse,3)
  )
)

table2_tbl

# Publication
write.csv(table_ds, "../out/table1.csv")
ggsave("../figs/figure1.png", plot = hist_eng )
ggsave("../figs/figure2.png", plot = fig2)
ggsave("../figs/figure3.png", plot = fig3)
write.csv(table2_tbl, "../out/table2.csv")

