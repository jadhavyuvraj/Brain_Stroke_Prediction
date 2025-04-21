library(tidyverse)
library(naniar)
library(randomForest)
library(caret)
library(ROSE)
library(ROCR)

data <- read.csv("D:\\Data Science\\Health Stroke.csv")
str(data)
data$gender <- as.factor(data$gender)
data$ever_married <- as.factor(data$ever_married)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
data$stroke <- factor(data$stroke, levels = c(0,1), labels = c("0","1"))
data$heart_disease <- factor(data$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
data$hypertension <- factor(data$hypertension, levels = c(0,1), labels = c("No", "Yes"))
data$bmi <- as.numeric(data$bmi)

str(data)
data%>%
  head(50)
t(t(sapply(data, function(x) sum(is.na(x)))))
gg_miss_var(data)
avgbmi <- data%>%
  group_by(gender)%>%
  summarise(avg_bmi = mean(bmi,na.rm = TRUE))

avgbmi

data$bmi <- ifelse(is.na(data$bmi)==TRUE,
                   avgbmi$avg_bmi[avgbmi$gender %in% data$gender],
                   data$bmi)
gg_miss_var(data)
numvars <- c('age','avg_glucose_level','bmi')
for (i in numvars){
  g <- data%>%
    ggplot(aes_string(x = i))+geom_histogram(color = 'black',fill = 'pink',binwidth = 3
    )+labs(title = i,y = 'frequency')
  plot(g)
  
}
data%>%
  group_by(age)%>%
  mutate(stroke = as.numeric(stroke))%>%
  summarise(strokesbyage = sum(stroke[stroke ==2]))%>%
  dplyr::filter(strokesbyage != 0)%>%
  ggplot(aes(x = age,weight = strokesbyage))+geom_histogram(fill = 'orange',color = 'black')+labs(
    title = 'Age distribution of patients who had strokes',
    y = 'Frequency'
  )


data%>%
  filter(stroke == 1 )%>%
  group_by(bmi)%>%
  count(stroke)%>%
  ggplot(aes(x = bmi,weight = n))+geom_histogram(fill = 'skyblue',color = 'black')+labs(
    title = 'BMI Distribution of Patients Who Had Strokes',
    y = "Frequency"
  )
data%>%
  filter(stroke == 1)%>%
  group_by(avg_glucose_level)%>%
  count(stroke)%>%
  ggplot(aes(x = avg_glucose_level,weight = n))+geom_histogram(fill = 'lightblue',color = 'black')+labs(
    title = 'Average Glucose Distribution of Patients Who Had Strokes',
    y = "Frequency"
  )

data%>%
  ggplot(aes(x = stroke))+geom_bar(fill = 'lightgreen')+labs(
    x = 'Had Stroke?',
    y = 'Count'
  )
#Setting seed for replication
set.seed(2401)
#Creating training and test sets
split <- sort(sample(nrow(data), nrow(data) * 0.7))
data_train <- data[split,]
data_test <- data[-split,]
glm_stroke <- glm(
  stroke ~ gender + age + hypertension + heart_disease + ever_married + 
    work_type + Residence_type + avg_glucose_level + bmi + smoking_status,
  data = data_train,
  family = binomial
)
summary(glm_stroke)
anova(glm_stroke, test = 'Chisq')
pscl::pR2(glm_stroke)["McFadden"]
glm_stroke_bic <- step(
  glm_stroke,
  direction = "both",
  trace = 0,
  k = log(nrow(data))
)
levels(glm_predict$pred)
levels(data_test$stroke)

summary(glm_stroke_bic)
anova(glm_stroke_bic, test = 'Chisq')
pscl::pR2(glm_stroke_bic)["McFadden"]
library(ROCR)
roc_pred <- prediction(
  predictions = glm_stroke_bic$fitted.values,
  labels = data_train$stroke
)

roc_perf <- performance(
  roc_pred,
  measure = "tpr",
  x.measure = "fpr"
)

roc_curve <- data.frame(
  Spec = 1 - unlist(roc_perf@x.values),
  Sens = unlist(roc_perf@y.values),
  thresh = unlist(roc_perf@alpha.values)
)

roc_curve$distance <- sqrt((1 - roc_curve$Spec)^2 + (1 - roc_curve$Sens)^2)

opt <- roc_curve %>%
  slice(
    distance %>% which.min()
  )

plot(
  roc_perf,
  main = "Logistic Regression for Strokes"
)

abline(0, 1, col = "grey80")
#Optimal Threshold
abline(v = 1 - opt$Spec, col = "gray80")
abline(h = opt$Sens, col = "gray80")
glm_predict <- predict.glm(glm_stroke_bic,
                           newdata =  data_test,
                           type = "response",
                           se.fit = FALSE) %>% as_tibble()
glm_predict$pred <- ifelse(glm_predict$value >= opt$thresh, 1, 0)
glm_predict <- glm_predict %>%
  mutate(
    pred = factor(pred)
  )
library(caret)
confusionMatrix(
  data = glm_predict$pred,
  reference = data_test$stroke,
  positive = "1"
)
data%>%
  group_by(stroke)%>%
  count(stroke)
set.seed(7)
sample_index <- sample(nrow(data),nrow(data)*0.8)
data_train <- data[sample_index,]
data_test <- data[-sample_index,]
forest1 <- randomForest(stroke~.-id,data = data_train,ntree = 1000,mtry = 5)
forest1
errorvalues <- vector()
for (i in 3:10){
  temprf <- randomForest(stroke~.-id,data = data_train,ntree = 1000,mtry = i)
  errorvalues[i] <- temprf$err.rate[nrow(temprf$err.rate),1]
}

plot(errorvalues)
forest2 <- randomForest(stroke~.-id,data = data_train,ntree = 1000,mtry = 3)
forest2
errorrate <- data.frame(
  Trees = rep(1:nrow(forest2$err.rate)),
  Error = forest2$err.rate[,'OOB']
)

errorrate%>%
  ggplot(aes(x = Trees, y = Error))+geom_line(color = 'brown',)
minerrorpoints <- errorrate$Tree[errorrate$Error == min(errorrate$Error)]

minerrorpoints
errorrate%>%
  ggplot(aes(x = Trees, y = Error))+geom_line(color = 'blue')+geom_vline(xintercept = minerrorpoints[1])
forest3 <- randomForest(stroke~.-id,data = data_train,ntree = minerrorpoints[1],mtry = 3)
forest3
predictions <-predict(forest3,newdata = data_test,type = 'prob')
predictions <- predictions[,2] 


hist(predictions)
g <- predict(forest3, newdata = data_test[,-12])
cf <- confusionMatrix(data_test$stroke,g)

cf
#Oversampling the data since the specificity is NA
data2 <- ovun.sample(stroke~.,data = data, method = 'over',p = 0.3)$data
#Confirming that the proportion of "yes" is higher now
table(data2$stroke)
sample_index <- sample(nrow(data2),nrow(data2)*0.8)
data_train2 <- data2[sample_index,]
data_test2 <- data2[-sample_index,]
forest1 <- randomForest(stroke~.-id,data = data_train2,ntree = 2000,mtry = 5)
forest1
errorvalues <- vector()
for (i in 3:10){
  temprf <- randomForest(stroke~.-id,data = data_train2,ntree = 2000,mtry = i)
  errorvalues[i] <- temprf$err.rate[nrow(temprf$err.rate),1]
}

plot(errorvalues)
#Creating a new RF with the optimal number of variables
forest2 <- randomForest(stroke~.-id,data = data_train2,ntree = 2000,mtry = 4)
forest2
#Finding and plotting the optimal number of trees
errorrate <- data.frame(
  Trees = rep(1:nrow(forest2$err.rate)),
  Error = forest2$err.rate[,'OOB']
)

minerrorpoints <- errorrate$Trees[errorrate$Error == min(errorrate$Error)]

minerrorpoints
errorrate%>%
  ggplot(aes(x = Trees, y = Error))+geom_line(color = 'green')+geom_vline(xintercept = minerrorpoints[1])
forest3 <- randomForest(stroke~.-id,data = data_train2,ntree = minerrorpoints[1],mtry = 4)
forest3
g <- predict(forest3, newdata = data_test[,-12])
cf <- confusionMatrix(data_test$stroke,g)

cf
#Creating and modifying a vector of predictions
predictions <-predict(forest3,newdata = data_test,type = 'prob')

predictions <- predictions[,2]

#Viewing distribution of predictions
hist(predictions)
varImp2 <- varImp(forest3)


varImp2$labels <- factor(rownames(varImp2))
varImp2$labels <- reorder(varImp2$labels, varImp2$Overall)

varImp2<-varImp2%>%
  arrange(-Overall)

varImp2%>%
  ggplot(aes(x=labels,y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='red') + xlab(" Importance Score")+
  ggtitle("Variable Importance") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
#ROC and AUC
predictions <-predict(forest3,newdata = data_test[,-12],type = 'prob')

predictions <- predictions[,2]

pred <- prediction(predictions,data_test$stroke)

myauc <- slot(performance(pred, "auc"), "y.values")[[1]]

perf <- performance(pred, 'tpr','fpr')
plot(perf,colorize = TRUE)

myauc
