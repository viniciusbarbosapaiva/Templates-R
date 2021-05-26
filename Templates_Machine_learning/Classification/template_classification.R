# Regression Template
# Data available on https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)
# set diretory
setwd("~/Desktop/FCD")
getwd()

#---------------------------------------------------------------------------------#
# Importing Lybraries
install.packages("Amelia")
install.packages('lubridate')
library(ggplot2)
library(dplyr)
library(Amelia)
library(lubridate)
library(corrplot)
library(ggthemes)
library(ROCR)
library(data.table)
library(skimr)

#---------------------------------------------------------------------------------#
# Functions That Will be Used on Project
# Transforming variables to factor
numeric_to_factor <- function(x,y){
  grade <- sort(unique(x))
  grade_name <- y
  grade_mod <- c()
  for(i in x){
    for(k in 1:length(grade)){
      if(i==grade[k]){
        grade_mod <- append(grade_mod,grade_name[k])
      }
    }
  }
  x <- grade_mod
  x <- factor(x,levels = grade_name, labels = grade_name)
}

# Plotting ROC
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf, col = "black", lty = 1, lwd = 2, 
       main = title.text, cex.main = 0.6, 
       cex.lab = 0.8, xaxs="i", yaxs="i")
  abline(0,1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4, legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
  
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
}

#---------------------------------------------------------------------------------#
# Importing the dataset
df = read.csv('credito.csv', header = FALSE)
typeof(df$A11)
dim(df)
nrow(df)
ncol(df)
str(df)
View(df)
head(df)
tail(df)
summary(df)

#---------------------------------------------------------------------------------#
# looking for null values
null_values <- c()
for (i in 1:length(df)){
  sum_null_values <- (sum(is.na(df[,i]))/dim(df)[1])*100
  null_values <- append(null_values,sum_null_values)
}
null_values <- data_frame(round(null_values,2))
names(null_values) <- c('% of Null Values')
row.names(null_values) <- c(colnames(df))
null_values

missmap(df, 
        main = "German Credit Data - Missing Values Map", 
        col = c("yellow", "black"), 
        legend = TRUE)

#---------------------------------------------------------------------------------#
##### Feature Engineering #####
#### Deleting unused Columns ####
### Deleting instant column ###
df_feature <- df 
df_feature$instant <- NULL
View(df_feature)

### Changing Columns Name ###
new_columns <- 'CheckingAcctStat, Duration, CreditHistory, Purpose, CreditAmount, SavingsBonds, Employment, InstallmentRatePecnt, SexAndStatus, OtherDetorsGuarantors, PresentResidenceTime, Property, Age, OtherInstallments, Housing, ExistingCreditsAtBank, Job, NumberDependents, Telephone, ForeignWorker, CreditStatus'
new_columns <- strsplit(new_columns,', ')
names(df_feature) <- unlist(new_columns)
View(df_feature)

#---------------------------------------------------------------------------------#
# Converting a numeric variable to factor variable
str(df_feature)
View(df_feature)

df_feature$CheckingAcctStat <- numeric_to_factor(df_feature$CheckingAcctStat,
                                       c('... < 0 DM', '0 <= ... < 200 DM',
                                         '... >= 200 DM / salary assignments for at least 1 year',
                                         'no checking account'))

df_feature$CreditHistory <- numeric_to_factor(df_feature$CreditHistory,
                                   c('no credits taken/ all credits paid back duly',
                                     'all credits at this bank paid back duly',
                                     'existing credits paid back duly till now',
                                     'delay in paying off in the past',
                                     'critical account/ other credits existing (not at this bank)'))

df_feature$Purpose <- numeric_to_factor(df_feature$Purpose,
                                        c('car (new)',
                                          'car (used)',
                                          'furniture/equipment',
                                          'radio/television',
                                          'domestic appliances',
                                          'repairs',
                                          'education',
                                          '(vacation - does not exist?)',
                                          'retraining',
                                          'business',
                                          'others'))

df_feature$SavingsBonds <- numeric_to_factor(df_feature$SavingsBonds,
                                              c('... < 100 DM',
                                                '100 <= ... < 500 DM',
                                                '500 <= ... < 1000 DM',
                                                '.. >= 1000 DM',
                                                'unknown/ no savings account'))

df_feature$Employment <- numeric_to_factor(df_feature$Employment,
                                             c('unemployed',
                                               '... < 1 year',
                                               '1 <= ... < 4 years',
                                               '4 <= ... < 7 years',
                                               '.. >= 7 years'))

df_feature$SexAndStatus <- numeric_to_factor(df_feature$SexAndStatus,
                                           c('male : divorced/separated',
                                             'female : divorced/separated/married',
                                             'male : single',
                                             'male : married/widowed',
                                             'female : single'))

df_feature$Property <- numeric_to_factor(df_feature$Property,
                                                 c('real estate',
                                                   'building society savings agreement/ life insurance',
                                                   'ar or other, not in attribute 6',
                                                   'unknown / no property'))

df_feature$OtherDetorsGuarantors <- numeric_to_factor(df_feature$OtherDetorsGuarantors,
                                                      c('none',
                                                        'co-applicant',
                                                        'guarantor'))

df_feature$OtherInstallments <- numeric_to_factor(df_feature$OtherInstallments,
                                                      c('bank',
                                                        'stores',
                                                        'none'))

df_feature$Housing <- numeric_to_factor(df_feature$Housing,
                                                  c('rent',
                                                    'own',
                                                    'for free'))

df_feature$Job <- numeric_to_factor(df_feature$Job,
                                         c('unemployed/ unskilled - non-resident',
                                           'unskilled - resident',
                                           'skilled employee / official',
                                           'management/ self-employed/highly qualified employee/ officer'))

df_feature$Telephone <- numeric_to_factor(df_feature$Telephone,
                                        c('none',
                                          'yes, registered under the customers name'))

df_feature$ForeignWorker <- numeric_to_factor(df_feature$ForeignWorker,
                                          c('yes',
                                            'no'))

df_feature$CreditStatus <- numeric_to_factor(df_feature$CreditStatus,
                                              c('good',
                                                'bad'))
str(df_feature)

#---------------------------------------------------------------------------------#
#How many Target Variable?
how_many <- table(df_feature$CreditStatus)
how_many_percentage <- prop.table(table(df_feature$CreditStatus))

barplot(how_many, main = 'Number of Dependent Variable', col=c('gray', 'black'))
barplot(how_many_percentage, main = 'Number of Dependent Variable', col=c('gray', 'black'))

#---------------------------------------------------------------------------------#
# Converting string columns to timestamp columns
str(df_feature)
df_feature$month_f <- lubridate::month(df_feature$dteday, label=TRUE)
df_feature$weekday_f <- lubridate::wday(df_feature$dteday,label=TRUE, week_start=7)
View(df_feature)

#---------------------------------------------------------------------------------#
# Deleting dteday
df_feature$dteday <-NULL
View(df_feature)
str(df_feature)

#---------------------------------------------------------------------------------#
# Plotting
names(df_feature)

month_day_count <- df_feature %>%
  group_by(month_f,weekday_f) %>%
  summarise(cnt_value = sum(cnt))
View(month_day_count)
summary(month_day_count)

ggplot(month_day_count, aes(x= weekday_f, y=cnt_value,
                            fill = factor(weekday_f)))+
  geom_bar(stat='identity', position = position_dodge()) +
  facet_wrap(facet=vars(month_f))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = 'bold'),
        axis.text.y = element_text(face='bold'),
        plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold")) + 
  ggtitle('Bikes Rent By Month X Day of Week') +
  xlab('Day of Week') + 
  ylab('Count of Bike Rents') + 
  labs(fill='Day of Week')

season_group <- df_feature %>%
  group_by(season) %>%
  summarise(cnt_season = sum(cnt))
View(season_group)  

point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
ggplot(season_group, aes(x=season, y=cnt_season, fill=season)) +
  geom_bar(stat='identity') + 
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold')) + 
  scale_y_continuous(labels = point) + 
  ggtitle('Bikes Rent By Season') +
  xlab('Season') + 
  ylab('Count of Bike Rents') + 
  labs(fill='Season') + 
  geom_text(aes(label=cnt_season), position = position_dodge(width = 0.9),
            vjust=-0.25)
View(df_feature)

#---------------------------------------------------------------------------------#
## Correlation between Variables
columns_numeric <- df_feature[,names(select_if(df_feature, is.numeric))]
columns_numeric$cnt <-NULL #Deleting the dependent variable

corrplot(cor(columns_numeric),
         method = "number",
         type = "upper" # show only upper side
)

#---------------------------------------------------------------------------------#
## Correlation with independent Variable
correlation_with_target <- cor(columns_numeric,as.numeric(df_feature$CreditStatus) )
correlation_with_target <- as.data.frame(correlation_with_target)


ggplot(correlation_with_target,aes(x=row.names(correlation_with_target), y=V1, 
                                fill=row.names(correlation_with_target))) + 
  geom_bar(stat='identity') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = 'bold'),
        axis.text.y = element_text(face='bold')) + 
  ggtitle('Correlation between Independent and Dependent Variables (CreditStatus)') + 
  xlab('Independent Variables') + 
  ylab('Pearson Correlation Method ') + 
  labs(fill='Independent Variables') +
  geom_text(aes(label=round(V1,2)), position = position_dodge(width = 0.9),
            vjust=-0.25) 
   
#---------------------------------------------------------------------------------#
# Deleting month and weekday
df_feature$mnth <- NULL
df_feature$weekday <- NULL
View(df_feature)
str(df_feature)

#---------------------------------------------------------------------------------#
# Feature Selection with Random Forest
library(randomForest) 
modelo <- randomForest(CreditStatus ~ . , 
                       data = df_feature, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)
varImpPlot(modelo)
df_saida <- df_feature[, c("CreditStatus", rownames(modelo$importance))]
View(df_saida)
str(df_saida)

#---------------------------------------------------------------------------------#
# Get Dummies and avoiding dummy trap 
#install.packages('fastDummies')
#library(fastDummies)

##df_dummies <- dummy_cols(df_saida, remove_first_dummy = TRUE,
                         #remove_selected_columns=TRUE,
                         #select_columns = names(select_if(df_saida[,names(df_saida)], is.factor)))

#View(df_dummies)
#str(df_dummies)

#---------------------------------------------------------------------------------#
#Feature scaling
var_numeric <- names(select_if(df_saida[,names(df_saida)], is.numeric))
df_temp <- df_saida[,names(select_if(df_saida[,names(df_saida)], is.numeric))]
maxs <- apply(df_temp, 2, max) 
mins <- apply(df_temp, 2, min)

var_numeric_norm <- as.data.frame(scale(df_temp, center = mins, scale = maxs - mins))
View(var_numeric_norm)

dados_norm <- subset(df_saida, select =names(df_saida[!names(df_saida) %in% c(var_numeric)]))
dados_norm <- cbind(dados_norm,var_numeric_norm)
head(dados_norm)
View(dados_norm)

#---------------------------------------------------------------------------------#
# Balancing DataSet
install.packages('ROSE')
library(ROSE)

dados_norm <- ROSE(CreditStatus ~ . , data = dados_norm,seed=1)
dados_norm<- as.data.frame(dados_norm$data)
table(dados_norm$CreditStatus)

#---------------------------------------------------------------------------------#
#Splitting the Dataset into the training set and test set
library(caTools)
split = sample.split(dados_norm$CreditStatus, SplitRatio = 0.70)

treino = subset(dados_norm, split == TRUE)
teste= subset(dados_norm, split == FALSE)

View(treino)
str(treino)
View(teste)

#---------------------------------------------------------------------------------#
#### Model Building ####
### Comparing Models
## Logist Regression
library(caret)
library(MLmetrics)
set.seed(123)
train_control <- trainControl(method = "cv", 
                              number = 10, repeats = 10) 
lr_classifier <- glm(CreditStatus ~ ., data = treino, family = 'binomial')
summary(lr_classifier)

# Predicting Test Set
previsao1 <- predict(lr_classifier, teste, type="response")
previsao1 <- round(previsao1)
previsao1
previsao1_factor <- numeric_to_factor(previsao1,c('good', 'bad'))

# Measuring Performance
cm <- confusionMatrix(table(data = previsao1_factor, reference = teste$CreditStatus), positive = 'good')
cm
new_metric <- table(previsao1_factor,teste$CreditStatus)
new_metric

# Metrics
library (ROCR)
accuracy_model <- cm$overall[[1]]
precision_model <- precision(new_metric)
recall_model <- recall(new_metric)
f1_score_model <- F1_Score(teste$CreditStatus,previsao1_factor,positive = 'good')

results <- data.frame(Model='Logistic Classifier',
                      Accuracy = accuracy_model,
                      Precision = precision_model,
                      Recall = recall_model,
                      F1_Score = recall_model)
results

#---------------------------------------------------------------------------------#
## Random Forest Classifier
set.seed(123)
rf_classifier <- randomForest(CreditStatus ~ . , 
                              data = treino, 
                              ntree = 100, 
                              nodesize = 10,
                              importance = FALSE)
summary(rf_classifier)

# Predicting Test Set
previsao1 <- predict(rf_classifier, teste, type="response")
previsao1

# Measuring Performance
cm <- confusionMatrix(table(data = previsao1, reference = teste$CreditStatus), positive = 'good')
new_metric <- table(previsao1,teste$CreditStatus)

# Metrics
accuracy_model <- cm$overall[[1]]
precision_model <- precision(new_metric)
recall_model <- recall(new_metric)
f1_score_model <- F1_Score(teste$CreditStatus,previsao1_factor,positive = 'good')

model_results <- data.frame(Model='Random Forest Classifier',
                            Accuracy = accuracy_model,
                            Precision = precision_model,
                            Recall = recall_model,
                            F1_Score = recall_model)
results <- rbind(results, model_results) 
results
#---------------------------------------------------------------------------------#
## Support Vector Classifier
library(e1071)
svr_classifier <- svm(CreditStatus ~ ., data = treino, kernel = "linear", cost = 10, scale = FALSE,
                      trControl = train_control, type = 'C-classification')
svr_classifier

# Predicting Test Set
previsao1 <- predict(svr_classifier,teste)
previsao1

# Measuring Performance
cm <- confusionMatrix(table(data = previsao1, reference = teste$CreditStatus), positive = 'good')
new_metric <- table(previsao1,teste$CreditStatus)

# Metrics
accuracy_model <- cm$overall[[1]]
precision_model <- precision(new_metric)
recall_model <- recall(new_metric)
f1_score_model <- F1_Score(teste$CreditStatus,previsao1_factor,positive = 'good')

model_results <- data.frame(Model='SVM Classifier',
                            Accuracy = accuracy_model,
                            Precision = precision_model,
                            Recall = recall_model,
                            F1_Score = recall_model)
results <- rbind(results, model_results) 
results
#---------------------------------------------------------------------------------#
## Decision Tree Classifier
#install.packages("rpart")
library(rpart)
dt_classifier <- rpart(CreditStatus ~ ., data = treino, method='class')
dt_classifier

plot(dt_classifier, uniform=TRUE, 
     main="Regression Tree for CreditStatus")
text(dt_classifier, use.n=TRUE, cex = .6)

# Predicting Test Set
previsao1 <- predict(dt_classifier,teste)
previsao1 <- round(previsao1)
previsao1
previsao1_factor <- numeric_to_factor(previsao1[,1],c('good', 'bad'))
previsao1_factor

# Measuring Performance
cm <- confusionMatrix(table(data = previsao1_factor, reference = teste$CreditStatus), positive = 'good')
new_metric <- table(previsao1_factor,teste$CreditStatus)

# Metrics
accuracy_model <- cm$overall[[1]]
precision_model <- precision(new_metric)
recall_model <- recall(new_metric)
f1_score_model <- F1_Score(teste$CreditStatus,previsao1_factor,positive = 'good')

model_results <- data.frame(Model='Decision Tree Classifier',
                            Accuracy = accuracy_model,
                            Precision = precision_model,
                            Recall = recall_model,
                            F1_Score = recall_model)
results <- rbind(results, model_results) 
results

#---------------------------------------------------------------------------------#
## XGB Classifier
library(xgboost)
xgb_classifier <- train(CreditStatus ~ . , 
                        data = treino, 
                        method = "xgbTree",
                        trControl = train_control)
xgb_classifier

# Predicting Test Set
previsao1 <- predict(xgb_classifier,teste)
previsao1

# Measuring Performance
cm <- confusionMatrix(table(data = previsao1, reference = teste$CreditStatus), positive = 'good')
new_metric <- table(previsao1,teste$CreditStatus)

# Metrics
accuracy_model <- cm$overall[[1]]
precision_model <- precision(new_metric)
recall_model <- recall(new_metric)
f1_score_model <- F1_Score(teste$CreditStatus,previsao1_factor,positive = 'good')

model_results <- data.frame(Model='XGB Classifier',
                            Accuracy = accuracy_model,
                            Precision = precision_model,
                            Recall = recall_model,
                            F1_Score = recall_model)
results <- rbind(results, model_results) 
results
#---------------------------------------------------------------------------------#
# The Best Classifier
best <- results[with(results, order(Accuracy, decreasing = TRUE)),]
best

#---------------------------------------------------------------------------------#
# Plotting ROC
predictions <- prediction(as.numeric(previsao1), as.numeric(teste$CreditStatus))
par(mfrow = c(1,2))
plot.roc.curve(predictions, title.text = "Curva ROC")
plot.pr.curve(predictions, title.text = "Curva Precision/Recall")

#---------------------------------------------------------------------------------#
# Visualyzing predicts and observed values 
resultados <- cbind(previsao1, teste$CreditStatus) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
View(resultados)



