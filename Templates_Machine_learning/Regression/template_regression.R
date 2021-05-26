# Regression Template
# Data available on https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset
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

# Feature Scaling
denormalized <- function(x,y){
  new_data <-(x) * (max(y) - min(y)) + min(y)
  return(new_data)
}

#---------------------------------------------------------------------------------#
# Importing the dataset
df = read.csv('bikes.csv')
typeof(df$instant)
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
        main = "Bikes Data - Missing Values Map", 
        col = c("yellow", "black"), 
        legend = TRUE)

#---------------------------------------------------------------------------------#
##### Feature Engineering #####
#### Deleting unused Columns ####
### Deleting instant column ###
df_feature <- df 
df_feature$instant <- NULL

### Deleting Sunday and Saturday rows from weekday column ###
weekday_sum <- df_feature %>%
  group_by(weekday)
weekday_sum %>% summarise(
  workingday = sum(workingday),
  cnt = sum(cnt)
)

View(df_feature)

#---------------------------------------------------------------------------------#
# Converting a numeric variable to factor variable
str(df_feature)
View(df_feature)
df_feature$season <- numeric_to_factor(df_feature$season,
                                       c('winter', 'spring', 'summer', 'fall'))
df_feature$yr <- numeric_to_factor(df_feature$yr,
                                   c('2011','2012'))
df_feature$weathersit <- numeric_to_factor(df_feature$weathersit,
                                        c('clear', 'mist', 'light', 'heavy'))
df_feature$holiday <- numeric_to_factor(df_feature$holiday,
                                        c('no', 'yes'))
df_feature$workingday <- numeric_to_factor(df_feature$workingday,
                                        c('no', 'yes'))

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
modelo <- randomForest(cnt ~ . , 
                       data = df_feature, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)
varImpPlot(modelo)
df_saida <- df_feature[, c("cnt", rownames(modelo$importance))]
View(df_saida)

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
#Splitting the Dataset into the training set and test set
library(caTools)
split = sample.split(dados_norm$cnt, SplitRatio = 0.70)


treino = subset(dados_norm, split == TRUE)
teste= subset(dados_norm, split == FALSE)

View(treino)
View(teste)

#---------------------------------------------------------------------------------#
#### Model Building ####
### Comparing Models
## Multiple Linear Regression Regression
library(caret)
set.seed(123)
train_control <- trainControl(method = "cv", 
                              number = 10) 
k = dim(teste)[2]
n = dim(teste)[1]
columns_names <- names(treino[!names(treino) %in% c('cnt')])
columns_names

formula <- as.formula(paste("cnt ~", 
                            paste(columns_names, collapse = " + ")))
lr_regression <- lm(formula, data = treino, trControl = train_control)
lr_regression

# Predicting Test Set
previsao1 <- predict(lr_regression,teste)
previsao1 <- as.numeric(previsao1)
View(previsao1)
str(previsao1)

# Metrics
library(MLmetrics)
mae <- MAE(previsao1,teste$cnt)
mse <- MSE(previsao1,teste$cnt)
rmse <- sqrt(mse)
r2 <- R2_Score(previsao1,teste$cnt)
adj_r2 <- 1-(1-r2) * (n-1)/(n-k-1)
results <- data.frame(Model='Multiple Linear Regression',
                      MAE_Value = mae,
                      MSE_Value = mse,
                      RMSE_Value = rmse,
                      R2_Score_Value = r2,
                      Adjusted_R2 = adj_r2)
results
#---------------------------------------------------------------------------------#
## Polynomial Regression
strsplit(as.character(formula) ,'~')[[3]]
poly_regression <- lm(cnt ~ poly(hr +
                                   temp +
                                   atemp +
                                   hum +
                                   windspeed +
                                   casual +
                                   registered +
                                   season_spring +
                                   season_summer +
                                   season_fall +
                                   yr_2012 +
                                   holiday_yes +
                                   workingday_yes +
                                   weathersit_mist +
                                   weathersit_light +
                                   weathersit_heavy +
                                   month_f_fev +
                                   month_f_mar +
                                   month_f_abr +
                                   month_f_mai +
                                   month_f_jun +
                                   month_f_jul +
                                   month_f_ago +
                                   month_f_set +
                                   month_f_out +
                                   month_f_nov +
                                   month_f_dez +
                                   weekday_f_seg +
                                   weekday_f_ter +
                                   weekday_f_qua +
                                   weekday_f_qui +
                                   weekday_f_sex +
                                   weekday_f_sáb,2,raw = TRUE),
                      data = treino,  trControl = train_control)
poly_regression

# Predicting Test Set
previsao1 <- predict(poly_regression,teste)
previsao1 <- as.numeric(previsao1)
View(previsao1)
str(previsao1)

# Metrics
mae <- MAE(previsao1,teste$cnt)
mse <- MSE(previsao1,teste$cnt)
rmse <- sqrt(mse)
r2 <- R2_Score(previsao1,teste$cnt)
adj_r2 <- 1-(1-r2) * (n-1)/(n-k-1)
model_results <- data.frame(Model='Multiple Polynomial Regression',
                      MAE_Value = mae,
                      MSE_Value = mse,
                      RMSE_Value = rmse,
                      R2_Score_Value = r2,
                      Adjusted_R2 = adj_r2)
results <- rbind(results, model_results) 
results
#---------------------------------------------------------------------------------#
## Support Vector Regression
library(e1071)
svr_regression <- svm(formula, data = treino, kernel = "linear", cost = 10, scale = FALSE,
                      trControl = train_control)
svr_regression

# Predicting Test Set
previsao1 <- predict(svr_regression,teste)
previsao1 <- as.numeric(previsao1)
View(previsao1)
str(previsao1)

# Metrics
mae <- MAE(previsao1,teste$cnt)
mse <- MSE(previsao1,teste$cnt)
rmse <- sqrt(mse)
r2 <- R2_Score(previsao1,teste$cnt)
adj_r2 <- 1-(1-r2) * (n-1)/(n-k-1)
model_results <- data.frame(Model='Support Vector RBF',
                            MAE_Value = mae,
                            MSE_Value = mse,
                            RMSE_Value = rmse,
                            R2_Score_Value = r2,
                            Adjusted_R2 = adj_r2)
results <- rbind(results, model_results) 
results
#---------------------------------------------------------------------------------#
## Decision Tree Regression
#install.packages("rpart")
library(rpart)
dt_regression <- rpart(formula, data = treino, method='anova')
dt_regression

plot(dt_regression, uniform=TRUE, 
     main="Regression Tree for cnt")
text(dt_regression, use.n=TRUE, cex = .6)

# Predicting Test Set
previsao1 <- predict(dt_regression,teste)
previsao1 <- as.numeric(previsao1)
View(previsao1)
str(previsao1)

# Metrics
mae <- MAE(previsao1,teste$cnt)
mse <- MSE(previsao1,teste$cnt)
rmse <- sqrt(mse)
r2 <- R2_Score(previsao1,teste$cnt)
adj_r2 <- 1-(1-r2) * (n-1)/(n-k-1)
model_results <- data.frame(Model='Decision Tree Regression',
                            MAE_Value = mae,
                            MSE_Value = mse,
                            RMSE_Value = rmse,
                            R2_Score_Value = r2,
                            Adjusted_R2 = adj_r2)
results <- rbind(results, model_results) 
results
#---------------------------------------------------------------------------------#
## Random Forest Regression
rf_regression <- randomForest(formula , 
                              data = treino, 
                              ntree = 100, 
                              nodesize = 10,
                              importance = FALSE,
                              trControl = train_control)
rf_regression

# Predicting Test Set
previsao1 <- predict(rf_regression,teste)
previsao1 <- as.numeric(previsao1)
View(previsao1)
str(previsao1)

# Metrics
mae <- MAE(previsao1,teste$cnt)
mse <- MSE(previsao1,teste$cnt)
rmse <- sqrt(mse)
r2 <- R2_Score(previsao1,teste$cnt)
adj_r2 <- 1-(1-r2) * (n-1)/(n-k-1)
model_results <- data.frame(Model='Random Forest Regression',
                            MAE_Value = mae,
                            MSE_Value = mse,
                            RMSE_Value = rmse,
                            R2_Score_Value = r2,
                            Adjusted_R2 = adj_r2)
results <- rbind(results, model_results) 
results

#---------------------------------------------------------------------------------#
## XGB Regression
library(xgboost)
xgb_regression <- train(formula , 
                        data = treino, 
                        method = "xgbTree",
                        trControl = trainControl("cv", number = 10))
xgb_regression

# Predicting Test Set
previsao1 <- predict(xgb_regression,teste)
previsao1 <- as.numeric(previsao1)
View(previsao1)
str(previsao1)

# Metrics
mae <- MAE(previsao1,teste$cnt)
mse <- MSE(previsao1,teste$cnt)
rmse <- sqrt(mse)
r2 <- R2_Score(previsao1,teste$cnt)
adj_r2 <- 1-(1-r2) * (n-1)/(n-k-1)
model_results <- data.frame(Model='XGB Regression',
                            MAE_Value = mae,
                            MSE_Value = mse,
                            RMSE_Value = rmse,
                            R2_Score_Value = r2,
                            Adjusted_R2 = adj_r2)
results <- rbind(results, model_results) 
results
#---------------------------------------------------------------------------------#
# The Best Classifier
best <- results[with(results, order(Adjusted_R2, decreasing = TRUE)),]
best

#---------------------------------------------------------------------------------#
# analyzing Coeficients From the Best Model
summary(xgb_regression)
res <- residuals(xgb_regression)
res <- as.data.frame(res)
colnames(res) <- c('residual')
head(res)

ggplot(res, aes(residual))+
  geom_histogram(binwidth = 1)
plot(xgb_regression)
View(res)

#---------------------------------------------------------------------------------#
# Visualyzing predicts and observed values 
previsao1_denormalized <- denormalized(previsao1, df_dummies$cnt)
previsao1_denormalized <- lapply(previsao1_denormalized, round)
teste_denormalized <- denormalized(teste$cnt, df_dummies$cnt)
resultados <- cbind(previsao1_denormalized, teste_denormalized) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados




