## Clear all the global variables and set working directory
rm(list=ls(all=T))
setwd("C:/Users/Mohammeds.Fakir/Kotak Case Study")

# For Reading Error Metrics in normal form, rather than in exponential
options(scipen = 999)

# Set Seed for getting constant results
set.seed(12345)

## Load Libraries
x = c("dummies","caret","rpart.plot","plotly","plyr","dplyr","gbm", "Matrix","ggplot2","reticulate",
      "rpart","xgboost","DMwR","randomForest","usdm","corrgram","DataCombine","xlsx", "e1071")
lapply(x,require, character.only = TRUE)
rm(x)

# Read Data
column_names = c("Sex", "Length", "Diameter", "Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight", "Rings")
abalone_df = read.csv("abalone.data", sep = ',', header = FALSE)
colnames(abalone_df) <- column_names

######################################## EXPLORATORY DATA ANALYSIS ########################################
# Check shape
print(paste0("Abalone Dataset Consists of ", dim(abalone_df)[1], " observartions and ",dim(abalone_df)[2], " features"))

# Display Top 5 rows
head(abalone_df)

# Describe dataframe
str(abalone_df)

# Every feature as per problem statement is associated with proper datatype, hence no need to converting to appropirate datatypes

# As per Problem Statement, Distributing colmnns into categorical factors as they contain unqiue values
category_column_names= c("Sex")

numerical_column_names = c("Length", "Diameter", "Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight", "Rings")

##################################Missing Values Analysis###############################################

# Creating dataframe with missing values present in each variable
missing_val = data.frame(apply(abalone_df, 2, function(x) { sum(is.na(x))} ))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"

# Calculating percentage missing value
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(abalone_df)) * 100

# Sorting missing_val in Descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL

# Reordering columns
missing_val = missing_val[,c(2,1)]

# Write to CSV file
write.csv(missing_val, "R_Missing_perc.csv", row.names = F)

# Displaying missing values found
missing_val

# Plot Percentage of missing values in all columns
ggplot(data = missing_val[1:9,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()

# As seen on above missing value data frame and plot, there's no missing value found.

######################################## DATA DISTRIBUTION USING GRAPHS ########################################

############### Univariate Analysis ##########################

# Distribution of Target variable : Rings
fit <- density(abalone_df$Rings)
plot_ly(x = abalone_df$Rings, type = "histogram", name = "Count Distribution") %>%
  add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))

# Distribution of All Numerical Columns and Check for skewness
for( i in column_names){
  if(i != "Sex"){
  # for checking skewness
  print(paste0("Skewness for ",i," : ",skewness(abalone_df[,i])))
  
  # for plotting the distribution
  fit <- density(abalone_df[,i])
  p <- plot_ly(x = abalone_df[,i], type = "histogram", name = "Distribution") %>%
    add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
    layout(yaxis2 = list(overlaying = "y", side = "right") , title = i)
  print(p)
  Sys.sleep(2)
  }
}

### All the numerical features are closely follow normal distribution which is a good thing (with Maximum Skewness for Height)

# Countplot for all Categorical Columns
bar1 = ggplot(data = abalone_df, aes(x = Sex)) + geom_bar() + ggtitle("Countplot for Sex") + theme_bw()
bar1

# It is clearly visible from the above plot that we have almost same count of Female and Infact whereas Male has the highest number of records

#### Key Insights : -
  # No missing values in the dataset
  # All numerical features but 'sex'
  # Though features are not normaly distributed, are close to normality
  # Each feature has difference scale range


############### Bivariate Analysis ####################

# Absentism on Basis of Sex
df_temp <- aggregate(Rings ~ Sex, abalone_df, sum)
plot_ly(x = ~Sex, y = ~Rings , data = df_temp, type = "bar", text = ~Rings , marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)', width = 1.5)))

# Pairplot for all the numerical features
pairs.panels(abalone_df[2:9], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

################################# Outlier Analysis - Box plot ########################
# Plot Outliers for all of the Numerical Features
for(i in numerical_column_names){
  p <- plot_ly(x=abalone_df[,i], type = "box") %>% 
    layout(yaxis2 = list(overlaying = "y", side = "right") , title = i)
  print(p)
  Sys.sleep(2)
}


############ Remove outliers using boxplot method

##loop to remove from all variables
for(i in numerical_column_names)
{
  print(paste0("Removing : ",i))
  val = abalone_df[,i][abalone_df[,i] %in% boxplot.stats(abalone_df[,i])$out]
  print(length(val))
  abalone_df = abalone_df[which(!abalone_df[,i] %in% val),]
}

#Replace all outliers with NA for imputing
for(i in numerical_column_names)
{
  val = abalone_df[,i][abalone_df[,i] %in% boxplot.stats(abalone_df[,i])$out]
  print(length(val))
  abalone_df[,i][abalone_df[,i] %in% val] = NA
}

# Imputing missing values
abalone_df = knnImputation(abalone_df,k=3)

## Check for Null Values
sum(is.na(abalone_df))


######################################## FEATURE SELECTION ########################################
#Check for multicollinearity using VIF

vifcor(abalone_df[,numerical_column_names])

#Check for multicollinearity using corelation graph
corrgram(abalone_df[,numerical_column_names], order = F, upper.panel=panel.pie, 
         text.panel=panel.txt, main = "Correlation Plot")

## ANOVA test for Categoprical variable
summary(aov(formula = Rings~Sex,data = abalone_df))

###### Remove variables if needed!

######################################## FEATURE SCALING ########################################

#Normalization of continuous variables
for(i in numerical_column_names[1:7]){
  print(i)
  abalone_df[,i] = (abalone_df[,i] - min(abalone_df[,i]))/
    (max(abalone_df[,i]) - min(abalone_df[,i]))
}

#Create dummy variables of factor variables
abalone_df = dummy.data.frame(abalone_df, category_column_names)

df = abalone_df

######################## Model Development ######################

#Splitting data into train and test data
train_index = sample(1:nrow(df), 0.8*nrow(df))        
train = df[train_index,]
test = df[-train_index,]

################### Evaluation Function #######################

validate_results <- function(y_test, predictions){
  
  df1 = data.frame('y_test'= y_test,'predictions'= predictions)
  
  df1['Actual_Class'] = ifelse(y_test > 5, 'yes', 'no')
  df1['Predicted_Class'] = ifelse(predictions > 5, 'yes', 'no')
  
  print(head(df1))
  
  df1$Actual_Class <- as.factor(df1$Actual_Class)
  df1$Predicted_Class <- as.factor(df1$Predicted_Class)
  
  result <- confusionMatrix(table(df1$Actual_Class, df1$Predicted_Class))
  print(paste0("Final Accuracy for No. of Rings will be greater than 5 or not :",result$overall[1]))
  
}

####################### DECISION TREE############################

#Build decsion tree using rpart
c50_model = rpart(Rings ~ ., data = train, method = "anova")

#Plot the tree
rpart.plot(c50_model)

#Perdict for test cases
c50_predictions = predict(c50_model, test[,-11])

#Create data frame for actual and predicted values
c50_pred = data.frame("actual"=test[,11], "predictions"=c50_predictions)
head(c50_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = c50_predictions, obs = test[,11]))

#Plot a graph for actual vs predicted values
plot(test$Rings,type="l",lty=2,col="green")
lines(c50_predictions,col="blue")


#RMSE: 1.74
#MAE: 1.36
#R squared: 0.42

######################################## RANDOM FOREST ########################################
##Train the model using training data
rf_model = randomForest(Rings~., data = train, ntree = 1000)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-11])

#Create dataframe for actual and predicted values
rf_pred = data.frame("actual"=test[,11], "predictions"=rf_predictions)
head(rf_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test[,11]))

#Plot a graph for actual vs predicted values
plot(test$Rings,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

#RMSE: 1.55
#MAE: 1.19
#R squared: 0.54

validate_results(test[,11], rf_predictions)

########################################LINEAR REGRESSION########################################

##Train the model using training data
linear_regressor = lm(formula = Rings~., data = train)

#Get the summary of the model
summary(linear_regressor)

#Predict the test cases
lr_predictions = predict(linear_regressor, test[,-11])

#Create dataframe for actual and predicted values
df_pred = data.frame("actual"=test[,11], "predictions"=rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs = test[,11]))

#Plot a graph for actual vs predicted values
plot(test$Rings,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#RMSE: 1.63
#MAE: 1.26
#R squared: 0.49

validate_results(test[,11], lr_predictions)

############################# XGBoost ##########################
train_matrix <- sparse.model.matrix(Rings ~ .-1, data = train)
test_matrix <- sparse.model.matrix(Rings ~ .-1, data = test)

xgb <- xgboost(data = as.matrix(train_matrix),
               label = as.matrix(train$Rings),
               booster = "gbtree", 
               objective = "reg:linear", 
               max.depth = 8, 
               eta = 0.5, 
               nthread = 2, 
               nround = 100, 
               min_child_weight = 1, 
               subsample = 0.75, 
               colsample_bytree = 1, 
               num_parallel_tree = 3)

xgb_predictions <- predict(xgb, test_matrix)

#Create dataframe for actual and predicted values
df_pred = data.frame("actual"=test[,11], "predictions"=xgb_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = xgb_predictions, obs = test[,11]))

#Plot a graph for actual vs predicted values
plot(test$Rings,type="l",lty=2,col="green")
lines(xgb_predictions,col="blue")

#RMSE: 1.30
#MAE: 1.67
#R squared: 0.48

validate_results(test[,11], xgb_predictions)

########################################DIMENSION REDUCTION USING PCA########################################
#Principal component analysis
prin_comp = prcomp(train)

#Compute standard deviation of each principal component
pr_stdev = prin_comp$sdev

#Compute variance
pr_var = pr_stdev^2

#Proportion of variance explained
prop_var = pr_var/sum(pr_var)

#Cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Add a training set with principal components
train.data = data.frame(Rings = train$Rings, prin_comp$x)

# From the above plot selecting 4 components since it explains almost 95+ % data variance
train.data =train.data[,1:4]

#Transform test data into PCA
test.data = predict(prin_comp, newdata = test)
test.data = as.data.frame(test.data)

#Select the first 45 components
test.data=test.data[,1:4]


########################################DECISION TREE########################################

#Build decsion tree using rpart
dt_model = rpart(Rings ~., data = train.data, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model,test.data)

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,11], "dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test$Rings))

#Plot a graph for actual vs predicted values
plot(test$Rings,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

#RMSE: 0.307
#MAE: 0.166
#R squared: 0.98


########################################RANDOM FOREST########################################

#Train the model using training data
rf_model = randomForest(Rings~., data = train.data, ntrees = 1000)

#Extract the rules generated as a result of random Forest model
library("inTrees")
rules_list = RF2List(rf_model)

#Extract rules from rules_list
rules = extractRules(rules_list, train.data[,-4])
rules[1:2,]

#Convert the rules in readable format
read_rules = presentRules(rules,colnames(train))
read_rules[1:2,]

#Determining the rule metric
rule_metric = getRuleMetric(rules, train[,-4], train$Rings)
rule_metric[1:2,]


#Predict the test cases
rf_predictions = predict(rf_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test$Rings))

#Plot a graph for actual vs predicted values
plot(test$Rings,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

#RMSE: 0.132
#MAE: 0.057
#R squared: 0.996

validate_results(test$Rings, rf_predictions)

##################################### XGBOOST ##############################################

#Develop Model on training data
fit_XGB = gbm(Rings~., data = train.data, n.trees = 500, interaction.depth = 2)

#Lets predict for training data
pred_XGB_train = predict(fit_XGB, train.data, n.trees = 500)

#Lets predict for testing data
pred_XGB_test = predict(fit_XGB,test.data, n.trees = 500)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,pred_XGB_test)
head(df_pred)

# For testing data 
print(postResample(pred = pred_XGB_test, obs = test$Rings))

#RMSE: 0.000027
#MAE: 0.0000025
#R squared: 0.999

validate_results(test$Rings, pred_XGB_test)

######################################## LINEAR REGRESSION ########################################

#Train the model using training data
lr_model = lm(Rings ~ ., data = train.data)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model,test.data)

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs =test$Rings))

#Plot a graph for actual vs predicted values
plot(test$Rings,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#RMSE: 0.0157
#MAE: 0.0124
#R squared: 0.999

validate_results(test$Rings, lr_predictions)

#### As seen above, after PCA, every algorithm gives the best result with very less RMSE and R^square of 0.99

############ Writing Final Results into CSV ##############
df_pred['Actual_Class'] = ifelse(test$Rings > 5, 'yes', 'no')
df_pred['DecisionTree_Prediction'] = ifelse(c50_predictions > 5, 'yes', 'no')
df_pred['RandomForest_Prediction'] = ifelse(rf_predictions > 5, 'yes', 'no')
df_pred['LinearRegression_Prediction'] = ifelse(lr_predictions > 5, 'yes', 'no')
df_pred['XGBoost_Prediction'] = ifelse(xgb_predictions > 5, 'yes', 'no')

write.csv(df_pred, "Final_Predictions_R.csv", row.names = T)
