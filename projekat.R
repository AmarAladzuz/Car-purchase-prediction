#import packages
library(ggplot2)
library(fastDummies)
library(caTools)
library(randomForest)
library(MLmetrics)
library(rpart)
library(caret)
library(dplyr)

# import files
customers_info = read.csv(file = 'customer_info.csv')
has_purchased = read.csv(file = 'has_purchased.csv')

# preview and drop new index column
summary(has_purchased)
summary(customers_info)

head(customers_info)
customers_info = customers_info[, c(2,3,4,5)]
head(customers_info)

head(has_purchased)
has_purchased = has_purchased[, c(2,3)]
head(has_purchased)

# check data types
str(customers_info)
str(has_purchased)

# calculate monthly salary
customers_info['Monthly_Salary'] = round(customers_info['AnnualSalary']/12, 0)

# join customer_info and has_purchased
all_data = merge(x = customers_info, y = has_purchased, by = "User.ID")

#normalize data (z scores) for age and monthly salary
normalised_data = scale(all_data[, c(3,5)])
colnames(normalised_data)[1] = "Age_Norm"
colnames(normalised_data)[2] = "Monthly_Salary_Norm"
all_data = cbind(all_data, normalised_data)


# split data to x and y
#x = all_data[,c(2,7,8)]
#x = dummy_cols(x, select_columns = "Gender", remove_first_dummy = TRUE)
#x = x[, c(2,3,4)]
#y = all_data[6]


#split data to train and test
set.seed(12)
split = sample.split(all_data, SplitRatio = 0.8)
train = subset(all_data[,c(2,6,7,8)], split == "TRUE")
test = subset(all_data[,c(2,6,7,8)], split == "FALSE")


#random forest classifier
set.seed(12)
rf_model = randomForest(x = train[-2], y = as.factor(train$Purchased))
y_pred <- predict(rf_model, newdata = test[-2], type= "class")
confusion_mtx = table(test[, 2], y_pred)
confusion_mtx
Accuracy(y_pred, test[, 2])


#decision tree classifier
set.seed(12)
dt_model = rpart(formula = as.factor(Purchased) ~ Gender + Age_Norm + Monthly_Salary_Norm, 
                 data = train)

y_pred = predict(dt_model, newdata = test[-2], type = 'class')
confusion_mtx = table(test[, 2], y_pred)
confusion_mtx
Accuracy(y_pred, test[, 2])



#Logistic regression classification
set.seed(12)
logistic_model = glm(as.factor(Purchased) ~ Gender + Age_Norm + Monthly_Salary_Norm,
                      data = train, family = "binomial")

y_pred = predict(logistic_model, newdata = test[-2], type = "response")
y_pred = ifelse(y_pred > 0.5, 1, 0)

confusion_mtx = table(test[, 2], y_pred)
confusion_mtx
Accuracy(y_pred, test[, 2])



boxplot(all_data$Age, ylab = "Years", main = "Age Boxplot")
boxplot(all_data$Monthly_Salary, ylab = "Dolars", main = "Monthly Salary Boxplot")


agg_df <- aggregate(all_data$Purchased, by=list(all_data$Purchased), FUN = length)
agg_df

barplot(height = agg_df$x, main = "'Purchased' Distribution", xlab = "Purchased",
        ylab = "Count", names.arg = agg_df$Group.1, col="blue")







agg_df <- aggregate(all_data$Gender, by=list(all_data$Gender), FUN = length)
agg_df

barplot(height = agg_df$x, main = "'Gender' Distribution", xlab = "Gender",
        ylab = "Count", names.arg = agg_df$Group.1, col="blue")

