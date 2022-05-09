setwd("~/Career/MBA/Admits/Yale/Academics/Spring 2/Big Data/Final Project")
hr = read.csv("HR.csv")
View(hr)
dim(hr)
#[1] 1470   35
#see excel file for current thinking

is.factor(hr$Attrition)
#[1] FALSE
hr$Attrition = factor(hr$Attrition)
set.seed(2)
nrow(hr)
#[1] 1470
trainindex=sample(1470, 80*1470/100)
80*1470/100
#[1] 1176
train=hr[trainindex,]
valid=hr[-trainindex,]
library(tree)
hr.tree = tree(Attrition~.,train)
# Warning message:
#   In tree(Attrition ~ ., train) : NAs introduced by coercion
sum(!complete.cases(hr))
#[1] 0
sum(!complete.cases(hr$Attrition))
#[1] 0
#No NAs in the rows.. what was that error message about?!


#generate Tree
dev.new()
#NULL
plot(hr.tree)
text(hr.tree, pretty=0)


#Confusion matrix for training data
tree.pred = predict(hr.tree, train, type="class")
#Warning message:
#  In pred1.tree(object, tree.matrix(newdata)) : NAs introduced by coercion
Actual.class = train$Attrition
table(Actual.class, tree.pred)
#tree.pred
#Actual.class  No Yes
#No  961  24
#Yes 150  41
overall.error = sum(Actual.class != tree.pred)/1176
overall.error
#[1] 0.1479592

  
#Confusion matrix for test data
tree.pred = predict(hr.tree , valid, type = "class")
# Warning message:
#   In pred1.tree(object, tree.matrix(newdata)) : NAs introduced by coercion
Actual.class = valid$Attrition
table(Actual.class, tree.pred)
#tree.pred
#Actual.class  No Yes
#No  242   6
#Yes  37   9
overall.error = sum(Actual.class != tree.pred)/294
overall.error
#[1] 0.1462585
 
  
#Pruning the tree  
cv = cv.tree(hr.tree, FUN=prune.misclass)

#Realized here that a bunch of variables are not represented.. 
#Cos it's text! Need to convert it into columns 
#Using package FastDummies
-------------------------------------------------------------------------------------------------
#Clean up the data by introducing DUMMY VARIABLES 

library("fastDummies")
trial = hr
#Using FastDummies package, it creates dummy variables for each variable listed + can delete the original variables! 
trial = dummy_cols(trial, select_columns = c('BusinessTravel',"Department", 'EducationField', 'Gender', 'JobRole', 'MaritalStatus', 'Over18', 'OverTime'), remove_selected_columns = TRUE)
View(trial)

#Trying to deal with the error of "Undefined Columns"
#Used package Janitor and the "Clean_names" function, used the make.names function to remove stopwords by introducting .

hrc = trial
dim(hrc)
#[1] 1470   56
is.factor(hrc$Attrition)
#[1] TRUE
set.seed(2)
nrow(hr)
#[1] 1470
train = hrc[trainindex,]
valid = hrc[-trainindex,]
hrc.tree = tree(Attrition~.,train)
hrc.tree = tree(Attrition~.,)
old.col = "Department_Research & Development"
new.col = "Department_ResearchDevelopment"
names(hrc)[names(hrc)=='old.col'] = 'new.col'
View(hrc)
names(hrc)[names(hrc)==old.col] = new.col
View(hrc)
hrc.tree = tree(Attrition~.,train)



install.packages("janitor")
library(janitor)
clean_names(hrc)
hrc.tree = tree(Attrition~.,train)
#Error in `[.data.frame`(frame, predictors) : undefined columns selected
train = hrc[trainindex,]
hrc.tree = tree(Attrition~.,train)
#Error in `[.data.frame`(frame, predictors) : undefined columns selected
View(hrc)
colnames(hrc) <- make.names(colnames(hrc))
hrc.tree = tree(Attrition~.,train)
#Error in `[.data.frame`(frame, predictors) : undefined columns selected
View(hrc)
hrc.tree = tree(Attrition~.,data=train)
#Error in `[.data.frame`(frame, predictors) : undefined columns selected
is.factor(train$Attrition)
#[1] TRUE
train = hr[trainindex,]
hrc.tree = tree(Attrition~.,data=train)
#Warning message:
#  In tree(Attrition ~ ., data = train) : NAs introduced by coercion
train = hrc[trainindex,]
hrc.tree = tree(Attrition~.,data=train)
View(train)
#LOL, how did that work??


#HRC TREE - UNPRUNED!!!!!!!!!!!!!
dev.new()
NULL
plot(hrc.tree)
text(hrc.tree, pretty=0)


#confusion matrix for Train data
tree.pred = predict(hrc.tree, train, type = "class")
Actual.class = train$Attrition
table(Actual.class, tree.pred)
#tree.pred
#Actual.class  No Yes
#No  974  11
#Yes 135  56
overall.error = sum(Actual.class != tree.pred)/1176
overall.error
#[1] 0.1241497


#Confusion matrix for Test data
tree.pred = predict(hrc.tree, valid, type = "class")
#Error in eval(predvars, data, env) : 
#  object 'BusinessTravel_Non.Travel' not found
valid = hrc[-trainindex,]
tree.pred = predict(hrc.tree, valid, type = "class")
Actual.class = valid$Attrition
table(Actual.class, tree.pred)
#tree.pred
#Actual.class  No Yes
#No  240   8
#Yes  39   7
overall.error = sum(Actual.class != tree.pred)/294
overall.error
#[1] 0.1598639


#Build the Overall Error vs Terminal Nodes
cv = cv.tree(hrc.tree, FUN = prune.misclass)
num_nodes = cv$size
overall_error = cv$dev
dev.new()
#NULL
plot(num_nodes, overall_error, type = "b")

#8 nodes seem to have lowest error
  
  
# Prune the tree with n=8
title('Cross validated overall error as function of #terminal nodes')
pruned.tree = prune.misclass(hrc.tree, best = 8)
pruned.tree
# #node), split, n, deviance, yval, (yprob)
# * denotes terminal node
# 
# 1) root 1176 1043.000 No ( 0.83759 0.16241 )  
# 2) OverTime_No < 0.5 331  403.900 No ( 0.70091 0.29909 )  
# 4) MonthlyIncome < 3751.5 110  150.200 Yes ( 0.42727 0.57273 )  
# 8) ï..Age < 30.5 50   52.690 Yes ( 0.22000 0.78000 ) *
#   9) ï..Age 30.5 60   80.760 No ( 0.60000 0.40000 ) *
#   5) MonthlyIncome 3751.5 221  196.400 No ( 0.83710 0.16290 )  
# 10) JobRole_Sales.Executive < 0.5 144   82.610 No ( 0.91667 0.08333 ) *
#   11) JobRole_Sales.Executive 0.5 77   95.550 No ( 0.68831 0.31169 )  
# 22) DistanceFromHome < 11 53   48.290 No ( 0.83019 0.16981 ) *
#   23) DistanceFromHome 11 24   31.760 Yes ( 0.37500 0.62500 ) *
#   3) OverTime_No 0.5 845  581.600 No ( 0.89112 0.10888 )  
# 6) TotalWorkingYears < 1.5 53   69.170 No ( 0.64151 0.35849 )  
# 12) DistanceFromHome < 16 45   52.190 No ( 0.73333 0.26667 ) *
#   13) DistanceFromHome 16 8    6.028 Yes ( 0.12500 0.87500 ) *
#   7) TotalWorkingYears 1.5 792  487.100 No ( 0.90783 0.09217 ) *
#   ev.new()
# Error in ev.new() : could not find function "ev.new"

# Pruned Tree! 8 Nodes
dev.new()
#NULL
plot(pruned.tree)
text(pruned.tree, pretty =0)
title('Pruned tree - 8 nodes')


#Confusion matrix for train with Pruned Tree
tree.pred = predict(pruned.tree, train, type = "class")
Actual.class = train$Attrition
table(Actual.class, tree.pred)
# tree.pred
# Actual.class  No Yes
# No  964  21
# Yes 130  61
overall.error = sum(Actual.class != tree.pred)/1176
overall.error
#[1] 0.1284014

  
#Confusion matrix for Test with Pruned Tree
tree.pred = predict(pruned.tree, valid, type = "class")
Actuall.class = valid$Attrition
table(Actual.class, tree.pred)
#Error in table(Actual.class, tree.pred) : 
#  all arguments must have the same length
table(Actuall.class, tree.pred)
# tree.pred
# Actuall.class  No Yes
# No  237  11
# Yes  35  11
overall.error = sum(Actuall.class != tree.pred)/294
overall.error
#[1] 0.1564626

  
  
#Recalculating the number of nodes, to see if it is indeed 8.. 
cv=cv.tree(hrc.tree, FUN=prune.misclass)
cv=cv.tree(pruned.tree, FUN=prune.misclass)
num_nodes = cv$size
overall_error = cv$dev
dev.new()
#NULL
plot(num_nodes, overall_error, type='b')
cv=cv.tree(hrc.tree, FUN=prune.misclass)
num_nodes = cv$size
overall_error = cv$dev
dev.new()
NULL
plot(num_nodes, overall_error, type='b')
dev.new()
NULL
plot(num_nodes, overall_error, type='b')


#Prune the tree to 10 nodes
title("Cross validated overall error as function of #terminal nodes")
pruned.tree=prune.misclass(hrc.tree, best = 10)
pruned.tree
# node), split, n, deviance, yval, (yprob)
# * denotes terminal node
# 
# 1) root 1176 1043.000 No ( 0.83759 0.16241 )  
# 2) OverTime_No < 0.5 331  403.900 No ( 0.70091 0.29909 )  
# 4) MonthlyIncome < 3751.5 110  150.200 Yes ( 0.42727 0.57273 )  
# 8) ï..Age < 30.5 50   52.690 Yes ( 0.22000 0.78000 )  
# 16) YearsWithCurrManager < 0.5 22    0.000 Yes ( 0.00000 1.00000 ) *
#   17) YearsWithCurrManager 0.5 28   37.520 Yes ( 0.39286 0.60714 )  
# 34) JobRole_Research.Scientist < 0.5 13    7.051 Yes ( 0.07692 0.92308 ) *
#   35) JobRole_Research.Scientist 0.5 15   19.100 No ( 0.66667 0.33333 ) *
#   9) ï..Age 30.5 60   80.760 No ( 0.60000 0.40000 ) *
#   5) MonthlyIncome 3751.5 221  196.400 No ( 0.83710 0.16290 )  
# 10) JobRole_Sales.Executive < 0.5 144   82.610 No ( 0.91667 0.08333 ) *
#   11) JobRole_Sales.Executive 0.5 77   95.550 No ( 0.68831 0.31169 )  
# 22) DistanceFromHome < 11 53   48.290 No ( 0.83019 0.16981 ) *
#   23) DistanceFromHome 11 24   31.760 Yes ( 0.37500 0.62500 ) *
#   3) OverTime_No 0.5 845  581.600 No ( 0.89112 0.10888 )  
# 6) TotalWorkingYears < 1.5 53   69.170 No ( 0.64151 0.35849 )  
# 12) DistanceFromHome < 16 45   52.190 No ( 0.73333 0.26667 ) *
#   13) DistanceFromHome 16 8    6.028 Yes ( 0.12500 0.87500 ) *
#   7) TotalWorkingYears 1.5 792  487.100 No ( 0.90783 0.09217 ) *
  dev.new()
#NULL
plot(pruned.tree)
text(pruned.tree, pretty=0)
title('Pruned tree - 10 nodes')


#Confusion matrix for train with Pruned Tree, 10 nodes
tree.pred = predict(pruned.tree, train, type = "class")
Actual.class = train$Attrition
table(Actual.class, tree.pred)
# tree.pred
# Actual.class  No Yes
# No  974  11
# Yes 135  56
overall.error = sum(Actual.class != tree.pred)/1176
overall.error
#[1] 0.1241497


#Confusion matrix for Test with Pruned Tree, 10 nodes
tree.pred = predict(pruned.tree, valid, type = "class")
Actual.class = valid$Attrition
table(Actual.class, tree.pred)
# tree.pred
# Actual.class  No Yes
# No  240   8
# Yes  39   7
overall.error = sum(Actuall.class != tree.pred)/294
overall.error
#[1] 0.1598639

  
  