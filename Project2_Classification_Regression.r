setwd("~/Career/MBA/Admits/Yale/Academics/Spring 2/Big Data/Final Project")
hr = read.csv("HR.csv")
View(hr)
#see excel file for current thinking

#THIS IS 2nd Classification Tree built with different Seed

is.factor(hr$Attrition)
#[1] FALSE
hr$Attrition = factor(hr$Attrition)
set.seed(3)
trainindex=sample(1470, 80*1470/100)

library(tree)
library("fastDummies")

trial = hr
#Using FastDummies package, it creates dummy variables for each variable listed + can delete the original variables! 
trial = dummy_cols(trial, select_columns = c('BusinessTravel',"Department", 'EducationField', 'Gender', 'JobRole', 'MaritalStatus', 'Over18', 'OverTime'), remove_selected_columns = TRUE)
#Incredible, that worked fast

hrc = trial
dim(hrc)
#[1] 1470   56

is.factor(hrc$Attrition)
#[1] TRUE
nrow(hr)
#[1] 1470
install.packages("janitor")
library(janitor)

clean_names(hrc)
#names cleaned but still it says undefined columns selected

train = hrc[trainindex,]
colnames(hrc) <- make.names(colnames(hrc))
train = hrc[trainindex,]
valid = hrc[-trainindex,]
hrc.tree = tree(Attrition~.,data=train)

#Generate Unpruned Tree
dev.new()
NULL
plot(hrc.tree)
text(hrc.tree, pretty=0)
title("Unpruned Tree")

#confusion matrix for Train data
tree.pred = predict(hrc.tree, train, type = "class")
Actual.class = train$Attrition
table(Actual.class, tree.pred)
#tree.pred
#Actual.class  No Yes
#No  949  38
#Yes 101  88
overall.error = sum(Actual.class != tree.pred)/1176
overall.error
#[1] 0.1181973


#Confusion matrix for Test data
tree.pred = predict(hrc.tree, valid, type = "class")
Actual.class = valid$Attrition
table(Actual.class, tree.pred)
tree.pred
#Actual.class  No Yes
#No  237   9
#Yes  38  10
overall.error = sum(Actual.class != tree.pred)/294
overall.error
#[1] 0.1598639


#Calculating the number of nodes with minimum error
cv=cv.tree(hrc.tree, FUN=prune.misclass)
num_nodes = cv$size
overall_error = cv$dev
dev.new()
#NULL
plot(num_nodes, overall_error, type='b')
title("Cross validated overall error as function of #terminal nodes")
cv
#$size
#[1] 17  8  7  3  1

#$dev
#[1] 203 201 182 178 194

#$k
#[1] -Inf  0.0  3.0  5.0 11.5

#$method
#[1] "misclass"

#attr(,"class")
[1] "prune"         "tree.sequence"


#Prune the tree to 3 nodes
pruned.tree=prune.misclass(hrc.tree, best = 3)
pruned.tree
dev.new()
plot(pruned.tree)
text(pruned.tree, pretty=0)
title('Pruned tree - 3 nodes')


#Confusion matrix for train with Pruned Tree, 3 nodes
tree.pred = predict(pruned.tree, train, type = "class")
Actual.class = train$Attrition
table(Actual.class, tree.pred)
#tree.pred
#Actual.class  No Yes
#No  967  20
#Yes 146  43
overall.error = sum(Actual.class != tree.pred)/1176
overall.error
#[1] 0.1411565

#Realized it got worse with 3 nodes. The RMS is very narrow
#between 180-200, hence cannot nitpick. Running the entire 
#code again gave me a different graph! (due to randomness). Selecting 17. 
#Prune the tree to 10 nodes
pruned.tree=prune.misclass(hrc.tree, best = 3)
pruned.tree
dev.new()
plot(pruned.tree)
text(pruned.tree, pretty=0)
title('Pruned tree - 3 nodes')

#Ran for 10 nodes
#10 Nodes (9-17 actually) look EXACTLY the same.. 

#Prune the tree to 8 nodes
pruned.tree=prune.misclass(hrc.tree, best = 8)
pruned.tree
dev.new()
plot(pruned.tree)
text(pruned.tree, pretty=0)
title('Pruned tree - 8 nodes')
#8 Looks different!

#Confusion matrix for train with Pruned Tree, 8 nodes
tree.pred = predict(pruned.tree, train, type = "class")
Actual.class = train$Attrition
table(Actual.class, tree.pred)
#tree.pred
#Actual.class  No Yes
#No  961  26
#Yes 117  72
overall.error = sum(Actual.class != tree.pred)/1176
overall.error
#[1] 0.1215986


#Confusion matrix for Test data with pruned tree, 8 nodes
tree.pred = predict(pruned.tree, valid, type = "class")
Actual.class = valid$Attrition
table(Actual.class, tree.pred)

tree.pred Actual.class  
    No    Yes
No  239     7
Yes  39     9

overall.error = sum(Actual.class != tree.pred)/294
overall.error
#[1] 0.1564626



#-------------------------------
# Regression Attempt

library(fastDummies)

#Remove Attrition as a Factor and replace with numeric (since it is regression not classification)
hrc2 = dummy_cols(hrc, select_columns = 'Attrition', remove_selected_columns = TRUE)
View(hrc2)
cols.dont.want="Attrition_No"
hrc2 <- hrc2[, ! names(hrc2) %in% cols.dont.want, drop = F]

reg=lm(Attrition_Yes~., data = hrc2)

train=hrc2[trainindex,]
valid = hrc2[-trainindex,]

summary(reg)

# Call:
#   lm(formula = Attrition_Yes ~ ., data = hrc2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.55266 -0.20551 -0.08396  0.08281  1.14588 
# 
# Coefficients: (10 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                        1.318e+00  1.292e-01  10.203  < 2e-16 ***
#   ï..Age                            -3.504e-03  1.327e-03  -2.640 0.008370 ** 
#   DailyRate                         -2.698e-05  2.120e-05  -1.272 0.203414    
# DistanceFromHome                   3.624e-03  1.048e-03   3.457 0.000562 ***
#   Education                          1.909e-03  8.543e-03   0.223 0.823252    
# EmployeeCount                             NA         NA      NA       NA    
# EmployeeNumber                    -7.553e-06  1.420e-05  -0.532 0.594843    
# EnvironmentSatisfaction           -4.040e-02  7.800e-03  -5.179 2.55e-07 ***
#   HourlyRate                        -1.688e-04  4.188e-04  -0.403 0.686901    
# JobInvolvement                    -5.800e-02  1.199e-02  -4.836 1.47e-06 ***
#   JobLevel                          -5.416e-03  2.855e-02  -0.190 0.849544    
# JobSatisfaction                   -3.735e-02  7.718e-03  -4.839 1.45e-06 ***
#   MonthlyIncome                      1.460e-06  7.600e-06   0.192 0.847726    
# MonthlyRate                        4.697e-07  1.193e-06   0.394 0.693790    
# NumCompaniesWorked                 1.720e-02  3.807e-03   4.519 6.72e-06 ***
#   PercentSalaryHike                 -2.181e-03  3.675e-03  -0.594 0.552852    
# PerformanceRating                  1.826e-02  3.717e-02   0.491 0.623347    
# RelationshipSatisfaction          -2.330e-02  7.892e-03  -2.953 0.003202 ** 
#   StandardHours                             NA         NA      NA       NA    
# StockOptionLevel                  -1.654e-02  1.367e-02  -1.210 0.226380    
# TotalWorkingYears                 -3.715e-03  2.417e-03  -1.537 0.124436    
# TrainingTimesLastYear             -1.341e-02  6.635e-03  -2.021 0.043491 *  
#   WorkLifeBalance                   -3.137e-02  1.206e-02  -2.601 0.009384 ** 
#   YearsAtCompany                     5.499e-03  2.989e-03   1.840 0.065995 .  
# YearsInCurrentRole                -9.218e-03  3.876e-03  -2.378 0.017517 *  
#   YearsSinceLastPromotion            1.081e-02  3.416e-03   3.164 0.001588 ** 
#   YearsWithCurrManager              -9.565e-03  3.971e-03  -2.408 0.016150 *  
#   BusinessTravel_Non.Travel         -6.561e-02  2.853e-02  -2.300 0.021586 *  
#   BusinessTravel_Travel_Frequently   8.672e-02  2.198e-02   3.945 8.35e-05 ***
#   BusinessTravel_Travel_Rarely              NA         NA      NA       NA    
# Department_Human.Resources        -1.053e-01  1.211e-01  -0.869 0.384814    
# Department_Research...Development  2.408e-02  7.064e-02   0.341 0.733221    
# Department_Sales                          NA         NA      NA       NA    
# EducationField_Human.Resources     2.674e-02  8.748e-02   0.306 0.759905    
# EducationField_Life.Sciences      -9.572e-02  3.123e-02  -3.065 0.002217 ** 
#   EducationField_Marketing          -5.535e-02  4.188e-02  -1.322 0.186491    
# EducationField_Medical            -1.077e-01  3.220e-02  -3.344 0.000846 ***
#   EducationField_Other              -1.175e-01  4.588e-02  -2.562 0.010521 *  
#   EducationField_Technical.Degree           NA         NA      NA       NA    
# Gender_Female                     -3.527e-02  1.742e-02  -2.025 0.043058 *  
#   Gender_Male                               NA         NA      NA       NA    
# JobRole_Healthcare.Representative -2.553e-01  8.608e-02  -2.965 0.003073 ** 
#   JobRole_Human.Resources           -3.900e-02  1.262e-01  -0.309 0.757394    
# JobRole_Laboratory.Technician     -1.184e-01  8.129e-02  -1.457 0.145412    
# JobRole_Manager                   -2.047e-01  8.889e-02  -2.303 0.021448 *  
#   JobRole_Manufacturing.Director    -2.406e-01  8.573e-02  -2.807 0.005073 ** 
#   JobRole_Research.Director         -2.587e-01  1.035e-01  -2.499 0.012557 *  
#   JobRole_Research.Scientist        -2.167e-01  8.114e-02  -2.671 0.007656 ** 
#   JobRole_Sales.Executive           -1.536e-01  4.541e-02  -3.382 0.000739 ***
#   JobRole_Sales.Representative              NA         NA      NA       NA    
# MaritalStatus_Divorced            -1.102e-01  3.145e-02  -3.503 0.000475 ***
#   MaritalStatus_Married             -9.694e-02  2.420e-02  -4.006 6.49e-05 ***
#   MaritalStatus_Single                      NA         NA      NA       NA    
# Over18_Y                                  NA         NA      NA       NA    
# OverTime_No                       -2.105e-01  1.896e-02 -11.102  < 2e-16 ***
#   OverTime_Yes                              NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3219 on 1424 degrees of freedom
# Multiple R-squared:  0.2578,	 
# F-statistic: 10.99 on 45 and 1424 DF,  p-value: < 2.2e-16

#RMS for training set
pred = predict(reg, new = train)
# Warning message:
#   In predict.lm(reg, new = train) :
#   prediction from a rank-deficient fit may be misleading
trainRSS=sum((train$Attrition_Yes-pred)^2)
trainRMS = sqrt(trainRSS/nrow(train))
trainRMS
#[1] 0.3134199

# RMS for Validation set
pred = predict(reg, new = valid)
# Warning message:
#   In predict.lm(reg, new = valid) :
#   prediction from a rank-deficient fit may be misleading
validRSS = sum((valid$Attrition_Yes - pred)^2)
validRMS = sqrt(validRSS/nrow(valid))
validRMS
#[1] 0.330019

#Select Max number of Variables? 
reg_bestset = regsubsets( Attrition_Yes ~ ., data = train, nvmax = 12, method = "exhaustive" )
# Reordering variables and trying again:
#   Error in leaps.exhaustive(a, really.big) : 
#   Exhaustive search will be S L O W, must specify really.big=T
# In addition: Warning message:
#   In leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in,  :
#                    10  linear dependencies found

reg_bestset = regsubsets( Attrition_Yes ~ ., data = train, nvmax = 12, method = "exhaustive", really.big = TRUE)

bestset = summary(reg_bestset)

names(bestset)
#[1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   
adjusted_r2 = bestset$adjr2

cbind(adjusted_r2, bestset$which)
# adjusted_r2 (Intercept) ï..Age DailyRate DistanceFromHome Education
# 1   0.06427702           1      0         0                0         0
# 2   0.10094980           1      0         0                0         0
# 3   0.12461607           1      0         0                0         0
# 4   0.14164211           1      0         0                0         0
# 5   0.15801065           1      0         0                0         0
# 6   0.17754593           1      0         0                0         0
# 7   0.19418321           1      0         0                0         0
# 8   0.20814133           1      0         0                0         0
# 9   0.21750912           1      0         0                0         0
# 10  0.22113404           1      0         0                0         0
# 11  0.22455753           1      0         0                0         0
# 12  0.22847317           1      0         0                1         0
# 13  0.23252746           1      0         0                1         0
# EmployeeCount EmployeeNumber EnvironmentSatisfaction HourlyRate JobInvolvement
# 1              0              0                       0          0              0
# 2              0              0                       0          0              0
# 3              0              0                       0          0              0
# 4              0              0                       0          0              0
# 5              0              0                       1          0              0
# 6              0              0                       1          0              0
# 7              0              0                       1          0              0
# 8              0              0                       1          0              1
# 9              0              0                       1          0              1
# 10             0              0                       1          0              1
# 11             0              0                       1          0              1
# 12             0              0                       0          0              1
# 13             0              0                       1          0              1
# JobLevel JobSatisfaction MonthlyIncome MonthlyRate NumCompaniesWorked
# 1         0               0             0           0                  0
# 2         0               0             0           0                  0
# 3         0               0             0           0                  0
# 4         0               1             0           0                  0
# 5         0               1             0           0                  0
# 6         1               0             0           0                  0
# 7         1               1             0           0                  0
# 8         1               1             0           0                  0
# 9         1               1             0           0                  0
# 10        1               1             0           0                  0
# 11        1               1             0           0                  0
# 12        0               1             0           0                  1
# 13        0               1             0           0                  1
# PercentSalaryHike PerformanceRating RelationshipSatisfaction StandardHours
# 1                  0                 0                        0             0
# 2                  0                 0                        0             0
# 3                  0                 0                        0             0
# 4                  0                 0                        0             0
# 5                  0                 0                        0             0
# 6                  0                 0                        0             0
# 7                  0                 0                        0             0
# 8                  0                 0                        0             0
# 9                  0                 0                        0             0
# 10                 0                 0                        0             0
# 11                 0                 0                        0             0
# 12                 0                 0                        0             0
# 13                 0                 0                        0             0
# StockOptionLevel TotalWorkingYears TrainingTimesLastYear WorkLifeBalance
# 1                 0                 0                     0               0
# 2                 0                 0                     0               0
# 3                 0                 1                     0               0
# 4                 0                 1                     0               0
# 5                 0                 1                     0               0
# 6                 0                 0                     0               0
# 7                 0                 0                     0               0
# 8                 0                 0                     0               0
# 9                 0                 0                     0               0
# 10                0                 0                     0               0
# 11                0                 0                     1               0
# 12                0                 1                     0               0
# 13                0                 1                     0               0
# YearsAtCompany YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager
# 1               0                  0                       0                    0
# 2               0                  0                       0                    0
# 3               0                  0                       0                    0
# 4               0                  0                       0                    0
# 5               0                  0                       0                    0
# 6               0                  0                       0                    0
# 7               0                  0                       0                    0
# 8               0                  0                       0                    0
# 9               0                  0                       0                    0
# 10              0                  0                       0                    0
# 11              0                  0                       0                    0
# 12              0                  0                       0                    0
# 13              0                  0                       0                    0
# BusinessTravel_Non.Travel BusinessTravel_Travel_Frequently
# 1                          0                                0
# 2                          0                                0
# 3                          0                                0
# 4                          0                                0
# 5                          0                                0
# 6                          0                                0
# 7                          0                                0
# 8                          0                                0
# 9                          0                                1
# 10                         0                                1
# 11                         0                                1
# 12                         0                                1
# 13                         0                                1
# BusinessTravel_Travel_Rarely Department_Human.Resources
# 1                             0                          0
# 2                             0                          0
# 3                             0                          0
# 4                             0                          0
# 5                             0                          0
# 6                             0                          0
# 7                             0                          0
# 8                             0                          0
# 9                             0                          0
# 10                            0                          0
# 11                            0                          0
# 12                            0                          0
# 13                            0                          0
# Department_Research...Development Department_Sales
# 1                                  0                0
# 2                                  0                0
# 3                                  0                0
# 4                                  0                0
# 5                                  0                0
# 6                                  0                0
# 7                                  0                0
# 8                                  0                0
# 9                                  0                0
# 10                                 0                0
# 11                                 0                0
# 12                                 0                0
# 13                                 0                0
# EducationField_Human.Resources EducationField_Life.Sciences
# 1                               0                            0
# 2                               0                            0
# 3                               0                            0
# 4                               0                            0
# 5                               0                            0
# 6                               0                            0
# 7                               0                            0
# 8                               0                            0
# 9                               0                            0
# 10                              0                            0
# 11                              0                            0
# 12                              0                            0
# 13                              0                            0
# EducationField_Marketing EducationField_Medical EducationField_Other
# 1                         0                      0                    0
# 2                         0                      0                    0
# 3                         0                      0                    0
# 4                         0                      0                    0
# 5                         0                      0                    0
# 6                         0                      0                    0
# 7                         0                      0                    0
# 8                         0                      0                    0
# 9                         0                      0                    0
# 10                        1                      0                    0
# 11                        0                      0                    0
# 12                        0                      0                    0
# 13                        0                      0                    0
# EducationField_Technical.Degree Gender_Female Gender_Male
# 1                                0             0           0
# 2                                0             0           0
# 3                                0             0           0
# 4                                0             0           0
# 5                                0             0           0
# 6                                0             0           0
# 7                                0             0           0
# 8                                0             0           0
# 9                                0             0           0
# 10                               0             0           0
# 11                               0             0           0
# 12                               0             0           0
# 13                               0             1           0
# JobRole_Healthcare.Representative JobRole_Human.Resources
# 1                                  0                       0
# 2                                  0                       0
# 3                                  0                       0
# 4                                  0                       0
# 5                                  0                       0
# 6                                  0                       0
# 7                                  0                       0
# 8                                  0                       0
# 9                                  0                       0
# 10                                 0                       0
# 11                                 0                       0
# 12                                 0                       0
# 13                                 0                       0
# JobRole_Laboratory.Technician JobRole_Manager JobRole_Manufacturing.Director
# 1                              0               0                              0
# 2                              0               0                              0
# 3                              0               0                              0
# 4                              0               0                              0
# 5                              0               0                              0
# 6                              0               0                              0
# 7                              0               0                              0
# 8                              0               0                              0
# 9                              0               0                              0
# 10                             0               0                              0
# 11                             0               0                              0
# 12                             1               0                              0
# 13                             1               0                              0
# JobRole_Research.Director JobRole_Research.Scientist JobRole_Sales.Executive
# 1                          0                          0                       0
# 2                          0                          0                       0
# 3                          0                          0                       0
# 4                          0                          0                       0
# 5                          0                          0                       0
# 6                          0                          0                       0
# 7                          0                          0                       0
# 8                          0                          0                       0
# 9                          0                          0                       0
# 10                         0                          0                       0
# 11                         0                          1                       0
# 12                         0                          0                       0
# 13                         0                          0                       0
# JobRole_Sales.Representative MaritalStatus_Divorced MaritalStatus_Married
# 1                             0                      0                     0
# 2                             0                      0                     0
# 3                             0                      0                     0
# 4                             0                      0                     0
# 5                             0                      0                     0
# 6                             1                      0                     0
# 7                             1                      0                     0
# 8                             1                      0                     0
# 9                             1                      0                     0
# 10                            1                      0                     0
# 11                            1                      0                     0
# 12                            1                      1                     1
# 13                            1                      0                     0
# MaritalStatus_Single Over18_Y OverTime_No OverTime_Yes
# 1                     0        0           1            0
# 2                     1        0           1            0
# 3                     1        0           1            0
# 4                     1        0           1            0
# 5                     1        0           1            0
# 6                     1        0           1            1
# 7                     1        0           1            1
# 8                     1        0           1            1
# 9                     1        0           1            1
# 10                    1        0           1            1
# 11                    1        0           1            1
# 12                    0        0           1            1
# 13                    1        0           1            1

#Run Regression model for dataset containing these 12 variables
reg_bestset = lm(Attrition_Yes ~ DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + 
                   JobSatisfaction + NumCompaniesWorked + TotalWorkingYears + BusinessTravel_Travel_Frequently 
                 + Gender_Female + JobRole_Laboratory.Technician + JobRole_Sales.Representative + OverTime_No 
                 + OverTime_Yes, data = train)

RMS Error for Training Data
pred = predict(reg_bestset, new = train)
# Warning message:
#   In predict.lm(reg_bestset, new = train) :
#   prediction from a rank-deficient fit may be misleading
trainRSS=sum((train$Attrition_Yes-pred)^2)
trainRMS = sqrt(trainRSS/nrow(train))
trainRMS
#[1] 0.3280286

RMS Error for Validation Data
pred = predict(reg_bestset, new = valid)
# Warning message:
#   In predict.lm(reg_bestset, new = valid) :
#   prediction from a rank-deficient fit may be misleading
validRSS_bestset = sum( (valid$Attrition_Yes - pred)^2 )
validRMS_bestset = sqrt( validRSS_bestset/nrow(valid) )
validRMS_bestset
#[1] 0.346973

#CONCLUSION: We dropped from 56 to 12 variables, and almost maintained the accuracy!

reg_bestset
# 
# Call:
#   lm(formula = Attrition_Yes ~ DistanceFromHome + EnvironmentSatisfaction + 
#        JobInvolvement + JobSatisfaction + NumCompaniesWorked + TotalWorkingYears + 
#        BusinessTravel_Travel_Frequently + Gender_Female + JobRole_Laboratory.Technician + 
#        JobRole_Sales.Representative + OverTime_No + OverTime_Yes, 
#      data = train)
# 
# Coefficients:
#   (Intercept)                  DistanceFromHome  
# 0.689075                          0.003919  
# EnvironmentSatisfaction                    JobInvolvement  
# -0.041996                         -0.066718  
# JobSatisfaction                NumCompaniesWorked  
# -0.042147                          0.017607  
# TotalWorkingYears  BusinessTravel_Travel_Frequently  
# -0.007311                          0.104460  
# Gender_Female     JobRole_Laboratory.Technician  
# -0.030162                          0.083539  
# JobRole_Sales.Representative                       OverTime_No  
# 0.236380                         -0.214577  
# OverTime_Yes  
# NA  


summary(reg_bestset)
# 
# Call:
#   lm(formula = Attrition_Yes ~ DistanceFromHome + EnvironmentSatisfaction + 
#        JobInvolvement + JobSatisfaction + NumCompaniesWorked + TotalWorkingYears + 
#        BusinessTravel_Travel_Frequently + Gender_Female + JobRole_Laboratory.Technician + 
#        JobRole_Sales.Representative + OverTime_No + OverTime_Yes, 
#      data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.57975 -0.19908 -0.08050  0.04384  1.27707 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       0.689075   0.059309  11.618  < 2e-16 ***
#   DistanceFromHome                  0.003919   0.001183   3.314 0.000949 ***
#   EnvironmentSatisfaction          -0.041996   0.008890  -4.724 2.59e-06 ***
#   JobInvolvement                   -0.066718   0.013272  -5.027 5.76e-07 ***
#   JobSatisfaction                  -0.042147   0.008698  -4.845 1.43e-06 ***
#   NumCompaniesWorked                0.017607   0.004007   4.394 1.21e-05 ***
#   TotalWorkingYears                -0.007311   0.001351  -5.410 7.63e-08 ***
#   BusinessTravel_Travel_Frequently  0.104460   0.025023   4.175 3.21e-05 ***
#   Gender_Female                    -0.030162   0.019644  -1.535 0.124950    
# JobRole_Laboratory.Technician     0.083539   0.026729   3.125 0.001820 ** 
#   JobRole_Sales.Representative      0.236380   0.042860   5.515 4.29e-08 ***
#   OverTime_No                      -0.214577   0.021330 -10.060  < 2e-16 ***
#   OverTime_Yes                            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3297 on 1164 degrees of freedom
# Multiple R-squared:  0.2023,	Adjusted R-squared:  0.1947 
# F-statistic: 26.83 on 11 and 1164 DF,  p-value: < 2.2e-16