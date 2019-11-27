# PA CA: Loan Approval

# Load the libraries and install the ones we do not have in the system
pacman::p_load(tidyverse, caret, corrplot, caTools, knitr, car, ROCR, IRdisplay, e1071, earth, purrr, ggplot2,
               stringr, data.table, modelr, broom, relaimpo, glue, readxl, psych, lubridate, ggpubr, moments)

# Set working directory to source file directory
# setwd("C:/Users/isspcs/Desktop/workshop-data")

# Read the data file 
data = read.csv("loans.csv")
data = as.tibble(data)
summary(data)
str(data)
head(data)
colnames(data)

# set the variables to factors (categorical data)
cols =  c('creditpolicy', 'targetloanstatus')
data[,cols] = lapply(data[,cols], as.factor)
str(data)

# check for the 0 and 1 using contrasts
contrasts(data$targetloanstatus)

data %>%
  group_by(targetloanstatus)%>%
  summarise(tot = n()/nrow(data))%>%
  ggplot(aes(x=targetloanstatus, y=tot, fill = targetloanstatus)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label = round(tot, 2)), vjust = 2)

# Data Cleaning
data.clean <- subset(data, data$emplength != "n/a")
data.clean <- subset(data.clean, data.clean$homeownership != "NONE")
data.clean <- subset(data.clean, !(is.na(data.clean$annualinc)))
data.clean <- subset(data.clean, data.clean$dti != 0)
data.clean <- subset(data.clean, !(is.na(data.clean$delinq2yrs)))
data.clean <- subset(data.clean, !(is.na(data.clean$revolutil)))

summary(data.clean)
str(data.clean)

# 41,143 records left after trimming the missing values
# Of which 15% with targetloanstatus = 1
data.clean %>%
  group_by(targetloanstatus)%>%
  summarise(tot = n()/nrow(data.clean))%>%
  ggplot(aes(x=targetloanstatus, y=tot, fill = targetloanstatus)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label = round(tot, 2)), vjust = 2)

# Data visualisation
hist(data.clean$loanamnt)
hist(data.clean$intrate)
hist(data.clean$installment)
boxplot(log(data.clean$annualinc))
hist(data.clean$dti)
hist(data.clean$delinq2yrs)
hist(data.clean$inqlast6mths)
hist(data.clean$openacc)
hist(data.clean$revolbal)
hist(data.clean$revolutil)
hist(data.clean$totalacc)


# Split the data into training and testing
#set initial seed
set.seed(123)

# create a boolean flag to split data
splitData = sample.split(data.clean$targetloanstatus, SplitRatio = 0.5)

# create train and test datasets
train_set = data.clean[splitData,]
nrow(train_set)/nrow(data.clean)

test_set = data.clean[!splitData,]
nrow(test_set)/nrow(data.clean)

colnames(train_set)

# use train to create our 1st logistic regression model
# use all independent variables 
model1 = glm(targetloanstatus ~ . -id - annualinc +log(annualinc), data = train_set, family = binomial)
summary(model1)

# check for multi-collinearity
vif(model1)

model1 = step(model1, trace = F)

summary(model1)

# test it on the train set
trainPredict = predict(model1, newdata = train_set, 
                       type = 'response')


# assign 0s or 1s for the values
p_class = ifelse(trainPredict > 0.15, 1, 0)

matrix_table = table(train_set$targetloanstatus, p_class)
matrix_table

# Accuracy 
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)

#confusionMatrix(table(p_class,train_set$subscribed), positive='1')

varImp(model1)

# test it on the test set
testPredict = predict(model1, newdata = test_set, 
                      type = 'response')


p_class = ifelse(testPredict > 0.15, 1, 0)

matrix_table = table(test_set$targetloanstatus, p_class)
matrix_table

# Accuracy 
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)

head(sort(testPredict, decreasing = T),10)

# And then a lift chart
pred = prediction(trainPredict, train_set$targetloanstatus)

perf = performance(pred, "lift", "rpp" )
plot(perf, main="lift curve", xlab = 'Proportion of Approved Loans (sorted prob)')

# p-value for the model
with(model1, pchisq(null.deviance - deviance,
                    df.null - df.residual, lower.tail=F))

confusionMatrix(table(p_class, test_set$targetloanstatus), positive='1')

options(repr.plot.width=8, repr.plot.height=6)

# we can examine many cut-offs visually
pred <- prediction( trainPredict, train_set$targetloanstatus )

perf <- performance( pred, "tpr", "fpr" )

plot( perf, colorize = TRUE,
     print.cutoffs.at = seq(0,1,0.1), 
     text.adj = c(-0.2, 1.7))

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

cost.perf = performance(pred, "cost", cost.fp = 4, cost.fn = 1)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
