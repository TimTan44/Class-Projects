
pacman::p_load(tidyverse, purrr, stringr, data.table, modelr)

## Code in all the examples from slide 33 onwards

## map function
files = c('marketing.csv', 'salaries.csv')
multiple_files = map(files, read.csv)
str(multiple_files)

mtcars = data.frame(mtcars)

mtcars

models = mtcars %>% 
  split(.$cyl) %>% 
  map( ~ lm(mpg ~ wt, data = .)) 

####################################

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

## walk2 function

df = ChickWeight
str(ChickWeight)

datasets = ChickWeight %>% 
  split(.$Diet) 

paths = str_c(names(datasets), 'diet', '.csv')
walk2(datasets, paths, write.csv)

split_by_diet = df %>% 
  group_by(Diet) %>% 
  nest()

###################

LinearModel = function(df){
  lm(weight ~ Time + Chick, data= df)
}

split_by_diet = split_by_diet %>% 
  mutate(model = map(split_by_diet$data, LinearModel))

#########################

split_by_diet %>% 
  mutate(quality = map(model, broom::glance)) %>% 
  unnest(quality, .drop = T) %>% 
  arrange(r.squared)

split_by_diet = split_by_diet %>%
  mutate(resids = map2(data, model, add_residuals))
  
#################### ACTIVITY

############################################################################################

#################### TASK 1
set.seed(10)
df = data.frame(Emp.id = 1:9,
                dep = sample(rep(c('front-desk', 'back-office', 'Reception'), each = 3)),
                Bonus.Cur = sample(2000:5000, replace = TRUE, size = 9),
                Bonus.Prev = sample(2000:5000, replace = TRUE, size = 9),
                Increment.Cur = round(runif(9, 0.0, 0.05), digits = 2),
                Increment.Prev = round(runif(9, 0.0, 0.05), digits = 2))
str(df)


dfnew = mutate(df, avg.incr = (df$Increment.Cur + df$Increment.Prev)/2,
         avg.bonus = (df$Bonus.Cur + df$Bonus.Prev)/2)

dfnew 
str(dfnew)
dfnew$Emp.id = as.factor(dfnew$Emp.id)

dfnew %>% 
  group_by(dep) %>% 
  ggplot(aes(x = avg.incr, y = avg.bonus), label ) +
  geom_point(aes(colour = as.factor(Emp.id))) +
  facet_wrap(~dep) +
  theme(legend.position = "right")+
  geom_text(aes(label = Emp.id), hjust=1)
  
######################## TASK 2

df = ChickWeight
View(df)

install.packages("directlabels")
library(directlabels)

df %>% 
  group_by(Diet) %>% 
  ggplot(aes(x = Time, y = weight, group = Chick)) +
  geom_line()+
  facet_wrap(~Diet)+
  geom_dl (aes(label = Chick), method = "last.points")

######################## TASK 3

setwd("C:/Users/Dehong Tan/Desktop/NUS/EBA5002 Business Analytics Practice/Statistics Bootcamp/Day 5/Tourism")
myfiles = list.files(pattern="*.xlsx")
View(myfiles)

tourism = ldply(myfiles, read_excel)
View(tourism)

?ldply

tourism = gather(tourism, "Month", "Total", Jan:Dec)
outliers = tourism %>% 
  filter(Year == 2019, Month != "Jan", Month != "Feb", Month != "Mar")

tourism2 = tourism %>% 
  filter(Year != 2019)

tourism2
View(tourism2)

tourism3 = tourism2 %>%  
  arrange(Continent, Year) %>% 
  mutate("TimeMth2010" = 1:768) %>% 
  split(.$Continent)

tourism3 = tourism2 %>%  
  arrange(Continent, Year) %>% 
  split(.$Continent)

View(tourism3)

paths = str_c(names(tourism3), 'Continent', '.csv')
walk2(tourism3, paths, write.csv)

Africa = read.csv("AFRICAContinent.csv")
Americas = read.csv("AMERICASContinent.csv")
Europe = read.csv("EUROPEContinent.csv")
Northasia = read.csv("NORTH ASIAContinent.csv")
Oceania = read.csv("OCEANIAContinent.csv")
Southasia = read.csv("SOUTH ASIAContinent.csv")
SEasia = read.csv("SOUTHEAST ASIAContinent.csv")
Westasia = read.csv("WEST ASIAContinent.csv")

View(Africa)

Africa = Africa %>% 
  mutate("TimeMth2010" = 1:n())

Americas = Americas %>% 
  mutate("TimeMth2010" = 1:n())

Europe = Europe %>% 
  mutate("TimeMth2010" = 1:n())

Northasia = Northasia %>% 
  mutate("TimeMth2010" = 1:n())

Oceania = Oceania %>% 
  mutate("TimeMth2010" = 1:n())

Southasia = Southasia %>% 
  mutate("TimeMth2010" = 1:n())

SEasia = SEasia %>% 
  mutate("TimeMth2010" = 1:n())

Westasia = Westasia %>% 
  mutate("TimeMth2010" = 1:n())

View(Americas2)

Americas2 %>% 
plot (ltotal, TimeMth2010)

Americas2 = Americas %>% 
  mutate(ltotal = log(Total))

modela1 = lm (ltotal ~ TimeMth2010, data = Americas2)
summary(modela1) 

modela2 = lm (ltotal ~ TimeMth2010 + I(TimeMth2010^2), data = Americas2)
summary(model2new) 

modela3 = lm (Total ~ TimeMth2010 + I(TimeMth2010^2), data = Americas)
summary(model2new) 

  
cor(Americas$Total, Americas$TimeMth2010)

p1 = ggplot(Africa, aes(x = Africa$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")
p2 = ggplot(Americas, aes(x = Americas$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")
p3 = ggplot(Europe, aes(x = Europe$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")
p4 = ggplot(Northasia, aes(x = Northasia$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")
p5 = ggplot(Oceania, aes(x = Oceania$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")
p6 = ggplot(Southasia, aes(x = Southasia$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")
p7 = ggplot(SEasia, aes(x = SEasia$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")
p8 = ggplot(Westasia, aes(x = Westasia$TimeMth2010, y = Total)) + geom_point() + geom_smooth (method = "loess")

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8 + rremove("x.text"),
          labels = c("Africa", "Americas", "Europe", 'NAsia', 'Oceania', 'SAsia', 'SEAsia', 'WAsia'),
          ncol = 4, nrow = 2)

##AFRICA##

model1 = lm (Total ~ TimeMth2010, data = Africa)
summary(model1) 

cor(Africa$Total, Africa$TimeMth2010) # Little correlation between Total and Time

plot (Africa$TimeMth2010, Africa$Total)

##AMERICAS##

model2 = lm (Total ~ TimeMth2010, data = Americas)
summary(model2) 

cor(Americas$Total, Americas$TimeMth2010)

res = residuals(model2)
plot(res)

model2new = lm (Total ~ TimeMth2010 + I(TimeMth2010^2), data = Americas)
summary(model2new) 

##EUROPE##

model3 = lm (Total ~ TimeMth2010, data = Europe)
summary(model3) 

model3new = lm (Total ~ TimeMth2010 + I(TimeMth2010^2), data = Europe)
summary(model3new) 

cor(Europe$Total, Europe$TimeMth2010)

##NORTHASIA##

model4 = lm (Total ~ TimeMth2010, data = Northasia)
summary(model4) 

model4new = lm (Total ~ TimeMth2010 + I(TimeMth2010^2), data = Northasia)
summary(model4new) 

cor(Northasia$Total, Northasia$TimeMth2010)

##OCEANIA##

model5 = lm (Total ~ TimeMth2010, data = Oceania)
summary(model5) 

plot (Oceania$TimeMth2010, Oceania$Total)

cor(Oceania$Total, Oceania$TimeMth2010) # Little correlation between Total and Time

##SOUTHASIA##

model6 = lm (Total ~ TimeMth2010, data = Southasia)
summary(model6) 

model6new = lm (Total ~ TimeMth2010 + I(TimeMth2010^2), data = Southasia)
summary(model6new) 

cor(Southasia$Total, Southasia$TimeMth2010)

##SEASIA##

model7 = lm (Total ~ TimeMth2010, data = SEasia)
summary(model7) 


model7new = lm (Total ~ TimeMth2010 + I(TimeMth2010^2), data = SEasia)
summary(model7new) 

cor(SEasia$Total, SEasia$TimeMth2010)

##WASIA##

model8 = lm (Total ~ TimeMth2010, data = Westasia)
summary(model8) 

plot (Westasia$TimeMth2010, Westasia$Total)

cor(Westasia$Total, Westasia$TimeMth2010) # Little correlation between Total and Time


######################## TASK 4

Delay <- read_csv("C:/Users/Dehong Tan/Desktop/NUS/EBA5002 Business Analytics Practice/Statistics Bootcamp/Day 5/Delay.csv")
View(Delay)
str(Delay)

Delay$`Flight Status`= factor(Delay$`Flight Status`,
                              levels = c("ontime", "delayed"),
                              labels = c(0,1))

Delay$CARRIER= factor(Delay$CARRIER)
Delay$ORIGIN= factor(Delay$ORIGIN)
Delay$Weather= factor(Delay$Weather)

model = glm (`Flight Status` ~ CARRIER + ORIGIN + Weather, family = binomial, data = Delay)
summary (model)

# Removing CARRIER as predictor variable

model = glm (`Flight Status` ~ ORIGIN + Weather, family = binomial, data = Delay)
summary (model) # Model did not improve as no significant improvement in AIC. There is Increase in residual variance vs previous too

# Removing ORIGIN as predictor variable

model = glm (`Flight Status` ~ CARRIER + Weather, family = binomial, data = Delay)
summary (model) # Model did not improve as no significant improvement in AIC/Increase in residual variance

# Removing WEATHER as predictor variable

model = glm (`Flight Status` ~ CARRIER + ORIGIN, family = binomial, data = Delay)
summary (model) # Model did not improve as no significant improvement in AIC/Increase in residual variance

# Adding in Days as predictor Variable

Delay$DAY_WEEK = factor(Delay$DAY_WEEK)
Delay$DAY_OF_MONTH= factor(Delay$DAY_OF_MONTH)

model = glm (`Flight Status` ~ CARRIER + ORIGIN + Weather + DAY_WEEK, family = binomial, data = Delay)
summary (model)

# Model did not significantly improve. Nonetheless to retain Day_WEEK as predictor as quite a number of Days
# are having low p-values indicating signifance in their presence in the model.

######################## TASK 5

pima = read.csv("C:/Users/Dehong Tan/Desktop/NUS/EBA5002 Business Analytics Practice/Statistics Bootcamp/Day 5/pima-indians-diabetes.csv",
                col.names = c("Pregnant", "Plasma_Glucose", "Dias_BP", "Triceps_Skin", "Serum_Insulin", "BMI", "DPF", "Age", "Diabetes"))

str(pima)
View(pima)

pima$Diabetes= factor(pima$Diabetes,
                              levels = c("0", "1"),
                              labels = c(0,1))

model = glm (Diabetes~ Pregnant + Plasma_Glucose + Dias_BP + Triceps_Skin + Serum_Insulin + BMI + Age + DPF, family = binomial, data = pima)
summary(model)

# Removing variables which are not significant and observing the resultant AIC and drop in residual variance
# Remove Triceps_Skin

model = glm (Diabetes~ Pregnant + Plasma_Glucose + Dias_BP + Serum_Insulin + BMI + Age + DPF, family = binomial, data = pima)
summary(model) # AIC Dropped slightly, can consider to permanently remove

# Remove Serum_Insulin

model = glm (Diabetes~ Pregnant + Plasma_Glucose + Dias_BP + BMI + Age + DPF, family = binomial, data = pima)
summary(model) # AIC Dropped slightly, can consider to permanently remove

# Remove Age

model = glm (Diabetes~ Pregnant + Plasma_Glucose + Dias_BP + BMI + DPF, family = binomial, data = pima)
summary(model) # Insignicant effects from dropping Age - to reinstate Age as it rationally could be a predicting factor

model = glm (Diabetes~ Pregnant + Plasma_Glucose + Dias_BP + BMI + Age + DPF, family = binomial, data = pima)
summary(model) # FINAL MODEL


######################## TASK 6

restaurants = read_csv("C:/Users/Dehong Tan/Desktop/NUS/EBA5002 Business Analytics Practice/Statistics Bootcamp/Day 5/restaurant/studenmunds_restaurants.csv")
str(restaurants)

plot(restaurants)
shapiro.test((restaurants$sales))
qqnorm(restaurants$sales)
qqline(restaurants$sales)

#tests indicate normality of Sales for linear regression

model = lm (sales~ competition + population + income, data = restaurants)
summary (model)

res = residuals(model)
plot(res) # normality of residuals, all variables are significant

# Sales = 1.02e05 - 9.08e03*Competition + 3.55*population + 1.29e00*income
# For every 1 unit increase in the predictor variable, sales increase/decrease by the corresponding regression coefficient of the predictor variable.
# All variables are significant from the p-values, at 5% level of significance. Judging from the magnitude of the regression coefficients, Competition 
# appears to have the greatest impact (inverse impact) on the profitability of the restaurant.
# Additional data which could be useful include demographics of surrounding population and average rents in area



######################## TASK 7

uni <- read_csv("C:/Users/Dehong Tan/Desktop/NUS/EBA5002 Business Analytics Practice/Statistics Bootcamp/Day 5/uni.csv")
View (uni)
str(uni)

shapiro.test(uni$salary) #p-value at 0.05, indicating normality of Salary 

unimale = uni %>% 
  filter(uni$sex == "Male")

unifemale = uni %>% 
  filter(uni$sex == "Female")

shapiro.test(unifemale$salary)
shapiro.test(unimale$salary)

t.test(unimale$salary, unifemale$salary, alternative = "greater") # high p-value indicates insufficent value to reject null hypothesis, indicating no significant
# difference in salaries of male and females

uniasstprof = uni %>% 
  filter(rank == "AsstProf")

uniassocprof = uni %>% 
  filter(rank == "AssocProf")

uniprof = uni %>% 
  filter(rank == "Prof")

t.test(uniasstprof$salary, uniassocprof$salary, alternative = "less") #high p-value indicates no significant difference btw assoc and assistant profs
t.test(uniprof$salary, uniasstprof$salary, alternative = "greater") # low p-value indicates significant difference between Prof and Asst Prof Salary
t.test(uniprof$salary, uniassocprof$salary, alternative = "greater") # high p-value indicates no significant difference between Prof and Assoc Prof salary


