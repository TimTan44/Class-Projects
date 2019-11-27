
# setting up the infrastructure
#install.packages('pacman', repos = "http://cran.us.r-project.org")

# using this we load the libraries 
# it installs the ones we do not have in our system
pacman::p_load(tidyverse, reshape, lubridate)

# pay attention to the '/'
setwd("C:/Users/Dehong Tan/Desktop/NUS/MBAP/Day 3")

# Complete code:
titanic = read.csv("titanic3.csv", sep=",", 
                   comment.char = "", stringsAsFactor = F)

dim(titanic)

# Complete code: print the structure of dataset

install.packages('datasauRus', repos = "http://cran.us.r-project.org")
head(datasaurus_dozen)

ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
  geom_point()+
  facet_wrap(~dataset, ncol=4)

# Complete code: print first 4 records
titanic %>% top_n(4, wt=pclass)
head(titanic,4)

cols_to_change = c('pclass','survived')

# check
sapply(titanic[cols_to_change], class)

# complete code
titanic[cols_to_change] = lapply(titanic[cols_to_change],factor)

# check
sapply(titanic[cols_to_change], class)
sapply(titanic, class)

# print column names
colnames(titanic)

# base function summary()
summary(titanic)

# Summary by a grouped column
# column: embarked
# method: summarise_all() [dplyr]

titanic %>%
group_by(embarked) %>%
summarise_all(mean)

warnings()
# Summary by a grouped column
# group column: embarked
# method: summarise_if() [dplyr]

titanic %>%
group_by(embarked) %>%
summarise_if(is.numeric, mean, na.rm = T)


# including multiple measures
# mean, median, max etc.
titanic %>%
group_by(embarked) %>%
summarise_if(is.numeric, funs(mean, median, max), na.rm = T)

# compute the proportion of missing data
# use complete.cases(.)
# Complete code:

missing = titanic %>%
          filter(!complete.cases(.))

missing
# find proportion of missing values
# Complete code:

nrow(missing)/nrow(titanic)

# go by columns 
# compute missing proportions

colMeans(is.na(titanic))

# Count of embarked by category values

titanic %>%
count(embarked)

# counts of embarked for each level of pclass variable
# Complete code:

titanic %>%
          
summarise(count = n())

# counts of embarked 
# for each level of pclass and sex variable

titanic %>%
count(embarked, pclass, sex)

# people survived in each class

tabClass = table(titanic$survived, titanic$pclass)
tabClass


# add row and column sums
addmargins(tabClass)

# various boats and suvival counts
tabBoat = table(titanic$survived, titanic$boat)
tabBoat

# instead of counts
# show proportion in tabClass

prop.table(tabClass)

# proportion of male, female
# survived in each class
# Complete code:

titanic%>%
group_by(pclass, sex, survived)%>% count(.) %>%
  mutate(freq = n / sum(n))

unique(titanic$pclass)

# Complete code: find unique values using distinct() function from dplyr

titanic %>% distinct(titanic$pclass)

# top 4 ages
titanic %>%
arrange(desc(age)) %>%
top_n(4, wt = age)

# Write code: top 2 ages by class
titanic %>%
  arrange(desc(age)) %>%
  top_n(2, wt = age)

# Write code: bottom 2 ages by class
titanic %>%
  arrange(desc(age)) %>%
  top_n(-2, wt = age)

# Complete code: for every class, for every gender see top 3  fares

titanic %>%
group_by(pclass,sex)%>%
  select(pclass, sex, fare)%>%
  top_n(3, wt=fare)%>%
  distinct(fare) # why distinct ?

# Write code: challenge-1
# find out the name, age of oldest passanger. Barkworth, Mr. Algernon Henry Wilson, 80
# did he/she survive? YES

titanic %>%
  arrange(desc(age))%>%
  top_n(1, wt = age)

# Write code: challenge-2
# name, age of youngest passenger who didn't make it alive. Danbom, Master. Gilbert Sigvard Emanuel, 0.33

titanic %>%
  arrange(age)

t = titanic
ggplot(t, aes(x=survived)) + geom_bar()

# putting in some labels

survivedBar = ggplot(t, aes(x=survived)) + geom_bar()


survivedLabels = labs(x= "survival", y= "Number of Passengers", 
                      title = "Survival Rate", subtitle = "On the titanic")

# join 2 components
survivedBar + survivedLabels

# a more elaborate bar chart
# fill with a numeric col gives gradient
# fill with category gives distinct colors
# stat: The statistical transformation to use on the data for this layer
t %>%
    group_by(survived) %>%
    summarise(count_level = n(),
    percentage = n()/nrow(t))%>%
    ggplot(aes(x = as.factor(survived), 
               y = count_level,fill=survived )) +
    geom_bar(stat='identity') +
    geom_text(aes(label=round(percentage,2)),vjust = 2) %>%
    labs(x= "survival", y= "Number of Passengers", 
         title = "Survival Rate", subtitle = "On the titanic")

# Write code:try with the variable: sex


# Write code:
# now try with these variables:
#   sex
#   pclass with 'fill = survived'; plot the counts
#   group_by( survived, sex) plot proportions of survived
#   group ages into children (0-15), adults, elderly (55+)
#   and analyze the survival proportions

############################################################################
#children (0-15), adults, elderly (55+) and analyze the survival proportions
############################################################################
t$level[t$age < 15 & t$age >= 0] = 'children'
t$level[t$age < 55 & t$age >= 15] = 'adults'
t$level[t$age >= 55] = 'elderly'

# Write code:
# now try with these variables:
#   level
#   pclass with 'fill = survived'; plot the counts
#   group_by( survived, sex) plot proportions of survived
#   group ages into children (0-15), adults, elderly (55+)
#   and analyze the survival proportions

view(t)

t %>%
  ggplot(aes(x = pclass, fill = factor(survived))) +
  geom_bar(stat='count') +
  labs(x = 'class') 

t %>%
  ggplot(aes(x = pclass, fill = factor(survived))) +
  geom_bar(position = 'fill') +
  labs(x = 'class') 

t %>%
  ggplot(aes(x = pclass, fill = factor(survived))) +
  geom_bar(position='dodge') +
  labs(x = 'class') 


t %>%
  ggplot(aes(x = level, fill = factor(survived))) +
  geom_bar(position='dodge') +
  labs(x = 'Age Group') 

ggplot(t, aes(x=age)) + geom_histogram()

ggplot(t, aes(x=age)) + geom_histogram() + facet_grid(~sex)  

# control bin size
ggplot(t, aes(x=age)) + geom_histogram(bins = 10) + facet_grid(.~sex) 

# Write code: box plot of age by class
ggplot (t, aes(as.factor(pclass),age)) + geom_boxplot()

# Write code: box plot of fare by sex
ggplot (t, aes(as.factor(sex),fare)) + geom_boxplot()

# Write code: box plot of age by boat
ggplot (t, aes(as.factor(boat),age)) + geom_boxplot(na.rm = T)


# Scatter plot
t%>%
ggplot(aes(age, fare)) +
geom_point()

t%>%
ggplot(aes(age, fare)) +
geom_point() +
stat_smooth(method= 'lm')

t%>%
ggplot(aes(age, fare, color = pclass)) +
geom_point() +
stat_smooth(method= 'lm', se = F)

t%>%
ggplot(aes(age, fare)) +
geom_point() + facet_wrap(~pclass)+
stat_smooth(method= 'lm', se = F)

t%>%
ggplot(aes(pclass, age, colour = factor(survived))) +
geom_point()

t%>%
ggplot(aes(pclass, age, colour = factor(survived))) +
geom_jitter(alpha = 0.6)

# Write code:
# - plot age and embarked on scatter plot.
# - use survived as differentiator of markers

t%>%
  ggplot(aes(age, embarked, colour = survived)) +
  geom_point()

# split by sex
head(t,2)
list_col = list(Age=t$age, Cabin=t$cabin,SibSp=t$age, Parch=t$parch, 
                Sex=t$sex, Pclass=t$pclass, Survived=t$survived, Emb = t$embarked, Fare = t$fare)

g1 = as.data.frame(list_col)
g1

# Complete code:
#   - split the earlier chart by variable sex
#   - create similar chart with: survived, embarked, fare

ggplot() +
  geom_jitter(data=g1, aes(Pclass, Age, colour = )) + 
  facet_grid() + 
  theme_light()

# Complete code: similar chart with survived, embarked, fare

ggplot() +
  geom_jitter(data=g1, aes(Survived, Emb)) + 
  facet_grid() + 
  theme_light()

stocks = read.csv('stock.csv')
head(stocks)

# proper data type is necessary
stocks$Date = dmy(stocks$Date)
sapply(stocks, class)

head(stocks,2)

ggplot(stocks, aes(x = Date, y = Adj_Close)) +
  geom_line(color = 'red') + ggtitle("Stock Prices")

ggplot(stocks, aes(x = Date)) +
  geom_line(aes(y = Open,color = 'Open')) +
  geom_line(aes(y = Close, color = 'Close')) +
  geom_line(aes(y = Adj_Close, color = 'Adj_Close'))

head(ChickWeight,4)

view(ChickWeight)

# Write code:
# - use the ChickWeight dataset (inbuilt)
# - plot the 4 diets to see weight gain over time for each Chick
# - after geom_line() add geom_smooth() with one of the methods as below:
#    method : smoothing method to be used. Possible values are lm, glm, gam, loess, rlm
#    method = "loess": This is the default and it computes a smooth local regression
#    method ="lm": fits a linear model 

ggplot(ChickWeight, aes(x = Time, y = weight)) +
  geom_line(color = 'red') + ggtitle("Weight Gain Over Time") +
  geom_smooth(method = "loess")
