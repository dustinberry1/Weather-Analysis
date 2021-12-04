day <- read.csv("~/Maryville/R Programming/FinalProject/day.csv")
  View(day)

library(ggplot2) #load ggplot2 for plots
library(pscl) #load pscl for logistic models
library(arm) #load arm for bayesglm function

#2a convert numerical season to characters
day$season <- as.character(day$season)
day$season[day$season == "1"] <- "spring"
day$season[day$season == "2"] <- "summer"
day$season[day$season == "3"] <- "fall"
day$season[day$season == "4"] <- "winter"

#2b convert numerical weathersit to characters
day$weathersit <- as.character(day$weathersit)
day$weathersit[day$weathersit == "1"] <- "Good"
day$weathersit[day$weathersit == "2"] <- "Mist"
day$weathersit[day$weathersit == "3"] <- "Bad"
day$weathersit[day$weathersit == "4"] <- "Severe" #this never occurs in the data set

#3 consider the predictors and list all categorical values from list, determine type of variable
#test predictors for type
is.numeric(day$season)
is.numeric(day$holiday)
is.numeric(day$workingday)
is.numeric(day$weathersit)
is.numeric(day$atemp)
is.numeric(day$hum)
is.numeric(day$windspeed)
is.numeric(day$casual)
#convert the categorical variables to factors
day$season <- as.factor(day$season)
day$weathersit <- as.factor(day$weathersit)

#4 calculate the min, max, mean, median, sd, and quartiles of cnt
min(day$cnt)
max(day$cnt)
mean(day$cnt)
median(day$cnt)
sd(day$cnt)
quantile(day$cnt, probs = c(.25, .5, .75))

#5 calculate the min, max, mean, median, sd, and quartiles of registered
min(day$registered)
max(day$registered)
mean(day$registered)
median(day$registered)
sd(day$registered)
quantile(day$registered, probs = c(.25, .5, .75))

#6 calculate the correlation coefficient of the two variables registered and cnt
cor(day$registered, day$cnt)

#7 calculate the frequency table of season
table(day$season)  #calculate the frequency table
names(sort(table(day$season), decreasing = TRUE))[1]  #find the mode

#8 calculate the cross table of season and weathersit, then produce proportions by rows and columns
xtabs(~ season + weathersit, data = day)  #calculate cross table
xtabs.season.weathersit <- xtabs(~ season + weathersit, data = day)  #assign variable for calculation
prop.table(xtabs.season.weathersit, margin = 1)  #produce proportions by row
prop.table(xtabs.season.weathersit, margin = 2)  #produce proportions by column

#9 plot the histogram and density of the cnt and add the vertical line denoting the mean using ggplot2
ggplot(data = day, aes(x = cnt)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "deepskyblue2") + 
  ggtitle("Histogram and Density of Count") +
  geom_density(alpha = .3, fill = "khaki1") + 
  geom_vline(aes(xintercept = mean(cnt)), color = "red", linetype = "dotted", size = 1.5)

#10 scatter plot of cnt against registered and add trend line using ggplot2
ggplot(data = day, aes(x = registered, y = cnt)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Scatter Plot of Count against Registered") + 
  labs(y = "Count", x = "Registered")

#11 barplot of season and weathersit on the same barplot using ggplot2
ggplot(data = day, aes(x = weathersit, y = ..count..)) + 
  geom_bar(aes(fill = season)) + scale_fill_manual(values = c("#FFFF66", "#FFCC33", "#9966CC", "#339933"))

#12 boxplot cnt against weathersit and save graph in file using ggplot2
ggplot(data = day, aes(x = weathersit, y = cnt)) + 
  geom_boxplot(aes(col = weathersit ), notch = TRUE) + 
  xlab("Weather Situation") + ylab("Count")
ggsave("C:\\Users\\dusti\\Documents\\Maryville\\R Programming\\FinalProject\\cntweather.jpg", 
       width = 10, height = 10, units = "cm")

#13a perform multiple linear regression with cnt as response and season, weathersit, atemp, and registered as predictors
lm.result1 <- lm(cnt ~ season + weathersit + atemp + registered, data = day)
summary(lm.result1)

#13b perform multiple linear regression with cnt as response and season, workingday, weathersit, atemp, and registered as predictors
lm.result2 <- lm(cnt ~ season + workingday + weathersit + atemp + registered, data = day)
summary(lm.result2)

#13c perform multiple linear regression with cnt as response and season, holiday, workingday, weathersit, atemp, hum, windspeed, and registered as predictors
lm.result3 <- lm(cnt ~ season + holiday + workingday + weathersit + atemp + hum + windspeed + registered, data = day)
summary(lm.result3)

#13d Based on the adjusted R squared of .9653, I recommend the model in 13c (variable lm.result3).  This is due to it having the highest adjusted R squared value with the same p-value for all models, and six predictors given a three star statistical significance rating.

#14a forecast holiday using cnt, season, and registered
glm.a <- glm(holiday ~ cnt + season + registered, family = binomial, data = day)
summary(glm.a)

#14b forecast holiday using cnt, season, weathersit, and registered
glm.b <- glm(holiday ~ cnt + season + weathersit + registered, family = binomial, data = day)
summary(glm.b)

#14c forecast holiday using cnt, season, weathersit, workingday, and registered
#used bayesglm() to remove the 0 to 1 error
glm.c <- bayesglm(holiday ~ cnt + season + weathersit + workingday + registered, family = binomial, data = day)
summary(glm.c)

#14d compile McFadden/pseudo R squared for analysis
pR2(glm.a)
pR2(glm.b)
pR2(glm.c)
#Based on McFadden/pseudo R squared compiled below I recommend the model in 14c (variable glm.c).  It has the highest value at .27583974.  The McFadden value in 14c is over double that of any other model resulting in the best fit.

#15 will be in a seperate .Rmd file


