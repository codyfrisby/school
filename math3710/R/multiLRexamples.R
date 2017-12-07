# Multi Linear Reg examples from Coursera's Regression class by Brian Caffo
library(ggplot2); library(GGally); library(datasets); library(dplyr)
data(swiss)
g <- ggpairs(swiss, lower = list(continuous = "smooth"))
g # this is a slow plot
# useful ways of quickly looking at the model will all variables
lm(Fertility ~ ., data=swiss)$coef
summary(lm(Fertility ~ ., data=swiss))
summary(lm(Fertility ~ ., data=swiss))$coef
# now with just agriculture
summary(lm(Fertility ~ Agriculture, data=swiss))$coef
# look how the sign has flipped on Agriculture

# example using insect spray data set
data("InsectSprays")
g <- ggplot(data = InsectSprays, aes(x = spray, y = count, fill = spray))
g <- g + geom_violin(colour = "black", size = 2)
g <- g + xlab("Type of Insect Spray") + ylab("Count")
g
# linear model
summary(lm(count ~ spray, data = InsectSprays))$coef
# same as linear model
summary(aov(count ~ spray, data = InsectSprays))

# Dummy variables
summary(lm(count ~ 
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F'))
           , data = InsectSprays))$coef
# All six vars, notice the NA with the spray A coeff
lm(count ~ 
     I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
     I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
     I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays)

# too many vars

# now what if we remove the intercept?  This is done by adding a -1
# to the model formula
summary(lm(count ~ spray -1, InsectSprays))$coef
# same as 
summarise(group_by(InsectSprays, spray), mn = mean(count))
# notice something very important.  We can run statistical tests 
# between catagorical treatments by fitting a linear model and 
# putting the comparison factor first.  We can run the same test 
# comparing each level to zero by omitting the intercept.
# we can change the reference level by using the relevel() 
# command in R.
?relevel
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, InsectSprays))$coef
# A caveat of this example is that the data is count data.  Some
# of our linear model assumptions are not valid here.  Also, looking
# a the violin plot it is apparent that homoscedasticity is not valid.

# further examples
library(datasets); library(dplyr); data(swiss)
head(swiss)
# the dist of Catholic var is bimodal....we are going to create a 
# recoded binary variable using this data
swiss <- mutate(swiss, Catholicbin = 1 * (Catholic > 50))
head(swiss)
# plot the data, overlaying catholicbin
g <- ggplot(data=swiss, aes(x=Agriculture, y=Fertility, 
                            colour=Catholicbin))
g <- g + geom_point(size = 4, colour = "black") + 
          geom_point(aes(colour=factor(Catholicbin)))
g <- g + xlab("% in Agriculture") + ylab("Fertility")
g

