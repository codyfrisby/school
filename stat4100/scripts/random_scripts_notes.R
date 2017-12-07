### Notes and code that don't have a home ###
#############################################

# read the data
plants <- read.table("data/plants.txt", header = T)
# interaction plot
with(plants, interaction.plot(Irrigation, Fertilizer, Response, 
                              col = 2:3))

fit.plants <- aov(Response ~ Irrigation*Fertilizer, plants)
summary(fit.plants)
plot(fit.plants, which = c(1,2)) # n is small

library(phia) # library for
interactionMeans(fit.plants)
plot(interactionMeans(fit.plants))

fit.class <- aov(Response ~ Percent*Pressure*Speed, class.example)
library(dplyr)
groups <- class.example %>%
    group_by(Percent, Pressure, Speed) %>%
    summarise_each(funs(mean), Response)
summary(fit.class)
plot(fit.class, 1:2)


# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- mtcars$mpg 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

data(OrchardSprays)
str(OrchardSprays)

plot(decrease~treatment,data=OrchardSprays)
model1=lm(decrease~treatment,data=OrchardSprays)
summary(model1)

library(nlme)
model2=gls(decrease~treatment,data=OrchardSprays)
model3=update(model2,weights=varIdent(form=~1|treatment)) 

AIC(model2,model3) # Akaike´s information criterion

summary(model2)
summary(model3)

# successive difference contrasts
library(MASS) # Venables & Ripley
#contr.sdif()

options(contrasts=c("contr.sdif","contr.sdif"))
model4=gls(decrease~treatment,data=OrchardSprays)
model5=update(model2,weights=varIdent(form=~1|treatment)) 

summary(model5)

###############
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
# standardized residuals versus fitted values by gender
plot(fm1, resid(., type = "p") ~ fitted(.) | Sex, abline = 0)
# box-plots of residuals by Subject
plot(fm1, Subject ~ resid(.))
# observed versus fitted values by Subject
plot(fm1, distance ~ fitted(.) | Subject, abline = c(0,1))
##############

day1 <- subset(H, H$C  == 19.95)
day2 <- subset(H, H$C == 24.95)
par(mfrow=c(1,2))
interaction.plot(day1$A, day1$B, day1$y, 
                 col = 2:4, xlab = "A", ylab="orders",
                 trace.label = "B", main = "19.95")
interaction.plot(day2$A, day2$B, day2$y, col = 2:4, 
                 xlab = "A", ylab="orders",
                 trace.label = "B", main = "24.95")



