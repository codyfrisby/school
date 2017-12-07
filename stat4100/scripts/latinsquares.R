# latin squares model in R
# source http://www.r-bloggers.com/latin-squares-design-in-r/

fertil <- c(rep("fertil1",1), rep("fertil2",1), rep("fertil3",1), 
            rep("fertil4",1), rep("fertil5",1))
treat <- c(rep("treatA",5), rep("treatB",5), rep("treatC",5), 
           rep("treatD",5), rep("treatE",5))
seed <- c("A","E","C","B","D", "C","B","A","D","E", "B","C","D",
          "E","A", "D","A","E","C","B", "E","D","B","A","C")
freq <- c(42,45,41,56,47, 47,54,46,52,49, 55,52,57,49,45, 51,44,47,
          50,54, 44,50,48,43,46)

mydata <- data.frame(treat, fertil, seed, freq)
matrix(mydata$seed, 5,5)
matrix(mydata$freq, 5,5)
# 3 plots are created
par(mfrow=c(2,2))
plot(freq ~ fertil+treat+seed, mydata)
myfit <- lm(freq ~ fertil+treat+seed, mydata)
anova(myfit) # same model as below
fit.aov <- aov(freq ~ fertil+treat+seed, mydata)
anova(fit.aov) # note, these are the same models

# model.table()
model.tables(fit.aov, "means")
test <- model.tables(fit.aov, "means")
# or effects
model.tables(fit.aov, "effects")

# random latin function
source("scripts/latin.R")
latin(4)

# latin squares example
rocket <- read.table("data/rocket.txt", header = TRUE)
par(mfrow=c(2,2))
plot(y~op+batch+treat, rocket) #creates 3 plots
rocket.lm <- lm(y~factor(op)+factor(batch)+treat, rocket)
anova(rocket.lm)
