####### Exploring 3^2 full factorial models using R ########

df <- read.csv("~/Documents/STAT4100/data/hw9.csv")
df.c <- df
fit.lm <- lm(y ~ glass*temp, df) # fit with continuous vars
fit.aov.c <- aov(y ~ .*., df)
df$glass <- as.factor(df$glass)
df$temp <- as.factor(df$temp)
fit.aov <- aov(y ~ .*., df) # need factor variables
summary(fit.aov)
summary(fit.aov.c)
# R package for response surface models
library(rsm)
fit.rsm <- rsm(y ~ SO(glass,temp), df.c) 
# predictors need to be continuous.
# also, the anova(fit.rsm) does not seperate
# out the individual SS terms.....lame
# to use R and see the SS terms for each term in the model...
# ... first create new variables
df.c$glassglass <- df.c$glass^2
df.c$temptemp <- df.c$temp^2
fit.full <- aov(y ~ .*., df.c)
# the below 2 values should be equal
sum(anova(fit.full)[1:2,2]);anova(fit.rsm)[1,2]
# yes! they are, what about...
anova(fit.rsm)[3,2]; sum(anova(fit.full)[c(3,4),2])
# Yes!, so what about the other interaction terms...
# I believe fit.full has too many terms...I may need to 
# fix this
fit.full2 <- aov(y ~ glass*temp+temptemp+glassglass, df.c)
# this model results in the same value for residuals as the 
# fit.rsm model....may be on the right track now.
# basically the same models...minus rounding error
anova(fit.full2, fit.rsm)
# tool life example from textbook
tool <- read.table("~/Documents/STAT4100/data/toollife.txt", 
                   header = TRUE)
tool$Angle2 <- tool$Angle^2; tool$Speed2 <- tool$Speed^2

anova(aov(Life ~ Angle*Speed, tool))
anova(aov(Life ~ Angle*Speed+Speed2+Angle2, tool))
# or like 9.1.2
anova(aov(Life ~ .*., tool))
