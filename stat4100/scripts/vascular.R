# trying to use R to account and calculate an RCBD 
# (randomized complete block design)
# book example pg 145

# allows you to input the data in the console.  enter a blank line to close.
x <- scan() #create your own vector one element at a time.
# or if you have forked the repo
x <- scan("data/vascgraft.txt")
PSI.labels <- c(8500,8700,8900,9100)
vasc <- data.frame(PSI=gl(4,6,24, labels = PSI.labels),
                         block=gl(6,1,24), x) 
vasc.aov <- aov(x~block + PSI, data = vasc)
anova(vasc.aov) # print the model anova table to the console
summary(vasc.aov) # print a summary of the model. similar to above.

# post hoc
TukeyHSD(vasc.aov)
# if we only want to see the PSI Tukey comparisons
TukeyHSD(vasc.aov)$PSI
plot(TukeyHSD(vasc.aov)) # plot tukey comparisons


#############   CHECK  ##############
# checking our anova results
# create summary stats and table
library(dplyr) # type install.packages("dplyr") is you do not have this.
vasc.blocks <- vasc %>%
  group_by(block) %>%
  summarize_each(funs(mean, sum), x)
vasc.trt <- vasc %>%
  group_by(PSI) %>%
  summarize_each(funs(mean, sum), x)
y.t <- vasc.trt$sum # treatment totals
y.b <- vasc.blocks$sum # block totals
y.. <- sum(x)
b <- length(y.b)
a <- length(y.t)
N <- length(x)
sst <- sum(x^2) - sum(x)^2/N
sstr <- (1/b) * sum(vasc.trt$sum^2) - y..^2/N
ssb <- (1/a) * sum(y.b^2) - y..^2/N
sse <- sst - sstr - ssb
mstr <- sstr/(a-1)
msb <- ssb/(b-1)
mse <- sse/((a-1)*(b-1))
F0 <- mstr/mse
p.F <- 1 - pf(F0, a-1, (a-1)*(b-1))