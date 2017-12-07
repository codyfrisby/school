#load my data into R
trees <- read.csv("C:/Users/cfrisby/Desktop/STAT/Project3/trees.csv")
#libraries used
library(data.table); library(dplyr)
trees <- data.table(trees)

#summarise my data
t <- trees %>%
        group_by(Stratum) %>%
        summarise_each(funs(mean, sd), Trees)
#add columns
t$N <- c(59, 66, 67) #this is N per strata
t$Tao <- t$mean * t$N #this is the estimate of Tao per strata.

boxplot(trees$Trees ~ trees$Stratum, main = "Trees by Stratum", 
        xlab = "Stratum", ylab = "Trees", col = "green")

#I will get estimates on the bound by bootstrapping the data without Strata
n <- length(c$Trees)
rfs <- 1000
x <- c$Trees 
C <- matrix(sample(x, n * rfs, replace = TRUE), rfs, n)
Tao_c <- apply(C, 1, mean) * 67
quantile(Tao_c, c(0.025, 0.975))
hist(Tao_c, col="green", xlab = "Trees", main = "Error on the Estimate for
     Total Trees in Stratum A")
# I did the above threes times.  One for each stratum.

D$Tao <- D$Tao_a + D$Tao_b + D$Tao_c
#Now we have a column that estimates trees using the strata data.
hist(D$Tao, col = "green", freq = FALSE)
mean(D$Tao)
quantile(D$Tao, c(0.025, 0.975))
