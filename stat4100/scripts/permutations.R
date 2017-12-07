library(pracma)
A <- c(7, 5, 10); B <- c(10, 9, 12)
cv <- mean(B) - mean(A) # this is our null hypothesis critical value
x <- c(A,B) # for ease of sampling
M <- perms(x) # matrix with all possible permutations of A and B.
# to calculate the mean difference of each permutation
means1 <- numeric(720)
for (j in 1:720){
  means1[j] <- mean(M[j,4:6]) - mean(M[j,1:3])
}
# trying with out a for loop
means <- apply(M[,4:6], 1, mean) - apply(M[,1:3], 1, mean)
# do i get the same result?
identical(means, means1)
# yes, so we forgo the for loop and choose to use the apply function.
M <- cbind(M, means)
quantile(means, c(0.025, 0.975))
# this is our 95% quantile.  If our cv is outside this quantile we
# would rejust the null hypothesis that B = A.

# p value
sum(means >= cv)/length(means)

#some added summary info
library(data.table)
M1 <- data.table(M)
Ms <- M1[, list(freq = .N), by = "means"]
Ms[order(means)]