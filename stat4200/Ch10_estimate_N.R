#if s is the r.v.
s <- function(t, n, s){
        N <- n*t/s; names(N) <- "N hat"
        B <- 2 * sqrt((t^2)*(n)*(n-s)/(s^3)); names(B) <- "Bound"
        q <- c(N-B, N+B) #Confidence Interval
        names(q) <- c("2.5%","97.5%")
        c(N, B, q)
}

#if n is the r.v.
n <- function(t, n, s){
        N <- n*t/s; names(N) <- "N hat"
        B <- 2 * sqrt((t^2)*(n)*(n-s)/((s^2*(s +1)))); names(B) <- "Bound"
        q <- c(N-B, N+B) #Confidence Interval
        names(q) <- c("2.5%","97.5%")
        c(N, B, q)
}