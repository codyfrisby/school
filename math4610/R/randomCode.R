## examples in Advanced R Programming ##
## show that 1^2 + 2^2 + 3^2 + ... + n^2 = .....
# use my interpolation solver:
source("~/Documents/school/math4610/R/qsolve.R")

f <- function(n) {
  for(i in 0:n) {
    x <- 1:i
    y <- x^2
    #print(sum(y))
  }
  return(sum(y))
}
f(3) # this function is garbage


# can I write it recursively ???
# here's my attempt:
g <- function(n, p = 2) { 
  if (n == 0) {
    return(0)
  } else {
    return(n^p + g(n - 1, p))
  }
}
g(3)

x <- c(1, 4, 6, 9)
y <- sapply(x, g)
interpolate(x, y)

# in class
x <- 0:3
y <- c(0, 1, 5, 14)
# and now using my function
interpolate(x, y)

# what about one more degree?
x <- sample(0:100, 4)
y <- sapply(x, g)
interpolate(x, y) # same result as the above one!!!!! awesome!

# ONE MORE
x <- 0:5
y <- c(0, 1, 5, 14, 30, 55)
interpolate(x, y) # same fuckin result!!!!
#Closures

power <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}

square <- power(2)
square(3)

cube <- power(3)
cube(2)

# what's the function made of?
square # not very useful
as.list(environment(square)) # much more useful
as.list(environment(cube))

# we can also use pryr:

pryr::unenclose(square)

## memory usage in R using pryr package
pryr::mem_used()
pryr::mem_change(x <- 1:1e6)
pryr::mem_change(rm(x))


srate <- 1e3 # sampling rate
f1 <- 1 # frequency
f2 <- 2 # frequency
a1 <- 1 # amplitude
a2 <- 2 # amplitude

time <- seq(0, 2, 1/srate)
n <- length(time)

## generate a signal
signal <- a1 * sin(2 * pi * f1 * time) + a2 * sin(2 * pi * f2 * time)
plot(time, signal, type = "l", ylab = "Amplitude", lwd = 2, 
     main = "Time Domain", xlab = "Time (s)")
grid()
abline(h = 0, lty =3); abline(v = 0, lty = 3)



i <- 0+1i # define i, imaginary number
ftime <- seq(0, (n-1)/n, length.out = n)
fcoef <- rep(0, length(signal))

t1 <- proc.time()
### start loop
for (fi in 1:n) {
  ## complex sine wave
  csw <- exp(-i * 2 * pi * (fi - 1) * ftime)
  # dot product between sine wave and signal
  fcoef[fi] <- sum(signal * csw) / n
} ## end fourier for loop
t2 <- proc.time()

# compute amplitudes
ampl <- 2 * abs(fcoef)
# compute frequencies
hz <- seq(0, srate/2, length.out = n/2)  

t3 <- proc.time()
Rfft <- data.frame(FFT = 2 * abs(fft(signal) / n)[1:length(hz)])
t4 <- proc.time()

cat("O(n^2): \n")
print(t2 - t1) # O(n^2)
cat("O(n log(n )): \n")
print(t4 - t3) # O(n log(n))

n1 <- n^2
n2 <- n * log(n)

cat("percent fewer operations: \n")
cat((n2/n1) * 100, "\n") # percent of the operations



## trying ggplot instead:
# get data ready frist
df <- data.frame(amplitude = ampl[1:length(hz)], hertz = hz)
Rfft <- data.frame(FFT = 2 * abs(fft(signal) / n)[1:length(hz)])


# now use ggplot
library(ggplot2)
library(scales)
gg <- ggplot(data = df, aes(x = hertz, y = amplitude))
gg <- gg + geom_segment(aes(xend = hertz, yend = amplitude - amplitude),
                        color = "dodgerblue") + 
  geom_point(size = 2, color = "dodgerblue") + 
  xlim(0, 10) + ylab("amplitude") + theme_bw() + 
  ggtitle(paste("a1 = ",a1,", a2 = ",a2,", f1 = ",f1, ", f2 = ",f2))
print(gg + geom_point(data = Rfft, 
                      mapping = aes(x = df$hertz, y = FFT), 
                      color = "red") + 
        scale_x_continuous(limits = c(1, 10) , breaks = pretty_breaks()))


