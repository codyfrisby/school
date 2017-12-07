## Udemy course on understanding the Fourier transform
# Fourier Transform Video.  Code was in matlab.  

f1 <- 2 # frequency
f2 <- 3 # frequency
a1 <- 4 # amplitude
a2 <- 5 # amplitude
srate <- 1000 # sampling rate
# Matlab -> time = 0:1/srate:2-1/srate;
time <- seq(0, 2, 1/srate)
n <- length(time)

## generate a signal
signal <- a1 * sin(2 * pi * f1 * time) + a2 * sin(2 * pi * f2 * time)

### Here's the plot of our signal
plot(time, signal, type = "l", ylab = "Amplitude", lwd = 2, 
     main = "Time Domain", xlab = "Time (s)")
grid()
abline(h = 0, lty =3); abline(v = 0, lty = 3)


### compute fourier transform with a for loop ###
# matlab: ftime = (0:pnts-1)/pnts
ftime <- seq(0, (n-1)/n, length.out = n)
fcoef <- rep(0, length(signal))
i <- 0+1i # define i, imaginary number
### start for loop
for (fi in 1:n) {
  ## complex sine wave
  csw <- exp(-i * 2 * pi * (fi - 1) * ftime) # complex sin wave
  # dot product between sine wave and signal
  fcoef[fi] <- sum(signal * csw) / n
} ## end fourier for loop

# compute amplitudes
ampl <- 2 * abs(fcoef)
# Matlab -> hz = linspace(0, srate, n)
# compute frequencies
hz <- seq(0, srate/2, length.out = n/2)

# plot the fourier results... did we recovery f and a ?
## trying ggplot instead:
df <- data.frame(amplitude = ampl[1:length(hz)], hertz = hz)

plot(hz, ampl[1:length(hz)], type = "b", 
     ylab = "Amplitude", xlim = c(0, 10), main = "Wrong Way to Visualize")
grid()

## compare results to R function fft
p <- 2 * abs(fft(signal) / n)
points(hz, p[1:length(hz)], col = "red", cex = 0.5)


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
