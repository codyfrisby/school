# Car MPG
ccf  
January 17, 2016  

This is an R markdown file for the car mpg data shown in class.  We will reporduce some of the SAS output using R and Rstudio.

First we need to read the data into R.


```r
carmpg <- read.table("~/Documents/MATH3710/cars/MPGData.txt", header = TRUE)
summary(carmpg)
```

```
##      Weight          MPG       
##  Min.   :1613   Min.   :13.00  
##  1st Qu.:2130   1st Qu.:21.00  
##  Median :2489   Median :26.00  
##  Mean   :2535   Mean   :26.66  
##  3rd Qu.:2930   3rd Qu.:31.50  
##  Max.   :3465   Max.   :46.60
```

```r
cor(carmpg)
```

```
##            Weight        MPG
## Weight  1.0000000 -0.7105168
## MPG    -0.7105168  1.0000000
```


Above are simple summary statistics of the data.

Now let's look at a scatter plot of the data.


```r
plot(MPG ~ Weight, data=carmpg, bg = "lightblue", 
     col = "black", cex = 1.5, pch = 21)
text(x=1800, y=15, paste0("Cor = ",round(cor(carmpg)[2], 4), cex = 0.7))
```

![](carmpg_files/figure-html/unnamed-chunk-2-1.png)\


Now we fit a linear model in R using the lm function. We will store it in a variable for more use.


```r
fit <- lm(MPG ~ Weight, data = carmpg)
summary(fit) # displays a summary of the model.
```

```
## 
## Call:
## lm(formula = MPG ~ Weight, data = carmpg)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.7011  -3.3404  -0.5987   2.3588  16.0605 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 51.5871689  1.4835394   34.77   <2e-16 ***
## Weight      -0.0098334  0.0005749  -17.11   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.723 on 287 degrees of freedom
## Multiple R-squared:  0.5048,	Adjusted R-squared:  0.5031 
## F-statistic: 292.6 on 1 and 287 DF,  p-value: < 2.2e-16
```


Now we will use R to check the assumptions of the model. First we will store the standardized residuals from "fit" and store them in a variable.


```r
e <- rstandard(fit)
summary(e)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -2.6970 -0.7087 -0.1273  0.0001  0.5022  3.4120
```

More diagnostic plots of our model:

```r
qqnorm(e)
qqline(e)
```

![](carmpg_files/figure-html/unnamed-chunk-5-1.png)\

```r
# Residuals vs. x
x <- carmpg$Weight
plot(x, e, bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
```

![](carmpg_files/figure-html/unnamed-chunk-5-2.png)\

One more plot.  Here we will look at a histogram of the residuals.


```r
hist(e, breaks = 25, col = "green", freq = FALSE, xlim = c(-4,4), 
     main = "Histogram of Standardized Residuals", xlab = "rstandard")
```

![](carmpg_files/figure-html/unnamed-chunk-6-1.png)\

Now we want to test our model.  We will take a sample of size 30 and call it "test".  The remaining 259 obs will be used to build our new model.


```r
carmpg$index <- 1:289
# first lets create an simple random sample of numbers.  We will use this to sample
# and subset the original car mpg data set.
ind <- sample(1:289, 30)
# now we can subset the carmpg data frame into 2 data frames using this index.
test <- carmpg[carmpg$index %in% ind, ]
#test <- read.csv("~/Documents/MATH3710/cars/test.csv")
training <- carmpg[!(carmpg$index %in% ind), ]
#training <- read.csv("~/Documents/MATH3710/cars/training.csv")
# now to fit a new model with our subsetted training data.
fit2 <- lm(MPG ~ Weight, data = training)
testing <- predict(fit2, newdata = test, interval = "confidence", se.fit = TRUE)
anova(fit2)
```

```
## Analysis of Variance Table
## 
## Response: MPG
##            Df Sum Sq Mean Sq F value    Pr(>F)    
## Weight      1 5902.7  5902.7  256.77 < 2.2e-16 ***
## Residuals 257 5907.9    23.0                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# SAS proc score is different from R predict() but essetially predict() is testing
# your training model with your test data.
# here is the predicted bias:
predicted_bias <- mean(test$MPG - testing$fit[,1])
mean_square_bias <- mean((test$MPG - testing$fit[,1])^2)
rpmse <- sqrt(mean_square_bias)
output <- as.data.frame(cbind(predicted_bias, mean_square_bias, rpmse))
output
```

```
##   predicted_bias mean_square_bias    rpmse
## 1    -0.05365392         16.45882 4.056948
```

Let's take this further by bootstrapping our predicted bias...This will be a challenge programatically but if we take it one step at a time we should be able to do it. Let's generate 100 random predicted bias estimates.


```r
bias <- vector()
for (i in 1:100) {
  ind <- sample(1:289, 30)
  # now we can subset the carmpg data frame into 2 data frames using this index.
  test <- carmpg[carmpg$index %in% ind,]
  training <- carmpg[!(carmpg$index %in% ind),]
  # now to fit a new model with our subsetted training data.
  fit2 <- lm(MPG ~ Weight, data = training)
  testing <- predict.lm(fit2, newdata = test)
  # here is the predicted bias:
  predicted_bias <- mean(test$MPG - testing)
  bias <- rbind(bias, predicted_bias)
}
mean(bias)
```

```
## [1] 0.08181333
```

```r
hist(bias, col = "green")
```

![](carmpg_files/figure-html/unnamed-chunk-8-1.png)\
