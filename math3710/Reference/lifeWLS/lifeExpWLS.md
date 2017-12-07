# WLS Life Expectancy
Cody Frisby  
March 21, 2016  

### Life Expectancy Data Weighted Least Squares Example  


```r
# read in the data from the excel file
life <- xlsx::read.xlsx("~/Documents/MATH3710/ProblemSets/problem6/LifeExpectancy.xlsx", sheetIndex = 1)
life$africa <- 0
life$oecd <- 0
life[life$Group == "africa", "africa"] <- 1
life[life$Group == "oecd", "oecd"] <- 1
# here we will add the weights using R.
life$w <- 0 #create a new column
# create logical index based on our 3 groups
a <- life$Group == "africa"
b <- life$Group == "oecd"
c <- life$Group == "other"
# now fill our new column with the calculated weight
life[life$Group == "africa", "w"] <- 1/var(life$LifeExp[a])
life[life$Group == "oecd", "w"] <- 1/var(life$LifeExp[b])
life[life$Group == "other", "w"] <- 1/var(life$LifeExp[c])
# and we now have a weights column in our data frame
head(life)
```

```
##           Country  Group   PPGDP LifeExp africa oecd          w
## 1 Afghanistan      other   499.0   49.49      0    0 0.03104533
## 2     Albania      other  3677.2   80.40      0    0 0.03104533
## 3     Algeria     africa  4473.0   75.00      1    0 0.01324345
## 4      Angola     africa  4321.9   53.17      1    0 0.01324345
## 5    Anguilla      other 13750.1   81.10      0    0 0.03104533
## 6   Argentina      other  9162.1   79.89      0    0 0.03104533
```

```r
# succint way to calculate each value in R
for (g in c("africa", "oecd", "other")) 
  print(1/var(life[life$Group == g,4]))
```

```
## [1] 0.01324345
## [1] 0.2278432
## [1] 0.03104533
```


