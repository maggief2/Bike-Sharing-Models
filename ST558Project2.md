ST558 Project 2
================
Maggie Feng and Vito Leonardo
July 16, 2021

# Introduction

In this report, we will be analyze the day.csv bike sharing data set. We
will start by summarizing the data to get an idea of the data set. Then
we will try to predict the number of users using predictive models. The
data set contains the count of bike share users per day over 731 days
along with the weather and climate data associated with each day. The
response variable that we are trying to predict is the `cnt` variable
which a count of all biking sharing users (registered plus casual) in
the given day. Our goal is to predict `cnt` based on what season it is,
`season`; whether it is a workday or not, `workday`; the weather,
`weathersit`; how the temperature feels that day, `atemp`; the wind
speed that day, `windspeed`; and the humidity, `hum`. We will predict
the count of bike shares with 4 models: 2 different linear regression
models, 1 random forest model, and 1 boosted tree model. After we fit
the 4 models, we compare the models on a test set and declare the
winner. The winner will be the model that predicts `cnt` the best; this
will be the model with the smallest Test MSE.

# Data

``` r
day <- which.max(params$day==Weekdays)

#Reading in the day dataset and filtered for Monday
Day <- read_csv("day.csv") %>% 
  mutate(weekday = weekday + 1) %>% 
  filter(weekday == day) %>%
  select(-instant,-dteday, -yr, -casual, -registered) %>%
  mutate(season = factor(season, c(1:4), 
                           c("Winter", "Spring", "Summer", "Fall")),
         workingday = factor(workingday, c(0, 1), c("No", "Yes")),
         weathersit = factor(weathersit, c(1, 2, 3, 4), 
                               c("Level 1", "Level 2", "Level 3", "Level 4")))

#Randomly sample from data
set.seed(123)
TrainIndex <- createDataPartition(Day$cnt, p = .7, list = FALSE)
Day.Train <- Day[TrainIndex,]
Day.Test <- Day[-TrainIndex,]
```

# Summarizations

## Quantitative Variables

Now that we read the data in, we can start summarizing and visualizing.
We will start by looking at the summary statistics of the quantitative
variables:

-   `atemp` the normalized feeling temperature in Celsius
-   `hum` the normalized humidity
-   `windspeed` the normalized wind speed

``` r
Day.Train %>% 
  select(cnt, atemp, hum, windspeed) %>%
  apply(MARGIN= 2,FUN = summary) %>%
  round(3) %>%
  kable(caption = paste0("Summary of Quantitative variables (",Weekdays[day],")"),
        row.names = TRUE) 
```

|         |      cnt | atemp |   hum | windspeed |
|:--------|---------:|------:|------:|----------:|
| Min.    |   22.000 | 0.177 | 0.302 |     0.042 |
| 1st Qu. | 3340.750 | 0.357 | 0.519 |     0.137 |
| Median  | 4350.000 | 0.483 | 0.653 |     0.180 |
| Mean    | 4394.105 | 0.471 | 0.635 |     0.193 |
| 3rd Qu. | 5890.250 | 0.596 | 0.736 |     0.235 |
| Max.    | 7525.000 | 0.721 | 0.925 |     0.418 |

Summary of Quantitative variables (Monday)

Now that we have some summary statistics for the data, lets build some
plots. For linear regression with a continuous response, `cnt`, the
model requires the assumption that the response variable follows a
normal distribution. If our sample size is large enough, the Central
Limit Theorem will allow us to fit even if we stray slighly from
normality. We can visualize the distribution of the response with a
histogram and a density contour overlayed on top. If `cnt` follows a
normal distribution, the histogram and density contour should be bell
shaped. If it is not bell shaped, then a linear regression might not be
the best model for prediction.

``` r
ggplot(data = Day.Train, mapping = aes(x=cnt)) +
  geom_histogram(aes(y = ..density..), bins = 20) + 
  geom_density(kernel = "gaussian", size = 2, color = 'red') +
  labs(title=paste0("Histogram of the Number Bike Sharing on ",Weekdays[day]), 
       x = "Number of total bike sharing on a given day")
```

![](ST558Project2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Now that we have observed the shape of the sample distribution of `cnt`,
we can now explore the relationships between `cnt` and the quantitative
predictors. We will do so by creating scatterplots with `cnt` and the
three quantitative predictors of interest. The plots below will contain
the scatter plot, the linear model line, and the correlation between
`cnt` and the predictor. Predictors with a high correlation will likely
be good predictors for our models.

``` r
Cor1 <- round(cor(Day.Train$cnt,Day.Train$atemp),3)
grob1 = grobTree(textGrob(paste0("Pearson Correlation = ", Cor1), 
                          x = 0.5, y = 0.10, hjust = 0, 
                          gp = gpar(col = "blue", fontsize = 11, fontface = "bold")))

ggplot(data = Day.Train, mapping = aes(x=cnt, y=atemp))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = paste0("Scatterplot for Count vs Temperature (",Weekdays[day],")"),
       x = "Count of Total Bikes Shared", 
       y = "Temperature") +
  annotation_custom(grob1)
```

![](ST558Project2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
Cor2 <- round(cor(Day.Train$cnt,Day.Train$hum),3)
grob2 = grobTree(textGrob(paste0("Pearson Correlation = ", Cor2), 
                          x = 0.5, y = 0.10, hjust = 0, 
                          gp = gpar(col = "blue", fontsize = 11, fontface = "bold")))
ggplot(data = Day.Train, mapping = aes(x=cnt, y=hum))+
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = paste0("Scatterplot for Count vs Humidity (",Weekdays[day],")"),
       x = "Count of Total Bikes Shared", 
       y = "Humidity") +
  annotation_custom(grob2)
```

![](ST558Project2_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
Cor3 <- round(cor(Day.Train$cnt,Day.Train$windspeed),3)
grob3 = grobTree(textGrob(paste0("Pearson Correlation = ", Cor3), 
                          x = 0.5, y = 0.10, hjust = 0, 
                          gp = gpar(col = "blue", fontsize = 11, fontface = "bold")))
ggplot(data = Day.Train, mapping = aes(x=cnt, y=windspeed))+
  geom_point()+
  geom_smooth(method=lm) +
  labs(title = paste0("Scatterplot for Count vs Wind Speed (",Weekdays[day],")"),
       x = "Count of Total Bikes Shared", 
       y = "Wind Speed") +
  annotation_custom(grob3)
```

![](ST558Project2_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

## Qualitative Variables

Our other variables of interest are qualitative:

-   `season` indicates the season:
    -   1 = Winter
    -   2 = Spring
    -   3 = Summer
    -   4 = Fall
-   `workingday` is an indicator for if the day is a weekend/holiday
    -   0 = A weekend or holiday
    -   1 = Not a weekend nor holiday
-   `weathersit` indicates the weather category
    -   1 = Clear, Few clouds, Partly cloudy
    -   2 = Mist & Cloudy, Mist & Broken clouds, Mist & Few clouds, Mist
    -   3 = Light Snow, Light Rain & Thunderstorm & Scattered clouds,
        Light Rain & Scattered clouds
    -   4 = Heavy Rain & Ice Pallets & Thunderstorm & Mist, Snow & Fog

We will create contingency tables to see how many observations fall
under each category since present observations or the lack of may
indicate which categories have more bike sharing uses.

``` r
#Contingency tables for each categorical variable
kable(table(Day.Train$season), caption = "Frequency by Season")
```

| Var1   | Freq |
|:-------|-----:|
| Winter |   18 |
| Spring |   17 |
| Summer |   19 |
| Fall   |   22 |

Frequency by Season

``` r
kable(table(Day.Train$workingday), caption = "Frequency by Workingday")
```

| Var1 | Freq |
|:-----|-----:|
| No   |   11 |
| Yes  |   65 |

Frequency by Workingday

``` r
kable(table(Day.Train$weathersit), caption = "Frequency by Weather")
```

| Var1    | Freq |
|:--------|-----:|
| Level 1 |   47 |
| Level 2 |   27 |
| Level 3 |    2 |
| Level 4 |    0 |

Frequency by Weather

For the qualitative data we can look at boxplots to explore each level
of the variables in relation to `cnt`. In each plot we will group by the
different levels and plot the five number summary of the `cnt` variable.
Using the plots below we will be able to see if there is a distinct
difference between the levels of the categorical factors, which would
help determine if the variable would be a good predictor for our models.

``` r
#Season
ggplot(Day.Train, aes(season, cnt)) + 
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  labs(title = paste0("Boxplot for Count by Season (",Weekdays[day],")"),
       x = "Season",
       y = "Total Bike Shares")
```

![](ST558Project2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#Workday
ggplot(Day.Train, aes(workingday, cnt)) + 
  geom_boxplot() +
  geom_jitter(aes(color = workingday)) +
  labs(title = paste0("Boxplot for Count by Workday (",Weekdays[day],")"),
       x = "Workingday",
       y = "Total Bike Shares")
```

![](ST558Project2_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
#Weathersit
ggplot(Day.Train, aes(weathersit, cnt)) + 
  geom_boxplot() +
  geom_jitter(aes(color = weathersit)) +
  labs(title = paste0("Boxplot for Count by Weather (",Weekdays[day],")"),
       x = "Weather",
       y = "Total Bike Shares")
```

![](ST558Project2_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

# Modeling

## Linear regression

A linear regression model fits explanatory variable(s) to a response
variable. The coefficients for the variables are given based on the line
that would provide the smallest number when calculating the squared
difference between the observed values to the expected values along the
line. If there is a singular explanatory variable to predict the
explanatory variable, it would be considered a simple linear regression.
If there are more than one variable, interaction terms, and/or
polynomial terms then it is considered multiple linear regression.

``` r
lm1.fit <- lm(cnt ~ atemp*season, data = Day.Train)
```

``` r
Day.Train <- Day.Train %>% select(-workingday)

lm2.fit <- lm(cnt~ ., data = Day.Train)
lm2.fit <- step(lm2.fit, trace = FALSE, direction = "both")
```

## Random Forest

``` r
p <- ncol(Day.Train)-1
rf.fit <- train(cnt ~ ., data = Day.Train, 
                method = "rf", 
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 5),
                tuneGrid = data.frame(mtry = c(1:p,sqrt(p))))
```

## Boosted Tree

``` r
B <- c(500,1000,1500,2500,5000)
lambda <- c(0.01, 0.001)
d <- 1:p
min <- 1
tunegrid <- expand.grid(B,lambda,d, min)
names(tunegrid) <- c("n.trees","shrinkage","interaction.depth","n.minobsinnode")
boosted.fit <- train(cnt ~. , data = Day.Train,
                     method = "gbm",
                     trControl = trainControl(method = "cv",
                                         number = 5),
                     tuneGrid = tunegrid)
```

# Comparison

``` r
lm1.pred <- predict(lm1.fit, newdata=Day.Test)
lm1.RMSE <- postResample(lm1.pred,Day.Test$cnt)[[1]]
```

``` r
lm2.pred <- predict(lm2.fit, newdata=Day.Test)
lm2.RMSE <- postResample(lm2.pred,Day.Test$cnt)[[1]]
```

``` r
rf.pred <- predict(rf.fit, newdata = Day.Test)
rf.RMSE <- postResample(rf.pred,Day.Test$cnt)[[1]]
```

``` r
boosted.pred <- predict(boosted.fit, newdata = Day.Test)
boosted.RMSE <- postResample(boosted.pred,Day.Test$cnt)[[1]]
```

``` r
RMSE <- data.frame( Model = c("LM1", "LM2", "RF", "Boosted"),
                    RMSE = c(lm1.RMSE, lm2.RMSE, rf.RMSE, boosted.RMSE))
kable(RMSE)
```

| Model   |     RMSE |
|:--------|---------:|
| LM1     | 1131.086 |
| LM2     | 1047.267 |
| RF      | 1295.378 |
| Boosted | 1272.129 |

The model with the lowest RMSE is Linear Model 2 with an RMSE of
1047.2671557. Linear Model 2 is the **winner** for Monday’s data!

# Automation
