# CS-Assignment-1

 
#The first independent variable we create is x1. It follows a uniform distribution between 0 and 1.
 
x_1 <- runif(1000, min = 0, max = 1)
 
#we create the second independent variable, x2. It follows a normal distribution with mean of 0 and variance of 1
 
x_2 <- rnorm(n = 1000, mean = 0, sd = 1)
 
#we create the third independent variable, x3. It is a binary variable that is either 0 or 1.
 
x_3 <- sample(c(0,1), 1000, replace = TRUE)
 
#Now we can create the dependent variable, y, based on the values created and the formula, y = 1000*x1 + 100*x2 + 10*x3. Noise will be 10*rnorm().
 
y <- 1000 * x_1 + 100 * x_2 + 10 * x_3 + 10 * rnorm(200)
 
#creating a dataframe to combine the independent variables and the dependent variable
df <- data.frame(x_1, x_2, x_3, y)
df
 

STEP 2
Tell a 2-3 sentence story about the data generating process you coded up above. What is it about and what each component means?
We first created three independent variables - x_1, x_2,and x_3 using the conditions described in the comments. Then, we came up with a formula  for the y variable. Finally, created a data frame called df using all variables - y, x_1, x_2, and x_3.

STEP 3
Using an incorrect model of the systematic component (of your choice), and using the simulation-based approach covered in class (the arm library, etc.), generate 1 prediction for every observation and calculate and report RMSE. Make sure you write out your model and report your RMSE.
Each prediction should be conditional on the predictor values of the corresponding observation. E.g., when predicting for observation #1, use the predictor values of observation #1.
# YOUR CODE HERE
set.seed(123)
 
x_1 <- runif(1000, min = 0, max = 1)
x_2 <- rnorm(n = 1000, mean = 0, sd = 1) 
x_3 <- sample(c(0,1), 1000, replace = TRUE)
y <- 1000 * x_1 + 100 * x_2 + 10 * x_3 + 10 * rnorm(200)
df <- data.frame(x_1, x_2, x_3, y) 
 
lm1 <- lm(y ~ x_1^10, data = df)
lm1
 
library(Matching)
library(arm)
 
pred <- rep(NA, nrow(df)) 
sim_result1 <- sim(lm1, n.sims = 1)
 
for (i in 1:nrow(df)) {
  x <- c(1, df$x_1[i])
  pred[i] <- sum(x * coef(sim_result1))
}
 
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
print(rmse(df$y, pred))
 

STEP 4
Using the correct model (correct systematic and stochastic components), and using the simulation-based approach covered in class (the arm library, etc.), generate 1 prediction for every observation and calculate & report your RMSE. Once again, write out your model and report your RMSE.
Each prediction should be conditional on the predictor values of the corresponding observation. E.g., when predicting for observation #1, use the predictor values of observation #1.
# YOUR CODE HERE
set.seed(123)
 
x_1 <- runif(1000, min = 0, max = 1)
x_2 <- rnorm(n = 1000, mean = 0, sd = 1)
x_3 <- sample(c(0,1), 1000, replace = TRUE)
y <- 1000*x_1 + 100*x_2 + 10*x_3 + 10*rnorm(200)
df <- data.frame(x_1, x_2, x_3, y)
 
lm2 <- lm(y ~ ., data = df)  #creating a correct model 
lm2
 
library(Matching)
library(arm)
 
pred <- rep(NA, nrow(df)) 
sim_result2 <- sim(lm2, n.sims = 1)
 
for (i in 1:nrow(df)) {
  x <- c(1, df$x_1[i], df$x_2[i], df$x_3[i])
  pred[i] <- sum(x * coef(sim_result2))
}
 
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
print(rmse(df$y, pred))
```

STEP 5
Which RMSE is larger: The one from the correct model or the one from the incorrect model? Why?
The one from the incorrect model is larger, because the incorrect model has a higher deviation from the data than the correct one. The RMSE function in the code cells above illustrates how higher deviation results in a higher RMSE. Therefore, it is logically sound that the correct model should have a lower RMSE. This is the case in our example.


QUESTION 2 
Imagine that you want to create a data viz that illustrates the sensitivity of regression to outlier data points. So, you want to create two figures:
One figure that shows a regression line fit to a 2-dimensional (x and y) scatterplot, such that the regression line clearly has a positive slope.
# YOUR CODE HERE
 
x <- rnorm(1000)
y = 31* x + 60 +  rnorm(1000)
datafr_1000 <- data.frame(x, y)
lm1 <- lm(y ~ x, data = datafr_1000)
summary(lm1)
 
# Generating 
datafr01 <- rbind(datafr_1000)
lm01<- lm(y ~ x, data = datafr01)
summary(lm01)
 
# Generating plot
plot(datafr01,
     main = "Upward sloping regression line",
     xlab = "Independent variable (x)",
     ylab = "Dependent variable (y)")
abline(a = coef(lm1)[1],
       b = coef(lm1)[2],
       col = "green")
 

And, another figure that shows a regression line with a negative slope fit to a scatter plot of the same data plus one additional outlier data point. This one data point is what changes the sign of the regression line’s slope from positive to negative.

# YOUR CODE HERE
x <- rnorm(1000)
y = 31 * x + 60 +  rnorm(1000)
datafr1 <- data.frame(x, y)
lm1<- lm(y ~ x, data = datafr1)
summary(lm1)
 
# Generating 
outlier <- c(30, -2000) 
datafr2 <- rbind(datafr1, outlier) 
lm2<- lm(y ~ x, data = datafr2)
summary(lm2)
 
# Generating plot
plot(datafr2,
     main = "Downward sloping regression line",
     xlab = "Independent variable (x)",
     ylab = "Dependent variable (y)")
abline(a = coef(lm2)[1],
       b = coef(lm2)[2],
       col = "red")
 
Be sure to label the axes and the title the figures appropriately. Include a brief paragraph that explains the number of observations and how you created the data set and the outlier.


This data visualization includes 1000 points created by the following data-generating equation y = 31 * x + 60 + rnorm(1000). The outlier was created by introducing a value on the further right of the x axis and a negative y value, (30, -2000). This changed the sign of the slope of the regression line from positive to negative.

QUESTION 3
STEP 1
Using the laLonde data set, run a linear regression that models re78 as a function of age, education, re74, re75, hisp, and black. Note that the lalonde data set comes with the package Matching.
# YOUR CODE HERE
install.packages("arm")
#load arm and Matching library 
library(arm)
library(Matching)
data("lalonde") 
#load lalonde dataset
head(lalonde)
lm_lalonde <- lm(re78 ~  age + educ + re74 +  re75 + hisp + black, data = lalonde) 
#regression with the lalonde dataset
summary(lm_lalonde)
#regression summary 

STEP 2
Report coefficients and R-squared.
Then calculate R-squared by hand and confirm / report that you get the same or nearly the same answer as the summary (lm) command. 
Write out hand calculations here.

# YOUR CODE HERE
set.seed(4325)
rows <- sample(nrow(lalonde)) #shuffles row indices

lalonde<-lalonde[rows, ] #randomly orders data

model <-lm(re78~ age + educ + re74 +  re75 + hisp + black, lalonde) #fits lm model on train
p<-predict(model, lalonde) #predicts on test

error<-p-test[["re78"]] #computes errors
sqrt(mean(error^2)) #calculates RMSE by hand

summary(model)
re78_mean<-mean(lalonde$re78)

tss<- sum((lalonde$re78-re78_mean)^2)
rss<-sum((p-lalonde$re78)^2)
rsq<-1-(rss/tss)
rsq


STEP 3 
Then, setting all the predictors at their means EXCEPT education, create a data visualization that shows the 95% confidence interval of the expected values of re78 as education varies from 3 to 16. Be sure to include axes labels and figure titles.
# YOUR CODE HERE
 
library(arm)
library(Matching)
data("lalonde")
 
 
lm_lalonde <- lm(re78 ~ age + educ + re74 +  re75 + hisp + black, data = lalonde)
 
mean_re74 <- mean(lalonde$re74)
mean_re75 <- mean(lalonde$re75)
mean_hisp <- mean(lalonde$hisp)
mean_black <- mean(lalonde$black)
mean_age <- mean(lalonde$age)
 
expected.val <- matrix(NA, nrow = 0, ncol = length(c(3:16)))
repeat {
  sim.lm1 <- sim(object = lm_lalonde, n.sims = 1000)
  storage.matrix <- matrix(NA, nrow = 100, ncol = length(c(3:16)))
  
  for (educ in c(3:16)){
    Xs <- c(1, mean_age, educ, mean_re74, mean_re75, mean_hisp, mean_black)
    for (i in 1:100) {
      storage.matrix[i, educ - 2] <- sum(Xs * sim.lm1@coef[i,])
    }
  }
  expected.val <- rbind(expected.val, colMeans(storage.matrix))
  
  if (nrow(expected.val) == 100) {
    break
  }
}
 
quan<-apply(expected.val, 2, quantile, probs = c(0.025, 0.975))
quan
 
confidence<-apply(expected.val, 2, quantile, probs=c(0.025, 0.975))
plot(x=c(1:100),y=c(1:100),type = "n", xlim = c(0,20), ylim = c(0,10000), 
     main = "Treatment by years of education", xlab = "Years of education", 
     ylab = "Treatment")
for (educ in 3:16){
    segments(
    x0=educ,
    y0=confidence[1,educ-2],
    x1=educ,
    y1=confidence[2,educ-2],
    lwd=2)
}

STEP 4
Then, do the same thing, but this time for the predicted values of re78. Be sure to include axes labels and figure titles.
# YOUR CODE HERE
 
library(arm)
library(Matching)
data("lalonde")
 
 
lm_lalonde <- lm(re78 ~ age + educ + re74 +  re75 + hisp + black, data = lalonde)
 
mean_re74 <- mean(lalonde$re74)
mean_re75 <- mean(lalonde$re75)
mean_hisp <- mean(lalonde$hisp)
mean_black <- mean(lalonde$black)
mean_age <- mean(lalonde$age)
 
expected.val <- matrix(NA, nrow = 0, ncol = length(c(3:16)))
repeat {
  sim.lm1 <- sim(object = lm_lalonde, n.sims = 1000)
  storage.matrix <- matrix(NA, nrow = 100, ncol = length(c(3:16)))
  
  for (educ in c(3:16)){
    Xs <- c(1, mean_age, educ, mean_re74, mean_re75, mean_hisp, mean_black)
    for (i in 1:100) {
      storage.matrix[i, educ - 2] <- sum(Xs * sim.lm1@coef[i,])
    }
  }
  expected.val <- rbind(expected.val, colMeans(storage.matrix))
  
  if (nrow(expected.val) == 100) {
    break
  }
}
quan<-apply(expected.val, 2, quantile, probs = c(0.025, 0.975))
quan
 
confidence<-apply(expected.val, 2, quantile, probs=c(0.025, 0.975))
plot(x=c(1:100),y=c(1:100),type = "n", xlim = c(0,20), ylim = c(0,10000), 
     main = "Treatment by years of education", xlab = "Years of education", 
     ylab = "Treatment")
for (educ in 3:16){
    segments(
    x0=educ,
    y0=confidence[1,educ-2],
    x1=educ,
    y1=confidence[2,educ-2],
    lwd=2)
}

STEP 5
Lastly, write a short paragraph with your reflections on this exercise (specifically, the length of intervals for given expected vs. predicted values) and the results you obtained.
	The length of intervals for the expected values is shorter because they only account for the estimation uncertainty. On the other hand, the predicted value interval lengths are longer because they account for both estimation and fundamental uncertainty. As a result, we get a higher diversity of treatment values for each independent variable,  from 3 to 16, on the predictive model than the expectation.

QUESTION 4
STEP 1
Using the lalonde data set, run a logistic regression, modeling treatment status as a function of age, education, hisp, re74 and re75. Report and interpret the regression coefficient and 95% confidence intervals for age and education.
# YOUR CODE HERE
 
 
log_reg_lalonde <- glm(re78 ~ age + educ + hisp+ re74 +  re75, data = lalonde)
summary(log_reg_lalonde) 
 
#using confint function to obtain 95% confidence interval
confint(log_reg_lalonde, level = 0.95)
 

Report and interpret regression coefficient and 95% confidence intervals for age and education here.
age - 1.165e-02
education - 6.922e-02
re75 - 5.221e-05
re74 - -2.278e-05
hisp - -6.024e-01
 
Considering the numeric predictor variables (age, education, re74, and re75), a regression coefficient will show us the effect of a one-unit change in the predictor variable. That means that, all else being equal, age would positively affect treatment. Likewise, education and re75 positively affect the outcome. However, re74 has a negative sign. Hisp also has a negative sign, and it’s the only categorical (values either 0 or 1) rather than numerical independent variable that is taken into account. All else being equal, being Hispanic would negatively affect the treatment. 
 

STEP 2
Use a simple bootstrap to estimate (and report) bootstrapped confidence intervals for age and education given the logistic regression above. Code the bootstrap algorithm yourself.
# YOUR CODE HERE
install.packages("boot")
library(boot)
 
logit.t <- function(formula, data, indices) {
  d <- data[indices,] 
  fit <- glm(formula, data=d, family = binomial)
  return(coef(fit))
}
results <- boot(data = lalonde, formula = treat ~ age + educ + hisp + re74 + re75, statistic = logit.t, R=1000)
age_coef <- boot.ci(results, index = 2, type="logit_test")
 
boot.ci(results, conf = 0.95, type = "all") 
 
results 
plot(results)

Report bootstrapped confidence intervals for age and education here.

STEP 3
Then, using the simulation-based approach and the arm library, set all the predictors at their means EXCEPT education, create a data visualization that shows the 95% confidence interval of the expected values of the probability of receiving treatment as education varies from 3 to 16. Be sure to include axes labels and figure titles.
# YOUR CODE HERE
exp <- matrix(NA, nrow = 0, ncol = length(c(3:16))) #expected values matrix lenght considering educ values
repeat {
  sim <- sim(log_reg_lalonde, n.sims = 1000)
  storage1 <- matrix(NA, nrow = 1000, ncol = length(c(3:16)))
  for (educ in c(3:16)){
    Xs <- c(1, mean(lalonde$age), educ, mean(lalonde$hisp), mean(lalonde$re74), mean(lalonde$re75))
    for (i in 1:1000) {
      storage1[i, educ - 2] <- sum(Xs * sim@coef[i,])
    }
  }
  exp <- rbind(exp, colMeans(storage1))
  if (nrow(exp) == 1000) {
    break
  }
}
confint1 <- apply(exp, 2, quantile, probs = c(0.025, 0.975)) #95% 
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(3,16), ylim = c(-0.9,0.2), main = "Expected probability given education and variables at their means",
     xlab = "Education in years", ylab = "Probability of getting treatment in %")
for (educ in c(3:16)) {
  segments(x0 = educ, y0 = confint1[1, educ - 2], x1 = educ, y1 = confint1[2, educ - 2],
    col = "green", lwd = 2)
  points(educ, mean(exp[, educ - 2]), col = "blue", pch = 19)
}

STEP 4
Then, do the same thing, but this time for the predicted values of the probability of receiving treatment as education varies from 3 to 16. Be sure to include axes labels and figure titles.
# YOUR CODE HERE
data(lalonde)
log_reg_lalonde <- glm(treat ~ age + educ + hisp + re74 + re75, family = "binomial", data = lalonde)
sim_reg <- sim(log_reg_lalonde, n.sims = 1000) #creating an S4 object 
 
storage.matrix4 <- matrix(NA, nrow = 1000, ncol = length(c(3:16))) 
 
for (educ in c(3:16)){
  Xs <- c(1, mean(lalonde$age), educ, mean(lalonde$hisp), mean(lalonde$re74), mean(lalonde$re75))
  
  for (i in 1:1000){
    storage.matrix4[i, educ - 2] <- sum(Xs * sim_reg@coef[i,])
  }
}
 
confint.glm1.2 <- apply(storage.matrix4, 2, quantile, probs = c(0.025, 0.975))
 
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(3,16), ylim = c(-3, 3),
     main = "Predicted Probability of Receiving Treatment Given Education and 
     Other Variables Held at Their Mean",
     xlab = "Education (Years)",
     ylab = "Probability of Receiving Treatment (%)")
 
for (educ in c(3:16)) {
  
  segments(
    x0 = educ,
    y0 = confint.glm1.2[1, educ - 2],
    x1 = educ,
    y1 = confint.glm1.2[2, educ - 2],
    col = rgb(0,0,1,.5),
    lwd = 2)
  points(educ, mean(storage.matrix4[, educ - 2]), col = "hotpink", pch = 19)
  
}
 

STEP 5
Lastly, write a short paragraph with your reflections on this exercise and the results you obtained.

First, we used logistic regression to model treatment given the independent variables of age, education, hisp, re74, and re75. Then we designed a bootstrap to show the confidence intervals of two of these independent variables - age and education. In step 3, We used arm library and simulation-based approach to show probabilities of receiving treatment depending on the education variable in the range 3:16 in 95% confidence interval. Similarly, we showed the 95% confidence interval for the probability of receiving treatment based on education, using a predictive model in step 4.

QUESTION 5
Write the executive summary for a decision brief about the impact of a stress therapy program, targeted at individuals age 18-42, intended to reduce average monthly stress. The program was tested via RCT, and the results are summarized by the figure that you get if you run this code chunk:
# Note that if you knit this document, this part of the code won't 
# show up in the final pdf which is OK. We don't need to see the code
# we wrote.

# How effective is a therapy method against stress

# Participants in the study record their stress level for a month.
# Every day, participants assign a value from 1 to 10 for their stress level. 
# At the end of the month, we average the results for each participant.

#adds the confidence interval (first row of the matrix is lower 
# bound, second row is the upper bound)
trt1 = matrix(NA,nrow=2,ncol=7)
ctrl = matrix(NA,nrow=2,ncol=7) 

trt1[,1]=c(3.7, 6.5) #18  
ctrl[,1]=c(5, 8)

trt1[,2]=c(5, 8.5) #22
ctrl[,2]=c(7.5, 9)

trt1[,3]=c(6, 9) #26
ctrl[,3]=c(8.5, 10)

trt1[,4]=c(5, 7) #30
ctrl[,4]=c(6, 8)

trt1[,5]=c(3.5, 5) #34
ctrl[,5]=c(4.5, 7)

trt1[,6]=c(2, 3.5) #38
ctrl[,6]=c(3.5, 6)

trt1[,7]=c(0.5, 2) #42
ctrl[,7]=c(2.5, 5)

# colors to each group
c1 = rgb(red = 0.3, green = 0, blue = 1, alpha = 0.7) #trt1
c2 = rgb(red = 1, green = 0.6, blue = 0, alpha = 1) #trt2
c3 = rgb(red = 0, green = 0.5, blue = 0, alpha = 0.7) #ctrl

# creates the background of the graph
plot(x = c(1:100), y = c(1:100), 
     type = "n", 
     xlim = c(17,43), 
     ylim = c(0,11), 
     cex.lab=1,
     main = "Stress Level - 95% Prediction Intervals", 
     xlab = "Age", 
     ylab = "Average Stress Level per Month", 
     xaxt = "n")

axis(1, at=seq(18,42,by=4), seq(18, 42, by=4))

grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted",
     lwd=par("lwd"), equilogs = TRUE)

# adds the legend
legend('topright',legend=c('Treatment','Control'),fill=c(c1,c2))

# iterates to add stuff to plot
for (age in seq(from=18,to=42,by=4)) { 
  #treatment
  segments(x0=age-0.2, y0=trt1[1, (age-18)/4+1],
           x1=age-0.2, y1=trt1[2, (age-18)/4+1], lwd=4, col=c1)
  
  #control
  segments(x0=age+0.2, y0=ctrl[1, (age-18)/4+1],
           x1=age+0.2, y1=ctrl[2, (age-18)/4+1], lwd=4, col=c2)
}

(Not that it matters, really, but you can imagine that these results were obtained via simulation, just like the results you have hopefully obtained for question 2 above).
Your executive summary should be between about 4 and 10 sentences long, it should briefly describe the purpose of the study, the methodology, and the policy implications/prescription. (Feel free to imaginatively but realistically embellish/fill-in-the-blanks with respect to any of the above, since I am not giving you backstory here).
Write your executive summary here.
	From the plot, it is clear that the average monthly stress level of individuals who were in the control group (orange) is higher than those in the treatment group(purple). the length of the vertical lines represent the range in which the individual datapoint falls 95% of the time of the simulation. The result suggests that the treatment is successful in reducing individual stress levels across ages. Moreover, an argument can be made that especially in the age range of 22-26 and 34-42, the treatment has proven exceptionally successful, while the results in the remaining age groups, while still exist, are less significant. 

QUESTION 6
Can we predict what projects end up being successful on Kickstarter?
We have data from the Kickstarter company.
From Wikipedia: Kickstarter is an American public-benefit corporation based in Brooklyn, New York, that maintains a global crowdfunding platform focused on creativity and merchandising. The company’s stated mission is to “help bring creative projects to life”. As of May 2019, Kickstarter has received more than $4 billion in pledges from 16.3 million backers to fund 445,000 projects, such as films, music, stage shows, comics, journalism, video games, technology, publishing, and food-related projects.
The data is collected by Mickaël Mouillé and is last updated in 2018. Columns are self explanatory. Note that usd_pledged is the column pledged in US dollars (conversion done by kickstarter) and usd_pledge_real is the pledged column in real US dollars of the pledged column. Finally, usd_goal_real is the column goal in real US dollars. You should use the real columns.
So what makes a project successful? Undoubtedly, there are many factors, but perhaps we could set up a prediction problem here, similar to the one from the bonus part of the last assignment where we used GDP to predict personnel contributions.
We have columns representing the number of backers, project length, the main category, and the real project goal in USD for each project.
Let’s explore the relationship between those predictors and the dependent variable of interest — the success of a project.
Instead of running a simple linear regression and calling it a day, let’s use cross-validation to make our prediction a little more sophisticated.
Our general plan is the following:
Build the model on a training data set
Apply the model on a new test data set to make predictions based on the inferred model parameters.
Compute and track the prediction errors to check performance using the mean squared difference between the observed and the predicted outcome values in the test set.
Let’s get to it, step, by step. Make sure you have loaded the necessary packages for this project.
STEP 1: Import & Clean the Data
Import the dataset from this link: https://tinyurl.com/KaggleDataCS112
Remove any rows that include missing values.
# YOUR CODE HERE
library(utils)
data <- read.csv("https://tinyurl.com/KaggleDataCS112")
data_omitted <- na.omit(data) #removing rows that contain missing values
head(data_omitted)

 
STEP 2: Codify outcome variable
Create a new variable that is either successful or NOT successful and call it success and save it in your dataframe. It should take values of 1 (successful) or 0 (unsuccessful).
# YOUR CODE HERE
data1 <- transform(data2, success = ifelse(usd_goal_real <= usd_pledged_real,
1, 0))
data1

STEP 3: Getting the project length variable
Projects on Kickstarter can last anywhere from 1 - 60 days. Kickstarter claims that projects lasting any longer are rarely successful and campaigns with shorter durations have higher success rates, and create a helpful sense of urgency around your project. Using the package lubridate or any other package in R you come across by Googling, create a new column that shows the length of the project by taking the difference between the variable deadline and the variable launched. Call the new column length and save it in your dataframe.
Remove any project length that is higher than 60.
# YOUR CODE HERE
data1$deadline <- ymd(data1$deadline, tz = "UTC")
data1$launched <- ymd_hms(data1$launched, tz = "UTC")
data1$length <- time_length(interval(data1$launched, data1$deadline), "day")
data1 <- data1[data1$length <= 60,]
data1

STEP 4: Splitting the data into a training and a testing set
While there are several variations of the k-fold cross-validation method, let’s stick with the simplest one where we just split randomly the dataset into two (k = 2) and split our available data from the link above into a training and a testing (aka validation) set.
Randomly select 80% of the data to be put in a training set and leave the rest for a test set.
# YOUR CODE HERE
samplesize<- floor(0.8 * nrow(data1))
 
train1 <- sample(seq_len(nrow(data1)), size = samplesize)
 
train <- data1[train1, ]
test <- data1[-train1, ]
 
STEP 5: Fitting a model
Use a logistic regression to find what factors determine the chances a project is successful. Use the variable indicating whether a project is successful or not as the dependent variables (Y) and number of backers, project length, main category of the project, and the real project goal as independent variables. Make sure to use the main category as a factor.
# YOUR CODE HERE
training_regression<- lm(success ~ backers + length + usd_goal_real, data=train)
summary(training_regression)

 
 
STEP 6: Predictions
Use the model you’ve inferred from the previous step to predict the success outcomes in the test set.
# YOUR CODE HERE
pred <- predict(training_regression, test)
pred_missclass <-ifelse(pred>=0.5,1,0)
 
STEP 7: How well did it do?
Report the Root Mean Squared Error (RMSE) of the predictions for the training and the test sets.
# YOUR CODE HERE
 
pred_test <- ifelse(pred_missclass == test$success, 1, 0) 
#when test success is predicted, prediction_test equals 1
 
true_class_test <- sum(pred_test == 1) 
 
false_class_test <- sum(pred_test == 0)
 
 
missclass_rate_test <- false_class_test/(true_class_test+false_class_test)
#misclass rate of the test
 
print(missclass_rate_test)
 

Step 8: LOOCV method
Apply the leave-one-out cross validation (LOOCV) method to the training set. What is the RMSE of the training and test sets. How similar are the RMSEs?
# YOUR CODE HERE
library(dplyr)
 
n <- 1000
loocv_s<- sample_n(train, n, replace= TRUE) #sampling from train data
 
error <- rep(0, dim(loocv_s)[1]) #we repeat depending on dimensions of loocv_s
for (i in 1:dim(loocv_s)[1]) {
    reg_loocv <- glm(success ~ backers + length + factor(main_category) + usd_goal_real, data =  loocv_s)
    pred.up <- predict.glm(reg_loocv, loocv_s[i, ], type = "response") > 0.5
    true.up <- loocv_s[i, ]$success == 1
    if (pred.up != true.up)
        error[i] <- 1
}
 
mean(error)
