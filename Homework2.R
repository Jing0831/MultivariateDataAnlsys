# HOMEWORK 2
# BAN250- JINGYI WANG


# 1.(a)
Sigma = matrix(c(1,-2,0,-2,5,0,0,0,2),nrow = 3,byrow = T)
Sigma
## Since the cov(X1,X2) = (-2,-2)', we can get the conclusion that:
## X1 and X2 are not independent.

# 1.(b)
## Since the cov(X2,X3) = (0,0)', we can get the conclusion that:
## X2 and X3 are independent.

# 1.(c)
## In order to test the independence of (X1+X2) and X3, let's set a matrix A as followed:
A = matrix(c(1,1,0,0,0,1), nrow = 2, byrow = T)
t(A)
## Set the covariance matrix of (X1+X2) and X3 as COV_c:
COV_c = A %*% Sigma %*% t(A)
COV_c
## Since the COV((X1+X2),X3) = (0,0)', they are independent.

# 1.(d)
## Since COV((X1+X2)/2,X3)= 1/2 COV((X1+X2),X3) = 0, We can get the conclusion that:
##       The variable (X1+X2)/2 and X3 are also independent.

## We can prove this conclusion by conducting the same calculation as followed:
## In order to test the independence of (X1+X2)/2 and X3, let's set a matrix B as followed:
B = matrix(c(1/2,1/2,0,0,0,1),nrow = 2,byrow = T)
## Set the covariance matrix of (X1+X2)/2 and X3 as COV_d:
COV_d = B %*% Sigma %*% t(B)
COV_d
## Since the COV((X1+X2)/2,X3) = (0,0)', we proved that they are independent.

# 1.(e)
## In order to test the independence of X2 and (X2-5/2X1-X3), let's set a matrix C as followed:
C = matrix(c(0,1,0,-5/2,1,-1),nrow = 2, byrow = T)
COV_e = C %*% Sigma %*% t(C)
COV_e
## Since the COV(X2,(X2-5/2X1-X3)) = (10,10)', they are not independent.


# 2.(a)
## Set a 2x2 matrix for A and B:
A = matrix(c(2,1,3,3), nrow = 2, byrow = T)
B = matrix(c(0,4,1,2), nrow = 2, byrow = T)
Com_AOBO = matrix(c(2,1,0,0,3,3,0,0,0,0,0,4,0,0,1,2), nrow = 4, byrow = T)
A
B
Com_AOBO
as.integer(det(Com_AOBO))==as.integer(det(A)*det(B)) 

# 2.(b)
## Set a 2X2 matrix for C:
C = matrix(c(1,6,4,3), nrow = 2, byrow = T)
Com_ACOB = matrix(c(2,1,1,6,3,3,4,3,0,0,0,4,0,0,1,2), nrow = 4, byrow = T)
A
B
C
Com_ACOB
as.integer(det(Com_ACOB))==as.integer(det(A)*det(B)) 


# 3.
## import the Nuclear data set
nuclear_raw <- read.csv("C:/Users/jwang/OneDrive/Desktop/R_BAN250/Homework/HW2/nuclear.csv")
## select columns 2 to 6, and set a new dataframe named df_nclear
df_nuclear = nuclear_raw[,c(2:6)]
## summary() can provide the mean for each column
summary(df_nuclear)
## We can use histogram to visualize the distribution of each variable:
hist(df_nuclear$cost)
hist(df_nuclear$date)
hist(df_nuclear$t1)
hist(df_nuclear$t2)
hist(df_nuclear$cap)
## From the histogram and the difference between the mean and median of the data in each column
## I think we can tentatively identify the variable t2 is normally distributed, 
## the variable t1 also looks like normally distributed, but the rest are not.

## Conduct qqplot for each of these variables:
## For the first column, variable: cost, Not Normally Distributed.
## The dots curve and deviate from the straight diagonal at both ends.
qqnorm(df_nuclear$cost)
qqline(df_nuclear$cost)
## For the second column, variable: date, Not Normally Distributed.
## The dots deviate far away from the straight line as the value become bigger, which also
## confirms the right-skewed distribution that we saw from the histogram.
qqnorm(df_nuclear$date)
qqline(df_nuclear$date)
## For the third column, variable: t1, Not Normally Distributed.
## We can see clearly that the dots deviate far away from the straight line as the value become bigger.
qqnorm(df_nuclear$t1)
qqline(df_nuclear$t1)
## For the forth column, variable: t2, Normally Distributed.
## The dots overlap closely with the diagonal straight line.
qqnorm(df_nuclear$t2)
qqline(df_nuclear$t2)
## For the last column, variable: cap, Not Normally Distributed.
## The dots deviate from the diagonal straight line at both sides.
qqnorm(df_nuclear$cap)
qqline(df_nuclear$cap)

## MVN Test
library(MVN)
## -- Mardia Test & Anderson-Darling Test
mvn(df_nuclear,mvnTest="mardia",multivariatePlot = "qq")
## -- Henze-Zirkler Test & Anderson-Darling Test
mvn(df_nuclear,mvnTest="hz",multivariatePlot = "qq")
## -- Royston Test & Anderson-Darling Test
mvn(df_nuclear,mvnTest="royston",multivariatePlot = "qq")

## Conclusion:
### For MVN Test:
### -- Ho: Data follows a multivariate normal distribution
### -- Ha: Data is not MVN
### -- The P-value for all three MVN tests is > 0.05, we do no reject Ho, which means we have evidence to support the null hypothesis.
### -- In other words, the distribution of data are not significantly different from multivariate normal distribution.
### For Univariate Test (Anderson-Darling Test):
### -- Ho: Data is normal distributed
### -- Ha: Data is not normal distributed
### -- For variables: cost, t1, and t2, P-value > 0.05,we do not reject Ho, which means
### -- We have evidence to support that the three variables: cost, t1, and t2 are normally distributed. 
### -- For variables: date, cap, P-value < 0.05, we reject Ho, which means
### -- there is less than 5% probability that the null hypothesis is true, therefore, we believe they are not normal distributed.


# 4.(a)
## Import the data set
propval <- read.table("C:/Users/jwang/OneDrive/Desktop/R_BAN250/Homework/HW2/propval.txt", header=TRUE, quote="\"")
## Test the normality for each variable by running Anderson-Darling Test:
mvn(propval)$univariateNormality
### Conclusion:
### -- Ho: Data is normal distributed
### -- Ha: Data is not normal distributed
### -- For variables: y(Sale price of the house),x3(Lot size),x4(Living space),and x8(Age of the home),
### -- P-value > 0.05,we do not reject Ho, which means we have evidence to support the Ho, they are normally distributed.
### -- For the rest of variables (x1,x2,x5,x6,x7, and x9), P-value < 0.05, we reject Ho, which means
### -- there is less than 5% probability that the Ho is true, therefore, we believe they are not normal distributed.

## Test the MVN 
## -- Mardia Test 
mardia = mvn(propval,mvnTest="mardia")$multivariateNormality
## -- Dornik-Haansen
dh = mvn(propval,mvnTest="dh")$multivariateNormality
## -- Royston Test 
royston = mvn(propval,mvnTest="royston")$multivariateNormality
mvn(propval,multivariatePlot = "qq")
### Conclusion: 
### -- Ho: Data is MVN
### -- Ha: Data is not MVN
### -- Two of the four tests considered the data to be consistent with the MVN distribution.
### -- Therefore I think we have sufficient evidence to support Ho, the distribution of data is not significantly different from MVN.

# 4.(b)
library(car)
library(lmtest)
library(nortest)
## Run a regression model with all the variables
md1 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, data = propval) 
summary(md1)
## The utility of this model is good, since the P-value of F-test is less than 0.05,we do not reject the Ho that our model is utility;
## However, only one variable is significant in our model:
## -- Ho: Beta_n is 0; Ha : Beta_n is not 0
## -- Only variable X1 has P-value < 0.05, we reject Ho; 
## --      in other words, we have evidence to support the coefficient of X1 is not 0;
## -- Other variables are all non significant, which means those variables are not significant in the model and therefore should not be included.
## The model has Adjusted R-squared: 78.54%, in other words, 78.54% of the variance in Y is explained by this regression model. 

vif(md1)
# 4.(c) Let's check 4 assumptions!
## No.1 Mean of the residuals is 0.
mean(md1$residuals)
### Conclusion: mean of the residuals is almost 0, therefore, md1 meets this assumption

## No.2 Residuals are normally distributed.
shapiro.test(md1$residuals)
ad.test(md1$residuals)
### Conclusion: md1 meets this assumption
### -- Ho: Residuals are normally distributed.
### -- Ha: Residuals are not normally distributed.
### -- P-value > 0.05, we have evidence to support Ho, DO NOT REJECT Ho.

## No.3 Residuals have constant variance.
bptest(md1)
### Conclusion: md1 meets this assumption
### -- Ho: Residuals have constant variance.
### -- Ha: Residuals does not have constant variance.
### -- P-value > 0.05, we have evidence to support Ho, DO NOT REJECT Ho.

## No.4 Residuals are independent of each other.
durbinWatsonTest(md1)
### Conclusion: md1 meets this assumption
### -- Ho: Residuals are independent.
### -- Ha: Residuals are not independent
### -- P-value > 0.05, we have evidence to support Ho, DO NOT REJECT Ho.
#### Surprisingly, all 4 assumptions are validated.

# 4.(d) 
## We can test for multicollinearity:
vif(md1)
### The numbers for variable X1, X6, and X7 are all bigger than 6, which indicates we have multicollinearity problem.


# 5.
library(MVN)
## Import the data
elementart <- read.csv("C:/Users/jwang/OneDrive/Desktop/R_BAN250/Homework/HW2/elementart(1).csv")
typeof(elementart)
elementart = as.data.frame(elementart,row.names = NULL)
is.data.frame(elementart)
## Select the following variables and make a new data frame:
## -- the number of English language learners: $ell ~ 8(As Dependent Variable)
## -- the percentage of free meals: $meals ~ 7
## -- year Round school: $yr_rnd ~ 9
## -- mobility: $mobility ~ 10
## -- average class size in k-3: $acs_k3 ~ 11
## -- average class size in 4-6: $acs_46 ~ 12
## -- pct of full credential: $full ~ 19
## -- pct of emer credentials: $emer ~ 20
## -- the number of students enrolled: $enroll ~ 21
rawdata = elementart[, c(7,9:12,19:21,8)]

## Review our new data set
## -- set variable yr_rnd as dummy variable (as.factor)
rawdata$yr_rnd = as.factor(rawdata$yr_rnd)
summary(rawdata)
## -- check missing data and outliers
clean_data = rawdata[complete.cases(rawdata),]
levels(clean_data$yr_rnd) # double-check the dummy variable
summary(clean_data)
## -- check the normality of numerical variables: Unfortunately, they are all not norally distributed
num_data = clean_data[,c(1,3:9)]
mvn(num_data)$univariateNormality

## Run a regression model with all the variables
model1 = lm(ell ~ meals + yr_rnd + mobility + acs_k3 + acs_46 + full + emer + enroll, data = clean_data) 
summary(model1)
### Passed F-test, with Adjusted R-squared: 69.29%; however, there are 3 non significant coefficients.

## Remove non significant coefficients (terms) but keep "acs_46":
## -- It's interesting that the coefficient of acs_46 is non significant, while the coefficient of acs_k3 is significant;
## -- However, since they are all stands for "class size", it doesn't make sense if we only remove one of them. 
model2 = lm(ell ~ meals + yr_rnd + mobility + acs_k3 + acs_46 + enroll, data = clean_data) 
summary(model2)
### The utility of model2 is good, since the P-value of F-test is less than 0.05,we do not reject the Ho that our model is utility;
### The Adjusted R-squared of model2 is 69.44%, which is improved relative to model1.

## Let's test Mulicollinearity:
vif(model2) 
### which is good, all smaller than 6, we don't have mulicollinearity problem.

## Let's test 4 regression assumptions:
## - No.1 Mean of the residuals is 0.
mean(model2$residuals)
### Conclusion: mean of the residuals is almost 0, therefore, model2 meets this assumption

## - No.2 Residuals are normally distributed.
shapiro.test(model2$residuals)
ad.test(model2$residuals)
### Conclusion: the result of shapiro test support that model2 meets this assumption.
### -- Ho: Residuals are normally distributed.
### -- Ha: Residuals are not normally distributed.
### -- P-value > 0.05, we have evidence to support Ho, DO NOT REJECT Ho.

## - No.3 Residuals have constant variance.
bptest(model2)
### Conclusion: model2 against this assumption
### -- Ho: Residuals have constant variance.
### -- Ha: Residuals does not have constant variance.
### -- P-value < 0.05, we do not have evidence to support Ho, we REJECT Ho.

## - No.4 Residuals are independent of each other.
durbinWatsonTest(model2)
### Conclusion: model2 against this assumption
### -- Ho: Residuals are independent.
### -- Ha: Residuals are not independent
### -- P-value < 0.05, we do not have evidence to support Ho, we REJECT Ho.

#### For model2: Only 2 assumptions out of 4 are validated.

## plot (meals ~ yr_rnd)  and (meals ~ acs_k3)
plot(clean_data$meals ~ clean_data$yr_rnd)
plot(clean_data$meals ~ clean_data$acs_k3)
plot(clean_data$ell ~ clean_data$yr_rnd)
plot(clean_data$ell ~ clean_data$acs_k3)
plot(clean_data$ell ~ clean_data$acs_46)
plot(clean_data$ell ~ clean_data$meals)
plot(clean_data$ell ~ clean_data$mobility)
plot(clean_data$ell ~ clean_data$enroll)

## Remove variable 'acs_k3','acs_46' from model2:
model3 = lm(ell ~ meals + yr_rnd + mobility + enroll, data = clean_data) 
summary(model3) # Pass F-test, Adjusted R-squared: 0.6912
## repeat the tests for 4 assumptions:
mean(model3$residuals)# PASS, Mean of residuals is 0
shapiro.test(model3$residuals)# Failed for normal distribution
ad.test(model3$residuals)# Failed for normal distribution
bptest(model3)# Failed for constant variance
durbinWatsonTest(model3)# Failed for constant variance
### Model 3 failed at normality test, as well as the bptest and DurbinWatson Test.
###         Only one assumption, mean of the residuals is 0, is validated.

model4 = lm(ell ~ meals + mobility + enroll, data = clean_data) 
summary(model4) # Pass F-test, with Adjusted R-squared: 0.688
mean(model4$residuals) # PASS, Mean of residuals is 0
shapiro.test(model4$residuals) # Failed for normal distribution
ad.test(model4$residuals) # Failed for normal distribution
bptest(model4) # Failed for constant variance
durbinWatsonTest(model4) # Failed for independency
### No better than Model 3, failed 3 assumptions as model 3 did.


## Based on the plot of ell and meals, I try to squared-root the variable meals based on model3:
model5 = lm(ell ~ I(meals^1/2) + yr_rnd + mobility + enroll, data = clean_data)
summary(model5) # Adjusted R-squared: 0.6912
mean(model5$residuals) # PASS, Mean of residuals is 0
shapiro.test(model5$residuals)# Failed for normal distribution
ad.test(model5$residuals) # Failed for normal distribution
bptest(model5) # Failed for constant variance
durbinWatsonTest(model5) # Failed for independency

#### Final Conclusion:
#### - We should notice at the beginning that our dependent variables are not normally distributed.
#### - Together with our relatively small sample size, regression model on this basis cannot be expected to yield a high degree of accuracy.
#### - I will go with Model3 (ell ~ meals + yr_rnd + mobility + enroll)
#### - Since Model3 explains 69.12% of the variance with 4 variables compared to 69.44% with 6 variables in Model2.
#### - The rest models are no more good than model3.
#### - Although all models have pass the F-test, they all not good at tests for regression assumptions:
#### - One of the possible reasons is that we may miss some important factors, such as age.
#### - Recall that the coefficient of acs_k3 is significant, while the coefficient of acs_46 is non significant;
#### - These two variables actually split our sample into two subsamples based on grade level, and there is evidence
#### - that this two subsamples are different, at least in our model.
#### - Another possible reason is that there are other correlations between the variables, only that we have not found them yet.


