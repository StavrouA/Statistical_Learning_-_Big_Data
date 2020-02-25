library('ISLR')
set.seed(1)
k=10
n=100

eps = rnorm(n, mean = 0, sd = 1)
x = rnorm(n, mean = 0, sd = 1)
y = x - 2*x^2 + eps
df = data.frame(y,x)

#Question 1
#plotting y, we see that y takes values from ~(-8,2), with a longer tail
#on the negative side, since we have x-2*x^2. We can't possibly know which polynomial is best fitting with this plot
plot(y)


#Question 2
fit_1 = lm(y ~ x)                   #coefficient \beta_1 is not statistical significant
fit_2 = lm(y ~ poly(x, degree = 2)) #all coefficients are statistical significant
fit_3 = lm(y ~ poly(x, degree = 3)) #coefficient \beta_3 is not statistical significant
fit_4 = lm(y ~ poly(x, degree = 4)) #coefficients \beta_3 and \beta_4 are not statistical significant

coef(summary(fit_1))
coef(summary(fit_2))
coef(summary(fit_3))
coef(summary(fit_4))

#Question 3

my_cv = function(df, y, x, k, power) {
  cv_error = matrix(NA, k)
  folds = sample(c(1:k), size = nrow(df), replace=TRUE)
  for(i in 1:k){
    training = df[(folds != i),]
    testing = df[!(folds != i),]
    
    myfit = lm(y~poly(x, power), training)
    pred = predict(myfit, testing)
    cv_error[i] = mean((testing$y - pred)^2)
  }
  mean_cv_mse = mean(cv_error)
  mean_cv_mse
}

cv_mat=rep(NA,10)

for(power in 1:length(cv_mat)){
  cv_mat[power]=my_cv(df,y,x,k,power)
}

cv_mat
min = which.min(cv_mat)
min

plot(cv_mat)
points(min, cv_mat[min], col = "red", cex = 2, pch = 20)


#Question 4 + Question 5
#The best model is the one with 5 variables. The train MSE increases from then on, due to overfitting (bias-variance trade-off). It is expected to have this 
#U shape test MSE as we increase the complexity. As we increase the complexity the bias is getting smaller, while the variance is getting bigger. After a point
#the variance is getting very big, because we tend to overfit our model (like R^2 is being increased in regression). The more regressors we have,
#the best fit we have on the train set, but in the test set we have huge variance (overfit)