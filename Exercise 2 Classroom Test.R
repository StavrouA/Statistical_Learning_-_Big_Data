library('ISLR')
data("College")
summary("College")

sum(is.na(College))
indices = 1:nrow(College)
trainIndex = sample(x = indices,size = floor(0.7*nrow(College)),replace = F)

training = College[trainIndex,]
testing = College[-trainIndex,]

myfit=lm(training$Apps~.,training)
test_mse = mean((testing$Apps - predict(myfit,testing))^2)

library('glmnet')
x1=model.matrix(Apps~.,training)[,-2]
x2=model.matrix(Apps~.,testing)[,-2]

#ridge
fit_ridge_cv = cv.glmnet(x1, training$Apps, alpha = 0, standardize = TRUE, intercept = TRUE, standardize.response = FALSE)
plot(fit_ridge_cv)
coef(fit_ridge_cv, s = "lambda.min")

ridgepred=predict(fit_ridge_cv, x2, s = "lambda.min")
ridge_mse = mean((testing$Apps - ridgepred) ^ 2)

#lasso
fit_lasso_cv = cv.glmnet(x1, training$Apps, alpha = 1,standardize = TRUE, intercept = TRUE, standardize.response = FALSE)
plot(fit_lasso_cv)
coef(fit_lasso_cv, s = "lambda.min")

lassopred=predict(fit_lasso_cv, x2, s = "lambda.min")
lasso_mse = mean((testing$Apps - lassopred) ^ 2)

#non zero betas
newfit=coef(fit_lasso_cv, s = "lambda.min")
length(newfit@x)

#we keep 16 out of 18 coefficients with the LASSO, where the mse is smaller than the one obtained by ridge regression.