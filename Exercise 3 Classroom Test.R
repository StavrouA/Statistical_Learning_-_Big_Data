library('ISLR')
library(tree)

data("OJ")
summary(OJ)
str(OJ)

#DATA INTERPRETATION
#The data contains 1070 purchases where the customer either purchased Citrus Hill or Minute Maid
#Orange Juice. A number of characteristics of the customer and product are recorded.

#Question a
set.seed(2)
indices = 1:nrow(OJ)
trainIndex = sample(x = indices, size = 800 ,replace = F)
training = OJ[trainIndex,]
testing = OJ[-trainIndex,]

dim(training)
dim(testing)

#Question b
my_tree = tree(Purchase ~ ., data = training)
summary(my_tree)     #Just 4 out of 18 variables used to predict the outcome, with missclassification error 16.75%. The tree has 8 terminal nodes.

#Question c
my_tree

#If we choose the first terminal node from the left, we interpret it as following:
#If a new individual comes that has "LoyalCH" (Customer brand loyalty for CH) < 0.04917 (and LoyalCH < 0.27 and <0.5 (which is true if <0.04))
#we predict that he will purchase Minute Maid (MM)

#3rd terminal node: if a new customer comes with "LoyalCH" higher than 0.276142 and the Price Difference (PriceDiff) is lower than 0.05$,
#we predict that he will purchase MM


#Question d
plot(my_tree)
text(my_tree, pretty = 0)
title(main = "Unpruned Classification Tree")

#This tree shows that in the first node (root) we divide our data into 2 groups, LoyalCH lower and higher than 0.5. The left path is
#always the argument that agrees with the condition, e.g. on the first left path we get the observations having LoyalCH<0.05 and so on.

#Question e
train_pred = predict(my_tree, training, type = "class")
test_pred = predict(my_tree, testing, type = "class")

#train confusion
table(predicted = train_pred, actual = training$Purchase)
#test confusion
table(predicted = test_pred, actual = testing$Purchase)

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

#train acc
tra = accuracy(predicted = train_pred, actual = training$Purchase)

#test acc
tea = accuracy(predicted = test_pred, actual = testing$Purchase)

#test error rate
test_error_rate=1-tea
test_error_rate

#Question f
set.seed(30)
my_tree_cv = cv.tree(my_tree, FUN = prune.misclass)

min_idex = which.min(my_tree_cv$dev)
min_idex

# number of terminal nodes in that tree
my_tree_cv$size[min_idex]

#Question g
plot(my_tree_cv$size,my_tree_cv$dev)
lines(my_tree_cv$size, my_tree_cv$dev, type = "l")

#Question h
#The tree with size=8, as shown in Question f

#QUestion i
my_tree_prune = prune.misclass(my_tree, best = 8)
summary(my_tree_prune)

plot(my_tree_prune)
text(my_tree_prune, pretty = 0)
title(main = "Pruned Classification Tree")

#Question j
#pruned train
my_prune_train_pred = predict(my_tree_prune, training, type = "class")
table(predicted = my_prune_train_pred, actual = training$Purchase)
summary(training$Purchases)
accuracy(predicted = my_prune_train_pred, actual = training$Purchase)
#accuracy(predicted = train_pred, actual = training$Purchase)
#The two errors are the same, since the pruned tree is the unpruned tree (in general, we expect the pruned tree to have lower error rates
#since the pruned error we got here is the tree with the lowest error, it just happens to be the one we got with our first tree)

#Question k
#pruned test
my_prune_test_pred = predict(my_tree_prune, testing, type = "class")
table(predicted = my_prune_test_pred, actual = testing$Purchase)
accuracy(predicted = my_prune_test_pred, actual = testing$Purchase)
#accuracy(predicted = test_pred, actual = testing$Purchase)
#The two errors are the same, like in Question j (because unpruned = pruned!)