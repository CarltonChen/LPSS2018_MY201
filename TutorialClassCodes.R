rm(list = ls())

#############################################################################
#                                                                           #
#                             Introduction to R                             #
#                                                                           #
#############################################################################
str1 = "Hello World"
str1

x = c(1, 3, 2, 5)
y = c(2, 3, 5, 3)
length(x)
length(y)
x + y

ls()
rm(x, y)
ls()

cat("\014")  # clear console or press "Ctrl + L"

?"matrix"

x = matrix (data = c(1, 2, 3, 4) ,
            nrow = 2,
            ncol = 2)
x
x = matrix (c(1, 2, 3, 4), 2, 2)

x = matrix (
  data = c(1, 2, 3, 4) ,
  nrow = 2,
  ncol = 2,
  byrow = TRUE
)
x

x = x ^ 2
x = sqrt(x)
x

A = array(1:13, dim = c(4, 4))
B = array(2:18, dim = c(4, 4))
A + B
A %*% B

A = array(c(1, 3, 2, 1), c(2, 2))
A
b = array(c(1, 0), c(2, 1))
solve(A, b)
solve(A)


A = outer(0:2, 0:2) # every possible value of AD and BC
freq = table(outer(A, A, "-")) # frequency for all values of AD-BC
freq
plot(freq / sum(freq), xlab = "Determinant value", ylab = "Probability")


A = matrix (data = 1:16,
            nrow = 4,
            ncol = 4)
eigen(A)
svd(A)
A
A[2, 3]
A[c(1, 3), c(2, 4)]
A[1:2 , ]
A[-c(1, 3), -c(1, 3, 4)]
dim(A)

weight = c(84.5, 72.6, 75.7, 94.8, 71.3)
height = c(86.5, 71.8, 77.2, 84.9, 75.4)
sheep = data.frame(weight, height)
mean(sheep$height)
fix(sheep)
sheep$backlength = c(130.4, 100.2, 109.4, 140.6, 101.4)
fix(sheep)


fileName = "Auto.data"
defaultDataDir = "C:/Users/qianc/Desktop/PUK-LSE summer School/2017 summer school documents/2017 LSE-PKU-Big Data/Data/Dataset"
fileLocation = file.path(defaultDataDir, fileName)
Auto = read.table(file = fileLocation)
fix(Auto)

Auto = read.table(file = fileLocation,
                  header = T,
                  na.strings = "?")
fix(Auto)

fileName = "Auto.csv"
fileLocation = file.path(defaultDataDir, fileName)
Auto = read.csv(file = fileLocation,
                header = T,
                na.strings = "?")
fix(Auto)
dim(Auto)

Auto = na.omit(Auto)
dim(Auto)
names(Auto)

summary(Auto)
summary(Auto$mpg)

listEx = list("Test", c(2, 0, 6), sheep)
length(listEx)
listEx[2]
listEx[[2]] + c(1, 2, 4)
names(listEx) = c("String", "Vector", "DataFrame")
df = as.data.frame(listEx[["DataFrame"]])
fix(df)


plot(cylinders, mpg)
plot(
  Auto$cylinders,
  Auto$mpg,
  type = "p",
  pch = "x",
  col = "blue",
  xlab = "cylinders",
  ylab = "mpg"
)
attach(Auto)
plot(cylinders, mpg)
plot(
  cylinders,
  mpg,
  col = "red",
  varwidth = T,
  xlab = "cylinders",
  ylab = "mpg"
)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
cylinders = as.factor(cylinders)
boxplot(acceleration, cylinders)
boxplot(mpg, xlab = "MPG", ylab = "Value")
hist(mpg, col = 2, breaks = seq(from = 5, to = 50, by = 5))

pairs(Auto)
pairs(
  formula = ~ mpg + displacement + horsepower + weight + acceleration,
  data = Auto
)

qqnorm(mpg, ylab = "mpg Quantiles")
qqline(mpg, col = 2, lwd = 2)

par(mfrow = c(1, 2))
x = seq(from = -2, to = 2, length = 50)
y = seq(from = -1, to = 1, length = 50)
z = outer(x ^ 2, y ^ 3)
contour(x, y, z)
persp(x, y, z, theta = -50, phi = 20)


x = seq(-5, 10, by = 1.5)

y = rep("NA", times = length(x))
for (i in 1:length(x))
{
  if (x[i] >= 0)
    y[i] = "non-negative"
  else
    y[i] = "negative"
}
x = seq(-5, 10, by = 1.5)
y = ifelse(x > 0, "non-negative", "negative")

x = 1

while (x < 6) {
  print(x)
  x = x + 1
}

fn = function(arg1) {
  if (arg1 <= 0) {
    100
  }
  else{
    2 + fn(arg1 - 1)
  }
}
m = fn(-5)
m

binomplot = function(size,
                     prob = 0.5,
                     colour = 3,
                     outputvals = FALSE)
{
  x = 0:size
  y = dbinom(x, size, prob)
  plot(x, y, type = "h", col = colour)
  if (outputvals)
    y
}
binomplot(50)

pbinom(q = 60, size = 85, prob = 0.6)
pnorm(
  q = 15,
  mean = 12,
  sd = 3,
  lower.tail = FALSE
) # 1 - pnorm(q = 15, mean = 12, sd = 3)
pbinom(q = 59, size = 85, prob = 0.6) * pnorm(
  q = 15,
  mean = 12,
  sd = 3,
  lower.tail = FALSE
)



x = seq(from = -3, to = 3, length = 200)
plot(x, dnorm(x), type = "l")
lines(x, dt(x, df = 16), col = 2)
lines(x, dt(x, df = 8), col = 3)
lines(x, dt(x, df = 4), col = 4)

x <- seq(0, 10, length = 200)
plot(x, dgamma(x, shape = 1, scale = 1), type = "l")
lines(x, dgamma(x, shape = 1, scale = 2), col = 2)
lines(x, dgamma(x, shape = 1, scale = 4), col = 3)
plot(x, dgamma(x, shape = 2, scale = 1), type = "l")
lines(x, dgamma(x, shape = 2, scale = 2), col = 2)
lines(x, dgamma(x, shape = 2, scale = 4), col = 3)


hist(Auto$mpg,
     breaks = seq(0, 60, length = 20),
     probability = TRUE)
lines(x, dnorm(x, mean = mean(Auto$mpg), sd = sd(Auto$mpg)), col = 2)


poissamp = rpois(n = 400, lambda = 2)
hist(poissamp, breaks = 0:10, probability = TRUE)
normsamp = rnorm(n = 250, mean = 10, sd = 5)
hist(normsamp,
     breaks = seq(-10, 30, length = 15),
     probability = TRUE)
x = seq(-10, 30, length = 200)
lines(x, dnorm(x, mean = 10, sd = 5), col = 2)

set.seed(1)
rnorm(5)
set.seed(1)
rnorm(5)

nvec = 1:6
sample(nvec)
sample(nvec, size = 3)
sample(nvec, replace = TRUE)
sample(nvec, size = 10, replace = TRUE)


poisSampMean = function(n, lambda, r) {
  simvals = rpois(n * r, lambda)
  simvals = matrix(simvals, n, r)
  colMeans(simvals)
}
set.seed(1)
poisSampMean(10, 3, 6)


histNorm = function(data, nbins = 21) {
  hist(
    data,
    breaks = seq(min(data), max(data), length = nbins),
    probability = TRUE,
    col = 5
  )
  x = seq(min(data), max(data), length = 200)
  lines(x, dnorm(x, mean = mean(data), sd = sd(data)), col = 2)
}
histNorm(poisSampMean(8, 1, 1000))

#############################################################################
#                                                                           #
#                             Datasets                                      #
#                                                                           #
#############################################################################

fileName = "Advertising.csv"
defaultDataDir = "/Users/Cheng/Desktop/PUK-LSE summer School/R Code/Introduction to Statistical Learning/Dataset"
# defaultDataDir = "C:/Users/LiCheng/Desktop/PKU - LSE Summer School/R Code/Introduction to Statistical Learning/Dataset"
fileLocation = file.path(defaultDataDir, fileName)
Advertising = read.csv(file = fileLocation, header = TRUE)
fix(Advertising)

fileName = "Auto.csv"
defaultDataDir = "/Users/Cheng/Desktop/PUK-LSE summer School/R Code/Introduction to Statistical Learning/Dataset"
# defaultDataDir = "C:/Users/LiCheng/Desktop/PKU - LSE Summer School/R Code/Introduction to Statistical Learning/Dataset"
fileLocation = file.path(defaultDataDir, fileName)
Auto = read.csv(
  file = fileLocation,
  header = TRUE,
  colClasses = c(horsepower = "numeric"),
  na.strings = "?"
)
Auto = na.omit(Auto)
fix(Auto)


library(ISLR)
Carseats = Carseats
fix(Carseats)

fileName = "Hitters.csv"
defaultDataDir = "/Users/cheng/Desktop"
fileLocation = file.path(defaultDataDir, fileName)
write.csv(x = Hitters,
          file = fileLocation,
          row.names = T)

library(ISLR)
fix(Default)

library(ISLR)
fix(Caravan)

library(ISLR)
fix(Hitters)

fileName = "spam.dat"
defaultDataDir = "/Users/Cheng/Desktop/PUK-LSE summer School/R Code/Introduction to Statistical Learning/Dataset"
# defaultDataDir = "C:/Users/LiCheng/Desktop/PKU - LSE Summer School/R Code/Introduction to Statistical Learning/Dataset"
fileLocation = file.path(defaultDataDir, fileName)
Spam = read.table(file = fileLocation,
                  header = TRUE,
                  na.strings = "?")
Spam = na.omit(Spam)
fix(Spam)



#############################################################################
#                                                                           #
#                             Classification                                #
#                                                                           #
#############################################################################

# Tree
rm(list = ls())
library(tree)
High = ifelse(Carseats$Sales > 8, "Yes", "No")
Carseats = data.frame(Carseats, High)
treeCarseats = tree(formula = High ~ . - Sales, data = Carseats)
summary(treeCarseats)
plot(treeCarseats)
text(treeCarseats, cex = 0.9, pretty = 0)
treeCarseats


set.seed (2)
train = sample (1:nrow(Carseats), 200)
CarseatsTest = Carseats[-train,]
HighTest = High[-train]
treeCarseats = tree(High ~ . - Sales, Carseats, subset = train)
treePred = predict(treeCarseats, CarseatsTest, type = "class")
table(treePred, HighTest)

set.seed (3)
cvCarseats = cv.tree(treeCarseats, FUN = prune.misclass)
cvCarseats

pruneCarseats = prune.misclass(treeCarseats, best = 9)
plot(pruneCarseats)
text(pruneCarseats, cex = 0.9, pretty = 0)

par(mfrow = c(1, 2))
plot(cvCarseats$size, cvCarseats$dev, type = "b")
plot(cvCarseats$k, cvCarseats$dev, type = "b")

treePred = predict(pruneCarseats, CarseatsTest, type = "class")
table(treePred, HighTest)

# Logistic Regression
attach(Default)
lgDefault = glm(formula = default ~ balance,
                data = Default,
                family = binomial)
summary (lgDefault)
plot(
  Default$balance,
  lgDefault$fitted.values,
  xlab = "Balance",
  ylab = "Probability of Default",
  col = 4
)
lgPrediction = predict(lgDefault, data.frame(balance = c(1000)), type = "response")
predict(lgDefault, data.frame(balance = c(1000, 2000)), type = "response")

lgDefault = glm(formula = default ~ student,
                data = Default,
                family = binomial)
summary (lgDefault)

lgDefault = glm(formula = default ~ .,
                data = Default,
                family = binomial)
summary (lgDefault)



par(mfrow = c(1, 2))
de1 = Default[Default["student"] == "No", ]
de2 = Default[Default["student"] == "Yes", ]
de1 = de1[sort(de1$balance),]
de2 = de2[sort(de2$balance),]
pre1 = predict(lgDefault, de1, type = "response")
pre2 = predict(lgDefault, de2, type = "response")
plot(de1$balance,
     pre1,
     col = 4,
     ylab = "Default Rate",
     xlab = "Credit Card Balance")
par(new = TRUE)
plot(de2$balance,
     pre2,
     col = 6,
     axes = F,
     ann = F)
par(new = F)
plot(
  Default$student,
  Default$balance,
  col = c("cyan3", "coral2"),
  xlab = "Student Status",
  ylab = "Credit Card Balance"
)

predict(lgDefault,
        data.frame(
          balance = c(1500),
          student = c("Yes", "No"),
          income = c(40000)
        ),
        type = "response")


#############################################################################
#                                                                           #
#                             Regression                                    #
#                                                                           #
#############################################################################
library(ISLR)


attach(Advertising)
adSLR = lm(formula = Sales ~ TV , data = Advertising)
summary(adSLR)

par(mfrow = c(1, 3))
plot(TV, Sales, col = "red")
abline(lm(formula = Sales ~ TV , data = Advertising),
       lwd = 3,
       col = "blue")
plot(Radio, Sales, col = "red")
abline(lm(formula = Sales ~ Radio , data = Advertising),
       lwd = 3,
       col = "blue")
plot(Newspaper, Sales, col = "red")
abline(lm(formula = Sales ~ Newspaper , data = Advertising),
       lwd = 3,
       col = "blue")

par(mfrow = c(2, 2))
plot(adSLR)

plot(predict(adSLR), residuals(adSLR))

multipleLR = lm(formula = Sales ~ TV + Radio + Newspaper , data = Advertising)
summary(multipleLR)

cor(Advertising[-1])
multipleLR = lm(formula = Sales ~ TV + Radio, data = Advertising)
predict(multipleLR, data.frame(TV = c(100), Radio = c(20)), interval = "confidence")
predict(multipleLR, data.frame(TV = c(100), Radio = c(20)), interval = "predict")

multipleLR = lm(formula = Sales ~ TV * Radio, data = Advertising)
coefficients(summary(multipleLR))

plot(multipleLR$residuals)
abline(0, 0, lty = "dashed")


multipleLR = lm(formula = Auto$mpg ~ Auto$horsepower + I(Auto$horsepower ^
                                                           2),
                data = Auto)
coefficients(summary(multipleLR))
plot(Auto$horsepower, Auto$mpg, col = "red")
lines(
  sort(Auto$horsepower),
  fitted(multipleLR)[order(Auto$horsepower)],
  col = 'blue',
  type = 'l',
  lwd = 3
)
multipleLR = lm(formula = Auto$mpg ~ poly(Auto$horsepower, 5),
                data = Auto)





#############################################################################
#                                                                           #
#                             Overfitting                                   #
#                                                                           #
#############################################################################
library(boot)
set.seed (20)
attach(Auto)
par(mfrow = c(1, 1))
cvError = rep (0, 10)
for (i in 1:10) {
  glmFit = glm(mpg ~ poly(horsepower , i), data = Auto)
  cvError[i] = cv.glm(Auto, glmFit, K = 10)$delta[1]
}
plot(
  1:10,
  cvError,
  type = "b",
  xlab = "Degree of Polynomial",
  ylab = "Mean Squared Error",
  col = 2
)
abline(min(cvError) + sqrt(min(cvError)), 0)

library(leaps)
library(ISLR)
attach(Hitters)
mpgRegSubsets = regsubsets(Salary ~ .,
                           data = Hitters,
                           nvmax = 19,
                           method = "forward")
summary(mpgRegSubsets)
regSummary = summary(mpgRegSubsets)
par(mfrow = c(2, 2))
plot(regSummary$rss,
     xlab = "Number of Variables",
     ylab = "RSS",
     type = "l")
plot(regSummary$adjr2,
     xlab = "Number of Variables",
     ylab = "Adjusted R^2",
     type = "l")
points(
  which.max(regSummary$adjr2),
  max(regSummary$adjr2),
  col = 2,
  cex = 2,
  pch = 20
)
plot(regSummary$cp,
     xlab = "Number of Variables",
     ylab = "Cp",
     type = "l")
points(
  which.min(regSummary$cp),
  min(regSummary$cp),
  col = 2,
  cex = 2,
  pch = 20
)
plot(regSummary$bic,
     xlab = "Number of Variables",
     ylab = "BIC",
     type = "l")
points(
  which.min(regSummary$bic),
  min(regSummary$bic),
  col = 2,
  cex = 2,
  pch = 20
)


# Ridge Regression
library(glmnet)
Hitters = na.omit(Hitters)
x = model.matrix(Salary ~ ., Hitters)[,-1]
y = Hitters$Salary
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = -train

set.seed(1)
grid = 10 ^ seq (5, -5, length = 100)
ridgeLR = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
cvRidgeLR = cv.glmnet(x[train,], y[train], alpha = 0)
bestlamda = cvRidgeLR$lambda.min
ridgePre = predict(ridgeLR, s = bestlamda, newx = x[test,])
plot(log(cvRidgeLR$lambda),
     cvRidgeLR$cvm,
     xlab = "log(Lambda)",
     ylab = "Mean-Squared Error")
points(
  log(cvRidgeLR$lambda[which.min(cvRidgeLR$cvm)]),
  min(cvRidgeLR$cvm),
  col = 2,
  cex = 2,
  pch = 20
)

set.seed(1)
linearRegression = lm(y[train] ~ ., data.frame(y[train], x[train,]))
lmPre = predict(linearRegression, as.data.frame(x[test, ]))
c(mean((ridgePre - y[test]) ^ 2), mean((lmPre - y[test]) ^ 2))

set.seed(1)
lassoLR = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
cvLassoLR = cv.glmnet(x[train,], y[train], alpha = 1)
bestlamda = cvLassoLR$lambda.min
lassoPre = predict(lassoLR, s = bestlamda, newx = x[test,])
lassoCoef = predict(lassoLR,
                    s = bestlamda,
                    newx = x[test,],
                    type = "coefficients")[1:20, ]
lassoCoef[lassoCoef != 0]
c(mean((lassoPre - y[test]) ^ 2), mean((ridgePre - y[test]) ^ 2), mean((lmPre -
                                                                          y[test]) ^ 2))


#############################################################################
#                                                                           #
#                                 KNN                                       #
#                                                                           #
#############################################################################
detach()

library(class)
library(ISLR)
Caravan = na.omit(Caravan)
dim(Caravan)
standardCaravan = scale(Caravan[,-86])
set.seed(100)
test = sample(1:nrow(standardCaravan), 1500)
train = -test
err = rep(0, 10)
for (i in 1:10) {
  out = knn.cv(standardCaravan[train,], cl = Caravan$Purchase[train], k = i)
  err[i] = mean(out != Caravan$Purchase[train])
}
plot(err,
     type = "b",
     ylab = "Error Rate",
     xlab = "K")

knnPred = knn(
  standardCaravan[train, ],
  standardCaravan[test, ],
  Caravan$Purchase[train],
  k = 5,
  prob = F
)
table(knnPred, Caravan$Purchase[test])

glm.fit = glm(Purchase ~ .,
              data = Caravan ,
              family = binomial,
              subset = train)
glm.probs = predict(glm.fit, Caravan[test, ], type = "response")
glm.pred = rep("No", 1500)
glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, Caravan$Purchase[test])



detach()

##########################################################################################
install.packages("FNN")
library(FNN)
Advertising = Advertising[, -1]

test = 1:100
train = -test
knnReg = knn.reg(
  train = Advertising[train, ],
  test = Advertising[test, ],
  y = Advertising$Sales[train],
  k = 5
)
mean((knnReg$pred - Advertising$Sales[test]) ^ 2)

multipleLR = lm(formula = Sales ~ TV * Radio ,
                data = Advertising,
                subset = train)
df = data.frame(Advertising[test, ])
mlrPred = predict(multipleLR, df)
mean((mlrPred - Advertising$Sales[test]) ^ 2)



#############################################################################
#                                                                           #
#                               Clustering                                  #
#                                                                           #
#############################################################################

# K - Means Clustering

rvMatrix = matrix(data = rpois(100, 100), ncol = 2)
rvMatrix[, 1] = rvMatrix[, 1] + 10
rvMatrix[, 2] = rvMatrix[, 2] - 5
kmCluster = kmeans(x = rvMatrix,
                   centers = 2,
                   nstart = 20)
plot(
  rvMatrix,
  col = kmCluster$cluster + 1,
  main = "K - Means Clustering Results with K = 2",
  xlab = "",
  ylab = "",
  pch = 20,
  cex = 2
)
kmCluster$tot.withinss

kmCluster = kmeans(x = rvMatrix,
                   centers = 3,
                   nstart = 20)
plot(
  rvMatrix,
  col = kmCluster$cluster + 1,
  main = "K - Means Clustering Results with K = 3",
  xlab = "",
  ylab = "",
  pch = 20,
  cex = 2
)
kmCluster$tot.withinss

par(mfrow = c(1, 3))
hcComplete = hclust(dist(rvMatrix), method = "complete")
plot(
  hcComplete,
  main = "Complete Linkage",
  xlab = "",
  sub = "",
  cex = .9
)
hcAverage = hclust(dist(rvMatrix), method = "average")
plot(
  hcAverage,
  main = "Average Linkage",
  xlab = "",
  sub = "",
  cex = .9
)
hcSingle = hclust(dist(rvMatrix), method = "single")
plot(
  hcSingle,
  main = "Single Linkage",
  xlab = "",
  sub = "",
  cex = .9
)

par(mfrow = c(1, 3))
hcCutComplete = cutree(hcComplete, k = 2)
plot(
  rvMatrix,
  col = hcCutComplete + 1,
  main = "Complete Linkage",
  xlab = "",
  ylab = "",
  pch = 20,
  cex = 2
)
hcCutAverage = cutree(hcAverage, k = 2)
plot(
  rvMatrix,
  col = hcCutAverage + 1,
  main = "Average Linkage",
  xlab = "",
  ylab = "",
  pch = 20,
  cex = 2
)
hcCutSingle = cutree(hcAverage, k = 2)
plot(
  rvMatrix,
  col = hcCutAverage + 1,
  main = "Single Linkage",
  xlab = "",
  ylab = "",
  pch = 20,
  cex = 2
)

library(scatterplot3d)
rvMatrix = matrix(data = rpois(150, 10),
                  ncol = 3,
                  byrow = T)
dd = as.dist(1 - cor(t(rvMatrix)))
plot(
  hclust(dd, method = "complete"),
  main = "Correlation-based Distance Measure",
  xlab = "",
  ylab = ""
)
hc = cutree(hclust(dd, method = "complete"), k = 3)
scatterplot3d(
  x = rvMatrix[, 1],
  y =  rvMatrix[, 2],
  z = rvMatrix[, 3],
  color = hc + 1,
  main = "Correlation-based Distance Measure",
  xlab = "",
  ylab = "",
  zlab = "",
  pch = 20
)
