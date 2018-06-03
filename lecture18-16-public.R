
## From last class...
## Obtaining a variable importnce measure
## predictions and "fitted" values

x.z <- read  zip train
xt.z <- read zip test

d <- c(7, 9, 1)
xx <- x.z[ x.z$V1 %in% d, ]
xx$V1 <- as.factor(xx$V1)
dd <- xt.z[ xt.z$V1 %in% d, ]
truth <- dd$V1 <- as.factor(dd$V1)

library(randomForest)
set.seed(123)
a.rf <- randomForest(V1 ~ ., data=xx, ntree=500)
arf.pr <- predict(a.rf, newdata=dd, type='response')
table(arf.pr, truth)
mean( arf.pr != truth )

#
varImpPlot(a.rf, main='Zip codes')
varUsed(a.rf)

(a.rf)
# confusion matrix is based on OOB votes
plot(a.rf)

# warning!!, "fitted" values aren't what they seem!
b <- predict(a.rf, type='class')
table(b, xx$V1)
bb <- predict(a.rf, newdata=xx, type='class')
table(bb, xx$V1)




# Boosting with the ISOLET data
# same pre-processing as when we last used
# these data
x <- read.table('../lecture16/isolet-train.data', sep=',')
xt <- read.table('../lecture16/isolet-test.data', sep=',')

# 2 and 16 "M" and "N"
l1 <- 13
l2 <- 14
letters[c(l1, l2)]
xa <- x[ x$V618 == l1, ]
xb <- x[ x$V618 == l2, ]
x.tr <- rbind(xa, xb)
x.tr$V618 <- as.factor(x.tr$V618)

xta <- xt[ xt$V618 == l1, ]
xtb <- xt[ xt$V618 == l2, ]
x.te <- rbind(xta, xtb)
x.te$V618 <- as.factor(x.te$V618)

# Random forest
library(randomForest)
set.seed(123)
diso.rf <- randomForest(V618 ~ ., data=x.tr, ntree=100)
(diso.rf)
plot(diso.rf)

# performance on the test set
disorf.pr <- predict(diso.rf, newdata=x.te, type='response')
table(disorf.pr, x.te[,618])
mean(disorf.pr != x.te[,618])

# Boosting
library(adabag)
set.seed(123)
diso.bo <- boosting(V618 ~ ., data=x.tr, boos=FALSE, mfinal=100,
                    coeflearn = 'Freund',
                    control=rpart.control(maxdepth=5))

diso.bo.pr <- predict(diso.bo, newdata=x.te)
table(diso.bo.pr$class, x.te[,618])
mean(diso.bo.pr$class != x.te[,618])

err.test <- errorevol(diso.bo, newdata=x.te)
err.tr <- errorevol(diso.bo, newdata=x.tr)

plot(err.test$error, type="l",   main="AdaBoost error Vs number of trees",
     xlab="Iterations", ylab="Error", col = "red", lwd=3, ylim=c(0, .15))
lines(err.tr$error, cex = .5 ,col="blue", lty=1, lwd=3)
legend("topright", c("test","train"), col = c("red", "blue"), lty=1, lwd=3)



# The spam data
# https://archive.ics.uci.edu/ml/datasets/Spambase

# Read the data
xsp <- read.table('../lecture16/spam.data', header=FALSE)
# V58 contains the class label ('1' == spam)

# Read the indicators for a training / test split
# spam.traintest = indicators of training / test split (1 = test)
tt <- scan('../lecture16/spam.traintest')

# Make the response a factor
xsp$V58 <- as.factor(xsp$V58)

# Build the training and test sets
xsp.tr <- xsp[ tt == 0, ]
xsp.te <- xsp[ tt == 1, ]


# Boosting
# note the choice of coefficients and the parameters
# to control the base classifiers
library(adabag)
dsp.bo <- boosting(V58 ~ ., data=xsp.tr, boos = FALSE, mfinal = 100,
                   coeflearn = 'Freund',
                   control=rpart.control(maxdepth=5))

# performance on the test set?
dbo.pr1 <- predict(dsp.bo, newdata=xsp.te)
table(dbo.pr1$class, xsp.te[,58])
mean(dbo.pr1$class != xsp.te[,58])

err.test <- errorevol(dsp.bo, newdata=xsp.te)
err.tr <- errorevol(dsp.bo, newdata=xsp.tr)

# plot
plot(err.test$error, type="l",   main="AdaBoost error Vs number of trees",
     xlab="Iterations", ylab="Error", col = "red", lwd=4, ylim=c(0, .1))
lines(err.tr$error, cex = .5 ,col="blue", lty=1, lwd=4)
legend("topright", c("test","train"), col = c("red", "blue"), lty=1, lwd=4)

