library(readxl)
getwd()
set.seed(42)
setwd("C:\\Users\\aaroh\\Documents\\BA with R")
organics <- read_excel("organics.xlsx")
head(organics)
str(organics)


targetorganic <- count(organics, c('TargetBuy'))
targetorganic
targetdata <- data.frame(organics$TargetBuy, organics$TargetAmt)
cor(targetdata)
plot(targetdata)


organics$DemCluster <- NULL
organics$TargetAmt <- NULL
organics$ID <- NULL

#check for missing values


sum(is.na(organics$DemAffl))
sum(is.na(organics$DemAge))
sum(is.na(organics$DemClusterGroup))
sum(is.na(organics$DemGender))
sum(is.na(organics$DemReg))
sum(is.na(organics$DemTVReg))
sum(is.na(organics$PromClass))
sum(is.na(organics$PromSpend))
sum(is.na(organics$PromTime))
sum(is.na(organics$TargetBuy))
nrow(organics)

#Impute missing values

organics$DemAge <- replace(organics$DemAge, is.na(organics$DemAge), mean(na.omit(organics$DemAge)))
organics$DemAffl <- replace(organics$DemAffl, is.na(organics$DemAffl), mean(na.omit(organics$DemAffl)))
organics$PromTime <- replace(organics$PromTime, is.na(organics$PromTime), mean(na.omit(organics$PromTime)))


library(plyr)

#Replace missing categorical values with most frequent value
gendercount <- count(organics, c('DemGender'))
gendercount
gendermax <- gendercount[which.max(gendercount$freq), c('DemGender')]
gendermax
organics$DemGender <- replace(organics$DemGender, is.na(organics$DemGender), gendermax)


regioncount <- count(organics, c('DemReg'))
regioncount
regionmax <- regioncount[which.max(regioncount$freq), c('DemReg')]
regionmax
organics$DemReg <- replace(organics$DemReg, is.na(organics$DemReg), regionmax)

tvregcount <- count(organics, c('DemTVReg'))
tvregcount
tvregmax <- tvregcount[which.max(tvregcount$freq), c('DemTVReg')]
tvregmax
organics$DemTVReg <- replace(organics$DemTVReg, is.na(organics$DemTVReg), tvregmax)


clustergroupcount <- count(organics, c('DemClusterGroup'))
clustergroupcount
clustergroupmax <- clustergroupcount[which.max(clustergroupcount$freq), c('DemClusterGroup')]
clustergroupmax
organics$DemClusterGroup <- replace(organics$DemClusterGroup, is.na(organics$DemClusterGroup), clustergroupmax)


#Randomize data
rand <- runif(nrow(organics)) 
organicrand <- organics[order(rand), ]

#Partition data
organictrain <- organicrand[1:11112, ]
organictest <- organicrand[11113:22224, ]

library(rpart)
library(rpart.plot)

#Build decision tree
organictree <- rpart(TargetBuy ~ ., data = organictrain, method = "class")
organictree
printcp(organictree)
plotcp(organictree)
rpart.plot(organictree)

organictreetest <- rpart(TargetBuy ~ ., data = organictest, method = "class")
organictreetest
printcp(organictreetest)
plotcp(organictreetest)
rpart.plot(organictreetest)

organictrain$pred <- predict(organictree, organictrain, type = "class") #create a prediction using our tree
table(Actual = organictrain$TargetBuy, Predicted = organictrain$pred) #create a confusion matrix

organictrain$correct <- organictrain$TargetBuy == organictrain$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
traincorrectcount <- length(which(organictrain$correct))
trainincorrectcount <- nrow(organictrain) - traincorrectcount
trainerrorrate <- trainincorrectcount/nrow(organictrain)
trainaccuracy <- 1-trainerrorrate

#Now look at test
organictest$pred <- predict(organictreetest, organictest, type = "class") #create a prediction using our tree
table(Actual = organictest$TargetBuy, Predicted = organictest$pred) #create a confusion matrix

organictest$correct <- organictest$TargetBuy == organictest$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
testcorrectcount <- length(which(organictest$correct))
testincorrectcount <- nrow(organictest) - testcorrectcount
testerrorrate <- testincorrectcount/nrow(organictest)
testaccuracy <- 1-testerrorrate

#Compare
paste("TRAIN: Error Rate (", trainerrorrate, ") Accuracy (", trainaccuracy, ")")
paste("TEST: Error Rate (", testerrorrate, ") Accuracy (", testaccuracy, ")")


### Test Model Performance - Creates a 2X2 confusion matrix and associated metrics
testModelPerformance <- function(model, dataset, target, prediction) {
  if(missing(prediction))
  {
    print("here")
    dataset$pred <- predict(model, dataset, type = "class")
  }
  else
  {
    print("here2")
    dataset$pred <- prediction
  }
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(model))))
  writeLines(paste("Target:", deparse(substitute(target))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = target, Predicted = dataset$pred)
  truePos <- confMatrix[2,2]
  falseNeg <- confMatrix[2,1]
  falsePos <- confMatrix[1,2]
  trueNeg <- confMatrix[1,1]
  print(confMatrix)
  writeLines("\n\n")
  
  accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
  sensitivity <- truePos/(truePos + falseNeg)
  specificity <- trueNeg/(falsePos + trueNeg)
  falsePosRate <- falsePos/(falsePos + trueNeg)
  falseNegRate <- falseNeg/(truePos + falseNeg)
  precision <- truePos/(truePos + falsePos)
  
  writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
  writeLines(paste("Specificity:", round(specificity, digits = 4)))
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  dataset
}


#Evaluate max tree performance
ortrain <- testModelPerformance(organictree, organictrain, organictrain$TargetBuy)
orval <- testModelPerformance(organictreetest, organictest, organictest$TargetBuy)

#Logit Regression

### Log Likelihood - Computes log likelihood 
llh <- function(y, py) {
  sum(y * log(py) + (1-y) * log(1-py))
}


organiclogit <- glm(TargetBuy ~., data = organictrain, family = binomial(link = "logit"))
summary(organiclogit)
organiclogit <- glm(TargetBuy ~ DemAffl+DemGender+DemAge+DemReg, data = organictrain, family = binomial(link = "logit"))
summary(organiclogit)


confint.default(organiclogit) #Build confidence intervals
exp(coef(organiclogit)) #Calculate odds ratio

#Calculate Chi-Square
devdiff <- with(organiclogit, null.deviance - deviance) #difference in deviance between null and this model
dofdiff <- with(organiclogit, df.null - df.residual) #difference in degrees of freedom between null and this model
pval <- pchisq(devdiff, dofdiff, lower.tail = FALSE )
paste("Chi-Square: ", devdiff, " df: ", dofdiff, " p-value: ", pval)

#Calculate Psuedo R2
organictrain$probdonate <- predict(organiclogit, newdata = organictrain, type = "response")
resid.dev <- 2 * llh(organictrain$TargetBuy, organictrain$probdonate)
null.dev <- 2 * llh(organictrain$TargetBuy, mean(organictrain$TargetBuy))
pr2 <- 1-(resid.dev/null.dev)
paste("Psuedo R2: ", pr2)


#Evaluate logit performance
organictrain$logitpred <- round(organictrain$probdonate)
organictrain$logitpred

#logit classification on test data

organiclogittest <- glm(TargetBuy ~., data = organictest, family = binomial(link = "logit"))
summary(organiclogittest)
organiclogittest <- glm(TargetBuy ~ DemAffl+DemGender+DemAge, data = organictest, family = binomial(link = "logit"))
summary(organiclogittest)

confint.default(organiclogittest) #Build confidence intervals
exp(coef(organiclogittest)) #Calculate odds ratio

#Calculate Chi-Square
devdifftest <- with(organiclogittest, null.deviance - deviance) #difference in deviance between null and this model
dofdifftest <- with(organiclogittest, df.null - df.residual) #difference in degrees of freedom between null and this model
pvaltest <- pchisq(devdifftest, dofdifftest, lower.tail = FALSE )
paste("Chi-Square: ", devdifftest, " df: ", dofdifftest, " p-value: ", pvaltest)

#Calculate Psuedo R2
organictest$probdonate <- predict(organiclogittest, newdata = organictest, type = "response")
resid.devtest <- 2 * llh(organictest$TargetBuy, organictest$probdonate)
null.devtest <- 2 * llh(organictest$TargetBuy, mean(organictest$TargetBuy))
pr2test <- 1-(resid.devtest/null.devtest)
paste("Psuedo R2: ", pr2test)


#Evaluate logit performance
organictest$logitpred <- round(organictest$probdonate)
organictest$logitpred

lotrain <- testModelPerformance(organiclogit, organictrain, organictrain$TargetBuy, organictrain$logitpred)
lotest <- testModelPerformance(organiclogittest, organictest, organictest$TargetBuy, organictest$logitpred)










 