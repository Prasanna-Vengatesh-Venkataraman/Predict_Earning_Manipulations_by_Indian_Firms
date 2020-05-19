rm(list=ls())
install.packages(c("xlsx" , "caret", "e1071" , "ROSE" , "party" , "rpart.plot", "randomForest", "ROCR"))
library (caret)
library (e1071)
library(ROSE)
library(xlsx)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

ML <- read.xlsx(file.choose(), sheetIndex = 4, header = TRUE)

#C:\UIC\IDS 572 - Data Mining for Business\Assignments\Assignment 5

sum(complete.cases(ML))/nrow(ML)*100
#No missing cases

ML$C.MANIPULATOR <- as.factor(ML$C.MANIPULATOR)
str(ML)
options(scipen = 99)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#a)
colnames(ML)
#Calculating M-score using Beneish model's formula
mscore <- -4.84 + (0.92*ML$DSRI) + (0.528*ML$GMI) + (0.404*ML$AQI) + (0.892*ML$SGI) + 
            (0.115*ML$DEPI) - (0.172*ML$SGAI) + (4.679*ML$ACCR) - (0.327*ML$LEVI)

#Predicting manipulator by setting threshold as -2.22
manip <- ifelse(mscore>-2.22,"Yes","No")
manip <- as.factor(manip)

#Developing a confusion matrix we see that Sensitivity is 100
ML$Manipulater <- relevel(ML$Manipulater, ref = "Yes")
manip <- relevel(manip, ref = "Yes")
a <- confusionMatrix(manip, ML$Manipulater)

#Thus in the existing dataset False negatives (Manipulators predicted as non-manipulators) are zero.

a$table
a$byClass

# Precision = 9.51% & Sensitivity = 100%
# Athough sensitivity is very high, precision of Beneish model is very low for the current data and hence the model leads to a wastage of resources
# Hence Beneish model does not hold good for the Indian data

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#b)
#Due to an unbalanced class in a binary classification problem (ie. more number of samples in one class [majority class]
#and very few samples in another classs [minority class]), the models developed would be more biased towards the majority class
#The model might have a high accuracy, but if the class of interest is the minority class then recall and precision might be very low 

#Ensemble methods are generally robust to handle unbalanced data since it does not depend on just one model but a collection of models
#Ensemble methods like random forets may oversample the minority class and provide better results

#Unbalanced data can be handled through methods such as 
#1) Oversampling - adding more copies of the minority class. It is a good choice data set is not huge
#2) Undersampling - removing some observations of the majority class. It is a good choice data set is huge
#3) A combination of bot the above
#4) SMOTE (Synthetic Minority Oversampling Technique) - Uses a nearest neighbors algorithm to generate new and synthetic data for training  model 
#5) Using metrics such as Recall, Precision, Specificity and False alarm instead of accuracy to evaluate the models

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#c)
#Splitting the data set into Manipulators and non-manipulators
Yes <- ML[ML$Manipulater=="Yes",]
No <- ML[ML$Manipulater=="No",]

#Using sample to determine values randomly for setting seeds while selecting rows from Non-manipulators dataset
set.seed(123)
a <- sample(10000, 100, replace = F)
m <- matrix(NA, ncol = 9, nrow = 100)

#Determining significant variables by running 100 samples
for (i in 1:100) {
  set.seed(a[i])
  row <- sample(nrow(No), 220-39, replace = F)
  s <- rbind(Yes, No[row,])
  s <- s[sample(nrow(s)),]
  s <- s[,-c(1,11)]
  sam <- ovun.sample(Manipulater ~ ., data = s, method = "under", N = (nrow(Yes)*2), seed = 1)$data
  
  full <- glm(Manipulater~., data=sam, family = "binomial")
  null <- glm(Manipulater~1, data=sam, family = "binomial")
  b <- step(null, scope = list(lower=null, upper=full), direction = "both")
  c <- summary(b)
  d <- sort(rownames(c$coefficients))
  
  if(length(d)!=9)
  {
    for(j in 1:(9-length(d)))
    {
      d <- c(d,NA)
    }
  }
  
  m[i,] <- d
}

View(table(m))

#(Intercept)        ACCR         AQI        DEPI        DSRI         GMI        LEVI        SGAI         SGI 
#   100             100          100         31         100          86          21           7         100 

#We can see that columns "DEPI", "LEVI" & "SGAI" have appeared as significant variables in less than 35% of our models and hence can be neglected
#The remaining columns can be considered as significant columns, "GMI" appeaars in 86% of our models while the remaining four appear in all of our models.

model <- glm(Manipulater~ACCR+AQI+DSRI+GMI+SGI, data = s, family = "binomial")
summary(model)

#Based on summary statsitics of the model we can determine the logit of odds as below
#b0 = 6.0832 - (4.8990*ACCR) - (0.3217*AQI) - (0.8530*DSRI) - (0.6610*GMI) - (1.6763*SGI)

#The probability for both classes can be determined by
#Probability of yes = 1 / [1 + e^b0]
#Probability of no = e^b0 / [1 + e^b0]

b0 <- rep(NA,nrow(ML))
prob_yes <- rep(NA,nrow(ML))
prob_no <- rep(NA,nrow(ML))

for(i in 1:nrow(ML)) {
  b0[i] <- 6.0832 - (4.8990*ML[i,"ACCR"]) - (0.3217*ML[i,"AQI"]) - (0.8530*ML[i,"DSRI"]) -
              (0.6610*ML[i,"GMI"]) - (1.6763*ML[i,"SGI"])
  
  prob_yes[i] <- 1/(1+exp(b0[i]))
  prob_no[i] <- exp(b0[i])/(1+exp(b0[i]))
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#d)

predict <- predict(model, newdata = ML, type="response")

with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#p-value = 0.000000000000363458

with(model, df.null, df.residual)
#Degrees of Freedom = 219

model_performance <- data.frame(Cut_off=numeric(), TP=numeric(), TN=numeric(), FP=numeric(), FN=numeric(), Sensitivity=numeric(), Precision=numeric(), Specificity=numeric(), False_Alarm=numeric())
cutoff = 0.5

for(i in 1:50)
{
  pred <- ifelse(predict>cutoff,"No","Yes")
  pred <- as.factor(pred)
  pred <- relevel(pred, ref = "Yes")
  
  cm <- confusionMatrix(pred,ML$Manipulater)
  
  model_performance[i,1] <- cutoff
  model_performance[i,2] <- cm$table[1]
  model_performance[i,3] <- cm$table[4]
  model_performance[i,4] <- cm$table[3]
  model_performance[i,5] <- cm$table[2]
  model_performance[i,6] <- cm$byClass[1]
  model_performance[i,7] <- cm$byClass[5]
  model_performance[i,8] <- cm$byClass[2]
  model_performance[i,9] <- (1 - cm$byClass[2])
  
  cutoff = cutoff+0.01
}

View(model_performance)

#In the model developed when the cutoff probability is 0.5 there is a higher preciion but lower recall. Thus there are more number of false negatives and this may encourage manipulators to repeat the practice
#As the cut-off probability increases, the precision decreases but also results in an increase in recall
#Thus there is a trade-off between recall and precision with changes in cut-off point

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#e)
#Youden's index
model_performance$Youden_Index <- model_performance$Sensitivity + model_performance$Specificity - 1

#Cost-Based method
#In the given data set, classifying a manipulator as a non-manipulator (False Negative) has a considerably higher risk compared to classifying a non-manipulator as a manipulator (False Positive) 
#Higher False negatives might encourage the manipulators to repeat the practice
#Hence a higer penalty (0.85) needs to be assigned for False Negatives as compared to False Positives (0.15)

#False Negative (FN) indicates the number of manipulators classified as non-manipulators. Total number of manipulators is 39
#Proportion of manipulators classified as non-manipulators = FN/39


#False Positive (FP) indicates the number of non-manipulators classified as manipulators. Total number of non-manipulators is 1200
#Proportion of manipulators classified as non-manipulators = FP/1200

model_performance$cost <- (0.85*model_performance$FN/39) + (0.15*model_performance$FP/1200)

#Plot
plot(model_performance$cost~model_performance$Cut_off, col="orange", lwd=3, type="l", xlim=c(0.5,0.99), ylim=c(0,0.8), xlab="Cut-off", ylab="Youden's Index & Cost", main="Youden's Index vs Cost")
lines(model_performance$Youden_Index~model_performance$Cut_off, col="blue", lwd=3)
legend("topleft", col = c("blue", "orange"), legend = c("Youden's Index","Cost"), cex=0.8, pt.cex = 1, lty=1, lwd = 3)

#We can see that the cost is relatively high initially and decreases by larger margins once cut-off point exceeds 0.7
#Youden's index increases very slowly until the cut-off point reaches 0.7 after which it increases by larger margins followed by a sudden drop
#Plotting values for the cut-off points between 0.85 & 0.95

plot(model_performance$cost~model_performance$Cut_off, col="orange", lwd=3, type="l", xlim=c(0.85,0.95), ylim=c(0,0.8), xlab="Cut-off", ylab="Youden's Index & Cost", main="Youden's Index vs Cost")
lines(model_performance$Youden_Index~model_performance$Cut_off, col="blue", lwd=3)
legend("topleft", col = c("blue", "orange"), legend = c("Youden's Index","Cost"), cex=0.8, pt.cex = 1, lty=1, lwd = 3)

#As seen from the plot at cut-off point of 0.97 the youden's index reaches its peak value and cost is relatively low.
#Hence using both Youden's index & Cost-based method we can set the cut-off point as 0.97

View(model_performance[model_performance$Cut_off=="0.89",])

#Sensiivity and Specificity are high when cut-off is 0.89
#Sensiivity = 84.62% & Specificity = 86.92%
#MCA Technology Solutions can deploy this logistic regression model with cut-off point as 0.89 (above which would be non-manipulator)

#Metrics of undersampled model at optimal cut-off point
model1 <- c("Logistic Regression [Undersampled Data]", model_performance[model_performance$Cut_off=="0.89",c(1:11)])

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#f)Given, M-score is the value of the linear combination of statically significant variables corresponding to the cut-off probability that is chosen to classify between manipulators and non-manipulators
#FOr the above model built we can see that the significant variables are ACCR, AQI, DSRI, GMI and SGI

#Thus modifying the M-socre equation for Beneish model including only these variables, we get

mscore <- -4.84 + (0.92*ML$DSRI) + (0.528*ML$GMI) + (0.404*ML$AQI) + (0.892*ML$SGI) + (4.679*ML$ACCR)


#The cut-of probability is 0.89
#Instance 370 has the probability (0.89006) closest to the cut-of probability 
predict[370]

#Thus calculating the M-score at instancce 370, we get

mscore <- -4.84 + (0.92*ML$DSRI) + (0.528*ML$GMI) + (0.404*ML$AQI) + (0.892*ML$SGI) + (4.679*ML$ACCR)
mscore[370]

#Thus based on the models developed in questions 4 and 5, regulators can use a M-score of -1.924658 to identify potential manipulators.

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#g)
#Selecting training data and performing a combination of oversampling and undersampling to handle the unbalanced data
set.seed(1234)
TrainData <- No[sample(nrow(No),181),-c(1,11)]
TrainData <- rbind(TrainData,Yes[,-c(1,11)])
TrainData <- TrainData[sample(nrow(TrainData)),]
TrainData <- ovun.sample(Manipulater ~ ., data = TrainData, method = "both", p=0.4, seed=1)$data

round(prop.table(table(TrainData$Manipulater))*100,2)
#No - 58.64% and Yes - 41.36% of cases

#Building a CART tree using the training data data
cart_tree <- rpart(Manipulater~., data=TrainData, parms = list(split="gini"), control = rpart.control(cp=0))
rpart.plot(cart_tree)

#Decision Rules:
#Rule 1: If SGAI>=1.4 , then yes the company is a manipulator
#Support = 20% and Confidence = 0.86

#Rule 2: If SGAI<1.4 and ACCR>=-0.014 and SGI>=1.4 , then yes the company is a manipulator
#Support = 11% & Confidence = 0.96

#Rule 3: If SGAI<1.4 and ACCR>=-0.014 and SGI<1.4 and DEPI>=0.95 and DSRI>=1.3, then yes the company is a manipulator
#Support = 6% & Confidence = 1

#The support and confidence for class "No" is much less compared to class "Yes". But since the class of interest in the mdoel is "Yes" we can consider this model
ctree_cm <- confusionMatrix(predict(cart_tree, newdata = ML, type = "class"),ML$Manipulater)
ctree_cm
ctree_cm$byClass

#Sensitivity = 89.74% & precision = 11.21%

#Metrics for CART Tree
model4 <- c("C&R Tree", NA, ctree_cm$table[1],ctree_cm$table[4],ctree_cm$table[3],ctree_cm$table[2], ctree_cm$byClass[1], ctree_cm$byClass[5], ctree_cm$byClass[2], (1 - ctree_cm$byClass[2]), (ctree_cm$byClass[1]+ctree_cm$byClass[2]-1), ((0.85*ctree_cm$table[2]/39)+(0.15*ctree_cm$table[3]/1200)))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#h)

#Stepwise method or variabe selecction 
full <- glm(Manipulater~., data=ML[,-c(1,11)], family = "binomial")
null <- glm(Manipulater~1, data=ML, family = "binomial")
step(null, scope = list(lower=null, upper=full), direction = "both")

full_model <- glm(formula = Manipulater ~ DSRI + SGI + ACCR + AQI + GMI + SGAI, family = "binomial", data = ML)

#Predictions ith cut-off pint at 0.5
pred <- ifelse(full_model$fitted.values>0.5, "No", "Yes")
pred <- as.factor(pred)
pred <- relevel(pred,ref = "Yes")
a <- confusionMatrix(pred, ML$Manipulater)

#Metrics of new model with cut-off point at 0.5
model2 <- c("Logistic Regression [Full Dataset]", 0.5, a$table[1],a$table[4],a$table[3],a$table[2], a$byClass[1], a$byClass[5], a$byClass[2], (1 - a$byClass[2]), (a$byClass[1]+a$byClass[2]-1), ((0.85*a$table[2]/39)+(0.15*a$table[3]/1200)))

#Steps to determine optimal cut-off point in new model
model_performance <- data.frame(Cut_off=numeric(), Youden_Index=numeric(), Cost=numeric())
cutoff = 0.5

for(i in 1:50)
{
  pred <- ifelse(full_model$fitted.values>cutoff,"No","Yes")
  pred <- as.factor(pred)
  pred <- relevel(pred, ref = "Yes")
  
  cm <- confusionMatrix(pred,ML$Manipulater)
  model_performance[i,1] <- cutoff
  model_performance[i,2] <- (cm$byClass[1]+cm$byClass[2]-1)
  model_performance[i,3] <- ((0.85*cm$table[2]/39)+(0.15*cm$table[3]/1200))
  cutoff = cutoff+0.01
}

#Plotting to find optimal cut-off point based on Youden's Index and Cost based method
dev.off()
plot(model_performance$Cost~model_performance$Cut_off, col="orange", lwd=3, type="l", xlab="Cut-off", ylab="Youden's Index & Cost", ylim=c(0.1,0.7), main="Youden's Index vs Cost")
lines(model_performance$Youden_Index~model_performance$Cut_off, col="blue", lwd=3)
legend("topleft", col = c("blue", "orange"), legend = c("Youden's Index","Cost"), cex=0.8, pt.cex = 1, lty=1, lwd = 3)

dev.off()
plot(model_performance$Cost~model_performance$Cut_off, col="orange", lwd=3, type="l", xlim=c(0.9,0.99), xlab="Cut-off", ylab="Youden's Index & Cost", ylim=c(0.1,0.7), main="Youden's Index vs Cost")
lines(model_performance$Youden_Index~model_performance$Cut_off, col="blue", lwd=3)
legend("topleft", col = c("blue", "orange"), legend = c("Youden's Index","Cost"), cex=0.8, pt.cex = 1, lty=1, lwd = 3)

#Optimal cut-off point for new model = 0.98
#Predicitons based on optimal ccut-off point for new model
pred <- ifelse(full_model$fitted.values>0.98, "No", "Yes")
pred <- as.factor(pred)
pred <- relevel(pred,ref = "Yes")
a <- confusionMatrix(pred, ML$Manipulater)

#Metrics of new model at optimal cut-off point
model3 <- c("Logistic Regression [Full Dataset]", 0.98, a$table[1],a$table[4],a$table[3],a$table[2], a$byClass[1], a$byClass[5], a$byClass[2], (1 - cm$byClass[2]), (a$byClass[1]+a$byClass[2]-1), ((0.85*a$table[2]/39)+(0.15*a$table[3]/1200)))

#Data-Frame to compare previous under-sampled model with the new model developed on the full dataset
Model_comparison <- data.frame(Model=character(), Cut_off=numeric(), TP=numeric(), TN=numeric(), FP=numeric(), FN=numeric(), Sensitivity=numeric(), Precision=numeric(), Specificity=numeric(), False_Alarm=numeric(), Youden_Index=numeric(), Cost=numeric(), stringsAsFactors=FALSE)

for (i in 1:12) {
  Model_comparison[1,i] <- model1[i]
  Model_comparison[2,i] <- model2[i]
  Model_comparison[3,i] <- model3[i]
}

for(i in 7:12){
  Model_comparison[,c(i)] <- round(as.numeric(Model_comparison[,c(i)]),4)}

View(Model_comparison[,c(1,2,7:12)])

#In the above table 
#Model 1 - Logistic regression model built based on the undersampled data
#Model 2 - Logistic regression model developed on full dataset with cut-off point at 0.5
#Model 3 - Logistic regression model developed on full dataset with optimal cut-off point (0.97)

#Model 1 has better metrics compared to the other models.
#Model 1 has the highest Youden's Index among all 3 models. The cost of this model is slightly higher (but negligible) compared to Model 3
#The Youden's Index of Model 1 is nearly three times the Youden's Index of the Model 2
#Although the sensitivity of Model 1 is slightly lower than Model 3, Model 1 has a higher precision and Youden's Index compared to Model 3
#Thus Model 1 (the previous model built on the undersampled data) can be considered as the better model among all 3 

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#i) Random Forest
set.seed(1234)
rf <- randomForest(Manipulater~., data=TrainData,
          mtry=sqrt(ncol(TrainData)-1), ntree=100, 
            proximity=T, importance=T)
plot(rf)
#The error decreases as number of trees increases and sturates when number of trees are between 25 to 30

#Prediction across entire dataset
rf_pred <- predict(rf, newdata = ML, type = "class")
rf_pred <- relevel(rf_pred, ref = "Yes")
rf_cm <- confusionMatrix(rf_pred,ML$Manipulater)
#Recall = 94.87% & Precision = 23.27

#Important variables
varImpPlot(rf)
importance(rf)
#ACCR, SGI & SGAI seems to be the top 3 important variables according to Mean Decrease Gini and Accuracy

#RandomForest metrics
model5 <- c("Random Forest", NA, rf_cm$table[1],rf_cm$table[4],rf_cm$table[3],rf_cm$table[2], rf_cm$byClass[1], rf_cm$byClass[5], rf_cm$byClass[2], (1 - rf_cm$byClass[2]), (rf_cm$byClass[1]+rf_cm$byClass[2]-1), ((0.85*rf_cm$table[2]/39)+(0.15*rf_cm$table[3]/1200)))

#------------------------------------------------------------------------

#Ada Boosting
install.packages("adabag")
library("adabag")
set.seed(123)
adab <- boosting(Manipulater~., data = TrainData, mfinal = 10)

#important variables
sort(adab$importance, decreasing = T)

#Prediction across entire dataset
pred <- predict(adab,newdata = ML)

#Confusion Matrix
pred$class <- relevel(as.factor(pred$class), ref = "Yes")
adab_cm <- confusionMatrix(pred$class,ML$Manipulater)

#Error at each iteration
err.train <- errorevol(adab, TrainData)
err.test <- errorevol(adab, ML)

plot(err.train$error, type="l", col = "blue", lty = 2, lwd = 3, 
     ylim = c(0,0.3), main = "Errors across iterations", 
      ylab = "Error", xlab = "Iteration")

lines(err.test$error, type = "l", col = "red", lwd = 3)

legend("topright", col = c("red", "blue"), lty=c(1,2),
       legend = c("Test data","Training Data"), lwd = 1)

#AdaBoosting model metrics
model6 <- c("Adaboosting", NA, adab_cm$table[1],adab_cm$table[4],adab_cm$table[3],adab_cm$table[2], adab_cm$byClass[1], adab_cm$byClass[5], adab_cm$byClass[2], (1 - adab_cm$byClass[2]), (adab_cm$byClass[1]+adab_cm$byClass[2]-1), ((0.85*adab_cm$table[2]/39)+(0.15*adab_cm$table[3]/1200)))

#Comparison of various models
for (i in 1:12) {
  Model_comparison[4,i] <- model4[i]
  Model_comparison[5,i] <- model5[i]
  Model_comparison[6,i] <- model6[i]
}

for(i in 7:12){
  Model_comparison[,c(i)] <- round(as.numeric(Model_comparison[,c(i)]),4)}


View(Model_comparison)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#j) Final Recommendation:

#Companies can use ensemble method such as Random Forest and AdaBoosting to predict earnings manipulators
#These methods have the highest Sensitivity among all the models.
#Sensitivity of Random Forest is 94.87% while for AdaBoosting is 97.43%. Hence these models help predict most of the manipulators correctly which covers the main use of the model.
#Although both these models have a precision of 23% approx, it higher than most of the models. This value does indicate that a higher number of false positives (predicting Non-manipulator as manipulator) are predicted
#But the impact of False positives is generally less compared to False Negative, using this imapct the models can be evaluated using Cost-Based method.
#Both the ensemble methods have the lowest cost and highest Youden's Index which further supports the recommendation to use these methods
