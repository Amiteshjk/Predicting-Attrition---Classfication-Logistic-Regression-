setwd("/Users/amiteshjk/Desktop/JGBS/Term 3/Predictive Analytics/Logistic Regression/")
library(readxl) # to read excel file
attrition <- read_excel("LR.xlsx")
attrition
str(attrition)
attrition$Attrition=as.factor(attrition$Attrition)
attrition$Work_Challenging=as.factor(attrition$Work_Challenging)
attrition$Work_Envir=as.factor(attrition$Work_Envir)
attrition$Compensation=as.factor(attrition$Compensation)
attrition$Tech_Exper=as.factor(attrition$Tech_Exper)
str(attrition)
summary(attrition)
attrition$Attrition

#lR model
## Check for p-value significance.If >0.5, remove from model and re-run
attri_model=glm(Attrition~Yrs_Exp+Work_Challenging+Work_Envir+Compensation+Tech_Exper,data=attrition,family=binomial(link="logit"))
summary(attri_model)

#model 2
## Removed yrs of experience
attri_model2=glm(Attrition~Work_Challenging+Work_Envir+Compensation+Tech_Exper,data=attrition,family=binomial(link="logit"))
summary(attri_model2)

############## Not required ###################

#residual deviance reduces with the addition of coefficient
anova(attri_model2,"PChiSq")
#comparison of both the models
anova(attri_model2,attri_model, test="Chisq")


#Model Fit verification
(pseudo_R_sq=1-(attri_model2$deviance/attri_model2$null.deviance))

###############################################

#model validation-DATA PARTITION - Use other partitioning method
set.seed(1234)
ind <- sample(2, nrow(attrition),replace = TRUE,prob = c(0.8, 0.2))
Training_Data=attrition[ind==1,]
Testing_Data=attrition[ind==2,]
summary(Training_Data)
summary(Testing_Data)




## Create model using Training data
model=glm(Attrition~Work_Challenging+Work_Envir+Compensation+Tech_Exper,data=Training_Data,family=binomial(link="logit"))
summary(model)

#predict training data using original model

p1=predict(attri_model2,Training_Data,  type="response")
p1
attributes(p1)

## set threshold and assign prediction

p1=ifelse(p1>.5,"Yes","No")
p1=as.factor(p1)
p1

## Performance of Training data with original model
confusionMatrix(p1,Training_Data$Attrition)

## Manual confusion matrix
(t1=table(Predicted = p1 , Actual = Training_Data$Attrition))

#miscalssification error (1-Accuracy) -train data
1-sum(diag(t1))/sum(t1)




#predict testing data using training model
p2=predict(model,Testing_Data,  type="response")
p2
p2=ifelse(p2>.5,"Yes","No")
p2=as.factor(p2)

##Confusion matrix directly
confusionMatrix(p2,Testing_Data$Attrition)

##Confusion matrix
(t2=table(Predicted = p2, Actual = Testing_Data$Attrition))

#miscalssification (1-Accuracy) error -test data
1-sum(diag(t2))/sum(t2)

#Goodness of fit
with(attri_model2,pchisq(attri_model2$null.deviance-attri_model2$deviance,attri_model2$df.null-attri_model2$df.residual,lower.tail = F))

#new Data
new_data=data.frame(Work_Challenging="No",Work_Envir="Low",Compensation="Excellent",Tech_Exper="Excellent")

#predict using overall model
pn=predict(attri_model2,new_data,  type="response")
pn

attributes(pn)

## based on threshold, renaming to attrition yes and no
pn1=ifelse(pn>.5,"attrition yes","attrition no")
pn1

?set.seed

attrition$Attrition
attri_model2$fitted.values

#ROC CURVE
library(pROC)
roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE)
par(pty = "s")
roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE)
roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE,legacy.axes=TRUE)

roc.info <- roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE,legacy.axes=TRUE)
roc.info$thresholds
roc.info



## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "attrition".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "no attrition". 
## Thus, TPP = 0% and FPP = 0%

## now let's look at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

## We can calculate the area under the curve...
roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

#roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")
