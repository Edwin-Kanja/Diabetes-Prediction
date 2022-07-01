# DIABETES PREDICTION USING R
diabetes<-read.csv("C:/Users/EDWIN/Desktop/Projects/Diabetes/diabetes.csv")
View(diabetes)
# Object names
names(diabetes)



# SPLIT DATA
#set seed-same sample ca be reproduced in future
set.seed(101)

# split testing data 80%
split<-sample.int(n=nrow(diabetes), size=floor(.70*nrow(diabetes)), replace=F)
train_data<-diabetes[split,]
test_data<-diabetes[-split,]
count(test_data)
count(train_data)

# CORRELATION BETWEEN VARIABLES IN THE TRAIN SET
cor(train_data)
pairs(cor(train_data))

# correlation plot
panel.cor<-function(x,y,digits=3,prefix="",cex.cor, ...){
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(0,1,0,1))
  Cor<-abs(cor(x,y))
  txt<-paste0(prefix, format(c(Cor, 0.123456789), digits=digits)[1])
  if(missing(cex.cor)){
    cex.cor<-0.4/strwidth(txt)
  }
  text(0.5,0.5, txt,
       cex=1+cex.cor*Cor)
}
pairs(train_data,
      upper.panel = panel.cor,
      lower.panel = panel.smooth)

# DETERMINE WHICH VARIABLES SUITS OUR MODEL
# USE LOGISTIC REGRESSION

model<-glm(Outcome~.,data=train_data)
model
summary(model)#Alternative is step.1 below

# STEPWISE METHOD. consider the variable with lowest p-value
# Step.1
summary(glm(Outcome ~ Pregnancies,data= train_data))
summary(glm(Outcome~Glucose,data=train_data))
summary(glm(Outcome~BloodPressure,data= train_data))
summary(glm(Outcome~SkinThickness,data= train_data))
summary(glm(Outcome~Insulin,data=train_data))
summary(glm(Outcome~BMI,data= train_data))
summary(glm(Outcome~DiabetesPedigreeFunction,data=train_data))
summary(glm(Outcome~Age,data=train_data))
#Glucose has the lovest p-value in this case. consider as the next predictor.

# Step.2
summary(glm(Outcome~Glucose+Pregnancies,data= train_data))
summary(glm(Outcome~Glucose+BloodPressure,data= train_data))
summary(glm(Outcome~Glucose+SkinThickness,data= train_data))
summary(glm(Outcome~Glucose+Insulin,data=train_data))
summary(glm(Outcome~Glucose+BMI,data= train_data))
summary(glm(Outcome~Glucose+DiabetesPedigreeFunction,data=train_data))
summary(glm(Outcome~Glucose+Age,data=train_data))
#BMI has the lowest p-value in this case. consider it as the next predictor.

# Step.3
summary(glm(Outcome~Glucose+BMI+Pregnancies,data=train_data))
summary(glm(Outcome~Glucose+BMI+BloodPressure,data=train_data))
summary(glm(Outcome~Glucose+BMI+SkinThickness,data=train_data))
summary(glm(Outcome~Glucose+BMI+Insulin,data=train_data))
summary(glm(Outcome~Glucose+BMI+DiabetesPedigreeFunction,data=train_data))
summary(glm(Outcome~Glucose+BMI+Age,data=train_data))
#Pregnancies has the lowest p-Value in this case. consider is as the next predictor

# Step.4
summary(glm(Outcome~Glucose+BMI+Pregnancies+BloodPressure,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+SkinThickness,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+Insulin,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+Age,data=train_data))
#DiabetesPedigreeFunction has the lowest p-value in this case. consider it as the next predictor.

# Step.5
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+BloodPressure,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+SkinThickness,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+Insulin,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+Age,data=train_data))
#BloodPressure has the lowest p-value in this case. conside it as the next predictor.

# Step.6
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+BloodPressure+SkinThickness,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+BloodPressure+Insulin,data=train_data))
summary(glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+BloodPressure+Age,data=train_data))
#THE P-VALUES ARE NOW GRETER THAN 0.05. SO WE STOP THERE AND CONSIDER THE Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+BloodPressure FOR OUR MODEL.

# our model is
Model1<-glm(Outcome~Glucose+BMI+Pregnancies+DiabetesPedigreeFunction+BloodPressure+Age,data=diabetes)
summary(Model1)

# ALTERNATIVE INCASE STEPWISE IS TOO MUCH WORK. 
# in this formula 'Age' is included, we shall include it in our model.
library(MASS)
step.model<-stepAIC(model, direction='both')
summary(step.model)

head(test_data)
tail(test_data,10)
# Predict for a row(no.27) in test_data who is diabetic
pre1<-data.frame(Glucose=147,BMI=39.4,Pregnancies=7,
                DiabetesPedigreeFunction=0.257,BloodPressure=76, Age=43)
answer<-predict(Model1, pre, type='response')
answer #0.6417- the patient has DIABETES hence true


# 761 has no diabetes, let us proof
pre2<-data.frame(Glucose=88,BMI=28.4,Pregnancies=2,
                 DiabetesPedigreeFunction=0.766,BloodPressure=58, Age=22)
answer2<-predict(Model1, pre2, type='response')
answer2 #0.112- the patient is not diabetic

# MODEL ACCURACY
test_data1<-predict(Model1, test_data, type = 'response')
test_data1 #Testing all in the test_dataset
table(ActualValue=test_data$Outcome,
      PredicedValue=test_data1>0.5)#0.5 is our threshhold
(131+48)/(131+15+37+48) #calculates the accuracy which is 77.49%
# 37 patients were diabetic hence the model considered them not diabetic which is wrong.
# Now we reduce that number to a smaller number by finding a new threshold.
library(ROCR)
train_data1<-predict(Model1, train_data, type = 'response')
train_data1
rocrpred<-prediction(train_data1, train_data$Outcome)
ROCRPREF<-performance(rocrpred, "tpr", "fpr")
plot(ROCRPREF, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

table(ActualValue=test_data$Outcome,
      PredicedValue=test_data1>0.4) #our suitable threshold is 0.4 which has a higher positive rate and a low false positive rate.
(118+63)/(118+28+22+62)# our model accuracy 78.7%, an increase from a threshold of 0.5.
