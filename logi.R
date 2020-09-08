
##############################
#### Logistic Regression ####
#############################

# Goal- predict an individual has diabetes or not
# Dependent Variable-outcome(0-no, 1-yes)
# Independent variable-(Pregnancies, Glucose, BloodPressure, SkinThickness,
#                       Insulin, BMI, DiabetesPedigreeFunction, Age)


#########################
#### Import Dataset ####
########################
diab_data <- read.csv(file= "C:/Users/WASIM/Documents/diabetes.csv", header=TRUE)
str(diab_data)
diab_data$Outcome <- as.factor(diab_data$Outcome)
str(diab_data)

###########################################
#### Split dataset into test and train ####
###########################################

set.seed(789)
p<- c(0.9,0.1)
sample_diab_data <- sample(2, nrow(diab_data),replace = TRUE, prob = p)
train_data <- diab_data[sample_diab_data==1, ]
test_data <- diab_data[sample_diab_data==2, ]

#######################
#### Model  fitting####
#######################

train_model<-glm(Outcome~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,
                 data=train_data, family= 'binomial')
summary(train_model)

####################################################
#### Drop statistically insignificant varibles  ####
####################################################
# It is evident from summary that 'SkinThickness', 'Insulin' and 'Age' are statistically insignificant.
# So we drop those variables and rerun the model.

train_model<-glm(Outcome~ Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction,
                 data=train_data, family= 'binomial')
summary(train_model)
# Further it appears that 'BloodPressure' is not a statistically significant predictor.
# So we drop it from our final model
train_model<-glm(Outcome~ Pregnancies+Glucose+BMI+DiabetesPedigreeFunction,
                 data=train_data, family= 'binomial')
summary(train_model)

####################
#### Prediction ####
####################

pred <- predict(train_model,train_data,type = 'response')

head(pred)
head(train_data)

####################################################################
### Model Accuaracy & Misclassified percentage for training data####
####################################################################
Pred_accuracy <-ifelse(pred> 0.6, 1,0)
Pred_accuracy_table <- table(Predicted= Pred_accuracy, Real= train_data$Outcome )
Pred_accuracy_table
Percentage_misclass_train <-(1-sum(diag(Pred_accuracy_table))/sum(Pred_accuracy_table))*100
Percentage_misclass_train

#################################################################
#### Model Accuaracy & Misclassified percentage for test data####
#################################################################
pred_test <- predict(train_model,test_data,type = 'response')

Pred_accuracy <-ifelse(pred_test> 0.6, 1,0)
Pred_accuracy_table <- table(Predicted= Pred_accuracy, Real= test_data$Outcome )
Pred_accuracy_table
Percentage_misclass_test <-(1-sum(diag(Pred_accuracy_table))/sum(Pred_accuracy_table))*100
Percentage_misclass_test
