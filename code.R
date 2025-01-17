setwd("~/Downloads/DSA data")
library(class)
library("rpart")
library("rpart.plot")
library(e1071)
library(ROCR)
set.seed(1101)
data = read.csv("diabetes-dataset.csv")
head(data)
dim(data)
data$hypertension = as.factor(data$hypertension)
data$heart_disease = as.factor(data$heart_disease)
data$diabetes = as.factor(data$diabetes)
attach(data)
##gender and diabetes (cate-cate)
table1 = table(gender, diabetes)
tab1 = prop.table(table1, "gender"); tab1
#females have smaller proportion of diabetes people in comparision to males
#other gender doesn't have any diabetes people

##age and diabetes (quant-cate)
boxplot1 = boxplot(age ~ diabetes); boxplot1
#older people are more prone to diabetes

##hypertension and diabetes (cate-cate)
table2 = table(hypertension, diabetes)
tab2 = prop.table(table2, "hypertension"); tab2
or2 = (tab2[1,2]/tab2[1,1])/(tab2[2,2]/tab2[2,1]); or2
#odds of diabetes in hypertension group is 0.1924851 times odds of diabetes in not hypertension group

##heart_disease and diabetes (cate-cate)
table3 = table(heart_disease, diabetes)
tab3 = prop.table(table3, "heart_disease"); tab3
or3 = (tab3[1,2]/tab3[1,1])/(tab3[2,2]/tab2[2,1]); or3
#odds of diabetes in heart disease group is 0.182677 times odds of diabetes in not heart disease group

##smoking history and diabetes (cate-cate)
table4 = table(smoking_history, diabetes)
tab4 = prop.table(table4, "smoking_history"); tab4

##bmi and diabetes (quant-cate)
boxplot2 = boxplot(bmi ~ diabetes); boxplot2
#diabetes people have higher bmi than not diabetes people

#HbA1c_level and diabetes (quant_cate)
boxplot3 = boxplot(HbA1c_level ~ diabetes); boxplot3
#diabetes people have higher HbA1c_level than not diabetes people

##blood_glucose_level and diabetes (quant-cate)
boxplot4 = boxplot(blood_glucose_level ~ diabetes); boxplot4
#diabetes people have higher blood_glucose_level than not diabetes people

##divide into train and test data set
index = sample(1:100000)
train.index = index[1:80000]
test.index = index[81000:100000]
train.data = data[train.index, ]
test.data = data[test.index, ]

##KNN
data_knn <- data[, c("age", "bmi", "HbA1c_level", "blood_glucose_level")]
data_knn$age = scale(data_knn$age)
data_knn$bmi = scale(data_knn$bmi)
data_knn$HbA1c_level = scale(data_knn$HbA1c_level)
data_knn$blood_glucose_level = scale(data_knn$blood_glucose_level)

response_knn <- data[,c("diabetes")]

n = dim(data_knn)[1]

train.index.knn <- sample(1:n)[1:(0.8 * n)]
train_data_knn <- data_knn[train.index.knn,]
test_data_knn <- data_knn[-train.index.knn,]
train_response_knn <- response_knn[train.index.knn]
test_response_knn <- response_knn[-train.index.knn]

library(class)
library(ROCR)

n_folds = 3
folds_j <- sample(rep(1:n_folds, length.out = (0.8*n)))

type1.knn <- numeric()
type2.knn <- numeric()
auc.knn <- numeric()

for (i in c(2, 5, 10, 15, 30, 50, 100, 200, 300)) {
  type1.knn.fold = 0
  type2.knn.fold = 0
  auc.knn.fold = 0
  for (j in 1:3) {
    test_j <- which(folds_j == j)
    train.data.fold <- train_data_knn[-test_j,]
    test.data.fold <- train_data_knn[test_j,]
    train.response.fold <- train_response_knn[-test_j]
    knn.pred <- knn(train.data.fold, test.data.fold, train.response.fold, k = i)
    
    confusion.matrix <- table(train_response_knn[test_j], knn.pred)
    type1.knn.fold = type1.knn.fold + (confusion.matrix[1, 2]/(confusion.matrix[1, 2] + confusion.matrix[1, 1]))
    type2.knn.fold = type2.knn.fold + (confusion.matrix[2, 1]/(confusion.matrix[2, 1] + confusion.matrix[2, 2]))
    
    
    knn.roc.pred <- as.numeric(paste(knn.pred))
    test.response.fold <- train_response_knn[test_j]
    curve <- prediction(knn.roc.pred, test.response.fold)
    auc.knn.fold = auc.knn.fold + performance(curve, "auc")@y.values[[1]]
  }
  type1.knn <- c(type1.knn, type1.knn.fold/3)
  type2.knn <- c(type2.knn, type2.knn.fold/3)
  auc.knn <- c(auc.knn, auc.knn.fold/3)
}

type1.knn
type2.knn
auc.knn

plot(c(2, 5, 10, 15, 30, 50, 100, 200, 300), type1.knn,xlab = "K", col = "blue", pch = 20)
plot(c(2, 5, 10, 15, 30, 50, 100, 200, 300), type2.knn,xlab = "K", col = "red", pch = 20)
plot(c(2, 5, 10, 15, 30, 50, 100, 200, 300), auc.knn,xlab = "K", col = "purple", pch = 20)

#testing
model.knn <- knn(train_data_knn, test_data_knn, train_response_knn, k = 2)
comparison_knn <- table(test_response_knn, model.knn)
comparison_knn

type1.knn.overall <- comparison_knn[1, 2]/(comparison_knn[1, 2] + comparison_knn[1, 1])
type2.knn.overall <- comparison_knn[2, 1]/(comparison_knn[2, 1] + comparison_knn[2, 2])

prob <- knn(train_data_knn, test_data_knn, train_response_knn, k = 2, prob = TRUE)
pred.roc.knn.overall <- 1 - attr(prob, "prob")
roc.knn <- prediction(pred.roc.knn.overall, test_response_knn)
perf.knn <- performance(roc.knn, "tpr", "fpr")
auc.knn.overall <- performance(roc.knn, "auc")@y.values[[1]]
plot(perf.knn, lwd = 2, main = paste("Area under the curve:", round(auc.knn.overall,4)))
abline (a=0, b=1, col ="blue", lty =3)

alpha <- round (as.numeric(unlist(perf.knn@alpha.values)) ,4)
length(alpha) 
fpr <- round(as.numeric(unlist(perf.knn@x.values)) ,4)
tpr <- round(as.numeric(unlist(perf.knn@y.values)) ,4)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))

plot(alpha ,tpr , xlab ="Threshold",
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(alpha ,fpr , xlab ="", ylab ="", axes =F, type ="l", col = "red" )
axis( side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.18 ,0 , "FPR")
text(0.2 ,0.18, "TPR")

type1.knn.overall
type2.knn.overall
auc.knn.overall

##DT
# choose 7 input variables: "gender," "age," "hypertension," heart_disease,"
# "bmi," "HbA1c_level," "blood_glucose_level"
# The training and testing set will be the same as other models

train.index.dt <- train.index.knn
train_data_dt <- data[train.index.dt,]
test_data_dt <- data[-train.index.dt, c("gender", "age", "hypertension", "heart_disease", "bmi", "HbA1c_level", "blood_glucose_level")]
test_data_dt_response <- data[-train.index.dt, c("diabetes")]

library(rpart)
library(rpart.plot)

model_dt <- rpart(diabetes ~ gender + age + hypertension + heart_disease + bmi + HbA1c_level + blood_glucose_level,
                  method = "class", data = train_data_dt, control = rpart.control(minsplit = 6), parms = list(split = 'information'))

rpart.plot(model_dt, type = 4, extra = 2)

dt.pred.prob <- predict(model_dt, newdata = test_data_dt, type = "prob")
dt.pred.class <- predict(model_dt, newdata = test_data_dt, type = "class")

confusion.matrix.dt <- table(test_data_dt_response, dt.pred.class)
confusion.matrix.dt

type1.dt <- confusion.matrix.dt[1, 2]/(confusion.matrix.dt[1, 2] + confusion.matrix.dt[1, 1])
type2.dt <- confusion.matrix.dt[2, 1]/(confusion.matrix.dt[2, 1] + confusion.matrix.dt[2, 2])

score_dt <- dt.pred.prob[, c("1")]
actual_class_dt <- test_data_dt_response
pred.dt <- prediction(score_dt, actual_class_dt)
perf.dt <- performance(pred.dt, "tpr", "fpr")
auc.dt <- performance(pred.dt, "auc")@y.values[[1]]
plot(perf.dt, lwd =2, main = paste("Area under the curve:", round(auc.dt, 4)))
abline(a=0, b=1, col ="blue", lty =3)


type1.dt
type2.dt
auc.dt

##NB
# choose 7 input variables: "gender," "age," "hypertension," heart_disease,"
# "bmi," "HbA1c_level," "blood_glucose_level"
# The training and testing set will be the same as other models

train.index.nb <- train.index.knn
train_data_nb <- data[train.index.nb,]
test_data_nb <- data[-train.index.nb, c("gender", "age", "hypertension", "heart_disease", "bmi", "HbA1c_level", "blood_glucose_level")]
test_data_nb_response <- data[-train.index.nb, c("diabetes")]

library(e1071)

model_nb <- naiveBayes(diabetes ~ gender + age + hypertension + heart_disease + bmi + HbA1c_level + blood_glucose_level, train_data_nb)

nb.pred.raw <- predict(model_nb, newdata = test_data_nb, type = "raw")
nb.pred.class <- predict(model_nb, newdata = test_data_nb, type = "class")

confusion.matrix.nb <- table(test_data_nb_response, nb.pred.class)
confusion.matrix.nb

type1.nb <- confusion.matrix.nb[1, 2]/(confusion.matrix.nb[1, 2] + confusion.matrix.nb[1, 1])
type2.nb <- confusion.matrix.nb[2, 1]/(confusion.matrix.nb[2, 1] + confusion.matrix.nb[2, 2])

score_nb <- nb.pred.raw[, c("1")]
actual_class_nb <- test_data_nb_response
pred.nb <- prediction(score_nb, actual_class_nb)
perf.nb <- performance(pred.nb, "tpr", "fpr")
auc.nb <- performance(pred.nb, "auc")@y.values[[1]]
plot(perf.nb, lwd =2, main = paste("Area under the curve:", round(auc.nb, 4)))
abline(a=0, b=1, col ="blue", lty =3)


type1.nb
type2.nb
auc.nb

##Logistic Regression
#choose 7 input variables: "gender," "age," "hypertension," heart_disease,"
# "bmi," "HbA1c_level," "blood_glucose_level"
# The training and testing set will be the same as other models

train.index.log <- train.index.knn
train_data_log <- data[train.index.log,]
test_data_log <- data[-train.index.log, c("gender", "age", "hypertension", "heart_disease", "bmi", "HbA1c_level", "blood_glucose_level")]
test_data_log_response <- data[-train.index.log, c("diabetes")]

model_log <- glm(diabetes ~ gender + age + hypertension + heart_disease + bmi + 
                   HbA1c_level + blood_glucose_level, data = train_data_log, 
                   family = binomial(link ="logit"))

log.pred.response <- predict(model_log, newdata = test_data_log, type = "response")

score_log <- log.pred.response
actual_class_log <- test_data_log_response
pred.log <- prediction(score_log, actual_class_log)
perf.log <- performance(pred.log, "tpr", "fpr")
auc.log <- performance(pred.log, "auc")@y.values[[1]]
plot(perf.log, lwd =2, main = paste("Area under the curve:", round(auc.log, 4)))
abline(a=0, b=1, col ="blue", lty =3)

# Visualize how the threshold changes will change TPR and FPR
threshold <- round(as.numeric(unlist(perf.log@alpha.values)) ,4)
fpr <- round(as.numeric(unlist(perf.log@x.values)) ,4)
tpr <- round(as.numeric(unlist(perf.log@y.values)) ,4)
par(mar = c(5 ,5 ,2 ,5))
plot(threshold ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par(new ="True")
plot(threshold ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis(side =4)
mtext(side =4, line =3, "False positive rate")
text(0.4 ,0.05 , "FPR")
text(0.6 ,0.65 , "TPR")

# Choosing threshold = 0.1 will be ideal
log.pred.class <- ifelse(log.pred.response > 0.1, "1", "0")

confusion.matrix.log <- table(test_data_log_response, log.pred.class)
confusion.matrix.log

type1.log <- confusion.matrix.log[1, 2]/(confusion.matrix.log[1, 2] + confusion.matrix.log[1, 1])
type2.log <- confusion.matrix.log[2, 1]/(confusion.matrix.log[2, 1] + confusion.matrix.log[2, 2])



type1.log
type2.log
auc.log

















