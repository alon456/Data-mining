##################
# Initialization #
##################

##reviews moddle  Buildning

rm(list = ls()) # :) remove all variables from global environment
cat("\014") # clear the screen

##making sure we are on the right folder
setwd("C:/Users/user/Documents/upload")
getwd()

##loading the file for tranning the moddle 
train = read.csv("text_training.csv")

##enforcig rating to be a factor
train$rating = factor(train$rating)

p = runif(1400, 0, 1) # create uniformly distributed random variable in [0,1]in vector p
hist(p, seq(0,1,by=0.1))
training = train[p < 0.7,]  # approximately %70 of Train-set    
testing  = train[p >= 0.7,] # approximately %30 of Train-set    

##randomForest##
library("randomForest")
set.seed(90210)
mdl_rf_v1 = randomForest(rating ~ . -ID -rating,data=training, ntree=100, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v1, newdata=testing, type="prob")
hist(y_hat[,1], main="Histogram of train-set prediction probabilities")
y_hat = (y_hat[,2] > y_hat[,1]) 
# Real popularity values
y = testing$rating
# Calculating the confusion matrix manually
tp = sum(((y_hat==1) & (y==1)))   # true positive
fp = sum(((y_hat==1) & (y==0)))  # false positive
fn = sum(((y_hat==0) & (y==1)))  # false negative
tn = sum(((y_hat==0) & (y==0))) # true negative

pr=100*(tp+tn)/(tp+fp+fn+tn)


rollout = read.csv("reviews_training.csv")

y_hat = predict(mdl_rf_v1, data = rollout , type="prob")
y_hat = (y_hat[,2] > y_hat[,1]) 



rollout_ID = rollout$ID
rollout_BF = rep(y_hat) # replace this line with your predictions!
write.csv(	cbind(ID = rollout_ID ,rating=rollout_BF),
           "reviews_training.csv",
           row.names = FALSE)	



### at this point we used EXCEL to transfer the data to ffp_train.csv



########################################################
##to main work folder:
setwd("C:/Users/user/Documents/upload")
getwd()

Revenu = 170
cost = 32
threshold = 0.1584

##uploding the data

train = read.csv("ffp_train.csv")

##enforcig BUYER_FLAG to be a factor
train$BUYER_FLAG = factor(train$BUYER_FLAG)

###looking at the data
var_na = colSums(is.na(train))
var_na

#' 1. Split the dataset into separated train Train-set and Rollout-set
#' 
ds_tr = train[1:21701,]
ds_ro = train[21702:31000,]
#'
#' 2. Split the Train-set into training and testing sets
#'
set.seed(2017)
p = runif(21701, 0, 1) # create uniformly distributed random variable in [0,1]in vector p
training = ds_tr[p < 0.7,]  # approximately %70 of Train-set    
testing  = ds_tr[p >= 0.7,] # approximately %30 of Train-set    


###(A)Simple Modal for choosing
y_hat=1
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_s1 = sum(((y_hat==1) & (y==1)))   # true positive
fp_s1 = sum(((y_hat==1) & (y==0)))  # false positive
fn_s1 = sum(((y_hat==0) & (y==1)))  # false negative
tn_s1 = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_S1 = tp_s1*Revenu - fp_s1*cost
pr=100*(tp_s1+tn_s1)/(tp_s1+fp_s1+fn_s1+tn_s1)


##(B) randomForest 
library("randomForest")
##(B.1)100 trees
mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG ,data=training ,ntree=100, na.action = na.omit)

###chocing var for finnle moddle
importance(mdl_rf_v2 , type=2)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=testing,type="prob")
colnames(y_hat) <- c("No","Yes")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RF1 = tp_RF*Revenu - fp_RF*cost
pr=100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)

##(B.2) 500 trees

mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG,data=training, ntree=500, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=testing , type="prob")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RF2 = tp_RF*Revenu - fp_RF*cost
pr = 100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)

##(B.3) 500 trees without STATUS_PANTINUM

mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG -STATUS_PANTINUM
                         ,data=training, ntree=500, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=testing , type="prob")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RF3 = tp_RF*Revenu - fp_RF*cost
pr = 100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)


##(C.1)logistic regresion All

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr1 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)



##(C.2)logistic regresion without STATUS_PANTINUM

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG -STATUS_PANTINUM, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr2 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)


##(C.3)logistic regresion without STATUS_GOLD

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG -STATUS_GOLD, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr3 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)


##(C.4)logistic regresion without STATUS_GOLD and STATUS_PANTINUM

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG -STATUS_GOLD -STATUS_PANTINUM, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr4 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)


##(C.5)logistic regresion without STATUS_GOLD and STATUS_PANTINUM and CALL_CENTER_FLAG

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -Rating -BUYER_FLAG -STATUS_GOLD -STATUS_PANTINUM -CALL_CENTER_FLAG, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr5 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)


####NAIVE BAYES 
##(D.1)All var
library (naivebayes)
mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_PANTINUM + STATUS_GOLD 
                             + STATUS_SILVER + NUM_DEAL + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y1 + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_all = tp_nb*Revenu - fp_nb*cost

pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)

###(D.2)without KM_L_Y1

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_PANTINUM + STATUS_GOLD 
                             + STATUS_SILVER + NUM_DEAL + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_all = tp_nb*Revenu - fp_nb*cost

pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)


###(D.3)without STATUS_PANTINU and KM_L_Y1 

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_GOLD 
                             + STATUS_SILVER + NUM_DEAL + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_1 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)




###(D.4)without STATUS_GOLD and STATUS_PANTINUM and KM_L_Y1

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_SILVER + NUM_DEAL 
                             + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_3 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)

###(D.5)without STATUS_GOLD and STATUS_PANTINUM and CALL_CENTER_FLAG and KM_L_Y1

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_SILVER + NUM_DEAL 
                             + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + VIP_SERVICE, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_3 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)

###(D.6)without STATUS_GOLD and STATUS_PANTINUM and CALL_CENTER_FLAG and KM_L_Y1 STATUS_SILVER

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + NUM_DEAL 
                             + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + VIP_SERVICE, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_3 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)


###(D.7)without STATUS_GOLD and STATUS_PANTINUM and CALL_CENTER_FLAG and KM_L_Y1 STATUS_SILVER VIP_SERVICE

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + NUM_DEAL 
                             + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR , data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_3 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)


###(D.8)without STATUS_GOLD and STATUS_PANTINUM and CALL_CENTER_FLAG and KM_L_Y1 STATUS_SILVER VIP_SERVICE DELAYED_CAR

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + NUM_DEAL 
                             + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR , data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_3 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)

###(D.10)without STATUS_GOLD and STATUS_PANTINUM and CALL_CENTER_FLAG and KM_L_Y1 STATUS_SILVER VIP_SERVICE DELAYED_CAR CHANGE_CAR

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + NUM_DEAL 
                             + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             , data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_3 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)


#############with Rating var!!!


##randomForest## 
library("randomForest")
##(E.1)100 trees
mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -BUYER_FLAG,data=training ,ntree=100, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=testing,type="prob")
colnames(y_hat) <- c("No","Yes")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RF1 = tp_RF*Revenu - fp_RF*cost
pr = 100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)

##(E.2) 500 trees

mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -BUYER_FLAG,data=training, ntree=500, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=testing , type="prob")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RF2 = tp_RF*Revenu - fp_RF*cost
pr = 100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)

##(E.3) 500 trees without STATUS_PANTINUM

mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -STATUS_PANTINUM -BUYER_FLAG 
                         ,data=training, ntree=500, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=testing , type="prob")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RF3 = tp_RF*Revenu - fp_RF*cost
pr = 100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)


##(E.4) 500 trees without STATUS_PANTINUM STATUS_GOLD

mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -STATUS_PANTINUM -STATUS_GOLD -BUYER_FLAG 
                         ,data=training, ntree=500, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=testing , type="prob")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RF3 = tp_RF*Revenu - fp_RF*cost
pr = 100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)



##(F.1)logistic regresion All

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -BUYER_FLAG, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr1 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)



##(F.2)logistic regresion without STATUS_PANTINUM

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -BUYER_FLAG -STATUS_PANTINUM, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr2 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)


##(F.3)logistic regresion without STATUS_GOLD

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -BUYER_FLAG -STATUS_GOLD, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr3 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)


##(F.4)logistic regresion without STATUS_GOLD and STATUS_PANTINUM

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -BUYER_FLAG -STATUS_GOLD -STATUS_PANTINUM, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr4 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)


##(F.5)logistic regresion without STATUS_GOLD and STATUS_PANTINUM and CALL_CENTER_FLAG

mdl_lr_v1 = glm(
  BUYER_FLAG ~ . -ID -BUYER_FLAG -STATUS_GOLD -STATUS_PANTINUM  -CALL_CENTER_FLAG, 
  family="binomial" , data=training)

# Predict popularity values
y_hat_lr = predict(mdl_lr_v1, newdata=testing, type="response")
y_hat_lr = (y_hat_lr > threshold)
# Real popularity values
y_lr = testing$BUYER_FLAG
# Calculating the confusion matrix manually
tp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==1)))   # true positive
fp_lr_v1 = sum(((y_hat_lr==1) & (y_lr==0)))  # false positive
fn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==1)))  # false negative
tn_lr_v1 = sum(((y_hat_lr==0) & (y_lr==0))) # true negative


TR_mo_lr5 = tp_lr_v1*Revenu - fp_lr_v1*cost
pr = 100*(tp_lr_v1+tn_lr_v1)/(tp_lr_v1+fp_lr_v1+fn_lr_v1+tn_lr_v1)

####NAIVE BAYES 
##(G.1)All var without KM_L_Y1
library (naivebayes)
mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_PANTINUM + STATUS_GOLD 
                             + STATUS_SILVER + NUM_DEAL + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE +Rating , data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_all = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)



###(G.2)without STATUS_PANTINU + KM_L_Y1

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_GOLD 
                             + STATUS_SILVER + NUM_DEAL + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE + Rating, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_1 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)

###(G.3) without STATUS_GOLD + KM_L_Y1

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_PANTINUM  
                             + STATUS_SILVER + NUM_DEAL + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE + Rating, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_2 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)


###(G.4)without STATUS_GOLD and STATUS_PANTINUM + KM_L_Y1

mdl_naivebayes = naive_bayes(BUYER_FLAG ~ MARKETING_CODE + STATUS_SILVER + NUM_DEAL 
                             + LAST_DEAL + ADVANCE_PURCHASE 
                             + FARE_L_Y1 + FARE_L_Y2 + FARE_L_Y3 + FARE_L_Y4 + FARE_L_Y5
                             + KM_L_Y2 + KM_L_Y3 + KM_L_Y4 + KM_L_Y5 
                             + CHANGE_CAR + DELAYED_CAR + CALL_CENTER_FLAG + VIP_SERVICE + Rating, data=training)

y_hat_nb = predict(mdl_naivebayes, newdata=testing, type="prob")
y_hat_nb = y_hat_nb[,2] 

y_hat_nb = (y_hat_nb > threshold)
y = testing$BUYER_FLAG

tp_nb = sum(((y_hat_nb==TRUE) & (y==1)))   # true positive
fp_nb = sum(((y_hat_nb==TRUE) & (y==0)))  # false positive
fn_nb = sum(((y_hat_nb==FALSE) & (y==1)))  # false negative
tn_nb = sum(((y_hat_nb==FALSE) & (y==0))) # true negative

TR_nb_3 = tp_nb*Revenu - fp_nb*cost
pr = 100*(tp_nb+tn_nb)/(tp_nb+fp_nb+fn_nb+tn_nb)



####Testing: 500 trees in RandomForst without STATUS_PANTINUM


mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -STATUS_PANTINUM -BUYER_FLAG 
                         ,data=ds_tr, ntree=500, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata=ds_ro , type="prob")
y_hat = (y_hat[,2] >threshold) 
# Real popularity values
y = ds_ro$BUYER_FLAG
# Calculating the confusion matrix manually
tp_RF = sum(((y_hat==1) & (y==1)))   # true positive
fp_RF = sum(((y_hat==1) & (y==0)))  # false positive
fn_RF = sum(((y_hat==0) & (y==1)))  # false negative
tn_RF = sum(((y_hat==0) & (y==0))) # true negative

TR_mo_RFT = tp_RF*Revenu - fp_RF*cost
pr = 100*(tp_RF+tn_RF)/(tp_RF+fp_RF+fn_RF+tn_RF)


#########################prepering the rollout file's

##loading the file for training the moddle 

train = read.csv("reviews_training_All.csv")

##enforcig rating to be a factor
train$rating = factor(train$rating)

mdl_rf_v1 = randomForest(rating ~ . -ID -rating,data=train, ntree=100, na.action = na.omit)

rollout = read.csv("reviews_rollout.csv")

rollout$Rating = factor(rollout$rating)

y_hat = predict(mdl_rf_v1, data = rollout , type="prob")
y_hat = (y_hat[,2] > y_hat[,1]) 



rollout_ID = rollout$ID
rollout_BF = rep(y_hat, times = 1, length.out = NA, each = 1) # replace this line with your predictions!
write.csv(	cbind(ID = rollout_ID ,rating=rollout_BF),
           "reviews_ff_rollout.csv",
           row.names = TRUE)	


### at this point we used EXCEL to transfer the data to ffp_rollout_X.csv





####Finale moddle

rollout = read.csv("ffp_rollout_X.csv")

train = read.csv("ffp_train.csv")

train$BUYER_FLAG = factor(train$BUYER_FLAG)

##Random Forest
mdl_rf_v2 = randomForest(BUYER_FLAG ~ . -ID -STATUS_PANTINUM -BUYER_FLAG 
                         ,data=train, ntree=500, na.action = na.omit)

# Predict popularity values
y_hat = predict(mdl_rf_v2, newdata = rollout , type="prob")
y_hat = (y_hat[,2] >threshold) 


rollout_ID = 31001:41000
rollout_BF = rep(y_hat) # replace this line with your predictions!
write.csv(	cbind(ID=rollout_ID, BUYER_FLAG=rollout_BF),
           "recommendations.csv",
           row.names = FALSE)	



