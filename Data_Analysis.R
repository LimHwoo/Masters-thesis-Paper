################### Data correlation
# install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
# install.packages("tree")
# install.packages("rlist")
# install.packages("pROC")
# install.packages("class")
# install.packages("randomForest")
# install.packages("rpart.plot")
# install.packages("unbalanced")
# install.packages("MLmetrics)
# library(unbalanced)
# library(bnclassify)
# library(dplyr)
# library(rlist)
# library(graph)
# library(pROC)
# library(rpart)
# library(rpart.plot)
# library(class)
# library(randomForest)
# Sys.setenv('R_MAX_VSIZE'=32000000000)
#install.packages("progress")
#library(progress)

rm(list=ls())
par(mfrow=c(2,3))
source('/Users/hyunwoo/Desktop/연구/Metrics.R')
source('/Users/hyunwoo/Desktop/연구/cl_learning.R')

eye = read.csv("/Users/hyunwoo/Desktop/Eye_data/eye_data.csv",header=T)
eye$AGE_G = ifelse(eye$AGE_G<5,0,ifelse(eye$AGE_G<15,1,ifelse(eye$AGE_G<22,2,3)))
for(i in 1:length(eye)){
  eye[,i] = as.factor(eye[,i])
}


blood = read.csv("/Users/hyunwoo/Desktop/blood_data/blood_data.csv",header=T)
blood$HGB = ifelse(blood$SEX==1,ifelse(blood$HGB<12,0,ifelse(blood$HGB<13,1,ifelse(blood$HGB<=16.5,2,3))),blood$HGB)
blood$HGB = ifelse(blood$SEX==2,ifelse(blood$HGB<10,0,ifelse(blood$HGB<12,1,ifelse(blood$HGB<=15.5,2,3))),blood$HGB)
blood$TCHOL = ifelse(blood$TCHOL<200,0,ifelse(blood$TCHOL<240,1,2))
blood$TG = ifelse(blood$TG<150,0,ifelse(blood$TG<200,1,ifelse(blood$TG<500,2,3)))
blood$HDL = ifelse(blood$HDL<40,0,ifelse(blood$HDL<60,1,2))
blood$AGE_G = ifelse(blood$AGE_G<5,0,ifelse(blood$AGE_G<15,1,ifelse(blood$AGE_G<22,2,3)))

for(i in 1:length(blood)){
  blood[,i] = as.factor(blood[,i])
}


breast = read.table("/Users/hyunwoo/Desktop/breast_cancer/breast-cancer.data",sep=",")
colnames(breast) = c("Class","age","menopause","tumor_size","inv_nodes","node_caps","deg_malig","berast","breast_quad","irradiat")
na_index=c()
for(i in 1:NROW(breast)){
  if(sum(breast[i,]=='?')>=1){
    na_index=c(na_index,i)
  }
}
breast = breast[-na_index,]

for(i in 1:length(breast)){
  breast[,i]=as.factor(breast[,i])
}
for(i in 1:length(breast)){
  levels(breast[,i])=c(0:(length(levels(breast[,i]))-1))
}
breast$age = as.integer(breast$age)
breast$age = ifelse(breast$age<4,0,ifelse(breast$age<5,1,2))
breast$age = as.factor(breast$age)

# install.packages("DescTools")
# install.packages("psych")
# install.packages("plot.matrix")
# library(DescTools)
# library(psych)
# library(plot.matrix)

################################# 1에가까울수록 연관
################################# 
breast_cor = matrix(0,nrow=10,ncol=10)
for(i in 1:10){
  for(j in 1:10){
    breast_cor[i,j] = CramerV(breast[,i],breast[,j])
  
  }
}
colnames(breast_cor) = colnames(breast)
rownames(breast_cor) = colnames(breast)
plot(breast_cor)

eye_cor = matrix(0,nrow=8,ncol=8)
for(i in 1:8){
  for(j in 1:8){
    eye_cor[i,j] = CramerV(eye[,i],eye[,j])
    
  }
}
colnames(eye_cor) = colnames(eye)
rownames(eye_cor) = colnames(eye)
plot(eye_cor)

blood_cor = matrix(0,nrow=9,ncol=9)
for(i in 1:9){
  for(j in 1:9){
    blood_cor[i,j] = CramerV(blood[,i],blood[,j])
    
  }
}
colnames(blood_cor) = colnames(blood)
rownames(blood_cor) = colnames(blood)
plot(blood_cor)
################################################################ with Class
eye1= eye
blood1 = blood
breast1 = breast

blood1$STK = as.factor(paste(blood$STK,blood$SEX,blood$AGE_G,sep=''))
blood1 = blood1 %>% select(-c('SEX',"AGE_G"))
eye1$DR = as.factor(paste(eye$DR,eye$SEX,eye$AGE_G,sep=''))
eye1 = eye1 %>% select(-c("SEX","AGE_G"))
breast1$Class = as.factor(paste(breast$Class,breast$age))
breast1 = breast1 %>% select( -"age")

breast1_cor = matrix(0,nrow=9,ncol=9)
for(i in 1:9){
  for(j in 1:9){
    breast1_cor[i,j] = CramerV(breast1[,i],breast1[,j])
    
  }
}
colnames(breast1_cor) = colnames(breast1)
rownames(breast1_cor) = colnames(breast1)
plot(breast1_cor)

eye1_cor = matrix(0,nrow=length(eye1),ncol=length(eye1))
for(i in 1:length(eye1)){
  for(j in 1:length(eye1)){
    eye1_cor[i,j] = CramerV(eye1[,i],eye1[,j])
    
  }
}
colnames(eye1_cor) = colnames(eye1)
rownames(eye1_cor) = colnames(eye1)
plot(eye1_cor)

blood1_cor = matrix(0,nrow=7,ncol=7)
for(i in 1:7){
  for(j in 1:7){
    blood1_cor[i,j] = CramerV(blood1[,i],blood1[,j])
    
  }
}
colnames(blood1_cor) = colnames(blood1)
rownames(blood1_cor) = colnames(blood1)
plot(blood1_cor)

# ########################################################################### with smote
# 
# eye_temp<- ubSMOTE(X= eye[,-8],  Y= eye$DR,
#                perc.over = 200, perc.under = 150,k = 5, verbose = FALSE)
# eye_smote<-cbind(eye_temp$X, eye_temp$Y)
# colnames(eye_smote)[8]<- "DR"
# 
# 
# blood_temp<- ubSMOTE(X= blood[,-9],  Y= blood$STK,
#                perc.over = 100, perc.under = 200,k = 5, verbose = FALSE)
# blood_smote<-cbind(blood_temp$X, blood_temp$Y)
# colnames(blood_smote)[9]<- "STK"
# 
# breast_temp<- ubSMOTE(X= breast[,-1],  Y= breast$Class,
#                perc.over = 100, perc.under = 200 , k = 7, verbose = FALSE)
# breast_smote<-cbind(breast_temp$X, breast_temp$Y)
# colnames(breast_smote)[10]<- "Class"
# 
# breast_smote_cor = matrix(0,nrow=10,ncol=10)
# for(i in 1:10){
#   for(j in 1:10){
#     breast_smote_cor[i,j] = CramerV(breast_smote[,i],breast_smote[,j])
#     
#   }
# }
# colnames(breast_smote_cor) = colnames(breast_smote)
# rownames(breast_smote_cor) = colnames(breast_smote)
# plot(breast_smote_cor)
# 
# eye_smote_cor = matrix(0,nrow=8,ncol=8)
# for(i in 1:8){
#   for(j in 1:8){
#     eye_smote_cor[i,j] = CramerV(eye_smote[,i],eye_smote[,j])
#     
#   }
# }
# colnames(eye_smote_cor) = colnames(eye_smote)
# rownames(eye_smote_cor) = colnames(eye_smote)
# plot(eye_smote_cor)
# 
# blood_smote_cor = matrix(0,nrow=9,ncol=9)
# for(i in 1:9){
#   for(j in 1:9){
#     blood_smote_cor[i,j] = CramerV(blood_smote[,i],blood_smote[,j])
#     
#   }
# }
# colnames(blood_smote_cor) = colnames(blood_smote)
# rownames(blood_smote_cor) = colnames(blood_smote)
# plot(blood_smote_cor)
# 
# ########################################### with smote + class
# 
# eye_smote1= eye_smote
# blood_smote1 = blood_smote
# breast_smote1 = breast_smote
# 
# blood_smote1$STK = as.factor(paste(blood_smote$STK,blood_smote$SEX,blood_smote$AGE_G,sep=''))
# blood_smote1 = blood_smote1 %>% select(-c('SEX',"AGE_G"))
# eye_smote1$DR = as.factor(paste(eye_smote$DR,eye_smote$SEX,eye_smote$AGE_G,sep=''))
# eye_smote1 = eye_smote1 %>% select(-c("SEX","AGE_G"))
# breast_smote1$Class = as.factor(paste(breast_smote$Class,breast_smote$age))
# breast_smote1 = breast_smote1 %>% select( -"age")
# 
# breast_smote1_cor = matrix(0,nrow=9,ncol=9)
# for(i in 1:9){
#   for(j in 1:9){
#     breast_smote1_cor[i,j] = CramerV(breast_smote1[,i],breast_smote1[,j])
#     
#   }
# }
# colnames(breast_smote1_cor) = colnames(breast_smote1)
# rownames(breast_smote1_cor) = colnames(breast_smote1)
# plot(breast_smote1_cor)
# 
# eye_smote1_cor = matrix(0,nrow=length(eye_smote1),ncol=length(eye_smote1))
# for(i in 1:length(eye_smote1)){
#   for(j in 1:length(eye_smote1)){
#     eye_smote1_cor[i,j] = CramerV(eye_smote1[,i],eye_smote1[,j])
#     
#   }
# }
# colnames(eye_smote1_cor) = colnames(eye_smote1)
# rownames(eye_smote1_cor) = colnames(eye_smote1)
# plot(eye_smote1_cor)
# 
# blood_smote1_cor = matrix(0,nrow=7,ncol=7)
# for(i in 1:7){
#   for(j in 1:7){
#     blood_smote1_cor[i,j] = CramerV(blood_smote1[,i],blood_smote1[,j])
#     
#   }
# }
# colnames(blood_smote1_cor) = colnames(blood_smote1)
# rownames(blood_smote1_cor) = colnames(blood_smote1)
# plot(blood_smote1_cor)
# 
# ########################################################################### Smote_chisq
# alpha = 0.05
# breast_smote_chisq = matrix(0,nrow=9,ncol=9)
# for(i in 1:9){
#   for(j in 1:9){
#     breast_smote_chisq[i,j] = ifelse(ci.test(breast_smote[,i],breast_smote[,j],breast_smote$Class,test='x2')$p.value<alpha,
#                                      1,0)
# 
#   }
# }
# colnames(breast_smote_chisq) = colnames(breast_smote)[1:9]
# rownames(breast_smote_chisq) = colnames(breast_smote)[1:9]
# plot(breast_smote_chisq)
# 
# eye_smote_chisq = matrix(0,nrow=7,ncol=7)
# for(i in 1:7){
#   for(j in 1:7){
#     
#     eye_smote_chisq[i,j] = ifelse(ci.test(eye_smote[,i],eye_smote[,j],eye_smote$DR,test='x2')$p.value<alpha,
#                                   1,0)
#   }
# }
# 
# colnames(eye_smote_chisq) = colnames(eye_smote)[1:7]
# rownames(eye_smote_chisq) = colnames(eye_smote)[1:7]
# plot(eye_smote_chisq)
# 
# blood_smote_chisq = matrix(0,nrow=8,ncol=8)
# for(i in 1:8){
#   for(j in 1:8){
#     blood_smote_chisq[i,j] = ifelse(ci.test(blood_smote[,i],blood_smote[,j],blood_smote$STK,test='x2')$p.value<alpha,
#                                   1,0)
#   }
# }
# 
# colnames(blood_smote_chisq) = colnames(blood_smote)[1:8]
# rownames(blood_smote_chisq) = colnames(blood_smote)[1:8]
# plot(blood_smote_chisq)
# 
###################################################################### chisq
alpha=0.05
breast_chisq = matrix(0,nrow=9,ncol=9)
for(i in 2:10){
  for(j in 2:10){
    breast_chisq[i-1,j-1] = ifelse(ci.test(breast[,i],breast[,j],breast$Class,test='x2')$p.value<alpha,
                                     1,0)
    
  }
}
colnames(breast_chisq) = colnames(breast )[2:10]
rownames(breast_chisq) = colnames(breast )[2:10]
plot(breast_chisq)

eye_chisq = matrix(0,nrow=7,ncol=7)
for(i in 1:7){
  for(j in 1:7){
        eye_chisq[i,j] = ifelse(ci.test(eye[,i],eye[,j],eye$DR,test='x2')$p.value<alpha,
                                  1,0)
  }
}

colnames(eye_chisq) = colnames(eye)[1:7]
rownames(eye_chisq) = colnames(eye)[1:7]
plot(eye_chisq)

blood_chisq = matrix(0,nrow=8,ncol=8)
for(i in 1:8){
  for(j in 1:8){
    blood_chisq[i,j] = ifelse(ci.test(blood[,i],blood[,j],blood$STK,test='x2')$p.value<alpha,
                                    1,0)
  }
}

colnames(blood_chisq) = colnames(blood)[1:8]
rownames(blood_chisq) = colnames(blood)[1:8]
plot(blood_chisq)

############################################################## class + chisqr
breast1_chisq = matrix(0,nrow=8,ncol=8)
for(i in 2:9){
  for(j in 2:9){
    breast1_chisq[i-1,j-1] = ifelse(ci.test(breast1[,i],breast1[,j],breast1$Class,test='x2')$p.value<alpha,
                                   1,0)
    
  }
}
colnames(breast1_chisq) = colnames(breast1 )[2:9]
rownames(breast1_chisq) = colnames(breast1 )[2:9]
plot(breast1_chisq)

eye1_chisq = matrix(0,nrow=5,ncol=5)
for(i in 1:5){
  for(j in 1:5){
    eye1_chisq[i,j] = ifelse(ci.test(eye1[,i],eye1[,j],eye1$DR,test='x2')$p.value<alpha,
                            1,0)
  }
}

colnames(eye1_chisq) = colnames(eye1)[1:5]
rownames(eye1_chisq) = colnames(eye1)[1:5]
plot(eye1_chisq)

blood1_chisq = matrix(0,nrow=6,ncol=6)
for(i in 1:6){
  for(j in 1:6){
    blood1_chisq[i,j] = ifelse(ci.test(blood1[,i],blood1[,j],blood1$STK,test='x2')$p.value<alpha,
                              1,0)
  }
}
colnames(blood1_chisq) = colnames(blood1)[1:6]
rownames(blood1_chisq) = colnames(blood1)[1:6]
plot(blood1_chisq)

#####################################################3 성능비교 
######################## 1.eye
pb <- progress_bar$new(
  format = "진행상황 : [:bar] :percent, 완료예정시간 : :eta , 총 소요시간 : :elapsedfull",total = 100)
eye_sum = 0
blood_sum = 0
iter = sample(1:10000,100,replace=F)
for(i in 1:100){
set.seed(iter)
pb$tick()
Sys.sleep(0.01)
data = eye
class='DR'
newindex = sample(1:NROW(data),NROW(data)/10,replace=F)
testdata = data[newindex,]
traindata = data[-newindex,]
temp<- ubSMOTE(X= traindata[,-8],  Y= traindata$DR,
               perc.over = 100, perc.under = 200,k = 5, verbose = FALSE)
data_smote<-cbind(temp$X, temp$Y)
colnames(data_smote)[8]<- "DR"
traindata = data_smote
traindata$class = as.factor(paste(traindata$DR,traindata$SEX,traindata$AGE_G,sep=''))
testdata$class = as.factor(paste(testdata$DR,testdata$SEX,testdata$AGE_G,sep=''))

result_eye = cl_algorithm(class=class,data=traindata,root_='information',del='class')
results_eye=learn_parameter(result_eye$data,result_eye,class=class,alpha=1) 
fit_eye = bnc('tan_cl','DR',traindata[,c(-9)],smooth=1)
fit_eye$.params=results_eye
end_eyetime1 = Sys.time()

start_eyetime2 = Sys.time()
result_eye2 = cl_algorithm(class=class,data=traindata,root_='information',del=c('SEX','AGE_G','class'))
results_eye2=learn_parameter(result_eye2$data,result_eye2,class=class,alpha=1) 
fit_eye2 = bnc('tan_cl','DR',traindata[,c(-1,-2,-9)],smooth=1)
fit_eye2$.params=results_eye2
end_eyetime2 = Sys.time()

start_eyetime3 = Sys.time()
result_eye3 = cl_algorithm(class='class',data=traindata,root_='information',del=c('SEX','AGE_G','DR'))
results_eye3=learn_parameter(result_eye3$data,result_eye3,class='class',alpha=1) 
fit_eye3 = bnc('tan_cl','class',traindata[,c(-1,-2,-8)],smooth=1)
fit_eye3$.params=results_eye3
end_eyetime3 = Sys.time()

start_eyetime4 = Sys.time()
result_eye4 = cl_algorithm(class='class',data=traindata,root_=result_eye2$information[1,1],
                           del=c('SEX','AGE_G','DR'))
results_eye4=learn_parameter(result_eye4$data,result_eye4,class='class',alpha=1) 
fit_eye3_1 = bnc('tan_cl','class',traindata[,c(-1,-2,-8)],smooth=1)
fit_eye3_1$.params=results_eye4
end_eyetime4 = Sys.time()

pred_eye=predict(fit_eye,testdata)
eye_prob = predict(fit_eye,testdata,prob=T)
eye_acc = accuracy(pred_eye,testdata$DR)
eye_tab=table(pred_eye,testdata$DR,dnn=c("pred","real"))
eye_sens = Sensitivity(testdata$DR,pred_eye)
eye_spec = Specificity(testdata$DR,pred_eye)
eye_precision = Precision(testdata$DR,pred_eye)
eye_f1 = F1_Score(testdata$DR,pred_eye)
eye_roc = roc(testdata$DR,eye_prob[,2],quiet=T)$auc[1] ################### ROC / AUC

pred_eye2=predict(fit_eye2,testdata)
eye_prob2 = predict(fit_eye2,testdata,prob=T)
eye_acc2 = accuracy(pred_eye2,testdata$DR)
eye_tab2 = table(pred_eye2,testdata$DR,dnn=c("pred","real"))
eye_sens2 = Sensitivity(testdata$DR,pred_eye2)
eye_spec2 = Specificity(testdata$DR,pred_eye2)
eye_precision2 = Precision(testdata$DR,pred_eye2)
eye_f12 = F1_Score(testdata$DR,pred_eye2)
eye_roc2 = roc(testdata$DR,eye_prob2[,2],quiet=T)$auc[1] ################### ROC / AUC

pred_eye3=predict(fit_eye3,testdata)
pred_eye3 = as.factor(ifelse(as.integer(pred_eye3)<9,0,1))
eye_prob3 = predict(fit_eye3,testdata,prob=T)
eye_acc3 = accuracy(pred_eye3,testdata$DR)
eye_tab3 = table(pred_eye3,testdata$DR,dnn=c("pred","real"))
eye_sens3 = Sensitivity(testdata$DR,pred_eye3)
eye_spec3 = Specificity(testdata$DR,pred_eye3)
eye_precision3 = Precision(testdata$DR,pred_eye3)
eye_f13 = F1_Score(testdata$DR,pred_eye3)
eye_roc3 = roc(testdata$DR,apply(eye_prob3[,9:16],1,sum),quiet=T)$auc[1] ################### ROC / AUC

pred_eye3_1=predict(fit_eye3_1,testdata)
pred_eye3_1 = as.factor(ifelse(as.integer(pred_eye3_1)<9,0,1))
eye_prob3_1 = predict(fit_eye3_1,testdata,prob=T)
eye_acc3_1 = accuracy(pred_eye3_1,testdata$DR)
eye_tab3_1 = table(pred_eye3_1,testdata$DR,dnn=c("pred","real"))
eye_sens3_1 = Sensitivity(testdata$DR,pred_eye3_1)
eye_spec3_1 = Specificity(testdata$DR,pred_eye3_1)
eye_precision3_1 = Precision(testdata$DR,pred_eye3_1)
eye_f13_1 = F1_Score(testdata$DR,pred_eye3_1)
eye_roc3_1 = roc(testdata$DR,apply(eye_prob3_1[,9:16],1,sum),quiet=T)$auc[1] ################### ROC / AUC

eye_naive = bnc('nb','class',traindata[,-c(1,2,8)],smooth=1)
eye_naive_pred=predict(eye_naive,testdata)
eye_naive_pred = as.factor(ifelse(as.integer(eye_naive_pred)<9,0,1))
eye_naive_prob = predict(eye_naive,testdata,prob=T)
eye_naive_tab = table(eye_naive_pred,testdata$DR)

eye_naive_sens = Sensitivity(testdata$DR,eye_naive_pred)
eye_naive_spec = Specificity(testdata$DR,eye_naive_pred)
eye_naive_precision = Precision(testdata$DR,eye_naive_pred)
eye_naive_f1 = F1_Score(testdata$DR,eye_naive_pred)

eye_naive_acc = accuracy(eye_naive_pred,testdata$DR)  #####################3 Acc of eye_naive
eye_naive_roc = roc(testdata$DR,apply(eye_naive_prob[,9:16],1,sum),quiet=T)$auc[1] ################# AUC of eye_naive

eye_naive1 = bnc('nb','DR',traindata[,-9],smooth=1)
eye_naive1_pred=predict(eye_naive1,testdata)
eye_naive1_prob = predict(eye_naive1,testdata,prob=T)
eye_naive1_tab = table(eye_naive1_pred,testdata$DR)

eye_naive1_sens = Sensitivity(testdata$DR,eye_naive1_pred)
eye_naive1_spec = Specificity(testdata$DR,eye_naive1_pred)
eye_naive1_precision = Precision(testdata$DR,eye_naive1_pred)
eye_naive1_f1 = F1_Score(testdata$DR,eye_naive1_pred)

eye_naive1_acc = accuracy(eye_naive1_pred,testdata$DR)  #####################3 Acc of eye_naive1
eye_naive1_roc = roc(testdata$DR,eye_naive1_prob[,2],quiet=T)$auc[1] ################# AUC of eye_naive1

eye_result = c(eye_acc,eye_roc,eye_sens,eye_spec,eye_precision,eye_f1)
eye_result2 = c(eye_acc2,eye_roc2,eye_sens2,eye_spec2,eye_precision2,eye_f12)
eye_result3 = c(eye_acc3,eye_roc3,eye_sens3,eye_spec3,eye_precision3,eye_f13)
eye_result3_1 = c(eye_acc3_1,eye_roc3_1,eye_sens3_1,eye_spec3_1,eye_precision3_1,eye_f13_1)
eye_naive1_result = c(eye_naive1_acc,eye_naive1_roc,eye_naive1_sens,eye_naive1_spec,eye_naive1_precision,eye_naive1_f1)
eye_naive_result = c(eye_naive_acc,eye_naive_roc,eye_naive_sens,eye_naive_spec,eye_naive_precision,eye_naive_f1)
eye_final_result = round(rbind(eye_result,eye_result2,eye_result3,eye_result3_1,
                           eye_naive1_result,eye_naive_result),3)
colnames(eye_final_result) = c("Acc","AUC","Sens","Spec","Precision","F1")
eye_sum = eye_sum + eye_final_result
########################################################2.blood
data = blood
class='STK'
newindex = sample(1:NROW(data),NROW(data)/10,replace=F)
testdata = data[newindex,]
traindata = data[-newindex,]
temp<- ubSMOTE(X= traindata[,-9],  Y= traindata$STK,
               perc.over = 100, perc.under = 200,k = 5, verbose = FALSE)
data_smote<-cbind(temp$X, temp$Y)
colnames(data_smote)[9]<- "STK"
traindata = data_smote
traindata$class = as.factor(paste(traindata$STK,traindata$SEX,traindata$AGE_G,sep=''))
testdata$class = as.factor(paste(testdata$STK,testdata$SEX,testdata$AGE_G,sep=''))

result_blood = cl_algorithm(class=class,data=traindata,root_='information',del='class')
results_blood=learn_parameter(result_blood$data,result_blood,class=class,alpha=1) 
fit_blood = bnc('tan_cl','STK',traindata[,c(-10)],smooth=1)
fit_blood$.params=results_blood
end_bloodtime1 = Sys.time()

start_bloodtime2 = Sys.time()
result_blood2 = cl_algorithm(class=class,data=traindata,root_='information',del=c('SEX','AGE_G','class'))
results_blood2=learn_parameter(result_blood2$data,result_blood2,class=class,alpha=1) 
fit_blood2 = bnc('tan_cl','STK',traindata[,c(-1,-2,-10)],smooth=1)
fit_blood2$.params=results_blood2
end_bloodtime2 = Sys.time()

start_bloodtime3 = Sys.time()
result_blood3 = cl_algorithm(class='class',data=traindata,root_='information',del=c('SEX','AGE_G','STK'))
results_blood3=learn_parameter(result_blood3$data,result_blood3,class='class',alpha=1) 
fit_blood3 = bnc('tan_cl','class',traindata[,c(-1,-2,-9)],smooth=1)
fit_blood3$.params=results_blood3
end_bloodtime3 = Sys.time()

start_bloodtime4 = Sys.time()
result_blood4 = cl_algorithm(class='class',data=traindata,root_=result_blood2$information[1,1],
                           del=c('SEX','AGE_G','STK'))
results_blood4=learn_parameter(result_blood4$data,result_blood4,class='class',alpha=1) 
fit_blood3_1 = bnc('tan_cl','class',traindata[,c(-1,-2,-9)],smooth=1)
fit_blood3_1$.params=results_blood4
end_bloodtime4 = Sys.time()

pred_blood=predict(fit_blood,testdata)
blood_prob = predict(fit_blood,testdata,prob=T)
blood_acc = accuracy(pred_blood,testdata$STK)
blood_tab=table(pred_blood,testdata$STK,dnn=c("pred","real"))
blood_sens = Sensitivity(testdata$STK,pred_blood)
blood_spec = Specificity(testdata$STK,pred_blood)
blood_precision = Precision(testdata$STK,pred_blood)
blood_f1 = F1_Score(testdata$STK,pred_blood)
blood_roc = roc(testdata$STK,blood_prob[,2],quiet=T)$auc[1] ################### ROC / AUC

pred_blood2=predict(fit_blood2,testdata)
blood_prob2 = predict(fit_blood2,testdata,prob=T)
blood_acc2 = accuracy(pred_blood2,testdata$STK)
blood_tab2 = table(pred_blood2,testdata$STK,dnn=c("pred","real"))
blood_sens2 = Sensitivity(testdata$STK,pred_blood2)
blood_spec2 = Specificity(testdata$STK,pred_blood2)
blood_precision2 = Precision(testdata$STK,pred_blood2)
blood_f12 = F1_Score(testdata$STK,pred_blood2)
blood_roc2 = roc(testdata$STK,blood_prob2[,2],quiet=T)$auc[1] ################### ROC / AUC

pred_blood3=predict(fit_blood3,testdata)
pred_blood3 = as.factor(ifelse(as.integer(pred_blood3)<9,0,1))
blood_prob3 = predict(fit_blood3,testdata,prob=T)
blood_acc3 = accuracy(pred_blood3,testdata$STK)
blood_tab3 = table(pred_blood3,testdata$STK,dnn=c("pred","real"))
blood_sens3 = Sensitivity(testdata$STK,pred_blood3)
blood_spec3 = Specificity(testdata$STK,pred_blood3)
blood_precision3 = Precision(testdata$STK,pred_blood3)
blood_f13 = F1_Score(testdata$STK,pred_blood3)
blood_roc3 = roc(testdata$STK,apply(blood_prob3[,9:16],1,sum),quiet=T)$auc[1] ################### ROC / AUC

pred_blood3_1=predict(fit_blood3_1,testdata)
pred_blood3_1 = as.factor(ifelse(as.integer(pred_blood3_1)<9,0,1))
blood_prob3_1 = predict(fit_blood3_1,testdata,prob=T)
blood_acc3_1 = accuracy(pred_blood3_1,testdata$STK)
blood_tab3_1 = table(pred_blood3_1,testdata$STK,dnn=c("pred","real"))
blood_sens3_1 = Sensitivity(testdata$STK,pred_blood3_1)
blood_spec3_1 = Specificity(testdata$STK,pred_blood3_1)
blood_precision3_1 = Precision(testdata$STK,pred_blood3_1)
blood_f13_1 = F1_Score(testdata$STK,pred_blood3_1)
blood_roc3_1 = roc(testdata$STK,apply(blood_prob3_1[,9:16],1,sum),quiet=T)$auc[1] ################### ROC / AUC

blood_naive = bnc('nb','class',traindata[,-c(1,2,9)],smooth=1)
blood_naive_pred=predict(blood_naive,testdata)
blood_naive_pred = as.factor(ifelse(as.integer(blood_naive_pred)<9,0,1))
blood_naive_prob = predict(blood_naive,testdata,prob=T)
blood_naive_tab = table(blood_naive_pred,testdata$STK)

blood_naive_sens = Sensitivity(testdata$STK,blood_naive_pred)
blood_naive_spec = Specificity(testdata$STK,blood_naive_pred)
blood_naive_precision = Precision(testdata$STK,blood_naive_pred)
blood_naive_f1 = F1_Score(testdata$STK,blood_naive_pred)

blood_naive_acc = accuracy(blood_naive_pred,testdata$STK)  #####################3 Acc of blood_naive
blood_naive_roc = roc(testdata$STK,apply(blood_naive_prob[,9:16],1,sum),quiet=T)$auc[1] ################# AUC of blood_naive

blood_naive1 = bnc('nb','STK',traindata[,-10],smooth=1)
blood_naive1_pred=predict(blood_naive1,testdata)
blood_naive1_prob = predict(blood_naive1,testdata,prob=T)
blood_naive1_tab = table(blood_naive1_pred,testdata$STK)

blood_naive1_sens = Sensitivity(testdata$STK,blood_naive1_pred)
blood_naive1_spec = Specificity(testdata$STK,blood_naive1_pred)
blood_naive1_precision = Precision(testdata$STK,blood_naive1_pred)
blood_naive1_f1 = F1_Score(testdata$STK,blood_naive1_pred)

blood_naive1_acc = accuracy(blood_naive1_pred,testdata$STK)  #####################3 Acc of blood_naive1
blood_naive1_roc = roc(testdata$STK,blood_naive1_prob[,2],quiet=T)$auc[1] ################# AUC of blood_naive1

blood_result = c(blood_acc,blood_roc,blood_sens,blood_spec,blood_precision,blood_f1)
blood_result2 = c(blood_acc2,blood_roc2,blood_sens2,blood_spec2,blood_precision2,blood_f12)
blood_result3 = c(blood_acc3,blood_roc3,blood_sens3,blood_spec3,blood_precision3,blood_f13)
blood_result3_1 = c(blood_acc3_1,blood_roc3_1,blood_sens3_1,blood_spec3_1,blood_precision3_1,blood_f13_1)
blood_naive1_result = c(blood_naive1_acc,blood_naive1_roc,blood_naive1_sens,blood_naive1_spec,blood_naive1_precision,blood_naive1_f1)
blood_naive_result = c(blood_naive_acc,blood_naive_roc,blood_naive_sens,blood_naive_spec,blood_naive_precision,blood_naive_f1)
blood_final_result = round(rbind(blood_result,blood_result2,blood_result3,blood_result3_1,
                               blood_naive1_result,blood_naive_result),3)

colnames(blood_final_result) = c("Acc","AUC","Sens","Spec","Precision","F1")
blood_sum = blood_sum + blood_final_result
}


############################################################# 3.UCI
class='Class'
breast_sum = 0
pb <- progress_bar$new(
  format = "진행상황 : [:bar] :percent, 완료예정시간 : :eta , 총 소요시간 : :elapsedfull",total = 1000)

for(i in sample(1:10000,1000,replace=F)){
  pb$tick()
  Sys.sleep(0.01)
data = breast
newindex = sample(1:NROW(data),NROW(data)/3,replace=F)
testdata = data[newindex,]
traindata = data[-newindex,]
temp<- ubSMOTE(X= traindata[,-1],  Y= traindata$Class,
               perc.over = 100, perc.under = 200,k = 7, verbose = FALSE)
data_smote<-cbind(temp$X, temp$Y)
colnames(data_smote)[10]<- "Class"
traindata = data_smote
traindata$class = as.factor(paste(traindata$Class,traindata$age,sep=''))
testdata$class = as.factor(paste(testdata$Class,testdata$age,sep=''))

result_breast = cl_algorithm(class=class,data=traindata,root_='information',del='class')
results_breast=learn_parameter(result_breast$data,result_breast,class=class,alpha=1) 
fit_breast = bnc('tan_cl','Class',traindata[,c(-11)],smooth=1)
fit_breast$.params=results_breast
end_breasttime1 = Sys.time()

start_breasttime2 = Sys.time()
result_breast2 = cl_algorithm(class=class,data=traindata,root_='information',del=c('age','class'))
results_breast2=learn_parameter(result_breast2$data,result_breast2,class=class,alpha=1) 
fit_breast2 = bnc('tan_cl','Class',traindata[,c(-1,-11)],smooth=1)
fit_breast2$.params=results_breast2
end_breasttime2 = Sys.time()

start_breasttime3 = Sys.time()
result_breast3 = cl_algorithm(class='class',data=traindata,root_='information',del=c('age','Class'))
results_breast3=learn_parameter(result_breast3$data,result_breast3,class='class',alpha=1) 
fit_breast3 = bnc('tan_cl','class',traindata[,c(-1,-10)],smooth=1)
fit_breast3$.params=results_breast3
end_breasttime3 = Sys.time()

start_breasttime4 = Sys.time()
result_breast4 = cl_algorithm(class='class',data=traindata,root_=result_breast2$information[1,1],
                             del=c('age','Class'))
results_breast4=learn_parameter(result_breast4$data,result_breast4,class='class',alpha=1) 
fit_breast3_1 = bnc('tan_cl','class',traindata[,c(-1,-10)],smooth=1)
fit_breast3_1$.params=results_breast4
end_breasttime4 = Sys.time()

pred_breast=predict(fit_breast,testdata)
breast_prob = predict(fit_breast,testdata,prob=T)
breast_acc = accuracy(pred_breast,testdata$Class)
breast_tab=table(pred_breast,testdata$Class,dnn=c("pred","real"))
breast_sens = Sensitivity(testdata$Class,pred_breast)
breast_spec = Specificity(testdata$Class,pred_breast)
breast_precision = Precision(testdata$Class,pred_breast)
breast_f1 = F1_Score(testdata$Class,pred_breast)
breast_roc = roc(testdata$Class,breast_prob[,2],quiet=T)$auc[1] ################### ROC / AUC

pred_breast2=predict(fit_breast2,testdata)
breast_prob2 = predict(fit_breast2,testdata,prob=T)
breast_acc2 = accuracy(pred_breast2,testdata$Class)
breast_tab2 = table(pred_breast2,testdata$Class,dnn=c("pred","real"))
breast_sens2 = Sensitivity(testdata$Class,pred_breast2)
breast_spec2 = Specificity(testdata$Class,pred_breast2)
breast_precision2 = Precision(testdata$Class,pred_breast2)
breast_f12 = F1_Score(testdata$Class,pred_breast2)
breast_roc2 = roc(testdata$Class,breast_prob2[,2],quiet=T)$auc[1] ################### ROC / AUC

pred_breast3=predict(fit_breast3,testdata)
pred_breast3 = as.factor(ifelse(as.integer(pred_breast3)<4,0,1))
breast_prob3 = predict(fit_breast3,testdata,prob=T)
breast_acc3 = accuracy(pred_breast3,testdata$Class)
breast_tab3 = table(pred_breast3,testdata$Class,dnn=c("pred","real"))
breast_sens3 = Sensitivity(testdata$Class,pred_breast3)
breast_spec3 = Specificity(testdata$Class,pred_breast3)
breast_precision3 = Precision(testdata$Class,pred_breast3)
breast_f13 = F1_Score(testdata$Class,pred_breast3)
breast_roc3 = roc(testdata$Class,apply(breast_prob3[,4:6],1,sum),quiet=T)$auc[1] ################### ROC / AUC

pred_breast3_1=predict(fit_breast3_1,testdata)
pred_breast3_1 = as.factor(ifelse(as.integer(pred_breast3_1)<4,0,1))
breast_prob3_1 = predict(fit_breast3_1,testdata,prob=T)
breast_acc3_1 = accuracy(pred_breast3_1,testdata$Class)
breast_tab3_1 = table(pred_breast3_1,testdata$Class,dnn=c("pred","real"))
breast_sens3_1 = Sensitivity(testdata$Class,pred_breast3_1)
breast_spec3_1 = Specificity(testdata$Class,pred_breast3_1)
breast_precision3_1 = Precision(testdata$Class,pred_breast3_1)
breast_f13_1 = F1_Score(testdata$Class,pred_breast3_1)
breast_roc3_1 = roc(testdata$Class,apply(breast_prob3_1[,4:6],1,sum),quiet=T)$auc[1] ################### ROC / AUC

breast_naive = bnc('nb','class',traindata[,-c(1,10)],smooth=1)
breast_naive_pred=predict(breast_naive,testdata)
breast_naive_pred = as.factor(ifelse(as.integer(breast_naive_pred)<4,0,1))
breast_naive_prob = predict(breast_naive,testdata,prob=T)
breast_naive_tab = table(breast_naive_pred,testdata$Class)

breast_naive_sens = Sensitivity(testdata$Class,breast_naive_pred)
breast_naive_spec = Specificity(testdata$Class,breast_naive_pred)
breast_naive_precision = Precision(testdata$Class,breast_naive_pred)
breast_naive_f1 = F1_Score(testdata$Class,breast_naive_pred)

breast_naive_acc = accuracy(breast_naive_pred,testdata$Class)  #####################3 Acc of breast_naive
breast_naive_roc = roc(testdata$Class,apply(breast_naive_prob[,4:6],1,sum),quiet=T)$auc[1] ################# AUC of breast_naive

breast_naive1 = bnc('nb','Class',traindata[,-11],smooth=1)
breast_naive1_pred=predict(breast_naive1,testdata)
breast_naive1_prob = predict(breast_naive1,testdata,prob=T)
breast_naive1_tab = table(breast_naive1_pred,testdata$Class)

breast_naive1_sens = Sensitivity(testdata$Class,breast_naive1_pred)
breast_naive1_spec = Specificity(testdata$Class,breast_naive1_pred)
breast_naive1_precision = Precision(testdata$Class,breast_naive1_pred)
breast_naive1_f1 = F1_Score(testdata$Class,breast_naive1_pred)

breast_naive1_acc = accuracy(breast_naive1_pred,testdata$Class)  #####################3 Acc of breast_naive1
breast_naive1_roc = roc(testdata$Class,breast_naive1_prob[,2],quiet=T)$auc[1] ################# AUC of breast_naive1

breast_result = c(breast_acc,breast_roc,breast_sens,breast_spec,breast_precision,breast_f1)
breast_result2 = c(breast_acc2,breast_roc2,breast_sens2,breast_spec2,breast_precision2,breast_f12)
breast_result3 = c(breast_acc3,breast_roc3,breast_sens3,breast_spec3,breast_precision3,breast_f13)
breast_result3_1 = c(breast_acc3_1,breast_roc3_1,breast_sens3_1,breast_spec3_1,breast_precision3_1,breast_f13_1)
breast_naive1_result = c(breast_naive1_acc,breast_naive1_roc,breast_naive1_sens,breast_naive1_spec,breast_naive1_precision,breast_naive1_f1)
breast_naive_result = c(breast_naive_acc,breast_naive_roc,breast_naive_sens,breast_naive_spec,breast_naive_precision,breast_naive_f1)
breast_final_result = round(rbind(breast_result,breast_result2,breast_result3,breast_result3_1,
                                 breast_naive1_result,breast_naive_result),3)
breast_sum = breast_sum + breast_final_result
colnames(breast_final_result) = c("Acc","AUC","Sens","Spec","Precision","F1")
}
breast_sum = round(breast_sum/1000,3)
colnames(breast_sum) = c("Acc","AUC","Sens","Spec","Precision","F1")
write.csv(breast_sum,"/Users/hyunwoo/Desktop/breast_cancer/breast_compare_Bayesian_Network.csv")
eye_sum = round(eye_sum/100,3)
blood_sum = round(blood_sum/100,3)
write.csv(eye_sum,"/Users/hyunwoo/Desktop/eye_data/eye_compare_Bayesian_Network.csv")
write.csv(blood_sum,"/Users/hyunwoo/Desktop/blood_data/blood_compare_Bayesian_Network.csv")
