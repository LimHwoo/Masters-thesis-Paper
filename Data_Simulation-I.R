######################################## New Data-Simulation-I 
rm(list=ls())
source('/Users/hyunwoo/Desktop/연구/Metrics.R')
source('/Users/hyunwoo/Desktop/연구/cl_learning.R')
logitlink = function(num){
  result = exp(num)/(1+exp(num))
  return(result)
}
probit = function(num){
  result = pnorm(num)
  return(pnorm(num))
}

########################################
simulation3_sum = 0
N=50000
iter = 1
split=10
pb <- progress_bar$new(
  format = "진행상황 : [:bar] :percent, 완료예정시간 : :eta , 총 소요시간 : :elapsedfull",total = iter)
for(i in 1:iter){
  pb$tick()
  Sys.sleep(0.01)
  data = data.frame()
  for(i in 1:N){
    target = rbinom(1,1,0.5)
    sex = rbinom(1,1,0.5)
    x1 = rbinom(1,1,prob=logitlink(-1.5+3*target))
    x2 = rbinom(1,1,prob=probit(-1+2*x1))
    x3 = rnorm(1,mean=10 + 5*x1,sd=2)
    var = rbinom(1,1,prob=logitlink(-1+2*x2))
    x4 = rnorm(1,mean=50 + 15*x2,sd=3)
    x5 = rbinom(1,1,prob=logitlink(1.5-3*var))
    x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
    x7 = rbinom(1,1,prob=probit(1-2*var))
    data1 = cbind(x1,x2,x3,x4,x5,x6,x7,var,sex,target)
    data = rbind(data,data1)
  }
  
  for(i in 1:length(data)){
    if(max(data[,i])==1){
    data[,i] = as.factor(ifelse(data[,i] > mean(data[,i]),1,0))
    }
    else{
      data[,i] = as.factor(ifelse(data[,i]<summary(data[,i])['1st Qu.'],0,
                                  ifelse(data[,i]<summary(data[,i])['Mean'],1,
                                         ifelse(data[,i]<summary(data[,i])['3rd Qu.'],2,3))))
    }
  }
  
  newindex = sample(1:NROW(data),NROW(data)/split,replace=F)
  testdata = data[newindex,]
  traindata = data[-newindex,]
  
  traindata$class = as.factor(paste(traindata$target,traindata$sex,traindata$var,sep=''))

  class='target'
  del = c('sex','var')
  result_simulation3 = cl_algorithm(class=class,data=traindata,root_='information',del='class')
  results_simulation3=learn_parameter(result_simulation3$data,result_simulation3,class=class,alpha=1) 
  fit_simulation3 = bnc('tan_cl','target',traindata %>% select(-"class"),smooth=1)
  fit_simulation3$.params=results_simulation3
  end_simulation3time1 = Sys.time()
  
  start_simulation3time2 = Sys.time()
  result_simulation32 = cl_algorithm(class=class,data=traindata,root_='information',del=c(del,'class'))
  results_simulation32=learn_parameter(result_simulation32$data,result_simulation32,class=class,alpha=1) 
  fit_simulation32 = bnc('tan_cl','target',traindata %>% select(-c(del,'class')),smooth=1)
  fit_simulation32$.params=results_simulation32
  end_simulation3time2 = Sys.time()
  
  start_simulation3time3 = Sys.time()
  result_simulation33 = cl_algorithm(class='class',data=traindata,root_='information',del=c(del,'target'))
  results_simulation33=learn_parameter(result_simulation33$data,result_simulation33,class='class',alpha=1) 
  fit_simulation33 = bnc('tan_cl','class',traindata %>% select(-c(del,'target')),smooth=1)
  fit_simulation33$.params=results_simulation33
  end_simulation3time3 = Sys.time()
  
  start_simulation3time4 = Sys.time()
  result_simulation34 = cl_algorithm(class='class',data=traindata,root_=result_simulation32$information[1,1],
                                     del=c(del,'target'))
  results_simulation34=learn_parameter(result_simulation34$data,result_simulation34,class='class',alpha=1) 
  fit_simulation33_1 = bnc('tan_cl','class',traindata %>% select(-c(del,'target')),smooth=1)
  fit_simulation33_1$.params=results_simulation34
  end_simulation3time4 = Sys.time()
  
  pred_simulation3=predict(fit_simulation3,testdata)
  simulation3_prob = predict(fit_simulation3,testdata,prob=T)
  simulation3_acc = accuracy(pred_simulation3,testdata$target)
  simulation3_tab=table(pred_simulation3,testdata$target,dnn=c("pred","real"))
  simulation3_sens = Sensitivity(testdata$target,pred_simulation3)
  simulation3_spec = Specificity(testdata$target,pred_simulation3)
  simulation3_precision = Precision(testdata$target,pred_simulation3)
  simulation3_f1 = F1_Score(testdata$target,pred_simulation3)
  simulation3_roc = roc(testdata$target,simulation3_prob[,2],quiet=T)$auc[1] ################### ROC / AUC
  
  pred_simulation32=predict(fit_simulation32,testdata)
  simulation3_prob2 = predict(fit_simulation32,testdata,prob=T)
  simulation3_acc2 = accuracy(pred_simulation32,testdata$target)
  simulation3_tab2 = table(pred_simulation32,testdata$target,dnn=c("pred","real"))
  simulation3_sens2 = Sensitivity(testdata$target,pred_simulation32)
  simulation3_spec2 = Specificity(testdata$target,pred_simulation32)
  simulation3_precision2 = Precision(testdata$target,pred_simulation32)
  simulation3_f12 = F1_Score(testdata$target,pred_simulation32)
  simulation3_roc2 = roc(testdata$target,simulation3_prob2[,2],quiet=T)$auc[1] ################### ROC / AUC
  
  pred_simulation33=predict(fit_simulation33,testdata)
  pred_simulation33 = as.factor(ifelse(substr(pred_simulation33,1,1)=='0',0,1))
  simulation3_prob3 = predict(fit_simulation33,testdata,prob=T)
  simulation3_acc3 = accuracy(pred_simulation33,testdata$target)
  simulation3_tab3 = table(pred_simulation33,testdata$target,dnn=c("pred","real"))
  simulation3_sens3 = Sensitivity(testdata$target,pred_simulation33)
  simulation3_spec3 = Specificity(testdata$target,pred_simulation33)
  simulation3_precision3 = Precision(testdata$target,pred_simulation33)
  simulation3_f13 = F1_Score(testdata$target,pred_simulation33)
  simulation3_roc3 = roc(testdata$target,apply(simulation3_prob3[,substr(colnames(simulation3_prob3),1,1)=='1'],1,sum),quiet=T)$auc[1] ################### ROC / AUC
  
  pred_simulation33_1=predict(fit_simulation33_1,testdata)
  pred_simulation33_1 = as.factor(ifelse(substr(pred_simulation33_1,1,1)=='0',0,1))
  simulation3_prob3_1 = predict(fit_simulation33_1,testdata,prob=T)
  simulation3_acc3_1 = accuracy(pred_simulation33_1,testdata$target)
  simulation3_tab3_1 = table(pred_simulation33_1,testdata$target,dnn=c("pred","real"))
  simulation3_sens3_1 = Sensitivity(testdata$target,pred_simulation33_1)
  simulation3_spec3_1 = Specificity(testdata$target,pred_simulation33_1)
  simulation3_precision3_1 = Precision(testdata$target,pred_simulation33_1)
  simulation3_f13_1 = F1_Score(testdata$target,pred_simulation33_1)
  simulation3_roc3_1 = roc(testdata$target,apply(simulation3_prob3_1[,substr(colnames(simulation3_prob3_1),1,1)=='1'],1,sum),quiet=T)$auc[1] ################### ROC / AUC
  
  simulation3_naive = bnc('nb','class',traindata %>% select(-c(del,'target')),smooth=1)
  simulation3_naive_pred=predict(simulation3_naive,testdata)
  simulation3_naive_pred = as.factor(ifelse(substr(simulation3_naive_pred,1,1)=='0',0,1))
  simulation3_naive_prob = predict(simulation3_naive,testdata,prob=T)
  simulation3_naive_tab = table(simulation3_naive_pred,testdata$target)
  
  simulation3_naive_sens = Sensitivity(testdata$target,simulation3_naive_pred)
  simulation3_naive_spec = Specificity(testdata$target,simulation3_naive_pred)
  simulation3_naive_precision = Precision(testdata$target,simulation3_naive_pred)
  simulation3_naive_f1 = F1_Score(testdata$target,simulation3_naive_pred)
  
  simulation3_naive_acc = accuracy(simulation3_naive_pred,testdata$target)  #####################3 Acc of simulation3_naive
  simulation3_naive_roc = roc(testdata$target,apply(simulation3_naive_prob[,substr(colnames(simulation3_naive_prob),1,1)=='1'],1,sum),quiet=T)$auc[1] ################# AUC of simulation3_naive
  
  simulation3_naive1 = bnc('nb','target',traindata %>% select(-'class'),smooth=1)
  simulation3_naive1_pred=predict(simulation3_naive1,testdata)
  simulation3_naive1_prob = predict(simulation3_naive1,testdata,prob=T)
  simulation3_naive1_tab = table(simulation3_naive1_pred,testdata$target)
  
  simulation3_naive1_sens = Sensitivity(testdata$target,simulation3_naive1_pred)
  simulation3_naive1_spec = Specificity(testdata$target,simulation3_naive1_pred)
  simulation3_naive1_precision = Precision(testdata$target,simulation3_naive1_pred)
  simulation3_naive1_f1 = F1_Score(testdata$target,simulation3_naive1_pred)
  
  simulation3_naive1_acc = accuracy(simulation3_naive1_pred,testdata$target)  #####################3 Acc of simulation3_naive1
  simulation3_naive1_roc = roc(testdata$target,simulation3_naive1_prob[,2],quiet=T)$auc[1] ################# AUC of simulation3_naive1
  
  simulation3_result = c(simulation3_acc,simulation3_roc,simulation3_sens,simulation3_spec,simulation3_precision,simulation3_f1)
  simulation3_result2 = c(simulation3_acc2,simulation3_roc2,simulation3_sens2,simulation3_spec2,simulation3_precision2,simulation3_f12)
  simulation3_result3 = c(simulation3_acc3,simulation3_roc3,simulation3_sens3,simulation3_spec3,simulation3_precision3,simulation3_f13)
  simulation3_result3_1 = c(simulation3_acc3_1,simulation3_roc3_1,simulation3_sens3_1,simulation3_spec3_1,simulation3_precision3_1,simulation3_f13_1)
  simulation3_naive1_result = c(simulation3_naive1_acc,simulation3_naive1_roc,simulation3_naive1_sens,simulation3_naive1_spec,simulation3_naive1_precision,simulation3_naive1_f1)
  simulation3_naive_result = c(simulation3_naive_acc,simulation3_naive_roc,simulation3_naive_sens,simulation3_naive_spec,simulation3_naive_precision,simulation3_naive_f1)
  simulation3_final_result = round(rbind(simulation3_result,simulation3_result2,simulation3_result3,simulation3_result3_1,
                                         simulation3_naive1_result,simulation3_naive_result),3)
  colnames(simulation3_final_result) = c("Acc","AUC","Recall","Spec","Precision","F1")
  simulation3_sum = simulation3_sum + simulation3_final_result
}

simulation3_sum/iter
write.csv(round(simulation3_sum/iter,3),paste("/Users/hyunwoo/Downloads/Data 분석/TAN_Simulation_results/TAN_Simulation_N=",N,".csv"))
par(mfrow=c(1,2))
plot(PairApply(traindata[,-10],FUN=CramerV),main=paste("TAN_Simulation_N=",N))
plot(PairApply(traindata[,-c(7,8,9)],FUN=CramerV),main=paste("TAN_Simulation_N=",N))

plot(result_simulation3$nb)
plot(result_simulation32$nb)
plot(result_simulation33$nb)
plot(result_simulation34$nb)






#################################################### Simulation2 Variable간 관계가 별로없고 class와만 관계
simulation4_sum = 0
N=50000
iter = 100
split=10
pb <- progress_bar$new(
  format = "진행상황 : [:bar] :percent, 완료예정시간 : :eta , 총 소요시간 : :elapsedfull",total = iter)

for(i in 1:iter){
  pb$tick()
  Sys.sleep(0.01)
  data = data.frame()
  for(i in 1:N){
    target = rbinom(1,1,0.5)
    x1 = rbinom(1,1,0.8*target + 0.2*(1-target))
    x2 = rbinom(1,1,0.7*target + 0.3*(1-target))
    x3 = sample(0:3,1,prob=c(0.4-0.3*target,0.3-0.1*target,0.2+0.1*target,0.1+0.3*target))
    x4 = sample(0:4,1,prob=c(0.1*target+0.3*(1-target),0.1*target+0.3*(1-target),0.2*target+0.2*(1-target),0.25*target+0.1*(1-target),0.35*target+0.1*(1-target)))
    x5 = rbinom(1,1,0.3*target + 0.7*(1-target))
    x6 = rbinom(1,1,0.2*target+0.8*(1-target))
    age = sample(0:3,1,prob=c(0.3-0.1*target,0.3-0.1*target,0.2+0.1*target,0.2+0.1*target))
    sex = rbinom(1,1,0.5)
    data1 = cbind(x1,x2,x3,x4,x5,x6,age,sex,target)
    data = rbind(data,data1)
  }
  
  for(i in 1:length(data)){
    data[,i] = as.factor(data[,i])
  }
  
  newindex = sample(1:NROW(data),NROW(data)/split,replace=F)
  testdata = data[newindex,]
  traindata = data[-newindex,]
  
  traindata$class = as.factor(paste(traindata$target,traindata$sex,traindata$age,sep=''))
  testdata$class = as.factor(paste(testdata$target,testdata$sex,testdata$age,sep=''))
  class='target'
  
  result_simulation4 = cl_algorithm(class=class,data=traindata,root_='information',del='class')
  results_simulation4=learn_parameter(result_simulation4$data,result_simulation4,class=class,alpha=1) 
  fit_simulation4 = bnc('tan_cl','target',traindata[,c(-10)],smooth=1)
  fit_simulation4$.params=results_simulation4
  end_simulation4time1 = Sys.time()
  
  start_simulation4time2 = Sys.time()
  result_simulation42 = cl_algorithm(class=class,data=traindata,root_='information',del=c('sex','age','class'))
  results_simulation42=learn_parameter(result_simulation42$data,result_simulation42,class=class,alpha=1) 
  fit_simulation42 = bnc('tan_cl','target',traindata[,c(-7,-8,-10)],smooth=1)
  fit_simulation42$.params=results_simulation42
  end_simulation4time2 = Sys.time()
  
  start_simulation4time3 = Sys.time()
  result_simulation43 = cl_algorithm(class='class',data=traindata,root_='information',del=c('sex','age','target'))
  results_simulation43=learn_parameter(result_simulation43$data,result_simulation43,class='class',alpha=1) 
  fit_simulation43 = bnc('tan_cl','class',traindata[,c(-7,-8,-9)],smooth=1)
  fit_simulation43$.params=results_simulation43
  end_simulation4time3 = Sys.time()
  
  start_simulation4time4 = Sys.time()
  result_simulation44 = cl_algorithm(class='class',data=traindata,root_=result_simulation42$information[1,1],
                                     del=c('sex','age','target'))
  results_simulation44=learn_parameter(result_simulation44$data,result_simulation44,class='class',alpha=1) 
  fit_simulation43_1 = bnc('tan_cl','class',traindata[,c(-7,-8,-9)],smooth=1)
  fit_simulation43_1$.params=results_simulation44
  end_simulation4time4 = Sys.time()
  
  pred_simulation4=predict(fit_simulation4,testdata)
  simulation4_prob = predict(fit_simulation4,testdata,prob=T)
  simulation4_acc = accuracy(pred_simulation4,testdata$target)
  simulation4_tab=table(pred_simulation4,testdata$target,dnn=c("pred","real"))
  simulation4_sens = Sensitivity(testdata$target,pred_simulation4)
  simulation4_spec = Specificity(testdata$target,pred_simulation4)
  simulation4_precision = Precision(testdata$target,pred_simulation4)
  simulation4_f1 = F1_Score(testdata$target,pred_simulation4)
  simulation4_roc = roc(testdata$target,simulation4_prob[,2],quiet=T)$auc[1] ################### ROC / AUC
  
  pred_simulation42=predict(fit_simulation42,testdata)
  simulation4_prob2 = predict(fit_simulation42,testdata,prob=T)
  simulation4_acc2 = accuracy(pred_simulation42,testdata$target)
  simulation4_tab2 = table(pred_simulation42,testdata$target,dnn=c("pred","real"))
  simulation4_sens2 = Sensitivity(testdata$target,pred_simulation42)
  simulation4_spec2 = Specificity(testdata$target,pred_simulation42)
  simulation4_precision2 = Precision(testdata$target,pred_simulation42)
  simulation4_f12 = F1_Score(testdata$target,pred_simulation42)
  simulation4_roc2 = roc(testdata$target,simulation4_prob2[,2],quiet=T)$auc[1] ################### ROC / AUC
  
  pred_simulation43=predict(fit_simulation43,testdata)
  pred_simulation43 = as.factor(ifelse(substr(pred_simulation43,1,1)=='0',0,1))
  simulation4_prob3 = predict(fit_simulation43,testdata,prob=T)
  simulation4_acc3 = accuracy(pred_simulation43,testdata$target)
  simulation4_tab3 = table(pred_simulation43,testdata$target,dnn=c("pred","real"))
  simulation4_sens3 = Sensitivity(testdata$target,pred_simulation43)
  simulation4_spec3 = Specificity(testdata$target,pred_simulation43)
  simulation4_precision3 = Precision(testdata$target,pred_simulation43)
  simulation4_f13 = F1_Score(testdata$target,pred_simulation43)
  simulation4_roc3 = roc(testdata$target,apply(simulation4_prob3[,substr(colnames(simulation4_prob3),1,1)=='1'],1,sum),quiet=T)$auc[1] ################### ROC / AUC
  
  pred_simulation43_1=predict(fit_simulation43_1,testdata)
  pred_simulation43_1 = as.factor(ifelse(substr(pred_simulation43_1,1,1)=='0',0,1))
  simulation4_prob3_1 = predict(fit_simulation43_1,testdata,prob=T)
  simulation4_acc3_1 = accuracy(pred_simulation43_1,testdata$target)
  simulation4_tab3_1 = table(pred_simulation43_1,testdata$target,dnn=c("pred","real"))
  simulation4_sens3_1 = Sensitivity(testdata$target,pred_simulation43_1)
  simulation4_spec3_1 = Specificity(testdata$target,pred_simulation43_1)
  simulation4_precision3_1 = Precision(testdata$target,pred_simulation43_1)
  simulation4_f13_1 = F1_Score(testdata$target,pred_simulation43_1)
  simulation4_roc3_1 = roc(testdata$target,apply(simulation4_prob3_1[,substr(colnames(simulation4_prob3_1),1,1)=='1'],1,sum),quiet=T)$auc[1] ################### ROC / AUC
  
  simulation4_naive = bnc('nb','class',traindata[,-c(7,8,9)],smooth=1)
  simulation4_naive_pred=predict(simulation4_naive,testdata)
  simulation4_naive_pred = as.factor(ifelse(substr(simulation4_naive_pred,1,1)=='0',0,1))
  simulation4_naive_prob = predict(simulation4_naive,testdata,prob=T)
  simulation4_naive_tab = table(simulation4_naive_pred,testdata$target)
  
  simulation4_naive_sens = Sensitivity(testdata$target,simulation4_naive_pred)
  simulation4_naive_spec = Specificity(testdata$target,simulation4_naive_pred)
  simulation4_naive_precision = Precision(testdata$target,simulation4_naive_pred)
  simulation4_naive_f1 = F1_Score(testdata$target,simulation4_naive_pred)
  
  simulation4_naive_acc = accuracy(simulation4_naive_pred,testdata$target)  #####################3 Acc of simulation4_naive
  simulation4_naive_roc = roc(testdata$target,apply(simulation4_naive_prob[,substr(colnames(simulation4_naive_prob),1,1)=='1'],1,sum),quiet=T)$auc[1] ################# AUC of simulation4_naive
  
  simulation4_naive1 = bnc('nb','target',traindata[,-10],smooth=1)
  simulation4_naive1_pred=predict(simulation4_naive1,testdata)
  simulation4_naive1_prob = predict(simulation4_naive1,testdata,prob=T)
  simulation4_naive1_tab = table(simulation4_naive1_pred,testdata$target)
  
  simulation4_naive1_sens = Sensitivity(testdata$target,simulation4_naive1_pred)
  simulation4_naive1_spec = Specificity(testdata$target,simulation4_naive1_pred)
  simulation4_naive1_precision = Precision(testdata$target,simulation4_naive1_pred)
  simulation4_naive1_f1 = F1_Score(testdata$target,simulation4_naive1_pred)
  
  simulation4_naive1_acc = accuracy(simulation4_naive1_pred,testdata$target)  #####################3 Acc of simulation4_naive1
  simulation4_naive1_roc = roc(testdata$target,simulation4_naive1_prob[,2],quiet=T)$auc[1] ################# AUC of simulation4_naive1
  
  simulation4_result = c(simulation4_acc,simulation4_roc,simulation4_sens,simulation4_spec,simulation4_precision,simulation4_f1)
  simulation4_result2 = c(simulation4_acc2,simulation4_roc2,simulation4_sens2,simulation4_spec2,simulation4_precision2,simulation4_f12)
  simulation4_result3 = c(simulation4_acc3,simulation4_roc3,simulation4_sens3,simulation4_spec3,simulation4_precision3,simulation4_f13)
  simulation4_result3_1 = c(simulation4_acc3_1,simulation4_roc3_1,simulation4_sens3_1,simulation4_spec3_1,simulation4_precision3_1,simulation4_f13_1)
  simulation4_naive1_result = c(simulation4_naive1_acc,simulation4_naive1_roc,simulation4_naive1_sens,simulation4_naive1_spec,simulation4_naive1_precision,simulation4_naive1_f1)
  simulation4_naive_result = c(simulation4_naive_acc,simulation4_naive_roc,simulation4_naive_sens,simulation4_naive_spec,simulation4_naive_precision,simulation4_naive_f1)
  simulation4_final_result = round(rbind(simulation4_result,simulation4_result2,simulation4_result3,simulation4_result3_1,
                                         simulation4_naive1_result,simulation4_naive_result),3)
  colnames(simulation4_final_result) = c("Acc","AUC","Sens","Spec","Precision","F1")
  simulation4_sum = simulation4_sum + simulation4_final_result
}

simulation4_sum/iter
write.csv(round(simulation4_sum/iter,3),paste("/Users/hyunwoo/Downloads/Data 분석/Naive_Simulation_results/Naive_Simulation_N=",N,".csv"))

par(mfrow=c(1,2))
plot(PairApply(traindata[,-10],FUN=CramerV),main=paste("Naive_Simulation_N=",N))
plot(PairApply(traindata[,-c(7,8,9)],FUN=CramerV),main=paste("Naive_Simulation_N=",N))
plot(result_simulation4$nb)
