######################################## Data-Simulation-II P개수에따른 비교
rm(list=ls())
source('/Users/hyunwoo/Desktop/연구/Metrics.R')
source('/Users/hyunwoo/Desktop/연구/cl_learning.R')

######################################## Sample = 50000
simulation3_sum = 0
N=50000
iter = 100
pb <- progress_bar$new(
  format = "진행상황 : [:bar] :percent, 완료예정시간 : :eta , 총 소요시간 : :elapsedfull",total = iter)
for(i in 1:iter){
  pb$tick()
  Sys.sleep(0.01)
  data = data.frame()
  # ####################### p=10, del = c('sex','age')
  # p=10
  #   target = rbinom(1,1,0.5)
  #   sex = rbinom(1,1,0.5)
  #   x1 = rbinom(1,1,prob=logitlink(-1.5+3*target))  
  #   x2 = rbinom(1,1,prob=probit(-1+2*x1))
  #   x3 = rnorm(1,mean=10 + 5*x1,sd=2)
  #   var = rbinom(1,1,prob=logitlink(-1+2*x2))
  #   x4 = rnorm(1,mean=50 + 15*x2,sd=3)
  #   x5 = rbinom(1,1,prob=logitlink(1.5-3*var))
  #   x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
  #   x7 = rbinom(1,1,prob=probit(1-2*var))
  #   data1 = cbind(x1,x2,x3,x4,x5,x6,x7,var,sex,target)
  #   data = rbind(data,data1)
  # }
  # 
  ########################### p=25 , del=c('sex','age')
  # p = 25
  # for(i in 1:N){
  #   target = rbinom(1,1,0.5)
  #   sex = rbinom(1,1,0.5)
  #   x1 = rbinom(1,1,prob=logitlink(-1.5+3*target))
  #   x2 = rbinom(1,1,prob=probit(-1+2*x1))
  #   x3 = rnorm(1,mean=10 + 5*x1,sd=2)
  #   var = rbinom(1,1,prob=logitlink(-1+2*x2))
  #   x4 = rnorm(1,mean=50 + 15*x2,sd=3)
  #   x5 = rbinom(1,1,prob=logitlink(1.5-3*var))
  #   x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
  #   x7 = rbinom(1,1,prob=probit(1-2*var))
  # 
  #   x8 = rbinom(1,1,prob=logitlink(-1+2*target))
  #   x9 = rbinom(1,1,prob=probit(-1.5+3*var))
  #   x10 = rnorm(1,mean=30+3*x8,sd=1.5)
  #   x11 = rnorm(1,mean=15+2*x9,sd=1)
  #   x12 = rnorm(1,mean=50+1.5*x10,sd=2)
  #   x13 = rbinom(1,1,prob=probit(-1+2*x9))
  #   x14 = rbinom(1,1,prob=logitlink(-1+2*x13))
  # 
  #   x15 = rbinom(1,1,prob=logitlink(0.5-target))
  #   x16 = rbinom(1,1,prob=probit(2-4*var))
  #   x17 = rnorm(1,mean=50-3*x15,sd=2)
  #   x18 = rbinom(1,1,prob=probit(1-2*x16))
  #   x19 = rbinom(1,1,prob=logitlink(-1+2*x16))
  #   x20 = rbinom(1,1,prob=logitlink(-1.5+3*x19))
  #   x21 = rnorm(1,mean=10+x17,sd=1)
  #   x22 = rbinom(1,1,prob=logitlink(-0.5+x18))
  # 
  #   data1 = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
  #                 var,sex,target)
  #   data = rbind(data,data1)
  # }
  
  ########################### p=40 , del=c('sex','age')
  # p=40
  # for(i in 1:N){
  # target = rbinom(1,1,0.5)
  # sex = rbinom(1,1,0.5)
  # x1 = rbinom(1,1,prob=logitlink(-1.5+3*target))
  # x2 = rbinom(1,1,prob=probit(-1+2*x1))
  # x3 = rnorm(1,mean=10 + 5*x1,sd=2)
  # var = rbinom(1,1,prob=logitlink(-1+2*x2))
  # x4 = rnorm(1,mean=50 + 15*x2,sd=3)
  # x5 = rbinom(1,1,prob=logitlink(1.5-3*var))
  # x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
  # x7 = rbinom(1,1,prob=probit(1-2*var))
  # 
  # x8 = rbinom(1,1,prob=logitlink(-1+2*target))
  # x9 = rbinom(1,1,prob=probit(-1.5+3*var))
  # x10 = rnorm(1,mean=30+3*x8,sd=1.5)
  # x11 = rnorm(1,mean=15+2*x9,sd=1)
  # x12 = rnorm(1,mean=50+1.5*x10,sd=2)
  # x13 = rbinom(1,1,prob=probit(-1+2*x9))
  # x14 = rbinom(1,1,prob=logitlink(-1+2*x13))
  # 
  # x15 = rbinom(1,1,prob=logitlink(-0.5+target))
  # x16 = rbinom(1,1,prob=probit(2-4*var))
  # x17 = rnorm(1,mean=50-3*x15,sd=2)
  # x18 = rbinom(1,1,prob=probit(1-2*x16))
  # x19 = rbinom(1,1,prob=logitlink(-1+2*x16))
  # x20 = rbinom(1,1,prob=logitlink(-1.5+3*x19))
  # x21 = rnorm(1,mean=10+x17,sd=1)
  # x22 = rbinom(1,1,prob=logitlink(-0.5+x18))
  # 
  # x23 = rbinom(1,1,prob=logitlink(-0.5+target))
  # x24 = rbinom(1,1,prob=probit(2-4*var))
  # x25 = rnorm(1,mean=15-5*x23,sd=1)
  # x26 = rnorm(1,mean=25+3*x24,sd=1)
  # x27 = rnorm(1,mean=30+1.25*x25,sd=1.5)
  # x28 = rbinom(1,1,prob=probit(1-2*x24))
  # x29 =  rbinom(1,1,prob=probit(-1+2*x28))
  # 
  # x30 = rbinom(1,1,prob=logitlink(1-2*target))
  # x31 = rbinom(1,1,prob=logitlink(2-4*var))
  # x32 = rnorm(1,mean=5+4*x30,sd=1)
  # x33 = rbinom(1,1,prob=probit(-1.5+3*x31))
  # x34 = rbinom(1,1,prob=logitlink(-1+2*x31))
  # x35 = rbinom(1,1,prob=logitlink(-1.5+3*x34))
  # x36 = rnorm(1,mean=11-x32,sd=1)
  # x37 = rbinom(1,1,prob=logitlink(0.5-x33))
  # 
  # data1 = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
  #                 x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,
  #                 var,sex,target)
  # data = rbind(data,data1)
  # }
  
  ########################### p=70 , del=c('sex','age')
  p=70
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

   x8 = rbinom(1,1,prob=logitlink(-1+2*target))
   x9 = rbinom(1,1,prob=probit(-1.5+3*var))
   x10 = rnorm(1,mean=30+3*x8,sd=1.5)
   x11 = rnorm(1,mean=15+2*x9,sd=1)
   x12 = rnorm(1,mean=50+1.5*x10,sd=2)
   x13 = rbinom(1,1,prob=probit(-1+2*x9))
   x14 = rbinom(1,1,prob=logitlink(-1+2*x13))

   x15 = rbinom(1,1,prob=logitlink(-0.5+target))
   x16 = rbinom(1,1,prob=probit(2-4*var))
   x17 = rnorm(1,mean=50-3*x15,sd=2)
   x18 = rbinom(1,1,prob=probit(1-2*x16))
   x19 = rbinom(1,1,prob=logitlink(-1+2*x16))
   x20 = rbinom(1,1,prob=logitlink(-1.5+3*x19))
   x21 = rnorm(1,mean=10+x17,sd=1)
   x22 = rbinom(1,1,prob=logitlink(-0.5+x18))

   x23 = rbinom(1,1,prob=logitlink(-0.5+target))
   x24 = rbinom(1,1,prob=probit(2-4*var))
   x25 = rnorm(1,mean=15-5*x23,sd=1)
   x26 = rnorm(1,mean=25+3*x24,sd=1)
   x27 = rnorm(1,mean=30+1.25*x25,sd=1.5)
   x28 = rbinom(1,1,prob=probit(1-2*x24))
   x29 =  rbinom(1,1,prob=probit(-1+2*x28))

   x30 = rbinom(1,1,prob=logitlink(1-2*target))
   x31 = rbinom(1,1,prob=logitlink(2-4*var))
   x32 = rnorm(1,mean=5+4*x30,sd=1)
   x33 = rbinom(1,1,prob=probit(-1.5+3*x31))
   x34 = rbinom(1,1,prob=logitlink(-1+2*x31))
   x35 = rbinom(1,1,prob=logitlink(-1.5+3*x34))
   x36 = rnorm(1,mean=11-x32,sd=1)
   x37 = rbinom(1,1,prob=logitlink(0.5-x33))

    x38 = rbinom(1,1,prob=logitlink(-0.5+target))
    x39 = rbinom(1,1,prob=logitlink(2-4*var))
    x40 = rnorm(1,mean=20-5*x38,sd=1)
    x41 = rnorm(1,mean=25+2*x39,sd=1)
    x42 = rnorm(1,mean=10+1.25*x40,sd=1.5)
    x43 = rbinom(1,1,prob=probit(1-2*x39))
    x44 = rbinom(1,1,prob=probit(-1+2*x43))

    x45 = rbinom(1,1,prob=logitlink(0.5-target))
    x46 = rbinom(1,1,prob=probit(2-4*var))
    x47 = rnorm(1,mean=7+2*x45,sd=1)
    x48 = rbinom(1,1,prob=probit(-1.5+3*x46))
    x49 = rbinom(1,1,prob=logitlink(-1+2*x46))
    x50 = rbinom(1,1,prob=logitlink(-1.5+3*x49))
    x51 = rnorm(1,mean=25-1.5*x47,sd=1)
    x52 = rbinom(1,1,prob=logitlink(0.5-x48))

    x53 = rbinom(1,1,prob=logitlink(1-2*target))
    x54 = rbinom(1,1,prob=logitlink(2-4*var))
    x55 = rnorm(1,mean=20+5*x53,sd=1)
    x56 = rnorm(1,mean=25-2*x54,sd=1)
    x57 = rnorm(1,mean=15+1.3*x55,sd=1.5)
    x58 = rbinom(1,1,prob=probit(1-2*x54))
    x59 = rbinom(1,1,prob=probit(-1+2*x58))

    x60 = rbinom(1,1,prob=probit(-0.5+target))
    x61 = rbinom(1,1,prob=logitlink(2-4*var))
    x62 = rnorm(1,mean=10+1.5*x60,sd=1)
    x63 = rbinom(1,1,prob=probit(-1.5+3*x61))
    x64 = rbinom(1,1,prob=logitlink(1-2*x61))
    x65 = rbinom(1,1,prob=logitlink(-1.5+3*x64))
    x66 = rnorm(1,mean=20-2*x62,sd=1)
    x67 = rbinom(1,1,prob=logitlink(0.5-x63))

    data1 = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
                  x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,
                  x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49,x50,x51,x52,
                  x53,x54,x55,x56,x57,x58,x59,x60,x61,x62,x63,x64,x65,x66,x67,
                  sex,var,target)
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
  
  newindex = sample(1:NROW(data),NROW(data)/10,replace=F)
  testdata = data[newindex,]
  traindata = data[-newindex,]
  
  traindata$class = as.factor(paste(traindata$target,traindata$sex,traindata$var,sep=''))
  #testdata$class = as.factor(paste(testdata$target,testdata$sex,testdata$age,sep=''))
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
write.csv(round(simulation3_sum/iter,3),paste("/Users/hyunwoo/Downloads/Data 분석/TAN_Simulation_results/TAN_Simulation_P=",p,"_Sample=50000.csv"))
par(mfrow=c(1,2))

par(mfrow=c(1,1))
plot(result_simulation3$nb,main = paste("TAN-I_p=",p))
plot(result_simulation32$nb,main = paste("TAN-II_p=",p))
plot(result_simulation33$nb,main = paste('TAN-III_p=',p))
plot(result_simulation34$nb,main = paste('TAN-IV_p=',p))

