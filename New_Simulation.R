######################################## Data-Simulation-II P개수에따른 비교
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
# install.packages("progress")
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
# library(progress)
# library(bnlearn)
# library(mccr)
rm(list=ls())
source('/Users/hyunwoo/Desktop/연구/Metrics.R')
source('/Users/hyunwoo/Desktop/연구/cl_learning.R')

######################################## Sample = 50000
simulation3_sum = 0
N = 50000
iter = 300
split = 10
logitlink = function(num){
  result = exp(num)/(1+exp(num))
  return(result)
}
probit = function(num){
  result = pnorm(num)
  return(pnorm(num))
}
{
Tan_cv_acc = c()
Tan_cv_sens = c()
Tan_cv_spec = c()
Tan_cv_precision = c()
Tan_cv_f1 = c()
Tan_cv_roc = c()
Tan_cv_mcc = c()
Tan_cv_sim = c()

Tan_cv_acc2 = c()
Tan_cv_sens2 = c()
Tan_cv_spec2 = c()
Tan_cv_precision2 = c()
Tan_cv_f12 = c()
Tan_cv_roc2 = c()
Tan_cv_mcc2 = c()
Tan_cv_sim2 = c()


Tan_cv_acc3_1 = c()
Tan_cv_sens3_1 = c()
Tan_cv_spec3_1 = c()
Tan_cv_precision3_1 = c()
Tan_cv_f13_1 = c()
Tan_cv_roc3_1 = c()
Tan_cv_mcc3_1 = c()
Tan_cv_sim4 = c()

Naive_cv_sens=c()
Naive_cv_spec = c()
Naive_cv_precision = c()
Naive_cv_acc = c()
Naive_cv_f1 = c()
Naive_cv_roc = c()
Naive_cv_mcc = c()

Naive1_cv_sens=c()
Naive1_cv_spec = c()
Naive1_cv_precision = c()
Naive1_cv_acc = c()
Naive1_cv_f1 = c()
Naive1_cv_roc = c()
Naive1_cv_mcc = c()
}
pb <- progress_bar$new(
  format = "진행상황 : [:bar] :percent, 완료예정시간 : :eta , 총 소요시간 : :elapsedfull",total = iter)
for(i in 1:iter){
  pb$tick()
  Sys.sleep(0.01)
  data = data.frame()
  # ####################### p=10, del = c('sex','age')
  # p=10
  # for(i in 1:N){
  #   Y = rbinom(1,1,0.5)
  #   sex = rbinom(1,1,0.5)
  #   x1 = rbinom(1,1,prob=logitlink(-1.5+3*Y))
  #   x2 = rbinom(1,1,prob=probit(-1+2*x1))
  #   x3 = rnorm(1,mean=10 + 5*x1,sd=2)
  #   age = rbinom(1,1,prob=logitlink(-1+2*x2))
  #   x4 = rnorm(1,mean=50 + 15*x2,sd=3)
  #   x5 = rbinom(1,1,prob=logitlink(1.5-3*age-x2))
  #   x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
  #   x7 = rbinom(1,1,prob=probit(1-2*age-0.5*x2))
  #   data1 = cbind(x1,x2,x3,x4,x5,x6,x7,age,sex,Y)
  #   data = rbind(data,data1)
  # }
  # dag=model2network("[Y][x1|Y][x2|x1][x3|x1][age][x4|x2][x5|x2][x6|x3][x7|x2][sex]")
  
  ########################### p=25 , del=c('sex','age')
  p = 25
  for(i in 1:N){
    Y = rbinom(1,1,0.5)
    sex = rbinom(1,1,0.5)
    x1 = rbinom(1,1,prob=logitlink(-1.5+3*Y))
    x2 = rbinom(1,1,prob=probit(-1+2*x1))
    x3 = rnorm(1,mean=10 + 5*x1,sd=2)
    age = rbinom(1,1,prob=logitlink(-1+2*x2))
    x4 = rnorm(1,mean=50 + 15*x2,sd=3)
    x5 = rbinom(1,1,prob=logitlink(1.5-3*age-x2))
    x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
    x7 = rbinom(1,1,prob=probit(1-2*age-0.5*x2))

    x8 = rbinom(1,1,prob=logitlink(-1+2*Y))
    x9 = rbinom(1,1,prob=probit(-1.5+3*age-x2))
    x10 = rnorm(1,mean=30+3*x8,sd=1.5)
    x11 = rnorm(1,mean=15+2*x9,sd=1)
    x12 = rnorm(1,mean=50+1.5*x10,sd=2)
    x13 = rbinom(1,1,prob=probit(-1+2*x9))
    x14 = rbinom(1,1,prob=logitlink(-1+2*x13))

    x15 = rbinom(1,1,prob=logitlink(0.5-Y))
    x16 = rbinom(1,1,prob=probit(2-4*age-x2))
    x17 = rnorm(1,mean=50-3*x15,sd=2)
    x18 = rbinom(1,1,prob=probit(1-2*x16))
    x19 = rbinom(1,1,prob=logitlink(-1+2*x16))
    x20 = rbinom(1,1,prob=logitlink(-1.5+3*x19))
    x21 = rnorm(1,mean=10+x17,sd=1)
    x22 = rbinom(1,1,prob=logitlink(-0.5+x18))

    data1 = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
                  age,sex,Y)
    data = rbind(data,data1)
  }
  dag=model2network("[Y][x1|Y][x2|x1][x3|x1][age][x4|x2][x5|x2][x6|x3][x7|x2][sex][x8|Y][x9|x2][x10|x8][x11|x9][x12|x10][x13|x9][x14|x13][x15|Y][x16|x2][x17|x15][x18|x16][x19|x16][x20|x19][x21|x17][x22|x18]")
  
  ########################### p=40 , del=c('sex','age')
  # p=40
  # for(i in 1:N){
  # Y = rbinom(1,1,0.5)
  # sex = rbinom(1,1,0.5)
  # x1 = rbinom(1,1,prob=logitlink(-1.5+3*Y))
  # x2 = rbinom(1,1,prob=probit(-1+2*x1))
  # x3 = rnorm(1,mean=10 + 5*x1,sd=2)
  # age = rbinom(1,1,prob=logitlink(-1+2*x2))
  # x4 = rnorm(1,mean=50 + 15*x2,sd=3)
  # x5 = rbinom(1,1,prob=logitlink(1.5-3*age-x2))
  # x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
  # x7 = rbinom(1,1,prob=probit(1-2*age-0.5*x2))
  # 
  # x8 = rbinom(1,1,prob=logitlink(-1+2*Y))
  # x9 = rbinom(1,1,prob=probit(-1.5+3*age-x2))
  # x10 = rnorm(1,mean=30+3*x8,sd=1.5)
  # x11 = rnorm(1,mean=15+2*x9,sd=1)
  # x12 = rnorm(1,mean=50+1.5*x10,sd=2)
  # x13 = rbinom(1,1,prob=probit(-1+2*x9))
  # x14 = rbinom(1,1,prob=logitlink(-1+2*x13))
  # 
  # x15 = rbinom(1,1,prob=logitlink(0.5-Y))
  # x16 = rbinom(1,1,prob=probit(2-4*age-x2))
  # x17 = rnorm(1,mean=50-3*x15,sd=2)
  # x18 = rbinom(1,1,prob=probit(1-2*x16))
  # x19 = rbinom(1,1,prob=logitlink(-1+2*x16))
  # x20 = rbinom(1,1,prob=logitlink(-1.5+3*x19))
  # x21 = rnorm(1,mean=10+x17,sd=1)
  # x22 = rbinom(1,1,prob=logitlink(-0.5+x18))
  # 
  # x23 = rbinom(1,1,prob=logitlink(-0.5+Y))
  # x24 = rbinom(1,1,prob=probit(2-4*age-x2))
  # x25 = rnorm(1,mean=15-5*x23,sd=1)
  # x26 = rnorm(1,mean=25+3*x24,sd=1)
  # x27 = rnorm(1,mean=30+1.25*x25,sd=1.5)
  # x28 = rbinom(1,1,prob=probit(1-2*x24))
  # x29 =  rbinom(1,1,prob=probit(-1+2*x28))
  # 
  # x30 = rbinom(1,1,prob=logitlink(1-2*Y))
  # x31 = rbinom(1,1,prob=logitlink(2-4*age-x2))
  # x32 = rnorm(1,mean=5+4*x30,sd=1)
  # x33 = rbinom(1,1,prob=probit(-1.5+3*x31))
  # x34 = rbinom(1,1,prob=logitlink(-1+2*x31))
  # x35 = rbinom(1,1,prob=logitlink(-1.5+3*x34))
  # x36 = rnorm(1,mean=11-x32,sd=1)
  # x37 = rbinom(1,1,prob=logitlink(0.5-x33))
  # 
  # data1 = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
  #                 x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,
  #                 age,sex,Y)
  # data = rbind(data,data1)
  # }
  #dag=model2network("[Y][x1|Y][x2|x1][x3|x1][age][x4|x2][x5|x2][x6|x3][x7|x2][sex][x8|Y][x9|x2][x10|x8][x11|x9][x12|x10][x13|x9][x14|x13][x15|Y][x16|x2][x17|x15][x18|x16][x19|x16][x20|x19][x21|x17][x22|x18][x23|Y][x24|x2][x25|x23][x26|x24][x27|x25][x28|x24][x29|x28][x30|Y][x31|x2][x32|x30][x33|x31][x34|x31][x35|x34][x36|x32][x37|x33]")

  ########################### p=70 , del=c('sex','age')
  #  p=70
  #  for(i in 1:N){
  # Y = rbinom(1,1,0.5)
  # sex = rbinom(1,1,0.5)
  # x1 = rbinom(1,1,prob=logitlink(-1.5+3*Y))
  # x2 = rbinom(1,1,prob=probit(-1+2*x1))
  # x3 = rnorm(1,mean=10 + 5*x1,sd=2)
  # age = rbinom(1,1,prob=logitlink(-1+2*x2))
  # x4 = rnorm(1,mean=50 + 15*x2,sd=3)
  # x5 = rbinom(1,1,prob=logitlink(1.5-3*age-x2))
  # x6 = rnorm(1,mean=100 + 1.5*x3,sd=5)
  # x7 = rbinom(1,1,prob=probit(1-2*age-0.5*x2))
  # 
  # x8 = rbinom(1,1,prob=logitlink(-1+2*Y))
  # x9 = rbinom(1,1,prob=probit(-1.5+3*age-x2))
  # x10 = rnorm(1,mean=30+3*x8,sd=1.5)
  # x11 = rnorm(1,mean=15+2*x9,sd=1)
  # x12 = rnorm(1,mean=50+1.5*x10,sd=2)
  # x13 = rbinom(1,1,prob=probit(-1+2*x9))
  # x14 = rbinom(1,1,prob=logitlink(-1+2*x13))
  # 
  # x15 = rbinom(1,1,prob=logitlink(0.5-Y))
  # x16 = rbinom(1,1,prob=probit(2-4*age-x2))
  # x17 = rnorm(1,mean=50-3*x15,sd=2)
  # x18 = rbinom(1,1,prob=probit(1-2*x16))
  # x19 = rbinom(1,1,prob=logitlink(-1+2*x16))
  # x20 = rbinom(1,1,prob=logitlink(-1.5+3*x19))
  # x21 = rnorm(1,mean=10+x17,sd=1)
  # x22 = rbinom(1,1,prob=logitlink(-0.5+x18))
  # 
  # x23 = rbinom(1,1,prob=logitlink(-0.5+Y))
  # x24 = rbinom(1,1,prob=probit(2-4*age-x2))
  # x25 = rnorm(1,mean=15-5*x23,sd=1)
  # x26 = rnorm(1,mean=25+3*x24,sd=1)
  # x27 = rnorm(1,mean=30+1.25*x25,sd=1.5)
  # x28 = rbinom(1,1,prob=probit(1-2*x24))
  # x29 =  rbinom(1,1,prob=probit(-1+2*x28))
  # 
  # x30 = rbinom(1,1,prob=logitlink(1-2*Y))
  # x31 = rbinom(1,1,prob=logitlink(2-4*age-x2))
  # x32 = rnorm(1,mean=5+4*x30,sd=1)
  # x33 = rbinom(1,1,prob=probit(-1.5+3*x31))
  # x34 = rbinom(1,1,prob=logitlink(-1+2*x31))
  # x35 = rbinom(1,1,prob=logitlink(-1.5+3*x34))
  # x36 = rnorm(1,mean=11-x32,sd=1)
  # x37 = rbinom(1,1,prob=logitlink(0.5-x33))
   
    # x38 = rbinom(1,1,prob=logitlink(-0.5+Y))
    # x39 = rbinom(1,1,prob=logitlink(2-4*age-x2))
    # x40 = rnorm(1,mean=20-5*x38,sd=1)
    # x41 = rnorm(1,mean=25+2*x39,sd=1)
    # x42 = rnorm(1,mean=10+1.25*x40,sd=1.5)
    # x43 = rbinom(1,1,prob=probit(1-2*x39))
    # x44 = rbinom(1,1,prob=probit(-1+2*x43))
    # 
    # x45 = rbinom(1,1,prob=logitlink(0.5-Y))
    # x46 = rbinom(1,1,prob=probit(2-4*age-x2))
    # x47 = rnorm(1,mean=7+2*x45,sd=1)
    # x48 = rbinom(1,1,prob=probit(-1.5+3*x46))
    # x49 = rbinom(1,1,prob=logitlink(-1+2*x46))
    # x50 = rbinom(1,1,prob=logitlink(-1.5+3*x49))
    # x51 = rnorm(1,mean=25-1.5*x47,sd=1)
    # x52 = rbinom(1,1,prob=logitlink(0.5-x48))
    # 
    # x53 = rbinom(1,1,prob=logitlink(1-2*Y))
    # x54 = rbinom(1,1,prob=logitlink(2-4*age-x2))
    # x55 = rnorm(1,mean=20+5*x53,sd=1)
    # x56 = rnorm(1,mean=25-2*x54,sd=1)
    # x57 = rnorm(1,mean=15+1.3*x55,sd=1.5)
    # x58 = rbinom(1,1,prob=probit(1-2*x54))
    # x59 = rbinom(1,1,prob=probit(-1+2*x58))
    # 
    # x60 = rbinom(1,1,prob=probit(-0.5+Y))
    # x61 = rbinom(1,1,prob=logitlink(2-4*age-x2))
    # x62 = rnorm(1,mean=10+1.5*x60,sd=1)
    # x63 = rbinom(1,1,prob=probit(-1.5+3*x61))
    # x64 = rbinom(1,1,prob=logitlink(1-2*x61))
    # x65 = rbinom(1,1,prob=logitlink(-1.5+3*x64))
    # x66 = rnorm(1,mean=20-2*x62,sd=1)
    # x67 = rbinom(1,1,prob=logitlink(0.5-x63))
  # 
  #   data1 = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,
  #                 x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,
  #                 x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49,x50,x51,x52,
  #                 x53,x54,x55,x56,x57,x58,x59,x60,x61,x62,x63,x64,x65,x66,x67,
  #                 sex,age,Y)
  #   data = rbind(data,data1)
  # }
  #dag=model2network("[Y][x1|Y][x2|x1][x3|x1][age][x4|x2][x5|x2][x6|x3][x7|x2][sex][x8|Y][x9|x2][x10|x8][x11|x9][x12|x10][x13|x9][x14|x13][x15|Y][x16|x2][x17|x15][x18|x16][x19|x16][x20|x19][x21|x17][x22|x18][x23|Y][x24|x2][x25|x23][x26|x24][x27|x25][x28|x24][x29|x28][x30|Y][x31|x2][x32|x30][x33|x31][x34|x31][x35|x34][x36|x32][x37|x33][x38|Y][x39|x2][x40|x38][x41|x39][x42|x40][x43|x39][x44|x43][x45|Y][x46|x2][x47|x45][x48|x46][x49|x46][x50|x49][x51|x47][x52|x48][x53|Y][x54|x2][x55|x53][x56|x54][x57|x55][x58|x54][x59|x58][x60|Y][x61|x2][x62|x60][x63|x61][x64|x61][x65|x64][x66|x62][x67|x63]")
  
    
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
  
  traindata$NC = as.factor(paste(traindata$Y,traindata$sex,traindata$age,sep=''))
  #testdata$NC = as.factor(paste(testdata$Y,testdata$sex,testdata$age,sep=''))
  class='Y'
  del = c('sex','age')
  result_simulation3 = cl_algorithm(class=class,data=traindata,root_='information',del='NC')
  results_simulation3=learn_parameter(result_simulation3$data,result_simulation3,class=class,alpha=1) 
  fit_simulation3 = bnc('tan_cl','Y',traindata %>% select(-"NC"),smooth=1)
  fit_simulation3$.params=results_simulation3
  end_simulation3time1 = Sys.time()
  
  start_simulation3time2 = Sys.time()
  result_simulation32 = cl_algorithm(class=class,data=traindata,root_='information',del=c(del,'NC'))
  results_simulation32=learn_parameter(result_simulation32$data,result_simulation32,class=class,alpha=1) 
  fit_simulation32 = bnc('tan_cl','Y',traindata %>% select(-c(del,'NC')),smooth=1)
  fit_simulation32$.params=results_simulation32
  end_simulation3time2 = Sys.time()
  
  start_simulation3time4 = Sys.time()
  result_simulation34 = cl_algorithm(class='NC',data=traindata,root_=result_simulation32$information[1,1],
                                     del=c(del,'Y'))
  results_simulation34=learn_parameter(result_simulation34$data,result_simulation34,class='NC',alpha=1) 
  fit_simulation33_1 = bnc('tan_cl','NC',traindata %>% select(-c(del,'Y')),smooth=1)
  fit_simulation33_1$.params=results_simulation34
  end_simulation3time4 = Sys.time()
  
  pred_simulation3=predict(fit_simulation3,testdata)
  simulation3_prob = predict(fit_simulation3,testdata,prob=T)
  simulation3_acc = accuracy(pred_simulation3,testdata$Y)
  simulation3_tab=table(pred_simulation3,testdata$Y,dnn=c("pred","real"))
  simulation3_sens = Sensitivity(testdata$Y,pred_simulation3)
  simulation3_spec = Specificity(testdata$Y,pred_simulation3)
  simulation3_precision = Precision(testdata$Y,pred_simulation3)
  simulation3_f1 = F1_Score(testdata$Y,pred_simulation3)
  simulation3_roc = roc(testdata$Y,simulation3_prob[,2],quiet=T)$auc[1] ################### ROC / AUC
  simulation3_mcc = mccr(testdata$Y,pred_simulation3)
  similar = 0
  for(i in 1:NROW(result_simulation3$nodes)){
    for(j in 1:NROW(dag$arcs)){
      if (sum(result_simulation3$edge[i,] == dag$arcs[j,]) == 2) similar = similar + 1
    }
  }
  simulation3_sim = similar / NROW(result_simulation3$nodes)
  
  pred_simulation32=predict(fit_simulation32,testdata)
  simulation3_prob2 = predict(fit_simulation32,testdata,prob=T)
  simulation3_acc2 = accuracy(pred_simulation32,testdata$Y)
  simulation3_tab2 = table(pred_simulation32,testdata$Y,dnn=c("pred","real"))
  simulation3_sens2 = Sensitivity(testdata$Y,pred_simulation32)
  simulation3_spec2 = Specificity(testdata$Y,pred_simulation32)
  simulation3_precision2 = Precision(testdata$Y,pred_simulation32)
  simulation3_f12 = F1_Score(testdata$Y,pred_simulation32)
  simulation3_roc2 = roc(testdata$Y,simulation3_prob2[,2],quiet=T)$auc[1] ################### ROC / AUC
  simulation3_mcc2 = mccr(testdata$Y,pred_simulation32)
  similar = 0
  for(i in 1:NROW(result_simulation32$nodes)){
    for(j in 1:NROW(dag$arcs)){
      if (sum(result_simulation32$edge[i,] == dag$arcs[j,]) == 2) similar = similar + 1
    }
  }
  simulation32_sim = similar / NROW(result_simulation32$nodes)
    
  pred_simulation33_1=predict(fit_simulation33_1,testdata)
  pred_simulation33_1 = as.factor(ifelse(substr(pred_simulation33_1,1,1)=='0',0,1))
  simulation3_prob3_1 = predict(fit_simulation33_1,testdata,prob=T)
  simulation3_acc3_1 = accuracy(pred_simulation33_1,testdata$Y)
  simulation3_tab3_1 = table(pred_simulation33_1,testdata$Y,dnn=c("pred","real"))
  simulation3_sens3_1 = Sensitivity(testdata$Y,pred_simulation33_1)
  simulation3_spec3_1 = Specificity(testdata$Y,pred_simulation33_1)
  simulation3_precision3_1 = Precision(testdata$Y,pred_simulation33_1)
  simulation3_f13_1 = F1_Score(testdata$Y,pred_simulation33_1)
  simulation3_roc3_1 = roc(testdata$Y,apply(simulation3_prob3_1[,substr(colnames(simulation3_prob3_1),1,1)=='1'],1,sum),quiet=T)$auc[1] ################### ROC / AUC
  simulation3_mcc3_1 = mccr(testdata$Y,pred_simulation33_1)
  similar = 0
  for(i in 1:NROW(result_simulation34$nodes)){
    for(j in 1:NROW(dag$arcs)){
      if (sum(result_simulation34$edge[i,] == dag$arcs[j,]) == 2) similar = similar + 1
    }
  }
  simulation34_sim = similar / NROW(result_simulation34$nodes)

  
  simulation3_naive = bnc('nb','NC',traindata %>% select(-c(del,'Y')),smooth=1)
  simulation3_naive_pred=predict(simulation3_naive,testdata)
  simulation3_naive_pred = as.factor(ifelse(substr(simulation3_naive_pred,1,1)=='0',0,1))
  simulation3_naive_prob = predict(simulation3_naive,testdata,prob=T)
  simulation3_naive_tab = table(simulation3_naive_pred,testdata$Y)
  
  simulation3_naive_sens = Sensitivity(testdata$Y,simulation3_naive_pred)
  simulation3_naive_spec = Specificity(testdata$Y,simulation3_naive_pred)
  simulation3_naive_precision = Precision(testdata$Y,simulation3_naive_pred)
  simulation3_naive_f1 = F1_Score(testdata$Y,simulation3_naive_pred)
  
  simulation3_naive_acc = accuracy(simulation3_naive_pred,testdata$Y)  #####################3 Acc of simulation3_naive
  simulation3_naive_roc = roc(testdata$Y,apply(simulation3_naive_prob[,substr(colnames(simulation3_naive_prob),1,1)=='1'],1,sum),quiet=T)$auc[1] ################# AUC of simulation3_naive
  simulation3_naive_mcc = mccr(testdata$Y,simulation3_naive_pred)
  
  
  simulation3_naive1 = bnc('nb','Y',traindata %>% select(-'NC'),smooth=1)
  simulation3_naive1_pred=predict(simulation3_naive1,testdata)
  simulation3_naive1_prob = predict(simulation3_naive1,testdata,prob=T)
  simulation3_naive1_tab = table(simulation3_naive1_pred,testdata$Y)
  
  simulation3_naive1_sens = Sensitivity(testdata$Y,simulation3_naive1_pred)
  simulation3_naive1_spec = Specificity(testdata$Y,simulation3_naive1_pred)
  simulation3_naive1_precision = Precision(testdata$Y,simulation3_naive1_pred)
  simulation3_naive1_f1 = F1_Score(testdata$Y,simulation3_naive1_pred)
  
  simulation3_naive1_acc = accuracy(simulation3_naive1_pred,testdata$Y)  #####################3 Acc of simulation3_naive1
  simulation3_naive1_roc = roc(testdata$Y,simulation3_naive1_prob[,2],quiet=T)$auc[1] ################# AUC of simulation3_naive1
  simulation3_naive1_mcc = mccr(testdata$Y,simulation3_naive1_pred)
  
  Tan_cv_acc = c(Tan_cv_acc,simulation3_acc)
  Tan_cv_sens = c(Tan_cv_sens,simulation3_sens)
  Tan_cv_spec = c(Tan_cv_spec,simulation3_spec)
  Tan_cv_roc = c(Tan_cv_roc,simulation3_roc)
  Tan_cv_f1 = c(Tan_cv_f1,simulation3_f1)
  Tan_cv_precision = c(Tan_cv_precision,simulation3_precision)
  Tan_cv_sim = c(Tan_cv_sim,simulation3_sim)
  Tan_cv_mcc = c(Tan_cv_mcc,simulation3_mcc)
  
  Tan_cv_acc2 = c(Tan_cv_acc2,simulation3_acc2)
  Tan_cv_sens2 = c(Tan_cv_sens2,simulation3_sens2)
  Tan_cv_spec2 = c(Tan_cv_spec2,simulation3_spec2)
  Tan_cv_roc2 = c(Tan_cv_roc2,simulation3_roc2)
  Tan_cv_f12 = c(Tan_cv_f12,simulation3_f12)
  Tan_cv_precision2 = c(Tan_cv_precision2,simulation3_precision2)
  Tan_cv_sim2 = c(Tan_cv_sim2,simulation32_sim)
  Tan_cv_mcc2 = c(Tan_cv_mcc2,simulation3_mcc2)
  
  Tan_cv_acc3_1 = c(Tan_cv_acc3_1,simulation3_acc3_1)
  Tan_cv_sens3_1 = c(Tan_cv_sens3_1,simulation3_sens3_1)
  Tan_cv_spec3_1 = c(Tan_cv_spec3_1,simulation3_spec3_1)
  Tan_cv_roc3_1 = c(Tan_cv_roc3_1,simulation3_roc3_1)
  Tan_cv_f13_1 = c(Tan_cv_f13_1,simulation3_f13_1)
  Tan_cv_precision3_1 = c(Tan_cv_precision3_1,simulation3_precision3_1)
  Tan_cv_sim4 = c(Tan_cv_sim4,simulation34_sim)
  Tan_cv_mcc3_1 = c(Tan_cv_mcc3_1,simulation3_mcc3_1)
  
  Naive_cv_sens =c (Naive_cv_sens,simulation3_naive_sens)
  Naive_cv_spec = c(Naive_cv_spec,simulation3_naive_spec)
  Naive_cv_acc = c(Naive_cv_acc,simulation3_naive_acc)
  Naive_cv_roc = c(Naive_cv_roc,simulation3_naive_roc)
  Naive_cv_precision = c(Naive_cv_precision,simulation3_naive_precision)
  Naive_cv_f1 = c(Naive_cv_f1,simulation3_naive_f1)
  Naive_cv_mcc = c(Naive_cv_mcc,simulation3_naive_mcc)
  
  Naive1_cv_sens =c (Naive1_cv_sens,simulation3_naive1_sens)
  Naive1_cv_spec = c(Naive1_cv_spec,simulation3_naive1_spec)
  Naive1_cv_acc = c(Naive1_cv_acc,simulation3_naive1_acc)
  Naive1_cv_roc = c(Naive1_cv_roc,simulation3_naive1_roc)
  Naive1_cv_precision = c(Naive1_cv_precision,simulation3_naive1_precision)
  Naive1_cv_f1 = c(Naive1_cv_f1,simulation3_naive1_f1)
  Naive1_cv_mcc = c(Naive1_cv_mcc,simulation3_naive1_mcc)
  
  simulation3_result = c(simulation3_acc,simulation3_roc,simulation3_sens,simulation3_spec,simulation3_precision,simulation3_f1)
  simulation3_result2 = c(simulation3_acc2,simulation3_roc2,simulation3_sens2,simulation3_spec2,simulation3_precision2,simulation3_f12)
  simulation3_result3_1 = c(simulation3_acc3_1,simulation3_roc3_1,simulation3_sens3_1,simulation3_spec3_1,simulation3_precision3_1,simulation3_f13_1)
  simulation3_naive1_result = c(simulation3_naive1_acc,simulation3_naive1_roc,simulation3_naive1_sens,simulation3_naive1_spec,simulation3_naive1_precision,simulation3_naive1_f1)
  simulation3_naive_result = c(simulation3_naive_acc,simulation3_naive_roc,simulation3_naive_sens,simulation3_naive_spec,simulation3_naive_precision,simulation3_naive_f1)
  simulation3_final_result = round(rbind(simulation3_result,simulation3_result2,simulation3_result3_1,
                                         simulation3_naive1_result,simulation3_naive_result),3)
  colnames(simulation3_final_result) = c("Acc","AUC","Recall","Spec","Precision","F1")
  simulation3_sum = simulation3_sum + simulation3_final_result
}
{
Tan_cv_sss = Tan_cv_sens + Tan_cv_spec
Tan_cv_sss2 = Tan_cv_sens2 + Tan_cv_spec2
Tan_cv_sss3 = Tan_cv_sens3_1 + Tan_cv_spec3_1
Naive_cv_sss = Naive_cv_sens + Naive_cv_spec
Naive1_cv_sss = Naive1_cv_sens + Naive1_cv_spec

Tan_cv_result = c(mean(Tan_cv_acc),mean(Tan_cv_roc),mean(Tan_cv_sens),mean(Tan_cv_spec),mean(Tan_cv_precision),mean(Tan_cv_f1),mean(Tan_cv_mcc),mean(Tan_cv_sss))
Tan_cv_result2 = c(mean(Tan_cv_acc2),mean(Tan_cv_roc2),mean(Tan_cv_sens2),mean(Tan_cv_spec2),mean(Tan_cv_precision2),mean(Tan_cv_f12),mean(Tan_cv_mcc2),mean(Tan_cv_sss2))
Tan_cv_result3_1 = c(mean(Tan_cv_acc3_1),mean(Tan_cv_roc3_1),mean(Tan_cv_sens3_1),mean(Tan_cv_spec3_1),mean(Tan_cv_precision3_1),mean(Tan_cv_f13_1),mean(Tan_cv_mcc3_1),mean(Tan_cv_sss3))
Naive_cv_result = c(mean(Naive_cv_acc),mean(Naive_cv_roc),mean(Naive_cv_sens),mean(Naive_cv_spec),mean(Naive_cv_precision),mean(Naive_cv_f1),mean(Naive_cv_mcc),mean(Naive_cv_sss))
Naive1_cv_result = c(mean(Naive1_cv_acc),mean(Naive1_cv_roc),mean(Naive1_cv_sens),mean(Naive1_cv_spec),mean(Naive1_cv_precision),mean(Naive1_cv_f1),mean(Naive1_cv_mcc),mean(Naive1_cv_sss))
Tan_cv_result_sim = c(mean(Tan_cv_sim),mean(Tan_cv_sim2),mean(Tan_cv_sim4))
final_cv_result = round(rbind(Tan_cv_result,Tan_cv_result2,Tan_cv_result3_1,Naive1_cv_result,Naive_cv_result),3)
colnames(final_cv_result) = c("Acc","AUC","Sens","Spec","Precision","F1","MCC","SSS")

Tan_cv_result_sd = c(sd(Tan_cv_acc),sd(Tan_cv_roc),sd(Tan_cv_sens),sd(Tan_cv_spec),sd(Tan_cv_precision),sd(Tan_cv_f1),sd(Tan_cv_mcc),sd(Tan_cv_sss))
Tan_cv_result2_sd = c(sd(Tan_cv_acc2),sd(Tan_cv_roc2),sd(Tan_cv_sens2),sd(Tan_cv_spec2),sd(Tan_cv_precision2),sd(Tan_cv_f12),sd(Tan_cv_mcc2),sd(Tan_cv_sss2))
Tan_cv_result3_1_sd = c(sd(Tan_cv_acc3_1),sd(Tan_cv_roc3_1),sd(Tan_cv_sens3_1),sd(Tan_cv_spec3_1),sd(Tan_cv_precision3_1),sd(Tan_cv_f13_1),sd(Tan_cv_mcc3_1),sd(Tan_cv_sss3))
Naive_cv_result_sd = c(sd(Naive_cv_acc),sd(Naive_cv_roc),sd(Naive_cv_sens),sd(Naive_cv_spec),sd(Naive_cv_precision),sd(Naive_cv_f1),sd(Naive_cv_mcc),sd(Naive_cv_sss))
Naive1_cv_result_sd = c(sd(Naive1_cv_acc),sd(Naive1_cv_roc),sd(Naive1_cv_sens),sd(Naive1_cv_spec),sd(Naive1_cv_precision),sd(Naive1_cv_f1),sd(Naive1_cv_mcc),sd(Naive1_cv_sss))
Tan_cv_result_sim_sd = c(sd(Tan_cv_sim),sd(Tan_cv_sim2),sd(Tan_cv_sim4))
final_cv_result_sd = round(rbind(Tan_cv_result_sd,Tan_cv_result2_sd,Tan_cv_result3_1_sd,Naive1_cv_result_sd,Naive_cv_result_sd),3)
colnames(final_cv_result_sd) = c("Acc","AUC","Sens","Spec","Precision","F1","MCC","SSS")
}
final_cv_result
final_cv_result_sd

Tan_cv_result_sim
Tan_cv_result_sim_sd

write.csv(final_cv_result,paste("/Users/hyunwoo/Downloads/Simulation_new/TAN_Simulation_P=",p,"_Sample=",N,".csv"))
write.csv(final_cv_result_sd,paste("/Users/hyunwoo/Downloads/Simulation_new/TAN_Simulation_sd_P=",p,"_Sample=",N,".csv"))
write.csv(Tan_cv_result_sim,paste("/Users/hyunwoo/Downloads/Simulation_new/TAN_Simulation_sim_P=",p,"_Sample=",N,".csv"))
write.csv(Tan_cv_result_sim_sd,paste("/Users/hyunwoo/Downloads/Simulation_new/TAN_Simulation_sim_sd_P=",p,"_Sample=",N,".csv"))


par(mfrow=c(1,3))
plot(result_simulation3$nb,main = paste("TAN-I_p=",p),fontsize=50)
plot(result_simulation32$nb,main = paste("TAN-II_p=",p),fontsize=45)
plot(result_simulation34$nb,main = paste('TAN-IV_p=',p),fontsize=50)

par(mfrow=c(2,1))
plot(simulation3_naive1,fontsize=25)
plot(simulation3_naive,fontsize=20)
result_simulation34$nodes
dag$arcs
plot(dag)
par(mfrow=c(1,1))

