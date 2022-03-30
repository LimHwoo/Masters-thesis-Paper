##################### 시력 건강검진 데이터 UCI with Sampling 
##################### TAN3 실험
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
################################ Data load
rm(list=ls())
{
  Tan_cv_acc = c()
  Tan_cv_sens = c()
  Tan_cv_spec = c()
  Tan_cv_precision = c()
  Tan_cv_f1 = c()
  Tan_cv_roc = c()
  Tan_cv_time1 = c()
  
  
  Tan_cv_acc2 = c()
  Tan_cv_sens2 = c()
  Tan_cv_spec2 = c()
  Tan_cv_precision2 = c()
  Tan_cv_f12 = c()
  Tan_cv_roc2 = c()
  Tan_cv_time2 = c()
  
  
  Tan_cv_acc3 = c()
  Tan_cv_sens3 = c()
  Tan_cv_spec3 = c()
  Tan_cv_precision3 = c()
  Tan_cv_f13 = c()
  Tan_cv_roc3 = c()
  Tan_cv_time3 = c()
  
  
  Tan_cv_acc3_1 = c()
  Tan_cv_sens3_1 = c()
  Tan_cv_spec3_1 = c()
  Tan_cv_precision3_1 = c()
  Tan_cv_f13_1 = c()
  Tan_cv_roc3_1 = c()
  Tan_cv_time4 = c()
  
  Naive_cv_sens=c()
  Naive_cv_spec = c()
  Naive_cv_precision = c()
  Naive_cv_acc = c()
  Naive_cv_f1 = c()
  Naive_cv_roc = c()
  Naive_cv_time = c()
  
  tree_cv_sens=c()
  tree_cv_spec = c()
  tree_cv_acc = c()
  tree_cv_precision = c()
  tree_cv_f1 = c()
  tree_cv_roc = c()
  tree_cv_time = c()
  
  knn_cv_sens=c()
  knn_cv_spec = c()
  knn_cv_acc = c()
  knn_cv_precision = c()
  knn_cv_f1 = c()
  knn_cv_roc = c()
  knn_cv_time = c()
  
  RF_cv_sens=c()
  RF_cv_spec = c()
  RF_cv_acc = c()
  RF_cv_precision = c()
  RF_cv_f1 = c()
  RF_cv_roc = c()
  RF_cv_time = c()
}
knn_hamming = function(traindata,class,testdata,k){
  new_class=NULL
  prob=NULL
  train = traindata %>% select(-class)
  test = testdata %>% select(-class)
  distance = function(a,b){
    testing=a
    for(i in 1:length(a)){
      testing[,i] = a[,i]!=b[,i]
    }
    dist=apply(testing,1,sum)
    return(dist)
  }
  
  for(j in 1:NROW(test)){
    if(j%%1000==0) print(j)
    dist=distance(train,test[j,])
    ind=names(sort(dist)[1:k])
    label=traindata[ind,] %>% select(class)
    p=sum(table(label)["1"])/k
    new_class[j]=names(sort(table(label),decreasing = T)[1])
    prob[j] = p
  }
  return(list(new_class=as.factor(new_class) , prob=prob))
}
source('/Users/hyunwoo/Desktop/Metrics.R')
source('/Users/hyunwoo/Desktop/cl_learning.R')

################################ Class
setwd("/Users/hyunwoo/Desktop/breast_cancer")
getwd()
data = read.table("breast-cancer.data",sep=",")
colnames(data) = c("Class","age","menopause","tumor_size","inv_nodes","node_caps","deg_malig","berast","breast_quad","irradiat")

na_index=c()
for(i in 1:NROW(data)){
  if(sum(data[i,]=='?')>=1){
    na_index=c(na_index,i)
  }
}
data = data[-na_index,]

for(i in 1:length(data)){
  data[,i]=as.factor(data[,i])
}
for(i in 1:length(data)){
  levels(data[,i])=c(0:(length(levels(data[,i]))-1))
}
data$age = as.integer(data$age)
data$age = ifelse(data$age<4,0,ifelse(data$age<5,1,2))
data$age = as.factor(data$age)
##############################################
################################# choose best rootvariable
bestroot = c()
root=c()
for( i in 1:100){
  print(i)
  for(j in 1:100){
    newindex = sample(1:NROW(data),NROW(data)/3,replace=F)
    testdata = data[newindex,]
    traindata = data[-newindex,]
    temp<- ubSMOTE(X= traindata[,-1],  Y= traindata$Class,
                   perc.over = 100, perc.under = 200,k = 5, verbose = FALSE)
    data_smote<-cbind(temp$X, temp$Y)
    colnames(data_smote)[10]<- "Class"
    traindata = data_smote
    result_Tan = cl_algorithm(class=class,data=traindata,root_='information',del=NULL)
    root = c(root,result_Tan$information[1,1])
  }
  bestroot = c(bestroot,names(sort(table(root),decreasing = T)[1]))
  root=c()
  }
root=names(sort(table(bestroot),decreasing = T)[1]) ############3 'inv_nodes'
#######################################################################
n=1
iter = sample(1:10000,100,replace=F)
class='Class'
root = 'inv_nodes'

for(x in iter){
  print(paste0("Strating ",n,"th Simulation..."))
  seed=x
  set.seed(seed)
  
  newindex = sample(1:NROW(data),NROW(data)/3,replace=F)
  testdata = data[newindex,]
  traindata = data[-newindex,]
  
  #############################Over-sampling
  #traindata = rbind(traindata,traindata[sample(which(traindata$Class==1),sum(traindata$Class==0)-sum(traindata$Class!=0),replace=T),])
  #traindata = traindata[sample(1:NROW(traindata),50000),]
  #############################Under-sampling
  #traindata = rbind(traindata[sample(which(traindata$Class==0),sum(traindata$Class==1)),],
  #                  traindata[which(traindata$Class==1),])
  ############### SMOTE
  temp<- ubSMOTE(X= traindata[,-1],  Y= traindata$Class,
                 perc.over = 100, perc.under = 200 , k = 7, verbose = FALSE)
  data_smote<-cbind(temp$X, temp$Y)
  colnames(data_smote)[10]<- "Class"
  traindata = data_smote
  traindata$class = as.factor(paste(traindata$Class,traindata$age,sep=''))
  testdata$class = as.factor(paste(testdata$Class,testdata$age,sep=''))
  
  ####################prediction
  
  print(paste0("Strating Tan Simulation...(",n,")"))
  start_Tantime1 = Sys.time()
  result_Tan = cl_algorithm(class=class,data=traindata,root_=root,del='class')
  results_Tan=learn_parameter(result_Tan$data,result_Tan,class=class,alpha=1) 
  fit_Tan = bnc('tan_cl','Class',traindata[,c(-11)],smooth=1)
  fit_Tan$.params=results_Tan
  end_Tantime1 = Sys.time()
  
  start_Tantime2 = Sys.time()
  result_Tan2 = cl_algorithm(class=class,data=traindata,root_=root,del=c('age','class'))
  results_Tan2=learn_parameter(result_Tan2$data,result_Tan2,class=class,alpha=1) 
  fit_Tan2 = bnc('tan_cl','Class',traindata[,c(-1,-11)],smooth=1)
  fit_Tan2$.params=results_Tan2
  end_Tantime2 = Sys.time()
  
  start_Tantime3 = Sys.time()
  result_Tan3 = cl_algorithm(class='class',data=traindata,root_=root,del=c('age','Class'))
  results_Tan3=learn_parameter(result_Tan3$data,result_Tan3,class='class',alpha=1) 
  fit_Tan3 = bnc('tan_cl','class',traindata[,c(-1,-10)],smooth=1)
  fit_Tan3$.params=results_Tan3
  end_Tantime3 = Sys.time()
  
  start_Tantime4 = Sys.time()
  result_Tan4 = cl_algorithm(class='class',data=traindata,root_= root,
                             del=c('age','Class'))
  results_Tan4=learn_parameter(result_Tan4$data,result_Tan4,class='class',alpha=1) 
  fit_Tan3_1 = bnc('tan_cl','class',traindata[,c(-1,-10)],smooth=1)
  fit_Tan3_1$.params=results_Tan4
  end_Tantime4 = Sys.time()
  
  par(mfrow=c(1,2))
  plot(result_Tan$nb);plot(result_Tan2$nb);plot(result_Tan3$nb);plot(result_Tan4$nb)
  
  pred_Tan=predict(fit_Tan,testdata)
  Tan_prob = predict(fit_Tan,testdata,prob=T)
  Tan_acc = accuracy(pred_Tan,testdata$Class)
  Tan_tab=table(pred_Tan,testdata$Class,dnn=c("pred","real"))
  Tan_sens = Sensitivity(testdata$Class,pred_Tan)
  Tan_spec = Specificity(testdata$Class,pred_Tan)
  Tan_precision = Precision(testdata$Class,pred_Tan)
  Tan_f1 = F1_Score(testdata$Class,pred_Tan)
  Tan_roc = roc(testdata$Class,Tan_prob[,2],quiet=T)$auc[1] ################### ROC / AUC
  
  pred_Tan2=predict(fit_Tan2,testdata)
  Tan_prob2 = predict(fit_Tan2,testdata,prob=T)
  Tan_acc2 = accuracy(pred_Tan2,testdata$Class)
  Tan_tab2 = table(pred_Tan2,testdata$Class,dnn=c("pred","real"))
  Tan_sens2 = Sensitivity(testdata$Class,pred_Tan2)
  Tan_spec2 = Specificity(testdata$Class,pred_Tan2)
  Tan_precision2 = Precision(testdata$Class,pred_Tan2)
  Tan_f12 = F1_Score(testdata$Class,pred_Tan2)
  Tan_roc2 = roc(testdata$Class,Tan_prob2[,2],quiet=T)$auc[1] ################### ROC / AUC
  
  pred_Tan3=predict(fit_Tan3,testdata)
  pred_Tan3 = as.factor(ifelse(as.integer(pred_Tan3)<4,0,1))
  Tan_prob3 = predict(fit_Tan3,testdata,prob=T)
  Tan_acc3 = accuracy(pred_Tan3,testdata$Class)
  Tan_tab3 = table(pred_Tan3,testdata$Class,dnn=c("pred","real"))
  Tan_sens3 = Sensitivity(testdata$Class,pred_Tan3)
  Tan_spec3 = Specificity(testdata$Class,pred_Tan3)
  Tan_precision3 = Precision(testdata$Class,pred_Tan3)
  Tan_f13 = F1_Score(testdata$Class,pred_Tan3)
  Tan_roc3 = roc(testdata$Class,apply(Tan_prob3[,4:6],1,sum),quiet=T)$auc[1] ################### ROC / AUC
  
  pred_Tan3_1=predict(fit_Tan3_1,testdata)
  pred_Tan3_1 = as.factor(ifelse(as.integer(pred_Tan3_1)<4,0,1))
  Tan_prob3_1 = predict(fit_Tan3_1,testdata,prob=T)
  Tan_acc3_1 = accuracy(pred_Tan3_1,testdata$Class)
  Tan_tab3_1 = table(pred_Tan3_1,testdata$Class,dnn=c("pred","real"))
  Tan_sens3_1 = Sensitivity(testdata$Class,pred_Tan3_1)
  Tan_spec3_1 = Specificity(testdata$Class,pred_Tan3_1)
  Tan_precision3_1 = Precision(testdata$Class,pred_Tan3_1)
  Tan_f13_1 = F1_Score(testdata$Class,pred_Tan3_1)
  Tan_roc3_1 = roc(testdata$Class,apply(Tan_prob3_1[,4:6],1,sum),quiet=T)$auc[1] ################### ROC / AUC
  
  # plot.roc(Tan_roc,   # roc를 계산한 value를 입력합니다.
  #          col="red",   # 선의 색상을 설정합니다.
  #          print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
  #          max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
  #          print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
  #          auc.polygon=TRUE, auc.polygon.col="#D1F2EB")
  ########################
  traindata = subset(traindata,select= -class)
  testdata = subset(testdata,select = -class)
  start_knntime=Sys.time()
  print(paste0("Strating KNN Simulation...(",n,")"))
  k=10
  id = sample(1:k,NROW(traindata),replace=TRUE)
  traindata$id=id
  k_result=list()
  k_ne = c(3,5,7,9)
  for(j in 1:length(k_ne)){
    valid_result=c()
    for(i in 1:k){
      train = traindata[which(traindata$id!=k),]
      valid = traindata[which(traindata$id==k),]
      trainlabel=train[,'Class']
      validlabel=valid[,'Class']
      valid_fit = knn_hamming(traindata=train[,-11],class='Class',testdata=valid[,-11],k=k_ne[j])
      valid_acc = accuracy(as.factor(valid_fit$new_class),as.factor(validlabel))
      valid_result = c(valid_result,valid_acc)
    }
    k_result[[j]]=mean(valid_result)
  }
  opt_k=k_ne[which.max(k_result)]
  traindata = subset(traindata,select=-id)
  test_label = testdata[,'Class']
  
  pred <- knn_hamming(traindata , class="Class",testdata, k=opt_k)
  knn_tab = table(pred$new_class,testdata$Class)
  
  knn_sens=Sensitivity(test_label,pred$new_class)
  knn_spec=Specificity(test_label,pred$new_class)
  knn_precision = Precision(test_label,pred$new_class)
  knn_f1 = F1_Score(test_label,pred$new_class)
  
  knn_acc = accuracy(pred$new_class,as.factor(test_label)) ############ Acc of KNN
  knn_roc = roc(test_label,pred$prob,quiet=T)$auc[1]################# AUC of KNN
  end_knntime=Sys.time()
  
  # plot.roc(knn_roc,   # roc를 계산한 value를 입력합니다.
  #          col="red",   # 선의 색상을 설정합니다.
  #          print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
  #          max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
  #          print.thres=TRUE, print.thres.pch=19, print.thres.col = "red", 
  #          auc.polygon=TRUE, auc.polygon.col="#D1F2EB")
  
  ###################### Naive Bayes
  start_Naivetime=Sys.time()
  print(paste0("Strating Naive Simulation...(",n,")"))
  
  fit3 = bnc('nb','Class',traindata,smooth=1)
  fit3_pred=predict(fit3,testdata)
  Naive_prob = predict(fit3,testdata,prob=T)
  Naive_tab = table(fit3_pred,testdata$Class)
  
  Naive_sens = Sensitivity(testdata$Class,fit3_pred)
  Naive_spec = Specificity(testdata$Class,fit3_pred)
  Naive_precision = Precision(testdata$Class,fit3_pred)
  Naive_f1 = F1_Score(testdata$Class,fit3_pred)
  
  Naive_acc = accuracy(fit3_pred,testdata$Class)  #####################3 Acc of Naive
  Naive_roc = roc(testdata$Class,Naive_prob[,2],quiet=T)$auc[1] ################# AUC of Naive
  end_Naivetime = Sys.time()
  # #plot.roc(Naive_roc,   # roc를 계산한 value를 입력합니다.
  #          col="red",   # 선의 색상을 설정합니다.
  #          print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
  #          max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
  #          print.thres=TRUE, print.thres.pch=19, print.thres.col = "red", 
  #          auc.polygon=TRUE, auc.polygon.col="#D1F2EB")
  
  ####################### Decision Tree (CART)
  start_treetime = Sys.time()
  print(paste0("Strating Tree Simulation...(",n,")"))
  
  tree_fit = rpart(Class~.,data=traindata , method='class')
  tree_fit = prune(tree_fit, cp= tree_fit$cptable[which.min(tree_fit$cptable[,"xerror"]),"CP"])
  
  tree_fit_pred = predict(tree_fit,testdata,type='class')
  tree_acc=accuracy(tree_fit_pred,testdata$Class)
  tree_fit_prob = predict(tree_fit,testdata)
  tree_tab = table(tree_fit_pred,testdata$Class)
  
  tree_sens = Sensitivity(testdata$Class,tree_fit_pred)
  tree_spec = Specificity(testdata$Class,tree_fit_pred)
  tree_precision = Precision(testdata$Class,tree_fit_pred)
  tree_f1 = F1_Score(testdata$Class,tree_fit_pred)
  tree_roc = roc(testdata$Class,tree_fit_prob[,2],quiet=T)$auc[1] ################# AUC of Tree
  
  end_treetime = Sys.time()
  # #plot.roc(tree_roc,   # roc를 계산한 value를 입력합니다.
  #          col="red",   # 선의 색상을 설정합니다.
  #          print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
  #          max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
  #          print.thres=TRUE, print.thres.pch=19, print.thres.col = "red", 
  #          auc.polygon=TRUE, auc.polygon.col="#D1F2EB")
  
  ########################## Random Forest
  
  ################################## cv
  start_rftime = Sys.time()
  print(paste0("Strating RandomForest Simulation...(",n,")"))
  
  k=10
  id = sample(1:k,NROW(traindata),replace=TRUE)
  traindata$id=id
  rf_result=list()
  trees = c(500,1000,2000)
  for(j in 1:length(trees)){
    valid_result=c()
    for(i in 1:k){
      train = traindata[which(traindata$id!=k),]
      valid = traindata[which(traindata$id==k),]
      valid_fit = randomForest(Class~.,data=subset(train, select=-id),mtry=sqrt(length(traindata)-1),ntree=trees[j])
      valid_pred = predict(valid_fit,newdata = subset(valid,select=-id),type='class')
      valid_acc = accuracy(valid_pred,valid[,'Class'])
      valid_result = c(valid_result,valid_acc)
    }
    rf_result[[j]]=mean(valid_result)
  }
  opt=trees[which.max(rf_result)]
  
  traindata = subset(traindata,select=-id)
  ################################# Fitting RF
  RF_fit=randomForest(Class~.,data=traindata , mtry=sqrt(length(data)-1), ntree=opt)
  RF_pred = predict(RF_fit, newdata = testdata , type='class')
  RF_prob = predict(RF_fit, newdata = testdata , type ='prob')
  
  RF_tab = table(RF_pred , testdata$Class)
  RF_acc = accuracy(RF_pred,testdata$Class)
  RF_sens = Sensitivity(testdata$Class,RF_pred)
  RF_spec = Specificity(testdata$Class,RF_pred)
  RF_precision = Precision(testdata$Class,RF_pred)
  RF_f1 = F1_Score(testdata$Class,RF_pred)
  
  RF_roc = roc(testdata$Class,RF_prob[,2],quiet=T)$auc[1]################# AUC of RF
  end_rftime = Sys.time()
  
  ################################ Time
  Tan_time1=end_Tantime1 - start_Tantime1
  Tan_time2=end_Tantime2 - start_Tantime2
  Tan_time3=end_Tantime3 - start_Tantime3
  Tan_time4=end_Tantime4 - start_Tantime4
  Naive_time=end_Naivetime - start_Naivetime
  Knn_time = end_knntime - start_knntime
  Tree_time = end_treetime - start_treetime
  RF_time = end_rftime - start_rftime
  
  ################################## Result
  Tan_cv_acc = c(Tan_cv_acc,Tan_acc)
  Tan_cv_sens = c(Tan_cv_sens,Tan_sens)
  Tan_cv_spec = c(Tan_cv_spec,Tan_spec)
  Tan_cv_roc = c(Tan_cv_roc,Tan_roc)
  Tan_cv_f1 = c(Tan_cv_f1,Tan_f1)
  Tan_cv_precision = c(Tan_cv_precision,Tan_precision)
  Tan_cv_time1 = c(Tan_cv_time1,Tan_time1)
  
  Tan_cv_acc2 = c(Tan_cv_acc2,Tan_acc2)
  Tan_cv_sens2 = c(Tan_cv_sens2,Tan_sens2)
  Tan_cv_spec2 = c(Tan_cv_spec2,Tan_spec2)
  Tan_cv_roc2 = c(Tan_cv_roc2,Tan_roc2)
  Tan_cv_f12 = c(Tan_cv_f12,Tan_f12)
  Tan_cv_precision2 = c(Tan_cv_precision2,Tan_precision2)
  Tan_cv_time2 = c(Tan_cv_time2,Tan_time2)
  
  Tan_cv_acc3 = c(Tan_cv_acc3,Tan_acc3)
  Tan_cv_sens3 = c(Tan_cv_sens3,Tan_sens3)
  Tan_cv_spec3 = c(Tan_cv_spec3,Tan_spec3)
  Tan_cv_roc3 = c(Tan_cv_roc3,Tan_roc3)
  Tan_cv_f13 = c(Tan_cv_f13,Tan_f13)
  Tan_cv_precision3 = c(Tan_cv_precision3,Tan_precision3)
  Tan_cv_time3 = c(Tan_cv_time3,Tan_time3)
  
  Tan_cv_acc3_1 = c(Tan_cv_acc3_1,Tan_acc3_1)
  Tan_cv_sens3_1 = c(Tan_cv_sens3_1,Tan_sens3_1)
  Tan_cv_spec3_1 = c(Tan_cv_spec3_1,Tan_spec3_1)
  Tan_cv_roc3_1 = c(Tan_cv_roc3_1,Tan_roc3_1)
  Tan_cv_f13_1 = c(Tan_cv_f13_1,Tan_f13_1)
  Tan_cv_precision3_1 = c(Tan_cv_precision3_1,Tan_precision3_1)
  Tan_cv_time4 = c(Tan_cv_time4,Tan_time4)
  
  Naive_cv_sens =c (Naive_cv_sens,Naive_sens)
  Naive_cv_spec = c(Naive_cv_spec,Naive_spec)
  Naive_cv_acc = c(Naive_cv_acc,Naive_acc)
  Naive_cv_roc = c(Naive_cv_roc,Naive_roc)
  Naive_cv_precision = c(Naive_cv_precision,Naive_precision)
  Naive_cv_f1 = c(Naive_cv_f1,Naive_f1)
  Naive_cv_time = c(Naive_cv_time,Naive_time)
  
  tree_cv_sens=c(tree_cv_sens,tree_sens)
  tree_cv_spec = c(tree_cv_spec,tree_spec)
  tree_cv_acc = c(tree_cv_acc,tree_acc)
  tree_cv_roc = c(tree_cv_roc,tree_roc)
  tree_cv_precision = c(tree_cv_precision,tree_precision)
  tree_cv_f1 = c(tree_cv_f1,tree_f1)
  tree_cv_time = c(tree_cv_time,Tree_time)
  
  knn_cv_sens=c(knn_cv_sens,knn_sens)
  knn_cv_spec = c(knn_cv_spec,knn_spec)
  knn_cv_acc = c(knn_cv_acc,knn_acc)
  knn_cv_roc = c(knn_cv_roc,knn_roc)
  knn_cv_precision = c(knn_cv_precision,knn_precision)
  knn_cv_f1 = c(knn_cv_f1,knn_f1)
  knn_cv_time = c(knn_cv_time,Knn_time)
  
  RF_cv_sens=c(RF_cv_sens,RF_sens)
  RF_cv_spec = c(RF_cv_spec,RF_spec)
  RF_cv_acc = c(RF_cv_acc,RF_acc)
  RF_cv_roc = c(RF_cv_roc,RF_roc)
  RF_cv_precision = c(RF_cv_precision,RF_precision)
  RF_cv_f1 = c(RF_cv_f1,RF_f1)
  RF_cv_time = c(RF_cv_time,RF_time)
  
  tan_result = c(Tan_acc,Tan_roc,Tan_sens,Tan_spec,Tan_precision,Tan_f1,Tan_time1)
  tan_result2 = c(Tan_acc2,Tan_roc2,Tan_sens2,Tan_spec2,Tan_precision2,Tan_f12,Tan_time2)
  tan_result3 = c(Tan_acc3,Tan_roc3,Tan_sens3,Tan_spec3,Tan_precision3,Tan_f13,Tan_time3)
  tan_result3_1 = c(Tan_acc3_1,Tan_roc3_1,Tan_sens3_1,Tan_spec3_1,Tan_precision3_1,Tan_f13_1,Tan_time4)
  naive_result = c(Naive_acc,Naive_roc,Naive_sens,Naive_spec,Naive_precision,Naive_f1,Naive_time)
  knn_result = c(knn_acc,knn_roc,knn_sens,knn_spec,knn_precision,knn_f1,Knn_time)
  tree_result = c(tree_acc,tree_roc,tree_sens,tree_spec,tree_precision,tree_f1,Tree_time)
  RF_result = c(RF_acc , RF_roc , RF_sens , RF_spec , RF_precision,RF_f1,RF_time)
  
  final_result = round(rbind(tan_result,tan_result2,tan_result3,tan_result3_1,
                             naive_result,knn_result,tree_result,RF_result),3)
  colnames(final_result) = c("Acc","AUC","Sens","Spec","Precision","F1",time)
  #write.csv(final_result,paste("eye_data_Class_cv_seed=",seed,".csv"))
  #capture.output(results,file=paste("eye_data_Class_seed=",seed,".txt"))
  n=n+1
}
###################### 

Tan_cv_result = c(mean(Tan_cv_acc),mean(Tan_cv_roc),mean(Tan_cv_sens),mean(Tan_cv_spec),mean(Tan_cv_precision),mean(Tan_cv_f1),mean(Tan_cv_time1))
Tan_cv_result2 = c(mean(Tan_cv_acc2),mean(Tan_cv_roc2),mean(Tan_cv_sens2),mean(Tan_cv_spec2),mean(Tan_cv_precision2),mean(Tan_cv_f12),mean(Tan_cv_time2))
Tan_cv_result3 = c(mean(Tan_cv_acc3),mean(Tan_cv_roc3),mean(Tan_cv_sens3),mean(Tan_cv_spec3),mean(Tan_cv_precision3),mean(Tan_cv_f13),mean(Tan_cv_time3))
Tan_cv_result3_1 = c(mean(Tan_cv_acc3_1),mean(Tan_cv_roc3_1),mean(Tan_cv_sens3_1),mean(Tan_cv_spec3_1),mean(Tan_cv_precision3_1),mean(Tan_cv_f13_1),mean(Tan_cv_time4))
tree_cv_result = c(mean(tree_cv_acc),mean(tree_cv_roc),mean(tree_cv_sens),mean(tree_cv_spec),mean(tree_cv_precision),mean(tree_cv_f1),mean(tree_cv_time))
Naive_cv_result = c(mean(Naive_cv_acc),mean(Naive_cv_roc),mean(Naive_cv_sens),mean(Naive_cv_spec),mean(Naive_cv_precision),mean(Naive_cv_f1),mean(Naive_cv_time))
knn_cv_result = c(mean(knn_cv_acc),mean(knn_cv_roc),mean(knn_cv_sens),mean(knn_cv_spec),mean(knn_cv_precision),mean(knn_cv_f1),mean(knn_cv_time))
RF_cv_result = c(mean(RF_cv_acc),mean(RF_cv_roc),mean(RF_cv_sens),mean(RF_cv_spec),mean(RF_cv_precision),mean(RF_cv_f1),mean(RF_cv_time))
final_cv_result = round(rbind(Tan_cv_result,Tan_cv_result2,Tan_cv_result3,Tan_cv_result3_1,Naive_cv_result,knn_cv_result,tree_cv_result,RF_cv_result),3)
colnames(final_cv_result) = c("Acc","AUC","Recall","Spec","Precision","F1","Time")

write.csv(final_cv_result,paste("UCI_Total.csv"))



################################# choose best rootvariable
bestroot = c()
root=c()
for( i in 1:100){
print(i)
for(j in 1:100){
newindex = sample(1:NROW(data),NROW(data)/3,replace=F)
testdata = data[newindex,]
traindata = data[-newindex,]
temp<- ubSMOTE(X= traindata[,-1],  Y= traindata$Class,
               perc.over = 100, perc.under = 200,k = 5, verbose = FALSE)
data_smote<-cbind(temp$X, temp$Y)
colnames(data_smote)[10]<- "Class"
traindata = data_smote
result_Tan = cl_algorithm(class=class,data=traindata,root_='information',del=NULL)
root = c(root,result_Tan$information[1,1])
}
bestroot = c(bestroot,names(sort(table(root),decreasing = T)[1]))
}
