cl_algorithm=function(class,data,root_='information',del){
  data = data %>% select(-del)
  cla = data[,class]
  data = data %>% 
    select(-class)
  data[,class]=cla
  #data = cbind(data,class=cla)
  result = 0
  cinformation=c()
  name=colnames(data)
  names= c()
  ################################## conditional mutual information
  for(i in 1:(length(data)-2)){
    for(j in (i+1):(length(data)-1)){
      fulljoint =table(data[,i],data[,j],data[,class])/NROW(data)
      prior = table(data[,class])/NROW(data)
      marginaljoint1 = table(data[,i],data[,class])/NROW(data)
      marginaljoint2 = table(data[,j],data[,class])/NROW(data)
      for(k in 1:length(levels(data[,class]))){
        for(a in 1:length(levels(data[,i]))){
          for(b in 1:length(levels(data[,j]))){
            nominate = log(prior[k]*(fulljoint[a,b,k] / ((marginaljoint1[a,k]) * (marginaljoint2[b,k]))))
            if(is.nan(nominate)==T||nominate==-Inf||is.na(nominate)==T) nominate =0
            result = result+sum(fulljoint[a,b,k]*(nominate))
          }
        }
      }
      names=c(names,name[i],name[j])
      cinformation = c(cinformation,result)  
      result = 0
    }
  }
  cinforresult=data.frame(data.frame(matrix(names,ncol=2,byrow=T)),cinformation) 
  index = order(cinforresult[,3],decreasing = T)
  nodes=cinforresult[index,]
  
  sum = 0
  information = c()
  variable=c()
  for(i in 1:(length(data)-1)){
    marginal = table(data[,i])/NROW(data)
    prior = table(data[,class])/NROW(data)
    joint = table(data[,i],data[,class])/NROW(data)
    for(j in 1:length(levels(data[,i]))){
      for(k in 1:length(levels(data[,class]))){
        nom = log(joint[j,k]/(prior[k]*marginal[j]))
        if(is.nan(nom)==T || nom==-Inf) nom=0
        sum = sum + sum(joint[j,k]*nom)
      }
    }
    
    variable=c(variable,name[i])
    information = c(information,sum)
    sum=0
  }
  infor = data.frame(variable,information)
  index2 = order(infor[,2],decreasing = T)
  infor = infor[index2,]
  ################################ Maximum spanning tree nodes
  tree=nodes[1,]
  i=1
  nnode=1
  
  while(i<NROW(nodes)){
    i=i+1
    index1=which(nodes[i,1]==nodes[1:(i-1),1])
    index2=which(nodes[i,2]==nodes[1:(i-1),1])
    index3=which(nodes[i,1]==nodes[1:(i-1),2])
    index4=which(nodes[i,2]==nodes[1:(i-1),2])
    totalindex=c(index1,index2,index3,index4)
    
    ss=0
    
    name1=nodes[totalindex,] %>%
      filter(nodes[totalindex,1]==nodes[i,1])
    name2=nodes[totalindex,] %>%
      filter(nodes[totalindex,1]==nodes[i,2])
    name3 = nodes[totalindex,] %>%
      filter(nodes[totalindex,2]==nodes[i,1])
    name4 = nodes[totalindex,] %>%
      filter(nodes[totalindex,2]==nodes[i,2])
    if(sum(unique(name1[,2] %in% c(name2[,2],name3[,1],name4[,1])))==1||
       sum(unique(name2[,2] %in% c(name1[,2],name3[,1],name4[,1])))==1||
       sum(unique(name3[,1] %in% c(name2[,2],name1[,2],name4[,1])))==1||
       sum(unique(name4[,1] %in% c(name2[,2],name3[,1],name1[,2])))==1){
      ss= ss+1
    }
    if((((sum(unique(nodes[i,1] == nodes[1:(i-1),1]))+
          sum(unique(nodes[i,2] == nodes[1:(i-1),1]))>=2) ||
         (sum(unique(nodes[i,1] == nodes[1:(i-1),2]))+
          sum(unique(nodes[i,2] == nodes[1:(i-1),2]))>=2)||
         (sum(unique(nodes[i,1] == nodes[1:(i-1),1]))+ 
          sum(unique(nodes[i,2] == nodes[1:(i-1),2]))>=2)||
         (sum(unique(nodes[i,1] == nodes[1:(i-1),2]))+ 
          sum(unique(nodes[i,2] == nodes[1:(i-1),1]))>=2))&&
        ss>=1)||
       sum(nodes[i,-3] %in% nodes[1:(i-1),1]) +sum(nodes[i,-3] %in% nodes[1:(i-1),2])>2)
    {
      next
    }
    tree = rbind(tree,nodes[i,])
    nnode = nnode+1
    if(nnode==(length(data)-2)) break
  }
  
  nb=tan_cl(class,data)
  
  if(root_=='information'){
    root = infor[1,1]
    tree = nb$.dag$edges[1:(NCOL(data)-2),]
    edges=c()
    inde =c()
    remain=c()
    nod=tree
    for(i in 1:NROW(nod)){
      if(nod[i,1]==root){
        edges=rbind(edges,data.frame("From"=nod[i,1],"To"=nod[i,2]))
        inde = c(inde,i)
      }
      if(nod[i,2]==root){
        edges=rbind(edges,data.frame("From"=nod[i,2],"To"=nod[i,1]))
        inde = c(inde,i)
      }
    }
    colnames(edges)=c("From","To")
    remain=as.data.frame(nod)[-inde,]
    while(1){
      if(NROW(remain)==0) break
      ind=c()
      for(i in 1:NROW(edges)){
        for(j in 1:NROW(remain)){
          if(edges[i,2]==remain[j,1]){
            edges = rbind(edges,data.frame("From"=remain[j,1],"To"=remain[j,2]))
            ind=c(ind,j)
          }
          if(edges[i,2]==remain[j,2]){
            edges = rbind(edges,data.frame("From"=remain[j,2],"To"=remain[j,1]))
            ind=c(ind,j)
          }
        }
      }
      
      
      remain = remain[-ind,]
      if(sum(class(remain)=='character')==1){
        remain=t(remain)
      }
      if(NROW(remain)==0) break
    }
    nb$.dag$edges= rbind(as.matrix(edges),nb$.dag$edges[(NROW(nb$.dag$edges)-NCOL(data)+2):NROW(nb$.dag$edges),])
    rownames(nb$.dag$edges)=1:NROW(nb$.dag$edges)
  }
  
  if(root_!='information'){
    root = root_
    tree = nb$.dag$edges[1:(NCOL(data)-2),]
    edges=c()
    inde =c()
    remain=c()
    nod=tree
    for(i in 1:NROW(nod)){
      if(nod[i,1]==root){
        edges=rbind(edges,data.frame("From"=nod[i,1],"To"=nod[i,2]))
        inde = c(inde,i)
      }
      if(nod[i,2]==root){
        edges=rbind(edges,data.frame("From"=nod[i,2],"To"=nod[i,1]))
        inde = c(inde,i)
      }
    }
    colnames(edges)=c("From","To")
    remain=as.data.frame(nod)[-inde,]
    while(1){
      ind=c()
      if(NROW(remain)==0) break
      for(i in 1:NROW(edges)){
        for(j in 1:NROW(remain)){
          if(edges[i,2]==remain[j,1]){
            edges = rbind(edges,data.frame("From"=remain[j,1],"To"=remain[j,2]))
            ind=c(ind,j)
          }
          if(edges[i,2]==remain[j,2]){
            edges = rbind(edges,data.frame("From"=remain[j,2],"To"=remain[j,1]))
            ind=c(ind,j)
          }
        }
      }
      
      
      remain = remain[-ind,]
      if(sum(class(remain)=='character')==1){
        remain=t(remain)
      }
      if(NROW(remain)==0) break
    }
    nb$.dag$edges= rbind(as.matrix(edges),nb$.dag$edges[(NROW(nb$.dag$edges)-NCOL(data)+2):NROW(nb$.dag$edges),])
    rownames(nb$.dag$edges)=1:NROW(nb$.dag$edges)
  }
  
  ave = sum(nodes[,3]/(length(data)*(length(data)-1)))
  rownames(tree)=c(1:NROW(tree))
  
  return(list(data=data,cinformation=nodes,nodes=tree,nb=nb,edge=nb$.dag$edges,ave=ave,information=infor,variable=unique(c(unique(tree[,1]),unique(tree[,2])))))
}
################################### learning parameter
# data,result,class,alpha          when alpha=0 => MLE
learn_parameter = function(data,result,class,alpha){
  classname=class
  cla = data[,class]
  data = data %>% 
    select(-class)
  data[,class]=cla
  #data = cbind(data,class=cla)
  name = colnames(data[1:(length(data)-1)])
  
  results=list()
  for(i in 1:length(name)){
    index=which(result$edge[,2]==name[i])
    num=unique(result$edge[index,])
    
    if(length(index)==1){
      tab = table(data[,name[i]],data[,class],dnn=c(name[i],class))
      for(j in 1:length(levels(data[,name[i]]))){
        nominate = (tab[j,]+alpha)/(table(data[,class])+alpha*length(levels(data[,name[i]])))
        for(a in 1:length(levels(data[,name[i]]))){
          if(is.nan(nominate[a])==T){
            nominate[a] = 1/length(levels(data[,name[i]]))
          }
        }
        tab[j,] = nominate
      }
      results[[name[i]]] = tab
    }
    else{
      tab = table(data[,name[i]],data[,num[1]],data[,class],dnn=c(name[i],num[1],class))
      for(k in 1:length(levels(data[,class]))){
        for(h in 1:length(levels(data[,name[i]]))){
          nominate = (tab[h,,k]+alpha)/(table(data[,num[1]],data[,class])[,k]+alpha*length(levels(data[,i])))
          
          for(a in 1:length(levels(data[,num[1]]))){
            if(is.nan(nominate[a])==T) {
              nominate[a] = 1/length(levels(data[,name[i]]))
            } 
          } 
          tab[h,,k] = nominate
        }
        results[[name[i]]] = tab
      }
    }
  }
  prior = (table(data[,class],dnn=classname)+alpha)/(NROW(data)+alpha*length(levels(data[,class])))
  results[[paste0(classname)]]=prior
  return(results)
}
