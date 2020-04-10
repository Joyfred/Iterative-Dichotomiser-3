library(xlsx)
library(data.tree)
library(visNetwork)
transport = read.xlsx("data/transport.xlsx",1,stringsAsFactors=FALSE)
#transport = read.xlsx("data/Playtennis.xlsx",1,stringsAsFactors=FALSE)



dataset=transport
attribute_finder(dataset,0,0) 

attribute_finder=function(dataset,cur_attr_val,prev_iter_tree)
{
  #cat(sprintf("j=%d\n",j))
  #j<<-j+1 #determines the number of times this function is called
  #cat(sprintf("depth=%d\n",depth))
  IG1 = c()
  IG1 = IG_finder(dataset)
  print(IG1)
  attribute=names(which.max(IG1))
  t2 = prop.table( table( dataset[,attribute],dataset[,ncol(dataset)] ),1 )
  t2 = -(t2*log(t2,base=2))
  t2[is.nan(t2)] = 0 
  t2 = rowSums(t2)
  attr_val_with_non_zero_entry = names(t2[which(t2!=0)])
  attr_val_with_zero_entry = names(t2[which(t2==0)])
  
  print("impure")
  print(attr_val_with_non_zero_entry)
  print("pure")
  print(attr_val_with_zero_entry)
  
  edges=unique(dataset[,attribute])
  
  if(cur_attr_val==0)
  {
    tree<-Node$new(attribute)
    if(length(edges)!=0)
    {
      for(i in 1:length(edges))
      {
        tree$AddChild(edges[i])
        if(edges[i] %in% attr_val_with_zero_entry)
        {
          dummmy=FindNode(tree,edges[i])
          output=unique(subset(dataset,dataset[,attribute]==edges[i])[,ncol(dataset)])
          dummmy$AddChild(output)
        }
      }
      
    } 
  }
  else
  {
    a<-FindNode(prev_iter_tree,cur_attr_val)
    #print("printing a")
    #print(a)
    tree=a$AddChild(attribute)
    if(length(edges)!=0)
    {
      
      for(i in 1:length(edges))
      {
        tree$AddChild(edges[i])
        if(edges[i] %in% attr_val_with_zero_entry)
        {
          dummmy=FindNode(tree,edges[i])
          output=unique(subset(dataset,dataset[,attribute]==edges[i])[,ncol(dataset)])
          dummmy$AddChild(output)
        }
      }
    }
    #print("printing tree")
    #print(tree)
    #plot(tree)
    
  }
  
  if(length(attr_val_with_non_zero_entry)==0)
  {
    print("reached child")
    return (0)
  }
  
  for (i in 1:length(attr_val_with_non_zero_entry))
  {
    dataset1 = dataset[dataset[,attribute] == attr_val_with_non_zero_entry[i],]
    dataset1 = dataset1[,-which(names(dataset1)==attribute)]
    cat(sprintf("%s\n",attr_val_with_non_zero_entry[i]))
    print(dataset1)
    attribute_finder(dataset1,attr_val_with_non_zero_entry[i],tree)
  }
  print(tree)
  plot(tree)
}

IG_finder=function(data)
{
  IG=c()
  library(entropy)
  P_y=table( data[,ncol(data)] ) / length(data[,ncol(data)]) 
  H_y=entropy.empirical(P_y,unit="log2")
  
  for (i in 1:(ncol(data)-1))
  {
    #print(i)
    p1 = table(data[,i] )/length(data[,i])
    t1 = prop.table( table( data[,i],data[,ncol(data)] ),1 )
    t1 = -(t1*log(t1,base=2))
    t1[is.nan(t1)] <- 0 
    t1 = rowSums(t1)
    IG = c( IG , H_y-sum(t1*p1) )
  }
  names(IG)=colnames(data)[1:(ncol(data)-1)]
  return(IG)
}

#In-Built functions to print tree

# library(rpart)
# library(rpart.plot)
# tree=rpart(Play.Tennis~.,data=transport,method="class", control=rpart.control(minsplit=1, minbucket=1, cp=0.001,usesurrogate=3))
# rpart.plot(tree)