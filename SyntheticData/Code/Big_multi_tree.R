data_general = data_general_multi_tree_Big
data_synthetic_general = data_synthetic_general_multi_tree_Big

Goal = data_general$Goal
G = c(as.vector(Goal),rep("XX",nrow(data_synthetic_general)))
data_general = data_general[,colnames(data_general) %in% variables]
data_synthetic_general = data_synthetic_general[,colnames(data_synthetic_general) %in% variables]
data_tmp1 = data.frame(data_general, Goal=Goal, Tr = 1)
data_tmp2 = data.frame(data_synthetic_general, Goal=rep("XX",nrow(data_synthetic_general)), Tr = 0)
data_collapse  = rbind(data_tmp1,data_tmp2)

freq_multi_tree=simplify2array(mclapply(1:length(multi_tree),
                                        function(j){sum(as.vector(data_general_multi_tree$collapse)==multi_tree[j])},mc.cores=45))

multi_tree=multi_tree[order(freq_multi_tree,decreasing=T)]
freq_multi_tree=freq_multi_tree[order(freq_multi_tree,decreasing=T)]

for(J2 in 1:length(multi_tree))
{
  Ind1 = as.vector(data_collapse$collapse) == as.vector(multi_tree[J2])
  data_tmp = data.frame(as.matrix(cbind(data_collapse[which(Ind1),])))
  data_tmp = data_tmp[,apply(data_tmp,2,function(x){length(unique(x))})>1]    
  
  if(is.null(data_tmp$age)==FALSE){ data_tmp$age = as.numeric(data_tmp$age)}
  if(is.null(data_tmp$total_year)==FALSE){  data_tmp$total_year = as.numeric(data_tmp$total_year)}
  if(is.null(data_tmp$number_agency)==FALSE){  data_tmp$number_agency = as.numeric(data_tmp$number_agency)}
  if(is.null(data_tmp$M)==FALSE){  data_tmp$M = as.numeric(data_tmp$M)}
  
  
  if(is.null(data_tmp$Tr)==FALSE)
  {
    X=as.matrix(model.matrix(Goal~.,data=data_tmp))
    
    X=X[,apply(X,2,sd)>0,drop=FALSE]
    X_O = X[data_tmp$Tr==1,,drop=FALSE]
    X=X[,apply(X_O,2,sd)>0,drop=FALSE]
    X_O = X[data_tmp$Tr==1,,drop=FALSE]
    X_S = X[data_tmp$Tr==0,,drop=FALSE]
    G_O = as.vector(data_tmp$Goal[data_tmp$Tr==1])
    
    # Define the manner how the levels of G_O are going to be splitted
    M = length(unique(G_O))
    N = 25
    CAT=cbind(c(rep("A",N),rep("Collapse",M-N)))
    i=2
    while(M-i*N>0)
    {
      CAT=cbind(CAT,c(rep("0",(i-1)*N),c(rep("A",N),rep("Collapse",M-i*N))))
      i=i+1
    }
    CAT=cbind(CAT,c(rep("0",(i-1)*N),c(rep("A",M-(i-1)*N))))
    unique_level = names(sort(table(G_O),decreasing=T))
    
    
    f_trees = function(i)
    {
      # Fit multiple trees for a partition of the data
      BigTree <- function(j,Index)
      {
        thetree <- 1
        mindev = 1e-6
        while(length(thetree)==1)
        {
          mindev = mindev*10
          data_O_tmp = data_O[Index[j,1]:Index[j,2],]   
          thetree <- try(tree(G_O ~ .,data=data_O_tmp, mindev = mindev))
        }
        thetree
      }
      
      if(i==1)
      {
        unique_tmp1 = unique_level[CAT[,i]=="A"]
        unique_tmp2 = unique_level[CAT[,i]=="Collapse"]
        G_O_tmp = G_O
        G_O_tmp[G_O %in% unique_tmp2] = "Collapse"
        data_O = data.frame(X_O,G_O = G_O_tmp)
        if(nrow(data_O)<=N.split + 500)
        {
          thetree <- 1
          mindev = 1e-6
          while(length(thetree)==1)
          {
            mindev = mindev*10
            thetree <- try(tree(G_O ~ .,data=data_O, mindev = mindev))
          }
        }
        if(nrow(data_O)>N.split+500)
        {
          Index = cbind(1+0:floor(nrow(data_O)/N.split)*N.split,c(1:floor(nrow(data_O)/N.split)*N.split,nrow(data_O)))
          Index=Index[Index[,1]<=Index[,2],]
          thetree <- 1
          rm(thetree)
          thetree <- mclapply(1:nrow(Index),BigTree,Index=Index,mc.cores=cores.cartdraw)
          
        }
      }
      if(i>1)
      {
        unique_tmp1 = unique_level[CAT[,i]=="A"]
        unique_tmp2 = unique_level[CAT[,i]=="Collapse"]
        unique_tmp3 = unique_level[CAT[,i]=="0"]
        G_O_tmp = G_O
        G_O_tmp[G_O %in% unique_tmp2] = "Collapse"
        G_O_tmp[G_O %in% unique_tmp3] = "0"
        data_O = data.frame(X_O,G_O = G_O_tmp)
        data_O = data_O[G_O_tmp!="0",]
        if(nrow(data_O)<=N.split+500)
        {
          thetree <- 1
          mindev = 1e-6
          while(length(thetree)==1)
          {
            mindev = mindev*10
            thetree <- try(tree(G_O ~ .,data=data_O, mindev = mindev))
          }
        }
        
        if(nrow(data_O)>N.split+500)
        {
          Index = cbind(1+0:floor(nrow(data_O)/N.split)*N.split,c(1:floor(nrow(data_O)/N.split)*N.split,nrow(data_O)))
          #      rm(thetree)
          thetree <- mclapply(1:nrow(Index),BigTree,Index=Index,mc.cores=cores.cartdraw,mc.preschedule=FALSE)
        }
      }
      list(Ind =nrow(data_O)<=N.split+500,thetree=thetree)
    }
    
    print("############");print("############")
    print(paste("START Big multi_tree -- Year",J1," - J2",J2,"of",sum(freq_multi_tree>=BIG),"Sample Size",nrow(X_O),
                "Number of CAT", ncol(CAT) ))
    
    cores.cartdraw = 1
    
    if(ncol(CAT)<=cores.forest+1)
    {
      Forest = mclapply(1:ncol(CAT),f_trees,mc.cores=cores.forest)
    }
    
    if(ncol(CAT)>cores.forest+1)
    {
      Index = cbind(1+0:floor(ncol(CAT)/cores.forest)*cores.forest,c(1:floor(ncol(CAT)/cores.forest)*cores.forest,ncol(CAT)))
      Index=Index[Index[,1]<=Index[,2],,drop=FALSE]
      
      Forest = list()
      Forest = mclapply(Index[1,1]:Index[1,2],f_trees,mc.cores=cores.forest)
      
      for(j1  in 2:nrow(Index))
      {
        tmp = mclapply(Index[j1,1]:Index[j1,2],f_trees,mc.cores=cores.forest)
        for(j2 in Index[j1,1]:Index[j1,2]){ Forest[[j2]] = tmp[[j2-Index[j1,1]+1]]}
      }
    }
    
    
    # Prediction with BigTree at X_S
    cartdrawBIgTree<-function(thetree,X_S,cores.cartdraw)
    {
      tmp<-matrix(0,nrow=nrow(X_S),ncol=length(thetree))
      if(nrow(X_S)>N.cartdraw)
        for(j in 1:length(thetree)){tmp[,j] = cartdraw2(thetree[[j]],data.frame(X_S),cores.cartdraw)}
      if(nrow(X_S)<=N.cartdraw)
        for(j in 1:length(thetree)){tmp[,j] = cartdraw(thetree[[j]],data.frame(X_S))}
      apply(rbind(tmp),1,function(x){sample(x,1)})
    }
    
    cores.cartdraw = cores.cartdraw.pred    
    
    # level 1
    i=1
    G_O_pred = matrix("0",nrow = nrow(X_S), ncol=N.samples)
    print(paste("CAT = ",i , " number of CAT = ", ncol(CAT), "Left to predict" ,nrow(X_S)))
    thetree = Forest[[i]]$thetree
    if(Forest[[i]]$Ind==TRUE)
    {
      if(nrow(X_S)>N.cartdraw)
        for(j in 1:N.samples){G_O_pred[,j] = cartdraw2(thetree,data.frame(X_S),cores.cartdraw)}
      if(nrow(X_S)<=N.cartdraw)
        for(j in 1:N.samples){G_O_pred[,j] = cartdraw(thetree,data.frame(X_S))}
    }
    if(Forest[[i]]$Ind==FALSE)
      for(j in 1:N.samples){G_O_pred[,j] = cartdrawBIgTree(thetree,data.frame(X_S),cores.cartdraw)}
    
    
    # level >1
    for(i in 2:ncol(CAT))
    {
      
      print(paste("CAT = ",i , " number of CAT = ", ncol(CAT), "Left to predict" ,sum(G_O_pred == "Collapse")))
      
      if(sum(G_O_pred == "Collapse")>0)
      {
        thetree = Forest[[i]]$thetree
        if(Forest[[i]]$Ind==TRUE)
        {
          for(j in 1:N.samples)
          {
            if(sum(G_O_pred[,j]=="Collapse")>0)
            {
              Ind=G_O_pred[,j] == "Collapse"
              X_S_tmp = X_S[Ind,,drop=FALSE]
              
              if(nrow(X_S_tmp)>N.cartdraw)
                G_O_pred[Ind,j] = cartdraw2(thetree,data.frame(X_S_tmp),cores.cartdraw)
              
              if(nrow(X_S_tmp)<=N.cartdraw)
                G_O_pred[Ind,j] = cartdraw(thetree,data.frame(X_S_tmp))
            }
          }
        }
        
        if(Forest[[i]]$Ind==FALSE)
        {
          for(j in 1:N.samples)
          {
            if(sum(G_O_pred[,j]=="Collapse")>0)
            {
              Ind=G_O_pred[,j] == "Collapse"
              X_S_tmp = X_S[Ind,,drop=FALSE]
              G_O_pred[Ind,j] = cartdrawBIgTree(thetree,data.frame(X_S_tmp),cores.cartdraw)
            }
          }
        }
      }
    }
    
    
    #    if(J2==1)        
    print(paste("ENDS Big multi_tree -- Year",J1," - J2",J2,"of",sum(freq_multi_tree>=BIG)))
    
    if(J2%%100==0 & length(multi_tree)<=2000)        
      print(paste("multi_tree -- Year",J1," - J2",J2,"of",sum(freq_multi_tree>=BIG)))
    
    if(J2%%1000==0 & length(multi_tree)>2000)        
      print(paste("multi_tree -- Year",J1," - J2",J2,"of",sum(freq_multi_tree>=BIG)))
    
    output = list(Ind = which(synthetic_collapse == multi_tree[J2]), G = G_O_pred , N = nrow(X_S))
  }
  
  if(is.null(data_tmp$Tr))
    output = list(Ind = NULL, G = NULL, N =0)
  
  rm(Forest)
  rm(thetree)
  
  synthetic_Goal[output$Ind,] <- output$G
  
}
