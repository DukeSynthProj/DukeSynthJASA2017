# Fitting mono trees
if(length(mono_tree)>0)
{
  
  data_general = data_general_mono_tree
  data_synthetic_general = data_synthetic_general_mono_tree
  
  Goal = as.vector(data_general$Goal)
  G = c(as.vector(Goal),rep("XX",nrow(data_synthetic_general)))
  data_general = data_general[,colnames(data_general) %in% variables]
  data_synthetic_general = data_synthetic_general[,colnames(data_synthetic_general) %in% variables]
  data_tmp1 = data.frame(data_general, Goal=Goal, Tr = 1)
  data_tmp2 = data.frame(data_synthetic_general, Goal=rep("XX",nrow(data_synthetic_general)), Tr = 0)
  data_collapse  = rbind(data_tmp1,data_tmp2)
  
  
  f_mono_tree<-function(J2)
  {
    Ind1 = as.vector(data_collapse$collapse) == mono_tree[J2]
    data_tmp = data.frame(as.matrix(cbind(data_collapse[which(Ind1),])))
    data_tmp = data_tmp[,apply(data_tmp,2,function(x){length(unique(x))})>1]    
    if(is.factor(data_tmp)==FALSE)    
    {
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
        
        data_O = data.frame(X_O,G_O = G_O)
        
        thetree <- 1
        mindev = 1e-6
        while(length(thetree)==1)
        {
          mindev = mindev*10
          thetree <- try(tree(G_O ~ .,data=data_O, mindev = mindev))
        }
        
        
        
        G_O_pred = matrix("0",nrow = nrow(X_S), ncol=N.samples)
        
        if(nrow(X_S)>N.cartdraw)
          for(j in 1:N.samples){G_O_pred[,j] = cartdraw2(thetree,data.frame(X_S),cores.cartdraw)}
        
        if(nrow(X_S)<=N.cartdraw)
          for(j in 1:N.samples){G_O_pred[,j] = cartdraw(thetree,data.frame(X_S))}
        
        if(J2==1)        
          print(paste("mono_tree -- J2",J2,"of",length(mono_tree)))
        
        if(J2%%100==0 & length(mono_tree)<=2000)        
          print(paste("mono_tree -- J2",J2,"of",length(mono_tree)))
        
        if(J2%%1000==0 & length(mono_tree)>2000)        
          print(paste("mono_tree -- J2",J2,"of",length(mono_tree)))
        
        output = list(Ind = which(synthetic_collapse == mono_tree[J2]), G = G_O_pred , N = nrow(X_S))
      }
      
      if(is.null(data_tmp$Tr))
        output = list(Ind = NULL, G = NULL, N =0)
    }
    
    if(is.factor(data_tmp))    
    {
      Ind = which(synthetic_collapse == mono_tree[J2])
      if(length(Ind)==0)
        output = list(Ind = NULL, G = NULL, N =0)
      
      if(length(Ind)>0)
        output = list(Ind = Ind, G = sample(as.vector(data_tmp),length(Ind),TRUE), N =length(Ind))
      
    }
    
    output
  }
  
  
  if(length(mono_tree)>split.mono.tree)
    Index = cbind(1+0:floor(length(mono_tree)/split.mono.tree)*split.mono.tree,c(1:floor(length(mono_tree)/split.mono.tree)*split.mono.tree,length(mono_tree)))
  
  if(length(mono_tree)<=split.mono.tree)
    Index = cbind(1,length(mono_tree))
  
  for(j in 1:nrow(Index))
  {
    tmp = mclapply(Index[j,1]:Index[j,2],f_mono_tree,mc.cores=cores.mono.tree,mc.preschedule=TRUE)
    tmp_f3 = function(j){synthetic_Goal[tmp[[j]]$Ind,] <<- tmp[[j]]$G;NULL}
    sapply(1:length(Index[j,1]:Index[j,2]),tmp_f3)
  }
  
}

