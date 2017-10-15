f_multi_tree<-function(J2)
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
    
    #############
    ##############
    
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
    
    
    # level 1
    i=1
    
    unique_tmp1 = unique_level[CAT[,i]=="A"]
    unique_tmp2 = unique_level[CAT[,i]=="Collapse"]
    
    G_O_tmp = G_O
    G_O_tmp[G_O %in% unique_tmp2] = "Collapse"
    
    
    data_O = data.frame(X_O,G_O = G_O_tmp)
    
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
    
    
    # level >1
    for(i in 2:ncol(CAT))
    {
      unique_tmp1 = unique_level[CAT[,i]=="A"]
      unique_tmp2 = unique_level[CAT[,i]=="Collapse"]
      unique_tmp3 = unique_level[CAT[,i]=="0"]
      
      G_O_tmp = G_O
      G_O_tmp[G_O %in% unique_tmp2] = "Collapse"
      G_O_tmp[G_O %in% unique_tmp3] = "0"
      
      data_O = data.frame(X_O,G_O = G_O_tmp)
      
      data_O = data_O[G_O_tmp!="0",]
      
      thetree <- 1
      mindev = 1e-6
      while(length(thetree)==1)
      {
        mindev = mindev*10
        #        print(mindev)
        ###### Change required
        thetree <- try(tree(G_O ~ .,data=data_O, mindev = mindev))
      }
      
      if(sum(G_O_pred == "Collapse")>0)
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
    }
    
    if(J2==1)        
      print(paste("multi_tree -- J2",J2,"of",length(multi_tree)))
    
    if(J2%%100==0 & length(multi_tree)<=2000)        
      print(paste("multi_tree -- J2",J2,"of",length(multi_tree)))
    
    if(J2%%1000==0 & length(multi_tree)>2000)        
      print(paste("multi_tree -- J2",J2,"of",length(multi_tree)))
    
    output = list(Ind = which(synthetic_collapse == multi_tree[J2]), G = G_O_pred , N = nrow(X_S))
  }
  
  if(is.null(data_tmp$Tr))
    output = list(Ind = NULL, G = NULL, N =0)
  
  output
}


if(length(multi_tree)>split.multi.tree)
  Index = cbind(1+0:floor(length(multi_tree)/split.multi.tree)*split.multi.tree,c(1:floor(length(multi_tree)/split.multi.tree)*split.multi.tree,length(multi_tree)))

if(length(multi_tree)<=split.multi.tree)
  Index = cbind(1,length(multi_tree))

for(j in 1:nrow(Index))
{
  tmp = mclapply(Index[j,1]:Index[j,2],f_multi_tree,mc.cores=cores.multi.tree,mc.preschedule=TRUE)
  tmp_f3 = function(j){synthetic_Goal[tmp[[j]]$Ind,] <<- tmp[[j]]$G;NULL}
  sapply(1:length(Index[j,1]:Index[j,2]),tmp_f3)
}


