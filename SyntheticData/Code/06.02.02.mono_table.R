# Matching mono
if(length(mono)>0)
{
  print(paste("mono_table -- ",length(mono),"of total collapse", length(unique(data_general$collapse))))
  data_general = data_general_mono
  data_synthetic_general = data_synthetic_general_mono 
  
if(FALSE)
{    
  Ind = data_general$collapse %in% mono
  tmp_Goal = data_general$Goal[Ind]
  tmp_collapse = data_general$collapse[Ind]
  
  f<-function(j)
  {
    if(j==1)        
      print(paste("Pre - mono_table -- J",j,"of",length(mono)))  
    
    if(j%%1000==0)        
      print(paste("Pre - mono_table -- J",j,"of",length(mono)))  
    
    as.vector(tmp_Goal[mono[j]==as.vector(tmp_collapse)])
  }
  
  tmp = mclapply(1:length(mono),f,mc.cores=pre.cores.table)
  mono_table = cbind(as.vector(mono),as.vector(mono))
  for(j in 1:nrow(mono_table)){mono_table[j,2]=tmp[[j]][1]}
}

  mono_table  = cbind(as.vector(data_general$collapse),as.vector(data_general$Goal))

  unique_mono_table = unique(mono_table[,2])
  
 

  tmp_f_mono <- function(j)
  {
    if(j==1)        
      print(paste("Pre - mono_table -- J",j,"of",length(unique_mono_table)))
    
    if(j%%1000==0)        
      print(paste("Pre - mono_table -- J",j,"of",length(unique_mono_table)))  
    
    which(synthetic_collapse %in% mono_table[unique_mono_table[j] == mono_table[,2],1])
  }

  tmp = mclapply(1:length(unique_mono_table),tmp_f_mono, mc.cores=cores.mono.table,mc.preschedule=TRUE)

   tmp_f3 = function(j1){synthetic_Goal[tmp[[j1]],] <<- unique_mono_table[j1];NULL}
    sapply(1:length(unique_mono_table),tmp_f3)


if(FALSE)
{

 for(j in 1:length(unique_mono_table))
{
  synthetic_Goal[synthetic_collapse %in% mono_table[unique_mono_table[j] == mono_table[,2],1],] = unique_mono_table[j]
    if(j%%100==0)        
      print(paste("Pre - mono_table -- J",j,"of",length(unique_mono_table)))  

}

    
  tmp_f_mono <- function(j)
  {
    if(j==1)        
      print(paste("mono_table -- J",j,"of",nrow(mono_table)))  
    
    if(j%%1000==0)        
      print(paste("mono_table -- J",j,"of",nrow(mono_table)))  
    
    which(synthetic_collapse == mono_table[j,1])
  }
  
  if(length(mono) > split.mono.table)
    Index = cbind(1+0:floor(length(mono)/split.mono.table)*split.mono.table,c(1:floor(length(mono)/split.mono.table)*split.mono.table,length(mono)))
  
  if(length(mono) <= split.mono.table)
    Index = cbind(1,length(mono))
  
  for(j in 1:nrow(Index))
  {
    tmp = mclapply(Index[j,1]:Index[j,2],tmp_f_mono, mc.cores=cores.mono.table,mc.preschedule=TRUE)
    tmp_f3 = function(j1){synthetic_Goal[tmp[[j1]],] <<- mono_table[(Index[j,1]:Index[j,2])[j1],2];NULL}
    sapply(1:length(Index[j,1]:Index[j,2]),tmp_f3)
  }
}

}

