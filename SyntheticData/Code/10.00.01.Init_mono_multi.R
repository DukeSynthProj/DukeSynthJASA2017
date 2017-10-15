# Defining mono, mono_tree and multi_tree
names(data_general)[names(data_general)==Goal]="Goal"
synthetic_agency = as.vector(data_synthetic_general$agency)

data_general = data_general[data_general$agency!="0",]
data_synthetic_general = data_synthetic_general[data_synthetic_general$agency!="0",]

Names = unique(data_general$collapse)

if(length(Names) > 1e4)
  Index = cbind(1+0:floor(length(Names)/2e3)*2e3,c(1:floor(length(Names)/2e3)*2e3,length(Names)))

if(length(Names) <= 1e4)
  Index = cbind(1,length(Names))


# Function to compute the number of levels of Goal in each collapse level

f<-function(j)
{
  Ind = data_general$collapse %in% Names[Index[j,1]:Index[j,2]]
  tmp_table = table(as.vector(data_general$collapse[Ind]),as.vector(data_general$Goal[Ind]))
  tmp_table_2 = tmp_table!=0
  tmp_table_2 = tmp_table_2*1
  tmp1 = apply(tmp_table_2,1,sum)
  
  print(paste("mono_multi_table -- Year",J1," - j",j,"of",nrow(Index)))
  
  data.frame(Names=names(tmp1),Count=as.vector(tmp1))
}

tmp = mclapply(1:nrow(Index),f,mc.cores=cores.table)
tmp_table = tmp[[1]]
if(nrow(Index)>1)
{
  for(j in 2:nrow(Index)){tmp_table=rbind(tmp_table,tmp[[j]])}
}


# Determing mono, mono_tree and multi_tree and the corresponding subsets
Names = tmp_table[,1]
tmp=tmp_table[,2]

mono = Names[tmp==1]
mono_tree = Names[tmp>1 & tmp<30]
multi_tree = Names[tmp>=30]

data_general_mono = data_general[data_general$collapse %in% mono,]
data_synthetic_general_mono = data_synthetic_general[data_synthetic_general$collapse %in% mono,]

data_general_mono_tree = data_general[data_general$collapse %in% mono_tree,]
data_synthetic_general_mono_tree = data_synthetic_general[data_synthetic_general$collapse %in% mono_tree,]

data_general_multi_tree = data_general[data_general$collapse %in% multi_tree,]
data_synthetic_general_multi_tree = data_synthetic_general[data_synthetic_general$collapse %in% multi_tree,]

synthetic_Goal = matrix("0",nrow = length(data_synthetic_general$collapse),ncol=N.samples)
synthetic_collapse = as.vector(data_synthetic_general$collapse)

