Ind_collapse = matrix(FALSE,nrow = nrow(Collapse),ncol = ncol(Collapse))
Ind_synthetic_collapse = matrix(FALSE,nrow = nrow(Synthetic_Collapse),ncol = ncol(Synthetic_Collapse))

# -- 1
Ind = Collapse[,1] %in% Synthetic_Collapse[,1]
unique_collapse = unique(Collapse[Ind])

Ind_synthetic_collapse[,1] = Synthetic_Collapse[,1] %in% unique_collapse
Ind_collapse[,1] = Collapse[,1] %in% unique_collapse
print(apply(Ind_synthetic_collapse,2,sum))


for(j in 2:ncol(Synthetic_Collapse))
{
  
  Ind = Collapse[,j] %in% Synthetic_Collapse[,j] 
  unique_collapse = unique(Collapse[Ind,j])
  tmp = unique(Synthetic_Collapse[apply(Ind_synthetic_collapse[,1:(j-1),drop=FALSE],1,sum)==0,j])
  
  unique_collapse = unique_collapse[unique_collapse %in% tmp]
  
  Ind_collapse[,j] = Collapse[,j] %in% unique_collapse
  
  Ind_synthetic_collapse[,j] = Synthetic_Collapse[,j] %in% unique_collapse & apply(Ind_synthetic_collapse[,1:(j-1),drop=FALSE],1,sum)==0
  
  print(apply(Ind_synthetic_collapse,2,sum)); print(table(apply(Ind_synthetic_collapse,1,sum)))
}

