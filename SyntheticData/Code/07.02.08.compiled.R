Goal.tmp = Goal 
for(J in ncol(Synthetic_Collapse):1)
{
  if(sum( Ind_synthetic_collapse[,J]) > 0)
  {
   Goal = Goal.tmp 

    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, Goal, "-- Year",J1))
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
    
    
    
    data_other_variables_tmp = data.frame(data_other_variables,collapse=Collapse[,J])
    data_synthetic_other_variables_tmp = data.frame(data_synthetic_other_variables, collapse=Synthetic_Collapse[,J])
    
    
    ## Start Algorithm
    data_general=data_other_variables_tmp[Ind_collapse[,J] ,,drop=FALSE]
    data_general=data_general[data_general$race != "0" ,,drop=FALSE]

    data_synthetic_general=data_synthetic_other_variables_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]

    variables=variables
    N.samples=1
    
    
    # Getting collapse for mono_table, mono_tree and multi_tree
    cores.table=30
    source("Code/07.02.01.Init_mono_multi.R")
    
    
    # Matching with mono_table
    pre.cores.table = 20
    cores.table = 30
    split.mono.table=15000
    cores.mono.table=40
    source("Code/07.02.02.mono_table.R")
    
    
    # Fitting mono_tree
    split.mono.tree=2000
    cores.mono.tree=30
    N.cartdraw = 10000
    cores.cartdraw = 2
    source("Code/07.02.03.mono_tree.R")
    
    
    # Fitting multi_tree
    if(length(multi_tree)>0)
    {
      BIG = 1e4
      backup_multi_tree = multi_tree
      source("Code/07.02.04.Init_multi_tree.R")
      # Big
      if(length(BIG_multi_tree)>0)
      {
        
        data_general_multi_tree_Big = data_general_multi_tree
        data_synthetic_general_multi_tree_Big = data_synthetic_general_multi_tree
        
        multi_tree = BIG_multi_tree
        N.split = min(c(BIG,1e4))
        cores.forest = 40
        N.cartdraw = 10000
        cores.cartdraw.pred = 4
        source("Code/07.02.05.Big_multi_tree.R")
      }
      # Small
      if(length(Small_multi_tree)>0)
      {
        multi_tree = Small_multi_tree
        split.multi.tree=5000
        cores.multi.tree=30
        N.cartdraw = 10000
        cores.cartdraw = 2
        source("Code/07.02.06.multi_tree.R")
      }
    }
    
    
    # End
    synthetic_variable[Ind_synthetic_collapse[,J]] = synthetic_Goal
    
  }
}

