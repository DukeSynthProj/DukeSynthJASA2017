data_general = data_general_multi_tree
data_synthetic_general = data_synthetic_general_multi_tree

Goal = data_general$Goal
G = c(as.vector(Goal),rep("XX",nrow(data_synthetic_general)))
data_general = data_general[,colnames(data_general) %in% variables]
data_synthetic_general = data_synthetic_general[,colnames(data_synthetic_general) %in% variables]
data_tmp1 = data.frame(data_general, Goal=Goal, Tr = 1)
data_tmp2 = data.frame(data_synthetic_general, Goal=rep("XX",nrow(data_synthetic_general)), Tr = 0)
data_collapse  = rbind(data_tmp1,data_tmp2)

freq_multi_tree=simplify2array(mclapply(1:length(multi_tree),
                                        function(j){sum(as.vector(data_general_multi_tree$collapse)==multi_tree[j])},mc.cores=45))

multi_tree=multi_tree[order(freq_multi_tree,decreasing=F)]
freq_multi_tree=freq_multi_tree[order(freq_multi_tree,decreasing=F)]

BIG_multi_tree = as.vector(multi_tree[freq_multi_tree>BIG])
Small_multi_tree = as.vector(multi_tree[freq_multi_tree<=BIG])


