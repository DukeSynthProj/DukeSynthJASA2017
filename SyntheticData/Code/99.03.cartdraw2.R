library(parallel)

#this is the key function--used to simulate from CART model 
cartdraw2 = function(thetree, newdata, ncores = 9)
{
num = nrow(newdata)
Index = cbind((ceiling((num-1)/ncores)*0:(ncores-1)+1), c((ceiling((num-1)/ncores)*1:(ncores-1)),num))
Index = Index[Index[,1]<=Index[,2] & Index[,1] <= num &  Index[,2] <= num ,,drop=FALSE]
Index[nrow(Index),2] = num



tmpf <- function(j){cartdraw(thetree,newdata[Index[j,1]:Index[j,2],,drop=FALSE])}
tmp1 <- mclapply(1:nrow(Index),tmpf,mc.cores=ncores)
tmp2 <- tmp1[[1]]
for(j in 2:nrow(Index)){tmp2 <- c(tmp2,tmp1[[j]])}

if(nrow(newdata) != length(tmp2))
{
print(paste("Error in cartdraw2"))
break()
}

tmp2
}


