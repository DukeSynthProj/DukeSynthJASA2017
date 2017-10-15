#CARTDRAW function used to make synthetic data in R

#need to have this library
library(tree)

#this is the key function--used to simulate from CART model 
cartdraw = function(thetree, newdata, maxgrid, usedens = 0, adjustbw1 = 1, adjustbw2=1, top=1, limit = 1000000000)
{
#thetree is the tree for the response variable
#new data is the dataset that you want to run down the tree-- it is the synthetic data
#usedens = 0 if you want to draw values from leaves w/o smoothing -- we will use this at least initially
#usedens = 1 if you want to draw values from leaves w/ smoothing
#fakeydata set to numeric for synthesis when size of synthetic and original file do not match 
#Need to use fakeydata = thetree$y when dimensions of newdata and old data match  -- we will do this
#limit is used for expanding density range for leaves with values above limit when smoothing 
#top = multiplier for max range
#adjustbw = bandwidth multiplier

fakeydata = numeric(length  = nrow(newdata))  #### MODIFY BY FELIPE
#fakeydata = thetree$y

#this finds the names of the nodes in the tree for the synthetic data
newdatawhere = predict(thetree, newdata, "where")
newtreetable = table(newdatawhere)
treenodes = names(newtreetable)
oldtreenodes = names(table(thetree$where))

#this picks the synthetic values
for(i in 1:length(treenodes))
{
if(is.element(treenodes[i], oldtreenodes)) 
eligibles = thetree$y[thetree$where == treenodes[i]] else {
leaves = as.numeric(row.names(thetree[[1]]))[as.numeric(treenodes[i])]
temptree = snip.tree(thetree,max(1,floor(leaves/2))) 
eligibles = temptree$y[temptree$where == (1:nrow(temptree[[1]]))[as.numeric(row.names(temptree[[1]])) == max(1,floor(leaves/2))]]
}

if(usedens == 0){
fakeydata[newdatawhere == treenodes[i]] = sample(eligibles, size = sum(newdatawhere == treenodes[i]), replace=T)
}

if(usedens == 1)
{

if(max(eligibles) > limit) {
densgrid = round(top*min(maxgrid, (max(eligibles)-min(eligibles)+1)),0)
xdens = density(eligibles, n=densgrid, from = min(eligibles), to = top*max(eligibles), adjust = adjustbw2)
}
else {
densgrid = min(maxgrid, (max(eligibles)-min(eligibles)+1))
xdens = density(eligibles, n=densgrid, from = min(eligibles), to = max(eligibles), adjust = adjustbw1)
}
xdens$y = xdens$y / sum(xdens$y)
fakeydata[newdatawhere == treenodes[i]] = sample(xdens$x, size = sum(newdatawhere == treenodes[i]), replace = T, prob = xdens$y)
}
}

#### MODIFY BY FELIPE
fakeydata=levels(thetree$y)[fakeydata]

fakeydata
}


