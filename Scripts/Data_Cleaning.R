# We normalized LA and LD by dividing by LT, similar to Nagappan
# and Ballfs approach. We also normalized LT and NUC by dividing by NF since
# these metrics have high correlation with NF.

## this function is used if we need to normalize the data. 
doNormalize <- function(data){
  idx.la <- charmatch(c("la"), colnames(data))
  tmp.la <- data["la"]/data["lt"]
  data[(data["lt"] >= 1), idx.la] <- tmp.la[(data["lt"] >= 1)]

  idx.ld <- charmatch(c("ld"), colnames(data))
  tmp.ld <- data["ld"]/data["lt"]
  data[(data["lt"] >= 1), idx.ld] <- tmp.ld[(data["lt"] >= 1)]

  idx.lt <- charmatch(c("lt"), colnames(data))
  tmp.lt <- data["lt"]/data["nf"]
  data[(data["nf"] >= 1), idx.lt] <- tmp.lt[(data["nf"] >= 1)]

  idx.npt <- charmatch(c("npt"), colnames(data))
  tmp.npt <- data["npt"]/data["nf"]
  data[(data["nf"] >= 1), idx.npt] <- tmp.npt[(data["nf"] >= 1)]

  # if the num of files is less than 2, entropy is not normalized
  idx.ent <- charmatch(c("entropy"), colnames(data))
  tmp.ent <- data["entropy"]/log(data["nf"],2)
  data[(data["nf"] >= 2), idx.ent] <- tmp.ent[(data["nf"] >= 2)]
  
  return (data)
}


## this function is used for undersampling. The output of the function is a data with equal number of buggy and clean data

# minority(buggy):1  majority(non-buggy):0
doSampling <- function(data, obj="bug"){
  minority = data[data[obj]==1,]
  majority = data[data[obj]==0,]
  majority = majority[order(runif(nrow(majority))),]
  majority =  majority[1:nrow(minority),]
  data = rbind(minority, majority)
  
  return (data)
}
