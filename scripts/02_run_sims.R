setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")


source("scripts/sim_functions.R")

load("generated data/prepped_ML_persuade.RData")

nc = seq(100,950,by=50)
mods = c("bagEarth","cforest","cubist","glmnet",
         "earth", "enet","pcr","rf","ridge","rpart",
         "RRFglobal","svmLinear","svmPoly")

nets = c("avNNet","nnet", "pcaNNet")

ML.grid = expand.grid(n.coded=nc, model=mods)
nets.grid = expand.grid(n.coded=nc, model=nets)


n.sim = 2000 # we ran in batches of 100 simulations on a cluster, change as needed for your configuration
n.tune = 4
tx = 0

seed = Sys.getpid()
set.seed(seed)

res.out = data.frame()

for (j in 1:nrow(ML.grid)){
  
  n.coded = ML.grid$n.coded[j]
  model = ML.grid$model[j]

  res = plyr::rdply( n.sim, one.trial(dat.ML,n.coded, n.tune=n.tune, model=model, tx=tx), .progress="text")
  res$n.tune = n.tune
  
  res.out = rbind(res.out, res)
  rm(res)
}





for (k in 1:nrow(nets.grid)){
  
  n.coded = nets.grid$n.coded[k]
  model = nets.grid$model[k]
  
  res = plyr::rdply( n.sim, one.trial(dat.ML,n.coded, n.tune=n.tune, model=model, tx=tx, trace=F, linout=T, MaxNWts=1500), .progress="text")
  res$n.tune = n.tune
  
  res.out = rbind(res.out, res)
  rm(res)
}


save(res.out, file="generated data/sim-results-main.RData")


