setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")



load("generated data/prepped_ML_persuade.RData")


set.seed(12345)
kk = kmeans(dat[,-c(1:2)],3)
table(kk$cluster)
cor(kk$cluster,dat$Yobs)


group = ifelse(kk$cluster==1, 0, 1) # make treatment and control groups with different feature distributions to induce treatment effect
tau = diff(tapply(dat$Yobs,group,mean))
tau.std=diff(tapply(scale(dat$Yobs,center=F),group,mean))
print(c(tau,tau.std))

tapply(dat$Yobs,group, sd)

dat$Z = group




## 3. Run simulations with the embeddings feature set
source("scripts/sim_functions.R")


nc = seq(100,950,by=50)
mods = c("bagEarth","cforest","cubist","glmnet",
         "earth", "enet","pcr","rf","ridge","rpart",
         "RRFglobal","svmLinear","svmPoly","avNNet","nnet", "pcaNNet")

ML.grid = expand.grid(n.coded=nc, model=mods, fit.type=c("separate","combined"))

n.sim = 1000 # we ran in batches of 100 simulations on a cluster, change as needed for your configuration
n.tune = 4
tx = tau

seed = Sys.getpid()
set.seed(seed)
res.tx = data.frame()

for (j in 1:nrow(ML.grid)){
  
  n.coded = ML.grid$n.coded[j]
  model = ML.grid$model[j]
  
  if (!model %in% c("avNNet","nnet","pcaNNet")){
    res = plyr::rdply( n.sim, one.trial(dat.ML,n.coded, n.tune=n.tune, model=model,fit.type=fit.type, tx=tx), .progress="text")
  }
  else if (model %in% c("avNNet","nnet","pcaNNet")){
    res = plyr::rdply( n.sim, one.trial(dat.ML,n.coded, n.tune=n.tune, model=model, fit.type=fit.type, tx=tx, trace=F, linout=T, MaxNWts=1500), .progress="text")
    
  }
  res$n.tune = n.tune
  
  res.tx = rbind(res.tx, res)
  rm(res)
}


save(res.tx, file="results-supplement/appD-sim-results-tx.RData")
