
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")

load("generated data/MORE-main.RData")


dat = all.info %>% rename(Yobs=score, Z=more) %>% select(-docID)

sub = dat[dat$grade==1 & dat$subject=="science",] %>% select(-grade, -subject)

sub$Yobs = sub$Yobs/sd(sub$Yobs[sub$Z==0])
sstats = sub %>% group_by( Z ) %>% 
  dplyr::summarise( Ybar = mean( Yobs ),
                    n = n(),
                    var = var(Yobs))

tau.hat = diff( sstats$Ybar)
tau.hat


SE.tau.hat = sqrt(sstats$var[1]/sstats$n[1] + sstats$var[2]/sstats$n[2])
SE.tau.hat


sub$pi.hat = 1 # SRS, so equal sampling weights for all docs
X = sub %>% select(-any_of( c( "s_id", "Z", "Yobs", "pi.hat" )))
names(X)[32]="sent_prim_regr"


## Select an initial sample of documents to "code" (i.e., reveal known human-coded outcomes)
## These will be used to train our ML models
fixed.coding.budget = floor(nrow(sub)/3) # assume we will only code 33% of corpus

ceiling(table(sub$Z)/3)

n0 = 213
n1=241


set.seed(1234)
ID=1:nrow(sub)

# Take SRS to score
ind0 = which(sub$Z==0)
ind1 = which(sub$Z==1)

samp0 = sample(ind0, size=n0)
samp1 = sample(ind1, size=n1)
coded=ifelse(ID%in%c(samp0, samp1),1,0)


## With the selected sample, train a set of ML models and estimate model-assisted impacts for each
source("scripts/sim_functions.R")

mod.seed = 123
n.tune = 5

set.seed(mod.seed)
seeds <- vector(mode = "list", length = 6)
for (i in 1:5){seeds[[i]]=sample.int(1000,5)}
seeds[[6]]=sample.int(1000,1)


models=  c("bagEarth","svmPoly","glmnet","rf")

res.srs = data.frame()


for (j in 1:length(models)){
  set.seed(mod.seed)
  fit = train_ensemble(X=X, Z=sub$Z, Yobs=sub$Yobs, coded=coded, n.tune=n.tune,model=models[j])
  yhat=predict_scores(fit=fit,X=X, Z=sub$Z)
  
  
  tmp= get_ML_est(dat=sub, coded=coded, yhat=yhat)
  tmp$model = models[j]
  resid = yhat-sub$Yobs
  tmp$R2.OOS=1-(var(resid)/var(sub$Yobs))
  res.srs = rbind(res.srs,tmp)
  
  print(j)
}


set.seed(mod.seed)
fit2 = train_ensemble(X=X, Z=sub$Z, Yobs=sub$Yobs, coded=coded, n.tune=n.tune,model="avNNet",linout=T, trace=F)

yhat2=predict_scores(fit=fit2,X=X, Z=sub$Z)
nets=get_ML_est(dat=sub, coded=coded, yhat=yhat2)
nets$model="avNNet"
resid = yhat2-sub$Yobs

nets$R2.OOS=1-(var(resid)/var(sub$Yobs))


res.srs = rbind(res.srs, nets)


out0 = res.srs[1,] %>% select(nominal.est, nominal.SE, subset.est, subset.SEhat)

out = res.srs %>% select(model, ML.est, ML.SEhat, synth.est, synth.SEhat, R2.OOS) %>% 
  mutate(ML.LL = ML.est - 1.96*ML.SEhat, ML.UL = ML.est + 1.96*ML.SEhat,
         synth.LL = synth.est - 1.96*ML.SEhat, synth.UL = synth.est+1.96*synth.SEhat) %>% 
  mutate(CI.ML=paste0("(", sprintf("%.2f",ML.LL), ", ", sprintf("%.2f",ML.UL), ")"),
         CI.synth=paste0("(", sprintf("%.2f",synth.LL), ", ", sprintf("%.2f",synth.UL), ")")) %>% 
  mutate(est1 = sprintf("%.3f (%.3f)", ML.est, ML.SEhat),
         est2 = sprintf("%.3f (%.3f)", synth.est, synth.SEhat))%>%
  mutate(R2.OOS=round(R2.OOS,digits=3))




out= out %>% mutate(group = case_when(
  model %in% c("avNNet","nnet","pcaNNet") ~ "Neural Networks",
  model %in% c("enet","glmnet","ridge","BstLm","pcr") ~ "Linear Models",
  model%in%c("rf","RRFglobal","RRF","treebag","rpart") ~ "Tree-based Models",
  model %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
  model%in%c("svmLinear","svmPoly") ~ "Support Vector Machines"))


out2 = out %>% select(group,  est1, CI.ML, R2.OOS) %>% arrange(group)

knitr::kable(out2, format="latex",digits=3,booktabs=T)
