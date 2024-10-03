
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")


library(tidyverse)

load("generated data/MORE-main.RData")
main=read.csv("raw data/MORE_study_main.csv")


dat2 = merge(select(main, s_id,grade,subject,more,s_maprit_1819w), by=c("s_id","grade","subject","more"),all.info)


dat = dat2 %>% rename(Yobs=score, Z=more, pretest=s_maprit_1819w) %>% select(-docID)

sub = dat[dat$grade==1 & dat$subject=="science",] %>% select(-grade, -subject)

# Standardize pre-test and outcomes
sub$pretest=scale(sub$pretest)
sub$Yobs = sub$Yobs/sd(sub$Yobs[sub$Z==0])
sstats = sub %>% group_by( Z ) %>% 
  dplyr::summarise( Ybar = mean( Yobs),
                    n = n(),
                    var = var(Yobs))

tau.hat = diff( sstats$Ybar)
tau.hat


SE.tau.hat = sqrt(sstats$var[1]/sstats$n[1] + sstats$var[2]/sstats$n[2])
SE.tau.hat




sub$pi.hat = 1 # SRS, so equal sampling weights for all docs
X = sub %>% select(-any_of( c( "s_id", "Z", "Yobs", "Yobs.std","pi.hat" )))
names(X)=gsub("-","_",names(X),fixed=T)
X$pretest=X$pretest[,1]

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






pi0 = n0/sum(1-sub$Z)
pi1 = n1/sum(sub$Z)

Z = sub$Z
pi = (1-Z)*pi0 + Z*pi1

y.out=list()
pretest=sub$pretest


est_tx = function(X, sub, coded, mod,mod.seed,...){
  
  set.seed(mod.seed)
  fit = train_ensemble(X=X, Z=sub$Z, Yobs=sub$Yobs, coded=coded, n.tune=n.tune,model=mod,...)
  yhat=predict_scores(fit=fit,X=X, Z=sub$Z)
  resid = yhat-sub$Yobs
  R2.OOS=1-(var(resid)/var(sub$Yobs))
  cat(sprintf("Model: %s, R2=%.3f\n", mod, R2.OOS))
  
  Y.adj = yhat - (coded/pi)*(yhat-sub$Yobs)
  mod=lm(Y.adj ~ pretest + Z)

  return(mod)
}

out = lapply(models, function(x) est_tx(X,sub,coded,mod=x,mod.seed))

mod.oracle= lm(Yobs ~ pretest + Z, data=sub)

mod.subset = lm(Yobs ~ pretest + Z, data=sub[coded==1,])

mod.net = est_tx(X,sub,coded,mod="avNNet",mod.seed,linout=T,trace=F)

texreg::screenreg(c(list(mod.oracle,mod.subset),out, list(mod.net)), 
                  custom.model.names=c("Oracle","Subset Only",models,"avNNet"),
                  custom.coef.map=list("Intercept"=NA, "pretest"="Pre-test score","Z"="MORE"))



tab=texreg::get.data(c(list(mod.oracle,mod.subset),out, list(mod.net)), 
                    custom.model.names=c("Oracle","Subset Only",models,"avNNet"))
names(tab)=c("Oracle","Subset Only",models,"avNNet")

tab.out = data.frame()
for (j in 1:length(tab)){
  t=tab[[j]]
  tmp=data.frame(mod=names(tab)[j], coef=t@coef.names, est=t@coef, se=t@se,p=t@pvalues)
  tab.out=rbind(tab.out,tmp)
}
rownames(tab.out)=NULL

tab.out2 = tab.out %>% filter(coef!="(Intercept)") %>% select(-p)%>%
  mutate(est_out=sprintf("%.3f (%.2f)", est, se),
          LL=est-1.96*se, UL=est+1.96*se, 
          CI=paste0("(", sprintf("%.2f",LL), ", ", sprintf("%.2f",UL), ")")) %>% 
  pivot_wider(names_from=coef, values_from=c(est,se,est_out,LL,UL,CI)) %>% 
  select(mod, ends_with("Z"), ends_with("pretest"))

tab.est = tab.out2 %>% select(mod, est_out_pretest, CI_pretest, est_out_Z, CI_Z) %>%
  rename(est.x=est_out_pretest, CI.x=CI_pretest, est.z=est_out_Z, CI.z=CI_Z)



tab.est=tab.est %>% mutate(model= case_when(
  mod %in% c("avNNet","nnet","pcaNNet") ~ "Neural Networks",
  mod %in% c("enet","glmnet","ridge","BstLm","pcr") ~ "Linear Models",
  mod%in%c("rf","RRFglobal","RRF","treebag","rpart") ~ "Tree-based Models",
  mod %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
  mod%in%c("svmLinear","svmPoly") ~ "Support Vector Machines",
  mod=="Oracle" ~ "True Value (Oracle)",
  mod=="Subset Only" ~ "Subset Estimator"),
  ord = case_when(mod=="Oracle" ~ 1, mod=="Subset Only"~3, !mod%in%c("Oracle","Subset Only")~2)) %>%
  arrange(ord,model) %>% 
  select(model, est.x, est.z)


knitr::kable(tab.est, format="latex",digits=3,booktabs=T)
