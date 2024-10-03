
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")

library(tidyverse)

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


ID=1:nrow(sub)

source("scripts/sim_functions.R")
source("scripts-supplement/rep_tx_functions.R")


registerDoParallel(5)
iter=1000


if (FALSE){
set.seed(1234)
r1 = plyr::rdply(iter,function(...)est_tx(model="glmnet",n.tune=5))
r2 = plyr::rdply(iter,function(...)est_tx(model="rf",n.tune=5))
r3 = plyr::rdply(iter,function(...)est_tx(model="bagEarth",n.tune=5))
r4 = plyr::rdply(iter,function(...)est_tx(model="svmPoly",n.tune=5))
r5 = plyr::rdply(iter,function(...)est_tx(model="avNNet",n.tune=5,linout=T,trace=F))


res = rbind(r1,r2,r3,r4,r5)
save(res,file="results-supplement/appE-rep-MORE.RData")

}

load("results-supplement/appE-rep-MORE.RData")

res.ML = res %>% select(.n, model, n.tune, ML.est, ML.SEhat, R2.OOS) %>%
  group_by(model,n.tune) %>%
  arrange(.n) %>% mutate(index=row_number()) %>% filter(n.tune==5, index<=1000)





out1 = res.ML %>% group_by(model) %>% 
  mutate(ME=1.96*ML.SEhat, LL=ML.est-ME, UL=ML.est+ME,
              CI.width=UL-LL,
              covers=ifelse(LL>0,1,0)) %>%
  summarise(n=n(), mean.est=mean(ML.est), sd.est=sd(ML.est), mean.SE=mean(ML.SEhat), 
            sd.SE=sd(ML.SEhat), mean.LL=mean(LL), mean.UL=mean(UL), 
            mean.R2=mean(R2.OOS), coverage=mean(covers))


out1= out1 %>% mutate(group = case_when(
  model %in% c("avNNet","nnet","pcaNNet") ~ "Neural Networks",
  model %in% c("enet","glmnet","ridge","BstLm","pcr") ~ "Linear Models",
  model%in%c("rf","RRFglobal","RRF","treebag","rpart") ~ "Tree-based Models",
  model %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
  model%in%c("svmLinear","svmPoly") ~ "Support Vector Machines"))



set.seed(1234)
est.sub = plyr::rdply(iter,run_sub,.progress="text")
est.oracle = plyr::rdply(iter, boot_oracle,.progress="text")

est.sub$model="subset"
est.oracle$model="oracle"

est2 = rbind(est.sub, est.oracle) %>% mutate(n.tune=NA, n=iter) %>% 
  select(model, n.tune, everything())

out2 = est2 %>% group_by(model) %>% mutate(ME=1.96*SE, LL=est-ME, UL=est+ME,
                                   CI.width=UL-LL, covers=ifelse(LL>0,1,0)) %>% 
  summarise(n=n(), mean.est=mean(est), sd.est=sd(est), mean.SE=mean(SE), sd.SE=sd(SE),
            mean.LL=mean(LL),mean.UL=mean(UL), mean.R2=NA,  coverage=mean(covers)) %>% 
  mutate(group=ifelse(model=="oracle", "Oracle Estimator","Subset Estimator"))


out1 = out1 %>%  arrange(group)

out3 = rbind(out2[1,],out1,out2[2,]) %>% select(-n,-mean.LL, -mean.UL)

out4 = out3 %>% mutate(avg.est=sprintf("%.3f (%.2f)", mean.est, sd.est),
                       avg.SE=sprintf("%.3f (%.3f)", mean.SE, sd.SE)) %>%
  select(group, avg.est, avg.SE, mean.R2, coverage)




knitr::kable(out4,format="latex",digits=3,col.names=c("Method","Avg. Est (SD)",
                                                      "Avg. SE (SD)", "Avg. R2", "Empirical Power"),
             booktabs=T, align=c("lccc"))

