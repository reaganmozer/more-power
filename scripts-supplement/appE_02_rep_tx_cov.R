
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


pi0 = n0/sum(1-sub$Z)
pi1 = n1/sum(sub$Z)

Z = sub$Z
pretest=sub$pretest
pi = (1-Z)*pi0 + Z*pi1
ID=1:nrow(sub)


## With the selected sample, train a set of ML models and estimate model-assisted impacts for each
source("scripts/sim_functions.R")
source("scripts-supplement/rep_tx_functions.R")
n.tune = 5



# Take SRS to score


registerDoParallel(5)
iter=1000


if (FALSE){
  
set.seed(1234)
r1 = plyr::rdply(iter,function(...)est_tx_cov(model="glmnet",n.tune=5))
r2 = plyr::rdply(iter,function(...)est_tx_cov(model="rf",n.tune=5))
r3 = plyr::rdply(iter,function(...)est_tx_cov(model="bagEarth",n.tune=5))
r4 = plyr::rdply(iter,function(...)est_tx_cov(model="svmPoly",n.tune=5))
r5 = plyr::rdply(iter,function(...)est_tx_cov(model="avNNet",n.tune=5,linout=T,trace=F))

res = rbind(r1,r2,r3,r4,r5)
save(res,file="results-supplement/appE-rep-MORE-cov.RData")
}

load("results-supplement/appE-rep-MORE-cov.RData")

est1 = res %>% group_by(model,n.tune,term) %>%
  arrange(.n) %>% mutate(index=row_number()) %>% filter(n.tune==5, index<=1000) %>% select(-index) %>%
  mutate(group = case_when(
    model %in% c("avNNet","nnet","pcaNNet") ~ "Neural Networks",
    model %in% c("enet","glmnet","ridge","BstLm","pcr") ~ "Linear Models",
    model%in%c("rf","RRFglobal","RRF","treebag","rpart") ~ "Tree-based Models",
    model %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
    model%in%c("svmLinear","svmPoly") ~ "Support Vector Machines")) %>% arrange(group)


set.seed(1234)
est.sub = plyr::rdply(iter,run_sub_cov,.progress="text")
est.oracle = plyr::rdply(iter, boot_oracle_cov,.progress="text")


est.sub$model="subset"
est.oracle$model="oracle"

est2=rbind(est.sub, est.oracle) %>% mutate(n.tune=NA, R2.OOS=NA) %>%
  mutate(group=ifelse(model=="oracle", "Oracle Estimator","Subset Estimator")) %>% 
  select(names(est1))
  

out=rbind(est1, est2)

out2 = out %>% filter(term=="Z")%>% group_by(group, term) %>% 
  mutate(ME=1.96*std.error, LL=estimate-ME, UL=estimate+ME,
         CI.width=UL-LL,
         covers=ifelse(LL>0,1,0)) %>%
  summarise(n=n(), mean.est=mean(estimate), sd.est=sd(estimate), 
            mean.SE=mean(std.error),sd.SE=sd(std.error),
            mean.stat=mean(statistic),
            mean.LL=mean(LL), mean.UL=mean(UL), 
            mean.R2=mean(R2.OOS), coverage=mean(covers))



out3 =  out2 %>% mutate(avg.est=sprintf("%.3f (%.2f)", mean.est, sd.est),
               avg.SE=sprintf("%.3f (%.3f)", mean.SE, sd.SE)) %>%
  select(group, avg.est, avg.SE, mean.R2, coverage)


out3 = out3[c(4,1:3,6:7,5),]

knitr::kable(out3,format="latex",digits=3,col.names=c("Method","Avg. Est (SD)",
                                                      "Avg. SE (SD)", "Avg. R2", "Empirical Power"),
             booktabs=T, align=c("lccc"))




