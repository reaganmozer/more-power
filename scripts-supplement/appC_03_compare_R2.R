setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")



library(tidyverse)
load("generated data/prepped_ML_persuade.RData")
load("generated data/sim-results-main.RData")

load("results-supplement/appC-sim-results-embeddings.RData")

res.out$feats="curated"
res.embed$feats="embeddings"

res.out = rbind(res.out,res.embed)

res = res.out %>% group_by(model, n.coded,feats) %>% arrange(.n,seed) %>% mutate(index=row_number())
tapply(res$index,list(res$model,res$n.coded,res$feats),n_distinct)



res = res %>% mutate(ybar = (n.coded*ybar_train + (1000-n.coded)*ybar_test)/1000,
                     bias.ybar=ybar-mean(dat$Yobs))


# Pull best models from each group

best.mods = c("bagEarth","glmnet","avNNet","svmPoly","rf")


res1 = res %>% select(-ends_with("test"),-ends_with("train"),-any_of("seed"),R2_test) %>%
  tidyr::pivot_longer(cols=c(subset.est,ML.est,synth.est),names_to=c("meth"),values_to="est")
res1$meth=gsub(".est","",res1$meth)



res2 = res1 %>% filter(!is.na(est)) %>% tidyr::pivot_longer(cols=c(subset.SEhat,ML.SEhat,synth.SEhat), names_to=c("meth2"), values_to="SEhat")
res2$meth2=gsub(".SEhat","",res2$meth2)
res2 = res2 %>% filter(meth == meth2, !is.na(SEhat)) %>% select(-meth2)

res2 = res2 %>% tidyr::pivot_longer(cols=c(subset.SE,ML.SE,synth.SE), names_to=c("meth2"), values_to="SE")
res2$meth2=gsub(".SE","",res2$meth2)
res2 = res2 %>% filter(meth == meth2) %>% select(-meth2)

res2 = res2 %>% mutate(group = case_when(
  model %in% c("avNNet","nnet","pcaNNet") ~ "Neural Networks",
  model %in% c("enet","glmnet","ridge","BstLm","pcr") ~ "Linear Models",
  model%in%c("rf","RRFglobal","RRF","treebag","rpart","cforest") ~ "Tree-based Models",
  model %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
  model%in%c("svmLinear","svmPoly") ~ "Support Vector Machines"))
res2$group[res2$meth=="subset"]="Subset Estimator"
res2$group[res2$meth=="nominal"]="Oracle"

tau = 0
std.eff.size=0.25

res3 = res2 %>% filter(model%in%best.mods) %>% group_by( n.coded, group, meth, fit.type,feats) %>%
  mutate(ME = 1.96*SEhat,
         covers.nom = ifelse(tau>= est -ME  &tau<= est+ME, 1, 0),
         length.CI = 2*ME,
         rel.bias.est = (est-tau)/ifelse(tau>0,tau,1),
         rel.bias.SE = (SEhat-SE)/SE) %>%
  summarise(
    sd.est = sd(est),
    mean.SE = mean(SE),
    mean.SEhat = mean(SEhat),
    med.SEhat=median(SEhat),
    power = pnorm(2,mean=std.eff.size/sd.est,sd=1,lower.tail=F),
    MSE = mean(est^2),
    MSE.nom = mean(nominal.est^2),
    RE = MSE/MSE.nom,
    coverage=mean(covers.nom),
    length.CI=mean(length.CI),
    PRB = 100*mean(rel.bias.SE),
    PRB.est=100*mean(rel.bias.est),
    mean.R2=mean(R2_test))

pow.nom = pnorm(2,mean=std.eff.size/mean(res2$nominal.SE),sd=1,lower.tail=F)

cols = scale_color_manual(breaks=c("Adaptive & Rule-based Models","Linear Models","Neural Networks",
                                   "Support Vector Machines","Tree-based Models","Subset Estimator"),
                          values=c("red2","darkorange", "green4", "blue1","purple3", "black"),guide=guide_legend(""))

res3$feats=ifelse(res3$feats=="curated","Curated Features", "Embedding Features")

g6=ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=mean.R2,col=group))+geom_point()+geom_line()+theme_bw()+
  theme(panel.grid.minor=element_blank())+facet_wrap(~feats)+
  scale_y_continuous(limits=c(0.1,1),breaks=seq(0,1,by=0.1))+
  xlab("Number of coded documents (n)")+ylab("Mean R2 (Out-of-sample)")+cols+
  theme(legend.position = c(0.3,0.2)) +guides(col=guide_legend(""))+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))

ggsave("figures/supp-fig6.pdf", g6, width=7.5, height=5, units="in")

