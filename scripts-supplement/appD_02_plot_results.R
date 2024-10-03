setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")



library(tidyverse)
load("generated data/prepped_ML_persuade.RData")
load("results-supplement/appD-sim-results-tx.RData")



res = res.tx %>% group_by(model, n.coded,type) %>% arrange(.n,seed) %>% mutate(index=row_number())
tapply(res$index,list(res$model,res$n.coded,res$type),n_distinct)

res = res %>% filter(index<=1000, !model%in%c("ridge","svmLinear"))


res = res %>% mutate(ybar = (n.coded*ybar_train + (1000-n.coded)*ybar_test)/1000,
                     bias.ybar=ybar-mean(dat$Yobs))



# Pull best models from each group

best.mods = c("bagEarth","earth","glmnet","avNNet","svmPoly","rf")


res1 = res %>% select(-ends_with("test"),-ends_with("train"),-any_of("seed")) %>%
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
  model%in%c("rf","RRFglobal","RRF","treebag","rpart") ~ "Tree-based Models",
  model %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
  model%in%c("svmLinear","svmPoly") ~ "Support Vector Machines"))
res2$group[res2$meth=="subset"]="Subset Estimator"
res2$group[res2$meth=="nominal"]="Oracle"

tau = res$tx[1]
std.eff.size=0.25

res3 = res2 %>% filter(model%in%best.mods) %>% group_by(n.coded, group, meth, type) %>%
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
    MSE = mean((est-tau)^2),
    MSE.nom = mean((nominal.est-tau)^2),
    RE = MSE/MSE.nom,
    coverage=mean(covers.nom),
    length.CI=mean(length.CI),
    PRB = 100*mean(rel.bias.SE),
    PRB.est=100*mean(rel.bias.est))

pow.nom = pnorm(2,mean=std.eff.size/mean(res2$nominal.SE),sd=1,lower.tail=F)

cols = scale_color_manual(breaks=c("Adaptive & Rule-based Models","Linear Models","Neural Networks",
                                   "Support Vector Machines","Tree-based Models","Subset Estimator"),
                          values=c("red2","darkorange", "green4", "blue1","purple3", "black"),guide=guide_legend(""))


res3$type=as.factor(res3$type)
levels(res3$type)=c("Combined Models","Separate Models")


g1=ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=PRB.est,col=group))+geom_line()+geom_point()+theme_bw()+
  geom_hline(yintercept =0,lty=2)+
  theme(panel.grid.minor=element_blank())+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-5,5,by=1), oob=scales::oob_squish)+
  xlab("Number of coded documents (n)")+ylab("Relative Bias of ATE Estimator (%)")+
  cols+  guides(col=guide_legend(""))+
    theme(legend.position = c(0.85,0.225), strip.text = element_text(face="bold")) +guides(col=guide_legend(""))+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))+facet_wrap(~type)
ggsave("figures/supp-appD-fig7.pdf", g1, width=8, height=5, units="in")



g2=ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=power,col=group))+geom_point()+geom_line()+theme_bw()+
  geom_hline(yintercept = pow.nom,col="lightslategray", lty=1)+
  facet_wrap(~type)+
  theme(panel.grid.minor=element_blank())+
  geom_smooth(data=res3[res3$meth=="subset",],aes(x=n.coded,y=power,col=group),lty=2,se=F)+
  scale_y_continuous(limits=c(0.1,1),breaks=seq(0,1,by=0.1))+
  xlab("Number of coded documents (n)")+ylab("Power")+cols+
  theme(legend.position = c(0.85,0.25), strip.text = element_text(face="bold")) +guides(col=guide_legend(""))+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))

ggsave("figures/supp-appD-fig8.pdf", g2, width=8, height=5, units="in")



g3=ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=RE,col=group))+geom_line()+geom_point()+theme_bw()+
  geom_hline(yintercept =1,lty=1,col="lightslategray")+
  facet_wrap(~type)+
  theme(panel.grid.minor=element_blank())+
  geom_smooth(data=res3[res3$meth=="subset",],aes(x=n.coded,y=RE,col=group),lty=2,se=F)+
  xlab("Number of coded documents (n)")+ylab("Relative Efficiency (RE)")+
  theme(legend.position = c(0.85,0.7), strip.text = element_text(face="bold")) +
  guides(col=guide_legend(""))+cols+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))




g4=ggplot(res3[res3$meth%in%c("ML","synth"),],aes(x=n.coded,y=coverage,lty=meth,col=group))+geom_line()+geom_point()+
  scale_y_continuous(limits=c(0,1),breaks=c(seq(0,0.9,0.1),0.95,1),oob=scales::oob_squish) + theme_bw()+
  geom_hline(yintercept =0.95,lty=2)+ facet_wrap(~type)+
  theme(panel.grid.minor=element_blank())+cols+guides(col=guide_legend(""))+
  xlab("Number of coded documents (n)")+ylab("Coverage")+labs(col="Model")+
  scale_linetype_discrete(guide="none")+
  theme(legend.position = c(0.825,0.25), strip.text = element_text(face="bold"))+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))

ggsave("figures/supp-appD-fig9.pdf", g4, width=8, height=5.5, units="in")
  


# Check PRB of variance estimator
ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=PRB,col=group))+geom_line()+geom_point()+theme_bw()+
  geom_hline(yintercept =0,lty=2)+
  theme(panel.grid.minor=element_blank())+
  scale_y_continuous(limits=c(-10,5),breaks=seq(-10,5,by=1), oob=scales::oob_squish)+
  xlab("Number of coded documents (n)")+ylab("Relative Bias of Variance Estimator(%)")+
  cols+  theme(legend.position = c(0.8,0.2)) +guides(col=guide_legend(""))+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))+facet_wrap(~type)




# Plot point estimates and CI's for each method and modeling type

if (FALSE){

res4 = res %>% select(.n, n.coded,model, nominal.est:ML.SEhat,var.yhat,type) %>% 
  pivot_longer(cols=c(subset.est,synth.est,ML.est), names_to="meth", values_to="est") %>% 
  pivot_longer(cols=c(subset.SEhat, synth.SEhat,ML.SEhat), names_to="meth2",values_to="SEhat") %>%
  mutate(meth=gsub(".est","",meth),
         meth2=gsub(".SEhat","",meth2)) %>% filter(meth==meth2) %>% select(-meth2)
cov = res4 %>% mutate(ME = 1.96*SEhat,
                      covers.nom = ifelse(tau>= est -ME  & tau<= est+ME, 1, 0),
                      length.CI = 2*ME,
                      rel.bias= (est-tau)/tau)


ests =  cov %>% group_by( n.coded, meth,model,type) %>% 
  summarise_at(vars(nominal.est:SEhat,est,SEhat, ME),mean)


ests =  cov %>% group_by( n.coded, meth,model,type) %>% 
  summarise_at(vars(nominal.est:SEhat,est,SEhat, ME),mean)



ests = ests %>%mutate(group = case_when(
  model %in% c("avNNet","nnet","pcaNNet") ~ "Neural Networks",
  model %in% c("enet","glmnet","ridge","BstLm","pcr") ~ "Linear Models",
  model%in%c("rf","RRFglobal","RRF","treebag","rpart") ~ "Tree-based Models",
  model %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
  model%in%c("svmLinear","svmPoly") ~ "Support Vector Machines"))
ests$group[ests$meth=="subset"]="Subset Estimator"
ests$group[ests$meth=="nominal"]="Oracle"

ests = ests %>% filter(model %in% best.mods)

ests.sub = ests%>% filter(meth=="subset") %>% select(-meth,-group) %>% mutate(n.coded=n.coded-10,meth2=0)

ests$meth2=as.numeric(factor(ests$meth,levels=c("subset","ML","synth")))-1
ggplot(ests[ests$meth%in%c("synth","ML"),],aes(x=n.coded+10*meth2,y=est,col=meth))+geom_point()+facet_grid(type~group)+
  geom_errorbar(aes(ymin=est-ME, ymax=est+ME))+geom_hline(yintercept=tau,lty=2)+theme_bw()+
  geom_point(data=ests.sub,aes(x=n.coded,y=est),col="black",alpha=0.1)+
  geom_errorbar(data=ests.sub,aes(ymin=est-ME,ymax=est+ME),col="black",alpha=0.1)+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))+xlab("Number of coded documents")+
  ylab("Estimated treatment effect")
}

