setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")


load("generated data/prepped_ML_persuade.RData")
load("generated data/sim-results-main.RData")

res = res.out %>% group_by(model, n.coded,fit.type) %>% arrange(.n,seed) %>% mutate(index=row_number())
tapply(res$index,list(res$model,res$n.coded,res$fit.type),n_distinct)


res = res %>% mutate(ybar = (n.coded*ybar_train + (1000-n.coded)*ybar_test)/1000,
                     bias.ybar=ybar-mean(dat$Yobs))



res1 = res.out %>% select(-ends_with("test"),-ends_with("train"),-any_of("seed")) %>%
  tidyr::pivot_longer(cols=ends_with(".est"),names_to=c("meth"),values_to="est")
res1$meth=gsub(".est","",res1$meth)



res2 = res1 %>% filter(!is.na(est)) %>% tidyr::pivot_longer(cols=ends_with(".SEhat"), names_to=c("meth2"), values_to="SEhat")
res2$meth2=gsub(".SEhat","",res2$meth2)
res2 = res2 %>% filter(meth == meth2, !is.na(SEhat)) %>% select(-meth2)

res2 = res2 %>% tidyr::pivot_longer(cols=ends_with(".SE"), names_to=c("meth2"), values_to="SE")
res2$meth2=gsub(".SE","",res2$meth2)
res2 = res2 %>% filter(meth == meth2) %>% select(-meth2)


res2= res2 %>% filter(meth!="synth") %>% mutate(group = case_when(
  model %in% c("avNNet","nnet","pcaNNet") ~ "Neural Networks",
  model %in% c("enet","glmnet","ridge","BstLm","pcr") ~ "Linear Models",
  model%in%c("rf","RRFglobal","RRF","treebag","rpart","cforest") ~ "Tree-based Models",
  model %in% c("earth","bagEarth","cubist") ~ "Adaptive & Rule-based Models",
  model%in%c("svmLinear","svmPoly") ~ "Support Vector Machine"))
res2$group[res2$meth=="subset"]="Subset Estimator"
res2$group[res2$meth=="nominal"]="Oracle"

# Pull 2,000 sims of nominal and subset since these are unaffected by the model
res2.nom = res2 %>% filter(meth=="nominal")%>% 
  filter(model=="rf") %>% 
  group_by(n.coded, group, model, meth, fit.type)%>% 
  summarise(n=n(), mean.est=mean(est), sd.est=sd(est),
            mean.SEhat=mean(SEhat), med.SEhat=median(SEhat), 
            sd.SEhat=sd(SEhat),
            mean.SE=mean(SE), med.SE=median(SE))

res2.sub = res2 %>% filter(meth=="subset")%>% 
  filter(model=="rf") %>% 
  group_by(n.coded, group, model, meth, fit.type)%>% 
  summarise(n=n(), mean.est=mean(est), sd.est=sd(est),
            mean.SEhat=mean(SEhat), med.SEhat=median(SEhat), 
            sd.SEhat=sd(SEhat),
            mean.SE=mean(SE), med.SE=median(SE))





res2.ML = res2 %>% filter(!meth%in%c("nominal","subset")) %>% 
  group_by( n.coded, group, model, meth,fit.type) %>% 
  summarise(n=n(), mean.est=mean(est), sd.est=sd(est),
            mean.SEhat=mean(SEhat), med.SEhat=median(SEhat), 
            sd.SEhat=sd(SEhat),
            mean.SE=mean(SE), med.SE=median(SE))


std.eff.size= 0.25

res3 = rbind(res2.nom, res2.sub, res2.ML)
res3 = res3 %>% mutate(power=pnorm(2,mean=std.eff.size/sd.est,sd=1,lower.tail=F))

res3.ML=res3%>% filter(meth=="ML")%>%mutate(model=as.character(model),group=as.character(group))
res.sub=res3%>%filter(meth=="subset") %>% as.data.frame()%>%select(-model, -group)

g0=ggplot(res3.ML,aes(x=n.coded,y=power,col=model))+geom_smooth(se=F,span=0.75)+theme_bw()+facet_wrap(~group,ncol=3)+
  geom_smooth(data=res.sub,aes(x=n.coded,y=power),col="black",lty=2,se=F)+
  geom_hline(yintercept=0.8,lty=3)+scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))+
  theme(panel.grid.minor=element_blank())+guides(col=guide_legend("Model"))+ylab("Power")+xlab("Number of documents coded (n)")+
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.1))+
  geom_hline(yintercept = 0.92, col="lightslategray")


ggsave("figures/supp-fig1.pdf", g0, width=9, height=7, units="in")




# Pull best models from each group

best.mods = c("bagEarth","glmnet","avNNet","svmPoly","rf")


res1 = res.out %>% select(-ends_with("test"),-ends_with("train"),-any_of("seed")) %>%
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

tau = 0

res3 = res2 %>% filter(model%in%best.mods) %>% group_by( n.coded, group, meth, fit.type) %>%
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
    PRB.est=100*mean(rel.bias.est))

pow.nom = pnorm(2,mean=std.eff.size/mean(res2$nominal.SE),sd=1,lower.tail=F)

cols = scale_color_manual(breaks=c("Adaptive & Rule-based Models","Linear Models","Neural Networks",
                                   "Support Vector Machines","Tree-based Models","Subset Estimator"),
                          values=c("red2","darkorange", "green4", "blue1","purple3", "black"),guide=guide_legend(""))


g1=ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=power,col=group))+geom_point()+geom_line()+theme_bw()+
  geom_hline(yintercept = pow.nom,col="lightslategray", lty=1)+
  theme(panel.grid.minor=element_blank())+
  geom_smooth(data=res3[res3$meth=="subset",],aes(x=n.coded,y=power,col=group),lty=2,se=F)+
  scale_y_continuous(limits=c(0.1,1),breaks=seq(0,1,by=0.1))+
  xlab("Number of coded documents (n)")+ylab("Power")+cols+
  theme(legend.position = c(0.75,0.3)) +guides(col=guide_legend(""))+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))

ggsave("figures/figure1.pdf", g1, width=5.5, height=3.75, units="in")


g2=ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=RE,col=group))+geom_line()+geom_point()+theme_bw()+
  geom_hline(yintercept =1,lty=1,col="lightslategray")+
  theme(panel.grid.minor=element_blank())+
  geom_smooth(data=res3[res3$meth=="subset",],aes(x=n.coded,y=RE,col=group),lty=2,se=F)+
  scale_y_continuous(limits=c(0.95,8.5),breaks=seq(1,10,by=1),labels=scales::percent, oob=scales::oob_squish)+
  xlab("Number of coded documents (n)")+ylab("Relative Efficiency (RE)")+
  theme(legend.position = c(0.75,0.7)) +guides(col=guide_legend(""))+cols+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))+geom_hline(yintercept = 1.5,lty=3)

ggsave("figures/figure2.pdf", g2, width=5.5, height=3.75, units="in")


g3=ggplot(res3[res3$meth=="ML",],aes(x=n.coded,y=PRB,col=group))+geom_line()+geom_point()+theme_bw()+
  geom_hline(yintercept =0,lty=2)+
  theme(panel.grid.minor=element_blank())+
  scale_y_continuous(limits=c(-10,5),breaks=seq(-10,5,by=1), oob=scales::oob_squish)+
  xlab("Number of coded documents (n)")+ylab("Relative Bias (%)")+cols+
  theme(legend.position = c(0.75,0.25)) +guides(col=guide_legend(""))+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))
ggsave("figures/figure3.pdf", g3, width=5.5, height=3.75, units="in")



g4=ggplot(res3[res3$meth%in%c("ML","synth"),],aes(x=n.coded,y=coverage,lty=meth,col=group))+geom_line()+geom_point()+
  scale_y_continuous(limits=c(0.18,1),breaks=c(seq(0.2,0.9,0.1),0.95,1),oob=scales::oob_squish) + theme_bw()+
  geom_hline(yintercept =0.95,lty=2)+
  theme(panel.grid.minor=element_blank())+
  xlab("Number of coded documents (n)")+ylab("Coverage")+labs(col="Model")+
  theme(legend.position = c(0.75,0.25)) +
  scale_linetype_discrete(name = "Estimator", labels = c("Model-assisted","Synthetic"),guide="none")+
  scale_x_continuous(limits=c(80,1000),breaks=seq(100,1000,by=100))+cols
ggsave("figures/figure4.pdf", g4, width=5.5, height=3.75, units="in")


