# Calculates the estimated treatment effect (std. difference in means)
# using the model-assisted estimator with a random sample of n0 and n1 docs coded in control and treatment groups

est_tx = function(model, n.tune,...){
  
  samp0 = sample( which(sub$Z==0), size=n0)
  samp1 = sample(which(sub$Z==1), size=n1)
  coded=ifelse(ID%in%c(samp0, samp1),1,0)
  
  
  ## With the selected sample, train ML model and 
  # estimate model-assisted impact
  
  fit = train_ensemble(X=X, Z=sub$Z, Yobs=sub$Yobs, coded=coded, n.tune=n.tune,model=model,...)
  yhat=predict_scores(fit=fit,X=X, Z=sub$Z)
  
  
  res= get_ML_est(dat=sub, coded=coded, yhat=yhat)
  res$model = model
  resid = yhat-sub$Yobs
  res$R2.OOS=1-(var(resid)/var(sub$Yobs))
  
  res
}


# Calculates the subset-only estimate of the treatment effect (std. diff in means)
# based on random sample of n0 and n1 docs
run_sub=function(){
  samp0 = sample(which(sub$Z==0), size=n0)
  samp1 = sample(which(sub$Z==1), size=n1)
  coded=ifelse(ID%in%c(samp0, samp1),1,0)
  
  boot = sub %>% filter(coded==1)
  sstats = boot %>% group_by( Z ) %>% 
    dplyr::summarise( Ybar = mean( Yobs ),
                      n = n(),
                      var = var(Yobs))
  
  tau.hat = diff( sstats$Ybar)
  SE.tau.hat = sqrt(sstats$var[1]/sstats$n[1] + sstats$var[2]/sstats$n[2])
  
  out=data.frame(est=tau.hat, SE=SE.tau.hat)
  return(out)
  
  
}




# Calculates the oracle estimate of the treatment effect (std. diff in means) 
# by bootstrap resampling the full data set
boot_oracle=function(){
  samp=sample(1:nrow(sub),replace=T)
  boot=sub[samp, ]
  sstats = boot %>% group_by( Z ) %>% 
    dplyr::summarise( Ybar = mean( Yobs ),
                      n = n(),
                      var = var(Yobs))
  
  tau.hat = diff( sstats$Ybar)
  SE.tau.hat = sqrt(sstats$var[1]/sstats$n[1] + sstats$var[2]/sstats$n[2])
  
  out=data.frame(est=tau.hat, SE=SE.tau.hat)
  return(out)
  
  
}

# Calculates the estimated treatment effect after adjusting for pre-test scores
# using the model-assisted estimator with a random sample of n0 and n1 docs coded in control and treatment groups
est_tx_cov = function(model,n.tune,...){
  
  samp0 = sample(which(sub$Z==0), size=n0)
  samp1 = sample( which(sub$Z==1), size=n1)
  coded=ifelse(ID%in%c(samp0, samp1),1,0)
  
  fit = train_ensemble(X=X, Z=sub$Z, Yobs=sub$Yobs, coded=coded, n.tune=n.tune,model=model,...)
  yhat=predict_scores(fit=fit,X=X, Z=sub$Z)
  resid = yhat-sub$Yobs
  R2.OOS=1-(var(resid)/var(sub$Yobs))
  #cat(sprintf("Model: %s, R2=%.3f\n", model, R2.OOS))
  
  Y.adj = yhat - (coded/pi)*(yhat-sub$Yobs)
  mod=lm(Y.adj ~ pretest + Z)
  
  mod.out=broom::tidy(mod)
  out = data.frame(model=model, mod.out[2:3,], R2.OOS)
  
  return(out)
}




# Calculates the subset-only estimate of the treatment effect (adjusting for pre-test scores)
# based on random sample of n0 and n1 docs
run_sub_cov=function(){
  samp0 = sample(which(sub$Z==0), size=n0)
  samp1 = sample(which(sub$Z==1), size=n1)
  coded=ifelse(ID%in%c(samp0, samp1),1,0)
  
  mod.lm = lm(Yobs ~ pretest + Z, data=sub[coded==1,] )
  mod.out=broom::tidy(mod.lm)
  out = data.frame(model="subset only", mod.out[2:3,])
  

  return(out)
  
}


# Calculates the oracle estimate of the treatment effect (adjusting for pre-test scores) 
# by bootstrap resampling the full data set
boot_oracle_cov=function(){
  samp=sample(1:nrow(sub),replace=T)
  boot=sub[samp, ]
  
  mod.lm = lm(Yobs ~ pretest + Z, data=boot)
  mod.out=broom::tidy(mod.lm)
  out = data.frame(model="oracle", mod.out[2:3,])
  
  
  return(out)
  
}

