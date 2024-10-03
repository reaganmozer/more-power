pkg_load <- function(){
  require(foreach)
  require(doParallel)
  require(dplyr)
  require(caret)
  require(caretEnsemble)
  require(earth)
}

suppressPackageStartupMessages(pkg_load())

# Function to calculate subset estimator
get.sub = function(coded, data) {
  
  population = data
  est.pop = population %>%
    group_by(Z) %>%
    dplyr::summarise(n = n(),
                     var = var(Yobs),
                     .groups = "drop")
  
  var = sum(est.pop$var / est.pop$n)
  

  coded.samp= data[coded == 1,]
  
  
  est.part = coded.samp %>%
    group_by(Z) %>%
    dplyr::summarise(Ybar =  weighted.mean(Yobs, w=1/pi.hat),
                     n = n(),
                     var = var(Yobs),
                     .groups = "drop")
  
  est = diff(est.part$Ybar)
  var.hat = sum(est.part$var / est.part$n) 
  
  data.frame(est = est, SE=sqrt(var), SEhat = sqrt(var.hat))
}


# function to calculate synthetic and model-assisted estimators
get.ests = function(yhat,coded,data){
  population = data
  population$Yhat=yhat
  population$S = coded
  population$pi.hat=data$pi.hat
  
  
  est.part = population %>% group_by( Z ) %>%
    dplyr::summarise( # First calculate effect estimate using model-assisted estimator
      Ybar = mean( Yhat ),
      adj = weighted.mean( Yobs[S==1] - Yhat[S==1],  w=1/pi.hat[S==1] ),
      Ybar.adj = Ybar + adj,
      
      # Next calculate "true" variance of tau.hat
      N = n(),
      var.y = var(Yobs),
      var.resid = var(Yobs-Yhat),
      
      # Finally, calculate the estimated variance of tau.hat based on partial coding
      var.y.coded = var(Yobs[S==1]),
      n.z = sum(S),
      var.resid.coded = var(Yobs[S==1] - Yhat[S==1]),
      .groups="drop"
    )
  est = diff(est.part$Ybar.adj)
  var = sum(est.part$var.y/est.part$N) + sum( (est.part$N -est.part$n.z)/est.part$N * (est.part$var.resid/est.part$n.z))

  var.hat = sum(est.part$var.y.coded/est.part$N) + sum( (est.part$N -est.part$n.z)/est.part$N * (est.part$var.resid.coded/est.part$n.z))
  
  data.frame(est=est,SE=sqrt(var), SEhat=sqrt(var.hat))
}


# train ML models to predict outcomes from coded subset (separate models for treatment and control)
train_ensemble = function( X, Z, Yobs, coded, n.tune, bounds=NULL, preProc="zv",
                           model = "rf",seeds=NA,...
) {
  
  
  # Remove s_id and sampling weights from feature set for prediction
  population = data.frame(Z,Yobs,X)
  population$Yobs = as.numeric(population$Yobs)
  
  
  train = subset(population, coded==1)
  train0 = subset(train[train$Z==0,],select=-c(Z))
  train1 = subset(train[train$Z==1,],select=-c(Z))
  
  #registerDoParallel(5)
  
  control0 = caret::trainControl(method="cv", number=5, 
                                 savePredictions="final",
                                 predictionBounds = bounds,
                                 allowParallel=T,seeds=seeds)
  control1 = caret::trainControl(method="cv", number=5,
                                 savePredictions="final",
                                 predictionBounds = bounds,
                                 allowParallel=T,seeds=seeds)
  
  

  coded.X0 = as.matrix(subset(train0,select=-c(Yobs)))
  coded.X1 = as.matrix(subset(train1,select=-c(Yobs)))
  
  
  mod0 = caret::train(x=coded.X0, y=train0$Yobs,trControl=control0, preProcess=preProc,
                      tuneLength=n.tune, method=model,...)
  
  mod1 = caret::train(x=coded.X1, y=train1$Yobs,trControl=control1, preProcess=preProc,
                      tuneLength=n.tune, method=model,...)
  
  
  
  fit = list(mod0=mod0, mod1=mod1, coded=coded)
  
  
  return(fit)
}

# Calculate predicted scores for all documents using separate models for treatment and control
predict_scores = function(fit,  X, Z){
  
  X0 =  subset(X,Z==0)
  X1 =  subset(X, Z==1)

  yhat0 = predict(fit$mod0, X0)
  yhat1 = predict(fit$mod1, X1)
  
  
  yhat.pred = rep(NA, nrow(X))
  yhat.pred[Z==0] = yhat0
  yhat.pred[Z==1] = yhat1
  
  
  yhat.out0 = as.data.frame(fit$mod0$pred %>% arrange(rowIndex))$pred
  yhat.out1 = as.data.frame(fit$mod1$pred %>% arrange(rowIndex))$pred
  
  
  # Use OOS predictions for coded documents
  yhat.all = yhat.pred
  yhat.all[Z==0 & fit$coded==1]=yhat.out0
  yhat.all[Z==1 & fit$coded==1]=yhat.out1
  
  
  yhat.all = as.numeric(yhat.all)
  return(yhat.all)
}

# Get all estimates: nominal (oracle), subset, synthetic, and model-assisted
get_ML_est = function(dat, coded, yhat){
  
  
  est.ML =  get.ests(yhat,coded=coded,data=dat)
  names(est.ML)=paste0("ML.",names(est.ML))
  
  # Estimate based on the full hand-coded dataset
  est.nom = get.sub(coded=rep(1,nrow(dat)), dat)
  names(est.nom) = paste0("nominal.",names(est.nom))
  
  # Estimate based on the sample of hand-coded docs alone (n.coded)
  est.sub = get.sub(coded, dat)
  names(est.sub) = paste0("subset.",names(est.sub))
  
  # Oracle estimate (based on predictions alone)
  dat.pred = dat %>% mutate(Yobs=yhat)
  est.synth = get.sub(coded=rep(1,nrow(dat.pred)), dat.pred)
  names(est.synth) = paste0("synth.",names(est.synth))
  
  out = cbind(est.nom,est.sub,est.synth, est.ML)
  
  return(out)
}

# record in-sample and out-of-sample performance measures for ML model
eval_model = function(yhat, Yobs, coded){
  
  tmp = data.frame(yhat=as.vector(yhat), Yobs=Yobs,
                   coded=factor(coded,levels=0:1,labels=c("test","train")))
  

  out = tmp %>% dplyr::group_by(coded) %>% dplyr::summarise(ybar=mean(yhat), var.yhat=var(yhat),
                                                            R2=cor(yhat,Yobs)^2, mse=mean((Yobs-yhat)^2))
  
  out1 = out %>% tidyr::pivot_wider(names_from=coded,values_from=c(ybar,var.yhat,R2,mse)) %>% mutate(var.yhat=var(yhat))
  return(out1)
  
}



# Train a combined ML model to predict outcomes from coded subset (one model for both treatment and control groups)
train_ensemble_combined = function( X, Z, Yobs, coded, n.tune, 
                                    bounds=NULL, preProc="zv",
                                    model = "ranger",...
) {
  

  # Remove s_id and sampling weights from feature set for prediction
  population = data.frame(Z,Yobs,X)
  population$Yobs = as.numeric(population$Yobs)
  
  train = subset(population, coded==1)
  train = subset(train,select=-c(Z))
  
  cl=makeCluster(min(5,detectCores()))

  control = caret::trainControl(method="cv", number=5, 
                                savePredictions="final",
                                predictionBounds = bounds,
                                allowParallel=T)
  
  
  
  # Fit our "ML" models
  
  coded.X = as.matrix(subset(train,select=-c(Yobs)))
  
  mod0 = caret::train(x=coded.X, y=train$Yobs,trControl=control, preProcess=preProc,
                      tuneLength=n.tune, method=model,...)
  
  
  
  stopCluster(cl)
  
  
  fit = list(mod=mod0, coded=coded)
  
  
  return(fit)
}


# Get predicted scores from combined model trained on both treatment and control
predict_scores_combined = function(fit,  X, Z){
  
  X =  as.matrix(X)
  
  yhat.pred = predict(fit$mod, X)
  yhat.out = as.data.frame(fit$mod$pred %>% arrange(rowIndex))$pred
  
  
  # Use OOS predictions for coded documents
  yhat.all = yhat.pred
  yhat.all[fit$coded==1]=yhat.out
  
  
  yhat.all = as.numeric(yhat.all)
  return(yhat.all)
}




# Function to implement one iteration of the simulation
one.trial = function(dat, n.code, n.tune, 
                     model="rf", fit.type="separate",
                     tx=0,verbose=F,...){
  
  
  ##############
  
  # Select target full-coded data set of 1,000 docs from "universe"
  if (tx!=0){
    samp0= sample(which(dat$Z==0),size=500)
    samp1= sample(which(dat$Z==1),size=500)
    samp= c(samp0, samp1)
    dat.boot = dat[samp,]
    rownames(dat.boot)=NULL
  }
  else if (tx==0){
    samp=sample(1:nrow(dat), size=1000)
    dat.boot = dat[samp,]
    rownames(dat.boot)=NULL
    
    dat.boot$Z = sample(rep(0:1,each=500))
  }
  
  
  # check for and remove features with zero variance within this sample
  zv=apply(dat.boot,2,dplyr::n_distinct)
  if (min(zv)==1){
    ind.zv = which(zv==1)
    dat.boot = dat.boot[,-c(ind.zv)]
  }
  
  Z = dat.boot$Z
  Yobs = dat.boot$Yobs
  
  X.boot = dat.boot %>% select(-any_of( c( "id", "Z", "Yobs", "pi.hat" )  ))
  
  ind0 = which(Z==0)
  ind1 = which(Z==1)
  
  
  if (ncol(X.boot)>1){
    X0=X.boot[ind0,]
    X1=X.boot[ind1,]
  }
  else if (ncol(X.boot)==1){
    X0=subset(X.boot,Z==0)
    X1=subset(X.boot,Z==1)
  }
  
  ID = 1:nrow(dat.boot)
  dat.boot$pi.hat=rep(1, nrow(dat.boot)) # equal sampling weights under SRS
  
  
  # Code a subset of n.code using one of the three sampling schemes
  
  coded0 = sample(ind0, size=n.code/2)
  coded1 = sample(ind1, size=n.code/2)
  
  coded.samp = c(coded0, coded1)                        
  coded=ifelse(ID%in%coded.samp,1,0)
  
  
  
  if (verbose){
    print(paste0("Training ", model, " model based on ", n.code, " hand-coded documents and ", ncol(X.boot), " features")) }
  
  # Fit ML model using either separate or combined approach
  if (fit.type=="separate"){
    fit= suppressWarnings(suppressMessages(train_ensemble(X=X.boot,  Z=Z, Yobs=Yobs, coded=coded, n.tune=n.tune, model=model,...)))
    yhat=predict_scores(fit=fit,X=X.boot, Z=Z)
  }
  else if (fit.type=="combined"){
    fit= suppressWarnings(suppressMessages(train_ensemble_combined(X=X.boot,  Z=Z, Yobs=Yobs, coded=coded, n.tune=n.tune, model=model,...)))
    yhat=predict_scores_combined(fit=fit,X=X.boot, Z=Z) 
  }

  # Use predicted scores to calculate impact estimates
  res = get_ML_est(dat=dat.boot, coded=coded, yhat=yhat)
  eval.ML = eval_model(yhat, Yobs, coded)
  
  
  
  params = data.frame(n.coded=n.code,model=model,tx=tx)
  
  exp.res = cbind(params, res, eval.ML)
  
  
  
  return(exp.res)
  
  
}
