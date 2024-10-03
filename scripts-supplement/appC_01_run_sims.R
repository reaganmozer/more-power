setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")



## 1. Load and clean PERSUADE corpus

dat = read.csv("raw data/persuade_2.0_human_scores_demo_id_github.csv")
dat$docID = 1:nrow(dat)
dat = dat %>% select(docID, everything(), -source_text) %>% 
  rename(id=essay_id_comp, 
         text=full_text,
         score=holistic_essay_score,
         prompt=prompt_name, 
         grade=grade_level)

txt=stringi::stri_trim_right(dat$text)
txt2=textclean::replace_non_ascii(txt)
dat$text = iconv(txt2, from="UTF-8",to="ASCII",sub="")
text = tm::stripWhitespace(dat$text)

## 2. Generate 256-dimensional vector embeddings to use as input features to ML
# Replace the text below with your unique API token
API_KEY = "ENTER API TOKEN"
Sys.setenv(OPENAI_API_KEY=API_KEY)

library(openai)

source("scripts/utils.R")

# Generate embeddings and save the results
embed.large<- get_embeddings(model = "text-embedding-3-large",text= text, dimensions=256)
embed.large=as.data.frame(embed.large)
names(embed.large)=paste0("X",1:ncol(embed.large))


dat.embed = dat %>% select(id, score) %>% rename(Yobs=score)
dat.embed = cbind(dat.embed, embed.large)

save(dat.embed,file="generated data/prepped_ML_embeddings.RData")


## 3. Run simulations with the embeddings feature set
source("scripts/sim_functions.R")


nc = seq(100,950,by=50)
mods = c("bagEarth","cforest","cubist","glmnet",
         "earth", "enet","pcr","rf","ridge","rpart",
         "RRFglobal","svmLinear","svmPoly","avNNet","nnet", "pcaNNet")

ML.grid = expand.grid(n.coded=nc, model=mods)

n.sim = 1000 # we ran in batches of 100 simulations on a cluster, change as needed for your configuration
n.tune = 4
tx = 0

seed = Sys.getpid()
set.seed(seed)
res.embed = data.frame()

for (j in 1:nrow(ML.grid)){
  
  n.coded = ML.grid$n.coded[j]
  model = ML.grid$model[j]
  
  if (!model %in% c("avNNet","nnet","pcaNNet")){
    res = plyr::rdply( n.sim, one.trial(dat.ML,n.coded, n.tune=n.tune, model=model, tx=tx), .progress="text")
  }
  else if (model %in% c("avNNet","nnet","pcaNNet")){
    res = plyr::rdply( n.sim, one.trial(dat.ML,n.coded, n.tune=n.tune, model=model, tx=tx, trace=F, linout=T, MaxNWts=1500), .progress="text")
    
  }
  res$n.tune = n.tune
  
  res.embed = rbind(res.embed, res)
  rm(res)
}

save(res.embed, file="results-supplement/appC-sim-results-embeddings.RData")

