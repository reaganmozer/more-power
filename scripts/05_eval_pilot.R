setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")


load("generated data/MORE-pilot.RData")
## Evaluate performance of ML models for predicting human-coded outcomes on pilot data

tc = caret::trainControl(method="cv",number=5, allowParallel=T, savePredictions = "final")

X = all.pilot%>% select(-any_of(c("ID","docID","s_id","score","Z")))

library(parallel)
library(doParallel)
registerDoParallel(10)

ML.mods=c("rf","cubist","bagEarth","glmnet","svmPoly")

set.seed(1234)
mods = caretEnsemble::caretList(x=X, y=all.pilot$score, trControl=tc, 
                                methodList=ML.mods,
                                tuneLength=4)


mods # cubist regression is best here with R2=0.62

## Now we use this estimated R2 for power and sample size calculations

#' Calculate MDES given a range of sample fractions
calc_MDES <- function( N, p, R2, sigma = 1, stepsize = 1/30, 
                       h_vals = NULL,
                       alpha = 0.05, beta = 0.80 ) {

    var_func <- function(h_vals, p, N, R2, sigma) {
        var_terms <- sapply(h_vals, function(h) {
            (1/p + 1/(1-p)) * (1/N) * ( 1 + ((1-h)/h) * (1-R2) ) * sigma^2
        })
        return(var_terms)
    }

    if ( is.null( h_vals ) ) {
        h_vals <- seq( from = 0, to = 1, by = stepsize )
        h_vals = h_vals[ -1 ]
    } else {
        h_vals = sort( unique( c( 1, h_vals ) ) )
    }
    
    SE_oracle = sqrt( (1/p + 1/(1-p)) * (1/N) ) * sigma
    
    variances <- var_func(h_vals, p, N, R2, sigma)

    z = qnorm(1-alpha/2) + qnorm(beta)
    rs <- tibble( h = h_vals, 
            SE = sqrt( variances ) )
    
    rs$SE[ rs$h == 1 ] = SE_oracle
    
    rs <- mutate( rs,
                  inflate = SE / SE_oracle - 1, 
                  MDES = z * SE )
    
    return( rs )
}


res <- calc_MDES( N=1361, p = 722 / 1361, R2 = 0.62, sigma=1, 
                  stepsize = 1/100 )
res <- filter( res, h >= 0.1 )
res
ggplot( res, aes( h, MDES ) ) + 
    geom_hline( yintercept = 0.2, col="darkgrey" ) +
    geom_line() + 
    geom_vline( xintercept = 1/3, col="red" ) +
    theme_bw() + 
  labs( x = "Fraction of documents coded", y = "MDES" )  +
    scale_x_continuous(labels = scales::percent,
                       breaks = seq( 0, 1, by=0.1 ) ) +
  scale_y_continuous(breaks = seq( 0.1, 0.35, by=0.05 ),limits=c(0.15,0.35) ) 

