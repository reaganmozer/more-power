setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(stringsAsFactors = F)
setwd("../")


pkgs = c("caret", "caretEnsemble", "devtools", 
         "doParallel", "dplyr", "ggplot2", 
         "knitr", "quanteda", "quanteda.textstats", 
         "stringi","textclean", "tidyverse", "tm")

lapply(pkgs, require, character.only=T)


devtools::install_github("quanteda/quanteda.sentiment")
devtools::install_github("kbenoit/quanteda.dictionaries")

library(quanteda.sentiment)
library(quanteda.dictionaries)


# Our library
devtools::install_github("reaganmozer/rcttext")
library(rcttext)

# Set up needed directories to save intermediate data files
dir.create("generated data/", showWarnings = FALSE )
dir.create("figures/", showWarnings = FALSE )
dir.create("results-supplement/", showWarnings = FALSE )


