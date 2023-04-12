require(LSX)
##require(spacyr)
require(tidyverse)
require(quanteda)

parsed_sentences_dfm <- readRDS("parsed_sentences_dfm.RDS")

fear_words <- readRDS("fear_words.RDS")
admiration_words <- readRDS("admiration_words.RDS")

seed <- as.seedwords(list(admiration_words, fear_words))

## confusingly, this is random seed, not seed words.
set.seed(1212121)

lss_fit <- textmodel_lss(parsed_sentences_dfm, seeds = seed, auto_weight = TRUE, verbose = TRUE)

saveRDS(lss_fit, here::here("data", "admiration_model.RDS"))
## textplot_terms(lss_fit, highlighted = c(fear_words, admiration_words))
## tail(coef(lss_fit), n = 100)

## textplot_terms(lss_fit, highlighted = c(fear_words, admiration_words))
## target_sent <- readRDS("target_sent.RDS")
## lss_score <- predict(lss_fit, parsed_sentences_dfm)

