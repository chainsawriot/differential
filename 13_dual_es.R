require(tidyverse)

c("deutschtürken", "deutschtürke", "deutsch-türken", "deutsch-türke", "deutsch-türkin", "deutschtürkin", "deutsch-italiener", "deutsch-amerikaner", "deutsch-russe", "deutsch-russen", "deutschrusse", "deutschrussen", "deutsch-iraner", "deutsch-marokkaner", "deutsch-syrer") -> gl2

c("TUR", "TUR", "TUR", "TUR", "TUR", "TUR", "ITA", "USA", "RUS", "RUS", "RUS", "RUS", "IRN", "MAR", "SYR") -> group_name

group_df <- tibble(word = gl2, group_name = group_name)

require(LSX)
require(spacyr)
require(tidyverse)
require(quanteda)

parsed_sentences_dfm <- readRDS("parsed_sentences.RDS") %>% filter(pos %in% c("ADJ", "ADV", "VERB")) %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

lss_fit <- readRDS("admiration_model.RDS")
lss_score <- predict(lss_fit, parsed_sentences_dfm)

ori_dfm <- readRDS("parsed_sentences.RDS") %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

score_df <- tibble(sid = names(lss_score), score = lss_score)

dfm_select(ori_dfm, pattern = gl2, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble %>% pivot_longer(-doc_id, names_to = "word", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% group_by(word) %>% summarise(score = mean(score, na.rm = TRUE)) %>% left_join(group_df, by = "word") %>% mutate(dual = TRUE) -> exp_score_dual

saveRDS(exp_score_dual, "exp_score_dual.RDS")

######

w <- readRDS(here::here("data", "de_glove_overall.RDS"))

fear_words <- readRDS("fear_words.RDS")
admiration_words <- readRDS("admiration_words.RDS")
require(sweater)

model3 <- nas(w, tolower(gl2), A = admiration_words, B = fear_words)
feature3 <- tibble(score = model3$P, word = model3$S) %>% left_join(group_df, by = "word") %>% mutate(dual = TRUE) -> imp_score_dual

saveRDS(imp_score_dual, "imp_score_dual.RDS")

###

## dfm_select(ori_dfm, pattern = gl2, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble %>% pivot_longer(-doc_id, names_to = "word", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% filter(word == "deutsch-türkin") -> dt_sentence

## target_sent <- readRDS("target_sent.RDS")

## x <- "deutsch-türkin"
## dfm_select(ori_dfm, pattern = gl2, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble %>% pivot_longer(-doc_id, names_to = "word", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% filter(word == x) -> dt_sentence

## target_sent %>% filter(sid %in% dt_sentence$doc_id) %>% select(sentences) %>% pull(sentences)

## x <- "deutschrussen"
## dfm_select(ori_dfm, pattern = gl2, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble %>% pivot_longer(-doc_id, names_to = "word", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% filter(word == x) -> dt_sentence

## target_sent %>% filter(sid %in% dt_sentence$doc_id) %>% select(sentences) %>% pull(sentences)

## x <- "deutsch-marokkaner"
## dfm_select(ori_dfm, pattern = gl2, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble %>% pivot_longer(-doc_id, names_to = "word", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% filter(word == x) -> dt_sentence

## target_sent %>% filter(sid %in% dt_sentence$doc_id) %>% select(sentences) %>% pull(sentences)

