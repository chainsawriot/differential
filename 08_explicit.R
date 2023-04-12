require(LSX)
require(spacyr)
require(tidyverse)
require(quanteda)

parsed_sentences_dfm <- readRDS(here::here("data", "parsed_sentences.RDS")) %>% filter(pos %in% c("ADJ", "ADV", "VERB")) %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

lss_fit <- readRDS(here::here("data", "admiration_model.RDS"))
lss_score <- predict(lss_fit, parsed_sentences_dfm)

ori_dfm <- readRDS("parsed_sentences.RDS") %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

terms <- rio::import("Racism independent variables.xlsx") %>% tibble::as_tibble()

na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}

group_labels <- terms[,4] %>% pull %>% str_trim %>% str_split(" ")
group_names <- terms[,2] %>% pull %>% na.locf

tibble(group_name = group_names, group_labels = group_labels) %>% group_by(group_name) %>% unnest(cols = c(group_labels)) %>% ungroup -> group_df

group_df %>% group_by(group_name) %>% nest %>% mutate(labs = map(data, ~ (.$group_labels))) %>% select(-data) -> dict_df

label_dict <- dict_df$labs
names(label_dict) <- dict_df$group_name

label_dict <- dictionary(label_dict)

saveRDS(label_dict, "label_dict.RDS")

dfm_lookup(ori_dfm, label_dict) %>% convert(to = "data.frame") %>% as_tibble -> sent_match

score_df <- tibble(sid = names(lss_score), score = lss_score)

sent_match %>% left_join(score_df, by = c("doc_id" = "sid")) %>% summarize_at(vars(BGR:USA), list(~mean(score[.!=0], na.rm = TRUE), ~sd(score[.!=0], na.rm = TRUE))) %>% t -> sent_mean

tibble(lab = row.names(sent_mean), val = sent_mean[,1]) %>% mutate(country = map_chr(lab, ~ str_split(., "_")[[1]][1]), stat = map_chr(lab, ~ str_split(., "_")[[1]][2])) %>% select(-lab) %>% pivot_wider(id_cols = country, names_from = stat, values_from = val) %>% arrange(mean) %>% saveRDS("impl_summary.RDS")


indie_vars <- terms %>% select(2, c(6:9), 15)
colnames(indie_vars) <- c("country", "group_size", "group_size_f", "muslim", "gdppc", "distance")
indie_vars %>% filter(!is.na(country)) -> indie_vars

##target_sent <- readRDS("target_sent.RDS") %>% select(aid, sid, publication) %>% mutate(sid = as.character(sid))

target_sent_score <- readRDS(here::here("data", "target_sent_score.RDS"))

sent_match %>% pivot_longer(-doc_id, names_to = "country", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% left_join(indie_vars, by = "country") %>% rename("sid" = "doc_id") %>% left_join(target_sent_score, by = "sid") -> whole_data

saveRDS(whole_data, "whole_data.RDS")

dfm_select(ori_dfm, pattern = group_df$group_labels, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble %>% pivot_longer(-doc_id, names_to = "word", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% group_by(word) %>% summarise(score = mean(score, na.rm = TRUE)) %>% left_join((group_df %>% mutate(group_labels = tolower(group_labels))), by = c("word" = "group_labels")) %>% left_join(indie_vars, by = c("group_name" = "country")) -> word_data

saveRDS(word_data, "word_data.RDS")
