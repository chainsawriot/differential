require(tidyverse)
require(quanteda)
require(LSX)

terms2 <- rio::import("Racism independent variables.xlsx", sheet = 2) %>% tibble::as_tibble()

terms2[,3] %>% pull %>% str_split("[ \n]+") %>% unlist %>% discard(~.=="") %>% tolower -> gl3

group_df <- tibble(word = gl3, group_name = "h4")

parsed_sentences_dfm <- readRDS("parsed_sentences.RDS") %>% filter(pos %in% c("ADJ", "ADV", "VERB")) %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

lss_fit <- readRDS("admiration_model.RDS")
lss_score <- predict(lss_fit, parsed_sentences_dfm)

ori_dfm <- readRDS("parsed_sentences.RDS") %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

score_df <- tibble(sid = names(lss_score), score = lss_score)

dfm_select(ori_dfm, pattern = gl3, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble %>% pivot_longer(-doc_id, names_to = "word", values_to = "freq") %>% filter(freq != 0) %>% select(-freq) %>% left_join(score_df, by = c("doc_id" = "sid")) %>% group_by(word) %>% summarise(score = mean(score, na.rm = TRUE)) %>% left_join(group_df, by = "word") %>% mutate(h4 = TRUE) -> exp_score_h4

saveRDS(exp_score_h4, "exp_score_h4.RDS")

w <- readRDS(here::here("data", "de_glove_overall.RDS"))

fear_words <- readRDS("fear_words.RDS")
admiration_words <- readRDS("admiration_words.RDS")
require(sweater)

model3 <- nas(w, tolower(gl3), A = admiration_words, B = fear_words)
feature3 <- tibble(score = model3$P, word = model3$S) %>% left_join(group_df, by = "word") %>% mutate(h4 = TRUE) -> imp_score_h4

saveRDS(imp_score_h4, "imp_score_h4.RDS")

### vis

## imp_score_h4 <- readRDS("imp_score_h4.RDS")

## word_data <-  bind_rows(readRDS("word_data.RDS"), filter(readRDS("exp_score_h4.RDS"), word %in% imp_score_h4$word))
## word_data$score <- scale(word_data$score)[,1]

## imp_data <- bind_rows(readRDS("nas_data.RDS"), imp_score_h4)
## imp_data$score <- scale(imp_data$score)[,1]

## select(imp_data, word, score, group_name, h4) %>% mutate(h4 = !is.na(h4), exp = FALSE) -> imp_data

## set.seed(1010112)
## imp_data %>% sample_n(size = 30) %>% pull(word) -> random_names
## word_data %>% mutate(h4 = !is.na(h4)) %>% select(word, score, h4) %>% mutate(exp = TRUE) %>% bind_rows(imp_data) %>% mutate(exp = if_else(exp, "Explicit score (LSS)", "Implicit score (NAS)")) %>%  filter(word %in% c(random_names, imp_score_h4$word)) %>% mutate(word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% ggplot(aes(y = fct_reorder(word, h4), x = score, col = h4)) + geom_point() + facet_grid(~exp, scales = "free_y") + ylab("word") + theme_light() + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig2_word

## ggsave("fig2_word.png", fig2_word, width = 6, height = 9)

## imp_data %>% sample_n(size = 30) %>% pull(word) -> random_names
## word_data %>% mutate(h4 = !is.na(h4)) %>% select(word, score, h4) %>% mutate(exp = TRUE) %>% bind_rows(imp_data) %>% mutate(exp = if_else(exp, "Explicit score (LSS)", "Implicit score (NAS)")) %>%  filter(word %in% c(random_names, imp_score_h4$word)) %>% mutate(word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% group_by(h4, exp) %>% summarise(mean(score))

## ###

## word_data %>% mutate(h4 = !is.na(h4)) %>% select(word, score, h4, group_name) %>% mutate(exp = TRUE) %>% bind_rows(imp_data) -> whole_data

## whole_data$group_name[whole_data$h4] <- c("Jewish", "Jewish", "Jewish", "Jewish", "Romani", "Romani", "Romani", "Sinti", "Jewish", "Jewish", "Jewish", "Jewish", "Sinti", "Romani", "Romani", "Romani")

## whole_data %>% filter(h4) %>% group_by(group_name, exp) %>% summarise(mscore = mean(score)) %>% ungroup() -> h4_stat

## whole_data %>% filter(!h4) %>% group_by(group_name, exp) %>% summarise(mscore = mean(score)) %>% ungroup() -> nonh4_stat

## bind_rows(h4_stat, nonh4_stat) %>% mutate(h4 = group_name %in% c("Jewish", "Romani", "Sinti")) %>% mutate(exp = if_else(exp, "Explicit score (LSS)", "Implicit score (NAS)")) %>% ggplot(aes(y = fct_reorder(group_name, h4), x = mscore, col = h4)) + geom_point() + facet_grid(~exp, scales = "free_y") + xlab("Score") + ylab("Group") + theme_light() + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig2

## ggsave("fig2.png", fig2, width = 6, height = 9)
