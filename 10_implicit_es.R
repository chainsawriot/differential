require(sweater)
require(quanteda)
require(tidyverse)

w <- readRDS(here::here("data", "de_glove_overall.RDS"))

fear_words <- readRDS("fear_words.RDS")
admiration_words <- readRDS("admiration_words.RDS")

terms <- rio::import("Racism independent variables.xlsx") %>% tibble::as_tibble()

na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}

group_labels <- terms[,4] %>% pull %>% str_trim %>% str_split(" ")
group_names <- terms[,2] %>% pull %>% na.locf

tibble(group_name = group_names, group_labels = group_labels) %>% group_by(group_name) %>% unnest(cols = c(group_labels)) %>% ungroup -> group_df


## model <- rnsb(w, tolower(group_df$group_labels), A = admiration_words, B = fear_words)

indie_vars <- terms %>% select(2, c(6:9), 15)
colnames(indie_vars) <- c("country", "group_size", "group_size_f", "muslim", "gdppc", "distance")
indie_vars %>% filter(!is.na(country)) -> indie_vars

## feature <- tibble(score = model$P, word = model$S) %>% left_join((group_df %>% mutate(group_labels = tolower(group_labels))), by = c("word" = "group_labels")) %>% left_join(indie_vars, by = c("group_name" = "country"))

## saveRDS(feature, "rnsb_data.RDS")

## model2 <- rnd(w, tolower(group_df$group_labels), B = admiration_words, A = fear_words)

## feature2 <- tibble(score = model2$P, word = model2$S) %>% left_join((group_df %>% mutate(group_labels = tolower(group_labels))), by = c("word" = "group_labels")) %>% left_join(indie_vars, by = c("group_name" = "country"))

## saveRDS(feature2, "rnd_data.RDS")

model3 <- nas(w, tolower(group_df$group_labels), A = admiration_words, B = fear_words)

feature3 <- tibble(score = model3$P, word = model3$S) %>% left_join((group_df %>% mutate(group_labels = tolower(group_labels))), by = c("word" = "group_labels")) %>% left_join(indie_vars, by = c("group_name" = "country"))

saveRDS(feature3, "nas_data.RDS")
