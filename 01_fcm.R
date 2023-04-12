require(quanteda)
require(tidyverse)
require(here)
require(Matrix)
require(quanteda.textstats)

incl_media <- c("t-online", "focus", "spiegel", "tagesspiegel", "taz", "die-welt", "bild", "faz", "sueddeutsche", "zeit")
source("lib.R")


raw_data <- readRDS("incl_articles.RDS")


read_tcm <- function(pub) {
    print(pub)
    x1 <- readRDS(gen_filename(pub, "tcm_"))
    return(textstat_frequency(x1))
}

total_tf <- map_dfr(incl_media, read_tcm)

total_tf %>% group_by(feature) %>% summarise(df = sum(docfreq)) -> tf
tf %>% arrange(desc(df)) %>% filter(df >= 20) %>% pull(feature) -> topwords
saveRDS(topwords, "words.RDS")

gen_fcm2 <- function(pub, raw_data, window_size = 8, words, prefix = "fcm_") {
    print(pub)
    raw_data %>% filter(publication == pub) %>% pull(content) %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% tokens_tolower() -> pub_tokens
    pub_tokens <- tokens_select(pub_tokens, words, padding = TRUE)
    pub_fcm <- fcm(pub_tokens, context = "window", window = window_size, count = "weighted", weights = 1 / seq_len(window_size), tri = TRUE)
    saveRDS(standardize_fcm(pub_fcm, words), gen_fcm_filename(pub, prefix = prefix))
}

topwords <- readRDS("words.RDS")
walk(incl_media, gen_fcm2, raw_data = raw_data, words = topwords)
