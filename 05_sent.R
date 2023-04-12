require(quanteda)
require(tidyverse)

raw_data <- readRDS("incl_articles.RDS")
terms <- rio::import("Racism independent variables.xlsx") %>% tibble::as_tibble()

terms[,4] %>% pull %>% str_split("[ \n]+") %>% unlist %>% discard(~.=="") %>% tolower -> gl1

words <- readRDS("words.RDS")

words %>% str_subset("^deutsch-")
words %>% str_subset("^deutsch")

c("deutschtürken", "deutschtürke", "deutsch-türken", "deutsch-türke", "deutsch-türkin", "deutschtürkin", "deutsch-italiener", "deutsch-amerikaner", "deutsch-russe", "deutsch-russen", "deutschrusse", "deutschrussen", "deutsch-iraner", "deutsch-marokkaner", "deutsch-syrer") -> gl2

terms2 <- rio::import("Racism independent variables.xlsx", sheet = 2) %>% tibble::as_tibble()

terms2[,3] %>% pull %>% str_split("[ \n]+") %>% unlist %>% discard(~.=="") %>% tolower -> gl3

search_dict <- dictionary(list("anymatch" = c(gl1, gl2, gl3)))

saveRDS(search_dict, "search_dict.RDS")

incl_media <- c("t-online", "focus", "spiegel", "tagesspiegel", "taz", "die-welt", "bild", "faz", "sueddeutsche", "zeit")
source("lib.R")

read_tcm <- function(pub) {
    print(pub)
    x1 <- readRDS(gen_filename(pub, "tcm_"))
    dfm_lookup(x1, search_dict, valuetype = "fixed") %>% convert(to = "data.frame") %>% as_tibble
}

article_match <- map_dfr(incl_media, read_tcm)

article_match %>% filter(anymatch != 0) %>% pull(doc_id) -> matched_aid

raw_data %>% filter(aid %in% matched_aid) %>% mutate(sentences = as.list(tokens(content, what = "sentence"))) -> matched_articles

saveRDS(matched_articles, "sentences_tibble.RDS")
