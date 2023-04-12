require(quanteda)
require(tidyverse)

sent <- readRDS("sentences_tibble.RDS") %>% unnest(sentences) %>% select(-content) %>% mutate(sid = row_number())

sent_tok <- tokens(sent$sentences)
saveRDS(sent_tok, "sent_tok.RDS")

search_dict <- readRDS("search_dict.RDS")
docnames(sent_tok) <- sent$sid
dfm(sent_tok, tolower = TRUE) %>% dfm_lookup(search_dict) -> matched_sent

matched_sent %>% convert(to = "data.frame") %>% as_tibble %>% filter(anymatch != 0) %>% pull(doc_id) -> matched_sid

sent %>% filter(sid %in% matched_sid) -> target_sent
saveRDS(target_sent, "target_sent.RDS")

require(spacyr)
spacy_initialize(model = "de")

text <- target_sent$sentences
names(text) <- target_sent$sid

pp <- function(s) {
    x <- suppressWarnings(spacy_parse(s, dependency = TRUE, lemma = FALSE))
}

s1 <- Sys.time()
x <- pp(text)
Sys.time() - s1
saveRDS(x, "parsed_sentences.RDS")

## doc_id here is actually sid

parsed_sentences_dfm <- readRDS("parsed_sentences.RDS") %>% filter(pos %in% c("ADJ", "ADV", "VERB")) %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

saveRDS(parsed_sentences_dfm, here::here("data", "parsed_sentences_dfm.RDS"))
