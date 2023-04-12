require(tidyverse)

sent <- readRDS("sentences_tibble.RDS") %>% unnest(sentences) %>% select(-content) %>% mutate(sid = as.character(row_number()))

wholedata <- readRDS("whole_data.RDS") %>% filter(!is.na(score))

sent %>% left_join(wholedata, "sid") %>% filter(!is.na(score)) %>% arrange(score) %>% head(30) %>% select(sid, sentences, country, score) %>% pull(sentences)

sent %>% left_join(wholedata, "sid") %>% filter(!is.na(score)) %>% arrange(score) %>% head(100) %>% select(sid, publication.x, sentences, country, score) %>% rio::export("examples_low.csv")

sent %>% left_join(wholedata, "sid") %>% filter(!is.na(score)) %>% arrange(desc(score)) %>% head(100) %>% select(sid, publication.x, sentences, country, score) %>% rio::export("examples_high.csv")
