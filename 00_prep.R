require(quanteda)
require(tidyverse)
load("../rassmon/all_articles.RData")

## Zeit, Süddeutsche, FAZ, BILD, Die Welt, T-Online, Focus, Spiegel, Tagesspiegel, taz

incl_media <- c("t-online", "focus", "spiegel", "tagesspiegel", "taz", "die-welt", "bild", "faz", "sueddeutsche", "zeit")

alle_Artikel %>% filter(from_name %in% incl_media) %>% as_tibble -> incl_articles

incl_articles %>% select(title, from_name, text, pubDate3) %>% mutate(publication = from_name, title = title, content = text, date_published = pubDate3) %>% select(publication, title, content, date_published) %>% mutate("aid" = row_number()) %>% saveRDS("incl_articles.RDS")

source("lib.R")

require(tidyverse)
require(quanteda)
require(here)
require(Matrix)
require(quanteda.textstats)

raw_data <- readRDS("incl_articles.RDS")

##raw_data %>% pull(content) %>% str_replace_all("[[:punct:]]", " ") %>% tokens(remove_numbers = TRUE) %>% tokens_tolower() -> tokenized_text

##saveRDS(tokenized_text, "tokenized_text.RDS")

gen_tcm <- function(pub, raw_data, prefix = "tcm_") {
    print(pub)
    raw_data %>% filter(publication == pub) -> target
    corpus(target$content, docnames = target$aid) %>% tokens(remove_numbers = TRUE) %>% tokens_tolower() %>% dfm(verbose = TRUE, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) -> x
    saveRDS(x, gen_filename(pub, prefix = prefix))
}

walk(incl_media, gen_tcm, raw_data = raw_data)
