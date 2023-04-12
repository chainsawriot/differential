require(verbformen)
require(tidyverse)

coded_dictionary <- rio::import("dictionary_candidates_coded.xlsx")

fear_words <- coded_dictionary$fear[coded_dictionary$f1 == 1]

admiration_words <- coded_dictionary$admiration[coded_dictionary$a1 == 1]

map(fear_words, ~unique(verbformen(., tidy = TRUE, pos = "adj")$wort)) %>% unlist %>% str_subset("^zu ", negate = TRUE)  %>% unique -> fear_expanded

saveRDS(fear_expanded, "fear_words.RDS")

resolve <- function(x) {
    print(x)
    unique(verbformen(x, tidy = TRUE, pos = "adj", sleep = 10)$wort)
}

map(admiration_words, safely(resolve)) -> res

## "herausragende" "hilfsbereit"
admiration_words[res %>% map("result") %>% map_lgl(is.null)]

res %>% map("result") %>% unlist %>% unique %>% str_subset(" ", negate = TRUE) %>% c("herausragende", "herausragender", "herausragendes", "herausragendem", "herausragenden", "herausragend", "hilfsbereit", "hilfsbereite", "hilfsbereiter", "hilfsbereites", "hilfsbereiten", "hilfsbereitem") %>% saveRDS("admiration_words.RDS")
