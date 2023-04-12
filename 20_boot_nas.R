require(sweater)
require(quanteda)
require(tidyverse)


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

all_res <- purrr::map(1:20, ~nas(readRDS(here::here("data", paste0("boot", ., "_de_glove_overall.RDS"))), tolower(group_df$group_labels), A = admiration_words, B = fear_words))
saveRDS(all_res, "boot_all_nas.RDS")

indie_vars <- terms %>% select(2, c(6:9), 15)
colnames(indie_vars) <- c("country", "group_size", "group_size_f", "muslim", "gdppc", "distance")
indie_vars %>% filter(!is.na(country)) -> indie_vars


gen_f <- function(model) {
    tibble(score = model$P, word = model$S) %>% left_join((group_df %>% mutate(group_labels = tolower(group_labels))), by = c("word" = "group_labels")) %>% left_join(indie_vars, by = c("group_name" = "country"))
}

nas_data <- readRDS("nas_data.RDS")

purrr::map_dfr(all_res, gen_f) %>% group_by(word) %>% summarise(m = mean(score), sd = sd(score)) %>% ggplot(aes(y = m, x = fct_reorder(word, m))) + geom_point() + geom_errorbar(aes(x = fct_reorder(word, m), ymin = m - sd, ymax = m + sd)) + theme_light() + theme(axis.text.x = element_text(angle = 90)) + xlab("Word") + ylab("NAS (Bootstrap)") -> bootscore
saveRDS(bootscore, "bootscore.RDS")
ggsave("bootscore.pdf", bootscore, width = 14, height = 7)

purrr::map(all_res, gen_f) %>% saveRDS("boot_brms_data.RDS")
