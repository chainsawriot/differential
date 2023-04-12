require(quanteda)
require(tidyverse)
require(here)
require(Matrix)
require(quanteda.textstats)
require(tidyverse)
require(quanteda)
require(here)
require(Matrix)
require(fs)
require(text2vec)
args <- commandArgs(trailingOnly=TRUE)


incl_media <- c("t-online", "focus", "spiegel", "tagesspiegel", "taz", "die-welt", "bild", "faz", "sueddeutsche", "zeit")
##source("../weat/lib.R")

gen_filename <- function(pub, prefix = "fcm_") {
    here::here("data/boot/", paste0(prefix, pub, ".RDS"))
}

gen_fcm_filename <- function(pub, prefix = "fcm_") {
    gen_filename(pub, prefix)
}

raw_data <- readRDS("incl_articles.RDS")

gen_fcm2 <- function(pub, raw_data, window_size = 8, words, prefix = "fcm_") {
    print(pub)
    raw_data %>% filter(publication == pub) %>% pull(content) %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% tokens_tolower() -> pub_tokens
    pub_tokens <- tokens_select(pub_tokens, words, padding = TRUE)
    pub_fcm <- fcm(pub_tokens, context = "window", window = window_size, count = "weighted", weights = 1 / seq_len(window_size), tri = TRUE)
    saveRDS(standardize_fcm(pub_fcm, words), gen_fcm_filename(pub, prefix = prefix))
}

standardize_fcm <- function(input_fcm, words) {
    fcm_m <- as(input_fcm, "dgCMatrix")
    fcm_names <- colnames(fcm_m)
    fcm_summary <- summary(fcm_m)
    new_i <- match(fcm_names[fcm_summary$i], words)
    new_j <- match(fcm_names[fcm_summary$j], words)
    sparseMatrix(i = new_i, new_j, x = fcm_summary$x, dims = c(length(words), length(words)), dimnames = list(words, words), giveCsparse = FALSE)
}

topwords <- readRDS("words.RDS")


train_glove <- function(input_fcm, rank = 200, x_max = 100, learning_rate = 0.05) {
    glove <- GlobalVectors$new(rank = rank, x_max = x_max, learning_rate = learning_rate)
    wv_main <- glove$fit_transform(input_fcm, n_iter = 15, convergence_tol = 0.01, n_threads = 8)
    wv_context <- glove$components
    wM <- wv_main + t(wv_context)
    return(wM)
}

i <- args[1]

print(i)
boot_data <- raw_data[sample(seq_len(nrow(raw_data)), nrow(raw_data), replace = TRUE), ]
walk(incl_media, gen_fcm2, raw_data = boot_data, words = topwords)
all_fcm <- fs::dir_ls(here("data/boot"), regexp = "fcm_")
overall_fcm <- map(all_fcm, readRDS) %>% reduce(`+`)
## saveRDS(overall_fcm, here("data/boot", "de_overall_fcm.RDS"))

## overall_fcm <- readRDS(here::here("data/boot", "de_overall_fcm.RDS"))
wM <- train_glove(overall_fcm)
saveRDS(wM, here::here("data", paste0("boot",i, "_de_glove_overall.RDS")))
