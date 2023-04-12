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

gen_comb <- function(n) {
    a <- sample(seq(1, (n - 1)), size = 1)
    b <- n - a
    return(c(a, b))
}

permute <- function(n, A, B) {
    comb <- gen_comb(n)
    if (comb[1] > length(A)) {
        newcomb <- length(A)
        diff <- comb[1] - length(A)
        comb[2] <- comb[2] + diff
        comb[1] <- newcomb
    }
    if (comb[2] > length(B)) {
        newcomb <- length(B)
        diff <- comb[2] - length(B)
        comb[1] <- comb[1] + diff
        comb[2] <- newcomb
    }
    return(list(sample(A, comb[1]), sample(B, comb[2])))
}

gen_res <- function(n, A, B, S, w) {
    words <- permute(n, A, B)
    nas(w, tolower(S), A = words[[1]], B = words[[2]])
}

w[rownames(w) %in% c(admiration_words, fear_words, tolower(group_df$group_labels)), ] -> w

A <- admiration_words[admiration_words %in% rownames(w)]
B <- fear_words[fear_words %in% rownames(w)]
S <- tolower(group_df$group_labels)[tolower(group_df$group_labels) %in% rownames(w)]

set.seed(831721)
sample_sizes <- sample(rep(seq(2, length(c(A, B)), by = 2), 40))

require(furrr)
plan(multisession, workers = 6)

res <- furrr::future_map(sample_sizes, gen_res, A = A, B = B, S = S, w = w, .progress = TRUE, .options = furrr_options(seed = 831721))

saveRDS(tibble(n = sample_sizes, res = res), "silo.RDS")
