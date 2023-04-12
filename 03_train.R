require(tidyverse)
require(quanteda)
require(here)
require(Matrix)
require(fs)

require(text2vec)

train_glove <- function(input_fcm, rank = 200, x_max = 100, learning_rate = 0.05) {
    glove <- GlobalVectors$new(rank = rank, x_max = x_max, learning_rate = learning_rate)
    wv_main <- glove$fit_transform(input_fcm, n_iter = 100, convergence_tol = 0.01, n_threads = 8)
    wv_context <- glove$components
    wM <- wv_main + t(wv_context)
    return(wM)
}

overall_fcm <- readRDS(here::here("data", "de_overall_fcm.RDS"))

wM <- train_glove(overall_fcm)

saveRDS(wM, here::here("data", "de_glove_overall.RDS"))
