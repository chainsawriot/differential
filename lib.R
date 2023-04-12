### FCM

get_feats <- function(pub_tokens, min_termfreq = 3) {
    dfm(pub_tokens, verbose = TRUE, remove_punct = TRUE, remove_numbers = TRUE) %>% dfm_trim(min_termfreq = min_termfreq) %>% featnames()
}

standardize_fcm <- function(input_fcm, words) {
    fcm_m <- as(input_fcm, "dgCMatrix")
    fcm_names <- colnames(fcm_m)
    fcm_summary <- summary(fcm_m)
    new_i <- match(fcm_names[fcm_summary$i], words)
    new_j <- match(fcm_names[fcm_summary$j], words)
    sparseMatrix(i = new_i, new_j, x = fcm_summary$x, dims = c(length(words), length(words)), dimnames = list(words, words), giveCsparse = FALSE)
}

gen_filename <- function(pub, prefix = "fcm_") {
    here::here("data", paste0(prefix, pub, ".RDS"))
}

gen_fcm_filename <- function(pub, prefix = "fcm_") {
    gen_filename(pub, prefix)
}

gen_fcm <- function(pub, raw_data, window_size = 8, words, prefix = "fcm_") {
    print(pub)
    raw_data %>% filter(publication == pub) %>% pull(content) %>% str_replace_all("[[:punct:]]", " ") %>% tokens(remove_numbers = TRUE) %>% tokens_tolower() -> pub_tokens
    pub_tokens <- tokens_select(pub_tokens, words, padding = TRUE)
    pub_fcm <- fcm(pub_tokens, context = "window", window = window_size, count = "weighted", weights = 1 / seq_len(window_size), tri = TRUE)
    saveRDS(standardize_fcm(pub_fcm, words), gen_fcm_filename(pub, prefix = prefix))
}
