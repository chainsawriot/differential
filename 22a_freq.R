require(tidyverse)
require(quanteda)
require(here)
require(Matrix)
require(quanteda.textstats)

incl_media <- c("t-online", "focus", "spiegel", "tagesspiegel", "taz", "die-welt", "bild", "faz", "sueddeutsche", "zeit")
source("lib.R")

read_tcm <- function(pub) {
    print(pub)
    x1 <- readRDS(gen_filename(pub, "tcm_"))
    return(textstat_frequency(x1))
}

total_tf <- map_dfr(incl_media, read_tcm)
total_tf %>% group_by(feature) %>% summarise(df = sum(docfreq)) -> tf
saveRDS(tf, "tf.RDS")

fear_words <- readRDS("fear_words.RDS")
admiration_words <- readRDS("admiration_words.RDS")

tf %>% filter(feature %in% fear_words) %>% arrange(df) %>% ggplot(aes(y = fct_reorder(feature, df), x = df)) + geom_point() + xlab("Document Frequency (Log-scaled)") + scale_x_continuous(trans='log2') + ylab("Fear words") + theme_light() -> fear_df

ggsave("fear_df.pdf", fear_df, width = 14, height = 7)

tf %>% filter(feature %in% admiration_words) %>% arrange(df) %>% ggplot(aes(y = fct_reorder(feature, df), x = df)) + geom_point() + xlab("Document Frequency (Log-scaled)") + scale_x_continuous(trans='log2') + ylab("Admiration words") + theme_light() -> admire_df

ggsave("admire_df.pdf", admire_df, width = 14, height = 20)

