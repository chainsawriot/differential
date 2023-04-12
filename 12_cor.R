require(tidyverse)

implicit_data <- readRDS("nas_data.RDS")
explicit_data <- readRDS("word_data.RDS")

implicit_data %>% select(score, word) %>% rename(impl_score = "score") %>% left_join(explicit_data, by = "word") -> res

z <- function(x) {
    scale(x)[,1]
}

cor.test(z(res$score), z(res$impl_score))

require(ggrepel)

res %>% mutate(exp = z(score), imp = z(impl_score)) %>% select(word, exp, imp, group_name) %>% mutate(word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% ggplot(aes(x = exp, y = imp, col = group_name)) + geom_point() + geom_text_repel(aes(label = word, alpha = 0.7), size = 4) + theme_light() + xlab("Explicit score (LSS)") + ylab("Implicit score (NAS)") + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig1_word

ggsave("fig1_word.png", fig1_word, width = 16, height = 9)

res %>% group_by(group_name) %>% summarise(exp = mean(score, na.rm = TRUE), imp = mean(impl_score, na.rm = TRUE), exp_n = n()) %>% ungroup() %>% mutate(exp = z(exp), imp = z(imp)) %>% ggplot(aes(x = exp, y = imp)) + geom_point(aes(size = exp_n)) + geom_text_repel(aes(label = group_name, alpha = 0.7), size = 4) + theme_light() + xlab("Explicit score (LSS)") + ylab("Implicit score (NAS)") + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig1

ggsave("fig1.png", fig1, width = 16, height = 9)

res %>% group_by(group_name) %>% summarise(exp = mean(score, na.rm = TRUE), imp = mean(impl_score, na.rm = TRUE), exp_n = n()) %>% ungroup() %>% mutate(exp = z(exp), imp = z(imp)) %>% ggplot(aes(x = exp, y = imp)) + geom_point(aes(size = exp_n)) + geom_text_repel(aes(label = group_name, alpha = 0.7), size = 4) + theme_light() + xlab("Explizites  Maß(LSS)") + ylab("Implizites Maß (NAS)") + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig1de

ggsave("fig1de.png", fig1de, width = 16, height = 9)


res %>% group_by(group_name) %>% summarise(exp = mean(score, na.rm = TRUE), imp = mean(impl_score, na.rm = TRUE), exp_n = n()) %>% ungroup() %>% mutate(exp = z(exp), imp = z(imp)) %>% summarise(cor(exp, imp))
