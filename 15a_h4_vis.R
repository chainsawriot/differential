require(tidyverse)
require(quanteda)
require(LSX)

imp_score_h4 <- readRDS("imp_score_h4.RDS")

word_data <-  bind_rows(readRDS("word_data.RDS"), filter(readRDS("exp_score_h4.RDS"), word %in% imp_score_h4$word))
word_data$score <- scale(word_data$score)[,1]

imp_data <- bind_rows(readRDS("nas_data.RDS"), imp_score_h4)
imp_data$score <- scale(imp_data$score)[,1]

select(imp_data, word, score, group_name, h4) %>% mutate(h4 = !is.na(h4), exp = FALSE) -> imp_data

set.seed(1010112)
imp_data %>% sample_n(size = 30) %>% pull(word) -> random_names
word_data %>% mutate(h4 = !is.na(h4)) %>% select(word, score, h4) %>% mutate(exp = TRUE) %>% bind_rows(imp_data) %>% mutate(exp = if_else(exp, "Explicit score (LSS)", "Implicit score (NAS)")) %>%  filter(word %in% c(random_names, imp_score_h4$word)) %>% mutate(word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% ggplot(aes(y = fct_reorder(word, h4), x = score, col = h4)) + geom_point() + facet_grid(~exp, scales = "free_y") + ylab("word") + theme_light() + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig2_word

ggsave("fig2_word.png", fig2_word, width = 6, height = 9)

imp_data %>% sample_n(size = 30) %>% pull(word) -> random_names
word_data %>% mutate(h4 = !is.na(h4)) %>% select(word, score, h4) %>% mutate(exp = TRUE) %>% bind_rows(imp_data) %>% mutate(exp = if_else(exp, "Explicit score (LSS)", "Implicit score (NAS)")) %>%  filter(word %in% c(random_names, imp_score_h4$word)) %>% mutate(word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% group_by(h4, exp) %>% summarise(mean(score))

###

word_data %>% mutate(h4 = !is.na(h4)) %>% select(word, score, h4, group_name) %>% mutate(exp = TRUE) %>% bind_rows(imp_data) -> whole_data

whole_data$group_name[whole_data$h4] <- c("Jewish", "Jewish", "Jewish", "Jewish", "Romani", "Romani", "Romani", "Sinti", "Jewish", "Jewish", "Jewish", "Jewish", "Sinti", "Romani", "Romani", "Romani")

whole_data %>% filter(h4) %>% group_by(group_name, exp) %>% summarise(mscore = mean(score)) %>% ungroup() -> h4_stat

whole_data %>% filter(!h4) %>% group_by(group_name, exp) %>% summarise(mscore = mean(score)) %>% ungroup() -> nonh4_stat

bind_rows(h4_stat, nonh4_stat) %>% mutate(h4 = group_name %in% c("Jewish", "Romani", "Sinti")) %>% mutate(exp = if_else(exp, "Explicit score (LSS)", "Implicit score (NAS)")) %>% ggplot(aes(y = fct_reorder(group_name, h4), x = mscore, col = h4)) + geom_point() + facet_grid(~exp, scales = "free_y") + xlab("Score") + ylab("Group") + theme_light() + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig2

ggsave("fig2.png", fig2, width = 6, height = 9)
