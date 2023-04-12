require(tidyverse)
require(brms)

word_data <-  bind_rows(readRDS("word_data.RDS"), readRDS("exp_score_dual.RDS"))

word_data$score <- scale(word_data$score)[,1]

countries <- word_data %>% filter(dual) %>% pull(group_name)

word_data %>% filter(group_name %in% countries) %>% mutate(dual = if_else(is.na(dual), FALSE, TRUE)) %>% select(score, group_name, word, dual) -> exp_dual_data


weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

set.seed(1212111)
dual_model <- brm(score~dual+(1|group_name), data = exp_dual_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(dual_model, "exp_dual_model.RDS")
require(parameters)
parameters(dual_model)

########################

word_data <- bind_rows(readRDS("nas_data.RDS"), readRDS("imp_score_dual.RDS"))
word_data$score <- scale(word_data$score)[,1]

countries <- word_data %>% filter(dual) %>% pull(group_name)

word_data %>% filter(group_name %in% countries) %>% mutate(dual = if_else(is.na(dual), FALSE, TRUE)) %>% select(score, group_name, word, dual) -> imp_dual_data


weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

set.seed(1212111)
imp_dual_model <- brm(score~dual+(1|group_name), data = imp_dual_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(imp_dual_model, "imp_dual_model.RDS")
parameters(imp_dual_model)

### VIS

word_data <-  bind_rows(readRDS("word_data.RDS"), readRDS("exp_score_dual.RDS"))

word_data$sscore <- scale(word_data$score)[,1]

countries <- word_data %>% filter(dual) %>% pull(group_name)

word_data %>% filter(group_name %in% countries) %>% mutate(dual = if_else(is.na(dual), FALSE, TRUE)) %>% select(sscore, score, group_name, word, dual) -> exp_dual_data

word_data <- bind_rows(readRDS("nas_data.RDS"), readRDS("imp_score_dual.RDS"))
word_data$sscore <- scale(word_data$score)[,1]

countries <- word_data %>% filter(dual) %>% pull(group_name)

word_data %>% filter(group_name %in% countries) %>% mutate(dual = if_else(is.na(dual), FALSE, TRUE)) %>% select(sscore, score, group_name, word, dual) -> imp_dual_data



bind_rows((exp_dual_data %>% mutate(exp = TRUE)), imp_dual_data) %>% mutate(exp = if_else(!is.na(exp), "Explicit score (LSS)", "Implicit score (NAS)"), start_d = str_detect(word, "^d"), word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% ggplot(aes(y = fct_reorder2(word, sscore, start_d), x = sscore, color = dual)) + geom_point(size = 1.5) + facet_grid(group_name ~ exp, scales = "free_y") + ylab("word") + theme_light() + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig3_word

ggsave("fig3_word.png", fig3_word, width = 6, height = 12)

bind_rows((exp_dual_data %>% mutate(exp = TRUE)), imp_dual_data) %>% mutate(exp = if_else(!is.na(exp), "Explicit score (LSS)", "Implicit score (NAS)"), start_d = str_detect(word, "^d"), word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% group_by(group_name, dual, exp) %>% summarise(mscore = mean(sscore)) %>% ungroup() %>% ggplot(aes(y = group_name, x = mscore, color = dual)) + geom_point(size = 2) + facet_grid(~ exp, scales = "free_y") + ylab("word") + xlab("Country") + theme_light() + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig3

ggsave("fig3.png", fig3, width = 6, height = 6)

bind_rows((exp_dual_data %>% mutate(exp = TRUE)), imp_dual_data) %>% mutate(exp = if_else(!is.na(exp), "Explicit score (LSS)", "Implicit score (NAS)"), start_d = str_detect(word, "^d"), word = str_to_title(word)) %>% mutate(word = str_replace(word, "^Us\\-", "US\\-")) %>% group_by(group_name, dual, exp) %>% summarise(mscore = mean(sscore)) %>% ungroup() %>% filter(group_name %in% c("TUR", "RUS")) %>% ggplot(aes(y = group_name, x = mscore, color = dual)) + geom_point(size = 2) + facet_grid(~ exp, scales = "free_y") + ylab("word") + xlab("Country") + theme_light() + theme(legend.position = "none", axis.title = element_text(size = rel(2))) -> fig3tr_ru

ggsave("fig3tr_ru.png", fig3, width = 6, height = 3)
