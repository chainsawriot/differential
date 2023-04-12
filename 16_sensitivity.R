require(brms)
require(tidyverse)
require(parameters)

## Exclude Polen
word_data <- readRDS("word_data.RDS") %>% filter(word != "polen")
word_data$score <- scale(word_data$score)[,1]


weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

weaklyinformative_prior0 <- c(prior_string("normal(0, 1)", class = "Intercept"))


null_model <- brm(score~1+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior0)

gdp_model <- brm(score~log(gdppc)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

distance_model <- brm(score~log(distance)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

muslim_model <- brm(score~log(muslim)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

## word_data[word_data$group_name %in% c("EGY", "DZA", "LBY", "TUN"),]$group_size <- 124/4

ratio <- c(32505, 18575, 14780, 35560)
p <- 124 * ratio / sum(ratio)

word_data[word_data$group_name == "EGY",]$group_size <- p[1]
word_data[word_data$group_name == "DZA",]$group_size <- p[2]
word_data[word_data$group_name == "LBY",]$group_size <- p[3]
word_data[word_data$group_name == "TUN",]$group_size <- p[4]

pf <- 13 * ratio / sum(ratio)

word_data[word_data$group_name == "EGY",]$group_size_f <- pf[1]
word_data[word_data$group_name == "DZA",]$group_size_f <- pf[2]
word_data[word_data$group_name == "LBY",]$group_size_f <- pf[3]
word_data[word_data$group_name == "TUN",]$group_size_f <- pf[4]

word_data$group_size_f1 <- word_data$group_size_f + 1

groupsize_model <- brm(score~log(group_size)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

groupsizef_model <- brm(score~log(group_size_f1)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

big_model <- brm(score~log(gdppc)+log(distance)+log(group_size)+log(muslim)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

parameters(gdp_model)
parameters(distance_model)
parameters(muslim_model)
parameters(groupsize_model)
parameters(groupsizef_model)
parameters(big_model)

saveRDS(list(gdp_model, distance_model, muslim_model, groupsize_model, groupsizef_model, big_model), "explicit_models_polen.R")

###################

set.seed(1111111)
word_data <- readRDS("nas_data.RDS") %>% filter(word != "polen")
word_data$score <- scale(word_data$score)[,1]

weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

weaklyinformative_prior0 <- c(prior_string("normal(0, 1)", class = "Intercept"))

null_model <- brm(score~1+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior0)

gdp_model <- brm(score~log(gdppc)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

distance_model <- brm(score~log(distance)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

muslim_model <- brm(score~log(muslim)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

ratio <- c(32505, 18575, 14780, 35560)
p <- 124 * ratio / sum(ratio)

word_data[word_data$group_name == "EGY",]$group_size <- p[1]
word_data[word_data$group_name == "DZA",]$group_size <- p[2]
word_data[word_data$group_name == "LBY",]$group_size <- p[3]
word_data[word_data$group_name == "TUN",]$group_size <- p[4]

pf <- 13 * ratio / sum(ratio)

word_data[word_data$group_name == "EGY",]$group_size_f <- pf[1]
word_data[word_data$group_name == "DZA",]$group_size_f <- pf[2]
word_data[word_data$group_name == "LBY",]$group_size_f <- pf[3]
word_data[word_data$group_name == "TUN",]$group_size_f <- pf[4]

word_data$group_size_f1 <- word_data$group_size_f + 1

groupsize_model <- brm(score~log(group_size)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

groupsizef_model <- brm(score~log(group_size_f1)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)


big_model <- brm(score~log(gdppc)+log(distance)+log(group_size)+log(muslim)+(1|group_name), data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)

saveRDS(list(gdp_model, distance_model, muslim_model, groupsize_model, groupsizef_model, big_model), "implicit_models_polen.R")
