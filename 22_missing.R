require(brms)
require(tidyverse)
require(parameters)
require(mice)

word_data <- readRDS("nas_data.RDS")
word_data$score <- scale(word_data$score)[,1]

weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

weaklyinformative_prior0 <- c(prior_string("normal(0, 1)", class = "Intercept"))

set.seed(1111)
imp <- mice(word_data, m = 100)

mi_distance_model <- brm_multiple(score~log(distance)+(1|group_name), data = imp, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(mi_distance_model, "mi_impl_distance_model.RDS")

word_data <- readRDS("word_data.RDS")
word_data$score <- scale(word_data$score)[,1]


weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

weaklyinformative_prior0 <- c(prior_string("normal(0, 1)", class = "Intercept"))

set.seed(1111)
imp <- mice(word_data, m = 100)

mi_distance_model <- brm_multiple(score~log(distance)+(1|group_name), data = imp, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(mi_distance_model, "mi_expl_distance_model.RDS")
