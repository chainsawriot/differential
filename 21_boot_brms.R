require(brms)
require(tidyverse)
require(parameters)

boot_data <- readRDS("boot_brms_data.RDS")


run_brms <- function(word_data, formula = score~log(gdppc)+(1|group_name)) {
    word_data$score <- scale(word_data$score)[,1]
    weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))
    weaklyinformative_prior0 <- c(prior_string("normal(0, 1)", class = "Intercept"))
    brm(formula, data = word_data, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
}

set.seed(1111111)

gdp_models <- purrr::map(boot_data, run_brms)
saveRDS(gdp_models, "boot_gdp.RDS")

set.seed(1111111)

distance_models <- purrr::map(boot_data, run_brms, formula = score~log(distance)+(1|group_name))

saveRDS(distance_models, "boot_distance.RDS")
