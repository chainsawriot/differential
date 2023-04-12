require(brms)
require(tidyverse)

wholedata <- readRDS("whole_data.RDS") %>% filter(!is.na(score))
weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

weaklyinformative_prior0 <- c(prior_string("normal(0, 1)", class = "Intercept"))

set.seed(100)
gdp_whole <- brm(score~log(gdppc)+(1|country)+(1|publication), data = wholedata, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(gdp_whole, "gdp_whole.RDS")

set.seed(100)
distance_whole <- brm(score~log(distance)+(1|country)+(1|publication), data = wholedata, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(distance_whole, "distance_whole.RDS")
set.seed(100)
muslim_whole <- brm(score~log(muslim)+(1|country)+(1|publication), data = wholedata, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(muslim_whole, "muslim_whole.RDS")


ratio <- c(32505, 18575, 14780, 35560)
p <- 124 * ratio / sum(ratio)

wholedata[wholedata$country == "EGY",]$group_size <- p[1]
wholedata[wholedata$country == "DZA",]$group_size <- p[2]
wholedata[wholedata$country == "LBY",]$group_size <- p[3]
wholedata[wholedata$country == "TUN",]$group_size <- p[4]

pf <- 13 * ratio / sum(ratio)

wholedata[wholedata$country == "EGY",]$group_size_f <- pf[1]
wholedata[wholedata$country == "DZA",]$group_size_f <- pf[2]
wholedata[wholedata$country == "LBY",]$group_size_f <- pf[3]
wholedata[wholedata$country == "TUN",]$group_size_f <- pf[4]

wholedata$group_size_f1 <- wholedata$group_size_f + 1

set.seed(100)
groupsize_whole <- brm(score~log(group_size)+(1|country)+(1|publication), data = wholedata, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(groupsize_whole, "groupsize_whole.RDS")

set.seed(100)
groupsizef_whole <- brm(score~log(group_size_f1)+(1|country)+(1|publication), data = wholedata, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(groupsizef_whole, "groupsizef_whole.RDS")

set.seed(100)
big_whole <- brm(score~log(gdppc)+log(distance)+log(group_size)+log(muslim)+(1|country)+(1|publication), data = wholedata, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(big_whole, "big_whole.RDS")

