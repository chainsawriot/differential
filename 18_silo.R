require(sweater)
require(tidyverse)

silo <- readRDS("silo.RDS")

S <- names(silo$res[[1]]$P)

silo$res[[1]]$P[S[1]]

extract <- function(res, s) {
    res$P[s]
}

## tibble(n = silo$n, nas = map_dbl(silo$res, extract, s = S[33])) %>% group_by(n) %>% summarise(max_nas = max(nas), min_nas = min(nas), med_nas = median(nas)) %>% ggplot(aes(x = n, y = med_nas)) + geom_ribbon(aes(ymin = min_nas, ymax = max_nas), fill = "grey70", alpha = 0.8) + geom_line()

## tibble(n = silo$n, nas = map_dbl(silo$res, extract, s = S[33])) %>% group_by(n) %>% summarise(max_nas = max(nas), min_nas = min(nas), med_nas = median(nas)) -> res

## length_x <- max(res$n) - min(res$n)
## length_y <- max(res$max_nas) - min(res$min_nas)

## mm_nas <- min(res$min_nas)
## min_area <- pracma::polyarea(res$n, res$min_nas - mm_nas)
## max_area <- pracma::polyarea(res$n, res$max_nas - mm_nas)


cal_silo <- function(s, silo) {
    tibble(n = silo$n, nas = map_dbl(silo$res, extract, s = s)) %>% group_by(n) %>% summarise(max_nas = max(nas), min_nas = min(nas), med_nas = median(nas)) -> res
    length_x <- max(res$n) - min(res$n)
    length_y <- max(res$max_nas) - min(res$min_nas)
    slide(res$n, ~.x, .before = 1, .complete = TRUE) %>% discard(is.null) %>% map_dbl(1) -> hmin
    slide(res$n, ~.x, .before = 1, .complete = TRUE) %>% discard(is.null) %>% map_dbl(2) -> hmax
    heights <- hmax - hmin
    slide(res$min_nas, ~.x, .before = 1, .complete = TRUE) %>% discard(is.null) %>% map_dbl(1) -> lowermin
    slide(res$min_nas, ~.x, .before = 1, .complete = TRUE) %>% discard(is.null) %>% map_dbl(2) -> uppermin
    slide(res$max_nas, ~.x, .before = 1, .complete = TRUE) %>% discard(is.null) %>% map_dbl(1) -> lowermax
    slide(res$max_nas, ~.x, .before = 1, .complete = TRUE) %>% discard(is.null) %>% map_dbl(2) -> uppermax
    top <- lowermax - lowermin
    bottom <- uppermax - uppermin
    trap_area <- sum(((top + bottom) * heights) / 2)
    total_area <- length_x * length_y
    1 - (trap_area / total_area)
}

res <- map_dbl(S, cal_silo, silo = silo)
data.frame(s = S, r = res) %>% arrange(r) %>% rio::export("silo.csv")

require(tidyverse)
silo <- rio::import("silo.csv")

ggplot(silo, aes(y = fct_reorder(s, r), x = r)) + geom_point() + xlab("r") + ylab("Group name") + theme_light() -> silo_fig

ggsave("silo_fig.pdf", silo_fig, width = 14, height = 20)
