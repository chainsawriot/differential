require(tidyverse)
require(quanteda)
require(here)
require(Matrix)
require(fs)
source("lib.R")

incl_media <- c("t-online", "focus", "spiegel", "tagesspiegel", "taz", "die-welt", "bild", "faz", "sueddeutsche", "zeit")

all_fcm <- dir_ls(here("data"), regexp = "fcm_")

overall_fcm <- map(all_fcm, readRDS) %>% reduce(`+`)

saveRDS(overall_fcm, here("data", "de_overall_fcm.RDS"))
