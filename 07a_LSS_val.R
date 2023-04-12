require(tidyverse)
rio::import(here::here("data", "RasMon_Coding_Sophia.xlsx"), skip = 2) %>% tibble::as_tibble() -> coding_so
colnames(coding_so)[3] <- "emo"

rio::import(here::here("data", "RasMon_Coding_Vanessa.xlsx"), skip = 2) %>% tibble::as_tibble() -> coding_va
colnames(coding_va)[3] <- "emo"

coding_va

require(spacyr)

spacy_initialize(model = "de")

sents <- coding_va$sent
names(sents) <- 1:9000
coding_parsed_sent <- spacy_parse(sents)

require(quanteda)
require(LSX)

coded_sentences <- coding_parsed_sent %>% filter(pos %in% c("ADJ", "ADV", "VERB")) %>% as.tokens %>% dfm(tolower = TRUE, remove = stopwords("de"), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE)

set.seed(121212)
ranid <- sample(1:10, 9000, replace = TRUE)

lss_fit <- readRDS(here::here("data", "admiration_model.RDS"))

pred <- predict(lss_fit, coded_sentences)

tibble(id = as.character(1:9000), ranid = ranid) %>% left_join(tibble(id = names(pred), score = pred), by = "id") %>% mutate(gt_fear = coding_so$emo == 1 & coding_va$emo == 1, gt_admire = coding_so$emo == 3 & coding_va$emo == 3) %>% filter(ranid > 5) -> pred_tibble

require(brms)
require(performance)
require(parameters)

pred_tibble %>% group_by(gt_fear) %>% summarise(mean = mean(score, na.rm = TRUE))
pred_tibble %>% group_by(gt_admire) %>% summarise(mean = mean(score, na.rm = TRUE))

saveRDS(pred_tibble, "pred_tibble.RDS")
set.seed(1212121)
brm(score ~ as.numeric(gt_fear), data = pred_tibble) %>% saveRDS("fear_val.RDS")
brm(score ~ as.numeric(gt_admire), data = pred_tibble) %>% saveRDS("admire_val.RDS")
