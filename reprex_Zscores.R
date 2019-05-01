# title: "reprex_Zscores"
# author: "Jason"
# date: "2019_04_30"

library(tidyverse)
library(reprex)
library(purrr)
library(here)

## IMPORT DATA FOR THE PILOT ANALYSES
dat <- read.csv(url('https://raw.githubusercontent.com/BrainStormCenter/ASQ_pilot/master/ASQ_pain_pilot_2019_04_19_v2.csv'), header = TRUE)
dat <- as_tibble(dat)
dat <- filter(dat, Groups != "other")

#		CREATE A SMALLER DATA SET
dat.asq <- select(dat, ID, sex, asq_1a, asq_2a, asq_3a, asq_4a)
#		CLACULATE Z-SCORES
dat.asq2 <- as.data.frame(scale(dat.asq[,c(grep("a$", colnames(dat.asq)))],
								center = TRUE, scale = TRUE))
#		ADD PREFIX TO CALCULATED SCORES
names(dat.asq2) <- paste0("z", names(dat.asq2))
#		ADD CALCULATED Z-SCORES TO INITIAL DATA SET
dat.asq3 <-  bind_cols(dat.asq, dat.asq2)
