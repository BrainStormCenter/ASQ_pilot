# title: "reprex_Zscores"
# author: "Jason"
# date: "2019_04_30"
#modified: "03/05/2019"

library(tidyverse)
library(reprex)
library(dplyr)
# library(purrr)
# library(here)

## Community answer

dat <- read.csv(url('https://raw.githubusercontent.com/BrainStormCenter/ASQ_pilot/master/ASQ_pain_pilot_2019_04_19_v2.csv'), header = TRUE)
#		This creates z-scores for a variable/vector and works
# z.dat1 <- dat %>%
# 	as_tibble() %>%
# 	filter(Groups != "other") %>%
# 	select(ID, sex, asq_1a, asq_2a, asq_3a, asq_4a, asq_5a, asq_6a,
# 		   asq_7a, asq_8a, asq_9a) %>%
# 	mutate_at(vars(starts_with("asq")), list(z1 = ~as.vector(scale(.))))

#		Trying to calculate z-scores rowwise
#		Community answer
z.dat2 <- dat %>%
	as_tibble() %>%
	filter(Groups != "other") %>%
	select(ID, sex, ends_with("a")) %>%
	gather(asq, value, -ID, -sex, na.rm = FALSE) %>%
	group_by(ID) %>%
	mutate(z = as.vector(scale(value))) %>%
	pivot_wider(names_from = asq, values_from = c(value, z))

z.dat2

z.dat3 <- select(z.dat2, ID, starts_with("z"))
# merge two data frames by ID
dat <- merge(dat, z.dat3, by = "ID")
