# ---
# title: "reprex vector type"
# author: "Jason"
# date: "4/12/2019"
# output: html_document
# ---

library(tidyverse)
library(reprex)
library(purrr)
#library(here)

## IMPORT DATA FOR THE PILOT ANALYSES

data <- read.csv(url('https://raw.githubusercontent.com/BrainStormCenter/ASQ_pilot/master/ASQ_pilot_2019_04_09.csv'), header = TRUE)
data <- as_tibble(data)
arrange(data, Count)

##	CREATING NEW VARIABLES
##	CREATE A SMALLER DATA SET
asq_sm <- select(data2,
				 participant_id,
				 matches("asq_..a") #	REGULAR EXPRESSION
				)

#	COMPUTE MEAN OF SOME ASQ VARIABLES
mutate(asq_sm,
	   asq_light = (asq_13a + asq_14a + asq_15a) / 3  )


# tmp <- select(data, participant_id, contains("asq_15"))
# #View(tmp)
# #	TRY TO CONVERT FACTOR TO DOUBLE
# tmp2 <- mutate_if(tmp, is.factor, as.numeric(as.character(tmp,"asq_15a")))

