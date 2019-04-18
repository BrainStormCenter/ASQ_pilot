#	Reprex correlations
#	modified: "17/04/2019"

library(readr)	# Some additional options for reading csv data
library(purrr)
library(ggplot2)
# library(modelr)
library(Hmisc)
# library(doBy)
# require(dplyr)
library(ez)
# library(stringr)
library(corrplot)
options(na.action = na.exclude)
library(PerformanceAnalytics)
library(tidyverse)
library(here)
here() # THIS SETS THE WORKING DIRECTORY TO THE LOCATION OF THIS FILE
dr_here(show_reason = FALSE)

#		RAW DATA ON GITHUB
data <- read.csv(url('https://raw.githubusercontent.com/BrainStormCenter/ASQ_pilot/master/ASQ_pilot_2019_04_17.csv'), header = TRUE)



#		CREATING THE ASQ-LIGHT VARIABLE
data <-
	mutate(data,
		   x = pmap_dbl(list(asq_1a, asq_2a, asq_3a, asq_4a, asq_5a,
		   				  asq_6a, asq_7a, asq_8a, asq_9a), function(...){
		   				  	row_values <- unlist(list(...))
		   				  	number_of_NAs <- sum(is.na(row_values))
		   				  	map_dbl(number_of_NAs, ~ case_when(
		   				  		.x == 0 ~ mean(row_values),
		   				  		.x >= 1 ~ mean(row_values, na.rm = TRUE) #,
		   				  		# .x == 1 ~ mean(row_values, na.rm = TRUE),
		   				  		# .x == 2 ~ mean(row_values, na.rm = TRUE),
		   				  		# .x == 3 ~ mean(row_values, na.rm = TRUE)
		   				  	))
		   				  })
	) %>%
	rename(asq_light = x )

#		CREATING THE ASQ-HEAVY VARIABLE
data <-
	mutate(data,
		   x = pmap_dbl(list(asq_10a, asq_11a, asq_12a, asq_13a, asq_14a,
		   				  asq_15a), function(...){
		   				  	row_values <- unlist(list(...))
		   				  	number_of_NAs <- sum(is.na(row_values))
		   				  	map_dbl(number_of_NAs, ~ case_when(
		   				  		.x == 0 ~ mean(row_values),
		   				  		.x >= 1 ~ mean(row_values, na.rm = TRUE) #,
		   				  		# .x == 1 ~ mean(row_values, na.rm = TRUE),
		   				  		# .x == 2 ~ mean(row_values, na.rm = TRUE),
		   				  		# .x == 3 ~ mean(row_values, na.rm = TRUE)
		   				  	))
		   				  })
	) %>%
	rename(asq_heavy = x)

#		FILTER OUT SUBJECT WHERE Groups3=other
#			LOOK FOR A WAY TO AVOID LOSING THIS SUBJECT ENTIRELY
data2 <- filter(data, Groups3 != "other")

#		CREATING A SMALLER DATASET OF ALL SUBJECTS OF 3 VARIABLES
vars_keep <- select(data2, asq_light, asq_heavy,max.temp)

#		CORELATIONS
"Pearson"
cor(vars_keep, use = "complete.obs", method = "pearson")
cor(vars_keep, use = "pairwise.complete", method = "pearson")

#		WHICH CORRELATION VALUES GO WITH THE P-VALUES FROM THE TEST BELOW?
#			WHICH METHOD DOES THIS USE?
cor.mtest(vars_keep)


write.csv(data, "ASQ_pilot_raw_2019_04_17.csv", row.names = FALSE)
