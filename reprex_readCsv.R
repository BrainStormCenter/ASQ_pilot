###############################
#		CREATED BY:		JASON CRAGGS
#		CREATED ON:		2019-04-18
#		USAGE:			REPREX TO READ CSV FILES
###############################
#
library(tidyverse)

#		LOAD DATA (SAME FILE BOTH TIMES)
data1 <- read.csv(url('https://raw.githubusercontent.com/BrainStormCenter/ASQ_pilot/master/ASQ_pain_pilot_2019_04_18.csv'), header = TRUE)
data2 <- read_csv(url('https://raw.githubusercontent.com/BrainStormCenter/ASQ_pilot/master/ASQ_pain_pilot_2019_04_18.csv'),
				  col_names = TRUE,
				  col_types = NULL,
				  quoted_na = FALSE)


#		ADD ASQ-LIGHT VARIABLE
#				THIS VERSION DOES WORK
data1.1 <-
	mutate(data1,
		   x = pmap_dbl(list(asq_1a, asq_2a, asq_3a, asq_4a, asq_5a,
		   				  asq_6a, asq_7a, asq_8a, asq_9a), function(...){
		   				  	row_values <- unlist(list(...))
		   				  	number_of_NAs <- sum(is.na(row_values))
		   				  	map_dbl(number_of_NAs, ~ case_when(
		   				  		.x == 0 ~ mean(row_values),
		   				  		.x >= 1 ~ mean(row_values, na.rm = TRUE)
		   				  	))
		   				  })
	) %>%
	rename(asq_light = x )

#				THIS VERSION DOES NOT WORK
data2.1 <-
	mutate(data2,
		   x = pmap_dbl(list(asq_1a, asq_2a, asq_3a, asq_4a, asq_5a,
		   				  asq_6a, asq_7a, asq_8a, asq_9a), function(...){
		   				  	row_values <- unlist(list(...))
		   				  	number_of_NAs <- sum(is.na(row_values))
		   				  	map_dbl(number_of_NAs, ~ case_when(
		   				  		.x == 0 ~ mean(row_values),
		   				  		.x >= 1 ~ mean(row_values, na.rm = TRUE)
		   				  	))
		   				  })
	) %>%
	rename(asq_light = x )
# ```
