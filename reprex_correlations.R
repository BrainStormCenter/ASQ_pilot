#	Reprex correlations
#	modified: "17/04/2019"

library(ggplot2)
library(ez)
library(corrplot)
options(na.action = na.exclude)
library(PerformanceAnalytics)
library(tidyverse)
library(here)
here() # THIS SETS THE WORKING DIRECTORY TO THE LOCATION OF THIS FILE
dr_here(show_reason = FALSE)

#		RAW DATA ON GITHUB
data <- read.csv(url('https://raw.githubusercontent.com/BrainStormCenter/ASQ_pilot/master/ASQ_pilot_raw_2019_04_17.csv'), header = TRUE)

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

#		WHERE ARE THE P-VALUES FOR THE GRAPHS BELOW
chart.Correlation(vars_keep, histogram = TRUE, method = "pearson")

cor_plot2 <- ezCor(
	data = vars_keep[,c(1:3)],
	r_size_lims = c(8,8),
	test_alpha = .05,
	label_size = 3
)
print(cor_plot2)
