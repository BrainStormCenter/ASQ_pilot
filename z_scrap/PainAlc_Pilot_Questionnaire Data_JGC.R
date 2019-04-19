# API Token: (not sure what to do with this right now, but maybe later??)
#3B03E4268D09386BC8BFD80EBD1B4928

# where files are:
#setwd("/Users/meredithjohnson/Box Sync/My Stuff/MyStuffForPainLab/Questionnaire Pilot Data")
setwd("/Users/jcraggs/Box Sync/Grants/Alcohol_Pain/PsychoPy ASQ-Questionniares_Pilot Data/")

# import data:
dat.full.may <- read.csv("ImagingPsychometrics_DATA_2017-05-25_1010.csv", header = TRUE)
dat.full <- read.csv("ImagingPsychometrics_DATA_2017-08-29_1040.csv", header = TRUE)

require(dplyr)
require(ez)


### Variables/Questionnaires we want to retain and score in dataset: 
# participant_id
# redcap_event_name (will need these 2 vars to match/merge datasets later)
# ASQ
# PDI
# McGill
# PILL (Pain inventory something something...)
# Life Experiences Survey (???) - nothing to do with pain. also only 15 people actually completed it. So no. 

### Subject variables:
# sex (1=male, 2=female)
# chronic
# chronic_pain: Have you been clinically diagnosed with a chronic pain condition?	
#     1 = yes 
#     2 = no
# condition: Right now, I fit into which category:	
#     1 = Healthy/No pain 
#     2 = Chronic pain
# paintype: What type of chronic pain do you have?	
#     1 = Chronic low back pain
#     2 = Fibromyalgia
# painduration: How long have you been experiencing said pain?	(open response)

#### ASQ Scoring ####

# asq_# questions: 0 = no, 1 = yes
# asq_3a questions: minimumn # of drinks to feel/do ___

# Labels for ASQ items added:
#   moretalk	Do you ever become more talkative after drinking alcohol?
# talknum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# moreflrt	Do you ever become more flirtatious after drinking alcohol?
# flrtnum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# everbuzz	Do you ever feel high or ìbuzzedî after drinking alcohol?
# buzznum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# atease	Do you ever feel more socially ìat easeî after drinking alcohol?
# easenum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# relax	Do you ever feel more relaxed after drinking alcohol?
# relaxnum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# everslug	Do you ever feel sluggish after drinking alcohol?
# slugnum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# inhibit	Do you ever feel less inhibited after drinking alcohol?
# inhibnum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# drive	Do you ever feel that your driving would be affected after drinking alcohol?
# drivenum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# sleepy	Do you ever feel sedated or sleepy after drinking alcohol?
# sleepnum	IF YES, what is the minimum number of drinks you could consume beforeÖ
# everhang	Do you ever experience a hangover after drinking alcohol? By ìhangover,î we mean that the day after an evening of drinking, you feel shaky or have a headache
# hangnum	IF YES, whatís the maximum number of drinks you could you consume in an evening withoutÖ
# everpass	Do you ever pass out after drinking alcohol?
# passnum	IF YES, whatís the maximum number of drinks you could you consume in an evening withoutÖ
# vomit	Do you ever throw up (vomit) after drinking alcohol?
# vomitnum	IF YES, whatís the maximum number of drinks you could you consume in an evening withoutÖ
# evernaus	Do you ever feel nauseated after drinking alcohol?
# nausnum	IF YES, whatís the maximum number of drinks you could you consume in an evening 
# forget	Do you ever forget a part of the evening (i.e., blackouts) after drinking alcohol?
# forgnum	IF YES, whatís the maximum number of drinks you could you consume in an evening 
# everdizz	Do you ever feel dizzy or feel things spinning after drinking alcohol?
# dizznum	IF YES, whatís the maximum number of drinks you could you consume in an evening 

# First pull ASQ questions and Subj ID, session, and gender variables.
#dat.asq <- dat.full[,c(1:2, grep("sex", colnames(dat.full)), grep("asq_[0-9]", colnames(dat.full)))] 
dat.asq <- dat.full[,c(1:2, grep("sex", colnames(dat.full)), grep("asq_", colnames(dat.full)))] 
#dat.asq.may.og <- dat.full.may[,c(1:2, grep("sex", colnames(dat.full.may)), grep("asq_", colnames(dat.full.may)))] 
# grep("asq_[0-9]", colnames(dat.full), value = TRUE) #grep with printing col names instead of column #s
#dat.asq <- dat.asq[dat.asq$redcap_event_name == "baseline_data_arm_1",] # retain Baseline data only
dat.asq <- dat.asq[dat.asq$asq_complete == "2",-4] # retain only completed ASQs and drop timestamp variable
#dat.asq.may.og <- dat.asq.may.og[dat.asq.may.og$asq_complete == "2",] # retain only completed ASQs and drop timestamp variable

### Fill in gender for individuals who completed ASQ not at baseline:
dat.sex <- dat.full[dat.full$redcap_event_name == "baseline_data_arm_1", c(1,11)] # make dataset with all Ps' sex (recorded at baseline only)
dat.asq <- left_join(dat.asq[,c(-3)], dat.sex, by = "participant_id") # merge the sex record dataset with the ASQ dataset.

write.csv(dat.asq, "ImagingPsychometrics_DATA_ASQ_2017-08-29.csv", row.names = FALSE)


### COMPUTING STANDARDIZED PERSON MEAN IMPUTATION FOR SENSITIVITY (ASQ) WITHIN SAMPLE
dat.asq.test <- dat.asq
# Step 1. z-score each of the sensitivity items (i.e., asq_#a - the numeric/count variables)
dat.asq.z <- scale(dat.asq.test[,c(grep("a$", colnames(dat.asq.test)))], center = TRUE, scale = TRUE)

# Step 2. Computing a mean of z-scores from non-missing items (i.e., computing the standardized person-mean imputation 
# based summary score in a standardized metric):

# COMPUTE ASQ2=mean.1(Ztalknum, Zflrtnum, 
#                     Zbuzznum, Zeasenum, Zrelaxnum,
#                     Zslugnum, Zinhibnum, Zdrivenum, Zsleepnum, Zhangnum,
#                     Zpassnum, Zvomitnum, Znausnum, Zforgnum, Zdizznum). 
# COMPUTE ASQL2=mean.1(Ztalknum, Zflrtnum, 
#                      Zbuzznum, Zeasenum, Zrelaxnum,
#                      Zslugnum, Zinhibnum, Zdrivenum, Zsleepnum).
# COMPUTE ASQH2=mean.1(Zhangnum,
#                      Zpassnum, Zvomitnum, Znausnum, Zforgnum, Zdizznum).
# EXECUTE.
# 

#### On second thought, maybe just run it in spss? On Thursday from the lab...? Yup. Done and done. ####
#### Import scored ASQ data ####
#dat.asq.scored <- read.csv("ImagingPsychometrics_DATA_2017-04-11_ASQ Scored.csv")
dat.asq.scored <- read.csv("ImagingPsychometrics_DATA_ASQ scored_2017-08-29.csv")

asqscores <- dat.asq.scored[,c(1:2,90)] # simplify the dataset.. so it's just SubID, Visit, and ASQ variable.

#### PDI Scoring ####
# Pull PDI questions and Subj ID and session vars.
grep("pdi", colnames(dat.full), value = TRUE) #grep with printing col names instead of column #s
dat.pdi <- dat.full[,c(1:2, grep("pdi", colnames(dat.full)))] 
#dat.asq <- dat.asq[dat.asq$redcap_event_name == "baseline_data_arm_1",] # retain Baseline data only
#dat.asq <- dat.asq[dat.asq$asq_complete == "2",-4] # retain only completed ASQs and drop timestamp variable

# To score, sum all items and divide sum by 10 (because our scale was a sliding score of 0-100 instead of 0-10).
# Final scores should range from 0-70. 

dat.pdi$PDI <- apply(dat.pdi[,c(grep("pdi[0-9]", colnames(dat.pdi)))], 1, sum, na.rm=TRUE)
dat.pdi$PDI[dat.pdi$pdi_complete == 0]=NA
# Note that I still gave people who skipped questions a PDI score... Looks like some had NAs even though they
# completed the questionnaire. Not sure why... Essentially NAs treated as 0s.

#### PILL ####
# Pull PILL questions and Subj ID and session vars.
grep("pill", colnames(dat.full), value = TRUE) #grep with printing col names instead of column #s
dat.pill <- dat.full[,c(1:2, grep("pill", colnames(dat.full)))] 

# Scoring: Sum the items. 
dat.pill$PILL <- apply(dat.pill[,c(grep("pill[0-9]", colnames(dat.pill)))], 1, sum, na.rm=TRUE)
# Note that I still gave people who skipped questions a PILL score... This is a problem though, because
# it treats NAs as 0s essentially. Hmm... Should be ok for now.. but consider rescaling or correcting for this later...
# In future, could subtract 1 from all item responses so that the scale is from 0-4 instead of 1-5. 
# Then sum those. And add 54 to the sums for everyone. 

# Just rescale it instead (so that scale is 0-4 for each item), don't add 54 to total. 
pill.rescaled <- dat.pill[,c(grep("pill[0-9]", colnames(dat.pill)))]-1
dat.pill$PILL_rescaled <- apply(pill.rescaled, 1, sum, na.rm=TRUE)
dat.pill$PILL[dat.pill$pill_complete == 0]=NA
dat.pill$PILL_rescaled[dat.pill$pill_complete == 0]=NA

#### McGill ####
dat.mcg <- dat.full[,c(1:2, grep("mcgill", colnames(dat.full)))]

# Scoring: Sum the items. 
dat.mcg$McGill <- apply(dat.mcg[,c(grep("mcgill[0-9]", colnames(dat.mcg)))], 1, sum, na.rm=TRUE)
dat.mcg$McGill[dat.mcg$mcgillmelzack_pain_questionnaire_complete == 0]=NA # change incomplete 
    # questionnaires to missing (NA) instead of 0 


##### Merge them all together ####
### Subject variables:
# sex (1=male, 2=female)
# chronic_pain: Have you been clinically diagnosed with a chronic pain condition?	
#     1 = yes 
#     2 = no
# condition: Right now, I fit into which category:	
#     1 = Healthy/No pain 
#     2 = Chronic pain
# paintype: What type of chronic pain do you have?	
#     1 = Chronic low back pain
#     2 = Fibromyalgia
# painduration: How long have you been experiencing said pain?	(open response)

# Subject variables: 
grep("chronic", colnames(dat.full), value = TRUE) #grep with printing col names instead of column #s
dat.subj <- dat.full[dat.full$redcap_event_name == "baseline_data_arm_1",c(1, grep(c("sex"), colnames(dat.full)), grep("chronic", colnames(dat.full)),
                        grep("paintype$", colnames(dat.full)), grep("painduration", colnames(dat.full)))] 
          # got rid of visits other than baseline because the above vars were only recorded at BL.
# Merge together with ASQ as the base dataset: 
dat.scored <- left_join(asqscores, dat.subj, by = "participant_id")
dat.scored <- left_join(dat.scored[,c(1:2,4:7,3)], dat.pdi[,c(1:2,12)], by = c("participant_id", "redcap_event_name"))
dat.scored <- left_join(dat.scored, dat.pill[,c(1:2,59:60)], by = c("participant_id", "redcap_event_name"))
dat.scored <- left_join(dat.scored, dat.mcg[,c(1:2,25)], by = c("participant_id", "redcap_event_name"))

#### Export data set ####
write.csv(dat.scored, "ImagingPsychometrics_QuestionnaireData_ASQ and Pain_scored_Aug2017.csv", row.names = FALSE)


#### Simple Analyses ####

# Make data frame with only the baseline data:
#dat.scored.bl <- dat.scored[dat.scored$redcap_event_name == "baseline_data_arm_1",]
# make data frame with the first ASQ score for each subject (some Ss have multiple ASQ scores):
dat.scored.1s <- top_n(group_by(dat.scored[!(is.na(dat.scored$ASQ)),], participant_id), 1, redcap_event_name)
n_distinct(dat.scored$participant_id[!(is.na(dat.scored$ASQ))]) # check to see if the number of unique values match the rows in the new dataset
n_distinct(dat.scored.1s$participant_id) # number of unique Ss should match row number

# dat.scored.1s$ASQ <- as.numeric(dat.scored.1s$ASQ)
# dat.scored.1s$PILL_rescaled <- as.numeric(dat.scored.1s$PILL_rescaled)
# dat.scored.cor <- dat.scored.1s[,c(7:8,10:11)]

behav.cor <- ezCor(
  data = dat.scored.1s[,c(7:8,10:11)]
  , r_size_lims = 5
  , point_alpha = .2
  , label_size = 3
)
# code won't run for some unknown reason...
# behav.cor <- ezCor(
#   data = dat.scored.cor[,c(1,3)]
#   , r_size_lims = 5
#   , point_alpha = .2
#   , label_size = 3
# )


print(behav.cor)

# More stats/details..
cor.test(dat.scored.1s$ASQ, dat.scored.1s$McGill)
# Results: 
# t = 1.3406, df = 24, p-value = 0.1926
#Aug17: t = 0.27877, df = 62, p-value = 0.7813, r = 0.03538158

cor.test(dat.scored.1s$ASQ, dat.scored.1s$PDI)
#  t = -0.30335, df = 24, p-value = 0.7642
# Aug17: t = -0.98312, df = 62, p-value = 0.3294, r = -0.1238942

cor.test(dat.scored.1s$ASQ, dat.scored.1s$PILL_rescaled)
# t = -1.6327, df = 24, p-value = 0.1156
# Aug17: t = -2.654, df = 62, p-value = 0.01009, r = -0.3194022 


dat.scored.t1t2 <- dat.scored[dat.scored$redcap_event_name == c("baseline_data_arm_1", "session_2_arm_1"),]

# testing...
PDI.rm <- ezANOVA(
  data = 
  , dv = PDI
  , wid = participant_id
  , between = ASQ
)
print(PDI.rm)

#  , within = redcap_event_name

#### Figure out who hasn't taken ASQ ####
dat.scored <- read.csv("ImagingPsychometrics_QuestionnaireData_ASQ and Pain_scored_April2017.csv")
dat.scored.bl <- dat.scored[dat.scored$redcap_event_name=="baseline_data_arm_1",]

#setwd("/Users/meredithjohnson/Box Sync/My Stuff/MyStuffForPainLab/Questionnaire Pilot Data")

dat.labels <- read.csv("ImagingPsychometrics_DATA_LABELS_2017-04-11_1426.csv")

#dat.emails <- dat.labels[c(1,grep("Email.address",colnames(dat.labels))),]

dat.emails <- left_join(dat.scored.bl, dat.labels[,c(1,grep("Email.address",colnames(dat.labels)))], by = "participant_id")

dat.emails <- dat.emails[c(1:4,6:7,12)]

#### Poster Subs  
setwd("/Users/meredithjohnson/Box Sync/My Stuff/MyStuffForPainLab/APS Conference/")
HCSubList <- scan("SubsList_HealthyCntrls.csv", what="character", sep = ",")
CLBPSubList <-scan("SubsList_CLBP.csv", what = "character", sep = ",")
dat.bl.MRISubs <- dat.bl[(dat.bl$participant_id %in% c(HCSubList, CLBPSubList)),]
dat.bl.MRISubs.demo <- select(dat.bl.MRISubs, participant_id, condition, sex, demo_dob, race___1, race___2, race___3,
                              race___4,race___5, ethnic_category, employed, marital_status, 
                              education, painduration, medication_list)
# Just keep everyone for now. 

# get rid of multiple lines per subject.
dat.emails.list <- dat.emails[grep("@", dat.emails$Email.address),]
grep("@", dat.emails$Email.address, value = TRUE)

# Make data frame with only people who are missing ASQ scores: 
dat.emails.NoASQ <- dat.emails.list[is.na(dat.emails.list$ASQ)==TRUE,]

# Export it:
setwd("/Users/meredithjohnson/Box Sync/My Stuff/MyStuffForPainLab/Questionnaire Pilot Data/")
write.csv(dat.emails.NoASQ, "ImagingPsychometrics_All Subjects Missing ASQ.csv", row.names = FALSE)

#### Acute Pain Ratings ####
# where files are:
setwd("/Users/meredithjohnson/Box Sync/My Stuff/MyStuffForPainLab/Questionnaire Pilot Data")
# import data:
dat.full <- read.csv("ImagingPsychometrics_DATA_2017-05-25_1010.csv", header = TRUE)

# import simplified data set to add variables to: 
#setwd("/Users/meredithjohnson/Box Sync/My Stuff/MyStuffForPainLab/APS Conference/")
#dat.painsleep <- read.csv("BehavData_PSQI ESS pain data_baseline_poster.csv", header = TRUE)
# import ASQ data set:
dat.asq.may <- read.csv("ImagingPsychometrics_QuestionnaireData_ASQ and Pain_scored_April2017.csv")

require(dplyr)
require(ez)

# Variables of interest: 
# visit1condition (1=Positive/ 2=Negative Mood)
# visit1painpre	visit1painpost	visit1painchange	v1recalledexperience (for visits 1, 2, 3)
# visit3painf1	visit3painf2	visit3painf3	visit3painf4	mood_and_pain_v3_complete
# rl_foot_43	painrating_43	rl_foot_44	painrating_44	rl_foot_45	painrating_45	
# rl_foot_46	painrating_46	rl_foot_47	painrating_47	rl_foot_48	painrating_48	
# rl_foot_49	painrating_49	rl_foot_50	painrating_50	calibration_data_complete
# rl_foot variable: 1=Left, 2=Right

# Pull all pain (visits 1 and 2) questions and Subj ID and session vars.
grep("visit[1-3]pain", colnames(dat.full), value = TRUE) #value=true prints col names instead of column #s
dat.pain <- dat.full[,c(1:2, 
                        grep("visit[1-3]condition", colnames(dat.full)), 
                        grep("visit[1-3]pain", colnames(dat.full)), 
                        grep("rl_foot", colnames(dat.full)), 
                        grep("painrating_", colnames(dat.full))
                        )] 

# Merge and export wide format: 
dat.wide <- right_join(dat.asq, dat.pain, by = c("participant_id", "redcap_event_name"))
write.csv(dat.wide, "AlcPain_WithAcutePainVars_Wide_2017-05-25.csv", row.names = FALSE)

# Reshape to long format: 

dat.long.cond <-reshape(dat.pain[,c(1:2,grep("condition", colnames(dat.pain)))],
                     varying = c("visit1condition", "visit2condition", "visit3condition"),
                     v.names = "MoodCondition",
                     timevar = "visit", 
                     times = c("visit1", "visit2", "visit3"), 
                     idvar = "participant_id",
                     new.row.names = 1:100000000,
                     direction = "long")
dat.long.cond<-dat.long.cond[!(is.na(dat.long.cond$MoodCondition)==TRUE),]

dat.long.pre <-reshape(dat.pain[,c(1:2,grep("painpre", colnames(dat.pain)))],
                     varying = c("visit1painpre", "visit2painpre", "visit3painpre"),
                     v.names = "pain_pre",
                     timevar = "visit", 
                     times = c("visit1", "visit2", "visit3"), 
                     idvar = "participant_id",
                     new.row.names = 1:100000000,
                     direction = "long")
dat.long.pre<-dat.long.pre[!(is.na(dat.long.pre$pain_pre)==TRUE),]

dat.long.post <-reshape(dat.pain[,c(1:2,grep("painpost", colnames(dat.pain)))],
                       varying = c("visit1painpost", "visit2painpost", "visit3painpost"),
                       v.names = "pain_post",
                       timevar = "visit", 
                       times = c("visit1", "visit2", "visit3"), 
                       idvar = "participant_id",
                       new.row.names = 1:100000000,
                       direction = "long")
dat.long.post<-dat.long.post[!(is.na(dat.long.post$pain_post)==TRUE),]


### Acute pain ratings: 
dat.long.v3midscan <-reshape(dat.pain[,c(1:2,grep("painf", colnames(dat.pain)))],
                       varying = c("visit3painf1", "visit3painf2", "visit3painf3", "visit3painf4"),
                       v.names = "v3MidScanPainRating",
                       timevar = "PostBlock", 
                       times = c("block1", "block2", "block3", "block4"), 
                       idvar = "participant_id",
                       new.row.names = 1:100000000,
                       direction = "long")
dat.long.v3midscan<-dat.long.v3midscan[!(is.na(dat.long.v3midscan$v3MidScanPainRating)==TRUE),]
# Maybe make an average across the 4 ratings? Or Model the ratings with ASQ x block? 

### Calibration data:
dat.long.foot <-reshape(dat.pain[,c(1:2,grep("foot", colnames(dat.pain)))],
                             varying = c("rl_foot_43", "rl_foot_44", "rl_foot_45", 
                                         "rl_foot_46", "rl_foot_47", "rl_foot_48", 
                                         "rl_foot_49", "rl_foot_50"),
                             v.names = "Foot_RL",
                             timevar = "Temp", 
                             times = c(43, 44, 45, 46, 47, 48, 49, 50), 
                             idvar = "participant_id",
                             new.row.names = 1:100000000,
                             direction = "long")
dat.long.foot<-dat.long.foot[!(is.na(dat.long.foot$Foot_RL)==TRUE),]

dat.long.calib <-reshape(dat.pain[,c(1:2,grep("rating", colnames(dat.pain)))],
                        varying = c("painrating_43", "painrating_44", "painrating_45", 
                                    "painrating_46", "painrating_47", "painrating_48", 
                                    "painrating_49", "painrating_50"),
                        v.names = "CalibPainRating",
                        timevar = "Temp", 
                        times = c(43, 44, 45, 46, 47, 48, 49, 50), 
                        idvar = "participant_id",
                        new.row.names = 1:100000000,
                        direction = "long")
dat.long.calib<-dat.long.calib[!(is.na(dat.long.calib$CalibPainRating)==TRUE),]

# Merge calibration data sets together: 
dat.long.calib <- full_join(dat.long.calib, dat.long.foot, by = c("participant_id", "redcap_event_name", "Temp"))

# What happened with participant # 161? 
dat.long.calib[dat.long.calib$participant_id=="IP161",]
# participant_id   redcap_event_name Temp CalibPainRating Foot_RL
# 55           IP161 baseline_data_arm_1   44              80       1
# 241          IP161 baseline_data_arm_1   43              NA       2
# The 43 value should actually be 80. Recode later, maybe. This person hasn't taken ASQ,
# so it's presently not worth it...

#### Merge and Export the Datasets ####

### Pre & Post 
dat.PrePost <- full_join(dat.long.cond, dat.long.pre, by = c("participant_id", "redcap_event_name", "visit"))
dat.PrePost <- full_join(dat.PrePost, dat.long.post, by = c("participant_id", "redcap_event_name", "visit"))

# First few variables of ASQ data set were only measured at BL. Add those first as 
# individual difference measures (this includes ASQ).
# dat.PrePost <- full_join(dat.asq[dat.asq$redcap_event_name=="baseline_data_arm_1",c(1,3:7)],
#                          dat.PrePost, by = "participant_id")
# Now add the variables that were measured every visit: (e.g., PDI, PILL, McGill, etc.)
# first just add visit variable to ASQ data set. 
unique(dat.asq$redcap_event_name) # print all the things listed under redcap variable
  # baseline_data_arm_1   session_2_arm_1     session_3_arm_1   
# dat.asq$visit[dat.asq$redcap_event_name=="baseline_data_arm_1"]="visit1"
# dat.asq$visit[dat.asq$redcap_event_name=="session_2_arm_1"]="visit2"
# dat.asq$visit[dat.asq$redcap_event_name=="session_3_arm_1"]="visit3"
dat.scored$visit[dat.asq$redcap_event_name=="baseline_data_arm_1"]="visit1"
dat.scored$visit[dat.asq$redcap_event_name=="session_2_arm_1"]="visit2"
dat.scored$visit[dat.asq$redcap_event_name=="session_3_arm_1"]="visit3"

# add ASQ scores to Pre/Post dataset:
dat.PrePost <- full_join(dat.PrePost[,c(1,3:6)], dat.scored[,c(1,12,7)], by = c("participant_id", "visit"))
write.csv(dat.PrePost, "AlcPain_BehavData_Long_PrePostPainRatings_ASQ_Aug2017.csv", row.names = FALSE)

### visit 3 mid-scan ratings:
# dat.v3pain <- left_join(dat.long.v3midscan, dat.PrePost[dat.PrePost$visit=="visit3",c(1:6,8:15)], 
#                         by = "participant_id")
dat.v3pain <- left_join(dat.long.v3midscan[,c(1,3:4)], dat.scored.1s[,c(1,3:5,7)], 
                        by = "participant_id")
write.csv(dat.v3pain, "AlcPain_BehavData_Long_Visit3MidScanRatings ASQ_Aug2017.csv", row.names = FALSE)

### Calibration Data: (all took place at baseline)
#ASQ individual diff measures recorded at BL:
# dat.calib <- left_join(dat.long.calib, 
#                        dat.asq[dat.asq$redcap_event_name=="baseline_data_arm_1",c(1,3:7)],
#                        by = "participant_id")
# Just use the ASQ dataset where there is one ASQ score per subject: 
dat.calib <- left_join(dat.long.calib[,c(1,3:5)], 
                       dat.scored.1s[,c(1,3:5,7)],
                       by = "participant_id")

write.csv(dat.calib, "AlcPain_BehavData_Long_CalibrationData ASQ_Aug2017.csv", row.names = FALSE)

### Recoding Calibration Data: 
library(doBy)

# make an average of 43 and 44 per subject (because this is the first right and left foot ratings). 
dat.calib.Avg1st2Ratings <- summaryBy(CalibPainRating ~ participant_id, 
                                      data = dat.calib[(dat.calib$Temp %in% c(43,44)),],
                                      FUN = mean)
dat.calib.AvgRatings.44.45 <- summaryBy(CalibPainRating ~ participant_id,
                                        data = dat.calib[dat.calib$Temp %in% c(44,45),],
                                        FUN = mean)
colnames(dat.calib.AvgRatings.44.45) <- c("participant_id", "CalibAvgRatings.44.45")

# look at the max temp value for subject and their rating for that..?

# dat.calib %>%
#   group_by(participant_id) %>%
#   summarise(Temp, Avg1st2Ratings = funs(max))
dat.calib.maxtemp <- summaryBy(Temp ~ participant_id, data = dat.calib, FUN = max)

# Now merge this data 
dat.calib.coded <- right_join(dat.calib[dat.calib$Temp==43,c(1,5:8)], dat.calib.maxtemp, by = "participant_id")
dat.calib.coded <- left_join(dat.calib.coded, dat.calib.Avg1st2Ratings, by = "participant_id")
colnames(dat.calib.coded)
colnames(dat.calib.coded) <- c("participant_id", "sex", "chronic_pain", "paintype",
                               "ASQ", "Temp.max", "CalibAvgRatings.43.44")
dat.calib.coded <- left_join(dat.calib.coded, dat.calib.AvgRatings.44.45, by = "participant_id")

# how many subs have calibration data and ASQ?
length(unique(dat.calib.coded[is.na(dat.calib.coded$ASQ)==FALSE,1]))
# 51 subjects with ASQ and calibration data. 

# fix name of avg1st2ratings variable so that it's less confusing: 
dat.calib.coded <- rename(dat.calib.coded, Avg1st2PRatings = CalibPainRating.mean)

# Export data set recoded: 
write.csv(dat.calib.coded, "AlcPain_BehavData_CalibrationRatings_Recoded_Aug2017.csv", row.names = FALSE)


##### Acute Pain - ASQ Analyses ####

calib.cor <- ezCor(
  data = dat.calib.coded[is.na(dat.calib.coded$ASQ)==FALSE,c(5:8)]
  , r_size_lims = c(5,5)
  , label_size = 3
)
print(calib.cor)

# Correlation Details: 
cor.test(dat.calib.coded$ASQ, dat.calib.coded$Temp.max)
# ASQ & Max Temp
# t = 1.256, df = 23, p-value = 0.2217
# r = 0.2533566;  95% CI:  -0.1575461  0.5894757
### New results with more Ss: (Aug2017)
# t = 2.1051, df = 49, p-value = 0.04043
# r = 0.2879871; 95% CI: 0.01347267 0.52213199

cor.test(dat.calib.coded$ASQ, dat.calib.coded$Avg1st2PRatings)
# ASQ & Avg1st2Ratings
# t = -1.1931, df = 23, p-value = 0.245
# r = -0.241429;    95% CI:  -0.5811231  0.1699107
### Results Aug2017: 
# t = -1.578, df = 49, p-value = 0.121
# r = -0.2199058;  95% CI:  -0.46717740  0.05926973


### Visit 3 Analysis: 
dat.v3pain.avg <- summaryBy(v3MidScanPainRating ~ participant_id, data = dat.v3pain, 
                            FUN = mean)
length(unique(dat.v3pain$participant_id))
# only 39 subs...?
# now 51 (Aug2017)

dat.v3pain.avg <- left_join(dat.v3pain.avg, dat.v3pain[dat.v3pain$PostBlock=="block1",c(1,4:7)])
length(unique(dat.v3pain.avg[is.na(dat.v3pain.avg$ASQ)==FALSE,1]))
# Only 43 participants who have mid-scan ratings for visit 3 and have ASQ scores. 

cor.test(dat.v3pain.avg$ASQ, dat.v3pain.avg$v3MidScanPainRating.mean)
# t = -0.53626, df = 9, p-value = 0.6048
# -0.175964;   95% CI:   -0.7017634  0.4739382
### New results: Aug2017
# t = -3.3018, df = 41, p-value = 0.001997
# r = -0.4583048;  95% CI:  -0.6668565 -0.1831748

v3pain.cor <- ezCor(
  data = dat.v3pain.avg[is.na(dat.v3pain.avg$ASQ)==FALSE,c(2,6)]
  , r_size_lims = c(5,5)
  , label_size = 3
)
print(v3pain.cor)

write.csv(dat.v3pain.avg, "AlcPain_BehavDat_Visit3MidScanPainRatingsMean ASQ_Aug2017.csv", row.names = FALSE)

## Maybe LSalc people are getting in the scanner and freaking out and rating 
# pain higher than they did during calibration on visit 1????


#### Calib and v3pain vars:

dat.acute.coded <- full_join(dat.calib.coded, dat.v3pain.avg[,c(1:2)], by = "participant_id")

acutepain.cor <- ezCor(
  data = dat.acute.coded[,c(5:8)]
  , r_size_lims = c(5,5)
  , label_size = 3
)
print(acutepain.cor)


###### Analyses of pain vs. non-pain group ####

#import separate spreadsheet I made using the Enrollment excel spreadsheet.
dat.groups <- read.csv("ImagingPsychometrics_SubjectGroups.csv", header = TRUE)
dat.groups <- dat.groups[!(is.na(dat.groups$Sub.ID)),] # get rid of extra rows

# Fix Sub ID Numbers:
library(stringr)
dat.groups$participant_id[dat.groups$Sub.ID > 99] = 
  paste("IP", dat.groups$Sub.ID[dat.groups$Sub.ID > 99], sep = "")
dat.groups$participant_id[dat.groups$Sub.ID < 100] = 
  paste("IP", dat.groups$Sub.ID[dat.groups$Sub.ID < 100], sep = "0")
dat.groups$participant_id[dat.groups$Sub.ID < 10] = 
  paste("IP", dat.groups$Sub.ID[dat.groups$Sub.ID < 10], sep = "00")
dat.groups <- dat.groups[,c(6,2:5)] # reorganize dataframe columns

# add column for CP vs. Ctrl:
dat.groups$CPvsHC.group[dat.groups$Group %in% c("CLBP", "FM")]="CP"
dat.groups$CPvsHC.group[dat.groups$Group == "HC"]="HC"

# fix gender variable:
dat.groups$Gender <- as.character(dat.groups$Gender)
dat.groups$Gender[dat.groups$Gender == "male "]="male"
unique(dat.groups$Gender)
dat.groups$Gender <- as.factor(dat.groups$Gender)

dat.groups <- dat.groups[,c(1:3,6,4:5)] # rearrange columns.

# Now we're ready to party...

write.csv(dat.groups, "ImagingPsychometrics_SubjectGroups_FixedSubIDs.csv", row.names = FALSE)
# Next time you can just skip the above steps and import this file.

# Merge with other data sets.
dat.calib.coded <- full_join(dat.groups, dat.calib.coded[,c(1,5:8)], by = "participant_id")
dat.scored.1s <- full_join(dat.groups[,c(1:4)], dat.scored.1s[,c(1:2,7:11)], by = "participant_id")

# Export data set recoded: 
write.csv(dat.calib.coded, "AlcPain_BehavData_CalibrationRatings_Recoded_Aug2017.csv", row.names = FALSE)
write.csv(dat.scored.1s, "ImagingPsychometrics_QuestionnaireData_ASQ and Pain_scored_1ASQscore per subj_Aug2017.csv", row.names = FALSE)

###### 
## Analyses ##

test <- dat.calib.coded[!(is.na(dat.calib.coded[,c(7:8)])==TRUE),]
test <- test[!is.na(test$participant_id),]

test$participant_id[test$Group == "HC"]
n_distinct(test$participant_id[test$Group == "HC"])
# only 11 healthy controls have taken the ASQ... :/ hmm..
n_distinct(test$participant_id[test$CPvsHC.group == "CP"])
# 40 CP subs.

## TempMax: 
anova(lm(Temp.max ~ ASQ + CPvsHC.group + ASQ*CPvsHC.group, data = dat.calib.coded))
# Response: Temp.max
#                   Df  Sum Sq Mean Sq F value  Pr(>F)  
# ASQ               1  20.471 20.4707  4.3004   0.04361 *
# CPvsHC.group      1   2.607  2.6066  0.5476   0.46298  
# ASQ:CPvsHC.group  1   0.019  0.0194  0.0041   0.94937  
# Residuals        47 223.727  4.7601 

#anova(lm(ASQ ~ Temp.max + CPvsHC.group + Temp.max*CPvsHC.group, data = dat.calib.coded))
# basically the same.. as I assumed it would be. Just making sure.. 

## CalibAvgRatings.43.44: 
anova(lm(CalibAvgRatings.43.44 ~ ASQ + CPvsHC.group + ASQ*CPvsHC.group, data = dat.calib.coded))
# Response: CalibAvgRatings.43.44
# Df  Sum Sq Mean Sq F value Pr(>F)
# ASQ               1  1079.3 1079.30  2.4265 0.1260
# CPvsHC.group      1   333.5  333.53  0.7498 0.3909
# ASQ:CPvsHC.group  1     0.1    0.15  0.0003 0.9856
# Residuals        47 20905.6  444.80  

## CalibAvgRatings.44.45: 
anova(lm(CalibAvgRatings.44.45 ~ ASQ + CPvsHC.group + ASQ*CPvsHC.group, data = dat.calib.coded))
# Response: CalibAvgRatings.44.45
# Df  Sum Sq Mean Sq F value Pr(>F)
# ASQ               1   442.4  442.41  1.3646 0.2493
# CPvsHC.group      1   718.1  718.06  2.2148 0.1442
# ASQ:CPvsHC.group  1     0.8    0.80  0.0025 0.9606
# Residuals        42 13616.8  324.21

## PDI:
anova(lm(PDI ~ ASQ + CPvsHC.group + ASQ*CPvsHC.group, data = dat.scored.1s))
# Response: PDI
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# ASQ               1   33773   33773  1.4851    0.2277    
# CPvsHC.group      1  801921  801921 35.2632 1.557e-07 ***
# ASQ:CPvsHC.group  1      63      63  0.0028    0.9583    
# Residuals        60 1364461   22741 

## PILL: 
anova(lm(PILL_rescaled ~ ASQ + CPvsHC.group + ASQ*CPvsHC.group, data = dat.scored.1s))
# Response: PILL_rescaled
# Df Sum Sq Mean Sq F value   Pr(>F)   
# ASQ               1   7748  7748.0  7.7966 0.007012 **
# CPvsHC.group      1   7153  7153.4  7.1983 0.009415 **
# ASQ:CPvsHC.group  1   1420  1420.2  1.4291 0.236618   
# Residuals        60  59626   993.8                    

## McGill: 
anova(lm(McGill ~ ASQ + CPvsHC.group + ASQ*CPvsHC.group, data = dat.scored.1s))
# Response: McGill
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# ASQ               1    29.1    29.1  0.1482    0.7016    
# CPvsHC.group      1 11377.7 11377.7 57.9777 2.233e-10 ***
#   ASQ:CPvsHC.group  1    57.5    57.5  0.2932    0.5902    
# Residuals        60 11774.5   196.2    

n_distinct(dat.scored.1s$participant_id[!is.na(dat.scored.1s$ASQ)])
#64 total with ASQ scores

###### Spliting ASQ into High/Low Factor subscales ####
# See Fleming et al., 2016

# Light drinkers/Factor 2:
# "talknum"    "flrtnum"    "buzznum"    "easenum"   
# "relaxnum"   "slugnum"    "inhibnum"   "drivenum"  
# "sleepnum"   

# Heavy drinkers/Factor 1: 
# "hangnum"    "passnum"    "vomitnum"  
#  "nausnum"    "forgnum"    "dizznum"

### Heavy subscale: 
# First take the mean of the female z-scored ASQ Heavy items:
dat.asq.scored.subscales <- 
  dat.asq.scored %>% rowwise() %>% mutate(HeavyASQ.f = mean(c(fZhangnum, fZpassnum, fZvomitnum, 
                                                          fZnausnum, fZforgnum, fZdizznum), 
                                                        na.rm = TRUE))
# Now do the same for the boys' z-scored ASQ Heavy items:
dat.asq.scored.subscales <- dat.asq.scored.subscales %>% 
  rowwise() %>% 
  mutate(HeavyASQ.m = mean(c(Zhangnum, Zpassnum, Zvomitnum, 
                           Znausnum, Zforgnum, Zdizznum), 
                         na.rm = TRUE))
# Put the two m and f columns together in one variable: 
dat.asq.scored.subscales$HeavyASQ <- apply(dat.asq.scored.subscales[,c(grep("HeavyASQ", colnames(dat.asq.scored.subscales)))],
                                  1, mean, na.rm = TRUE)
# Change NaN values (i.e., Not a Number) to NA instead:
dat.asq.scored.subscales$HeavyASQ[is.nan(dat.asq.scored.subscales$HeavyASQ)]=NA

### Light ASQ subscale: 
# First take the mean of the female z-scored ASQ Light items:
dat.asq.scored.subscales <- 
  dat.asq.scored.subscales %>% 
  rowwise() %>% 
  mutate(LightASQ.f = mean(c(fZtalknum, fZflrtnum, fZbuzznum, fZeasenum,
                             fZrelaxnum, fZslugnum, fZinhibnum, fZdrivenum,
                             fZsleepnum), 
                           na.rm = TRUE))
# Now do the same for the boys' z-scored ASQ Light items:
dat.asq.scored.subscales <- 
  dat.asq.scored.subscales %>% 
  rowwise() %>% 
  mutate(LightASQ.m = mean(c(Ztalknum, Zflrtnum, Zbuzznum, Zeasenum,
                             Zrelaxnum, Zslugnum, Zinhibnum, Zdrivenum,
                             Zsleepnum), 
                           na.rm = TRUE))
# Put the two m and f columns together in one variable: 
dat.asq.scored.subscales$LightASQ <- apply(dat.asq.scored.subscales[,c(grep("LightASQ", colnames(dat.asq.scored.subscales)))],
                                           1, mean, na.rm = TRUE)
# Change NaN values (i.e., Not a Number) to NA instead:
dat.asq.scored.subscales$LightASQ[is.nan(dat.asq.scored.subscales$LightASQ)]=NA

# Again keep only one ASQ score per subject:
n_distinct(dat.asq.scored.subscales$participant_id)
# 75 different subs. But 83 rows. Need to have only one per subject. 

# keep only the top "redcap_event_name" row for each subject (for whom ASQ is not NA):
dat.asq.scored.subscales.1s <- top_n(
  group_by(
    dat.asq.scored.subscales[!(is.na(dat.asq.scored.subscales$ASQ)),],
    participant_id),
  1, redcap_event_name)
n_distinct(dat.asq.scored.subscales$participant_id[!(is.na(dat.asq.scored.subscales$ASQ))]) # check to see if the number of unique values match the rows in the new dataset
n_distinct(dat.asq.scored.subscales.1s$participant_id) # number of unique Ss should match row number
# 64 subjects. 


# Import data sets: 
dat.calib.coded <- read.csv("AlcPain_BehavData_CalibrationRatings_Recoded_Aug2017.csv")
dat.scored.1s <- ("ImagingPsychometrics_QuestionnaireData_ASQ and Pain_scored_1ASQscore per subj_Aug2017.csv")

## Merge with datasets that contain Pain variables: 
dat.allpainvars <- full_join(dat.calib.coded, dat.scored.1s[,c(1,7:10)], by = "participant_id") # merge pain variables
dat.allpainvars <- full_join(dat.allpainvars, 
                             dat.asq.scored.subscales.1s[,c(1,93,96)],
                             by = "participant_id") # merge ASQ HEavy and light subscales to allpain dat

# Export data set: 
write.csv(dat.allpainvars, "ImagingPsychometrics_QuestionnaireData_ASQ and Pain and Calib_scored_1ASQscore per subj_Aug2017.csv",
          row.names = FALSE)

#### Redo correlations separately for ASQ subscales ####
cor.ASQsubscale <- ezCor(
  data = dat.allpainvars[is.na(dat.allpainvars$HeavyASQ)==FALSE,c(8:11,13:16)]
  , r_size_lims = c(4,4)
  , label_size = 3
)
cor.ASQsubscale <- ezCor(
  data = dat.allpainvars[,c(8:11,13:16)]
  , r_size_lims = c(4,4)
  , label_size = 3
)
print(cor.ASQsubscale)

### Correlation Details for some of the stronger correlations: 

# LightASQ and PILL: significant
cor.test(dat.allpainvars$LightASQ, dat.allpainvars$PILL_rescaled)
# t = -2.2979, df = 62, p-value = 0.02495
# r = -0.2801493; 95% CI:  -0.49207283, -0.03687968

# LightASQ and Temp.max
cor.test(dat.allpainvars$LightASQ, dat.allpainvars$Temp.max)
# t = 1.8379, df = 49, p-value = 0.07214
# r =0.2539529; 95% CI: -0.02325851 to 0.49490081

# LightASQ and CalibAvgRatings.43.44
cor.test(dat.allpainvars$LightASQ, dat.allpainvars$CalibAvgRatings.43.44)
# t = -1.6024, df = 49, p-value = 0.1155
# r = -0.2231363, 95% CI:  -0.46982895  0.05588377

# HeavyASQ and Temp.max
cor.test(dat.allpainvars$HeavyASQ, dat.allpainvars$Temp.max)
# t = 1.4765, df = 41, p-value = 0.1474
# r = 0.2246966; 95% CI: -0.08112191, 0.49184749

# HeavyASQ and CalibAvgRatings.43.44
cor.test(dat.allpainvars$HeavyASQ, dat.allpainvars$CalibAvgRatings.43.44)
# t = -1.1482, df = 41, p-value = 0.2575
# r = -0.176503, 95% CI: -0.4528411, 0.1307733

# Heavy ASQ and CalibAvgRatings.44.45
cor.test(dat.allpainvars$HeavyASQ, dat.allpainvars$CalibAvgRatings.44.45)
# t = -1.2323, df = 38, p-value = 0.2254
# r = -0.1960301; 95% CI:  -0.4783301,  0.1229897



