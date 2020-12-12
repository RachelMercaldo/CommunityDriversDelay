##### Diagnostic Pathway II Data Cleaning

#Clean up and get packages:

rm(list=ls())
library(tidyverse)
library(stringr)

#Read in the data:
steps<-read.csv("diagnostic2_raw.csv")
data=steps #save a copy

#remove 6 dups, which were expected:
data<-data[!duplicated(data$id),] 

#######################################################################################
######## After reviewing the original surveys, several variables must be fixed: #######

data$Visit15[data$id == 510]<-5 #5, not 50
data$Unit7[data$id == 554]<-3 #3, not 2
data$Unit4[data$id == 615]<-1 #1, not 21
data$Unit2[data$id == 515]<-2 #2, not 7
data$Unit7[data$id == 580]<-1 #1, not 5
data$Unit9[data$id == 654]<-1 #1, not 5

units<-paste("Unit", c(2:9,11:17),sep = "")
data[data$id == 666,colnames(data) %in% units]<-1

#The following ids (561,574,599,632) have issues that can't be fixed.
#561 has 2/3 of the lines in question 51 (pathway details) crossed out, with no explanation
#574 has codes of "5" in question 51, which have no meaning
#599 is missing all Visit/Unit values in question 51 (pathway details)
#632 has codes of "5" in question 51, which has no meaning

data<-data[!(data$id %in% c(561,574,599,632)),]

#ID 623 has some Contact Code issues, resolved by reviewing the original survey:
codes_for_623 <- c(8,11,13,5,8,5,14,10,1,5,5,6,8,6) 
code_cols<-paste("Code",1:14, sep ="")
for(i in 1:length(code_cols)){
  data[data$id == 623,colnames(data) %in% code_cols[i]] <- codes_for_623[i]
}

#Finally, recode Joy Clinic as 6, not 5 (TB provider instead of nonTB provider), 
#this per Dr. Sekandi.
cont_for_joy <- paste("Cont",1:17,sep = "")
code_cols <- paste("Code", 1:17, sep = "") #overwrite code_cols for new procedure

for(cont in 1:length(cont_for_joy)){
  col_joys <- grep("JOY", data[,cont_for_joy[cont]])
  if(length(col_joys != 0)){
    for(i in 1:length(col_joys)){
      data[col_joys[i],code_cols[cont]] <- 6
    }
  }
}


#########################################################
################## Outcome recoding: ####################

#create a cough duration variable from the day/week/month duration variables:

data$cough_duration <- data$cough_duration_b4_dx_days + (7*data$cough_duration_b4_dx_weeks) + (30.417*data$cough_duration_b4_dx_months)


#create a step_time variable for amount of time in diagnostic pathway, based
# on time in each step:
data$step_time<-NA

for (n in 1:nrow(data)){
  visits<-paste("Visit", 1:17, sep = "")
  units<-paste("Unit", 1:17,sep = "")
  #recode time between steps to days
  for (cols in 1:length(units))
  {
    if (! is.na(data[n,units[cols]]))
    {
      if (data[n,units[cols]]==1) {data[n,units[cols]] <- 1}
      if (data[n,units[cols]]==2) {data[n,units[cols]] <- 7}
      if (data[n,units[cols]]==3) {data[n,units[cols]] <- 30.4375}
      
      data[n,visits[cols]]=data[n,visits[cols]]*data[n,units[cols]]
    }
  }
  data[n,'step_time']<-sum(data[n,colnames(data) %in% visits], na.rm = TRUE)
}


######## Steps ##########
##Code stolen from Dr. Handel and modified as necessary for this data set##
# recode step types by combining individual types of steps (e.g. friend, healer, hospital, etc.) into 3 categories
#categories are TB/nonTB/social

codes<-paste("Code",1:17,sep="")
for (n in 1:nrow(data)) #loop over all individuals
{
  for (vis in codes) #loop over all steps, column 50-63 in codebook 
  { 
    if ( data[n,vis] %in% c(3,4,6) ) {data[n,vis]="TB"}         #Gov't hospital, health center and private hospital
    if ( data[n,vis] %in% c(1,2,5,7) ) {data[n,vis]="nonTB"}    #Herbal healer, private clinics, drugs stores, village health worker
    if ( data[n,vis] %in% c(8:15,88) ) {data[n,vis] = "social"} #Parent, spouse, siblings, adult child, other relative, friend, co-worker, neighbor
  }
}


#extract and modify a subset of data for easier handling
#note that step_time1 is the time from step 0 to 1, step_time2 the time from contact 1 to contact 2, etc.

subdata = data[,colnames(data) %in% codes]
subdata = cbind(subdata, data[,colnames(data) %in% visits])
subdata=cbind(Code0="self",subdata);

# Get #steps to first TB provider and days to first TB provider
littledat<-subdata
littledat$Visit1<-as.numeric(littledat$Visit1) #not appearing as numeric, for some reason

littledat$first_tb_step<-NA
littledat$first_tb_time<-NA


new_dat <- data.frame()
new_step <- NA

for(i in 1:nrow(littledat)){
  x<-min(which(littledat[i,] == "TB"))
  new_step[i] <- x
  if(x != Inf){
    y = x + 17
    new_dat<-bind_rows(new_dat,littledat[i,c(1,19:y)]) #if there is a tb provider, 
  } else {
    new_dat<-bind_rows(new_dat,littledat[i,c(1,19:35)])
  }
}

new_dat<-new_dat[,2:18]
new_dat$sums<-rowSums(new_dat,na.rm = T)

littledat$first_tb_time<-new_dat$sums
littledat$first_tb_step<-new_step-1 #or should we include the first step? (self)

data<-cbind(data,littledat[,36:37])

#calculate time between first TB contact and diagnosis
data$between_tb_dx<-data$step_time - data$first_tb_time

#other summary info about the steps
summary_dat <- data.frame("numSteps" = NA, "firstStep" = NA, "numSocial" = NA, "numNonTB" = NA, "numTB" = NA)
littlerdat <- littledat[,1:18]

for(ii in 1:nrow(littlerdat)){
  dat<-littlerdat[ii,]
  dat <- dat[,!is.na(dat)] 
  summary_dat[ii,"numSteps"] <- ncol(dat)
  summary_dat[ii,"firstStep"] <- as.character(dat$Code1)
  summary_dat[ii,"numSocial"] <- ifelse(sum(dat == "social") != 0, sum(dat == "social"), 0)
  summary_dat[ii,"numNonTB"] <- ifelse(sum(dat == "nonTB") != 0, sum(dat == "nonTB"), 0) 
  summary_dat[ii, "numTB"] <- ifelse(sum(dat == "TB") != 0, sum(dat == "TB"), 0)
}
summary_dat$numSteps <- summary_dat$numSteps - 1 #remove "self" step
data<-cbind(data,summary_dat)


########  Time following steps by category
new_littledat <- littledat[,2:35]
new_littledat$diagtime <- NA
newer_data<-data.frame()

for(ii in 1:nrow(new_littledat)){
  work<-new_littledat[ii,]
  socials<-which(work == "social") + 18
  nonTBs<-which(work == "nonTB") + 18
  TBs<-which(work=="TB") + 18
  if(length(socials) != 0) newer_data[ii,'social_time']<-sum(work[,c(socials)])
  if(length(nonTBs) !=0) newer_data[ii,'nonTB_time']<-sum(work[,c(nonTBs)])
  if(length(TBs) !=0) newer_data[ii,'TB_time'] <- sum(work[,c(TBs)], na.rm = T)
}

data<-cbind(data,newer_data)

#### Time following steps, by category, until first TB provider
newer_data1<-data.frame()
for(i in 1:nrow(littledat)){
  x<-min(which(littledat[i,] == "TB"))
  if(x != Inf){
    work<-littledat[i,]
    last <- x + 17
    socials<-which(work == "social") + 18
    socials<- socials[socials <= last]
    nonTBs<-which(work == "nonTB") + 18
    nonTBs<-nonTBs[nonTBs <= last]
  }
  #newer_data1[i,"row"] <- i
  if(length(socials) != 0){
    newer_data1[i,'social_time_pre']<-sum(work[,c(socials)])
  }else{newer_data1[i, 'social_time_pre'] <-0}
  if(length(nonTBs) !=0){
    newer_data1[i,'nonTB_time_pre']<-sum(work[,c(nonTBs)]) 
  }else{newer_data1[i,'nonTB_time_pre']<-0}
}

data<-cbind(data,newer_data1)

########   Step-Pairs (in case we use them later)  ########
#add new columns that show the step-pairs
start.col=which(colnames(subdata)=="Code0")
end.col=start.col+17;
for (m in start.col:end.col) #loop over all steps to create empty columns 
{
  stepname=paste('S',as.character(m-1),'.',as.character(m),sep='')
  subdata[stepname]<- 0 #add new columns
}

#loop over individuals
maxstep=rep(0,nrow(subdata)) # record max number of steps for everyone
for (n in 1:nrow(subdata)) #loop over all individuals
{
  for (m in start.col:end.col) #loop over all steps
  { 
    stepname=paste('S',as.character(m-1),'.',as.character(m),sep='')
    stepvalue=paste(as.character(subdata[n,m]),'.',as.character(subdata[n,m+1]),sep='')
    if ((!is.na(subdata[n,m+1]) & is.na(subdata[n,m+2])) | (!is.na(subdata[n,m+1]) & m==end.col) ) #last step, either if next one is NA or if we are at the last step 
    { 
      stepvalue=paste(stepvalue,'final',sep='') #this is the last step, add a "final" designation to the 2nd half of the step
      maxstep[n]=m
    } 
    subdata[n,stepname] = stepvalue #for each new step pair, assign some letter string, e.g. TB.TB or nonTB.social, etc.
  }
}


######### Find fraction of time with each contact type  #########

#compute new variables that record the fraction of steps 
#that are of one of the 3 summary types specified above, i.e. social, TB, nonTB
socialfrac = rep(0,length(nrow(data))); nonTBfrac=socialfrac; TBfrac=socialfrac;
for (n in 1:nrow(data)) 
{    
  socialfrac[n] = sum(subdata[n,2:18] == 'social',na.rm=TRUE) / sum(!is.na(subdata[n,2:18]))
  nonTBfrac[n] = sum(subdata[n,2:18] == 'nonTB',na.rm=TRUE) / sum(!is.na(subdata[n,2:18]))
  TBfrac[n] = sum(subdata[n,2:18] == 'TB',na.rm=TRUE) / sum(!is.na(subdata[n,2:18]))
}
data$socialfrac <- socialfrac
data$nonTBfrac <- nonTBfrac
data$TBfrac <- TBfrac


################################################################
################## Other variable recoding: ####################

########### Costs ###########

costDat<-data[184:303]
costDat[,2:120]<-sapply(costDat[,2:120],as.character)
for(n in 2:120){
  costDat[,n] <- str_replace(costDat[,n], "[:space:]", "")
}

#Medical
meds<-c("id_p5","Medication", "LabTest", paste("Medication",2:17,sep = "_"), paste("LabTest", 2:17, sep = "_"))
medical<-costDat[,colnames(costDat) %in% meds]
medical$Medication[medical$Medication == "~70"] <- 70 #fix, but check original surveys 
medical$Medication_3[medical$Medication_3 == "117 0 0"] <- 11700 #fix, but check original surveys
medical[,2:35]<-sapply(medical[,2:35],as.numeric)
medical$totalMedical <- NA
for(n in 1:nrow(medical)){
  medical$totalMedical[n]<-rowSums(medical[n,2:35],na.rm = TRUE) 
}
totalMedical<-medical$totalMedical
costDat<-cbind(costDat,totalMedical)

#Travel
trav<-c("id_p5", "Travel", "PhoneCalls", "Food", paste("Travel", 2:17, sep = "_"), paste("PhoneCalls",2:17, sep = "_"), paste("Food", 2:17,sep = "_"))
travel<-costDat[,colnames(costDat) %in% trav]
travel$PhoneCalls_10[travel$PhoneCalls_10 == "1~0"] <- 100 #fix, but check original surveys
travel$Travel[travel$Travel == "717 00"] <- 71700 #fix, but check original surveys
travel[,2:52]<-sapply(travel[,2:52],as.numeric)
travel$totaltravel <- NA
for(n in 1:nrow(travel)){
  travel$totaltravel[n]<-rowSums(travel[n,2:52],na.rm = TRUE) 
}
totalTravel<-travel$totaltravel
costDat<-cbind(costDat,totalTravel)

#Caregiver
care<-c("id_p5", "Caregiver", paste("Caregiver",2:17, sep = "_"))
caregiver<-costDat[,colnames(costDat) %in% care]
caregiver[,2:18]<-sapply(caregiver[,2:18],as.numeric)
caregiver$totalcaregiver <- NA
for(n in 1:nrow(caregiver)){
  caregiver$totalcaregiver[n]<-rowSums(caregiver[n,2:18],na.rm = TRUE) 
}
totalCaregiver<-caregiver$totalcaregiver
costDat<-cbind(costDat,totalCaregiver)

#total
for(n in 1:nrow(costDat)){
  costDat$totalCosts[n] <- rowSums(costDat[n,121:123], na.rm = TRUE)
}


#percent Medical
costDat$percentMedical <- costDat[,"totalMedical"]/costDat[,"totalCosts"]
costDat$percentTravel <- costDat[,"totalTravel"]/costDat[,"totalCosts"]
costDat$percentCaregiver <- costDat[,"totalCaregiver"]/costDat[,"totalCosts"]

data<-cbind(data,costDat[,121:127])


### some addition of time variables, converted to days: ###

#if you drink alcohol, for how long have you drunk alcohol?
data$alcohol_days[data$alcohol_days == 99] <- NA
data$alcohol_weeks[data$alcohol_weeks == 99] <- NA
data$alcohol_months[data$alcohol_months == 99] <- NA
data$Qn50Yrs[data$Qn50Yrs == 99] <- NA  #Qn50Yrs is the years for alcohol column, according to codebook
data$alcohol_time <- NA
for(n in 1:nrow(data)){
  data$alcohol_time[n] <- sum(data[n,"alcohol_days"], data[n,"alcohol_weeks"]*7, data[n,"alcohol_months"]*30.4375, data[n,"Qn50Yrs"]*365, na.rm = TRUE)
}

#if you smoke, for how long have you smoked?
data$smoking_days[data$smoking_days == 99] <- NA
data$smoking_weeks[data$smoking_weeks == 99] <- NA
data$smoking_months[data$smoking_months == 99] <- NA
data$smoking_yrs[data$smoking_yrs == 99] <- NA
data$smoking_time <- sum(data[n,"smoking_days"], data[n,"smoking_weeks"]*7, data[n, "smoking_months"]*30.4375, data[n,"smoking_yrs"]*365, na.rm = TRUE)

#do you owe anyone money? If so, when is it owed?
data$when_owed <- NA
data$when_owed_days[data$when_owed_days == 99] <- NA
data$when_owed_weeks[data$when_owed_weeks == 99] <- NA
data$when_owed_months[data$when_owed_months == 99] <- NA
for(n in 1:nrow(data)){
  data$when_owed[n] <- sum(data[n,"when_owed_days"], data[n,"when_owed_weeks"]*7, data[n,"when_owed_months"]*30.4375, na.rm = TRUE)
}

#duration of symptoms (cough?) before seeking first NONHEALTH advice. 99s for NA
data$symptom_dur_seek_nonhealth_days[data$symptom_dur_seek_nonhealth_days == 99] <- NA
data$symptom_dur_seek_nonhealth_weeks[data$symptom_dur_seek_nonhealth_weeks == 99] <- NA
data$symptom_dur_seek_nonhealth_months[data$symptom_dur_seek_nonhealth_months == 99] <- NA
data$symptom_dur_nonhealth <- NA
for(n in 1:nrow(data)){
  data$symptom_dur_nonhealth[n]<-sum(data[n,"symptom_dur_seek_nonhealth_days"],
                                     data[n,"symptom_dur_seek_nonhealth_weeks"]*7,
                                     data[n,"symptom_dur_seek_nonhealth_months"]*30.4375,na.rm = TRUE) 
}

#duration of symptoms (cough?) before seeking first HEALTH professional advice (no 99s for NA)
data$symptom_dur_health <- NA
for(n in 1:nrow(data)){
  data$symptom_dur_health[n]<-sum(data[n,"symptom_dur_seek_health_days"],
                                  data[n,"symptom_dur_seek_health_weeks"]*7,
                                  data[n,"symptom_dur_seek_health_months"]*30.4375, na.rm = TRUE) 
}

#how long have they owned a cell phone? (Qn86)
data$cell_phone_time <- NA
for(n in 1:nrow(data)){
  data$cell_phone_time[n]<-sum(data[n,"Qn86Days"],
                               data[n,"Qn86Weeks"]*7,
                               data[n,"Qn86Months"]*30.4375, 
                               data[n,"Qn86Yrs"]*365.25, na.rm = TRUE)
}



### Make factors pretty and ordered/unordered as necessary, and cleaning some related variables ###

#sex
data$sex<-factor(data$sex, levels = c(1:2), labels = c("Male", "Female"), ordered = FALSE)

#marital status
data$marital_status<-factor(data$marital_status, levels = c(1:4), labels = c("Single/Never Married",
                                                                             "Married/Cohabiting",
                                                                             "Separated/Divorced",
                                                                             "Widowed"), ordered = FALSE)

#division
data$division<-factor(data$division, levels = c(1:5,88), labels = c("Rubaga", "Nakawa", "Central",
                                                                    "Kawempe", "Makindye", "Other"), ordered = FALSE)
data$division_other<-as.character(data$division_other)

#education
data$education<-factor(data$education, levels = c(1:5), labels = c("None", "Primary", "Secondary", "Postsecondary", "University"), ordered = TRUE)

#employment
data$employment<-factor(data$employment, levels =c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#First TB episode?
data$first_tb<-factor(data$first_tb, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#Q44 HIV status
data$hiv_status<-factor(data$hiv_status, levels = c(1:3), labels = c("Positive", "Negative", "Don't Know"), ordered = FALSE)

#smoking
data$smoking_status<-factor(data$smoking_status, levels = c(1,3,2), labels = c("Never Smoked", "Former Smoker", "Current Smoker"), ordered = TRUE)

#sleep disruption:
data$night_disrupt<-factor(data$night_disrupt, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#daytime activities:
data$cough_disrupt_day<-factor(data$cough_disrupt_day, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#night or day disruption:
data$night_or_day<-factor(data$night_or_day, levels = c(1:2), labels = c("Day","Night"), ordered = FALSE)

#cough daily frequency 
data$cough_freq_days<-factor(data$cough_freq_days, levels = c(1, 3, 2, 4), labels = c("Rarely",
                                                                                      "Not so often",
                                                                                      "Often",
                                                                                      "Almost all the time"), ordered = TRUE)
#cough weekly frequency
data$cough_freq_weeks<-factor(data$cough_freq_weeks, levels = c(1:4), labels = c("Less than one day", "1-3 days", "4-6 days", "Every day"), ordered = TRUE)

#prior to dx, did cough produce sputum?
data$prior_dx_cough_sputum<-factor(data$prior_dx_cough_sputum, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#prior to dx, was there blood in sputum?
data$prior_dx_blood_sputum<-factor(data$prior_dx_blood_sputum, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#prior to dx, did cough cause shortness of breath?
data$prior_dx_cough_breath<-factor(data$prior_dx_cough_breath, levels = c(1,22), labels = c("Yes", "No"), ordered = FALSE)

#prior to dx, did cough cause pain?
data$prior_dx_cough_pain<-factor(data$prior_dx_cough_pain, levels = c(1, "Day"), labels = c("Yes", "No"), ordered = FALSE)

#did you take medicine for cough?
data$cough_med<-factor(data$cough_med, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#dx Location
#one dx_location is 23, 523: must look at his survey to see what value is.
#for now, converted to 88, assuming scanning error:
data$dx_location[data$dx_location == "23"] <- 88
data$dx_location<-factor(data$dx_location, levels = c(1:4,88), labels = c("Private Clinic",
                                                                          "Private Hospital",
                                                                          "Government Clinic",
                                                                          "Government Hospital",
                                                                          NA), ordered = FALSE)
#NOTE: Dx_location_other is completely NA: even for the dx_location that is "other", no listed value for dx_location_other

#Smear Results if smear done:
data$smear_result<-factor(data$smear_result, levels = c(1:5), labels = c("Smear Negative",
                                                                         "Smear Scanty",
                                                                         "Smear +",
                                                                         "Smear ++",
                                                                         "Smear +++"), ordered = TRUE)
#After first noticing cough, contact ANYONE for advice?
data$first_advice<-factor(data$first_advice, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)


#rate severity of cough in two weeks after onset:
data$cough_severe_2weeks[data$cough_severe_2weeks == "01  03"] <- 2 #taking average value
data$cough_severe_2weeks[data$cough_severe_2weeks == "02  03"] <- 2 #taking less extreme value
data$cough_severe_2weeks<-as.integer(data$cough_severe_2weeks)
data$cough_severe_2weeks<-factor(data$cough_severe_2weeks, levels = c(3:5), labels = c("Mild", "Moderate", "Severe"), ordered = TRUE)

#rate severity of cough entire time prior to dx:
data$cough_severe_prior_dx<-factor(data$cough_severe_prior_dx, levels = c(1:3), labels = c("Mild", "Moderate", "Severe"), ordered = TRUE)

#rate severity of cough one week prior:
data$cough_severe_week_prior[data$cough_severe_week_prior == "02  03"] <- 2
data$cough_severe_week_prior<-as.integer(data$cough_severe_week_prior)
data$cough_severe_week_prior<-factor(data$cough_severe_week_prior, levels = c(2:4), labels = c("Mild", "Moderate", "Severe"), ordered = TRUE)

#did patient seek care for symptoms that concerned them? Concerning symptoms in other question
data$seek_care_concerns<-factor(data$seek_care_concerns, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#did they suspect tb before dx?
data$tb_suspected<-factor(data$tb_suspected, levels = c(1:2), labels = c("Yes","No"), ordered = FALSE)

#Q43: did others express concern?
data$other_express_concern<-factor(data$other_express_concern, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#Q45: ART therapy
data$hiv_therapy <- factor(data$hiv_therapy, levels = c(1:3), labels = c("Yes", "No", "NA"), ordered = FALSE)

#Q46: do you have other conditions?
data$high_bp<-factor(data$high_bp, levels = c(1,2,77),labels = c("Yes", "No", "Uncertain"), ordered = FALSE)
data$diabetes<-factor(data$diabetes, levels = c(1,2,77), labels = c("Yes", "No", "Uncertain"), ordered = FALSE)
data$cancer<-factor(data$cancer, levels = c(1,2,77),labels = c("Yes", "No", "Uncertain"), ordered = FALSE)
data$kidney_disease<-factor(data$kidney_disease, levels = c(1,2,77), labels = c("Yes", "No", "Uncertain"), ordered = FALSE)
data$liver_disease<-factor(data$liver_disease, levels = c(1,2,77),labels = c("Yes", "No", "Uncertain"), ordered = FALSE)
data$asthma<-factor(data$asthma, levels = c(1,2,77), labels = c("Yes", "No", "Uncertain"), ordered = FALSE)
data$other_condition<-factor(data$other_condition, levels = c(1,2,77), labels = c("Yes", "No", "Uncertain"), ordered = FALSE)

#did you miss days of school/work prior to dx due to symptoms?
data$missed_days_prior<-factor(data$missed_days_prior, levels = c(1,2), labels = c("Yes", "No"), ordered = FALSE)

#did anyone have to stay home to care for you due to your symptoms?
data$stay_home_care <- factor(data$stay_home_care, levels = c(1,2), labels = c("Yes", "No"), ordered = FALSE)

#were you hospitalized for your symptoms prior to dx?
data$hospitalized_prior <- factor(data$hospitalized_prior, levels = c(1,2), labels = c("Yes", "No"), ordered = FALSE)

#Did you have to quit your job or schooling because of your tb?
#table(data$quit_tb) #one 01-99 (going with 01), and one 6 (going with 5)
data$quit_tb[data$quit_tb == "01  99"] <- 1
data$quit_tb <- factor(data$quit_tb, levels = c(1, 3, 6, 99), labels = c("No", "Quit Job", "Changed School", "NA"), ordered = FALSE)

#did you purchase any supplements to improve your symptoms?
data$buy_supplements <- factor(data$buy_supplements, levels = c(1:2),labels = c("Yes", "No"), ordered = FALSE)

#did you lose any daily earnings due to your tb?
data$lose_daily_earnings <- factor(data$lose_daily_earnings, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#did you borrow money due to your tb?
data$borrow_money <- factor(data$borrow_money, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#if you borrowed money, from whom?
data$borrowed_from <- factor(data$borrowed_from, levels = c(1:3,88), labels = c("Family", "Neighbors/Friends", "Bank", "Other"), ordered = FALSE)

#did others in your household (spouse, etc) have to borrow money due to your tb?
data$other_borrow <- factor(data$other_borrow, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#did you sell property due to your tb?
data$property_sold <- factor(data$property_sold, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#what items did you sell?
data$property_sold_item[data$property_sold_item == "04  99"] <- 4
data$property_sold_item <- factor(data$property_sold_item, levels = c(2:5), labels = c("Livestock", "Farm Produce", "Household Items", "Other"), ordered = FALSE)

#what were the value/cost of the items you sold?
for(n in 1:nrow(data)){
  data$items_cost[n] <- sum(data[n,"item_1_cost"], data[n,"item_2_cost"], data[n,"item_3_cost"], data[n,"item_4_cost"], na.rm = TRUE)
}

#did your children have to work due to your tb?
data$children_work <-factor(data$children_work, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)

#did your children miss school due to your tb?
data$children_miss_school<-factor(data$children_miss_school, levels = c(1:2), labels = c("Yes", "No"), ordered = FALSE)


########### Recode Multiple Responses ########
dat<-data #save progress
dat$id<-as.character(dat$id) 

### Q12: dx_method: method used to diagnose TB
smalldat<-dat[,c("id","dx_method")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$dx_method <- str_pad(smalldat$dx_method, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(dx_method, into = paste("D",1:2,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:3)
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04"),
                       labels = c("Diagnosed.Sputum.Smear", "Diagnosed.GeneXpert", 
                                  "Diagnosed.Chest.Xray", "Diagnosed.Sputum.Culture")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)
#if multiple dx methods, how many?
smalldat$Diagnosed.Multiple <- NA
for(n in 1:nrow(smalldat)){
  smalldat$Diagnosed.Multiple[n]<-sum(smalldat[n,2:5])
}
dat<-merge(dat,smalldat[,c(1:5,7)], by = "id")


### Q15: Who did you first contact when you noticed you had a cough?
smalldat<-dat[,c("id","first_contact")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$first_contact <- str_pad(smalldat$first_contact, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(first_contact, into = paste("C",1:4,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:5)
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04", "05", "88"),
                       labels = c("Contact.Family", "Contact.Friend", "Contact.Workmate", "Contact.FriendHealthWorker)", 
                                  "Contact.HealthProvider", "Contact.Other")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)
dat<-merge(dat,smalldat[,1:7], by = "id")


### Q36: Symptoms of TB that you knew about before receiving diagnosis
smalldat<-dat[,c("id","symptom_knowledge")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$symptom_knowledge <- str_pad(smalldat$symptom_knowledge, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(symptom_knowledge, into = paste("K",1:6,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:7)
smalldat$value[smalldat$value=="00"]<-"11"
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04", "05", "06","07","08","10","11"),
                       labels = c("Chronic.Cough.Know","Weight.Loss.Know", "Loss.Appetite.Know",
                                  "Chest.Pain.Know", "Fever.Know", "Malaise.Know", 
                                  "Bone.Joint.Know","Night.Sweats.Know","Cough.Blood.Know", 
                                  "Know.Not.Applicable")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)

smalldat$Cough.Know <- ifelse(rowSums(smalldat[,c("Chronic.Cough.Know", "Chest.Pain.Know","Cough.Blood.Know")]) > 0,1,0)
smalldat$Weight.Appetite.Know <- ifelse(rowSums(smalldat[,c("Weight.Loss.Know", "Loss.Appetite.Know")]) > 0, 1, 0)
smalldat$Fever.Sweat.Know <- ifelse(rowSums(smalldat[,c("Fever.Know", "Night.Sweats.Know")]) > 0, 1, 0)
dat<-merge(dat,smalldat[,-c(2:6,9:12)], by = "id")


### Q37: Symptoms of TB experienced
smalldat<-dat[,c("id","symptoms_experienced")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$symptoms_experienced <- str_pad(smalldat$symptoms_experienced, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(symptoms_experienced, into = paste("E",1:9,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:10)

smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04", "05", "06","07","08","10","99"),
                       labels = c("Experienced.Night.Sweats","Experienced.Appetite.Loss",
                                  "Experienced.Weight.Loss","Experienced.Chest.Pain","Experienced.Fever",
                                  "Experienced.Malaise","Experienced.Bone.Joint.Pain",
                                  "Experienced.Cough.Blood","Experienced.Cough.2Weeks","Experienced.NA")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)
dat<-merge(dat,smalldat[,1:11], by = "id")


### Q38: Symptoms of TB that concerned (other than cough)
smalldat<-dat[,c("id","symptoms_concerned")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$symptoms_concerned <- str_pad(smalldat$symptoms_concerned, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(symptoms_concerned, into = paste("E",1:9,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:10)
smalldat$value[smalldat$value=="00"]<-"99"
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04", "05", "06","07","08","99"),
                       labels = c("Concerned.Night.Sweats","Concerned.Appetite.Loss","Concerned.Weight.Loss",
                                  "Concerned.Chest.Pain","Concerned.Fever","Concerned.Malaise",
                                  "Concerned.Bone.Joint.Pain","Concerned.Cough.Blood","Concerned.NA")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)
dat<-merge(dat,smalldat[,1:10], by = "id")


### Q40: concerns_care_sought: if participant sought care for concerns, for which symptom?
smalldat<-dat[,c("id","concerns_care_sought")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$concerns_care_sought <- str_pad(smalldat$concerns_care_sought, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(concerns_care_sought, into = paste("E",1:6,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:7)
smalldat$value[smalldat$value=="00"]<-"99"
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04", "05", "06","07","08","99"),
                       labels = c("Sought.Night.Sweats","Sought.Appetite.Loss","Sought.Weight.Loss",
                                  "Sought.Chest.Pain","Sought.Fever","Sought.Malaise",
                                  "Sought.Bone.Joint.Pain","Sought.Cough.Blood","Sought.NA")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)

smalldat$Sought.Cough <- ifelse(rowSums(smalldat[,c("Sought.Chest.Pain","Sought.Cough.Blood")]) > 0,1,0)
smalldat$Sought.Weight.Appetite <- ifelse(rowSums(smalldat[,c("Sought.Weight.Loss", "Sought.Appetite.Loss")]) > 0, 1, 0)
smalldat$Sought.Fever.Sweat <- ifelse(rowSums(smalldat[,c("Sought.Fever", "Sought.Night.Sweats")]) > 0, 1, 0)
dat<-merge(dat,smalldat[,-c(2:6,9:11)], by = "id")



### Q41: which symptoms prompted seeking care SPECIFICALLY FOR TB: symptoms_prompt_eval
smalldat<-dat[,c("id","symptoms_prompt_eval")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$symptoms_prompt_eval <- str_pad(smalldat$symptoms_prompt_eval, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(symptoms_prompt_eval, into = paste("E",1:5,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:6)
smalldat$value[smalldat$value=="00"]<-"99"
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04", "05", "06","07","08","99"),
                       labels = c("Eval.Night.Sweats","Eval.Appetite.Loss","Eval.Weight.Loss",
                                  "Eval.Chest.Pain","Eval.Fever","Eval.Malaise",
                                  "Eval.Bone.Joint.Pain","Eval.Cough.Blood","Eval.NA")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)

smalldat$Eval.Cough <- ifelse(rowSums(smalldat[,c("Eval.Chest.Pain","Eval.Cough.Blood")]) > 0,1,0)
smalldat$Eval.Weight.Appetite <- ifelse(rowSums(smalldat[,c("Eval.Weight.Loss", "Eval.Appetite.Loss")]) > 0, 1, 0)
smalldat$Eval.Fever.Sweat <- ifelse(rowSums(smalldat[,c("Eval.Fever", "Eval.Night.Sweats")]) > 0, 1, 0)
dat<-merge(dat,smalldat[,-c(2:6,9:11)], by = "id")


### What prompted you to seek help?:
smalldat<-dat[,c("id","help_seeking_prompt")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$help_seeking_prompt <- str_pad(smalldat$help_seeking_prompt, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(help_seeking_prompt, c("First.Prompt", "Second.Prompt", "Third.Prompt"), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:4)
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04", "05","88","99"),
                       labels = c("prompt.self.relief", "prompt.freq.cough", "prompt.pain.cough", 
                                  "prompt.blood.cough", "prompt.TV.radio", "prompt.other", "prompt.NA")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) %>% distinct %>% spread(value, yesno, fill = 0)
#create prompt.number variable:
smalldat$prompt.number <- NA
for(n in 1:nrow(smalldat)){
  smalldat$prompt.number[n]<-sum(smalldat[n,2:8])
}
dat<-merge(dat,smalldat[,c(1:8,10)], by = "id")
#create cough-related prompt variable:
smalldat$cough.prompt.num <- NA
for(n in 1:nrow(dat)){
  smalldat$cough.prompt.num[n]<-sum(smalldat[n,3:5]) #cough frequently, cough blood, cough pain
}
dat<-merge(dat,smalldat[,c(1,11)], by = "id")

#Q67 what did you spend money on since medication for tb is free?
smalldat<-dat[,c("id","what_spent_after")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$what_spent_after <- str_pad(smalldat$what_spent_after, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(what_spent_after, into = paste("Spent",1:5,sep = ""), fill = "right") 
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:6)
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04","88","99"),
                       labels = c("Spent.Other.Medicines","Spent.Transport",
                                  "Spent.Buying.Food.Drink", "Spent.Supplementing.Food.Home",
                                  "Spent.Other","Spent.NA")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)
dat<-merge(dat,smalldat[,1:7], by = "id")

#54 supplements bought (did you buy supplements simple Yes/No, coded above)
smalldat<-dat[,c("id","supplements_bought")]
smalldat <- data.frame(lapply(smalldat, as.character), stringsAsFactors=FALSE)
smalldat$supplements_bought <- str_pad(smalldat$supplements_bought, width = 2, side = "left", pad = "0") 
smalldat <- smalldat %>% separate(supplements_bought, into = paste("Supplement",1:5,sep = ""), fill = "right")
smalldat <- smalldat %>% gather(key = "prompt", value = "value", 2:6)
smalldat$value<-factor(smalldat$value, levels = c("01", "02","03", "04","88","99"),
                       labels = c("Supplement.Fruit","Supplement.Drinks", "Supplement.Vit.Herbs",
                                  "Supplement.Meat","Supplement.Other","Supplement.NA")) 
smalldat <- smalldat[,-2] %>% mutate(yesno = 1) 
smalldat <- smalldat %>% distinct %>% spread(value, yesno, fill = 0)
dat<-merge(dat,smalldat[,1:7], by = "id") #now total of 488 variables

### Feature engineering to summarize several symptoms:
dat$Experienced_Concerned.Fever.Sweat <- ifelse(rowSums(dat[,c("Experienced.Night.Sweats","Concerned.Night.Sweats", 
                                                               "Experienced.Fever","Concerned.Fever")]) > 0, 1, 0)
dat$Experienced_concerned.Weight.Appetite <- ifelse(rowSums(dat[,c("Experienced.Appetite.Loss", "Concerned.Appetite.Loss", 
                                                                   "Experienced.Weight.Loss", "Concerned.Weight.Loss")]) > 0, 1, 0)
dat$Experienced_concerned.Cough <- ifelse(rowSums(dat[,c("Experienced.Chest.Pain", "Concerned.Chest.Pain", "Experienced.Cough.Blood", 
                                                         "Concerned.Cough.Blood", "Experienced.Cough.2Weeks")]) > 0, 1, 0)
dat$Experienced_concerned.Malaise <- ifelse(rowSums(dat[,c("Experienced.Malaise","Concerned.Malaise")]) > 0, 1, 0)
dat$Experienced_concerned.Bone.Joint.Pain <- ifelse(rowSums(dat[,c("Experienced.Bone.Joint.Pain", "Concerned.Bone.Joint.Pain")]) > 0, 1, 0)

######## Clean up final data set #########

#remove extra columns that were summarized or recoded above, pertain to the cell phone study recruitment, 
# or are identifiers for survey pages:
final <- dat[,c(1:4,7:15,18,20,25:26,31,35:38,40,43,45:48,50:51,53,55,57:58,60:61,63:66,
                  68,74:76,314:317,320:323,326:329,331:334,345,350,353,356:357,359,361,366,371,374,379:426,436,
                  445:460,462:468,470:474,476:481)]



write.csv(final, file = "diagnostic2.csv")

