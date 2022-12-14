#This script has the following goals:
#1-Get the final sample for DWI preprocessing, fixel reconstruction and NMF
#2-Create the list of subjects for FOD template creation
#3-Create the specific NMF data input file structure (list of bblids). This will be possible with using fixel_to_h5.sh as NMF will easily read h5 files.
#4-Create the split-half list of bblids and their file structure for the validation of NMF "optimal solution"

#### 1. GET FINAL SAMPLE FOR PREPROCESSING, FIXEL RECONSTRUCTION AND NMF ####
###################
## LOAD PACKAGES ##
###################
library(dplyr)


##################
## READ IN DATA ##
##################

##DEMOGRAPHIC DATA (n=1629)
demo<-read.csv("/cbica/projects/GURLAB/dataFreezes/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", 
              header=TRUE, na.strings="") 
#demo<-read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", 
#               header=TRUE, na.strings="") 

##COGNITIVE DATA
#summary factor scores
cogn<-read.csv("/cbica/projects/GURLAB/dataFreezes/n1601_dataFreeze/cnb/n1601_cnb_factor_scores_tymoore_20151006.csv", header=TRUE)
#cogn<-read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_factor_scores_tymoore_20151006.csv", header=TRUE)

##NEUROIMAGING DATA
#Diffusion
dwi <- read.csv("/cbica/projects/GURLAB/dataFreezes/n1601_dataFreeze/neuroimaging/dti/n1601_dti_qa_20170301.csv")
#dwi <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/dti/n1601_dti_qa_20170301.csv")

#Field map
B0 <- read.csv("/cbica/projects/GURLAB/dataFreezes/n1601_dataFreeze/neuroimaging/n1601_pnc_protocol_validation_params_status_20161220.csv")
#B0 <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/n1601_pnc_protocol_validation_params_status_20161220.csv")

#T1w - are not on cubic yet, so I downloaded them from chead to my local computer (might be on PMACS)
t1_qa <- read.csv("/Users/jbourque/UPENN/projects/PNC_fixel/Rfixel_cross-sectional/data_input/neuroimaging/n1601_t1QaData_20170306.csv")
#t1_qa <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv")

##EXCLUSION DATA - no missing data
ltnexclude<-read.csv("/cbica/projects/GURLAB/dataFreezes/n1601_dataFreeze/health/n1601_health_20170421.csv", 
                       header=TRUE)
#ltnexclude<-read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", 
#                        header=TRUE)


################
## MERGE DATA ##
################
neuro <- merge(dwi, B0 ,by=c("bblid","scanid"), all=T) 
neuro <- merge(neuro, t1_qa, by=c("bblid","scanid"), all=T )
neuro_demo<-merge(neuro, demo, by=c("bblid","scanid"), all.x=T)
neuro_demo_excl<-merge(neuro_demo, ltnexclude, by=c("bblid", "scanid"), all.x=T)
neuro_demo_excl<-merge(neuro_demo_excl, cogn, by=c("bblid", "scanid"), all.x=T)

#####################
## APPLY EXCLUSION ##
#####################
#We include subjects from ltnExclude considering the aim of the present study is to investigate "normal" brain development
#https://github.com/PennBBL/pncReproc2015Scripts/wiki/How-to-construct-a-sample-using-n=1,601-PNC-data
colnames(ltnexclude)
#use "ltnExlcudev2=0" for participants for normal development study
neuro_demo_ltn <- neuro_demo_excl%>%
  filter(ltnExcludev2==0)

##############################
## APPLYING NEUROIMAGING QA ##
##############################
neuro_demo_ltn <- neuro_demo_ltn%>%
  filter(dti64ProtocolValidationStatusExclude==0)%>% #dti protocol validation status exclusion
  filter(b0ProtocolValidationStatus==1) #fieldmap protocol validation status
  
neuro_demo_ltn <- neuro_demo_ltn%>%
  filter(t1Exclude==0)%>% #T1 QA exclusion
  filter(dti64Exclude==0) #David Roalf's QA exlcusion of DTI preprocessing

##########################
## EXCLUDE MISSING DATA ##
##########################
#Exclude those for which qsiprep did not work on flywheel - unique FOV parameters that did not match the rest of the sample
neuro_demo_ltn <- neuro_demo_ltn%>%
  filter(!bblid==82021)%>%
  filter(!bblid==85983)%>%
  filter(!bblid==90772)%>%
  filter(!bblid==91104)%>%
  filter(!bblid==92896)%>%
  filter(!bblid==93278)%>%
  filter(!bblid==93469)%>%
  filter(!bblid==96012)%>%
  filter(!bblid==98488)%>%
  filter(!bblid==108972)%>%
  filter(!bblid==109523)%>%
  filter(!bblid==112715)%>%
  filter(!bblid==112755)%>%
  filter(!bblid==113220)%>%
  filter(!bblid==114891)%>%
  filter(!bblid==118201)%>%
  filter(!bblid==120902)

#####################
## SAVE BBLID LIST ##
#####################
list<-neuro_demo_ltn%>%
  select(bblid)
list$bblid<-sort(list$bblid)

list$bblid<-as.character(list$bblid)
setwd("/cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/subject_lists")
write.table(list, "ltn_bblids.csv", append = FALSE, sep = ",", quote=F,
            row.names = F, col.names = F)


####2.TEMPLATE CREATION ####
###########################################
## APPLY EXCLUSION FOR TEMPLATE CREATION ##
###########################################
#use "squeakycleanExclude" to randomly select participants from different age bins for template creation
#https://github.com/PennBBL/pncReproc2015Scripts/wiki/How-to-construct-a-sample-using-n=1,601-PNC-data
neuro_demo_health <- neuro_demo_excl%>%
  filter(squeakycleanExclude==0)

#####################
## CREATE AGE BINS ##
#####################
(min(neuro_demo_health$ageAtScan1))/12
(max(neuro_demo_health$ageAtScan1))/12

#Create 16 bins from age 8 to age 23 with 2 participants within each bin (1 male, 1 female)
eight<-neuro_demo_health%>%
  filter(ageAtScan1>=96 & ageAtScan1<108)
nine<-neuro_demo_health%>%
  filter(ageAtScan1>=108 & ageAtScan1<120)
ten<-neuro_demo_health%>%
  filter(ageAtScan1>=120 & ageAtScan1<132)
eleven<-neuro_demo_health%>%
  filter(ageAtScan1>=132 & ageAtScan1<144)
twelve<-neuro_demo_health%>%
  filter(ageAtScan1>=144 & ageAtScan1<156)
thirteen<-neuro_demo_health%>%
  filter(ageAtScan1>=156 & ageAtScan1<168)
fourteen<-neuro_demo_health%>%
  filter(ageAtScan1>=168 & ageAtScan1<180)
fifteen<-neuro_demo_health%>%
  filter(ageAtScan1>=180 & ageAtScan1<192)
sixteen<-neuro_demo_health%>%
  filter(ageAtScan1>=192 & ageAtScan1<204)
seventeen<-neuro_demo_health%>%
  filter(ageAtScan1>=204 & ageAtScan1<216)
eighteen<-neuro_demo_health%>%
  filter(ageAtScan1>=216 & ageAtScan1<228)
nineteen<-neuro_demo_health%>%
  filter(ageAtScan1>=228 & ageAtScan1<240)
twenty<-neuro_demo_health%>%
  filter(ageAtScan1>=240 & ageAtScan1<252)
twentyone<-neuro_demo_health%>%
  filter(ageAtScan1>=252 & ageAtScan1<264)
twentytwo<-neuro_demo_health%>%
  filter(ageAtScan1>=264 & ageAtScan1<276)


#######################################
## SELECT 1M AND 1F FOR EACH AGE BIN ##
#######################################
a<-eight$bblid[eight$sex==1][sample(1:length(eight$bblid[eight$sex==1]),1)]
b<-eight$bblid[eight$sex==2][sample(1:length(eight$bblid[eight$sex==2]),1)]
c<-nine$bblid[nine$sex==1][sample(1:length(nine$bblid[nine$sex==1]),1)]
d<-nine$bblid[nine$sex==2][sample(1:length(nine$bblid[nine$sex==2]),1)]
e<-ten$bblid[ten$sex==1][sample(1:length(ten$bblid[ten$sex==1]),1)]
f<-ten$bblid[ten$sex==2][sample(1:length(ten$bblid[ten$sex==2]),1)]
g<-eleven$bblid[eleven$sex==1][sample(1:length(eleven$bblid[eleven$sex==1]),1)]
h<-eleven$bblid[eleven$sex==2][sample(1:length(eleven$bblid[eleven$sex==2]),1)]
j<-twelve$bblid[twelve$sex==1][sample(1:length(twelve$bblid[twelve$sex==1]),1)]
k<-twelve$bblid[twelve$sex==2][sample(1:length(twelve$bblid[twelve$sex==2]),1)]
l<-thirteen$bblid[thirteen$sex==1][sample(1:length(thirteen$bblid[thirteen$sex==1]),1)]
m<-thirteen$bblid[thirteen$sex==2][sample(1:length(thirteen$bblid[thirteen$sex==2]),1)]
n<-fourteen$bblid[fourteen$sex==1][sample(1:length(fourteen$bblid[fourteen$sex==1]),1)]
o<-fourteen$bblid[fourteen$sex==2][sample(1:length(fourteen$bblid[fourteen$sex==2]),1)]
p<-fifteen$bblid[fifteen$sex==1][sample(1:length(fifteen$bblid[fifteen$sex==1]),1)]
q<-fifteen$bblid[fifteen$sex==2][sample(1:length(fifteen$bblid[fifteen$sex==2]),1)]
r<-sixteen$bblid[sixteen$sex==1][sample(1:length(sixteen$bblid[sixteen$sex==1]),1)]
s<-sixteen$bblid[sixteen$sex==2][sample(1:length(sixteen$bblid[sixteen$sex==2]),1)]
t<-seventeen$bblid[seventeen$sex==1][sample(1:length(seventeen$bblid[seventeen$sex==1]),1)]
u<-seventeen$bblid[seventeen$sex==2][sample(1:length(seventeen$bblid[seventeen$sex==2]),1)]
v<-eighteen$bblid[eighteen$sex==1][sample(1:length(eighteen$bblid[eighteen$sex==1]),1)]
w<-eighteen$bblid[eighteen$sex==2][sample(1:length(eighteen$bblid[eighteen$sex==2]),1)]
x<-nineteen$bblid[nineteen$sex==1][sample(1:length(nineteen$bblid[nineteen$sex==1]),1)]
y<-nineteen$bblid[nineteen$sex==2][sample(1:length(nineteen$bblid[nineteen$sex==2]),1)]
z<-twenty$bblid[twenty$sex==1][sample(1:length(twenty$bblid[twenty$sex==1]),1)]
aa<-twenty$bblid[twenty$sex==2][sample(1:length(twenty$bblid[twenty$sex==2]),1)]
bb<-twentyone$bblid[twentyone$sex==1][sample(1:length(twentyone$bblid[twentyone$sex==1]),1)]
cc<-twentyone$bblid[twentyone$sex==2][sample(1:length(twentyone$bblid[twentyone$sex==2]),1)]
dd<-twentytwo$bblid[twentytwo$sex==1][sample(1:length(twentytwo$bblid[twentytwo$sex==1]),1)]
ee<-twentytwo$bblid[twentytwo$sex==2][sample(1:length(twentytwo$bblid[twentytwo$sex==2]),1)]

template<-cbind(a,b,c,d,e,f,g,h,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,bb,cc,dd,ee)
template<-as.data.frame(template)
template<-as.data.frame(t(template))
template<-template %>%
  dplyr::rename(bblid=V1)

write.table(template, "/cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/subject_lists/template.txt", append = FALSE, sep = " ",
            row.names = F, col.names = F)


#################################
## VALIDATION OF TEMPLATE LIST ##
#################################
#merge
valid<-semi_join(neuro_demo_excl,template, by="bblid")
colnames(valid)

valid <- valid %>%
  select(bblid, sex, race2, ethnicity, ageAtScan1, dti64QAManualScore) %>%
  mutate(age=ageAtScan1/12)

#Proportion of white (1), black (2) and others (3)
#Whole sample (n=1601)
demo<-semi_join(demo,healthexclude,by="bblid")

Race_ws_t<-table(demo$race2)
prop.table(Race_ws_t)

#template
Race_temp_t<-table(valid$race2)
prop.table(Race_temp_t)

#Proportion of ethnic groups. Hispanic ancestry (1), non-hispanic ancestry (2)
#Whole sample (n=1601)
Ethn_ws_t<-table(demo$ethnicity)
prop.table(Ethn_ws_t)

#template
Ethn_temp_t<-table(valid$ethnicity)
prop.table(Ethn_temp_t)


####3.CREATE NMF DATA INPUT FILE STRUCTURE ####
############################################
## CREATE SAMPLE CSV FILE FOR H5 CREATION ##
############################################
ltn <- neuro_demo_ltn %>%
  select(bblid) %>%
  mutate(scalar_name="FDC")%>%
  mutate(scalar_mif="FDC/fdc_10_smoothed_10fwhm_new/sub-")%>%
  mutate(mif=".mif")%>%
  dplyr::rename(subject=bblid)

ltn$scalar_mif=paste(ltn$scalar_mif,ltn$subject,ltn$mif,sep="")

ltn <- ltn %>%
  select(subject,scalar_name,scalar_mif)

ltn<-ltn[with(ltn,order(subject)),]

#save
write.table(ltn, file="/cbica/projects/pnc_fixel_cs/nmf/mif_h5/cohort_files/ltn_FDC.csv",row.names=F,sep=",")



####4.CREATE SPLIT-HALF FOR NMF SOLUTION VALIDATION####
#from Antonia Kaczkurkin's example script split-half for test and train samples
#/data/jux/BBL/projects/pncNmf/scripts/SplitHalfSample.rmd

###################
## LOAD PACKAGES ##
###################
library(ggplot2)
library(caret)
library(cowplot)
library(readr)

###################
## ORGANIZE DATA ##
###################
#Pull out only variables of interest for splitting and checking demographics.
dataToSplit <- neuro_demo_ltn[,c('bblid','sex','ageAtScan1','race2')]
dataToSplit<-dataToSplit %>%
  mutate(age=ageAtScan1/12)

#Remove any NA's from the variable you want to split on.
dataToSplit <- subset(dataToSplit,is.na(dataToSplit$age)==FALSE)

#Count number of subjects before and after removing NAs.
nrow(dataToSplit)

#Set a random number as the seed.
set.seed(1234)

##Split into the train and test data sets using caret.
#p=the percentage of data that goes to training (e.g., 50%)
#list=FALSE (gives a matrix with rows instead of list) 
#times=the number of partitions to create (number of training sets)
trainIndex <- createDataPartition(dataToSplit$age, p=0.50, list=F, times=1, 
                                groups=min(4,length(dataToSplit$age)))
#Pull the variables into the new train and test matrices.
dataTrain <- dataToSplit[trainIndex,]
dataTest <- dataToSplit[-trainIndex,]

#Count number of subjects in the train and test sets.
nrow(dataTrain)
nrow(dataTest)

#Or this option if you want to use multiple variables for the partition
#dataToSplit <- dataToSplit %>%
#  mutate(num = row_number()) %>% #create row number if you dont have one
#  select(num, everything()) # put 'n' at the front of the dataset
#train <- dataToSplit %>%
#  group_by(sex) %>% #any number of factors you wish to partition by proportionally
#  sample_frac(.5) # '.7' is the proportion of the original df you wish to sample
#test <- anti_join(dataToSplit, train) # creates test dataframe with those observations not in 'train.'

######################
## CHECK SIMILARITY ##
######################
hist(dataTrain$age,breaks=6)
hist(dataTest$age,breaks=6)

t.test(dataTrain$age,dataTest$age) #do not reject null hypothesis
sd(dataTrain$age)
sd(dataTest$age)

#Look for difference in sex ratio and ethnicity ratio
#Sex ratio
train_g<-dataTrain%>%
  mutate(Group=1) %>%
  group_by(Group, sex) %>%
  summarise(freq=n()) %>%
  ungroup()

test_g<-dataTest%>%
  mutate(Group=2) %>%
  group_by(Group, sex) %>%
  summarise(freq=n()) %>%
  ungroup()

sextab<-matrix(c(214,257,205,265),ncol=2, byrow=T)
colnames(sextab)<-c("Female","Male")
rownames(sextab)<-c("Train_sample","Test_sample")
sextab<-as.table(sextab)

chisq.test(sextab)

#Ethnicity ratio
train_eth<-dataTrain%>%
  mutate(Group=1) %>%
  group_by(Group, race2) %>%
  summarise(freq=n()) %>%
  ungroup()

test_eth<-dataTest%>%
  mutate(Group=2) %>%
  group_by(Group, race2) %>%
  summarise(freq=n()) %>%
  ungroup()

ethtab<-matrix(c(238,237,64,257,217,66),ncol=3, byrow=T)
colnames(ethtab)<-c("Caucasian","African-American","Other")
rownames(ethtab)<-c("Train_sample","Test_sample")
ethtab<-as.table(ethtab)

chisq.test(ethtab)


####################
## SAVE IDs LISTS ##
####################
?sort
train_id<-dataTrain %>%
  ungroup() %>%
  dplyr::select(bblid)

test_id<-dataTest %>%
  ungroup() %>%
  dplyr::select(bblid)

write.csv(train_id,'/cubic/projects/pnc_fixel_cs/nmf/mif_h5/cohort_files/train_list.csv',
          row.names=FALSE, quote=FALSE)
write.csv(test_id,'/cubic/projects/pnc_fixel_cs/nmf/mif_h5/cohort_files/test_list.csv',
          row.names=FALSE,quote=FALSE)

######################################
## CREATE CSV FILES FOR H5 CREATION ##
######################################
train_id_h5 <- train_id %>%
  mutate(scalar_name="FDC")%>%
  mutate(scalar_mif="FDC/fdc_10_smoothed_10fwhm_new/sub-")%>%
  mutate(mif=".mif")%>%
  dplyr::rename(subject=bblid)

train_id_h5$scalar_mif=paste(train_id_h5$scalar_mif,train_id_h5$subject,train_id_h5$mif,sep="")

train_id_h5 <- train_id_h5 %>%
  select(subject,scalar_name,scalar_mif)

train_id_h5<-train_id_h5[with(train_id_h5,order(subject)),]

test_id_h5 <- test_id %>%
  mutate(scalar_name="FDC")%>%
  mutate(scalar_mif="FDC/fd_10_smoothed_10fwhm_new/sub-")%>%
  mutate(mif=".mif")%>%
  dplyr::rename(subject=bblid)

test_id_h5$scalar_mif=paste(test_id_h5$scalar_mif,test_id_h5$subject,test_id_h5$mif,sep="")

test_id_h5 <- test_id_h5 %>%
  select(subject,scalar_name,scalar_mif)

test_id_h5<-test_id_h5[with(test_id_h5,order(subject)),]

#save
write.table(train_id_h5, file="/cbica/projects/pnc_fixel_cs/nmf/mif_h5/cohort_files/ltn_FDC_split1.csv",row.names=F,sep=",")
write.table(test_id_h5, file="/cbica/projects/pnc_fixel_cs/nmf/mif_h5/cohort_files/ltn_FDC_split2.csv",row.names=F,sep=",")
