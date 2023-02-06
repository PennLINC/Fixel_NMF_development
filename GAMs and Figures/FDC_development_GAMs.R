#This script is testing the development of white matter covariance networks in terms of Fiber density and cross-section (FDC) metric
#using Generelized Additive Models (GAMs). All GAMs include sex, mean DWI framewise displacement and number of DWI bad slices 
#(index of scan quality) as covariates. Included at the end of the script are sensitivity analyses in which we controlled 
#for Total Brain Volume and maternal education, respectively. 

########################
#### LOAD LIBRARIES ####
########################
library(rlang)
library(mgcv)
library(stringr)
library(ggplot2)
library(fitdistrplus)
require(utils)
library(mgcViz)
library(cowplot)
library(dplyr)
library(olsrr)

######################
#### READ IN DATA ####
######################
# root
root <- "/path/to/project/"

#load components data
fdc <- read.csv(paste0(root, "GAMs/input_data/14comp_loadings_fdc_ltn.csv"), header=F)
bblids<-read.csv(paste0(root, "nmf/mif_h5/cohort_files/ltn_FDC.csv"),header=T)

#load in-scanner QC data
QC <- read.csv(paste0(root, "/GAMs/input_data/QC_measures.csv"), header=T, sep=",")

#load behavioral data
# demos <- read.csv("/cbica/projects/GURLAB/dataFreezes/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv")
demo <- read.csv(paste0(root, "GAMs/input_data/n1601_demographics_go1_20161212.csv"))

#load cognitive data
cnb_tm <- read.csv(paste0(root, "GAMs/input_data/n1601_cnb_factor_scores_tymoore_20151006.csv"))

#load TBV data
TBV <- read.csv(paste0(root, "GAMs/input_data/n1601_ctVol20170412.csv"))

#######################
#### ORGANIZE DATA ####
#######################
bblids <- bblids%>%
  dplyr::rename(bblid = subject)

comp_fdc <- cbind(bblids$bblid,fdc)
#Rename variable correctly
comp_fdc <- comp_fdc%>%
  dplyr::rename(bblid = 'bblids$bblid')

#Just keep necessary variables
TBV <- TBV %>%
  dplyr::select(bblid,mprage_antsCT_vol_TBV) %>%
  rename(TBV = mprage_antsCT_vol_TBV)

colnames(cnb_tm)
cog <- cnb_tm %>%
  dplyr::select(bblid,F1_Exec_Comp_Res_Accuracy,F3_Executive_Efficiency)

#Make sex an ordered variable and age in years
demo <- demo%>%
  mutate(oSex = sex) %>%
  mutate(Age = ageAtScan1/12)
demo$oSex <- ordered(demo$oSex)

#Join dataframes
df_fdc <- left_join(comp_fdc, demo, by = "bblid")
df_fdc <- left_join(df_fdc, QC, by = "bblid")
df_fdc <- left_join(df_fdc, cog, by = "bblid")
df_fdc <- left_join(df_fdc, TBV, by = "bblid")

library(skimr)
skim(df_fdc)

#Get components numbering
comp_names<-df_fdc %>%
  dplyr::select(V1:V14)
Components <- names(comp_names)

createInteger <- function(f) {
  as.numeric(as.character(f))
}
comp_names <- as.data.frame(mapply(createInteger,comp_names))

#Check and remove missings for GAMs
sum(is.na(df_fdc$Age))
df_fdc <- df_fdc %>%
  filter(!is.na(Age)) %>%
  filter(!is.na(F1_Exec_Comp_Res_Accuracy)) %>%
  filter(!is.na(mean_fd)) %>%
  filter(!is.na(raw_num_bad_slices))

### FINAL SAMPLE FOR GAMs  IS N=939 (LOST N=2 DUE TO MISSING COGNITIVE DATA) ###
  
#List components' name
bundles <- c("Splenium","Fornix, cingulum","Inf. CST","Int. capsule","SLF, arcuate","Body of the CC","Rostrum","Sup. CST","Uncinate","SLF (parietal)","Middle CP",
           "Vermis","Sup. Cerebellum","U-fibers")
bundles <- as.data.frame(bundles)


########################
#### VISUALIZE DATA ####  ##Can skip this section##
########################
#Dependent variable (individuals' loadings on each of the 14 covariance networks)
foo <- function(x){
  require(ggplot2)
  ggplot(comp_names, aes(x = x)) + geom_histogram()
}
histograms<-lapply(comp_names,foo)
histogram<-plot_grid(plotlist=histograms,labels=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14"),ncol=3, nrow=5)
print(histogram) 

#SCATTER PLOT
#Explore raw age effects for all covariance network
smooth_plot_age <- function(yvar){
  require(ggplot2)
  ggplot(df_fdc, aes_(x=~Age, y=as.name(yvar))) + geom_point(color = "grey", alpha = 0.3) + 
    geom_smooth(method = "gam", formula = y ~s(x)) +
    theme_bw()
}
comp_age_list<-lapply(names(df_fdc[c(2:15)]), smooth_plot_age)
plot_age_comp_smooth<-plot_grid(plotlist=comp_age_list)
plot_age_comp_smooth


########################################
#### BUILD GAM MODELS - AGE EFFECTS ####
########################################
#Full model
gamModels_age_fdc <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + oSex + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc)
})

#Look at model summaries
models_age_fdc <- lapply(gamModels_age_fdc, summary)

#Pull F-statistics
fstat_age_fdc <- sapply(gamModels_age_fdc, function(v) summary(v)$s.table[3])
fstat_age_fdc <- as.data.frame(fstat_age_fdc)

#Pull age p-values 
p_age_fdc <- sapply(gamModels_age_fdc, function(v) summary(v)$s.table[4])
p_age_fdc <- as.data.frame(p_age_fdc)
p_age_fdc <- round(p_age_fdc,3)

#FDR corrected p-values
p_age_fdc_fdr <- as.data.frame(p.adjust(p_age_fdc[,1], method="fdr"))
p_age_fdc_fdr <- round(p_age_fdc_fdr,3)

#Join F-stats and FDR corrected p-values for age effects
age_gam <- cbind(bundles, fstat_age_fdc, p_age_fdc_fdr)

#Get partial R2 (as a metric of effect size) based on Chenying script (https://github.com/PennLINC/ModelArray_paper/blob/enh/figures/notebooks/utils.R#L12)
#Reduced models
df_fdc_min <- df_fdc %>% #no missing data here on any variable, thus can use df_fdc_min data for reduced model testing
  dplyr::select(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, Age, oSex, mean_fd, raw_num_bad_slices)
count(df_fdc_min[rowSums(is.na(df_fdc_min))==0,])

redmodel_fdc <- lapply(Components, function(x) {
  gam(substitute(i ~ oSex + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc_min)
})

partialRsq <- function(fullmodel, redmodel) {
  # calculating SSE: used observed y (i.e. excluding observations with NA), and fitted values, directly from model object
  
  sse.full <- sum( (fullmodel$y - fullmodel$fitted.values)^2 )
  sse.red <- sum( (redmodel$y - redmodel$fitted.values)^2 )
  
  partialRsq <- (sse.red - sse.full) / sse.red
  
  toReturn <- list(partialRsq = partialRsq,
                   sse.full = sse.full,
                   sse.red = sse.red)
  return(toReturn)
}

partialR2_V1 <- partialRsq(gamModels_age_fdc[[1]],redmodel_fdc[[1]])
partialR2_V2 <- partialRsq(gamModels_age_fdc[[2]],redmodel_fdc[[2]])
partialR2_V3 <- partialRsq(gamModels_age_fdc[[3]],redmodel_fdc[[3]])
partialR2_V4 <- partialRsq(gamModels_age_fdc[[4]],redmodel_fdc[[4]])
partialR2_V5 <- partialRsq(gamModels_age_fdc[[5]],redmodel_fdc[[5]])
partialR2_V6 <- partialRsq(gamModels_age_fdc[[6]],redmodel_fdc[[6]])
partialR2_V7 <- partialRsq(gamModels_age_fdc[[7]],redmodel_fdc[[7]])
partialR2_V8 <- partialRsq(gamModels_age_fdc[[8]],redmodel_fdc[[8]])
partialR2_V9 <- partialRsq(gamModels_age_fdc[[9]],redmodel_fdc[[9]])
partialR2_V10 <- partialRsq(gamModels_age_fdc[[10]],redmodel_fdc[[10]])
partialR2_V11 <- partialRsq(gamModels_age_fdc[[11]],redmodel_fdc[[11]])
partialR2_V12 <- partialRsq(gamModels_age_fdc[[12]],redmodel_fdc[[12]])
partialR2_V13 <- partialRsq(gamModels_age_fdc[[13]],redmodel_fdc[[13]])
partialR2_V14 <- partialRsq(gamModels_age_fdc[[14]],redmodel_fdc[[14]])

#Merge Partial R2 values with F-stats and p-values
partialR2<-as.data.frame(cbind(partialR2_V1[[1]],partialR2_V2[[1]],partialR2_V3[[1]],partialR2_V4[[1]],partialR2_V5[[1]],
                               partialR2_V6[[1]],partialR2_V7[[1]],partialR2_V8[[1]],partialR2_V9[[1]],partialR2_V10[[1]],
                               partialR2_V11[[1]],partialR2_V12[[1]],partialR2_V13[[1]],partialR2_V14[[1]]))
partialR2 <- as.data.frame(t(partialR2))
partialR2 <- partialR2 %>%
  dplyr::rename(partial_R2 = V1)
age_gam <- cbind(age_gam,partialR2)
age_gam$partial_R2 <- as.numeric(as.character(age_gam$partial_R2))
age_gam <- age_gam[order(-age_gam$partial_R2),]

########################################################################
#### FIGURE 3B - BAR PLOT OF PARTIAL R2 FOR EACH COVARIANCE NETWORK ####
########################################################################
# path to save figures at
root <- "path/to/project"

#First find colors similar to Figure 2 which depicts the 14 covariance networks on the brain
# Define the color ramp (returns a function object)
red <- colorRamp(c("brown2", "lightpink"))
# Define the ramp hex triplets
red.list <- rgb(red(seq(0, 1, length = 10)), max = 255)
print(red.list)
# Plot the color ramp
barplot(rep(1, 10), axes = FALSE, space = 0, col = red.list)

blue <- colorRamp(c("royalblue", "lightsteelblue1"))
blue.list<- rgb(blue(seq(0, 1, length = 10)), max = 255)
print(blue.list)
barplot(rep(1, 10), axes = FALSE, space = 0, col = blue.list)

green <-colorRamp(c("springgreen4", "grey92"))
green.list<- rgb(green(seq(0, 1, length = 10)), max = 255)
print(green.list)
barplot(rep(1, 10), axes = FALSE, space = 0, col = green.list)

yellow <-colorRamp(c("goldenrod2", "lightgoldenrodyellow"))
yellow.list<- rgb(yellow(seq(0, 1, length = 10)), max = 255)
print(yellow.list)
barplot(rep(1, 10), axes = FALSE, space = 0, col = yellow.list)

library(ggplot2)
age_gam$bundles <- factor(age_gam$bundles, levels = age_gam$bundles)
figure3b <- ggplot(age_gam, aes(x=bundles, y=partial_R2, fill=bundles)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual("Processing Method", values = c("Body of the CC" = "#F77F85", 
                                                    "SLF, arcuate" = "#008B45", 
                                                    "Splenium" = "#EE3B3B", 
                                                    "Fornix, cingulum" = "#42DFCE", 
                                                    "Sup. CST" = "#9CB9F5", 
                                                    "Inf. CST" = "#6E91EB", 
                                                    "U-fibers" = "#9CCBB3", 
                                                    "Rostrum" = "#FFB6C1", 
                                                    "SLF (parietal)" = "#D0E0D8", 
                                                    "Uncinate" = "#4EAB7C", 
                                                    "Sup. Cerebellum" = "#F7EAAA", 
                                                    "Vermis" = "#F3D370", 
                                                    "Int. capsule" = "#CAE1FF", 
                                                    "Middle CP" = "#EEB422")) + 
  labs(x = "Covariance Networks", y = expression(paste("Age Partial ", R^2))) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text.x = element_text(face="bold",size = 28, angle = 50, hjust = 1), 
        axis.text.y = element_text(face="bold",size = 30), 
        axis.title = element_text(size = 30), 
        axis.line= element_line(colour = 'black', size = 1.5), 
        legend.position = 'none', 
        plot.title = element_text(face="bold",size = 20)) + 
  # ggtitle("Non-linear age effects on FDC") +
  scale_y_continuous(breaks=c(0,0.05,0.10,0.15,0.20)) +
  theme(plot.title = element_text(hjust = 0.5))
figure3b
ggsave(plot = figure3b,filename = paste0(root, "figures/Figure3B_bargraph_partialR2.pdf"), device = "pdf",
       width = 210,height = 200,units = "mm")# Print the saved plot to the rmarkdown document (optional)


#########################################
#### COVARIANCE NETWORKS PREDICT AGE ####
#########################################
#Age ~ framewise_displacement + scan quality + 14NMF components, using linear modeling

Fullmodel_NMFpredictage <- lm(Age ~ oSex + raw_num_bad_slices + mean_fd + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14, data = df_fdc)
NullModel_NMFpredictage <- lm(Age ~  oSex + raw_num_bad_slices + mean_fd, data = df_fdc)
NMF_only_predict_age <- lm(Age ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14, data = df_fdc)


#Make sure network components are not collinear
ols_vif_tol(NMF_only_predict_age)

Fullmodel_NMFpredictage_summary <- summary(Fullmodel_NMFpredictage)
NullModel_NMFpredictage_summary <- summary(NullModel_NMFpredictage)
NMF_only_predict_age_summary <- summary(NMF_only_predict_age)
Fullmodel_NMFpredictage_summary
NullModel_NMFpredictage_summary
NMF_only_predict_age_summary # proportion of variance (R2) explained by networks

#1.Compare Fullmodel and Nullmodel with an F test to test significant contribution of networks in predicting age
Ftest <- anova(Fullmodel_NMFpredictage, NullModel_NMFpredictage)
Ftest

#2.Correlation between actual age and predicted age by NMF_only_predict_age model
NMFCor <- cor.test(predict(NMF_only_predict_age), df_fdc$Age)
NMFCor #the r value will be added in Figure 3C

###########################################################################
#### FIGURE 3C - CORRELATION PLOT BETWEEN PREDICTED AGE AND ACTUAL AGE ####
###########################################################################
# root <- "/Users/jbourque/UPENN/projects/PNC_fixel/gam_analysis/"
df_fdc$predicted_age <- predict(NMF_only_predict_age)
figure3c <- ggplot(aes(x = Age, y = predicted_age), data = df_fdc) +
  geom_point(color = "darkorange1", size = 2) +
  geom_smooth(method = "lm", formula = "y ~ x", color = "darkorange3", fill = "darkorange1") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.line= element_line(colour = 'black', size = 1.5)) +
  labs(x = "Age", y = "Predicted Age")
figure3c

ggsave(plot = figure3c, filename = paste0(root, "figures/Figure3C_NMF_predicted_vs_actual_age.pdf"), device = "pdf",
       width = 210, height = 200, units = "mm")


#####################################################################
#### FIGURE 4 - DEVELOPMENTAL PLOTS FOR EACH COVARIANCE NETWORKS ####
#####################################################################

# set plot font size
this_font_size = 50

# apply scatterplot and barplot functions to models
source(paste0(root, "/GAMs and Figures/plotting_functions.R"))
dev_plots_list <- lapply(X = gamModels_age_fdc, FUN = resid_plot, term = "Age", add.intercept = TRUE)
bar_plots_list <- lapply(X = gamModels_age_fdc, FUN = get_derivs_and_plot, smooth_var = "s(Age)")

# combine scatter plots and bar plots, save to pdf
final_plots <- lapply(seq_along(dev_plots_list),
       function(i) {
        allplots <- c(dev_plots_list[i], bar_plots_list[i])
        pg <- plot_grid(rel_heights = c(16,3.5), plotlist = allplots, align = "v", axis = "lr", ncol = 1)
        final_plot <- pg
        ggsave(plot = final_plot, filename = paste0(root, "figures/Figure4_derivative_V", i, ".pdf"),
               device = "pdf", width = 320, height = 290, units = "mm")
        return(final_plot)

       }
    )

################################
#### SUPPLEMENTARY ANALYSES ####
################################

################################################################
#### 1.Controlling for Total Brain Volume (TBV) in the GAMs ####
################################################################
gamModels_age_fdc_TBV <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + oSex + TBV + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc)
})

#Look at model summaries
models_age_fdc_TBV <- lapply(gamModels_age_fdc_TBV, summary)

## MAIN EFFECT OF AGE WITH TBV ##
#Pull F-statistics
fstat_fdc_TBV <- sapply(gamModels_age_fdc_TBV, function(v) summary(v)$s.table[3])
fstat_fdc_TBV <- as.data.frame(fstat_fdc_TBV)

#Pull p-values  
p_age_fdc_TBV <- sapply(gamModels_age_fdc_TBV, function(v) summary(v)$s.table[4])
p_age_fdc_TBV <- as.data.frame(p_age_fdc_TBV)
p_age_fdc_TBV <- round(p_age_fdc_TBV,3)

#FDR corrected p-values
p_age_fdc_fdr_TBV <- as.data.frame(p.adjust(p_age_fdc_TBV[,1], method="fdr"))
p_age_fdc_fdr_TBV <- round(p_age_fdc_fdr_TBV,3)

age_gam_TBV<-cbind(bundles,fstat_fdc_TBV,p_age_fdc_fdr_TBV)
age_gam_TBV<-age_gam_TBV%>%
  dplyr::rename(f_age=fstat_fdc_TBV)%>%
  dplyr::rename(p_fdr_age=`p.adjust(p_age_fdc_TBV[, 1], method = "fdr")`)

#Calculate Partial R2
df_fdc_TBV_min<-df_fdc %>% #no missing data here on any variable, thus can use df_fdc_min data for reduced model testing
  dplyr::select(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,Age,oSex,mean_fd,raw_num_bad_slices,TBV)
count(df_fdc_TBV_min[rowSums(is.na(df_fdc_TBV_min))==0,])

redmodel_fdc_TBV <- lapply(Components, function(x) {
  gam(substitute(i ~ oSex + raw_num_bad_slices + mean_fd + TBV, list(i = as.name(x))), method="REML", data = df_fdc_TBV_min)
})

partialR2_TBV_V1 <- partialRsq(gamModels_age_fdc_TBV[[1]],redmodel_fdc_TBV[[1]])
partialR2_TBV_V2 <- partialRsq(gamModels_age_fdc_TBV[[2]],redmodel_fdc_TBV[[2]])
partialR2_TBV_V3 <- partialRsq(gamModels_age_fdc_TBV[[3]],redmodel_fdc_TBV[[3]])
partialR2_TBV_V4 <- partialRsq(gamModels_age_fdc_TBV[[4]],redmodel_fdc_TBV[[4]])
partialR2_TBV_V5 <- partialRsq(gamModels_age_fdc_TBV[[5]],redmodel_fdc_TBV[[5]])
partialR2_TBV_V6 <- partialRsq(gamModels_age_fdc_TBV[[6]],redmodel_fdc_TBV[[6]])
partialR2_TBV_V7 <- partialRsq(gamModels_age_fdc_TBV[[7]],redmodel_fdc_TBV[[7]])
partialR2_TBV_V8 <- partialRsq(gamModels_age_fdc_TBV[[8]],redmodel_fdc_TBV[[8]])
partialR2_TBV_V9 <- partialRsq(gamModels_age_fdc_TBV[[9]],redmodel_fdc_TBV[[9]])
partialR2_TBV_V10 <- partialRsq(gamModels_age_fdc_TBV[[10]],redmodel_fdc_TBV[[10]])
partialR2_TBV_V11 <- partialRsq(gamModels_age_fdc_TBV[[11]],redmodel_fdc_TBV[[11]])
partialR2_TBV_V12 <- partialRsq(gamModels_age_fdc_TBV[[12]],redmodel_fdc_TBV[[12]])
partialR2_TBV_V13 <- partialRsq(gamModels_age_fdc_TBV[[13]],redmodel_fdc_TBV[[13]])
partialR2_TBV_V14 <- partialRsq(gamModels_age_fdc_TBV[[14]],redmodel_fdc_TBV[[14]])

#Merge Partial R2 values with F-stats and p-values
partialR2_TBV<-as.data.frame(cbind(partialR2_TBV_V1[[1]],partialR2_TBV_V2[[1]],partialR2_TBV_V3[[1]],partialR2_TBV_V4[[1]],partialR2_TBV_V5[[1]],
                                   partialR2_TBV_V6[[1]],partialR2_TBV_V7[[1]],partialR2_TBV_V8[[1]],partialR2_TBV_V9[[1]],partialR2_TBV_V10[[1]],
                                   partialR2_TBV_V11[[1]],partialR2_TBV_V12[[1]],partialR2_TBV_V13[[1]],partialR2_TBV_V14[[1]]))
partialR2_TBV<-as.data.frame(t(partialR2_TBV))
partialR2_TBV<-partialR2_TBV %>%
  dplyr::rename(partialR2=V1)
age_gam_TBV<-cbind(age_gam_TBV,partialR2_TBV)
age_gam_TBV$partialR2<-as.numeric(as.character(age_gam_TBV$partialR2))
age_gam_TBV<-age_gam_TBV[order(-age_gam_TBV$partialR2),]

##Bar plot of partial R2 while controlling for TBV
age_gam_TBV$bundles <- factor(age_gam_TBV$bundles, levels = age_gam_TBV$bundles)
figureS3A <- ggplot(age_gam_TBV, aes(x = bundles, y = partialR2, fill = bundles)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual("Processing Method", 
                    values = c("Body of the CC" = "#F77F85", 
                               "SLF, arcuate" = "#008B45", 
                               "Splenium" = "#EE3B3B", 
                               "Fornix, cingulum" = "#4169E1", 
                               "Sup. CST" = "#9CB9F5", 
                               "Inf. CST" = "#6E91EB", 
                               "U-fibers" = "#9CCBB3", 
                               "Rostrum" = "#FFB6C1", 
                               "SLF (parietal)" = "#D0E0D8", 
                               "Uncinate" = "#4EAB7C", 
                               "Sup. Cerebellum" = "#F7EAAA", 
                               "Vermis" = "#F3D370", 
                               "Int. capsule" = "#CAE1FF", 
                               "Middle CP" = "#EEB422")) + 
  labs(x = "Covariance Networks", y = expression(paste("Age Partial ", R^2))) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text.x = element_text(face="bold",size=18, angle = 50, hjust=1), 
        axis.text.y = element_text(face="bold",size=20), 
        axis.title = element_text(size=28), 
        axis.line= element_line(colour = 'black', size = 1.5), 
        legend.position = 'none', 
        legend.text = element_text(size=20), 
        legend.title = element_text(size = 24), 
        plot.title = element_text(face="bold",size = 20)) + 
  # ggtitle("Non-linear age effects on FDC") +
  scale_y_continuous(breaks=c(0,0.05,0.10,0.15,0.20)) +
  theme(plot.title = element_text(hjust = 0.5))
figureS3A
ggsave(plot = figureS3A,filename = paste0(root, "figures/FigureS3A_bargraph_partialR2_sensitivity_TBV.pdf"), device = "pdf",
       width = 210,height = 200,units = "mm")


##########################################################
#### 2.Controlling for Maternal education in the GAMs ####
##########################################################
gamModels_age_fdc_ME <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + oSex + medu1 + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc)
})

#Look at model summaries
models_age_fdc_ME <- lapply(gamModels_age_fdc_ME, summary)

## MAIN EFFECT OF AGE WITH maternal education ##
#Pull F-statistics
fstat_fdc_ME <- sapply(gamModels_age_fdc_ME, function(v) summary(v)$s.table[3])
fstat_fdc_ME <- as.data.frame(fstat_fdc_ME)

#Pull p-values - 
p_age_fdc_ME <- sapply(gamModels_age_fdc_ME, function(v) summary(v)$s.table[4])
p_age_fdc_ME <- as.data.frame(p_age_fdc_ME)
p_age_fdc_ME <- round(p_age_fdc_ME,3)

#FDR corrected p-values
p_age_fdc_fdr_ME <- as.data.frame(p.adjust(p_age_fdc_ME[,1], method="fdr"))
p_age_fdc_fdr_ME <- round(p_age_fdc_fdr_ME,3)

age_gam_ME<-cbind(bundles,fstat_fdc_ME,p_age_fdc_fdr_ME)
age_gam_ME<-age_gam_ME%>%
  dplyr::rename(f_age=fstat_fdc_ME)%>%
  dplyr::rename(p_fdr_age=`p.adjust(p_age_fdc_ME[, 1], method = "fdr")`)

#Calculate Partial R2
df_fdc_ME_min <- df_fdc %>% #no missing data here on any variable, thus can use df_fdc_min data for reduced model testing
  dplyr::select(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,Age,oSex,mean_fd,raw_num_bad_slices,medu1)
count(df_fdc_ME_min[rowSums(is.na(df_fdc_ME_min))==0,])

redmodel_fdc_ME <- lapply(Components, function(x) {
  gam(substitute(i ~ oSex + raw_num_bad_slices + mean_fd + medu1, list(i = as.name(x))), method="REML", data = df_fdc_ME_min)
})

partialR2_ME_V1 <- partialRsq(gamModels_age_fdc_ME[[1]],redmodel_fdc_ME[[1]])
partialR2_ME_V2 <- partialRsq(gamModels_age_fdc_ME[[2]],redmodel_fdc_ME[[2]])
partialR2_ME_V3 <- partialRsq(gamModels_age_fdc_ME[[3]],redmodel_fdc_ME[[3]])
partialR2_ME_V4 <- partialRsq(gamModels_age_fdc_ME[[4]],redmodel_fdc_ME[[4]])
partialR2_ME_V5 <- partialRsq(gamModels_age_fdc_ME[[5]],redmodel_fdc_ME[[5]])
partialR2_ME_V6 <- partialRsq(gamModels_age_fdc_ME[[6]],redmodel_fdc_ME[[6]])
partialR2_ME_V7 <- partialRsq(gamModels_age_fdc_ME[[7]],redmodel_fdc_ME[[7]])
partialR2_ME_V8 <- partialRsq(gamModels_age_fdc_ME[[8]],redmodel_fdc_ME[[8]])
partialR2_ME_V9 <- partialRsq(gamModels_age_fdc_ME[[9]],redmodel_fdc_ME[[9]])
partialR2_ME_V10 <- partialRsq(gamModels_age_fdc_ME[[10]],redmodel_fdc_ME[[10]])
partialR2_ME_V11 <- partialRsq(gamModels_age_fdc_ME[[11]],redmodel_fdc_ME[[11]])
partialR2_ME_V12 <- partialRsq(gamModels_age_fdc_ME[[12]],redmodel_fdc_ME[[12]])
partialR2_ME_V13 <- partialRsq(gamModels_age_fdc_ME[[13]],redmodel_fdc_ME[[13]])
partialR2_ME_V14 <- partialRsq(gamModels_age_fdc_ME[[14]],redmodel_fdc_ME[[14]])

#Merge Partial R2 values with F-stats and p-values
partialR2_ME<-as.data.frame(cbind(partialR2_ME_V1[[1]],partialR2_ME_V2[[1]],partialR2_ME_V3[[1]],partialR2_ME_V4[[1]],partialR2_ME_V5[[1]],
                                  partialR2_ME_V6[[1]],partialR2_ME_V7[[1]],partialR2_ME_V8[[1]],partialR2_ME_V9[[1]],partialR2_ME_V10[[1]],
                                  partialR2_ME_V11[[1]],partialR2_ME_V12[[1]],partialR2_ME_V13[[1]],partialR2_ME_V14[[1]]))
partialR2_ME<-as.data.frame(t(partialR2_ME))
partialR2_ME<-partialR2_ME %>%
  dplyr::rename(partialR2=V1)
age_gam_ME<-cbind(age_gam_ME,partialR2_ME)
age_gam_ME$partialR2<-as.numeric(as.character(age_gam_ME$partialR2))
age_gam_ME<-age_gam_ME[order(-age_gam_ME$partialR2),]

##Bar plot of partial R2 while controlling for Maternal education
age_gam_ME$bundles <- factor(age_gam_ME$bundles, levels = age_gam_ME$bundles)
figureS3B <- ggplot(age_gam_ME, aes(x = bundles, y = partialR2, fill = bundles)) + 
  geom_bar(stat="identity") + scale_fill_manual("Processing Method", 
                                                values = c("Body of the CC" = "#F77F85", 
                                                           "SLF, arcuate" = "#008B45", 
                                                           "Splenium" = "#EE3B3B", 
                                                           "Fornix, cingulum" = "#4169E1", 
                                                           "Sup. CST" = "#9CB9F5", 
                                                           "Inf. CST" = "#6E91EB", 
                                                           "U-fibers" = "#9CCBB3", 
                                                           "Rostrum" = "#FFB6C1", 
                                                           "SLF (parietal)" = "#D0E0D8", 
                                                           "Uncinate" = "#4EAB7C", 
                                                           "Sup. Cerebellum" = "#F7EAAA", 
                                                           "Vermis" = "#F3D370", 
                                                           "Int. capsule" = "#CAE1FF", 
                                                           "Middle CP" = "#EEB422")) + 
  labs(x = "Covariance Networks", y = expression(paste("Age Partial ", R^2))) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text.x = element_text(face="bold",size=18, angle = 50, hjust=1), 
        axis.text.y = element_text(face="bold",size=20), 
        axis.title = element_text(size=28), 
        axis.line= element_line(colour = 'black', size = 1.5), 
        legend.position = 'none', 
        legend.text = element_text(size=20), 
        legend.title = element_text(size = 24), 
        plot.title = element_text(face="bold",size = 20)) + 
  # ggtitle("Non-linear age effects on FDC") +
  scale_y_continuous(breaks=c(0,0.05,0.10,0.15,0.20)) +
  theme(plot.title = element_text(hjust = 0.5))
figureS3B
ggsave(plot = figureS3B,filename = paste0(root, "figures/FigureS3B_bargraph_partialR2_sensitivity_ME.pdf"), device = "pdf",
       width = 210,height = 200,units = "mm")



