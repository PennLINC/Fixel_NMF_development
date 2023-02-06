# This script is investigating whether white matter microstructure (FDC) underlies differences in executive function
# using Generelized Additive Models (GAMs). We used the Executive Efficiency factor to describe executive function performance. 
# All GAMs include sex, mean DWI framewise displacement and number of 
# DWI bad slices (index of scan quality) as covariates. Included at the end of the script are sensitivity analyses in which we 
# we controlled for Total Brain Volume and maternal education, respectively. 

######################
#### READ IN DATA ####
######################
source("./GAMs and Figures/FDC_development_GAMs.R")

# path to save figures at
root <- "/path/to/project/" 

########################
#### VISUALIZE DATA ####  ##Can skip this section##
########################
smooth_plot_EE <- function(yvar){
  require(ggplot2)
  ggplot(df_fdc, aes_(x=~F3_Executive_Efficiency,y=as.name(yvar))) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x))
}
plot_EE_list<-lapply(names(df_fdc[c(2:15)]),smooth_plot_EE)
plot_EE<-plot_grid(plotlist=plot_EE_list)
plot_EE

#######################################################
#### BUILD GAM MODELS - EXECUTIVE FUNCTION EFFECTS ####
#######################################################
# Executive Efficiency GAMs
gamModels_EE <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + F3_Executive_Efficiency + oSex + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc)
})

#Look at model summaries
models_EE <- lapply(gamModels_EE, summary)
models_EE[[1]]

#Pull t-values and p-values
cog_tvalue_EE <- sapply(gamModels_EE, function(v) summary(v)$p.table[2,3])
cog_tvalue_EE <- as.data.frame(cog_tvalue_EE)
cog_pvalue_EE <- sapply(gamModels_EE, function(v) summary(v)$p.table[2,4])
cog_pvalue_EE <- as.data.frame(cog_pvalue_EE)
cog_pvalue_EE <- round(cog_pvalue_EE,3)
cog_pvalue_EE_fdr <- as.data.frame(p.adjust(cog_pvalue_EE[,1], method="fdr"))
cog_pvalue_EE_fdr <- round(cog_pvalue_EE_fdr,3)

cog_stats<-cbind(bundles,cog_tvalue_EE,cog_pvalue_EE_fdr)
cog_stats<-cog_stats%>%
  dplyr::rename(t_EE=cog_tvalue_EE)%>%
  dplyr::rename(p_fdr_EE=`p.adjust(cog_pvalue_EE[, 1], method = "fdr")`)

#Pull Partial R2
df_fdc_cog_min <- df_fdc %>% #no missing data here on any variable, thus can use df_fdc_min data for reduced model testing
  dplyr::select(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,Age,oSex,mean_fd,raw_num_bad_slices,TBV,F1_Exec_Comp_Res_Accuracy,
                F3_Executive_Efficiency)
count(df_fdc_cog_min[rowSums(is.na(df_fdc_cog_min))==0,])

redmodel_cog <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + oSex + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc_cog_min)
})

partialR2_EE_V1 <- partialRsq(gamModels_EE[[1]],redmodel_cog[[1]])
partialR2_EE_V2 <- partialRsq(gamModels_EE[[2]],redmodel_cog[[2]])
partialR2_EE_V3 <- partialRsq(gamModels_EE[[3]],redmodel_cog[[3]])
partialR2_EE_V4 <- partialRsq(gamModels_EE[[4]],redmodel_cog[[4]])
partialR2_EE_V5 <- partialRsq(gamModels_EE[[5]],redmodel_cog[[5]])
partialR2_EE_V6 <- partialRsq(gamModels_EE[[6]],redmodel_cog[[6]])
partialR2_EE_V7 <- partialRsq(gamModels_EE[[7]],redmodel_cog[[7]])
partialR2_EE_V8 <- partialRsq(gamModels_EE[[8]],redmodel_cog[[8]])
partialR2_EE_V9 <- partialRsq(gamModels_EE[[9]],redmodel_cog[[9]])
partialR2_EE_V10 <- partialRsq(gamModels_EE[[10]],redmodel_cog[[10]])
partialR2_EE_V11 <- partialRsq(gamModels_EE[[11]],redmodel_cog[[11]])
partialR2_EE_V12 <- partialRsq(gamModels_EE[[12]],redmodel_cog[[12]])
partialR2_EE_V13 <- partialRsq(gamModels_EE[[13]],redmodel_cog[[13]])
partialR2_EE_V14 <- partialRsq(gamModels_EE[[14]],redmodel_cog[[14]])

partialR2_EE<-as.data.frame(cbind(partialR2_EE_V1[[1]],partialR2_EE_V2[[1]],partialR2_EE_V3[[1]],partialR2_EE_V4[[1]],partialR2_EE_V5[[1]],
                                   partialR2_EE_V6[[1]],partialR2_EE_V7[[1]],partialR2_EE_V8[[1]],partialR2_EE_V9[[1]],partialR2_EE_V10[[1]],
                                   partialR2_EE_V11[[1]],partialR2_EE_V12[[1]],partialR2_EE_V13[[1]],partialR2_EE_V14[[1]]))
partialR2_EE<-as.data.frame(t(partialR2_EE))
partialR2_EE<-partialR2_EE %>%
  dplyr::rename(partialR2_EE=V1)

#Join Partial R2 with t-stats and fdr p-values 
cog_stats<-cbind(cog_stats,partialR2_EE)
cog_stats$partialR2_EE<-as.numeric(as.character(cog_stats$partialR2_EE))
cog_stats<-cog_stats[order(-cog_stats$partialR2_EE),]

##############################################################################################
#### FIGURE 5B - BAR PLOT OF EXECUTIVE FUNCTIONING PARTIAL R2 FOR EACH COVARIANCE NETWORK ####
##############################################################################################
#Considering very similar results between EE and ECRA, in the paper we only presents results pertaining to the EE analysis (in Figure 5)
library(ggplot2)
cog_stats$bundles <- factor(cog_stats$bundles, levels = cog_stats$bundles)
figure5b <- ggplot(cog_stats, aes(x=bundles, y=partialR2_EE, fill=bundles)) + 
  geom_bar(stat="identity") + scale_fill_manual("Processing Method", values = c("Body of the CC" = "#F77F85", 
                                                                                "SLF, arcuate" = "#008B45", 
                                                                                "Splenium" = "#EE3B3B", 
                                                                                "Fornix, cingulum" = "#42DFCE", # "#4169E1", 
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
  labs(x ="Covariance Networks", y = expression(paste("EF Partial ", R^2))) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text.x = element_text(face="bold",size = 28, angle = 50, hjust=1), 
        axis.text.y = element_text(face="bold",size= 30), 
        axis.title = element_text(size = 30), 
        axis.line = element_line(colour = 'black', size = 1.5), 
        legend.position = 'none', legend.text = element_text(size=20), 
        legend.title = element_text(size = 24), 
        plot.title = element_text(face="bold",size = 20)) + 
  # ggtitle("Association of executive efficiency with white matter microstructure covariance networks") +
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0.2,0.2,0,1), "cm"))
figure5b
ggsave(plot = figure5b,filename = paste0(root, "figures/Figure5B_bargraph_partialR2_EE.pdf"),device = "pdf",width = 210,height = 200,units = "mm")# Print the saved plot to the rmarkdown document (optional)


########################################################
#### COVARIANCE NETWORKS PREDICT EXECUTIVE FUNCTION ####
########################################################
#Full model: EE ~ Age + sex + scan quality + motion + covariance networks 
#Null model: EE ~ Age + sex + scan quality + motion
#Ftest to test the significant contribution of covariance networks

# Full and Null Models for Executive Efficiency
FullModel_pred_EE <- lm(F3_Executive_Efficiency ~ Age + oSex + raw_num_bad_slices + mean_fd + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14,
                   data = df_fdc)

NullModel_pred_EE <- lm(F3_Executive_Efficiency ~  Age + oSex + raw_num_bad_slices + mean_fd,data = df_fdc)
ols_vif_tol(NullModel_pred_EE)

NMF_only_predict_EE <- lm(F3_Executive_Efficiency ~  V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14, data = df_fdc)

Full_pred_EE_summary <- summary(FullModel_pred_EE)
Null_pred_EE_summary <- summary(NullModel_pred_EE)
NMF_only_predict_EE_summary <- summary(NMF_only_predict_EE)
Full_pred_EE_summary
Null_pred_EE_summary
NMF_only_predict_EE_summary

# a.Compare Fullmodel and Nullmodel with an F test to test significant contribution of networks in predicting EE
Ftest_EE <- anova(FullModel_pred_EE, NullModel_pred_EE)
Ftest_EE # significant contribution of the covariance networks in predicting EE

# b.Correlation between actual EE and predicted EE by NMF_only_predict_EE model
NMF_EE_Cor <- cor.test(predict(NMF_only_predict_EE), df_fdc$F3_Executive_Efficiency)
NMF_EE_Cor #the r value will be added in Figure 5C

#########################################################################
#### FIGURE 5C - CORRELATION PLOT BETWEEN PREDICTED EE AND ACTUAL EE ####
#########################################################################
# pdf(file=paste0(root, "figures/Figure5C_NMF_predicted_vs_actual_EE.pdf")) # ,width=2000,height=2000,res=300
# par(mar=c(5,5,2,2), cex.axis=2, bty="l")
# plot(predict(NMF_only_predict_EE), df_fdc$F3_Executive_Efficiency, ylab="Executive Efficiency (z)", xlab="Predicted Executive Efficiency", cex.lab=2, pch=19, col="darkorange1")
# abline(a=0,b=1, col = "darkorange3", lwd = 4)
# dev.off()

df_fdc$predicted_F3 <- predict(NMF_only_predict_EE)
figure5c <- ggplot(aes(x = F3_Executive_Efficiency, y = predicted_F3), data = df_fdc) +
  geom_point(color = "darkorange1", size = 2) +
  geom_smooth(method = "lm", formula = "y ~ x", color = "darkorange3", fill = "darkorange1") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text.x = element_text(size = 30), 
        axis.text.y = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.line= element_line(colour = 'black', size = 1.5)) +
  labs(x = "Executive function (z)", y = "Predicted Executive function (z)")
figure5c

ggsave(plot = figure5c, filename = paste0(root, "figures/Figure5C_NMF_predicted_vs_actual_EE.pdf"), device = "pdf",
       width = 200, height = 210, units = "mm")


#######################################################################
#### FIGURE 5D - ASSOCIATION OF EE WITH FDC OF COVARIANCE NETWORKS ####
#######################################################################
#Create geom_smooth plot to show the above association for the three (3) most important covariance networks (based on partial R2)
#This part of the code was also taken from Adam Pines' 
#(https://github.com/PennBBL/multishell_diffusion/blob/master/PostProc/multishell_analyses.Rmd)

# plotting function
ef_plots <- function(p){
  p <-  p + 
    scale_y_continuous(breaks = seq(50, 150,by = 10)) +
    # ylab("FDC") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line.x = element_line(colour = 'gray20', size = 2), 
          axis.line.y = element_line(colour = 'gray20', size = 2), 
          axis.title.y = element_blank(),
          # axis.title.y = element_text(margin = margin(t = 0, r =55, b = 0, l = 0)),
          text = element_text(size=this_font_size),
          axis.text = element_text(size = this_font_size),
          # axis.title.y = element_text(margin = margin(t = 0, r = 39, b = 0, l = 0), size = 15),
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = this_font_size),
          legend.title = element_text(size = this_font_size),
          axis.title = element_text(size = this_font_size),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(0.2,0.2,0,0.2), "cm"),
          axis.ticks.length = unit(.25, "cm"),
          plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 30))
    ) #Top, left,Bottom, right
  return(p)
}

# apply scatterplot and barplot functions to significant EE and NMF networks associations:
# Association between EE and Fornix, cingulum (i.e.,V2)
# Association between EE and SLF (parietal) (i.e.,V10)
# Association between EE and SLF (i.e.,V5)
source(paste0(root, "/GAMs and Figures/plotting_functions.R"))
ef_plots_list <- lapply(X = gamModels_EE[c(2,10,5)], FUN = resid_plot, term = "F3_Executive_Efficiency", add.intercept = TRUE)
ggsave(plot = ef_plots_list[[1]], filename = paste0(root, "figures/Figure5D_EE_V2.pdf"), device = "pdf",width = 320, height = 220,units = "mm")# Print the saved plot to the rmarkdown document (optional)
ggsave(plot = ef_plots_list[[2]], filename = paste0(root, "figures/Figure5D_EE_V10.pdf"), device = "pdf",width = 320, height = 220,units = "mm")# Print the saved plot to the rmarkdown document (optional)
ggsave(plot = ef_plots_list[[3]], filename = paste0(root, "figures/Figure5D_EE_V5.pdf"), device = "pdf",width = 320, height = 220,units = "mm")# Print the saved plot to the rmarkdown document (optional)

################################
#### SUPPLEMENTARY ANALYSES ####
################################

##############################################################
#### Controlling for Total Brain Volume (TBV) in the GAMs ####
##############################################################
#Executive Efficiency
gamModels_EE_TBV <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + F3_Executive_Efficiency + oSex + TBV + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc)
})

#Look at model summaries
models_EE_TBV <- lapply(gamModels_EE_TBV, summary)
models_EE_TBV[[2]]

cog_tvalue_EE_TBV <- sapply(gamModels_EE_TBV, function(v) summary(v)$p.table[2,3])
cog_tvalue_EE_TBV <- as.data.frame(cog_tvalue_EE_TBV)
cog_pvalue_EE_TBV <- sapply(gamModels_EE_TBV, function(v) summary(v)$p.table[2,4])
cog_pvalue_EE_TBV <- as.data.frame(cog_pvalue_EE_TBV)
cog_pvalue_EE_TBV <- round(cog_pvalue_EE_TBV,3)
cog_pvalue_EE_TBV_fdr <- as.data.frame(p.adjust(cog_pvalue_EE_TBV[,1], method="fdr"))
cog_pvalue_EE_TBV_fdr <- round(cog_pvalue_EE_TBV_fdr,3)


cog_stats_TBV<-cbind(bundles,cog_tvalue_EE_TBV,cog_pvalue_EE_TBV_fdr)
cog_stats_TBV<-cog_stats_TBV%>%
  dplyr::rename(t_EE_TBV=cog_tvalue_EE_TBV)%>%
  dplyr::rename(p_fdr_EE_TBV=`p.adjust(cog_pvalue_EE_TBV[, 1], method = "fdr")`)

#Calculate Partial R2
redmodel_EE_TBV <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + oSex + raw_num_bad_slices + mean_fd + TBV, list(i = as.name(x))), method="REML", data = df_fdc)
})

partialR2_EE_TBV_V1 <- partialRsq(gamModels_EE_TBV[[1]],redmodel_EE_TBV[[1]])
partialR2_EE_TBV_V2 <- partialRsq(gamModels_EE_TBV[[2]],redmodel_EE_TBV[[2]])
partialR2_EE_TBV_V3 <- partialRsq(gamModels_EE_TBV[[3]],redmodel_EE_TBV[[3]])
partialR2_EE_TBV_V4 <- partialRsq(gamModels_EE_TBV[[4]],redmodel_EE_TBV[[4]])
partialR2_EE_TBV_V5 <- partialRsq(gamModels_EE_TBV[[5]],redmodel_EE_TBV[[5]])
partialR2_EE_TBV_V6 <- partialRsq(gamModels_EE_TBV[[6]],redmodel_EE_TBV[[6]])
partialR2_EE_TBV_V7 <- partialRsq(gamModels_EE_TBV[[7]],redmodel_EE_TBV[[7]])
partialR2_EE_TBV_V8 <- partialRsq(gamModels_EE_TBV[[8]],redmodel_EE_TBV[[8]])
partialR2_EE_TBV_V9 <- partialRsq(gamModels_EE_TBV[[9]],redmodel_EE_TBV[[9]])
partialR2_EE_TBV_V10 <- partialRsq(gamModels_EE_TBV[[10]],redmodel_EE_TBV[[10]])
partialR2_EE_TBV_V11 <- partialRsq(gamModels_EE_TBV[[11]],redmodel_EE_TBV[[11]])
partialR2_EE_TBV_V12 <- partialRsq(gamModels_EE_TBV[[12]],redmodel_EE_TBV[[12]])
partialR2_EE_TBV_V13 <- partialRsq(gamModels_EE_TBV[[13]],redmodel_EE_TBV[[13]])
partialR2_EE_TBV_V14 <- partialRsq(gamModels_EE_TBV[[14]],redmodel_EE_TBV[[14]])

partialR2_EE_TBV<-as.data.frame(cbind(partialR2_EE_TBV_V1[[1]],partialR2_EE_TBV_V2[[1]],partialR2_EE_TBV_V3[[1]],partialR2_EE_TBV_V4[[1]],partialR2_EE_TBV_V5[[1]],
                                      partialR2_EE_TBV_V6[[1]],partialR2_EE_TBV_V7[[1]],partialR2_EE_TBV_V8[[1]],partialR2_EE_TBV_V9[[1]],partialR2_EE_TBV_V10[[1]],
                                      partialR2_EE_TBV_V11[[1]],partialR2_EE_TBV_V12[[1]],partialR2_EE_TBV_V13[[1]],partialR2_EE_TBV_V14[[1]]))
partialR2_EE_TBV<-as.data.frame(t(partialR2_EE_TBV))
partialR2_EE_TBV<-partialR2_EE_TBV %>%
  dplyr::rename(partialR2_EE_TBV=V1)


#Merge Partial R2 values with F-stats and p-values
cog_stats_TBV<-cbind(cog_stats_TBV,partialR2_EE_TBV)
cog_stats_TBV$partialR2<-as.numeric(as.character(cog_stats_TBV$partialR2))
cog_stats_TBV_order<-cog_stats_TBV[order(-cog_stats_TBV$partialR2),]

cog_stats_all<-left_join(cog_stats,cog_stats_TBV, by="bundles")
colnames(cog_stats_all)
cog_stats_save<-cog_stats_all%>%
  dplyr::select(bundles,p_fdr_EE,partialR2_EE,p_fdr_EE_TBV,partialR2_EE_TBV)


##Bar plot of EE partial R2 while controlling for TBV
cog_stats_TBV_order$bundles <- factor(cog_stats_TBV_order$bundles, levels = cog_stats_TBV_order$bundles)
figureS4A<-ggplot(cog_stats_TBV_order, aes(x=bundles, y=partialR2, fill=bundles)) + 
  geom_bar(stat="identity") + 
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
  labs(x = "Covariance Networks", y = expression(paste("EF Partial ", R^2))) + 
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
  scale_y_continuous(breaks=c(0,0.005,0.01,0.015)) +
  theme(plot.title = element_text(hjust = 0.5))
figureS4A
ggsave(plot = figureS4A,filename = paste0(root, "figures/FigureS4A_bargraph_partialR2_EE_sensitivity_TBV.pdf"), device = "pdf",width = 200,height = 210,units = "mm")# Print the saved plot to the rmarkdown document (optional)


########################################################
#### Controlling for Maternal Education in the GAMs ####
########################################################
#Executive Efficiency
gamModels_EE_ME <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + F3_Executive_Efficiency + oSex + medu1 + raw_num_bad_slices + mean_fd, list(i = as.name(x))), method="REML", data = df_fdc)
})

#Look at model summaries
models_EE_ME <- lapply(gamModels_EE_ME, summary)
models_EE_ME[[2]]

cog_tvalue_EE_ME <- sapply(gamModels_EE_ME, function(v) summary(v)$p.table[2,3])
cog_tvalue_EE_ME <- as.data.frame(cog_tvalue_EE_ME)
cog_pvalue_EE_ME <- sapply(gamModels_EE_ME, function(v) summary(v)$p.table[2,4])
cog_pvalue_EE_ME <- as.data.frame(cog_pvalue_EE_ME)
cog_pvalue_EE_ME <- round(cog_pvalue_EE_ME,3)
cog_pvalue_EE_ME_fdr <- as.data.frame(p.adjust(cog_pvalue_EE_ME[,1], method="fdr"))
cog_pvalue_EE_ME_fdr <- round(cog_pvalue_EE_ME_fdr,3)


cog_stats_ME<-cbind(bundles,cog_tvalue_EE_ME,cog_pvalue_EE_ME_fdr)
cog_stats_ME<-cog_stats_ME%>%
  dplyr::rename(t_EE_ME=cog_tvalue_EE_ME)%>%
  dplyr::rename(p_fdr_EE_ME=`p.adjust(cog_pvalue_EE_ME[, 1], method = "fdr")`)

#Calculate Partial R2
redmodel_EE_ME <- lapply(Components, function(x) {
  gam(substitute(i ~ s(Age) + oSex + raw_num_bad_slices + mean_fd + medu1, list(i = as.name(x))), method="REML", data = df_fdc)
})

partialR2_EE_ME_V1 <- partialRsq(gamModels_EE_ME[[1]],redmodel_EE_ME[[1]])
partialR2_EE_ME_V2 <- partialRsq(gamModels_EE_ME[[2]],redmodel_EE_ME[[2]])
partialR2_EE_ME_V3 <- partialRsq(gamModels_EE_ME[[3]],redmodel_EE_ME[[3]])
partialR2_EE_ME_V4 <- partialRsq(gamModels_EE_ME[[4]],redmodel_EE_ME[[4]])
partialR2_EE_ME_V5 <- partialRsq(gamModels_EE_ME[[5]],redmodel_EE_ME[[5]])
partialR2_EE_ME_V6 <- partialRsq(gamModels_EE_ME[[6]],redmodel_EE_ME[[6]])
partialR2_EE_ME_V7 <- partialRsq(gamModels_EE_ME[[7]],redmodel_EE_ME[[7]])
partialR2_EE_ME_V8 <- partialRsq(gamModels_EE_ME[[8]],redmodel_EE_ME[[8]])
partialR2_EE_ME_V9 <- partialRsq(gamModels_EE_ME[[9]],redmodel_EE_ME[[9]])
partialR2_EE_ME_V10 <- partialRsq(gamModels_EE_ME[[10]],redmodel_EE_ME[[10]])
partialR2_EE_ME_V11 <- partialRsq(gamModels_EE_ME[[11]],redmodel_EE_ME[[11]])
partialR2_EE_ME_V12 <- partialRsq(gamModels_EE_ME[[12]],redmodel_EE_ME[[12]])
partialR2_EE_ME_V13 <- partialRsq(gamModels_EE_ME[[13]],redmodel_EE_ME[[13]])
partialR2_EE_ME_V14 <- partialRsq(gamModels_EE_ME[[14]],redmodel_EE_ME[[14]])

partialR2_EE_ME<-as.data.frame(cbind(partialR2_EE_ME_V1[[1]],partialR2_EE_ME_V2[[1]],partialR2_EE_ME_V3[[1]],partialR2_EE_ME_V4[[1]],partialR2_EE_ME_V5[[1]],
                                     partialR2_EE_ME_V6[[1]],partialR2_EE_ME_V7[[1]],partialR2_EE_ME_V8[[1]],partialR2_EE_ME_V9[[1]],partialR2_EE_ME_V10[[1]],
                                     partialR2_EE_ME_V11[[1]],partialR2_EE_ME_V12[[1]],partialR2_EE_ME_V13[[1]],partialR2_EE_ME_V14[[1]]))
partialR2_EE_ME<-as.data.frame(t(partialR2_EE_ME))
partialR2_EE_ME<-partialR2_EE_ME %>%
  dplyr::rename(partialR2_EE_ME=V1)


#Merge Partial R2 values with F-stats and p-values
cog_stats_ME<-cbind(cog_stats_ME,partialR2_EE_ME)
cog_stats_ME$partialR2<-as.numeric(as.character(cog_stats_ME$partialR2))
cog_stats_ME_order<-cog_stats_ME[order(-cog_stats_ME$partialR2),]

cog_stats_all<-left_join(cog_stats,cog_stats_ME, by="bundles")
colnames(cog_stats_all)
cog_stats_save<-cog_stats_all%>%
  dplyr::select(bundles,p_fdr_EE,partialR2_EE,p_fdr_EE_ME,partialR2_EE_ME)


##Bar plot of EE partial R2 while controlling for Maternal education
cog_stats_ME_order$bundles <- factor(cog_stats_ME_order$bundles, levels = cog_stats_ME_order$bundles)
figureS4B<-ggplot(cog_stats_ME_order, aes(x=bundles, y=partialR2, fill=bundles)) + 
  geom_bar(stat="identity") + 
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
  labs(x = "Covariance Networks", y = expression(paste("EF Partial ", R^2))) + 
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
  scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06)) +
  theme(plot.title = element_text(hjust = 0.5))
figureS4B
ggsave(plot = figureS4B,filename = paste0(root, "figures/figureS4B_bargraph_partialR2_EE_sensitivity_ME.pdf"), device = "pdf",width = 200,height = 210,units = "mm")# Print the saved plot to the rmarkdown document (optional)

