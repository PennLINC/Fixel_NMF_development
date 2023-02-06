########### PLOTTING FUNCTIONS FOR FIXEL PAPER ###############

# This script contains plotting functions to generate scatterplots and age related changes for figures 4 and 5. 

#### Load packages ####
library(gratia)
library(dplyr)
library(svglite)
library(cowplot)
library(scales)
library(grr)

#### Define network colors and names ####
# append coloring variable for each NMF network
line_colors <- c("#EE3B3B", # V1 - CC splenium
                 "#42DFCE", # V2 - fornix, cingulum
                 "#6E91EB", # V3 - inferior CST
                 "#CAE1FF", # V4 - internal capsule
                 "#008B45", # V5 - SLF, arcuate
                 "#F77F85", # V6 - body of the CC
                 "#FFB6C1", # V7 - rostrum of the CC
                 "#9CB9F5", # V8 - superior CST
                 "#4EAB7C", # V9 - uncinate
                 "#D0E0D8", # V10 - SLF (parietal)
                 "#EEB422", # V11 - middle cerebellar peduncle
                 "#F3D370", # V12 - vermis
                 "#F7EAAA", # V13 - superior cerebellum
                 "#9CCBB3" # V14 - U-fibers
) 
nmf_names <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14")
nmf_full_names <- c("Splenium of the CC", # V1
                    "Fornix, cingulum", # V2
                    "Inferior CST", # V3
                    "Internal capsule", # V4
                    "SLF, arcuate", # V5
                    "Body of the CC", # V6
                    "Rostrum of the CC",# V7
                    "Superior CST", # V8
                    "Uncinate", # V9
                    "SLF (parietal)", # V10
                    "Middle cerebellar peduncle", # V11
                    "Vermis", # V12
                    "Superior cerebellum", # V13
                    "U-fibers" # V14
)

# create dataframe with colors and names 
colors_df <- data.frame(nmf_names, nmf_full_names, line_colors)

##########################################################################
### Developmental scatterplots for each of the 14 covariance networks ####
# This is based on Bart Larsen's plotting function, adapted to show partial residuals
# Inputs: 
#   - modobj: model object (should work for lm and gam)
#   - term: the term you want to plot (as a string, e.g. “Age”)
#   - add.intercept: if set to true, this adds the intercept back to the residuals so that they are in same range of values as original data
resid_plot <- function(modobj,term,add.intercept=FALSE){
  df <- modobj$model
  mod.intercept <- modobj$coefficients["(Intercept)"]
  pterms <- predict(modobj,type = "terms",se.fit = TRUE)
  
  if (add.intercept==TRUE) {
    pterms.fit <- pterms$fit+mod.intercept
  } else{
    pterms.fit <- pterms$fit
  }
  pterms.sefit <- pterms$se.fit
  
  colnames(pterms.fit) <- gsub(x = colnames(pterms.fit),pattern = "s\\(",replacement = "")%>%
    gsub(pattern = "\\)",replacement = "")
  colnames(pterms.sefit) <- gsub(x = colnames(pterms.sefit),pattern = "s\\(",replacement = "")%>%
    gsub(pattern = "\\)",replacement = "")
  
  pterms.df <- data.frame(pterms.fit) %>% 
    dplyr::select(matches(term)) %>%
    rename(fit:=!!term) %>%
    cbind(data.frame(pterms.sefit) %>% 
    dplyr::select(matches(term)) %>%
    rename(se.fit:=!!term)) %>%
    mutate(upr = fit + 1.96*se.fit,
           lwr = fit - 1.96*se.fit)
  
  partial.residuals <- data.frame(pterms.fit) %>%
    mutate(across(.cols = everything(),.fns = function(x){x + resid(modobj)})) %>%
    cbind(rawdata=df[,term])
  
  plot.df <- cbind(partial.residuals,pterms.df)
  
  # default plot
  # ggplot(plot.df,aes_string(x = "rawdata", y = term)) +
  #   geom_point(size = 2, colour = "gray56") +
  #   geom_ribbon(aes(x = rawdata, y = fit, ymin = lwr, ymax = upr),inherit.aes = FALSE, alpha = .7) +
  #   geom_line(aes(x = rawdata,y = fit), inherit.aes = FALSE) +
  #   xlab(term) + ylab("partial.residuals")
  
  # get network names and coloring
  nmf_network <- all.vars(modobj$formula)[1]
  line_color <- colors_df[which(colors_df$nmf_names == nmf_network), "line_colors"]

  # adapted plot for paper figure aesthetics
  p <- ggplot(plot.df, aes_string(x = "rawdata", y = term)) +
    geom_point(size = 2, colour = "gray56") +
    geom_ribbon(aes(x = rawdata, y = fit, ymin = lwr, ymax = upr), fill = line_color,inherit.aes = FALSE, alpha = .7) +
    geom_line(aes(x = rawdata,y = fit), color = line_color, inherit.aes = FALSE) +
    labs(y = "FDC", title = toString(colors_df[which(colors_df$nmf_names == nmf_network), "nmf_full_names"])) +
    scale_y_continuous(breaks = seq(30, 150, by = 10)) +
    theme(text = element_text(size = this_font_size),
          axis.text = element_text(size = this_font_size),
          # axis.title.y = element_text(size = this_font_size, margin = margin(t = 0, r = 39, b = 0, l = 0)),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = unit(c(0.2,0.2,0.3,1), "cm"), #Top, left, Bottom, right
          axis.line.x = element_line(colour = 'gray20', size = 2),
          axis.line.y = element_line(colour = 'gray20', size = 2),
          axis.ticks.length = unit(.25, "cm"),
          plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 30)) # centered title
    )
  
  # define axis labels
  if (term == "Age"){
    x_label <- "Age (years)"
    p <- p +
      labs(x = toString(x_label)) +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
      
  } else if (term == "F3_Executive_Efficiency"){
    x_label <- "Executive function (z)"
    p <- p +
      labs(x = toString(x_label)) +
      theme(axis.title.y = element_blank())
  }
  
  return(p)
}

##########################################################################
#### compute derivatives and generate rate of change bars for all 14 networks ####
get_derivs_and_plot <- function(modobj, smooth_var, low_color = NULL, hi_color = NULL){
  nmf_network <- all.vars(modobj$formula)[1]
  
  # get model derivatives
  derv <- derivatives(modobj, term = smooth_var, interval = "simultaneous", unconditional = F)
  
  # add significance variable (true or false)
  derv <- derv %>%
    mutate(sig = !(0 > lower & 0 < upper)) #derivative is sig if the lower CI is not < 0 while the upper CI is > 0 (i.e., when the CI does not include 0)
  # new variable with only significant derivatives (non-sig. ones are set to 0)
  derv$sig_deriv = derv$derivative * derv$sig
  
  # print changes range if significant
  if (all(derv$sig_deriv == 0)){
    cat(sprintf("\n No significant change in %s \n", nmf_network))
  } else {
    cat(sprintf("\nSig change: %1.2f - %1.2f\n", min(derv$data[derv$sig == T]), max(derv$data[derv$sig == T])))
  }
  
  # plot change
  d <- ggplot(data = derv) + 
    geom_tile(aes(x = data, y = .5, fill = sig_deriv)) 
  
  # plot positive changes
  if (any(derv$sig_deriv > 0)){
    
    if (is.null(low_color)){low_color = "white"} # no change
    if (is.null(hi_color)){hi_color = "gray40"} # gray for very positive values
    d <- d + scale_fill_gradient(low = low_color, high = hi_color,limits = c(0, max(derv$derivative)))
    
    # plot negative changes (swap low and high colors)
  } else if (any(derv$sig_deriv < 0)){
    
    if (is.null(low_color)){low_color = "#D03C38"} # red for very negative values
    if (is.null(hi_color)){hi_color = "white"} # no change
    d <- d + scale_fill_gradient(low = low_color, high = hi_color,limits = c(min(derv$derivative), 0))
    
  } else if (all(derv$sig_deriv == 0)){ # no significant changes
    if (is.null(low_color)){low_color = "white"} # no change
    if (is.null(hi_color)){hi_color = "white"} # no change
    d <- d + scale_fill_gradient(low = low_color, high = hi_color,limits = c(min(derv$derivative), 0))
  }
  
  d <- d + 
    labs(x = "Age (years)", fill = "Change \nper year") + 
    theme(axis.title.y = element_blank(),
          # axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = this_font_size),
          axis.line = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = this_font_size),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 30),
          axis.title = element_text(size = this_font_size),
          legend.key.width = unit(1,"cm"),
          legend.position = "right",
          plot.margin = unit(c(0, 0, 0.5, 0), "cm"), #Top, left,Bottom, right
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'gray20', size = 1.5),
          axis.line.y = element_line(colour = 'gray20', size = 1.5),
          axis.ticks.length = unit(.25, "cm"),
          axis.text = element_text(size=50)) + 
    guides(fill = guide_colorbar(ticks = T, 
                                 ticks.linewidth = 2, 
                                 ticks.colour = "gray20", 
                                 draw.ulim = F,
                                 frame.colour = "gray20", 
                                 frame.linetype = 1, 
                                 frame.linewidth = 3,
                                 reverse = F, 
                                 direction = "vertical", 
                                 title.position = "top")) +
    geom_rect(aes(ymin = 0, ymax = 1, xmin = min(data), xmax = max(data)), color = "gray20", fill = "white", alpha = 0)
  return(d)
}


