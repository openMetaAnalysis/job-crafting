# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author: Robert Badgett. rbadgett@kumc.edu 
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-04-21
#This file is best used within R Studio and using the Studio's Document outline for navigation 

### Start ======================================= 
version 
citation(package = "base", lib.loc = NULL, auto = NULL) 

# If Rstudio 
if (Sys.getenv("RSTUDIO") == "1"){ 
  setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
  #ScriptsDir <- paste(getwd(),'/Scripts',sep='') 
}else{ 
  #setwd("P:/MPA CHC/Panel Management/") 
  #ScriptsDir <- paste(getwd(),'Scripts',sep='') 
} 

##* Constants--------- 
current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)) 

##* Functions ----- 
`%notin%` <- Negate(`%in%`) 
`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2)) 
`%==na%` <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2))) 

function_libraries_install <- function(packages){ 
  install.packages(setdiff(packages, rownames(installed.packages()))) 
  for(package_name in packages) 
  { 
    #library(package_name,character.only=TRUE,quietly=TRUE); 
    library(package_name,character.only=TRUE, quietly = FALSE); 
  } 
} 

function_plot_print <- function (plotname, plotheight, plotwidth){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  (current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}

##* Packages/libraries, essential ----- 
packages <- c('stringr','tcltk','openxlsx','dplyr') 
function_libraries_install(packages) 

##* Packages/libraries, other ----- 
packages_meta <- c("metafor", # 
                   'meta',   # Meta-analysis 
                   'boot',   # inv.logit to identify deviants 
                   'grid',   # Forest and blobbogram 
                   'gemtc',   # Blobbogram 
                   'esc'   # Campbell Collaboration 
) 

function_libraries_install(packages_meta) 

#==Create effect sizes with Campbell calculator------ 
# https://www.campbellcollaboration.org/research-resources/effect-size-calculator.html 

# Kooij has the data, but is not in their publication

# Data creation -----
Gordon1 <- esc_mean_gain( 
  # CONTROL
  pre1mean = 4.54,
  pre1sd = 0.85, 
  post1mean = 4.13, 
  post1sd = 0.44, 
  grp1n = 71, 
  # gain1mean, 
  # gain1sd, 
  # grp1r, 
  # EXPERIMENTAL
  pre2mean = 4.10, 
  pre2sd = 0.40, 
  post2mean = 4.27, 
  post2sd = 0.32, 
  grp2n = 48, 
  # gain2mean, 
  # gain2sd, 
  # grp2r, 
  # r, 
  es.type = "d", 
  study = "Gordon(1), 2018" 
) 

Gordon2 <- esc_mean_gain( 
  # CONTROL
  pre1mean = 4.00, 
  pre1sd = 0.62, 
  post1mean = 4.07, 
  post1sd = 0.67, 
  grp1n = 26, 
  # gain1mean, 
  # gain1sd, 
  # grp1r, 
  # EXPERIMENTAL
  pre2mean = 4.10, 
  pre2sd = 0.74, 
  post2mean = 4.68, # 120 minutes 
  post2sd = 0.74, 
  grp2n = 32, 
  # gain2mean, 
  # gain2sd, 
  # grp2r, 
  # r, 
  es.type = "d", 
  study = "Gordon(2), 2018" 
) 

van_Wingerden <- esc_mean_gain( 
  # CONTROL
  pre1mean = 4.20, 
  pre1sd = 0.38, 
  post1mean = 4.10, 
  post1sd = 0.43, 
  grp1n = 24, 
  # gain1mean, 
  # gain1sd, 
  # grp1r, 
  # EXPERIMENTAL
  pre2mean = 4.21, 
  pre2sd = 0.42, 
  post2mean = 4.28, 
  post2sd = 0.43, 
  grp2n = 43, 
  # gain2mean, 
  # gain2sd, 
  # grp2r, 
  # r, 
  es.type = "d", 
  study = "van Wingerden, 2016" 
) 

esc_mean_se(grp1m = 7, grp1se = 1.5, grp1n = 50,
            grp2m = 9, grp2se = 1.8, grp2n = 60, es.type = "or")

Engel <- esc_t( 
  p = 0.86, 
  totaln = 64, 
  study = "Engel, 2000" 
) 

data.studies <- combine_esc(Gordon1, Gordon2, van_Wingerden)  

data.studies$Year <- str_sub(data.studies$study, start = nchar(data.studies$study) - 4, end = nchar(data.studies$study)) 

## Meta-analysis from library meta ---- 

analyticmethod = "Random effects model" 
hartung = FALSE 
data.meta <- metagen(es, se, data = data.studies, sm="MD", subgroup=NULL, fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=data.studies$Study) 
data.meta <- metagen(es, se, data = data.studies, sm="SMD", subgroup=NULL, fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=data.studies$Study) 

#data.meta$data <- inner_join(x = data.studies, y = data.meta$data , by = "study") 

# https://handbook-5-1.cochrane.org/chapter_9/9_2_3_2_the_standardized_mean_difference.htm 

data.meta$TE.random 
data.meta$seTE.random 
sample.size <- sum(data.studies$sample.size) 
standard.deviation <- data.meta$seTE.random * sqrt(sample.size) 
(Mean <- data.meta$TE.random * standard.deviation) 

##** Forest plot------------------------ 

PlotName <- 'Task performance increase with\njob crafting vs control' 

# WARNING: after backtransf for display in forest plots, note that point estimates and I.I.s do not exactly match the data inputted in the table 'data' above 

par(mar=c(5.2,4.1,4.1,2.1), mfrow=c(1,1)) # (bottom, left, top, right)

meta::forest(data.meta, bylab =NULL, #sortvar = Year, 
             byseparator=NULL , bysort = FALSE, resid.hetlab = "Residual I2: ", 
             print.p=FALSE, text.random=NULL, text.random.w=NULL, method="", # analyticmethod 
             label.left = "Favors control",
             label.right = "Favors job crafting", 
             # xlim=c(0.2,5),  
             digits.addcols.left = 0,
             digits.n = 0,
             xlab= '', # "Standardized mean difference (d)",  
             smlab = "Standardized\nmean difference\u2020", 
             leftcols=c("study","sample.size"), leftlabs=c("Study", "Size"),  
             #leftcols=c("study"), leftlabs=c("Study"),  
             fixed = FALSE, 
             digits.addcols = 0, just.addcols.left = "right", colgap.left = "5mm", print.tau2=FALSE,col.diamond="blue", col.diamond.lines="blue", print.I2.ci=TRUE,overall=TRUE,test.subgroup=FALSE, test.subgroup.random=TRUE, print.Q.subgroup=FALSE) 

grid.text(PlotName, 0.5, 0.9, gp = gpar(fontsize = 14, fontface = "bold")) 

#Footer 
grid.text('Notes:', 0.08, 0.18, hjust=0, gp=gpar(cex=1, font=2)) 
Footer <- paste("\u2020 Standardized mean difference is the reported mean difference divided by the pooled standard deviation." , sep="\n")
Footer <- paste(Footer, "SMD = 0.2 is small; SMD = 0.5 is medium, SMD = 0.8 is large.", sep="\n") 
grid.text(Footer, 0.08, 0.09, hjust=0, gp=gpar(cex=1, font=1)) 

##** Export plot------------------------ 

function_plot_print (PlotName, 400, 800) 

