
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# ss3runsFun.R
# Residual diagnostic plots with Runs Tests for Stock Sythesis
# Designed for r4ss objects of report.sso files
# Developed by Henning Winker and Felipe Carvalho 2019
# --------------------------------------------------------------
# Functions
#---------------------------------------------------------------
# plot_ss3runs() # Novel run test plot visualization 
# get_ss3compsTA1.8() # Creat observed and predicted mean lengths
# get_runslibs() loads required packages
# plot_ss3runsrec() run test plot for recruitment residual
# jabba_resids_plot() overlays residuals from multiple time series 
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

# Set path for "ss3runsFun.R" file
ss3runsFun = "C:\\Users\\michelle.sculley\\Documents\\Uku"
# source ss3runsFun.R
source(paste0(ss3runsFun,"\\ss3runsFun.R"))

# Load packages
get_runslibs()

#------------------------------
# Uku Main Hawaiian Islands
#------------------------------
# Set Working Directory
setwd("C:\\Users\\michelle.sculley\\Documents\\Uku")
# Model name (folder with report.sso)
ssmod = "021_Base"
# Load Stock Synthesis report file
ss3rep=SS_output(dir=ssmod,covar=T,ncol=250)

# Runs plot for CPUE
plot_ss3runs(ss3rep$cpue,plotname = paste0(ssmod,"_cpue"))

# Create obs vs predicted mean length
lendat =get_ss3compsTA1.8(ss3rep,type="size") # here size (default is length)

# Runs plot for mean lengths
plot_ss3runs(lendat$runs_dat,plotname = paste0(ssmod,"_meanL"))

# Recruitment plot
plot_ss3runsrec(ss3rep$recruit,plotname = paste0(ssmod,"_RecDev"))

# JABBA residual plot CPUE
jabba_resids_plot(ss3rep$cpue,plotname = paste0(ssmod,"_JBcpue"))

# JABBA residual plot mean length
jabba_resids_plot(lendat$runs_dat,plotname = paste0(ssmod,"_JBmeanL"))



