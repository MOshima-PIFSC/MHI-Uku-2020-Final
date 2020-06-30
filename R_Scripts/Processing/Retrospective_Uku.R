# Retrospective analysis with Stock Synthesis 
# Example application 
# 2018 NOAA PIFSC Uku stock assessment
# Stock Synthesis (tested in version 3_30_05 for Windows) 
# r4ss (tested in version 1.34.0)  
# R (tested in version 3.5.3 64 bit)

library(r4ss)
#devtools::install_github('r4ss/r4ss')
#?r4ss

#################################################### Step 1
# Identify restrospective period
# e.g., for end.yr.vec   <- c(2018,2017,2016,2015,2014,2013)
start.retro <- 0    #end year of model e.g., 2018
end.retro   <- 5    #number of years for retrospective e.g., 2017,2016,2015,2014,2013

# Identify the base directory
dirname.base <- "C:\\Users\\michelle.sculley\\Documents\\Uku"
dirname.base

# Identify the directory where a completed model run is located
dirname.completed.model.run <- paste0(dirname.base,'\\021_Base')
dirname.completed.model.run

# Create a subdirectory for the Retrospectives
dirname.Retrospective <- paste0(dirname.base,'/Retrospective')
dir.create(path=dirname.Retrospective, showWarnings = TRUE, recursive = TRUE)
setwd(dirname.Retrospective)
getwd()

# Create a subdirectory for the Plots
dirname.plots <- paste0(dirname.Retrospective,"/plots_1")
dir.create(dirname.plots)

#################################################### Step 2
# Copy model files from the base run
file.copy(paste(dirname.completed.model.run,       "starter.ss_new", sep="/"),
          paste(dirname.Retrospective, "starter.ss", sep="/"))
file.copy(paste(dirname.completed.model.run,       "control.ss_new", sep="/"),
          paste(dirname.Retrospective, "Uku.ctl", sep="/"))
file.copy(paste(dirname.completed.model.run,       "data.ss_new", sep="/"),
          paste(dirname.Retrospective, "Uku.dat", sep="/"))	
file.copy(paste(dirname.completed.model.run,       "forecast.ss", sep="/"),
          paste(dirname.Retrospective, "forecast.ss", sep="/"))
file.copy(paste(dirname.completed.model.run,       "SS.exe", sep="/"),
          paste(dirname.Retrospective, "SS.exe", sep="/"))

#################################################### Step 3
starter <- readLines(paste(dirname.Retrospective, "/starter.ss", sep=""))

# 1) Starter File changes to speed up model runs
# Run Display Detail
#[8] "2 # run display detail (0,1,2)" 
linen <- grep("# run display detail", starter)
starter[linen] <- paste0( 1 , " # run display detail (0,1,2)" )

# 2) Starter File changes made to be consistent with projections									
linen <- grep("# Depletion basis:", starter)
starter[linen] <- paste0( 2 , " # Depletion basis:  denom is: 0=skip; 1=rel X*B0; 2=rel SPBmsy; 3=rel X*B_styr" )
linen <- grep("Depletion denominator", starter)
starter[linen] <- paste0( 1, " # Fraction (x) for Depletion denominator (e.g. 0.4)")
linen <- grep("# F_report_units:", starter)
starter[linen] <- paste0( 4 , " # F_report_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Frates); 4=true F for range of ages" )
linen <- grep("# F_report_basis:", starter)
starter[linen] <- paste0( 2, " # F_report_basis: 0=raw; 1=F/Fspr; 2=F/Fmsy ; 3=F/Fbtgt")


write(starter, paste(dirname.Retrospective, "starter.ss", sep="/"))

##################################################### Step 4
## Run the retrospective analyses with r4SS function "SS_doRetro"
SS_doRetro(masterdir=dirname.Retrospective, oldsubdir="", newsubdir="retrospectives", years=start.retro:-end.retro)

# Read "SS_doRetro" output
retroModels <- SSgetoutput(dirvec=file.path(dirname.Retrospective, "retrospectives",paste("retro",start.retro:-end.retro,sep="")))

# Summarize "SS_doRetro" output
retroSummary <- SSsummarize(retroModels)

# Set the ending year of each model in the set for plots
endyrvec <- retroSummary$endyrs + start.retro:-end.retro

plotdir <- dirname.plots
plotdir

SSplotComparisons(retroSummary, endyrvec=endyrvec, legendlabels=paste("Data",start.retro:-end.retro,"years"), png =TRUE, plotdir=plotdir)

for(index in unique(retroSummary$indices$Fleet)){
  SSplotComparisons(retroSummary, endyrvec=endyrvec, png=TRUE, indexUncertainty=TRUE,
                    plotdir=plotdir, 
                    subplot=11:12,indexfleets=index,
                    legendlabels=paste("Data",start.retro:-end.retro,"years"))
}

##################################################### Step 5
# make Squid Plot of recdev retrospectives 
setwd(plotdir)
getwd()

pdf('Retrospective_dev_plots.pdf',width = 5, height = 5)
par(mfrow=c(2,1))

# First scaled relative to most recent estimate
SSplotRetroRecruits(retroSummary, endyrvec=endyrvec, cohorts=1949:2018,
                     relative=TRUE, legend=FALSE, uncertainty=FALSE, main="", labels = c("Recruitment deviation", "", "", "Age"))

# Second without scaling
SSplotRetroRecruits(retroSummary, endyrvec=endyrvec, cohorts=1949:2018,
                 relative=FALSE, legend=FALSE,uncertainty=FALSE, main="", labels = c("Recruitment deviation", "", "", "Age"))
dev.off()

###separate png plots
png('Retrospective_dev_plots_scaled.png',width = 400, height = 300)
SSplotRetroRecruits(retroSummary, endyrvec=endyrvec, cohorts=1949:2018,
                    relative=TRUE, legend=FALSE, uncertainty=FALSE, main="", labels = c("Recruitment deviation", "", "", "Age"))
dev.off()

png('Retrospective_dev_plots_unscaled.png',width = 400, height = 300)
SSplotRetroRecruits(retroSummary, endyrvec=endyrvec, cohorts=1949:2018,
                    relative=FALSE, legend=FALSE,uncertainty=FALSE, main="", labels = c("Recruitment deviation", "", "", "Age"))
dev.off()

##################################################### Step 6
# Calculate Mohn's Rho values for select yrvecquantities
SSmohnsrho.out <- SSmohnsrho(retroSummary, endyrvec = endyrvec, startyr = 2017,
           verbose = FALSE)

write.table(SSmohnsrho.out, file="00_SSmohnsrho.txt", row.names=F)

### Key reference: Hurtado-Ferro et al., 2014. Looking in the rear-view mirror: bias and retrospective patterns in integrated, age-structured stock assessment models. ICES Journal of Marine Science.
### Given that the variability of Mohn's rho depends on life history, and that the statistic appears insensitive to F, 
### we propose the following rule of thumb when determining whether a retrospective pattern should be addressed explicitly: 
### values of Mohn's rho higher than 0.20 or lower than -0.15 for longer-lived species (upper and lower bounds of the 90% simulation intervals for the flatfish base case),
### or higher than 0.30 or lower than -0.22 for shorter-lived species (upper and lower bounds of the 90% simulation intervals for the sardine base case) 
### should be cause for concern and taken as indicators of retrospective patterns. 
### However, Mohn's rho values smaller than those proposed should not be taken as confirmation that a given assessment does not present a retrospective pattern, 
### and the choice of 90% means that a "false positive" will arise 10% of the time. 
### In both cases, model misspecification would be correctly detected more than half the time.

# Save image for later analysis
file.name<-paste('retro',format(Sys.time(), "%Y%m%d_%H%M"))
save.image(paste0(plotdir, "/",file.name, ".RData"))

##################################################### Step 7
#Load image for later analysis without rerunning retrospective
#load(
#  paste0("C:\\Users\\felip\\Documents\\Retrospective\\plots_1\\",
#         "retro 20190820_1314.RData")
#)

##################################################### OPTIONAL
# Source a seperate script to obtain likelihoods for each retrospective run 
# This is the script from R0 profile adapted for Retropsective
# Produces a table of likelihoods by fleet for each retrospective run
#
#source(paste0(dirname.base,"/Likelihood/","02_summaryplot_likelihood.r"))

##################################################### OPTIONAL
# Mohns rho function
# Produces plots in seperate directory 
#source(paste0(dirname.base,"/rho/","03_Mohns_rho.r"))



