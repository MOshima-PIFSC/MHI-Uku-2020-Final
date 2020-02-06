##
# Jitter Test for Global Convergence with Stock Synthesis 
# Stock Synthesis (tested in version 3_30_X for Windows) 
# r4ss (tested in version(s) 1.35.1 - 1.35.3)  
# R (tested in version(s) 3.3.2 - 3.5.2 64 bit Windows)
##
library(r4ss)
#devtools::install_github("r4ss/r4ss", ref="development")
#?r4ss

# Define the root directory for the run
dirname.root <- "C:\\Users\\felip\\Documents\\Uku\\Jitter_Diag\\Uku"
dirname.root

# Define the directory where a completed "base" model run is located
dirname.base <- paste0(dirname.root,'/Base')
dirname.base

# Create a subdirectory for the jitter run
dirname.jitter <- paste0(dirname.root,'/jitter')
dirname.jitter
dir.create(path=dirname.jitter, showWarnings = TRUE, recursive = TRUE)

# Create a subdirectory for the output
dirname.plots <- paste0(dirname.root,"/plots")
dirname.plots
dir.create(dirname.plots)

#----------------- copy base model files to jitter directory (DC) ----------------------------------------
# Option 1
file.copy(paste(dirname.base,       "starter.ss", sep="/"),
          paste(dirname.jitter,     "starter.ss", sep="/"))
file.copy(paste(dirname.base,       "Uku.ctl", sep="/"),
          paste(dirname.jitter,     "Uku.ctl", sep="/"))
file.copy(paste(dirname.base,       "Uku.dat", sep="/"),
          paste(dirname.jitter,     "Uku.dat", sep="/"))	
file.copy(paste(dirname.base,       "forecast.ss", sep="/"),
          paste(dirname.jitter,     "forecast.ss", sep="/"))
file.copy(paste(dirname.base,       "ss.exe", sep="/"),
          paste(dirname.jitter,     "ss.exe", sep="/"))
file.copy(paste(dirname.base,       "ss.par", sep="/"),
          paste(dirname.jitter,     "ss.par", sep="/"))


#Make Changes to the Starter.ss file (r4ss example)
starter <- SS_readstarter(file.path(dirname.jitter, 'starter.ss'))

# Change to use .par file
starter$init_values_src = 1

# Change jitter (0.1 is an arbitrary, but common choice for jitter amount)
starter$jitter_fraction = 0.1

# write modified starter file
SS_writestarter(starter, dir=dirname.jitter, overwrite=TRUE)


#------------ Run Jitter Test for Global Convergence with Stock Synthesis (MS MC) -------------------------------
#Set the number of iteration  
Njitter=200
#Njitter=1 #test

#### Run jitter using this function (deafult is nohess)
jit.likes <- SS_RunJitter(mydir=dirname.jitter, Njitter=Njitter, extras="")

setwd(dirname.plots)
getwd()


#Total likelihoods necessary to assess global convergence are saved to "jit.likes"
x<-as.numeric(jit.likes)
global.convergence.check<-table(x,exclude = NULL)
write.table(jit.likes,"jit_like.csv")
write.table(global.convergence.check,"global_convergence_check.csv")


#------------ Summarize more Jitter Test Results -------------------------------
#Save image for later analysis

wd <- dirname.jitter

jitter=seq(1:Njitter)
n=length(jitter)
n
witch_j <- SSgetoutput(keyvec=1:n, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary <- SSsummarize(witch_j)

#Likelihood across runs
likes=witch_j_summary$likelihoods

#Derived quants across runs
quants=witch_j_summary$quants

#Estimated parameters across runs
pars=witch_j_summary$pars

#Write more output tables to jitter directory
write.table(quants,"Quants.csv")
write.table(pars,"Pars.csv")
write.table(likes,"Likelihoods.csv")

#Retabulate total likelihoods necessary to assess global convergence and compare to jit.likes from above 
x<-as.numeric(likes[likes$Label=="TOTAL",1:n])
global.convergence<-table(x,exclude = NULL)
write.table(global.convergence,"global_convergence.csv")


#------------ Make plots with r4ss for runs ending at a converged solution -------------------------------
#Base case read in manually
Base <- SS_output(dir=dirname.base,covar=T,forecast=T)

#make some plots#make some plots
plotdir <- dirname.plots
setwd(plotdir)
getwd()

png("Jittering results.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(1:Njitter), witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter],ylab="LL",
     ylim=c(0,max(na.omit(jit.likes))*1.05)) ; mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F, plotdir = plotdir, ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()

png("jit likes.png", width = 480, height = 480)
par(mfrow=c(1,1), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(1:Njitter), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter model runs at a converged solution"
)
#mtext(side=3, line=0, "Jittering")     
abline(h=Base$likelihoods_used[1,1], col=2)
dev.off()


# Repeat for all converged runs 
x<-which(!is.na(witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter]))

jitter.converged=x
jitter.converged
n.converged=length(jitter.converged)
n.converged
witch_j.converged <- SSgetoutput(keyvec=jitter.converged, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary.converged <- SSsummarize(witch_j.converged)

png("Jittering results at converged solution.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(jitter.converged), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL", jitter.converged],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter runs at a converged solution"
)
mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary.converged,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F, plotdir = plotdir, ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()


#Repeat for converged runs at the minimum solution 
#Converged runs at min converged solution (should be same as base case to pass the test) 
#min(na.omit(jit.likes))
y<-which(witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter]==min(na.omit(jit.likes)))


jitter.min=y
jitter.min
n.min=length(jitter.min)
n.min
witch_j.min <- SSgetoutput(keyvec=jitter.min, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary.min <- SSsummarize(witch_j.min)


png("Jittering results at min converged solution.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(jitter.min), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL", jitter.min],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter runs at the minimum converged solution"
     )
mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary.min,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F, plotdir = plotdir, ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()

#Save image of all run data for later analysis
#file.name<-paste('jitter',format(Sys.time(), "%Y%m%d_%H%M"))
#save.image(paste0(dirname.plots, "/",file.name, ".RData"))


