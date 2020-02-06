library(r4ss);library(ggplot2);library(reshape2);library(scales);library(RColorBrewer);library(gridExtra);library(dplyr);library(tidyr);library("png"); library("grid");library("gridExtra")


##SensitivityRuns

## update as needed. Are nicer than the default SS output
#----------------

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\01_Base"
base_case <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\02a_NatMort009"
model_1 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\02b_NatMort011"
model_2 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\03a_Linf689"
model_3 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\03b_Linf842"
model_4 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\04a_Lmat403"
model_5 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\04b_Lmat493"
model_6 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\05a_SigR035"
model_7 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\05b_SigR043"
model_8 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\06a_Steep073"
model_9 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\06b_Steep089"
model_10 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\07a_RecCatchRatios"
model_11 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\07b_RecCatchPhoneCorrected"
model_12 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <-"C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\07c_RecCatchNeg30"
model_13 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\07d_RecCatchPos30"
model_14 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\08_OrigCPUECVs"
model_15 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\09a_CPUE_DSH_DSH"
model_16 <- SS_output(dir,ncols = 500,covar=FALSE)

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\09b_CPUE_DSH_ISH"
model_17 <- SS_output(dir,ncols = 500,covar=FALSE)            

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\09c_CPUE_DSH_TROL"
model_18 <- SS_output(dir,ncols = 500,covar=FALSE)   

dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\10_SizeFreqLambda01"
model_19 <- SS_output(dir,ncols = 500,covar=FALSE)   


model_dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final"
source("C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\Scripts\\Processing\\Scenarios_KobePlot.R")

NatM_sens <-SSsummarize(list(base_case,model_1,model_2))
SSplotComparisons(NatM_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\01_NatM"), legendlabels=c("Base","M=0.09","M=0.11"), col="black", subplots = c(1,7,18), shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_1,model_2),Label.Vect=c("Base","M=0.09","M=0.11"),plotdir=paste0(model_dir,"\\Plots\\01_NatM"))

Growth_sens <-SSsummarize(list(base_case,model_3,model_4))
SSplotComparisons(Growth_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\02_Growth"), legendlabels=c("Base","Linf=69cm","Linf=84cm"), col="black", subplots = c(1,7,18),shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_3,model_4),Label.Vect=c("Base","Linf=69cm","Linf=84cm"),plotdir=paste0(model_dir,"\\Plots\\02_Growth"))

Mat_sens<-SSsummarize(list(base_case,model_5,model_6))
SSplotComparisons(Mat_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\03_Mat"), legendlabels=c("Base","Lmat=40cm", "Lmat=49cm"), col="black", subplots = c(1,7,18),shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_5,model_6),Label.Vect=c("Base","Lmat=40cm", "Lmat=49cm"),plotdir=paste0(model_dir,"\\Plots\\03_Mat"))

SigR_sens<-SSsummarize(list(base_case,model_7,model_8))
SSplotComparisons(SigR_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\04_SigR"), legendlabels=c("Base","SigmaR=0.35","SigmaR=0.43"), col="black", subplots = c(1,7,18),shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_7,model_8),Label.Vect=c("Base","SigmaR=0.35","SigmaR=0.43"),plotdir=paste0(model_dir,"\\Plots\\04_SigR"))

h_sens<-SSsummarize(list(base_case,model_9,model_10))
SSplotComparisons(h_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\05_Steep"), legendlabels=c("Base","Steepness=0.73","Steepness=0.89"), col="black", subplots = c(1,7,18),shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_9,model_10),Label.Vect=c("base","h=0.73","h=0.89"),plotdir=paste0(model_dir,"\\Plots\\05_Steep"))

RecCatch_sens<-SSsummarize(list(base_case,model_11,model_12,model_13, model_14))
SSplotComparisons(RecCatch_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\06_RecCatch"), legendlabels=c("Base","Ratios","Corrected","Minus 30%", "Plus 30%"), col="black", subplots = c(1,7,18),shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_11,model_12,model_13, model_14),Label.Vect=c("Base","Ratios","Corrected","Minus 30%", "Plus 30%"),plotdir=paste0(model_dir,"\\Plots\\06_RecCatch"))

Index_sens<-SSsummarize(list(base_case,model_15,model_16,model_17, model_18))
SSplotComparisons(Index_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\07_Index"), legendlabels=c("Base","Orig. CPUE CV","DSH-DSH","DSH-ISH", "DSH-TROL"), col="black", subplots = c(1,7,18),shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_15,model_16,model_17, model_18),Label.Vect=c("Base","Orig. CPUE CV","DSH-DSH","DSH-ISH", "DSH-TROL"),plotdir=paste0(model_dir,"\\Plots\\07_Index"))

Lambda_sens<-SSsummarize(list(base_case, model_19))
SSplotComparisons(Lambda_sens,png=TRUE, plotdir = paste0(model_dir,"\\Plots\\08_SizeFreqLambda"), legendlabels=c("Base","SizeFreq Lambda=0.1"), col="black", subplots = c(1,7,18),shadealpha = 0)
Get_KobeScenarios(Model.List=list(base_case,model_19),Label.Vect=c("Base","SizeFreq Lambda=0.1"),plotdir=paste0(model_dir,"\\Plots\\08_SizeFreqLambda"))


# Put all four graphs in a grid and save
Groups.Name <- c("01_NatM","02_Growth","03_Mat","04_SigR","05_Steep","06_RecCatch","07_Index","08_SizeFreqLambda")
img.list <- list()
for(i in 1:length(Groups.Name)){
img.list[[1]] <- readPNG(paste0(model_dir,"\\Plots\\",Groups.Name[i],"\\compare1_spawnbio.png"))
img.list[[2]] <- readPNG(paste0(model_dir,"\\Plots\\",Groups.Name[i],"\\compare7_recruits.png"))
img.list[[3]] <- readPNG(paste0(model_dir,"\\Plots\\",Groups.Name[i],"\\compare18_Fvalue_uncertainty.png"))
img.list[[4]] <- readPNG(paste0(model_dir,"\\Plots\\",Groups.Name[i],"\\KobePlot.png"))

png(paste0(model_dir,"\\Plots\\",Groups.Name[i],"\\Final.png"),height=6,width = 7, units="in",res=300) 
grid.arrange(rasterGrob(img.list[[1]]),rasterGrob(img.list[[2]]),rasterGrob(img.list[[3]]),rasterGrob(img.list[[4]]),ncol=2)
dev.off()
}










