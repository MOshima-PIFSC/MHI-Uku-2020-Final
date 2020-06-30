library(r4ss);library(ggplot2);library(reshape2);library(scales);library(RColorBrewer);library(gridExtra);library(dplyr);library(tidyr);library("png"); library("grid");library("gridExtra")
library(ggsci);library(data.table);library(stringr)

##SensitivityRuns

## update as needed. Are nicer than the default SS output
#----------------
#base.dir<-c("C:\\Users\\michelle.sculley\\Documents\\Uku\\Assessment\\01_SS final")
base.dir<-c("C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\01_SS final\\")
#source("C:\\Users\\michelle.sculley\\Documents\\Uku\\Assessment\\Scripts\\Processing\\Scenarios_KobePlot.R")
source("C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\Scripts\\Processing\\Scenarios_KobePlot.R")


dir <- paste0(base.dir,"\\01_Base")
base_case <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\02a_NatMort009")
model_1 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\02b_NatMort011")
model_2 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\03a_Linf689")
model_3 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\03b_Linf842")
model_4 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\04a_Lmat403")
model_5 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\04b_Lmat493")
model_6 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\05a_SigR035")
model_7 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\05b_SigR043")
model_8 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\06a_Steep073")
model_9 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\06b_Steep089")
model_10 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\07a_RecCatchRatios")
model_11 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\07b_RecCatchPhoneCorrected")
model_12 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <-paste0(base.dir,"\\07c_RecCatchNeg30")
model_13 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\07d_RecCatchPos30")
model_14 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\08_OrigCPUECVs")
model_15 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\09a_CPUE_DSH_DSH")
model_16 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\09b_CPUE_DSH_ISH")
model_17 <- SS_output(dir,ncols = 500,covar=TRUE)            

dir <- paste0(base.dir,"\\09c_CPUE_DSH_TROL")
model_18 <- SS_output(dir,ncols = 500,covar=TRUE)   

dir <- paste0(base.dir,"\\10_SizeFreqLambda01")
model_19 <- SS_output(dir,ncols = 500,covar=TRUE)  

dir <- paste0(base.dir,"\\12_ISH1992")
model_21 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\13_MLorenz")
model_22 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\14_DSHsingleQ")
model_23 <- SS_output(dir,ncols = 500,covar=TRUE)

dir <- paste0(base.dir,"\\15_ISH1992with2Qs")
model_24 <- SS_output(dir,ncols = 500,covar=TRUE)            

dir <- paste0(base.dir,"\\16_LongDSH_ISH_Lorenz")
model_25 <- SS_output(dir,ncols = 500,covar=TRUE)   

dir <- paste0(base.dir,"\\17_1970Start")
model_26 <- SS_output(dir,ncols = 500,covar=TRUE) 

dir <- paste0(base.dir,"\\18_Estimate Sigma R")
model_27 <- SS_output(dir,ncols = 500,covar=TRUE) 

dir <- paste0(base.dir,"\\19_TimeVaryQ")
model_28 <- SS_output(dir,ncols = 500,covar=TRUE) 

dir <- paste0(base.dir,"\\20_EFFNfromSS")
model_29 <- SS_output(dir,ncols = 500,covar=TRUE) 

dir <- paste0(base.dir,"\\21_ExcludeDivers")
model_30 <- SS_output(dir,ncols = 500,covar=TRUE) 






plotsensitivity<-function(Summary, ModelLabels, NModels, PlotDir ){
  
  #Summary <-SSsummarize(list(base_case,model_1,model_2, model_22))
  #ModelLabels<-c("Base","M=0.09","M=0.11","Lorenzen M")
  #Directory<-paste0(base.dir,"\\Plots\\01_NatM")
  #dir.create(Directory)
  #NModels<-length(ModelLabels)
  
  
  ModelLabels<-paste0(letters[seq(from=1,to=length(ModelLabels))],") ",ModelLabels)
  
  
  aTheme <- theme(panel.border = element_rect(color="black",fill=NA,size=1),
                  panel.background = element_blank(), strip.background = element_blank(),
                  legend.title=element_blank(),legend.key=element_blank(),
                  legend.position="none")
  
  
  SummaryBio<-Summary$SpawnBio
  names(SummaryBio)<-c(ModelLabels,"Label","Yr")
  SSBMSY<-Summary$quants[which(Summary$quants$Label=="SSB_MSY"),]
  SSBMSST<-0.9*SSBMSY[,1:NModels]
  SummaryBratio<-as.data.frame(matrix(NA,ncol=NModels, nrow=nrow(SummaryBio)))
  for(i in 1:NModels){
    for (j in 1:nrow(SummaryBio)){
      SummaryBratio[j,i]<-SummaryBio[j,i]/SSBMSST[i]
    }}
  SSBMSST<-melt(SSBMSST)
  SummaryBratio[,c(NModels+1,NModels+2)]<-SummaryBio[,c(NModels+1,NModels+2)]
  SummaryBio<-melt(SummaryBio,id.vars=c("Label","Yr"))
  SummaryBio<-subset(SummaryBio,Yr>=1948)
  names(SummaryBratio)<-c(ModelLabels,"Label","Yr")
  SummaryBratio<-melt(SummaryBratio,id.vars=c("Label","Yr"))
  SummaryBratio<-subset(SummaryBratio,Yr>=1948)
  SpawnBioUpper<-Summary$SpawnBioUpper
  SpawnBioUpper<-melt(SpawnBioUpper, id.vars=c("Yr", "Label") )
  names(SpawnBioUpper)[3:4]<-c("Model","Upper")
  SpawnBioLower<-Summary$SpawnBioLower 
  SpawnBioLower<-melt(SpawnBioLower, id.vars=c("Yr", "Label") )
  names(SpawnBioLower)[3:4]<-c("Model","Lower")
  SpawnBioUncertainty<-merge(SpawnBioUpper,SpawnBioLower,by=c("Yr","Label","Model"))  

  Model.names          <- data.frame(Model= unique(SpawnBioUncertainty$Model) ) 
  Model.names$variable <- ModelLabels
  
  SpawnBioUncertainty <- SpawnBioUncertainty[-c(1:6),]
  SpawnBioUncertainty <- merge(SpawnBioUncertainty,Model.names,by="Model")
  colnames(SSBMSST)   <- c("Model","value")
  SSBMSST             <- merge(SSBMSST,Model.names,by="Model")
  
  SummaryBio          <- data.table(SummaryBio)
  SpawnBioUncertainty <- data.table(SpawnBioUncertainty)
  
  shapes <- c(16,17,18,4,3)
 
  if(ModelLabels[2]=="b) Start in 1970"){  
    SummaryBio          <- SummaryBio[!(variable=="b) Start in 1970"&Yr<1970)]  
    SpawnBioUncertainty <- SpawnBioUncertainty[!(variable=="b) Start in 1970"&Yr<1970)]
    }
  
thinned <- c(seq(from=1,to=nrow(SummaryBio),by=10))
a<-ggplot(data=SummaryBio,aes(x=Yr,color=variable,shape=variable))+
  geom_ribbon(aes(ymin=Lower,ymax=Upper),color=NA,data=SpawnBioUncertainty,fill="gray",alpha=0.2)+
  geom_line(aes(y=value),size=1.5) +
  geom_point(aes(y=value),data=SummaryBio[thinned,], size=4)+
  xlab("Year") + ylab("Spawning biomass (mt)") +
  geom_hline(aes(yintercept=value,color=variable),data=SSBMSST,size=1.5)+
  scale_shape_manual(values=shapes)+
  scale_color_jco()+
  theme_bw(base_size=20)+aTheme+theme(legend.position="none")
a

  FishingMort<-Summary$Fvalue
  names(FishingMort)<-c(ModelLabels,"Label","Yr")
  FishingMort<-melt(FishingMort,id.vars=c("Label","Yr"))
  FishingMort<-subset(FishingMort,Yr>=1948)
  FMSY<-Summary$quants[which(Summary$quants$Label=="Fstd_MSY"),1:NModels]
  FMSY<-melt(FMSY)
  FUpper<-Summary$FvalueUpper
  FUpper<-melt(FUpper, id.vars=c("Yr", "Label") )
  names(FUpper)[3:4]<-c("Model","FUpper")
  FLower<-Summary$FvalueLower 
  FLower<-melt(FLower, id.vars=c("Yr", "Label") )
  names(FLower)[3:4]<-c("Model","FLower")
  Funcertainty<-merge(FUpper,FLower,by=c("Yr","Label","Model"))  
  
  Model.names          <- data.frame(Model=as.character( unique(Funcertainty$Model) ) )
  Model.names$variable <- ModelLabels
  
  Funcertainty <- merge(Funcertainty,Model.names,by="Model")
  colnames(FMSY)   <- c("Model","value")
  FMSY             <- merge(FMSY,Model.names,by="Model")
  
  FishingMort  <- data.table(FishingMort)
  Funcertainty <- data.table(Funcertainty)
  
  if(ModelLabels[2]=="b) Start in 1970"){  
    FishingMort  <- FishingMort[!(variable=="b) Start in 1970"&Yr<1970)]  
    Funcertainty <- Funcertainty[!(variable=="b) Start in 1970"&Yr<1970)]
  }
  

thinned <- c(seq(from=1,to=nrow(FishingMort),by=10))
b<-ggplot(data=FishingMort,aes(x=Yr,color=variable,shape=variable))+
  geom_ribbon(aes(ymin=FLower,ymax=FUpper),color=NA,data=Funcertainty,fill="gray",alpha=0.2)+
  geom_line(aes(y=value),size=1.5) +
  geom_point(aes(y=value),data=FishingMort[thinned,], size=4)+
  xlab("Year") + ylab("Fishing mortality") +
  geom_hline(aes(yintercept=value,color=variable),data=FMSY,size=1.5)+
  scale_color_jco()+
  scale_shape_manual(values=shapes)+
  theme_bw(base_size=20)+aTheme+theme(legend.position="none")
b



  Recruits<-Summary$recruits
  names(Recruits)<-c(ModelLabels,"Label","Yr")
  Recruits<-melt(Recruits,id.vars=c("Label","Yr"))
  Recruits<-subset(Recruits,Yr>=1948)
  
  Recruits <- data.table(Recruits)
  Recruits <- Recruits[!(variable=="b) Start in 1970"&Yr<1970)]

thinned <- c(seq(from=1,to=nrow(Recruits),by=10))
c<-ggplot(data=Recruits,aes(x=Yr,y=value,color=variable,shape=variable)) +
  geom_line(size=1.5) +
  geom_point(data=Recruits[thinned,],size=4)+
  xlab("Year") + ylab("Age-0 recruits (1000s of fish)") +
  scale_linetype_manual(values=c(1:NModels),labels=c(ModelLabels))+
  scale_color_jco()+
  scale_shape_manual(values=shapes)+
  theme_bw(base_size=20)+aTheme
c

d<-Get_KobeScenarios(Model.List=Summary,Label.Vect=ModelLabels,plotdir=Directory,NModels)+theme(legend.position="right")
d

   png(paste0(PlotDir,"\\Sensitivity.png"),height=10,width=16,units="in",res=200)
    grid.arrange(a,c,b,d, nrow=2)
  dev.off()


}



NatM_sens <-SSsummarize(list(base_case,model_1,model_2, model_22))
ModelLabels<-c("Base","M=0.09","M=0.11","Lorenzen M")
Directory<-paste0(base.dir,"\\Plots\\01_NatM")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(NatM_sens,ModelLabels,NModels,Directory)



Growth_sens <-SSsummarize(list(base_case,model_3,model_4))
ModelLabels<-c("Base","Linf=69cm","Linf=84cm")
Directory<-paste0(base.dir,"\\Plots\\02_Growth")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(Growth_sens,ModelLabels,NModels,Directory)


Mat_sens<-SSsummarize(list(base_case,model_5,model_6))
ModelLabels<-c("Base","Lmat=40cm","Lmat=49cm")
Directory<-paste0(base.dir,"\\Plots\\03_Mat")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(Mat_sens,ModelLabels,NModels,Directory)


SigR_sens<-SSsummarize(list(base_case,model_7,model_8,model_27))
ModelLabels<-c("Base","SigmaR=0.35","SigmaR=0.43","Estimate SigmaR")
Directory<-paste0(base.dir,"\\Plots\\04_SigR")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(SigR_sens,ModelLabels,NModels,Directory)


h_sens<-SSsummarize(list(base_case,model_9,model_10))
ModelLabels<-c("Base","Steepness=0.73","Steepness=0.89")
Directory<-paste0(base.dir,"\\Plots\\05_Steep")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(h_sens,ModelLabels,NModels,Directory)



RecCatch_sens<-SSsummarize(list(base_case,model_11,model_12,model_13, model_14))
ModelLabels<-c("Base","Ratios","Corrected","Minus 30%", "Plus 30%")
Directory<-paste0(base.dir,"\\Plots\\06_RecCatch")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(RecCatch_sens,ModelLabels,NModels,Directory)


Index_sens<-SSsummarize(list(base_case,model_15,model_16,model_17, model_18))
ModelLabels<-c("Base","Orig. CPUE CV","DSH-DSH","DSH-ISH", "DSH-TROL")
Directory<-paste0(base.dir,"\\Plots\\07_Index")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(Index_sens,ModelLabels,NModels,Directory)


Lambda_sens<-SSsummarize(list(base_case, model_19))
ModelLabels<-c("Base","SizeFreq Lambda=0.1")
Directory<-paste0(base.dir,"\\Plots\\08_SizeFreqLambda")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(Lambda_sens,ModelLabels,NModels,Directory)


CPUELength_sens<-SSsummarize(list(base_case, model_21,model_23,model_24, model_25))
ModelLabels<-c("Base","Start ISH in 1992","1992 ISH w/2 Qs","Single Q DSH","Combo")
Directory<-paste0(base.dir,"\\Plots\\09_CPUELength")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(CPUELength_sens,ModelLabels,NModels,Directory)


Start1970_sens<-SSsummarize(list(base_case, model_26))
ModelLabels<-c("Base","Start in 1970")
Directory<-paste0(base.dir,"\\Plots\\11_Start1970")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(Start1970_sens,ModelLabels,NModels,Directory)

AltSetup_sens<-SSsummarize(list(base_case, model_28, model_29, model_30))
ModelLabels<-c("Base","Time Varying Q", "Effect N from SS","No diver survey")
Directory<-paste0(base.dir,"\\Plots\\12_TimeVaryQ")
dir.create(Directory)
NModels<-length(ModelLabels)
plotsensitivity(AltSetup_sens,ModelLabels,NModels,Directory)



# Create a summary table with all scenario runs
max_yr=2018

Model.folders <- list.dirs(path = "01_SS final", full.names = TRUE, recursive = F)
Model.folders <- Model.folders[-length(Model.folders)]
N <- length(Model.folders)
Results       <- data.table(Model=Model.folders, F2018=numeric(N),Fmsy=numeric(N),F2018_Fmsy=numeric(N),SSBmsy=numeric(N),SSB2018=numeric(N),SSB2018_SSBmsy=numeric(N),CatchMSY=numeric(N))
Results$Model <- str_remove(Results$Model, "01_SS final/")
for(i in 1:length(Model.folders)){
  
  aModel                    <- SS_output(Model.folders[i],ncols = 500,covar=FALSE)
  rnames                    <- aModel$derived_quants$Label
  index_SSB_MSY             <- which(rnames==paste("SSB_MSY",sep=""))
  index_Fstd_MSY            <- which(rnames==paste("Fstd_MSY",sep=""))
  index_SSB_TermYr          <- which(rnames==paste("SSB_",max_yr,sep=""))
  index_Fstd_TermYr         <- which(rnames==paste("F_",max_yr,sep=""))
  index_MSY                 <- which(rnames==paste("Dead_Catch_MSY",sep=""))
  
  Results[i]$SSBmsy         <- round(aModel$derived_quants[index_SSB_MSY:index_SSB_MSY,2],0)
  Results[i]$Fmsy           <- round(aModel$derived_quants[index_Fstd_MSY:index_Fstd_MSY,2],2)
  Results[i]$SSB2018        <- round(aModel$derived_quants[index_SSB_TermYr:index_SSB_TermYr,2],0)
  Results[i]$F2018          <- round(aModel$derived_quants[index_Fstd_TermYr:index_Fstd_TermYr,2],2)
  Results[i]$F2018_Fmsy     <- round(Results[i]$F2018/Results[i]$Fmsy,2)  
  Results[i]$SSB2018_SSBmsy <- round(Results[i]$SSB2018/Results[i]$SSBmsy,2)
  Results[i]$CatchMSY       <- round(aModel$derived_quants[index_MSY:index_MSY,2],0)
}

write.csv(Results,file="01_SS final/Plots/Results.csv")





