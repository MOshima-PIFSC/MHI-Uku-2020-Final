wd=c("C:\\Users\\michelle.sculley\\Documents\\Uku\\Assessment\\SS3\\010_Base")

wd=c("C:\\Users\\michelle.sculley\\Documents\\Uku\\021_Base")

wdF=c("C:\\Users\\michelle.sculley\\Documents\\Uku\\021_Base\\rawF")

setwd(wd)



## devtools::install_github("r4ss/r4ss") # to update r4ss
library(r4ss);library(ggplot2);library(reshape2);library(scales);library(RColorBrewer);library(gridExtra);library(dplyr);library(tidyr)

base.model<-SS_output(wd)
SS_plots(base.model) 

##run a model with F set in the starter menu to raw F
rawFModel<-SS_output(wdF)

##To make a better looking data plot
## only necessary if the data plot is squished. May need to adjust the margins to fit the data
# SSplotData(base.model,pheight=7, pwidth = 10, margins = c(5.1,2.1,2.1,18.1),plot=TRUE, print=TRUE)



##Retrospective analysis and plots

origdir<-c("C:\\Users\\michelle.sculley\\Documents\\Uku\\010_Base\\Retro")

##automatically does a 5 year retrospective. change years to do longer or shorter retros
SS_doRetro(masterdir=origdir,oldsubdir = "", newsubdir = "retrospectives",years=0:-5, extras = "-nohess -nox")
## load the models
retroModels <- SSgetoutput(dirvec=file.path(origdir, "retrospectives",paste("retro",0:-5,sep="")))
##plot the models
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary$endyrs + 0:-5
SSplotComparisons(retroSummary, endyrvec=endyrvec, legendlabels=paste("Data",0:-5,"years"),png=TRUE, plotdir=origdir, legend= FALSE, type="l", sprtarg =0, indexfleets = 1:10)

## create biomass and SPR retrospective plots. Be sure to update years as necessary
SummaryBio<-retroSummary$SpawnBio
names(SummaryBio)<-c("basecase","retro-1","retro-2","retro-3","retro-4","retro-5","Label","Yr")
SSBMSY<-retroSummary$quants[which(retroSummary$quants$Label=="SSB_MSY"),]
SSBMSST<-0.9*SSBMSY[,1:6]
SummaryBratio<-as.data.frame(matrix(NA,ncol=8, nrow=nrow(SummaryBio)))
for(i in 1:6){
  for (j in 1:nrow(SummaryBio)){
  SummaryBratio[j,i]<-SummaryBio[j,i]/SSBMSST[i]
}}
SummaryBratio[,7:8]<-SummaryBio[,7:8]
SummaryBio<-melt(SummaryBio,id.vars=c("Label","Yr"))
SummaryBio<-subset(SummaryBio,Yr>=1948)
RemoveVector<-c(which(SummaryBio$variable=="retro-1"&SummaryBio$Yr==2017),which(SummaryBio$variable=="retro-2"&SummaryBio$Yr>=2016),which(SummaryBio$variable=="retro-3"&SummaryBio$Yr>=2015),which(SummaryBio$variable=="retro-4"&SummaryBio$Yr>=2014),which(SummaryBio$variable=="retro-5"&SummaryBio$Yr>=2013))
SummaryBio<-SummaryBio[-RemoveVector,]

names(SummaryBratio)<-c("basecase","retro-1","retro-2","retro-3","retro-4","retro-5","Label","Yr")
SummaryBratio<-melt(SummaryBratio,id.vars=c("Label","Yr"))
SummaryBratio<-subset(SummaryBratio,Yr>=1948)
RemoveVector<-c(which(SummaryBratio$variable=="retro-1"&SummaryBratio$Yr==2017),which(SummaryBratio$variable=="retro-2"&SummaryBratio$Yr>=2016),which(SummaryBratio$variable=="retro-3"&SummaryBratio$Yr>=2015),which(SummaryBratio$variable=="retro-4"&SummaryBratio$Yr>=2014),which(SummaryBratio$variable=="retro-5"&SummaryBratio$Yr>=2013))
SummaryBratio<-SummaryBratio[-RemoveVector,]


a<-ggplot() +
  geom_line(aes(x=Yr,y=value,color=variable),data=SummaryBio, size=0.9) +
  theme(panel.border = element_rect(color="black",fill=NA,size=1),
        panel.background = element_blank(), strip.background = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("basecase" = "black","retro-1" = "red", "retro-2"="orange","retro-3"="yellow","retro-4"="green","retro-5"="blue", "basecase"="black")) + xlab("Year") + ylab("Spawning Biomass (mt)") +
  geom_line(aes(x=Yr,y=value),data=subset(SummaryBio,variable=="basecase"),color="black", size=1)



c<-ggplot() +
  geom_line(aes(x=Yr,y=value,color=variable),data=SummaryBratio, size=0.9) +
  theme(panel.border = element_rect(color="black",fill=NA,size=1),
        panel.background = element_blank(), strip.background = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("basecase" = "black","retro-1" = "red", "retro-2"="orange","retro-3"="yellow","retro-4"="green","retro-5"="blue", "basecase"="black")) + xlab("Year") + ylab("B/BMSST") +
  geom_line(aes(x=Yr,y=value),data=subset(SummaryBratio,variable=="basecase"),color="black", size=1) +
  ylim(0,3) + geom_hline(aes(yintercept=1), linetype="dashed", data=SummaryBratio)




FishingMort<-retroSummary$Fvalue
names(FishingMort)<-c("basecase","retro-1","retro-2","retro-3","retro-4","retro-5","Label","Yr")
FishingMort<-melt(FishingMort,id.vars=c("Label","Yr"))
FishingMort<-subset(FishingMort,Yr>=1948)
RemoveVector<-c(which(FishingMort$variable=="retro-1"&FishingMort$Yr==2017),which(FishingMort$variable=="retro-2"&FishingMort$Yr>=2016),which(FishingMort$variable=="retro-3"&FishingMort$Yr>=2015),which(FishingMort$variable=="retro-4"&FishingMort$Yr>=2014),which(FishingMort$variable=="retro-5"&FishingMort$Yr>=2013))
FishingMort<-FishingMort[-RemoveVector,]

b<-ggplot() +
  geom_line(aes(x=Yr,y=value,color=variable),data=FishingMort, size=1) +
  theme(panel.border = element_rect(color="black",fill=NA,size=0.9),
        panel.background = element_blank(), strip.background = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("basecase" = "black","retro-1" = "red", "retro-2"="orange","retro-3"="yellow","retro-4"="green","retro-5"="blue", "basecase"="black")) + xlab("Year") + ylab("F/FMSY") +
  geom_line(aes(x=Yr,y=value),data=subset(FishingMort,variable=="basecase"),color="black", size=1) +
  scale_y_continuous(limits = c(0,1.25)) + geom_hline(aes(yintercept=1), linetype="dashed", data=FishingMort)

png("Retrospectives.png",height=4, width=8, units="in", res=300)
grid.arrange(c,b,ncol=2)
dev.off()


## For the annual time series of F/Fmsy, Biomass, Recruitment, and SSB:

SummBio<-base.model$timeseries[,c("Yr","Seas","Bio_all","Bio_smry","SpawnBio","Recruit_0")]
SumBioAll<-subset(SummBio,Seas==1)[,c("Yr","Bio_all")]
SumBiosmry<-subset(SummBio,Seas==1)[,c("Yr","Bio_smry")]

##these have to be updated based upon the corresponding values in the base model (already  done for Uku)
SumRecruit<-base.model$derived_quants[76:146,]
SumRecruit$Year<-seq(1948,2018,1)
SumBioSpawn<-base.model$derived_quants[3:73,]
SumBioSpawn$Year<-seq(1948,2018,1)
SSBRatio<-SumBioSpawn$x/base.model$derived_quants[1,2]
Fseries<-rawFModel$derived_quants[218:288,]
Fseries$Year<-seq(1948,2018,1)

#Fishing Mortality

png("F_Series.png",height=8,width=16,units="in",res=300)
ggplot()+
  geom_point(aes(x=Year,y=Value), data=Fseries,size=4)+
  geom_errorbar(aes(x=Year,ymin=Value-1.96*StdDev,ymax=Value+1.96*StdDev),data=Fseries,size=1.5)+
  geom_line(aes(x=Year,y=Value),data=Fseries,size=1)+
  geom_hline(yintercept=0.16,color="green",linetype = 2, size=1.5)+
  ylab("Fishing Mortality (Ages 5-30)") +
  theme(axis.text.x=element_text(size=18,face="bold"), axis.title.x=element_text(size=24,face="bold"),
        axis.text.y=element_text(size=18,face="bold"),axis.title.y=element_text(size=24,face="bold"),
        panel.border = element_rect(color="black",fill=NA,size=2),
        panel.background = element_blank())+
  geom_text(aes(x = 2021,y=0.18,label=as.character(expression(F[MSY]))),parse=TRUE, size=8) +
  scale_x_continuous(breaks=seq(1948,2018,5))
dev.off()

##Recruitment
## recruitment should end in 2017 as it is the last year estimated in the model. the final year of the model (2018) is based upon the S/R curve and NOT the data

SumRecruit$LB<-SumRecruit$Value-1.96*SumRecruit$StdDev
SumRecruit$LB<-ifelse(SumRecruit$LB<0,0,SumRecruit$LB)
SumRecruit<-SumRecruit[-nrow(SumRecruit),]
png("Recruitment.png",height=8,width=18, units="in",res=300)
ggplot()+
  geom_point(aes(x=Year,y=Value), data=SumRecruit,size=4)+
  geom_errorbar(aes(x=Year,ymin=LB,ymax=Value+1.96*StdDev),data=SumRecruit,size=1.5)+
  geom_line(aes(x=Year,y=Value),data=SumRecruit,size=1)+
  ylab("Recruitment (thousands of age-0 recruits)") +
  theme(axis.text.x=element_text(size=18,face="bold"), axis.title.x=element_text(size=24,face="bold"),
        axis.text.y=element_text(size=18,face="bold"),axis.title.y=element_text(size=24,face="bold"),
        panel.border = element_rect(color="black",fill=NA,size=2),
        panel.background = element_blank())+
  scale_x_continuous(breaks=seq(1948,2018,5))
dev.off()

#SSB
png("Biomass_SSB.png",height=8,width=18, units="in",res=300)
ggplot()+
  geom_point(aes(x=Year,y=Value), data=SumBioSpawn,size=4)+
  geom_errorbar(aes(x=Year,ymin=Value-1.96*StdDev,ymax=Value+1.96*StdDev),data=SumBioSpawn,size=1.5)+
  geom_line(aes(x=Year,y=Value),data=SumBioSpawn,size=1)+
  geom_hline(yintercept=0.9*base.model$derived_quants[which(base.model$derived_quants[,1]=="SSB_MSY"),2],color="green",linetype = 2, size=1.5)+
  ylab("Female Spawning Biomass (mt)") +
  theme(axis.text.x=element_text(size=24,face="bold"), axis.title.x=element_text(size=30,face="bold"),
        axis.text.y=element_text(size=24,face="bold"),axis.title.y=element_text(size=30,face="bold"),
        panel.border = element_rect(color="black",fill=NA,size=2),
        panel.background = element_blank())+
  geom_text(aes(x = 2015,y=base.model$derived_quants[which(base.model$derived_quants[,1]=="SSB_MSY"),2]+10,label=as.character(expression(SSB[MSST]))),parse=TRUE, size=10) +
  scale_x_continuous(breaks=seq(1948,2018,5))

dev.off()

## Summary Biomass
png("Biomass_smry.png",height=8,width=18, units="in",res=300)
ggplot()+
  geom_point(aes(x=Yr,y=Bio_smry), data=SumBiosmry,size=4)+
  geom_line(aes(x=Yr,y=Bio_smry), data=SumBiosmry[-1,],size=1)+
  ylab("Biomass (mt, ages 1+)") +
  xlab("Year")+
  theme(axis.text.x=element_text(size=24,face="bold"), axis.title.x=element_text(size=30,face="bold"),
        axis.text.y=element_text(size=24,face="bold"),axis.title.y=element_text(size=30,face="bold"),
        panel.border = element_rect(color="black",fill=NA,size=2),
        panel.background = element_blank())+
  scale_x_continuous(breaks=seq(1948,2018,5)) +
  scale_y_continuous(limits = c(0,3000),label = comma)
dev.off()


##CPUE Plots

CPUE<-base.model$cpue[,c("Fleet","Fleet_name","Yr","Seas","Obs","Exp","SE")]
CPUE$SE<-ifelse(CPUE$Fleet==10,CPUE$SE,CPUE$SE+0.1)
CPUE$LL<-ifelse(CPUE$Obs-1.96*CPUE$SE>=0,CPUE$Obs-1.96*CPUE$SE,0)
png("CPUE_index.png",height=16,width=12,units="in",res=300)
ggplot()+
  geom_point(aes(x=Yr,y=Obs),data=CPUE) +
  geom_errorbar(aes(x=Yr,ymin=LL,ymax=Obs+1.96*SE),data=CPUE,width=0) +
  geom_line(aes(x=Yr,y=Exp),data=CPUE) +
  facet_wrap(~Fleet_name, ncol=1, scales="free_y") +
  theme_bw() +
  theme(panel.border = element_rect(color="black",fill=NA,size=1),
        panel.background = element_blank(), strip.background = element_blank(),
        strip.text = element_text(size=16),
        axis.text.x=element_text(size=20,face="bold"), axis.title.x=element_text(size=30,face="bold"),
        axis.text.y=element_text(size=24,face="bold"),axis.title.y=element_text(size=30,face="bold")) +
  xlab("Year") + ylab("CPUE") +
  scale_x_continuous(breaks=seq(1948,2018,5))
dev.off()


##ASPM Figure  
ASPMDir<-"C:\\Users\\michelle.sculley\\Documents\\Uku\\021_Base\\ASPM"
ASPM<-SS_output(ASPMDir)
SS_plots(ASPM)
ASPMSumBioSpawn<-ASPM$derived_quants[3:73,]
ASPMSumBioSpawn$Year<-seq(1948,2018,1)

png("ASPMBiomass_Spawn.png",height=8,width=18, units="in",res=300)
ggplot()+
    geom_point(aes(x=Year,y=Value), data=SumBioSpawn,size=4,fill="blue",color="blue")+
    geom_line(aes(x=Year,y=Value), data=SumBioSpawn,size=1,color="blue")+
    geom_point(aes(x=Year,y=Value), data=ASPMSumBioSpawn,size=4, shape=17,fill="red",color="red")+
    geom_line(aes(x=Year,y=Value), data=ASPMSumBioSpawn,size=1, linetype="dashed",color="red")+
    geom_ribbon(aes(x=Year,ymin=Value-1.96*StdDev,ymax=Value+1.96*StdDev),data=SumBioSpawn,alpha=0.1,color="blue",fill="blue")+
  geom_ribbon(aes(x=Year,ymin=Value-1.96*StdDev,ymax=Value+1.96*StdDev),data=ASPMSumBioSpawn,alpha=0.1,color="red",fill="red")+
    ylab("Spawning Biomass (mt)") +
    xlab("Year")+
    theme(axis.text.x=element_text(size=24,face="bold"), axis.title.x=element_text(size=30,face="bold"),
          axis.text.y=element_text(size=24,face="bold"),axis.title.y=element_text(size=30,face="bold"),
          panel.border = element_rect(color="black",fill=NA,size=2),
          panel.background = element_blank())+
    scale_x_continuous(breaks=seq(1948,2018,5))# +
    scale_y_continuous(limits = c(0,750),label = comma) 
dev.off()

## Jitter analysis
## 1. make a folder named "Jitter" with the starter, forecast, data, control, and ss.par files and ss executible

Jitterwd<-c("C:\\Users\\michelle.sculley\\Documents\\Uku\\Jitter")

#### Change starter file appropriately (can also edit file directly)
starter <- SS_readstarter(file.path(Jitterwd, 'starter.ss'))
# Change to use .par file
starter$init_values_src = 1
# Change jitter (0.1 is an arbitrary, but common choice for jitter amount)
starter$jitter_fraction = 0.1
# write modified starter file
SS_writestarter(starter, dir=Jitterwd, overwrite=TRUE)
# number of jitters to run
Njitter<-10
#### Run jitter using this function
jit.likes <- SS_RunJitter(mydir=Jitterwd, Njitter=NJitter, Intern=FALSE)

#### Read in results using other r4ss functions
# (note that un-jittered model can be read using keyvec=0:Njitter)
profilemodels <- SSgetoutput(dirvec=Jitterwd, keyvec=1:Njitter, getcovar=FALSE)
# summarize output
profilesummary <- SSsummarize(profilemodels)
# Likelihoods
likelihoods<-t(as.matrix(profilesummary$likelihoods[1,1:Njitter]))
R0<-t(as.matrix(profilesummary$pars[15,1:Njitter]))

# Parameters
#profilesummary$pars
ggplot()+
    geom_point(aes(x=R0,y=likelihoods)) +
    geom_point(aes(x=base.model$parameters["SR_LN(R0)","Value"], y=base.model$likelihoods_used["TOTAL","values"]),col="red", shape=15) +
    theme_bw() +
    ylab("Likelihood")+
    xlab("ln(R0)")
##plot Jitter MLEs vs R0

##For the catch summary figure (by year and fleet)
    ## This one looks better than the one from r4ss
## Unnecessary for Uku, useful for international fisheries
 Catch<-base.model$timeseries[,c("Yr","sel(B):_1","sel(B):_2","sel(B):_3","sel(B):_4","sel(B):_5")]
 names(Catch)<-c("Yr","Com_DSH","Com_ISH","Com_Trol","Com_Other","Rec")
 
 CatchTotal<-melt(Catch, id.vars=c("Yr"))
 names(CatchTotal)<-c("Year","Name","Obs")
 
 colourCount = length(unique(CatchTotal$Name))
 getPalette =colorRampPalette(brewer.pal(11, "Spectral"))
 Fill<-getPalette(colourCount)
 png("Catch.png", height=4, width=8, units="in",res=300)
 ggplot()+
     geom_bar(aes(x=Year,y=Obs,fill=Name),data=CatchTotal,stat="identity",color="black") +
     scale_fill_manual(values = Fill, name="")+
     xlab("Year") +
     ylab("Catch (mt)") +
     theme_bw()
dev.off()



##Bubble Plots of quarterly size composition by fleet
 SizeComp<-read.csv("C:\\Users\\michelle.sculley\\Documents\\Uku\\SizeComp.csv", header=TRUE)
 SizeCompPlot<-melt(SizeComp,id.vars=c("Year","Fleet"))
# 
 SizeCompPlot$variable<-gsub("X","",as.character(SizeCompPlot$variable))
 SizeCompPlot$variable<-as.numeric(SizeCompPlot$variable)
 SizeCompPlot$yr<-gsub("-","",as.character(SizeCompPlot$Year))
 SizeCompPlot$Year<-as.numeric(SizeCompPlot$Year)
 class(SizeCompPlot$variable)
 SizeCompPlot<-subset(SizeCompPlot,value>0)
 
 png("WeightComp.png",height=4, width=16,units="in",res=300) 
 ggplot()+
     geom_point(aes(x=Year,y=variable, size=value),data=SizeCompPlot, shape=21)+
     theme(panel.border = element_rect(color="black",fill=NA,size=2),
           panel.background = element_blank(), strip.background = element_blank()) +
     labs(x="Year",y="Weight (lbs)",size="Number of fish \n measured")+
     scale_x_continuous(breaks=seq(1948,2018,2), limits=c(1948,2018))
 dev.off()

 # ## Aggregate size composition plots (looks better than r4ss plots)
 SizeCompTot<-aggregate(SizeCompPlot$value,by=list(SizeCompPlot$Fleet,SizeCompPlot$variable), sum)
 names(SizeCompTot)<-c("Fleet","Length","NumMeasured")
 
 png("SizeCompAgg.png",height=10,width=8,units="in",res=300)
 ggplot()+
     geom_line(aes(x=Length, y=NumMeasured), data=SizeCompTot, color="blue") + 
     theme(panel.border = element_rect(color="black",fill=NA,size=2),
           panel.background = element_blank(), strip.background = element_blank())+
     labs(x="Eye-Fork Length (cm)",y="Number of fish measured") +
     geom_point(aes(x=Length,y=NumMeasured),data=subset(SizeCompTot, Fleet<=18), color="blue")
 dev.off()







