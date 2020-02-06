##read in agepro datafile and change projected catch line
#install.packages("foreach","doParallel")
setwd("I:\\Nadon\\01_Uku assessment 2019\\025_Base (for reference only)\\Projections")
library(reshape2); library(ggplot2); library(foreach); library(doParallel)
#read in data file
Input <- read.delim("Uku_Projection_base.INP", fill=FALSE, stringsAsFactors = FALSE, header=FALSE)

##change line 58 (Harvest amount) for 3 recruitment scenarios model, line 52 for single recruitment scenario model
##initial harvest scenario
HarvestScenario <- as.numeric(unlist(strsplit(Input[57,],split="  ")))
HarvestScenario <- c(108, 201,201,201,201,201,201,201)

#how much you'll change the catch
Decrease   <- 1
nProj      <- 200
FileName   <- list(NA)
SysCommand <- list(NA)

numCores   <- detectCores()
cl         <- makeCluster(numCores)
registerDoParallel(cl)

for (i in 1:nProj){

  HarvestScenario[2:length(HarvestScenario)] <- HarvestScenario[2:length(HarvestScenario)]-Decrease
  NewHarvest                                 <- paste(HarvestScenario[1:length(HarvestScenario)],sep="",collapse="  ")
  Input[57,1]                                <- NewHarvest
  FileName[[i]]                              <- paste0("UkuProjection",i,".INP")
  write.table(Input,FileName[[i]], quote=FALSE,row.names = FALSE, col.names=FALSE)
}

foreach (i=1:nProj) %dopar% {
        system("cmd.exe", input = paste0("AGEPRO40.exe ",FileName[[i]]))
}
stopCluster(cl)

PlotData <- data.frame("Catch"=NA,"Year"=NA,"FMort"=NA,"FProb"=NA,"SSB"=NA,"SSBProb"=NA)
FMort    <- matrix(NA,nrow=7,ncol=3,byrow = TRUE)
FProb    <- matrix(NA,nrow=7,ncol=2,byrow = TRUE)
SSB      <- matrix(NA,nrow=7,ncol=3,byrow = TRUE)
SSBProb  <- matrix(NA,nrow=7,ncol=2,byrow = TRUE)
for (i in 1:200) {
  
    file.name      <- paste0("UkuProjection",i,".out")
    Output         <- read.delim(file.name, fill=FALSE, stringsAsFactors = FALSE, header=FALSE)
    Catch          <- as.numeric(unlist(strsplit(Output[117,],split="        ")))[2]*1000
    FMort          <- data.frame(matrix(as.numeric(unlist(strsplit(Output[167:173,],split="     "))),ncol=10,nrow=7,byrow=TRUE))[,c(1,6)]
    names(FMort)   <- c("Year","FMort")
    FProb          <- data.frame(matrix(as.numeric(unlist(strsplit(Output[188:194,],split="    "))),ncol=2,nrow=7,byrow=TRUE))
    names(FProb)   <- c("Year","FProb")
    SSB            <- data.frame(matrix(as.numeric(unlist(strsplit(Output[67:73,],split="     "))),ncol=10,nrow=7,byrow=TRUE))[,c(1,6)]
    names(SSB)     <- c("Year","SSB")
    SSBProb        <- data.frame(matrix(as.numeric(unlist(strsplit(Output[177:183,],split="    "))),ncol=2,nrow=7,byrow=TRUE))
    names(SSBProb) <- c("Year","SSBProb")
    PlotDataTemp   <- cbind(Catch,FMort,"FProb"=FProb[,2],"SSB"=SSB[,2],"SSBProb"=SSBProb[,2])
    PlotData       <- rbind(PlotData,PlotDataTemp)    
}
PlotData <- PlotData[-1,]

###Plots
png("FishingMortalityProjections.png",height=4,width=4,units="in",res=200)
ggplot()+
    geom_line(aes(x=Catch,y=FMort,linetype=as.factor(Year)), data=PlotData)+
    theme_bw() +
    theme(legend.title=element_blank())+
    xlab("Catch (mt)")+
    ylab("Fishing Mortality")
dev.off()

png("FProbabilityProjections.png",height=4,width=4,units="in",res=200)
ggplot()+
    geom_line(aes(x=Catch,y=FProb,linetype=as.factor(Year)), data=PlotData)+
    theme_bw() +
    theme(legend.title=element_blank())+
    xlab("Catch (mt)")+
    ylab("Probability of F>FMSY")
dev.off()

png("SSBProjections.png",height=4,width=4,units="in",res=200)
ggplot()+
    geom_line(aes(x=Catch,y=SSB*1000/2,linetype=as.factor(Year)), data=PlotData)+  # Divide by 2 to convert to female-only SSB
    theme_bw() +
    theme(legend.title=element_blank())+
    xlab("Catch (mt)")+
    ylab("Spawning Stock Biomass (mt)")
dev.off()

png("SSBProbabilityProjections.png",height=4,width=4,units="in",res=200)
ggplot()+
    geom_line(aes(x=Catch,y=1-SSBProb,linetype=as.factor(Year)), data=PlotData)+
    theme_bw() +
    theme(legend.title=element_blank())+
    xlab("Catch (mt)")+
    ylab("Probability of SSB<SSBMSST")+
    scale_y_continuous(limits=c(0,1))
dev.off()

##Tables
minProb <- 0
maxProb <- 0.5

TableData       <- data.frame("Catch"=NA,"Year"=NA,"FMort"=NA,"FProb"=NA,"SSB"=NA,"SSBProb"=NA)
ProbabilityList <- seq(minProb,maxProb,0.05)
for (j in 1:length(ProbabilityList)){
    Probability <- ProbabilityList[j]
    
   for(i in 1:7){
    TableTemp  <- PlotData[which(PlotData$FProb<=Probability),]
    TableTemp1 <- TableTemp[which(TableTemp$Year==2019+i),]
    TableData  <- rbind(TableData,TableTemp1[which.max(TableTemp1$Catch),])
   }
}
TableData       <- TableData[-1,]
TableData$FProb <- round(TableData$FProb/0.05)*0.05
TableData       <- TableData[order(TableData$FProb),]

SSBProb_Table     <- dcast(TableData,Year~FProb,value.var="SSBProb")
SSB_Table         <- dcast(TableData,Year~FProb,value.var="SSB")
SSB_Table[,2:12]  <- SSB_Table[,2:12]/2 # Agepro SSB is male+female. Divide by 2 to convert to female-only SSB
Catch_Table       <- dcast(TableData,Year~FProb,value.var="Catch")
FMort_Table       <- dcast(TableData,Year~FProb,value.var="FMort")
write.csv(Catch_Table,"ProjectionCatch.csv")
write.csv(FMort_Table,"ProjectionFMort.csv")
write.csv(SSB_Table,"ProjectionSSB.csv")
write.csv(SSBProb_Table,"ProjectionSSBProb.csv")
ProbabilityList2  <- seq(minProb,maxProb,0.01)
ProjectionTable   <- matrix(NA,nrow=length(ProbabilityList2),ncol=7)
for (i in 1:length(ProbabilityList2)){
    Probability=ProbabilityList2[i]
    for (j in 1:7){
        Temp1<-PlotData[which(PlotData$FProb<=Probability),]
        Temp2<-Temp1[which(Temp1$Year==2019+j),]
        ProjectionTable[i,j]<-max(Temp2$Catch)
    }
}

ProjectionTableFinal        <- cbind.data.frame(ProbabilityList2,ProjectionTable)
names(ProjectionTableFinal) <- c("Probability","2020","2021","2022","2023","2024","2025","2026")
ProjectionTableFinal        <- ProjectionTableFinal[order(-ProjectionTableFinal$Probability),]
write.csv(ProjectionTableFinal,"CatchProjectionTable.csv")
