require(data.table);require(reshape2); require(openxlsx);require(plyr); require(boot); require(ggplot2); require(dplyr); require(stringr);require(sf); require(rnaturalearth);
library("png"); library("grid");library("gridExtra")

load("Data/ALL_REA_FISH_RAW.rdata")   
UVS <- data.table(df)

UVS[OBS_YEAR==2006]$OBS_YEAR <- 2005
UVS[OBS_YEAR==2013]$OBS_YEAR <- 2012

#===Core Filters==============================
UVS <- UVS[EXCLUDE_FLAG!=-1]
UVS$SIZE_ <- UVS$SIZE_*10*0.9 # Convert to mm FL
UVS  <- UVS[REGION=="MHI"]
UVS  <- UVS[METHOD!="nSPC-CCR"&METHOD!="SPC"]
UVS  <- UVS[OBS_TYPE=='U' | OBS_TYPE=='I' | OBS_TYPE=='N' ]
UVS <- subset(UVS,select=c("METHOD","OBS_TYPE","OBS_YEAR","REGION","ISLAND","SECTOR","SITE","LONGITUDE","LATITUDE","SITEVISITID","DEPTH_BIN","REP","DIVER","FAMILY","SPECIES","SIZE_","COUNT","DENSITY","BIOMASS_G_M2"))
UVS <- data.table(   mutate_if(UVS,is.factor,as.character)  )

# Create yearly map of the survey sites

MAP      <- UVS[,list(SUM=sum(COUNT)),by=list(OBS_YEAR,SECTOR,ISLAND,LONGITUDE,LATITUDE,SITEVISITID)]
coast    <- ne_coastline(scale="medium",returnclass="sf") 
aTheme   <- theme(axis.text=element_text(size=6),
                legend.title=element_text(size=4),
                axis.title=element_text(size=6),
                title=element_text(size=6)
             )

aList <- list()
YearList <- unique(MAP$OBS_YEAR)
YearList <- sort(YearList)
for(i in 1:length(YearList)){
aList[[i]] <- ggplot()+geom_sf(data=coast,lwd=1)+
  coord_sf(xlim=c(-154,-161),ylim=c(18,23))+
  geom_point(data=MAP[OBS_YEAR==YearList[i]],aes(x=LONGITUDE,y=LATITUDE),size=1,color="red")+
  ggtitle(YearList[i])+theme_bw(base_size=20)+aTheme

}

aGrid <- grid.arrange(aList[[1]],aList[[2]],aList[[3]],aList[[4]],aList[[5]],aList[[6]],ncol=2)


mypath <- paste("Outputs/Graphs/Spatial/01_SPAT_DIVESITES.png",sep="")
ggsave(mypath,plot=aGrid,units="cm",height=20,width=15, pointsize=1, dpi=300)

#====================Inputs====================
LS50 <- 350  # Recreational selectivity
LS95 <- 500
LWA  <- 0.0118 # cm to g
LWB  <- 3.043  # cm to g
GCF.PRES <- -0.8  # Gear calibration factor (substract this value from logit-transformed observation probability of blt to convert to SPC)
GCF.POS  <- 0.94  # Gear calibration factor (divide by this value the positive-only density of blt to convert to SPC)

UVS$SELECTIVITY   <- 1/(  1+exp(-log(19)*(LS50)/(LS95-LS50))  ) 
UVS$SEL.DENSITY   <- UVS$DENSITY*UVS$SELECTIVITY
UVS$BIOMASS_KG    <- UVS$SEL.DENSITY*(LWA*(UVS$SIZE_/10)^LWB )/1000

#UVS$ABUNDANCE     <- UVS$SEL.DENSITY # Select which measure of abundance to use
UVS$ABUNDANCE     <- UVS$BIOMASS_KG # Select which measure of abundance to use

#=====Sector weights============================
WEIGHTS <- data.frame(cbind(c("HAWAII","MAUI_NUI","OAHU","KAUAI_NIIHAU"),c(0.23,0.58,0.11,0.08)),stringsAsFactors = F)
colnames(WEIGHTS) <- c("ISLANDGROUP","WEIGHTS");   WEIGHTS$WEIGHTS <- as.numeric(WEIGHTS$WEIGHTS)

UVS$ISLANDGROUP[UVS$ISLAND=="Hawaii"]<-"HAWAII"
UVS$ISLANDGROUP[UVS$ISLAND=="Maui"|UVS$ISLAND=="Molokai"|UVS$ISLAND=="Lanai"|UVS$ISLAND=="Molokini"]<-"MAUI_NUI"
UVS$ISLANDGROUP[UVS$ISLAND=="Oahu"]<-"OAHU"
UVS$ISLANDGROUP[UVS$ISLAND=="Kauai"|UVS$ISLAND=="Niihau"|UVS$ISLAND=="Lehua"|UVS$ISLAND=="Kaula"]<-"KAUAI_NIIHAU"

UVS <- merge(UVS,WEIGHTS,by="ISLANDGROUP")

#============Some filters and conversions==================
UVS$DEPTH_BIN2 <- UVS$DEPTH_BIN


UVS[DEPTH_BIN=="Mid"|DEPTH_BIN=="Deep"]$DEPTH_BIN2 <- "Deep"
UVS <- UVS[DEPTH_BIN!="Shallow"]
#UVS <- UVS[DEPTH_BIN=="Mid"]
#UVS <- UVS[ISLANDGROUP=="MAUI_NUI"]

#=======Explore sample size and distribution===========================
SAMP_TOTAL <- UVS[,list(N=sum(COUNT)),by=list(OBS_YEAR,ISLANDGROUP,DEPTH_BIN2,SITEVISITID)]
SAMP_TOTAL <- SAMP_TOTAL[,list(N=.N),by=list(OBS_YEAR,ISLANDGROUP,DEPTH_BIN2)]
SAMP_TOTAL <- SAMP_TOTAL[order(OBS_YEAR,ISLANDGROUP,DEPTH_BIN2)]
SAMP_TOTAL <- dcast(SAMP_TOTAL,OBS_YEAR+DEPTH_BIN2~ISLANDGROUP,value.var="N",fill=0)

SAMP_UKU <- UVS[SPECIES=="APVI",list(NUM=sum(COUNT)),by=list(OBS_YEAR,ISLANDGROUP,DEPTH_BIN2)]
SAMP_UKU <- SAMP_UKU[order(OBS_YEAR,ISLANDGROUP,DEPTH_BIN2)]
SAMP_UKU <- dcast(SAMP_UKU,OBS_YEAR+DEPTH_BIN2~ISLANDGROUP,value.var="NUM",fill=0)

#==================ANALYZE DATA===========================================
UVS <- UVS[,list(ABUNDANCE=sum(ABUNDANCE)/2),by=list(METHOD,YEAR=OBS_YEAR,ISLANDGROUP,ISLAND,DEPTH_BIN2,WEIGHTS,SITE,REP,SPECIES)]
UVS <- dcast(UVS, YEAR+METHOD+ISLANDGROUP+ISLAND+DEPTH_BIN2+WEIGHTS+SITE+REP~SPECIES, value.var="ABUNDANCE", fill=0)
UVS <- data.table(  melt(UVS, id.vars=c("YEAR","ISLANDGROUP","ISLAND","DEPTH_BIN2","WEIGHTS","METHOD","SITE","REP"),variable.name="SPECIES",value.name="ABUNDANCE")  )
UVS <- UVS[,list(ABUNDANCE=mean(ABUNDANCE)),by=list(METHOD,YEAR,ISLANDGROUP,ISLAND,DEPTH_BIN2,WEIGHTS,SITE,SPECIES)]
UVS <- UVS[SPECIES=="APVI"]
UVS$PRES              <- 0
UVS[ABUNDANCE>0]$PRES <- 1

# Base case
BAS_POS       <- UVS[PRES==1,list(POS=mean(log(ABUNDANCE))),by=list(YEAR,ISLANDGROUP,WEIGHTS)]
BAS_POS       <- BAS_POS[,list(POS=weighted.mean(POS,WEIGHTS)),by=list(YEAR)]
BAS_PRES      <- UVS[,list(PRES=mean(PRES)),by=list(YEAR,ISLANDGROUP,WEIGHTS)]
BAS_PRES      <- BAS_PRES[,list(PRES=weighted.mean(PRES,WEIGHTS)),by=list(YEAR)]
BAS           <- merge(BAS_POS,BAS_PRES,by="YEAR")
BAS$ABUNDANCE <- exp(BAS$POS)*BAS$PRES

BAS$POS_CAL              <- BAS$POS
BAS[YEAR<=2008]$POS_CAL  <- BAS[YEAR<=2008]$POS_CAL/GCF.POS
BAS$PRES_CAL             <- BAS$PRES
BAS[YEAR<=2008]$PRES_CAL <- inv.logit(logit(BAS[YEAR<=2008]$PRES)-GCF.PRES)
BAS$ABUNDANCE_CAL        <- BAS$PRES_CAL*exp(BAS$POS_CAL)

ggplot(data=BAS,aes(x=YEAR))+geom_line(aes(y=ABUNDANCE),col="red")+geom_line(aes(y=ABUNDANCE_CAL),col="blue")+theme_bw()


Results <- data.table(YEAR=unique(UVS$YEAR))
ISLANDGROUPLIST <- unique(UVS$ISLANDGROUP)
for (i in 2:500){
  
  aDataset <- data.table(  ddply(UVS,.(YEAR,ISLANDGROUP),function(x) x[sample(nrow(x),replace=T),] )  )
  
  aPosData  <- aDataset[PRES==1,list(POS=mean(log(ABUNDANCE))),by=list(YEAR,ISLANDGROUP,WEIGHTS)]
  aPosData  <- aPosData[,list(POS=weighted.mean(POS,WEIGHTS)),by=list(YEAR)]
  
  aPresData <- aDataset[,list(PRES=mean(PRES)),by=list(YEAR,ISLANDGROUP,WEIGHTS)]
  aPresData <- aPresData[,list(PRES=weighted.mean(PRES,WEIGHTS)),by=list(YEAR)]
  
  aPosData[YEAR<=2008]$POS   <- aPosData[YEAR<=2008]$POS/GCF.POS
  aPresData[YEAR<=2008]$PRES <- inv.logit(logit(aPresData[YEAR<=2008]$PRES)-GCF.PRES)

  aFinal           <- merge(aPosData,aPresData)
  aFinal$ABUNDANCE <- exp(aFinal$POS)*aFinal$PRES
  
  Results <- cbind(Results,aFinal$ABUNDANCE)
  
  if(i%%10==0)print(i)
}

Final      <- data.table(YEAR=Results$YEAR)
Final$SD   <- apply(Results[,2:ncol(Results)],1,sd)

Final <- merge(BAS,Final,by="YEAR")
Final$CV   <- Final$SD/Final$ABUNDANCE_CAL

ggplot(data=Final,aes(x=YEAR,y=ABUNDANCE_CAL))+geom_errorbar(aes(ymin=ABUNDANCE_CAL-SD,ymax=ABUNDANCE_CAL+SD))+geom_line()+theme_bw()

Final <- select(Final[YEAR<=2018],YEAR,MEAN=ABUNDANCE_CAL,CV)
write.csv(Final,file="C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\Outputs\\SS3 inputs\\Final_DIVER.csv")


