# This script explores and processes length data.
require(data.table); require(parallel); require(dplyr); require(ggplot2); require(readxl); require(SDMTools); require(TMB.LBSPR); require(plyr)

# Read data in
LH <- data.table( read_excel("Data/LifeHistory.xlsx") )
L  <- readRDS("Outputs/CATCH_processed.rds")

# Obtain mean weight and mean length per trip
L           <- L[NUM!=0&LBS!=0] # Filter missing LBS and NUM
L$WEIGHT_G  <- L$LBS/L$NUM*453.592 # Calculate mean weight per trip and convert to grams 
L$LENGTH_MM <- (L$WEIGHT_G/LH[PAR=="LW_A"]$Value)^(1/LH[PAR=="LW_B"]$Value)
L$LENGTH_MM <- round(L$LENGTH_MM,0)

# Add selectivity (derived from LBSPR model fit, see last section)
LS50  <- 510
LS95  <- 700
L$SEL <- 1/(  1+exp(-log(19)*(L$LENGTH_MM-LS50)/(LS95-LS50))  ) 
L$NUM.SEL <- L$NUM*L$SEL

# Filter records with mean length that are impossibly large
L <- L[LENGTH_MM<=LH[PAR=="AbsMax"]$Value]

# Data processing steps
L <- L[SPECIES==20]
L <- L[AREA_C=="MauiNui"|AREA_C=="Kauai"|AREA_C=="Oahu"|AREA_C=="Hawaii"|AREA_C=="Penguin"|AREA_C=="NWHI"]

# Check patterns in mean weight by sector
sec.graph <- list()
area.list <- unique(L$AREA_C)
area.list <- area.list[area.list!="NWHI"]
for(ar in 1:5){
 
 aDataset   <- L[AREA_C==area.list[ar]&(GEAR_A=="DEEP_HANDLINE")&NUM<=1]
 aDataset   <- aDataset[,list(LENGTH_MM=mean(LENGTH_MM),WEIGHT_G=mean(WEIGHT_G),N=.N),by=list(FYEAR)]
 aDataset   <- aDataset[N>=20]   # Insure a minimum of length reports per region
 
 sec.graph[[ar]] <- ggplot(aDataset,aes(x=FYEAR,y=WEIGHT_G/1000))+geom_point()+geom_smooth(span=0.4)+
    ggtitle(paste0("Mean length in sector ",area.list[ar]))+
    theme_bw()  # Check GEAR_A by AREA_C for patterns
 filename <- file.path("Outputs/Graphs/Body",paste0("LFIGSECT",formatC(ar,width=2,flag="0"),".tiff"))
 ggsave(sec.graph[[ar]], file=filename, width = 14, height = 8, units = "cm",dpi=150) 
}

# For rest of analyses, combined Penguin into MauiNui
L[AREA_C=="Penguin"]$AREA_C <- "MauiNui" # Merge Penguin Bank with Maui Nui (sample size issue in early years)

# Regional weights to be use when merging all regions together
W     <- data.table(   dplyr::filter(LH,grepl("Weight",LH$PAR))   )
W     <- dplyr::select(W,PAR,Value)
W$PAR <- substr(W$PAR,8,14)
setnames(W,c("PAR","Value"),c("AREA_C","REGION.WEIGHT"))
W <- rbind(W,list("NWHI",1)) # Add NWHI
L     <- merge(L,W,by="AREA_C",all.x=T)

# Sample size by year and region
SMRY <- L[NUM==1&GEAR_A=="DEEP_HANDLINE"&AREA_C!="NWHI",list(N=.N),by=list(FYEAR,AREA_C)]
SMRY <- dcast(SMRY,FYEAR~AREA_C,value.var="N",fill=0)
SMRY$TOT <- rowSums(SMRY[,2:5])
write.csv(SMRY,file="Outputs/Graphs/Body/Sample size DSH.csv")

SMRY <- L[NUM==1&GEAR_A=="INSHORE_HANDLINE"&AREA_C!="NWHI",list(N=.N),by=list(FYEAR,AREA_C)]
SMRY <- dcast(SMRY,FYEAR~AREA_C,value.var="N",fill=0)
SMRY$TOT <- rowSums(SMRY[,2:5])
write.csv(SMRY,file="Outputs/Graphs/Body/Sample size ISH.csv")

SMRY <- L[NUM==1&GEAR_A=="TROLLING"&AREA_C!="NWHI",list(N=.N),by=list(FYEAR,AREA_C)]
SMRY <- dcast(SMRY,FYEAR~AREA_C,value.var="N",fill=0)
SMRY$TOT <- rowSums(SMRY[,2:5])
write.csv(SMRY,file="Outputs/Graphs/Body/Sample size TROLLING.csv")

# Distribution of mean length reported by individual trips (note: not a true size structure)
LGR1 <- ggplot(L[(GEAR_A=="DEEP_HANDLINE"|GEAR_A=="INSHORE_HANDLINE"|GEAR_A=="TROLLING")&AREA_C!="NWHI"],aes(x=LENGTH_MM))+geom_histogram()+facet_grid(rows=vars(GEAR_A),cols=vars(AREA_C))+theme_bw()  # Check GEAR_A by AREA_C for patterns
LGR2 <- ggplot(L[(GEAR_A=="DEEP_HANDLINE"|GEAR_A=="INSHORE_HANDLINE"|GEAR_A=="TROLLING")&AREA_C!="NWHI"],aes(x=LENGTH_MM))+geom_density()+facet_grid(rows=vars(GEAR_A),cols=vars(AREA_C))+theme_bw()  # Check GEAR_A by AREA_C for patterns
LGR3 <- ggplot(L[NUM==1&GEAR_A=="DEEP_HANDLINE"&AREA_C!="NWHI"],aes(x=LENGTH_MM))+geom_density()+facet_grid(rows=vars(AREA_C))+theme_bw()            # Check Region patterns
LGR4 <- ggplot(L[NUM==1&GEAR_A=="DEEP_HANDLINE"&AREA_C!="NWHI"],aes(x=LENGTH_MM))+geom_histogram()+facet_grid(rows=vars(AREA_C))+theme_bw()            # Check Region patterns

SC <- L[NUM==1&GEAR_A=="DEEP_HANDLINE"&AREA_C!="NWHI",list(NUM=sum(NUM)),by=list(FYEAR,AREA_C,WEIGHT_G)]
SC <- SC[,list(WEIGHT_KG=mean(WEIGHT_G/1000),NUM=sum(NUM)),by=list(FYEAR,AREA_C)]
SC <- SC[NUM>=20]

LGR5 <- ggplot(data=SC,aes(x=FYEAR,y=WEIGHT_KG,col=AREA_C))+geom_point()+stat_smooth(span=0.4)+theme_bw()


#=====================Investigate the effect of NUM on mean length
A1 <- L[AREA_C!="NWHI"&(GEAR_A=="DEEP_HANDLINE")&NUM==1,list(MW=weighted.mean(WEIGHT_G,w=NUM.SEL*REGION.WEIGHT)/1000,COUNT=.N),by=list(FYEAR)]
A2 <- L[AREA_C!="NWHI"&(GEAR_A=="DEEP_HANDLINE")&NUM<=5,list(MW=weighted.mean(WEIGHT_G,w=NUM.SEL*REGION.WEIGHT)/1000,COUNT=.N),by=list(FYEAR)]
A3 <- L[AREA_C!="NWHI"&(GEAR_A=="DEEP_HANDLINE")&NUM<=10,list(MW=weighted.mean(WEIGHT_G,w=NUM.SEL*REGION.WEIGHT)/1000,COUNT=.N),by=list(FYEAR)]
A4 <- L[AREA_C!="NWHI"&(GEAR_A=="DEEP_HANDLINE"),list(MW=weighted.mean(WEIGHT_G,w=NUM.SEL*REGION.WEIGHT)/1000,COUNT=.N),by=list(FYEAR)]

LGR6 <- ggplot()+geom_line(data=A1,aes(x=FYEAR,y=MW),col="blue")+geom_line(data=A2,aes(x=FYEAR,y=MW),col="red")+
  geom_line(data=A3,aes(x=FYEAR,y=MW),col="green")+geom_line(data=A4,aes(x=FYEAR,y=MW))+theme_bw()

#==========Mean length and weight trend====================
niter <- 500
MC <- list(niter)
for(i in 1:niter){
  
   aDataset   <- ddply(L[AREA_C!="NWHI"&GEAR_A=="DEEP_HANDLINE"&NUM==1],.(FYEAR,AREA_C),function(x) x[sample(nrow(x),replace=T),] )
   aDataset   <- data.table(aDataset)
   #aDataset   <- aDataset[,list(LENGTH_MM=wt.mean(LENGTH_MM,wt=NUM.SEL),WEIGHT_G=wt.mean(WEIGHT_G,wt=NUM.SEL),N=.N),by=list(FYEAR,AREA_C,REGION.WEIGHT)]
   aDataset   <- aDataset[,list(LENGTH_MM=wt.mean(LENGTH_MM,wt=NUM),WEIGHT_G=wt.mean(WEIGHT_G,wt=NUM),N=.N),by=list(FYEAR,AREA_C,REGION.WEIGHT)]
   aDataset   <- aDataset[N>=30]   # Insure a minimum of 30 length reports per region
   MC[[i]]    <- aDataset[,list(LENGTH_MM=wt.mean(LENGTH_MM,wt=REGION.WEIGHT),WEIGHT_G=wt.mean(WEIGHT_G,wt=REGION.WEIGHT),N=sum(N)),by=list(FYEAR)]
   
   if(i%%100==0) print(i)
}

D   <- rbindlist(MC)
D$N <- as.double(D$N)
D   <- D[,list(ML=mean(LENGTH_MM),ML.SD=sd(LENGTH_MM),MW=mean(WEIGHT_G/1000),MW.SD=sd(WEIGHT_G/1000),N=median(N)),by=list(FYEAR)]

D$MW.CV <- D$MW.SD/D$MW
D       <- dplyr::select(D,FYEAR,N,ML:MW,MW.CV,MW.SD,N)

LGR7 <- ggplot(data=D,aes(x=FYEAR,y=ML))+geom_point()+geom_smooth(span=0.3)+theme_bw()
LGR8 <- ggplot(data=D,aes(x=FYEAR,y=MW))+geom_point()+geom_smooth(span=0.3)+theme_bw()

write.csv(D,"Outputs/SS3 inputs/Final_MW_timeseries.csv",row.names=F)

for(i in 1:8){  
  
  fig      <- paste0("LGR",i) 
  filename <- file.path("Outputs/Graphs/Body",paste0("LFIG",formatC(i,width=2,flag="0"),".tiff"))
  ggsave(eval(parse(text=fig)), file=filename, width = 14, height = 8, units = "cm",dpi=150)  
}

NWHI_OBS <- L[AREA_D=="NWHI",list(ML=wt.mean(LENGTH_MM,wt=NUM.SEL),MW=wt.mean(WEIGHT_G,wt=NUM.SEL)/1000),by=list(AREA_D)]



#=============Generate size structure in different time periods to obtain selectivity parameters using TMB.LBSPR==================
# Create length bins - divide by 2 to get mid-point
BIN_SIZE     <- 40 # in mm
L$LENGTH_BIN <- L$LENGTH_MM-(L$LENGTH_MM%%BIN_SIZE)+BIN_SIZE # Add length bin endpoints to dataset

# Calculate abundance-at-length by Method, AREA_C (with appropriate weights), YEAR
K     <- L[AREA_C!="NWHI"&NUM==1,list(NUM=sum(NUM)),by=list(FYEAR,GEAR_B,AREA_C,REGION.WEIGHT,LENGTH_MM,LENGTH_BIN)]
K     <- K[,list(NUM=sum(NUM)),by=list(FYEAR,AREA_C,GEAR_B,REGION.WEIGHT,LENGTH_MM,LENGTH_BIN)]     # Calculate abundance-at-length by region first
K     <- K[,list(NUM=wt.mean(NUM,wt=REGION.WEIGHT)),by=list(FYEAR,GEAR_B,LENGTH_MM,LENGTH_BIN)]     # Apply regional weighting
K$NUM <- round(K$NUM,0)

# Sum abundance-at-length in the bins
K <- K[,list(NUM=sum(NUM)),by=list(FYEAR,GEAR_B,LENGTH_BIN)]
K <- K[order(FYEAR,GEAR_B,LENGTH_BIN)]

# Pool length data by time period to increase sample size
Period.length <- 10 # in years
K$PERIOD   <- K$FYEAR-(K$FYEAR%%Period.length)+Period.length # Add end year period to dataset
K$PERIOD   <- paste0("P",K$PERIOD)
K          <- K[,list(NUM=sum(NUM)),by=list(GEAR_B,PERIOD,LENGTH_BIN)]
Total_Year <- K[,list(TOT=sum(NUM)),by=list(PERIOD,GEAR_B)]
K          <- merge(K,Total_Year,by=c("PERIOD","GEAR_B"))
K$DENS     <- K$NUM/K$TOT

LGR8 <- ggplot(data=K[GEAR_B=="COM_DEEP_HANDLINE"],aes(x=LENGTH_BIN,y=DENS,col=PERIOD))+geom_line()+theme_bw()
ggsave(LGR8, file="Outputs/Graphs/Body/LFIG08.tiff", width = 14, height = 8, units = "cm",dpi=150)  

LGR9 <- ggplot(data=K[PERIOD=="P2010"],aes(x=LENGTH_BIN,y=DENS,col=GEAR_B))+geom_line()+theme_bw()
ggsave(LGR9, file="Outputs/Graphs/Body/LFIG09.tiff", width = 14, height = 8, units = "cm",dpi=150)  


SEL.TIME   <- data.table(  dcast(K[GEAR_B=="COM_DEEP_HANDLINE"],LENGTH_BIN~PERIOD,value.var="NUM",fill=0)  )

K2         <- K[PERIOD=="P2020",list(NUM=sum(NUM)),by=list(GEAR_B,LENGTH_BIN)]
SEL.GEAR   <- data.table(  dcast(K2,LENGTH_BIN~GEAR_B,value.var="NUM",fill=0)  )


write.csv(SEL.TIME,"Outputs/Size_structure_Time.csv",row.names=F)
write.csv(SEL.GEAR,"Outputs/Size_structure_Gear.csv",row.names=F)

#================================= NWHI analyses

# Calculate abundance-at-length by Method, AREA_C (with appropriate weights), YEAR
BIN_SIZE     <- 40 # in mm
L$LENGTH_BIN <- L$LENGTH_MM-(L$LENGTH_MM%%BIN_SIZE)+BIN_SIZE # Add length bin endpoints to dataset

NW     <- L[AREA_C!="NWHI"&(GEAR_A!="DEEP_HANDLINE")&NUM==1,list(NUM=sum(NUM)),by=list(FYEAR,GEAR_A,AREA_C,LENGTH_MM,LENGTH_BIN)]

# Sum abundance-at-length in the bins
NW <- NW[,list(NUM=sum(NUM)),by=list(FYEAR,LENGTH_BIN)]
NW <- NW[order(FYEAR,LENGTH_BIN)]

# Pool length data by time period to increase sample size
Period.length <- 10 # in years
NW$PERIOD <- NW$FYEAR-(NW$FYEAR%%Period.length)+Period.length/2 # Add year period midpoints to dataset
NW$PERIOD <- paste0("P",NW$PERIOD)
NW        <- NW[,list(NUM=sum(NUM)),by=list(PERIOD,LENGTH_BIN)]

ggplot(data=NW,aes(x=LENGTH_BIN,y=NUM,col=PERIOD))+geom_line()+theme_bw()

NW        <- dcast(NW,LENGTH_BIN~PERIOD,value.var="NUM",fill=0)
NW$ALL    <- rowSums(NW[,P1945:P2005])

#NW        <- NW[8:19,] # Remove last two length bins to help model fit data

write.csv(NW,"Outputs/Size_structure_Others.csv",row.names=F)

# ===============================Run LBSPR model
Model     <- "RARE"
INP       <- data.table(  read_excel("Data/LBSPR.xlsx",sheet=Model)  )

Results   <- run_analyses(INP,paste0("APVI_",Model),
                          n_iteration=500,
                          n_GTG=20,
                          starting=list(Fmort=0.1, LS50=350, LS95=600),
                          ManageF30 =T,
                          ManageLc30=T,
                          NumCores=7,
                          Seed=1)

