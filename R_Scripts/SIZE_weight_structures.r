# This script explores and processes length data.
require(data.table); require(parallel); require(dplyr); require(ggplot2); require(readxl); require(SDMTools); require(TMB.LBSPR); require(plyr)

Gear.name <- c("DEEP_HANDLINE","INSHORE_HANDLINE","TROLLING")[3]

# Read data in
LH <- data.table( read_excel("Data/LifeHistory.xlsx") )
LWA  <- LH[PAR=="LW_A"]$Value
LWB  <- LH[PAR=="LW_B"]$Value
MAXW <- LWA*(LH[PAR=="AbsMax"]$Value/10)^LWB*2.20462
W  <- readRDS("Outputs/CATCH_processed.rds")
W  <- W[SPECIES==20]
W  <- W[GEAR_A==Gear.name]
BIN_SIZE  <- 1 # in lbs

# Obtain mean weight and mean length per trip
W         <- W[NUM!=0&LBS!=0] # Filter missing LBS and NUM
W$MW_LBS  <- W$LBS/W$NUM
BINS      <- seq(BIN_SIZE/2,ceiling(MAXW)-BIN_SIZE,by=BIN_SIZE)
BINS      <- cbind(BINS,seq(1,32,by=1))
W         <- W[MW_LBS>0.5&MW_LBS<=MAXW]

W$WEIGHT_BIN_START <- round(W$MW_LBS)-0.5 # Add length bin start point to dataset

# Data processing steps
W <- W[AREA_C=="MauiNui"|AREA_C=="Kauai"|AREA_C=="Oahu"|AREA_C=="Hawaii"|AREA_C=="Penguin"]
W[AREA_C=="Penguin"]$AREA_C <- "MauiNui" # Merge Penguin Bank with Maui Nui (sample size issue in early years)

# Regional weights to be use when assigning effective sample size
RW     <- data.table(   dplyr::filter(LH,grepl("Weight",LH$PAR))   )
RW     <- dplyr::select(RW,PAR,Value)
RW$PAR <- substr(RW$PAR,8,14)
setnames(RW,c("PAR","Value"),c("AREA_C","REGION.WEIGHT"))
W      <- merge(W,RW,by="AREA_C",all.x=T)

# Effective Sample size by year
SAMPSIZE      <- W[NUM==1,list(N=.N),by=list(FYEAR,AREA_C,REGION.WEIGHT)]
SAMPSIZE$EFFN <- SAMPSIZE$N*SAMPSIZE$REGION.WEIGHT
SAMPSIZE      <- SAMPSIZE[,list(EFFN=sum(EFFN)),by=list(FYEAR)]
plot(SAMPSIZE$FYEAR,SAMPSIZE$EFFN)

# Calculate abundance-at-length
W <- W[NUM==1,list(NUM=sum(NUM)),by=list(FYEAR,WEIGHT_BIN_START)]
W <- W[order(FYEAR,WEIGHT_BIN_START)]

# Conver to kg
W$WEIGHT_BIN_START_KG <- W$WEIGHT_BIN_START*0.453592

W <- dcast.data.table(W,FYEAR~WEIGHT_BIN_START_KG,value.var="NUM",fill=0)

W <- merge(W,SAMPSIZE,by="FYEAR")
W <- select(W,FYEAR,EFFN,2:ncol(W))

write.csv(W,paste0("Outputs/SS3 inputs/Weight_structure_",Gear.name,"_year.csv"),row.names=F)

          