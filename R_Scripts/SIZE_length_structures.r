# This script explores and processes length data.
require(data.table); require(parallel); require(dplyr); require(ggplot2); require(readxl); require(SDMTools); require(TMB.LBSPR); require(plyr)

# Read data in
LH <- data.table( read_excel("Data/LifeHistory.xlsx") )
L  <- readRDS("Outputs/CATCH_processed.rds")
L  <- L[SPECIES==20]
L  <- L[GEAR_A=="DEEP_HANDLINE"]
BIN_SIZE     <- 25 # in mm

# Obtain mean weight and mean length per trip
L           <- L[NUM!=0&LBS!=0] # Filter missing LBS and NUM
L$WEIGHT_G  <- L$LBS/L$NUM*453.592 # Calculate mean weight per trip and convert to grams 
L$LENGTH_MM <- (L$WEIGHT_G/LH[PAR=="LW_A"]$MEAN)^(1/LH[PAR=="LW_B"]$MEAN)*10  
L$LENGTH_MM <- round(L$LENGTH_MM,0)
L$LENGTH_BIN <- L$LENGTH_MM-(L$LENGTH_MM%%BIN_SIZE)+BIN_SIZE # Add length bin endpoints to dataset

# Filter records with mean lengths that are impossibly large
L <- L[LENGTH_MM<=LH[PAR=="AbsMax"]$MEAN]

# Data processing steps
L <- L[AREA_C=="MauiNui"|AREA_C=="Kauai"|AREA_C=="Oahu"|AREA_C=="Hawaii"|AREA_C=="Penguin"]
L[AREA_C=="Penguin"]$AREA_C <- "MauiNui" # Merge Penguin Bank with Maui Nui (sample size issue in early years)

# Regional weights to be use when assigning effective sample size
W     <- data.table(   dplyr::filter(LH,grepl("Weight",LH$PAR))   )
W     <- dplyr::select(W,PAR,MEAN)
W$PAR <- substr(W$PAR,8,14)
setnames(W,c("PAR","MEAN"),c("AREA_C","REGION.WEIGHT"))
L     <- merge(L,W,by="AREA_C",all.x=T)

# Sample size by year and region
SAMPSIZE      <- L[NUM==1&GEAR_A=="DEEP_HANDLINE",list(N=.N),by=list(FYEAR,AREA_C,REGION.WEIGHT)]
SAMPSIZE$EFFN <- SAMPSIZE$N*SAMPSIZE$REGION.WEIGHT
SAMPSIZE      <- SAMPSIZE[,list(EFFN=sum(EFFN)),by=list(FYEAR)]
plot(SAMPSIZE$FYEAR,SAMPSIZE$EFFN)

# Calculate abundance-at-length
L <- L[NUM==1,list(NUM=sum(NUM)),by=list(FYEAR,LENGTH_BIN)]
L <- L[order(FYEAR,LENGTH_BIN)]

L <- dcast.data.table(L,FYEAR~LENGTH_BIN,value.var="NUM",fill=0)

L <- merge(L,SAMPSIZE,by="FYEAR")
L <- select(L,FYEAR,EFFN,3:ncol(L))

write.csv(L,"Outputs/SS3 inputs/Size_structure_year.csv",row.names=F)
