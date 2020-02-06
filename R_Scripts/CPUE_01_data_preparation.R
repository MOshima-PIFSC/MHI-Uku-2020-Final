#Purpose: Setup dataset for CPUE analysis for uku assessment. 
#By: Marc Nadon Nadon, based heavily on Brian Langseth's script for the Deep7 assessment
#Created: 3/1/2019
#Last updated: 4/19/2019

#Follows: CATCH_data_preparation.R (need to run first, to generate "CATCH_processed.rds")

#Supporting files:
# BF_Area_Grid_Reggie.csv (valid MHI grids)
# port_comport_table.csv (assigns ports to common ports)
# comport_area_distance_table.csv (dist between common ports and grids)
# FI to HDAR grid_priortoFY16.csv (location of fishers for survey)
# MHI_Fishchart2007110807_gridpts.dbf (midpoints of management grids)
# getWind.R (code to assign downloaded wind data to records)
# oceanwindsdly_X.NC where X is for separate files 1 to 28 (wind data)

#Input: Dataset output from CATCH_processed.R, which processes file sent by P. Tao
#       with MHI areas, fisher names and fishing years 1948-2018.

#Output: Best available dataset for CPUE time series for uku models.

require(data.table); require(parallel); require(dplyr); require(foreign); require(plyr)

A <- readRDS("Outputs/CATCH_processed.rds")

# Select the gear to be used for CPUE analyses

Gear.name <- c("DEEP_HANDLINE","INSHORE_HANDLINE","TROLLING")[1]
add.wind         <- T           # Remove windspeed calculations to reduce processing time. This can take several hours depending on the selected gear.

# Sum LBS in the dataset so that species are only mentioned once per record
A <- A[,list(LBS=sum(LBS)),by=list(DATE,MONTH,FYEAR,LICENSE,AREA,AREA_A,AREA_B,AREA_C,AREA_D,GEAR,GEAR_A,HOURS,PORT_LAND,FISHER,SPECIES)]

# Select records
A <- A[GEAR_A==Gear.name]        # Select only deep-sea handline
A <- A[AREA_D=="MHI"]  # Only Main 8 data (disregard NWHI and Invalid data)
A <- A[LICENSE>0]      # Remove license 0s

# Create a list of trips "TL", add some information, and provide a trip number to "A"
TL      <- unique(A[,c("DATE","LICENSE")])
TL$TRIP <- c(1:nrow(TL))  # Assign trip number
A       <- merge(A,TL,by=c("DATE","LICENSE"))

A[FYEAR==1976]$FISHER <- "Fisher.1976" # There was no available fisher name for all 1976 records. We simply give them the name Fisher1976.
A       <- A[FISHER!="na.na"] # Remove records with no fisher info

#=====STEP 1: Remove some trips where uku was likely not targeted, for dates before Sep. 2002==========
# Get catch infor by trip
TRIP_UKU  <- A[SPECIES==20,list(UKU_LBS=sum(LBS)),by=list(TRIP)]
TRIP_PMUS <- A[SPECIES%in%c(1:14,106,107,108,320,321,323,324,39,118),list(PMUS_LBS=sum(LBS)),by=list(TRIP)] 
TRIP_D7   <- A[SPECIES%in%c(15,17,19,21,22,36,58,97),list(D7_LBS=sum(LBS)),by=list(TRIP)] 
TRIP_S0   <- A[SPECIES==0,list(S0_LBS=sum(LBS)),by=list(TRIP)] # Records with catch of species code 0

TL <- merge(TL,TRIP_UKU,by="TRIP",all.x=T)
TL <- merge(TL,TRIP_PMUS,by="TRIP",all.x=T)
TL <- merge(TL,TRIP_D7,by="TRIP",all.x=T)
TL <- merge(TL,TRIP_S0,by="TRIP",all.x=T)
TL[is.na(TL)] <- 0

# Remove trips where no uku were caught, but they caught PMUS species, D7, or species=0 from Sept. 2002 back
TRIP_KEEP  <- TL[!(UKU_LBS==0&(PMUS_LBS!=0|D7_LBS!=0|S0_LBS!=0))]$TRIP
#TRIP_KEEP <- TL[!(UKU_LBS==0&(PMUS_LBS!=0|S0_LBS!=0))]$TRIP # Keep the trip that caught D7 with no uku
A          <- A[TRIP%in%TRIP_KEEP|DATE>="2003-01-01"] # Remove those trips, but just for the early data years

# Kona/South Point: Remove trips that caught any PMUS species while catching <50lbs of uku in FY1985 and before
BI_D_UKU  <- A[AREA%in%c(108,100,101,102,128,120,121,122)&SPECIES==20&FYEAR<=1985,list(UKU_LBS=sum(LBS)),by=list(TRIP)]  # DAR grids off Kona and South Point that caught uku
BI_D_PMUS <- A[AREA%in%c(108,100,101,102,128,120,121,122)&SPECIES%in%c(1:14,106,107,108,320,321,323,324,39,118)&FYEAR<=1985,list(PMUS_LBS=sum(LBS)),by=list(TRIP)]
BI_D      <- merge(BI_D_UKU,BI_D_PMUS,by="TRIP",all=T)
BI_D[is.na(BI_D)] <- 0

BI_D_KEEP <- BI_D[PMUS_LBS>0&UKU_LBS<50]$TRIP
A         <- A[!TRIP%in%BI_D_KEEP] # Remove those trips

# Remove records from fishers that only report catch on the first and last day of the month (for records prior to Sep. 2002)
MD         <- table(substr(A[DATE<"2003-01-01"]$DATE,6,10))
firstindex <- c(1,32,61,92,122,153,183,214,245,275,306,336)   #first of each month
lastindex  <- c(31,59,91,121,152,182,213,244,274,305,335,366) #end of each month
FL         <- c(names(MD[lastindex]),names(MD[firstindex])) #first and last dates of the month

# Explore the percent of reports that occur at the beginning and end of the month per fisher
A$MONTHDAY                     <- substr(A$DATE,6,10)
A$FIRSTLASTDAY                 <- 0 
A[MONTHDAY%in%FL]$FIRSTLASTDAY <- 1

FISHER_FLD  <- A[DATE<"2003-01-01",list(FLD=sum(FIRSTLASTDAY)),by=list(FISHER)] # No of records on first and last day of the month
FISHER_TOT  <- A[DATE<"2003-01-01",list(NUMREC=.N),by=list(FISHER)]
FISHER_INFO <- merge(FISHER_FLD,FISHER_TOT,by="FISHER")
FISHER_INFO$PROP_FLD <- FISHER_INFO$FLD/FISHER_INFO$NUMREC
FISHER_INFO <- FISHER_INFO[order(-PROP_FLD)]
hist(FISHER_INFO$PROP_FLD)
sum(FISHER_INFO[PROP_FLD>0.95]$NUMREC) # 965 Records from fishers that report catch on 1st/last of the month >95% of the time

# Explore records from fisher that never caught uku
FISHER_UKU  <- A[SPECIES==20,list(UKU_LBS=sum(LBS)),by=list(FISHER)] # No of records on first and last day of the month
FISHER_INFO <- merge(FISHER_INFO,FISHER_UKU,by="FISHER",all.x=T)

# Select fishers to keep
FISHER_KEEP <- FISHER_INFO[PROP_FLD<0.95&!is.na(UKU_LBS)]$FISHER
A <- A[FISHER%in%FISHER_KEEP|DATE>="2003-01-01"]  # Filter those fishers out (while making sure to keep the newer segment of the data)

# Remove fishing independent survey trips 
FI        <- read.csv("Data\\FI to HDAR grid_priortoFY16.csv",header=T)
FI$DATE   <- as.Date(FI$Date,format="%m/%d/%Y")
FI$FISHER <- tolower(paste(FI$first,FI$last,sep="."))
FI$ID     <- paste(FI$DATE,FI$HDAR.Grid,FI$FISHER)

A <- A[!paste(DATE,AREA,FISHER)%in%FI$ID] # Removed 98 records


#=====STEP 2: Fix effort information (Days before Sept. 2002, Hours afterwards)==========

#Read in distance values - These are files provide by Jerry Ault and Steve Smith from U of Miami 
DIST  <- data.table(  read.csv("Data\\comport_area_distance_table.csv",header=T,col.names=c("COM_PORT","AREA","SUBAREA","DISTANCE")) )# Distance between common ports and various areas
DIST$AREA  <- as.character(DIST$AREA)
DIST[SUBAREA!="Z"]$AREA  <- paste0(DIST[SUBAREA!="Z"]$AREA,DIST[SUBAREA!="Z"]$SUBAREA)
DIST       <- dplyr::select(DIST,AREA,COM_PORT,DISTANCE)

PORT  <- read.csv("Data\\port_comport_table.csv",header=T,col.names=c("PORT_LAND","COM_PORT")) # Port to common port assignment

# Some changes made by Jerry and Steve
PORT_CHANGE            <- matrix(c(101,182,201,291,427,411),3,2,byrow=T)
colnames(PORT_CHANGE)  <- c("PORT_LAND","COM_PORT")
PORT                   <- rbind(PORT_CHANGE,PORT)

B <- merge(A[DATE<"2003-01-01",],PORT,by="PORT_LAND",all.x=T) # Only need to caculate this for records before 2003 since after effort is recorded as hours.
table(B$COM_PORT,useNA="always") # 7067 records don't have ports
B <-B[!is.na(COM_PORT)]          # Filter them out
 
# Assign number of known common ports visited and number of areas visited on a trip
NUM_AREAS <- B[,list(NUM_AREAS=length(unique(AREA))),by=list(TRIP)]      # 2451 out of 251,287 trips visit >1 area
NUM_PORTS <- B[,list(NUM_PORTS=length(unique(COM_PORT))),by=list(TRIP)]  # 288 out of 251,287 trips visit >1 com_port

NUM_INFO <- merge(NUM_AREAS,NUM_PORTS,by="TRIP")
nrow(NUM_INFO[NUM_AREAS>1&NUM_PORTS>1])  # Only 18 trips have > 1 port and > 1 area reported.
nrow(NUM_INFO[NUM_AREAS==1&NUM_PORTS>1]) # Only 38 trips have more than 1 port when 1 area reported.
nrow(NUM_INFO[NUM_AREAS>1&NUM_PORTS==1]) # Only 817 trips have 1 port and > 1 area reported.

# Given the low number of trips wih >1 port, we simply filter them out of the dataset
B    <- merge(B,NUM_PORTS[NUM_PORTS==1],by="TRIP")
B    <- merge(B,DIST,by=c("AREA","COM_PORT"),all.x=T) 

B[is.na(DISTANCE)]$DISTANCE <- 1  # Records before 1948-07-01 don't have PORT_LAND and thus no DISTANCE value. Insert "1" as a default.

# Trips with >1 area, need to take greatest distance as unique distance
temp       <- B[,list(DISTANCE=max(DISTANCE)),by=list(TRIP)]
B$DISTANCE <- NULL
B          <- merge(B,temp,by="TRIP")

# Now determine distance cutoff for the definition of 1 fishing day
DT <- unique(B[,c("TRIP","FYEAR","DISTANCE")])
hist(DT[FYEAR<1955]$DISTANCE,breaks=50,main="<1955",xlim=c(0,300),xlab="Distances by trip (nm)")
hist(DT[FYEAR>=1955 & FYEAR<1960]$DISTANCE,breaks=50,main="1955-1959",xlim=c(0,300),xlab="Distances by trip (nm)")
hist(DT[FYEAR<1960]$DISTANCE,breaks=50,main="<1960",xlim=c(0,300),xlab="Distances by trip (nm)")
hist(DT[FYEAR>=1960 & FYEAR<1970]$DISTANCE,breaks=40,main="1960-1969",xlim=c(0,300),xlab="Distances by trip (nm)")
hist(DT[FYEAR>=1970 & FYEAR<1980]$DISTANCE,breaks=50,main="1970-1979",xlim=c(0,300),xlab="Distances by trip (nm)")
hist(DT[FYEAR>=1980 & FYEAR<1990]$DISTANCE,breaks=50,main="1980-1989",xlim=c(0,300),xlab="Distances by trip (nm)")
hist(DT[FYEAR>=1990 & FYEAR<2000]$DISTANCE,breaks=50,main="1990-1999",xlim=c(0,300),xlab="Distances by trip (nm)")
hist(DT[FYEAR>=2000 & FYEAR<2003]$DISTANCE,breaks=50,main="2000-2002",xlim=c(0,300),xlab="Distances by trip (nm)")
#Looks like 30nm is good cutoff in general. 20nm could be used for <1960 as 15nm seems too small but 30nm is good too. 
#Use 30nm for cutoff after 1960. Its inclusive of more data, and minimizes the amount of changes we make. There also is a break in the histograms at this value.

B$DAYS <- ceiling(B$DISTANCE/30) # Assign effort as number of fishing days

# Recombine with recent data
A$COM_PORT=A$DISTANCE=A$DAYS=A$NUM_PORTS=NA
A <- rbind(A[DATE>="2003-01-01"],B)
A <- dplyr::select(A,TRIP,DAYS,DATE,MONTH,LICENSE,FYEAR,AREA,AREA_A,AREA_B,AREA_C,GEAR,HOURS,FISHER,SPECIES,LBS)
A <- A[order(DATE,FISHER,SPECIES,LBS)]

#======STEP 4: Factors for CPUE standardization=====================

# Add a measure of experience to each trip (cumulative number of trips)
EXP         <- unique(dplyr::select(A,"FISHER","DATE","TRIP"))
EXP         <- EXP[order(FISHER,DATE,TRIP)]
EXP$CUM_EXP <- ave(EXP$TRIP, EXP$FISHER, FUN = seq_along)
A           <- merge(A,EXP,by=c("FISHER","DATE","TRIP"))

#Read in locations of management grids
AREA_LOC     <- data.table( read.dbf("Data\\Fishchart2007110807_gridpts.dbf",as.is=T) )
AREA_VALID   <- unique(A$AREA)
AREA_LOC     <- AREA_LOC[AREA_ID%in%AREA_VALID]     # Valid areas only
AREA_LOC     <- AREA_LOC[AREA_A=="C"|is.na(AREA_A)] # Only keep subarea "C" for the 16123 grid to avoid duplicates
AREA_LOC     <- AREA_LOC[Shape_Le_1>0.05]           # Remove some duplicated locations that can be IDed by their very small shape length.

# Add wind speed/direction, on a daily basis over 0.25 degree grids starting 1987-07-09. Use getWind function which is in getWind.R
if(add.wind==T){


source("Data\\wind\\getWind.R")
TEMP     <- A[DATE>="2003-01-01"] # Can only use wind for the 2nd CPUE time series that start in Oct. 2002
U_TRIPS  <- unique(TEMP$TRIP)

# Create a list of the data to be inputed in parralel function "Wind_parralel". Note: this takes about 5 minutes.
WIND_INPUT <- split(TEMP[,c("AREA","DATE","TRIP")],TEMP$TRIP)

# Function to be passed in parLapply function
Wind_Parallel <- function(TEMP_DATA){
  
  WIND        <- getWind(TEMP_DATA,AREA_LOC)
  WIND$TRIP   <- TEMP_DATA$TRIP[1]
  return(WIND)
}

test <- WIND_INPUT[1:500] # Small subset of the data to estimate how long parallel processing will take.

# Parallel processing steps
cl <- makeCluster(detectCores()-1)  
clusterEvalQ(cl,require(chron))
clusterEvalQ(cl,require(RNetCDF))
clusterEvalQ(cl,source("Data/wind/getWind.R"))
clusterExport(cl=cl, varlist=c("AREA_LOC"))

# Test run to estimate how long this step will take
start<-proc.time()[3]; OUT <- parLapply(cl,test,Wind_Parallel); print(paste("Estimated time:",round((proc.time()[3]-start)/60*length(U_TRIPS)/500,0),"minutes"))
# Actual run
start<-proc.time()[3]; OUT <- parLapply(cl,WIND_INPUT,Wind_Parallel);  print(paste("Time taken:",round((proc.time()[3]-start)/60,0),"minutes"))
stopCluster(cl)

TEMP_OUT <- data.table(  rbindlist(OUT)  )
TEMP     <- merge(TEMP,TEMP_OUT,by=c("TRIP","AREA"))

#Relink with old (prior to wind data) records and form updated dataset
OLD  <- A[DATE<"2003-01-01"]
OLD[,c("speed","xdir","ydir")] <- NA

A <- rbind(OLD,TEMP)
setnames(A,c("speed","xdir","ydir"),c("SPEED","XDIR","YDIR"))
#A <- A[DATE<"2015-01-01"] # Remove latest year of data since I don't have the wind data for this yet.

#Give lat/long of all AREA for future needs
AREA_LOC$AREA_ID <- as.character(AREA_LOC$AREA_ID)
AREA_LOC         <- dplyr::select(AREA_LOC,AREA_ID,LONG=POINT_X,LAT=POINT_Y)
A                <- merge(A,AREA_LOC,by.x="AREA",by.y="AREA_ID",all.x=T)


# When wind information is not available, the getwind function returns AREA (for some reason). These area value need to be changed to NA
A$SPEED <- as.numeric(A$SPEED)
A[SPEED>50]$SPEED <- NA 

A <- dplyr::select(A,FYEAR,MONTH,DATE,TRIP,FISHER,CUM_EXP,LICENSE,LAT,LONG,AREA,AREA_A,AREA_B,AREA_C,GEAR,HOURS,DAYS,SPEED,XDIR,YDIR,SPECIES,LBS)

} # END OF IF-GETWIND loop

if(add.wind==F){ 
A <- dplyr::select(A,FYEAR,MONTH,DATE,TRIP,FISHER,CUM_EXP,LICENSE,LAT,LONG,AREA,AREA_A,AREA_B,AREA_C,GEAR,HOURS,DAYS,SPECIES,LBS)
}

A <- A[order(DATE,FISHER,SPECIES,LBS)]

saveRDS(A,paste0("Outputs/CPUE_",Gear.name,"_StepA.rds")) # OUTPUT: Save a first step of the CPUE dataset

#========Step 5: Final processing of the data====================
require(data.table); require (dplyr); require(parallel); require(foreign)

B <- readRDS(paste0("Outputs/CPUE_",Gear.name,"_StepA.rds"))
# Collapse records reporting species-specific catch multiple time within the same trip
if(add.wind==T) B <- B[,list(LBS=sum(LBS)),by=list(FYEAR,MONTH,DATE,TRIP,FISHER,CUM_EXP,LICENSE,LAT,LONG,AREA,AREA_A,AREA_B,AREA_C,GEAR,HOURS,DAYS,SPEED,XDIR,YDIR,SPECIES)]
if(add.wind==F) B <- B[,list(LBS=sum(LBS)),by=list(FYEAR,MONTH,DATE,TRIP,FISHER,CUM_EXP,LICENSE,LAT,LONG,AREA,AREA_A,AREA_B,AREA_C,GEAR,HOURS,DAYS,SPECIES)]

#Determine individual trips that have different hour values reported and split those in seperate trips
RD         <- B[DATE>="2003-01-01"]
MH         <- unique(dplyr::select(RD,TRIP,HOURS))
MH         <- MH[order(TRIP,HOURS)]
MH$SUBTRIP <- ave(MH$HOURS,MH$TRIP,FUN=seq_along) # Add a subtrip column that differentiates trips with multiple hour values
RD         <- merge(RD,MH,by=c("TRIP","HOURS"))
RD$TRIP    <- paste(RD$TRIP,RD$SUBTRIP,sep=".")
RD$SUBTRIP <- NULL
B          <- rbind(B[DATE<"2003-01-01"],RD)     # Merge old and recent data back together
B          <- B[!(HOURS==0 & DATE>="2003-01-01")] # Remove recent data that have HOURS==0

# Assign a single area value for trips with multiple areas, for the recent data (this was done earlier for the old data)
TRIP_INFO  <- B[,list(NUM_AREAS=length(unique(AREA))),by=list(TRIP)]      # 884 out of 117,213 trips visit >1 area
TRIP_INFO  <- TRIP_INFO[order(-NUM_AREAS)]
TRIP_INFO  <- TRIP_INFO[NUM_AREAS>1]

# Calculate Uku lbs for all trips and merge back to main database
UKU_CAUGHT <- B[SPECIES==20,list(UKU_INTRIP=sum(LBS)),by=list(TRIP,AREA)]
B          <- merge(B,UKU_CAUGHT,by=c("TRIP","AREA"),all.x=T)
B[is.na(UKU_INTRIP)]$UKU_INTRIP <- 0

# Calculate area that has most uku in multi-area trips and replace with single area (can take several minutes)
Proc_Trips <- data.table()
for(i in 1:nrow(TRIP_INFO)){
 
  aTrip         <- B[TRIP%in%TRIP_INFO[i]]  # Extract the trip
  aTrip         <- select(aTrip,TRIP,FYEAR:LICENSE,GEAR:DAYS,SPECIES:UKU_INTRIP,AREA,LAT:AREA_C,SPEED:YDIR) # Re-order to make variable selection easier
  aTrip         <- aTrip[order(-UKU_INTRIP,AREA)] # Order records to have top line be highest uku or lowest Area number
  SELECTED      <- aTrip[1,14:22] # Select top trip
  aTrip[,14:22] <- SELECTED       # Apply top trip information to all records
  Proc_Trips    <- rbind(Proc_Trips,aTrip)
  print(paste(i,"of",nrow(TRIP_INFO),"trips"))
}
B <- B[!TRIP%in%TRIP_INFO$TRIP] # Delete trip from original dataset
B <- rbind(B,Proc_Trips)        # Replace deleted trips with new processed one
B <- dplyr::select(B,-UKU_INTRIP)

# Remove records from areas that are likely errors or marginal (i.e. in very deep water). In other words, remove AREA_Bs that are still numeric.
CORE_AREAS <- unique(B$AREA_B)
CORE_AREAS <- CORE_AREAS[is.na(as.numeric(CORE_AREAS))]
B          <- B[AREA_B%in%CORE_AREAS] 

# Check if this step is needed anymore (it has been added to CATCH_data_prep and is done higher up in this script)

if(add.wind==T)  B <- B[,list(LBS=sum(LBS)),by=list(TRIP,FYEAR,MONTH,DATE,FISHER,CUM_EXP,LICENSE,AREA,AREA_A,AREA_B,AREA_C,LAT,LONG,GEAR,HOURS,DAYS,SPEED,XDIR,YDIR,SPECIES)]
if(add.wind==F)  B <- B[,list(LBS=sum(LBS)),by=list(TRIP,FYEAR,MONTH,DATE,FISHER,CUM_EXP,LICENSE,AREA,AREA_A,AREA_B,AREA_C,LAT,LONG,GEAR,HOURS,DAYS,SPECIES)]

#Calculate CPUE. For records < "2002-10-01" effort is number of single-reporting days (dayeffort), for records >= "2002-10-01" 
#effort is reported hours (houreffort). However, the dataset is seperate at 2003 to make this transition cleaner. Catch is lbs of uku (ukucatch).
B$CPUE                     <- numeric()
B[DATE<"2003-01-01"]$CPUE  <- B[DATE<"2003-01-01"]$LBS/B[DATE<"2003-01-01"]$DAYS
B[DATE>="2003-01-01"]$CPUE <- B[DATE>="2003-01-01"]$LBS/B[DATE>="2003-01-01"]$HOURS

saveRDS(B,paste0("Outputs/CPUE_",Gear.name,"_StepB.rds"))

#======= Calculate uku targeting principal component values (Winker et al. 2013)============
require(data.table); require(dplyr); require(ggplot2); require(ggfortify)
C <- readRDS(paste0("Outputs/CPUE_",Gear.name,"_StepB.rds"))
C$SPECIES <- paste0("S",C$SPECIES)

#Determine which species to include in the xij variables which is 0 or 1 for each trip j, if species i is caught in it. 
#Use those species i making up 99% of the cumulative catch
SP_CATCH           <- C[,list(LBS=sum(LBS)),by=list(SPECIES)]
SP_CATCH           <- SP_CATCH[SPECIES!=999&SPECIES!=20]  # Remove unknown species and uku
SP_CATCH           <- SP_CATCH[order(LBS)]
SP_CATCH$PROP      <- SP_CATCH$LBS/sum(SP_CATCH$LBS)
SP_CATCH$CUMU      <- cumsum(SP_CATCH$LBS)
SP_CATCH$CUMU_PROP <- round(SP_CATCH$CUMU/sum(SP_CATCH$LBS),4)
#SP_REJECT           <- SP_CATCH[CUMU_PROP<0.01] #Drop species after 99% of the cumulative catch
SP_REJECT          <- SP_CATCH[PROP<0.01&SPECIES!="S20"]      #Alternatively, drop species corresponding to less than 1% of total catch (Winker et al. 2013)
No.species         <- length(unique(SP_CATCH[PROP>0.01|SPECIES=="S20"]$SPECIES))

if(add.wind==T) No.variables <- 16
if(add.wind==F) No.variables <- 13 

# Only keep the species making up 99% of the catch and uku (obviously)
if(add.wind==T) D <- dcast(C,TRIP+FYEAR+MONTH+DATE+FISHER+CUM_EXP+AREA+AREA_A+AREA_B+AREA_C+LAT+LONG+GEAR+SPEED+XDIR+YDIR~SPECIES,value.var="CPUE",fun.aggregate=sum)
if(add.wind==F) D <- dcast(C,TRIP+FYEAR+MONTH+DATE+FISHER+CUM_EXP+AREA+AREA_A+AREA_B+AREA_C+LAT+LONG+GEAR~SPECIES,value.var="CPUE",fun.aggregate=sum)

D <- data.table(D)
D <- D[, (SP_REJECT$SPECIES):=NULL]

D <- dplyr::select(D,1:No.variables,S20,(No.variables+1):ncol(D))

# Calculate prop. of each species per trip
#PROPS               <- D[,S20:ncol(D)]/rowSums(D[,S20:ncol(D)]) # Keeps uku in the PCA analyses - uncomment to create dataset for DCA analyses of Winker
PROPS               <- D[,(No.variables+2):ncol(D)]/rowSums(D[,(No.variables+2):ncol(D)])
PROPS[is.na(PROPS)] <- 0 
PROPS               <- (PROPS)^(1/4) # Square-root transformation to increase weight of rarer species
#PROPS               <- PROPS[rowSums(PROPS)>0,]

PCA <- prcomp(PROPS,center=T,scale.=T)
summary(PCA)

autoplot(PCA,loadings=T,loadings.label=T,loadings.label.colour="black",loadings.colour="black", colour="lightgrey")+theme_bw()

# Implement non-graphical Scree test to select best PCs (Winker et al. 2014)
require(nFactors)
eigenvalues <- PCA$sdev^2   # Extracts the observed eigenvalues
nsubjects   <- nrow(PROPS)  # Extracts the number of subjects
variables   <- length(eigenvalues) # Computes the number of variables   
rep         <- 100 # Number of replications for PA analysis
cent        <- 0.95 # Centile value of PA analysis

## PARALLEL ANALYSIS (qevpea for the centile criterion, mevpea for the mean criterion)
aparallel <- parallel(var = variables,
                      subject = nsubjects,
                      rep = rep,
                      cent = cent)$eigen$qevpea  

## NUMBER OF FACTORS RETAINED ACCORDING TO DIFFERENT RULES
results <- nScree(x=eigenvalues, aparallel=aparallel)
NF      <- results$Analysis
plotnScree(results)

# Number of factors to keep for CPUE standardization model is the min of the Optimal Coordinate and Kaiser rule (Winker 2014)
for(i in 1:variables) if(NF$Eigenvalues[i]>=1&(NF$Eigenvalues[i]>=NF$Pred.eig[i])){PC_KEEP<-i}else{break} 

# Add first four principal components back in dataset
E <- cbind(D[,1:S20],PCA$x[,1:4],PC_KEEP)

# Save this record
if(add.wind==T) E <- dplyr::select(E,TRIP,FYEAR,MONTH,DATE,FISHER,CUM_EXP,LAT,LONG,SPEED,XDIR,YDIR,AREA,AREA_A,AREA_B,AREA_C,PC1,PC2,PC3,PC4,PC_KEEP,UKUCPUE=S20)
if(add.wind==F) E <- dplyr::select(E,TRIP,FYEAR,MONTH,DATE,FISHER,CUM_EXP,LAT,LONG,AREA,AREA_A,AREA_B,AREA_C,PC1,PC2,PC3,PC4,PC_KEEP,UKUCPUE=S20)

E <- E[order(DATE,AREA_C,TRIP)]

saveRDS(E,paste0("Outputs/CPUE_",Gear.name,"_StepC.rds"))
