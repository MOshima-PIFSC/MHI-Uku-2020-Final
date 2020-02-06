require(data.table);  require(foreign); require(dplyr)

early <- readRDS("Data/HFY48_93E.rds")    # Data set sent to B. Langseth by Paul Tao, which includes his effort to recover fisher names.
late  <- readRDS("Data/HFY1994_2018.rds") # Data sent to M. Nadon on 5/13/2019 by Paul Tao
# Data originally sent as .dbf and csv, respectively. Converted to .rds to save space.

early <- dplyr::select(early,LICENSE,FISHED,AREA,SUBAREA,GEAR,HOURS,PORT_LAND,FNAME,LNAME,SPECIES,NUM_KEPT=CAUGHT,LBS,LBS_SOLD)
late  <- dplyr::select(late,LICENSE=FISHER_LIC_FK,FISHED=FISHED_DATE,AREA=AREA_FK,GEAR=GEAR_FK,HOURS=HOURS_FISHED,PORT_LAND=LANDING_PORT_FK,
               FNAME=FISHER_FNAME,LNAME=FISHER_LNAME,SPECIES=SPECIES_FK,NUM_KEPT,LBS=LBS_KEPT,LBS_SOLD=LBS_SOLD)

early$AREA    <- paste0(early$AREA,early$SUBAREA) # Merge AREA and SUBAREA since recent data merge these fields
early$AREA    <- gsub("NA","",early$AREA)
early$SUBAREA <- NULL

late$FISHED  <- as.Date(late$FISHED)

C       <- rbind(early,late)

#Add a few important fields
C$DATE  <- as.Date(C$FISHED)
C$YEAR  <- year(C$DATE)
C$MONTH <- month(C$DATE)
C$FYEAR <- C$YEAR
#C[MONTH>=7]$FYEAR <- as.integer(C[MONTH>=7]$YEAR+1) # Fishing year is from July 1st to June 30th
C$AREA  <- as.character(C$AREA)
C$LBS   <- pmax(C$LBS,C$LBS_SOLD,na.rm=T) 
C       <- dplyr::select(C,LICENSE,DATE,MONTH,FYEAR,AREA,GEAR,HOURS,PORT_LAND,FNAME,LNAME,SPECIES,NUM=NUM_KEPT,LBS)

C[SPECIES==36,"SPECIES"] <- 21 #Set ehu code 36 to new code 21
C[SPECIES==70,"SPECIES"] <- 24 #Set weke nono code 70 to new code 24

#Obtain official list of grids in the MHI
Official      <- data.table(  read.csv("Data/BF_Area_Grid_Reggie.csv"), stringsAsFactors=F)
setnames(Official,c("area","Valid"),c("AREA","VALID"))
Official$AREA <- as.character(Official$AREA)
Official      <- Official[!(AREA=="16123"&subarea!="I")] # Keep only 1 line for area 16123 so it can be merged.
Official      <- dplyr::select(Official,AREA,VALID)
C             <- merge(C,Official,all.x=T,by="AREA")

# Re-classify gear codes for easier data exploration
load("DATA/gear_codes.rdata")
GEARS <- dplyr::select(GEARS,GEAR_PK,GEAR_NAME)
setnames(GEARS,"GEAR_PK","GEAR")

GEARS$GEAR_A <- "OTHER"
GEARS[grepl("TROLLING",GEAR_NAME)]$GEAR_A                        <- "TROLLING"
GEARS[grepl("LINE",GEAR_NAME)|grepl("CASTING",GEAR_NAME)]$GEAR_A <- "OTHER_LINE"
GEARS[GEAR==3]$GEAR_A                                            <- "DEEP_HANDLINE"
GEARS[GEAR==4]$GEAR_A                                            <- "INSHORE_HANDLINE"
GEARS[grepl("SPEAR",GEAR_NAME)|grepl("DIVING",GEAR_NAME)]$GEAR_A <- "SPEAR"
GEARS[grepl("TRAP",GEAR_NAME)]$GEAR_A                            <- "TRAP"
GEARS[grepl("NET",GEAR_NAME)]$GEAR_A                             <- "NET"

C <- merge(C,GEARS,by="GEAR")

# Alternative spatial aggregation
RC           <- data.table(unique(C$AREA))
colnames(RC) <- "AREA"

# Spatial classification scheme "A"
RC$AREA_A <- RC$AREA
RC[AREA==594|AREA==595|AREA==578|AREA==579|AREA==580,]$AREA_A <- "593"
RC[AREA==508,]$AREA_A <- "528"
RC[AREA==506,]$AREA_A <- "526"
RC[AREA==505,]$AREA_A <- "525"
RC[AREA==502,]$AREA_A <- "522"
RC[AREA==501,]$AREA_A <- "521"
RC[AREA==500,]$AREA_A <- "520"
RC[AREA==504,]$AREA_A <- "524"
RC[AREA==503,]$AREA_A <- "523"

RC[AREA==400,]$AREA_A <- "420"
RC[AREA==401,]$AREA_A <- "421"
RC[AREA==402,]$AREA_A <- "422"
RC[AREA==403,]$AREA_A <- "423"
RC[AREA==404,]$AREA_A <- "424"
RC[AREA==405,]$AREA_A <- "425"
RC[AREA==406,]$AREA_A <- "426"
RC[AREA==407,]$AREA_A <- "427"
RC[AREA==408,]$AREA_A <- "428"
RC[AREA==409,]$AREA_A <- "429"

RC[AREA==300,]$AREA_A <- "320"
RC[AREA==309|AREA==301|AREA==310|AREA==314,]$AREA_A <- "321"
RC[AREA==311|AREA==351|AREA==452,]$AREA_A <- "331"
RC[AREA==302,]$AREA_A <- "322"
RC[AREA==303,]$AREA_A <- "323"
RC[AREA==304,]$AREA_A <- "324"
RC[AREA==305,]$AREA_A <- "325"
RC[AREA==306,]$AREA_A <- "326"
RC[AREA==307,]$AREA_A <- "327"
RC[AREA==308,]$AREA_A <- "327"
RC[AREA==312,]$AREA_A <- "332"
RC[AREA==313,]$AREA_A <- "333"

RC[AREA==100,]$AREA_A <- "120"
RC[AREA==101,]$AREA_A <- "121"
RC[AREA==102,]$AREA_A <- "122"
RC[AREA==103,]$AREA_A <- "123"
RC[AREA==104,]$AREA_A <- "124"
RC[AREA==105,]$AREA_A <- "125"
RC[AREA==106,]$AREA_A <- "126"
RC[AREA==107,]$AREA_A <- "127"
RC[AREA==108,]$AREA_A <- "128"

# Spatial classification scheme "B"
RC$AREA_B <- RC$AREA
RC[AREA_A==594|AREA_A==593|AREA_A==592|AREA_A==577|AREA_A==578|AREA_A==579|AREA_A==580|AREA_A==595|AREA_A==596,]$AREA_B <- "Middle Bank"
RC[AREA_A=="16123I"|AREA_A=="16123F"|AREA_A=="16123C",]$AREA_B          <- "Middle Bank"
RC[AREA_A==525|AREA_A==526|AREA_A==527|AREA_A==528,]$AREA_B             <- "Niihau"
RC[AREA_A==521|AREA_A==522|AREA_A==523,]$AREA_B                         <- "West Kauai"
RC[AREA_A==520|AREA_A==524,]$AREA_B                                     <- "East Kauai"
RC[AREA_A==429|AREA_A==420|AREA_A==421|AREA_A==422|AREA_A==423,]$AREA_B <- "South Oahu"
RC[AREA_A==424|AREA_A==425|AREA_A==426|AREA_A==427|AREA_A==428,]$AREA_B <- "North Oahu"

RC[AREA_A==331,]$AREA_B                         <- "Penguin"
RC[AREA_A==332|AREA_A==333,]$AREA_B             <- "North Molokai"
RC[AREA_A==321|AREA_A==320,]$AREA_B             <- "Triangle"
RC[AREA_A==327|AREA_A==328,]$AREA_B             <- "South Lanai"
RC[AREA_A==325|AREA_A==326,]$AREA_B             <- "South Kaho"
RC[AREA_A==322|AREA_A==323|AREA_A==324,]$AREA_B <- "East Maui"

RC[AREA_A==120|AREA_A==121|AREA_A==122,]$AREA_B <- "Kona"
RC[AREA_A==123|AREA_A==124|AREA_A==125,]$AREA_B <- "Hamakua"
RC[AREA_A==126|AREA_A==127|AREA_A==128,]$AREA_B <- "East Hawaii"

C <- merge(C,RC,by="AREA",all.x=T)

# Classify the areas as valid, invalid, or in the NWHI
C[VALID=="(pond)"|VALID=="No"]$AREA_A <- "Invalid" # Filter invalid codes but keep NWHI (i.e NAs)
C[VALID=="(pond)"|VALID=="No"]$AREA_B <- "Invalid" # Filter invalid codes but keep NWHI (i.e NAs)

C[is.na(VALID)]$AREA_A  <- "NWHI" # Anything not in BF_Area_Grid_Reggie.csv is considered in the NWHI
C[is.na(VALID)]$AREA_B  <- "NWHI" 
C[AREA_A=="16123A"|AREA_A=="16123B"|AREA_A=="16123D"|AREA_A=="16123E"|AREA_A=="16123G"|AREA_A=="16123H"]$AREA_A <- "NWHI"
C[AREA_A=="16123A"|AREA_A=="16123B"|AREA_A=="16123D"|AREA_A=="16123E"|AREA_A=="16123G"|AREA_A=="16123H"]$AREA_B <- "NWHI"
C[AREA_A=="16123"] <- "NWHI"

# Add broader fishing regions
C$AREA_C <- C$AREA_B
C[AREA_B=="Niihau"|AREA_B=="West Kauai"|AREA_B=="East Kauai"|AREA_B=="Middle Bank"]$AREA_C <- "Kauai"
C[AREA_B=="South Oahu"|AREA_B=="North Oahu"]$AREA_C                                        <- "Oahu"
C[AREA_B=="Penguin"]$AREA_C                                                                <- "Penguin"
C[AREA_B=="North Molokai"|AREA_B=="South Lanai"|AREA_B=="Triangle"|AREA_B=="East Maui"|AREA_B=="South Kaho"]$AREA_C <- "MauiNui"
C[AREA_B=="Kona"|AREA_B=="Hamakua"|AREA_B=="East Hawaii"]$AREA_C                           <- "Hawaii"

# Add even broader fishing regions
C$AREA_D <- C$AREA_C
C[AREA_B!="NWHI"&AREA_B!="Invalid"]$AREA_D <- "MHI"

# Create unique fisher ID based on first and last name
C$FNAME  <- gsub("[.]","",C$FNAME) # Remove periods
C$FNAME  <- gsub("[,]","",C$FNAME) # Remove commas
C$FNAME  <- gsub(" .*$","",C$FNAME)# Find a space ( ), followed by any characters (.), any number of times (*) until the end ($). Used to remove anything past the 1st space.
C$LNAME  <- gsub("[.]","",C$LNAME) # Remove periods
C$LNAME  <- gsub("[,]","",C$LNAME) # Remove commas
C$FISHER <- tolower(paste(C$FNAME,C$LNAME,sep="."))
C$FISHER <- gsub(" ",".",C$FISHER)


# Create a list linking LICENSE to FISHER for data after 1993-07-01
FISHER.INFO <- data.table(  unique(data.frame(C[DATE>="1993-07-01"&FISHER!="null.null"])[c("FISHER","LICENSE")])  )
FISHER.INFO <- C[DATE>="1993-07-01"&FISHER!="null.null",list(NUMREC=.N),by=list(FISHER,LICENSE)]
FISHER.INFO <- FISHER.INFO[order(LICENSE,-NUMREC)]
FISHER.INFO <- FISHER.INFO[!duplicated(LICENSE)]    # Remove licenses that have multiple fisher name selects LICENSE value with highest NUMREC by default)
FISHER.INFO <- FISHER.INFO[order(FISHER,-NUMREC)]
FISHER.INFO <- FISHER.INFO[!duplicated(FISHER)]    # Remove FISHER that have multiple LICENSE values selects FISHER value with highest NUMREC by default)
FISHER.INFO$NUMREC <- NULL
setnames(FISHER.INFO,"FISHER","FISHER2")

# Give a FISHER value for all records where they are missing, in the recent dataset
C <- merge(C,FISHER.INFO,by="LICENSE",all.x=T)
C[FISHER=="null.null"]$FISHER <- C[FISHER=="null.null"]$FISHER2
C[FNAME=="NULL"&LNAME=="NULL"&is.na(FISHER)]$FISHER <- paste0("Fisher.",C[FNAME=="NULL"&LNAME=="NULL"&is.na(FISHER)]$LICENSE) # Fix remaining records with no fisher name ever recorded.

C$FISHER2=C$FNAME=C$LNAME=NULL # No need anymore

# New gear classification for SS analyses
C$GEAR_B   <- C$GEAR_A
C[GEAR_B!="DEEP_HANDLINE"&GEAR_B!="INSHORE_HANDLINE"&GEAR_B!="TROLLING"]$GEAR_B <- "RARE"
C$GEAR_B   <- paste0("COM_",C$GEAR_B)

# Export processed raw data for further steps
C    <- dplyr::select(C,DATE,MONTH,FYEAR,LICENSE,AREA,AREA_A,AREA_B,AREA_C,AREA_D,GEAR,GEAR_A,GEAR_B,HOURS,PORT_LAND,FISHER,SPECIES,NUM,LBS)
C    <- C[order(DATE,LICENSE,AREA,GEAR,SPECIES,LBS)]

saveRDS(C,"Outputs/CATCH_processed.rds")





