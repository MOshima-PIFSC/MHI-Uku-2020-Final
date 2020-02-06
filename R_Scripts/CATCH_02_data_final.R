require(data.table); require(MASS); require(dplyr); require(ggplot2)

# Select species by code (e.g. 20 == uku)

Species.code <- 20

#--- Create final catch table for a specific species, for use in assessment models-------------------------------
C <- readRDS(file="Outputs/CATCH_processed.rds")

# Partition invalid areas to known mhi areas based on proportion of species/year in mhi area compared to nwhi area
aggr          <- C[SPECIES==Species.code,list(LBS=sum(LBS)),by=list(FYEAR,AREA_D,GEAR_B)]
aggr          <- dcast(aggr, FYEAR+GEAR_B~AREA_D,value.var="LBS",fill=0)
aggr$PROP_MHI <- aggr$MHI/(aggr$MHI+aggr$NWHI)
aggr$Inv_MHI  <- aggr$Invalid*aggr$PROP_MHI

D         <- C[SPECIES==Species.code&AREA_D=="MHI",list(LBS=sum(LBS)),by=list(FYEAR,GEAR_B)]
D         <- D[order(FYEAR,GEAR_B)]
D$LBS_COM <- D$LBS+aggr$Inv_MHI
D$LBS     <- NULL
D         <- dcast(D, FYEAR~GEAR_B,value.var="LBS_COM",fill=0)
D$COM_TOT <- D$COM_DEEP_HANDLINE+D$COM_INSHORE_HANDLINE+D$COM_RARE+D$COM_TROLLING

# Add MRIP recreational catch data
M          <- data.table( read.csv("Data\\mrip_GREEN_JOBFISH_catch_series.csv",skip=28, stringsAsFactors = F) )
M          <- dplyr::select(M,YEAR="Year",MODE="Fishing.Mode",NUM_MRIP=Total.Harvest..A.B1.)
M          <- M[MODE!="PARTY/CHARTER BOAT"]
M$NUM_MRIP <- as.numeric(M$NUM_MRIP)

# A correction factor for year 2003-2010 is necessary due to an error (Ma 2013, IR-13_006.pdf)
M[YEAR<=2010]$NUM_MRIP <- M[YEAR<=2010]$NUM_MRIP/1.22 

# Mail effort survey started officially in 2018, 2017 data can be transformed to mail-survey equivalent by applying correction factors
M[YEAR==2017&MODE=="SHORE"]$NUM_MRIP               <- M[YEAR==2017&MODE=="SHORE"]$NUM_MRIP*2.89
M[YEAR==2017&MODE=="PRIVATE/RENTAL BOAT"]$NUM_MRIP <- M[YEAR==2017&MODE=="PRIVATE/RENTAL BOAT"]$NUM_MRIP*2.33

M$LBS_MRIP <- M$NUM_MRIP*(3.2644*2.20462)    # Calculate catch in lbs by multiplying number caught by the mean kg weight of an uku from HMFRS 2003-2017 data (n=152)
M          <- data.table( dcast(M,YEAR~MODE,value.var="LBS_MRIP",fill=0) )

S      <- data.table( read.csv("Data\\ProportionSampleSizeV2.csv",skip=1) ) # Proportion of MRIP catch sold by year for "boat" mode ("shore" mode list zero sold catch)
S      <- dplyr::select(S,YEAR=year,PROP_SOLD=Annual.proportion.of.sold.uku..by.numbers.)

M         <- merge(M,S,by="YEAR")
M$LBS_REC <- M$`PRIVATE/RENTAL BOAT`*(1-M$PROP_SOLD) # Remove the proportion of the boat-based catch that was sold based on yearly prop. sold (more variable)
#M$LBS_REC <- M$`PRIVATE/RENTAL BOAT`*0.75              # Remove the proportion of the boat-based catch tat was sold based on average prop. sold of 0.25 (more stable)
M$LBS_REC <- M$LBS_REC+M$SHORE
M         <- dplyr::select(M,YEAR,LBS_REC)
median(M$LBS_REC)
median(D[FYEAR>=2003&FYEAR<=2017]$COM_TOT)
median(M$LBS_REC)/median(D[FYEAR>=2003&FYEAR<=2017]$COM_TOT)

# Method 1: Calculate ratio of rec to com catch and fit distribution to these ratios to capture uncertainty.
Ratios        <- median(M$LBS_REC)/D[FYEAR>=2003&FYEAR<=2017]$COM_TOT # Ratio of recreational to commercial catch
Ratio.dist    <- fitdistr(Ratios,"lognormal")
Ratio.logmean <- Ratio.dist$estimate[1]
Ratio.logsd   <- Ratio.dist$estimate[2]
x <- seq(0,3,by=0.1)
y <- dlnorm(x,Ratio.logmean,Ratio.logsd)
hist(Ratios,prob=T,xlim=c(0,3),ylim=c(0,1))
lines(x,y)

# Recent trends in recreational catch
rec_catch <- ggplot(data=M,aes(x=YEAR,y=LBS_REC*0.453592/1000))+geom_point()+
  ylab("Recreational catch (metric tons)")+ggtitle("HMFRS recreational uku catch")+
  geom_smooth(method="lm")+theme_bw()
ggsave(rec_catch,filename="Outputs/Graphs/Catch/FIG_REC.tiff",dpi=300,units="cm",width=13,height=6)


# Method 2: Calculate distribution of recreational catch between 2003-2017, removing small downward temporal trend
rec_lm     <- lm(data=M,log(LBS_REC)~YEAR)
plot(M$YEAR,log(M$LBS_REC))
abline(lm(log(M$LBS_REC)~M$YEAR))

Rec.logmean <- mean(log(M$LBS_REC))
Rec.logsd   <- 0.4   # Based on MRIP provided number, where SHORE CV=77% and BOAT CV=29% (SHORE is 23% of catch vs 77% for BOAT)
x <- seq(0,250000,by=50000)
y <- dlnorm(x,Rec.logmean,Rec.logsd)
hist(M$LBS_REC,breaks=5,prob=T)
lines(x,y)

# Calculate the proportion of yearly human pop. in Hawaii
P       <- data.table( read.csv("Data\\HIPOP.csv") )    
P$PROP  <- P$HIPOP/mean(P[DATE>=2003&DATE<=2017]$HIPOP) #based on reference years 2003 to 2017
P$PROP2 <- P$HIPOP/P[DATE==2003]$HIPOP                  #based on reference year 2003
P$PROP3 <- P$HIPOP/mean(P[DATE>=2003&DATE<=2007]$HIPOP) #based on reference year 2003 to 2007 (5 years)

# Calculate the lognormal distribution of catch for each year using 3 methods

# for years >=2013, we used a linear trend line to estimate yearly catch.
aVect_LBS <- aVect_KGS <- vector(length=1000)
for(i in 1:71){
  
  # Rec method 1: rec catch is proportional to com catch
  aVect_LBS             <- D$COM_TOT[i]+D$COM_TOT[i]*rlnorm(n=1000,Ratio.logmean,Ratio.logsd)
  aVect_KGS             <- aVect_LBS*0.453592
  D$KGS_ALL_M1[i]       <- fitdistr(aVect_KGS,"lognormal")$estimate[1] 
  D$KGS_ALL_M1.LOGSD[i] <- fitdistr(aVect_KGS,"lognormal")$estimate[2]
  
  # Rec method 2:  rec catch is prop. to pop. growth for early years, and is related to linear trend for recent year.
  if(i<56){
    aVect_LBS             <- rlnorm(n=1000,log(mean(M[YEAR<=2007]$LBS_REC)),Rec.logsd)*P$PROP3[i]
    aVect_KGS             <- aVect_LBS*0.453592
    D$KGS_REC_M2[i]       <- fitdistr(aVect_KGS,"lognormal")$estimate[1]
    D$KGS_REC_M2.LOGSD[i] <- fitdistr(aVect_KGS,"lognormal")$estimate[2]
  }
  if(i>=56){
    aVect_LBS             <- rlnorm(n=1000,coef(rec_lm)[1]+coef(rec_lm)[2]*D[i]$FYEAR,Rec.logsd)  
    aVect_KGS             <- aVect_LBS*0.453592  
    D$KGS_REC_M2[i]       <- fitdistr(aVect_KGS,"lognormal")$estimate[1]
    D$KGS_REC_M2.LOGSD[i] <- fitdistr(aVect_KGS,"lognormal")$estimate[2]
  }
}

D$MTONS_ALL_M1  <- exp(D$KGS_ALL_M1+D$KGS_ALL_M1.LOGSD^2/2)/1000
D$MTONS_ALL_M2  <- exp(D$KGS_REC_M2+D$KGS_REC_M2.LOGSD^2/2)/1000+D$COM_TOT*0.453592/1000
D$KGS_COM.LOGSD <- 0.01
D$MTONS_REC_M2  <- exp(D$KGS_REC_M2+D$KGS_REC_M2.LOGSD^2/2)/1000
D$MTONS_COM     <- D$COM_TOT*0.453592/1000

# Rec method 3: Use actual raw rec. catch data for recent years
D$MTONS_REC_M3         <- D$MTONS_REC_M2
D[56:71,]$MTONS_REC_M3 <- M$LBS_REC*0.453592/1000
D$MTONS_REC_M3.LOGSD   <- 0.4
D$MTONS_ALL_M3         <- D$MTONS_REC_M3+D$COM_TOT*0.453592/1000
D$MTONS_ALL_M3.LOGSD   <- (D$MTONS_REC_M3/D$MTONS_ALL_M3)*0.4
  
plot(D$FYEAR,D$MTONS_REC_M3)

# convert lbs to metric tons for commercial data
D$COM_DEEP_HANDLINE    <- D$COM_DEEP_HANDLINE*0.453592/1000
D$COM_INSHORE_HANDLINE <- D$COM_INSHORE_HANDLINE*0.453592/1000
D$COM_TROLLING         <- D$COM_TROLLING*0.453592/1000
D$COM_RARE             <- D$COM_RARE*0.453592/1000

D$COM_OTHER            <- D$COM_INSHORE_HANDLINE+D$COM_TROLLING+D$COM_RARE

catch_plot <- ggplot(data=D,aes(x=FYEAR))+geom_line(aes(y=MTONS_ALL_M1))+geom_line(aes(y=MTONS_ALL_M2),col="red")+
  geom_line(aes(y=MTONS_ALL_M3),col="blue")+
  ylab("Total catch (metric tons)")+ggtitle("Methods: Black=Ratios, Red=Human pop., Blue=Trend")+
  theme_bw()
ggsave(catch_plot,filename="Outputs/Graphs/Catch/FIG10a.tiff",dpi=300,units="cm",width=13,height=6)

catch_plot2 <- ggplot(data=D,aes(x=FYEAR))+geom_line(aes(y=MTONS_ALL_M3),size=1)+
  geom_line(aes(y=MTONS_ALL_M3+(MTONS_ALL_M3*MTONS_ALL_M3.LOGSD*1.96)),size=0.3)+
  geom_line(aes(y=MTONS_ALL_M3-(MTONS_ALL_M3*MTONS_ALL_M3.LOGSD*1.96)),size=0.3)+
  ylab("Total catch (metric tons)")+ggtitle("Catch using human pop. trend")+
  theme_bw()
ggsave(catch_plot2,filename="Outputs/Graphs/Catch/FIG10b.tiff",dpi=300,units="cm",width=13,height=6)

#D <- dplyr::select(D,FYEAR,COM_DEEP_HANDLINE,COM_OTHER,MTONS_REC_M3,MTONS_REC_M3.LOGSD)
D <- dplyr::select(D,FYEAR,COM_DEEP_HANDLINE,COM_INSHORE_HANDLINE,COM_TROLLING,COM_RARE,MTONS_REC_M3,MTONS_REC_M3.LOGSD)



write.csv(D,"Outputs\\SS3 inputs\\Final_CATCH.csv",row.names=F)
