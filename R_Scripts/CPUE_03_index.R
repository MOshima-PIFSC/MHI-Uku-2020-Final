require(data.table); require(ggplot2); require(mgcv);  require(MASS); require(dplyr); require(emmeans); require(lme4); require(boot); require(stringr); 
require(gridExtra); require(grid); require(statmod)

Gear.name <- c("DEEP_HANDLINE","INSHORE_HANDLINE","TROLLING")[3]

C         <- readRDS(paste0("Outputs/CPUE_",Gear.name,"_StepC.rds"))
MODELS    <- readRDS(paste0("Outputs","/CPUE models_",Gear.name,".rds"))

C$FYEAR           <- as.character(C$FYEAR)
C$PRES            <- 0
C[UKUCPUE>0]$PRES <- 1

# Arrange datasets for CPUE standardization
O <- C[DATE<"2003-01-01"]
R <- C[DATE>="2003-01-01"&!is.na(SPEED)]

O <- dplyr::select(O,TRIP,FYEAR,MONTH,CUM_EXP,FISHER,LAT,LONG,AREA,AREA_A,AREA_B,AREA_C,PC1,PC2,PC3,PC4,UKUCPUE,PRES)                 # Old data
R <- dplyr::select(R,TRIP,FYEAR,MONTH,CUM_EXP,FISHER,LAT,LONG,SPEED,XDIR,YDIR,AREA,AREA_A,AREA_B,AREA_C,PC1,PC2,PC3,PC4,UKUCPUE,PRES) # Recent data

O$FYEAR <- as.numeric(O$FYEAR)
R$FYEAR <- as.numeric(R$FYEAR)

# Sort best models
BestOldPos  <- MODELS[[1]]
BestOldPres <- MODELS[[2]]
BestRecPos  <- MODELS[[3]]
BestRecPres <- MODELS[[4]]

#===========================================================================================================
#===================== Calculate abundance index for Old dataset============================================
#===========================================================================================================
EFFECTS1           <- summary(BestOldPos)$coefficients[,1:2]
EFFECTS1           <- EFFECTS1[rownames(EFFECTS1)%like%"FYEAR",]
rownames(EFFECTS1) <- str_remove(rownames(EFFECTS1),"FYEAR")
EFFECTS1           <- data.table(  cbind(EFFECTS1,as.numeric(rownames(EFFECTS1)))  )
setnames(EFFECTS1,c("Estimate","V3","Std. Error"),c("LOGCPUE","FYEAR","LOGSDCPUE"))
Num.Years <- nrow(EFFECTS1)

OI                 <- data.table(FYEAR=seq(1948,2002))
OI                 <- merge(OI,EFFECTS1,by="FYEAR",all.x=T) 
OI$LOGCPUE[1]      <- summary(BestOldPos)$coefficients[1]                      # First year is set to intercept
OI$LOGCPUE[2:55]   <- OI$LOGCPUE[2:55]+summary(BestOldPos)$coefficients[1]     # Following years are set to intercept+YEAR effect
OI$LOGSDCPUE[1]    <- OI$LOGSDCPUE[2]   # Take SD from next year since this is base year.

EFFECTS2           <- summary(BestOldPres)$coefficients[,1:2]
EFFECTS2           <- EFFECTS2[rownames(EFFECTS2)%like%"FYEAR",]
rownames(EFFECTS2) <- str_remove(rownames(EFFECTS2),"FYEAR")
EFFECTS2           <- data.table(  cbind(EFFECTS2,as.numeric(rownames(EFFECTS2)))  )
setnames(EFFECTS2,c("Estimate","V3","Std. Error"),c("LOGIT","FYEAR","LOGITSD"))
Num.Years <- nrow(EFFECTS2)

EFFECTS3           <- summary(BestOldPres)$coefficients[,1:2]
EFFECTS3           <- EFFECTS3[!rownames(EFFECTS3)%like%"FYEAR",]
EFFECTS3           <- data.table(  cbind(EFFECTS3,rownames(EFFECTS3))  )
setnames(EFFECTS3,c("Estimate","V3","Std. Error"),c("LOGIT","VAR","LOGITSD"))
EFFECTS3$LOGIT     <- as.numeric(EFFECTS3$LOGIT)
EFFECTS3$LOGITSD   <- as.numeric(EFFECTS3$LOGITSD)

# Other effects necessary for logit model index calculations
AREA331.EFFECT <- EFFECTS3[VAR%like%"AREA_A331"]$LOGIT
JUNE.EFFECT    <- EFFECTS3[VAR=="factor(MONTH)6"]$LOGIT
CUM_EXP.EFFECT <- EFFECTS3[VAR%like%"CUM_EXP"]$LOGIT 
PC1a.EFFECT    <- EFFECTS3[VAR%like%"PC1"]$LOGIT[1]
PC1b.EFFECT    <- EFFECTS3[VAR%like%"PC1"]$LOGIT[2]
PC2a.EFFECT    <- EFFECTS3[VAR%like%"PC2"]$LOGIT[1]
PC2b.EFFECT    <- EFFECTS3[VAR%like%"PC2"]$LOGIT[2]


if(Gear.name=="DEEP_HANDLINE"){
  print(formula(BestOldPres))
  OI <- merge(OI,EFFECTS2,by="FYEAR",all.x=T)
  
  INTERCEPT <- summary(BestOldPres)$coefficients[1,1]+AREA331.EFFECT+JUNE.EFFECT+   # Intercept+AREA=331+JUNE
                 PC1a.EFFECT*median(O$PC1)+PC1b.EFFECT*median(O$PC1)^2+PC2a.EFFECT*median(O$PC2)+PC2b.EFFECT*median(O$PC2)^2  # PC1+PC2
  
  OI$LOGIT[1]    <- INTERCEPT
  OI$LOGIT[2:55] <- OI$LOGIT[2:55]+INTERCEPT
  OI$LOGITSD[1]  <- OI$LOGITSD[2]   # Take SD from next year since this is base year.
}

if(Gear.name=="INSHORE_HANDLINE"){
  print(formula(BestOldPres))
  OI <- merge(OI,EFFECTS2,by="FYEAR",all.x=T)
  
  INTERCEPT <- summary(BestOldPres)$coefficients[1,1]+AREA331.EFFECT+PC1.EFFECT*median(O$PC1)+PC2.EFFECT*median(O$PC2)+PC3.EFFECT*median(O$PC3)+ # Intercept+AREA=331+PCs
    JUNE.EFFECT+CUM_EXP.EFFECT*median(log(O$CUM_EXP))                                 # JUNE+EXPERIENCE
  
  OI$LOGIT[1]    <- INTERCEPT
  OI$LOGIT[2:55] <- OI$LOGIT[2:55]+INTERCEPT
  OI$LOGITSD[1]    <- OI$LOGITSD[2]   # Take SD from next year since this is base year.
}

if(Gear.name=="TROLLING"){
  print(formula(BestOldPres))
  OI <- merge(OI,EFFECTS2,by="FYEAR",all.x=T)
  
  INTERCEPT <- summary(BestOldPres)$coefficients[1,1]+AREA331.EFFECT+JUNE.EFFECT+CUM_EXP.EFFECT*median(log(O$CUM_EXP))+ # Intercept+AREA=331+JUNE+EXPERIENCE
                 PC1.EFFECT*median(O$PC1)+PC2.EFFECT*median(O$PC2)+PC3.EFFECT*median(O$PC3)    # PC1 to PC3
    
  OI$LOGIT[1]    <- INTERCEPT
  OI$LOGIT[2:55] <- OI$LOGIT[2:55]+INTERCEPT
  OI$LOGITSD[1]  <- OI$LOGITSD[2]   # Take SD from next year since this is base year.
}

# Create abundance index
OI <- OI[!is.na(LOGCPUE)&!is.na(LOGIT)]  # Remove years with incomplete CPUE estimates
OA <- data.table(FYEAR=OI$FYEAR,CPUE=numeric(nrow(OI)),LOGCPUESD=numeric(nrow(OI))) # Store final CPUE distribution by year for recent data
for(i in 1:nrow(OI)){
  
  MC     <- exp( rnorm(n=1000,mean=OI$LOGCPUE[i],sd=OI$LOGSDCPUE[i])+OI$LOGSDCPUE[i]^2/2 )*inv.logit( rnorm(n=1000,mean=OI$LOGIT[i],sd=OI$LOGITSD[i]) )
  FIT    <- fitdistr(MC[MC>0],"lognormal")
  OA[i]$CPUE   <- exp( OI$LOGCPUE[i]+OI$LOGSDCPUE[i]^2/2 )*inv.logit( OI$LOGIT[i] )
  OA[i]$LOGCPUESD <- FIT$estimate[2]  
}

# For SS input, enter un-transformed CPUE and log sd (since CV of normal dist. ~ log sd)
OA       <- OA[CPUE<Inf]

OA <- dplyr::select(OA,FYEAR,CPUE,LOGCPUESD)
write.csv(OA,paste0("Outputs/SS3 inputs/Final_",Gear.name,"_CPUE_old.csv"),row.names=F)


#===========================================================================================================
#===================== Calculate abundance index for Recent dataset=========================================
#===========================================================================================================
EFFECTS1           <- summary(BestRecPos)$coefficients[,1:2]
EFFECTS1           <- EFFECTS1[rownames(EFFECTS1)%like%"FYEAR",]
rownames(EFFECTS1) <- str_remove(rownames(EFFECTS1),"FYEAR")
EFFECTS1           <- data.table(  cbind(EFFECTS1,as.numeric(rownames(EFFECTS1)))  )
setnames(EFFECTS1,c("Estimate","V3","Std. Error"),c("LOGCPUE","FYEAR","LOGSDCPUE"))
Num.Years          <- nrow(EFFECTS1)

RI                 <- data.table(FYEAR=seq(2003,2018))
RI                 <- merge(RI,EFFECTS1,by="FYEAR",all.x=T) 
RI$LOGCPUE[1]      <- summary(BestRecPos)$coefficients[1]                    # First year is set to intercept
RI$LOGCPUE[2:16]   <- RI$LOGCPUE[2:16]+summary(BestRecPos)$coefficients[1]   # Following years are set to intercept+YEAR effect
RI$LOGSDCPUE[1]    <- RI$LOGSDCPUE[2]                                        # Take SD from next year since this is base year.

EFFECTS2           <- summary(BestRecPres)$coefficients[,1:2]
EFFECTS2           <- EFFECTS2[rownames(EFFECTS2)%like%"FYEAR",]
rownames(EFFECTS2) <- str_remove(rownames(EFFECTS2),"FYEAR")
EFFECTS2           <- data.table(  cbind(EFFECTS2,as.numeric(rownames(EFFECTS2)))  )
setnames(EFFECTS2,c("Estimate","V3","Std. Error"),c("LOGIT","FYEAR","LOGITSD"))
Num.Years          <- nrow(EFFECTS2)

EFFECTS3           <- summary(BestRecPres)$coefficients[,1:2]
EFFECTS3           <- EFFECTS3[!rownames(EFFECTS3)%like%"FYEAR",]
EFFECTS3           <- data.table(  cbind(EFFECTS3,rownames(EFFECTS3))  )
setnames(EFFECTS3,c("Estimate","V3","Std. Error"),c("LOGIT","VAR","LOGITSD"))
EFFECTS3$LOGIT     <- as.numeric(EFFECTS3$LOGIT)
EFFECTS3$LOGITSD   <- as.numeric(EFFECTS3$LOGITSD)

# Other effects necessary for logit model index calculations
AREA331.EFFECT <- EFFECTS3[VAR%like%"AREA_A331"]$LOGIT
JUNE.EFFECT    <- EFFECTS3[VAR=="factor(MONTH)6"]$LOGIT
CUM_EXP.EFFECT <- EFFECTS3[VAR%like%"CUM_EXP"]$LOGIT
SPEED.EFFECT   <- EFFECTS3[VAR%like%"SPEED"]$LOGIT
PC1a.EFFECT    <- EFFECTS3[VAR%like%"PC1"]$LOGIT[1]
PC1b.EFFECT    <- EFFECTS3[VAR%like%"PC1"]$LOGIT[2]
PC2a.EFFECT    <- EFFECTS3[VAR%like%"PC2"]$LOGIT[1]
PC2b.EFFECT    <- EFFECTS3[VAR%like%"PC2"]$LOGIT[2]

if(Gear.name=="DEEP_HANDLINE"){
  print(formula(BestRecPres))
  RI <- merge(RI,EFFECTS2,by="FYEAR",all.x=T)
  
  INTERCEPT <- summary(BestRecPres)$coefficients[1,1]+AREA331.EFFECT+JUNE.EFFECT+CUM_EXP.EFFECT*log(median(R$CUM_EXP,na.rm=T))+
    PC1a.EFFECT*median(R$PC1)+PC1b.EFFECT*median(R$PC1)^2+PC2a.EFFECT*median(R$PC2)+PC2b.EFFECT*median(R$PC2)^2  # PC1+PC2
                        
  RI$LOGIT[1]    <- INTERCEPT
  RI$LOGIT[2:16] <- RI$LOGIT[2:16]+INTERCEPT
  RI$LOGITSD[1]  <- RI$LOGITSD[2]   # Take SD from next year since this is base year.
}

if(Gear.name=="INSHORE_HANDLINE"){
  print(formula(BestRecPres))
  RI <- merge(RI,EFFECTS2,by="FYEAR",all.x=T)
  
  INTERCEPT <- summary(BestRecPres)$coefficients[1,1]+AREA331.EFFECT+JUNE.EFFECT+          # Intercept+AREA=331+JUNE+
                    PC1a.EFFECT*median(R$PC1)+PC1b.EFFECT*median(R$PC1)^2+PC2a.EFFECT*median(R$PC2)+PC2b.EFFECT*median(R$PC2)^2  # PC1+PC2
                          
  RI$LOGIT[1]    <- INTERCEPT
  RI$LOGIT[2:16] <- RI$LOGIT[2:16]+INTERCEPT
  RI$LOGITSD[1]  <- RI$LOGITSD[2]   # Take SD from next year since this is base year.
}

if(Gear.name=="TROLLING"){
  print(formula(BestRecPres))
  RI <- merge(RI,EFFECTS2,by="FYEAR",all.x=T)
  
  INTERCEPT <- summary(BestRecPres)$coefficients[1,1]+AREA331.EFFECT+JUNE.EFFECT+SPEED.EFFECT*median(R$SPEED,na.rm=T)+   # Intercept+AREA=331+JUNE+SPEED
                  CUM_EXP.EFFECT*log(median(R$CUM_EXP,na.rm=T))+
                  PC1a.EFFECT*median(R$PC1)+PC1b.EFFECT*median(R$PC1)^2+PC2a.EFFECT*median(R$PC2)+PC2b.EFFECT*median(R$PC2)^2  # PC1+PC2
  
  RI$LOGIT[1]    <- INTERCEPT
  RI$LOGIT[2:16] <- RI$LOGIT[2:16]+INTERCEPT
  RI$LOGITSD[1]  <- RI$LOGITSD[2]   # Take SD from next year since this is base year.
}

# Create abundance index
RI <- RI[!is.na(LOGCPUE)&!is.na(LOGIT)]  # Remove years with incomplete CPUE estimates
RA <- data.table(FYEAR=RI$FYEAR,CPUE=numeric(nrow(RI)),LOGCPUESD=numeric(nrow(RI))) # Store final CPUE distribution by year for recent data
for(i in 1:nrow(RI)){
  
  MC     <- exp( rnorm(n=1000,mean=RI$LOGCPUE[i],sd=RI$LOGSDCPUE[i])+RI$LOGSDCPUE[i]^2/2 )*inv.logit( rnorm(n=1000,mean=RI$LOGIT[i],sd=RI$LOGITSD[i]) )
  FIT    <- fitdistr(MC[MC>0],"lognormal")
  RA[i]$CPUE   <- exp( RI$LOGCPUE[i]+RI$LOGSDCPUE[i]^2/2 )*inv.logit( RI$LOGIT[i] )
  RA[i]$LOGCPUESD <- FIT$estimate[2]  
}

# For SS input, enter un-transformed CPUE and log sd (since CV of normal dist. ~ log sd)
RA       <- RA[CPUE<Inf]

RA <- dplyr::select(RA,FYEAR,CPUE,LOGCPUESD)
write.csv(RA,paste0("Outputs/SS3 inputs/Final_",Gear.name,"_CPUE_recent.csv"),row.names=F)




#=================== Compare standardized vs. nominal CPUE ===================
# Nominal CPUE and percent change from Year1
if(Gear.name=="DEEP_HANDLINE")    G <- data.table(FYEAR=seq(1948,2002))
if(Gear.name=="INSHORE_HANDLINE") G <- data.table(FYEAR=seq(1948,2002))
if(Gear.name=="TROLLING")         G <- data.table(FYEAR=seq(1980,2002))

NOMI1   <- O[UKUCPUE>0,list(NOMI1=mean(UKUCPUE)),by=list(FYEAR)]
NOMI2   <- O[,list(NOMI2=mean(PRES)),by=list(FYEAR)]
G       <- merge(G,NOMI1,by="FYEAR",all.x=T)
G       <- merge(G,NOMI2,by="FYEAR",all.x=T)
G$NOMI  <- G$NOMI1*G$NOMI2

G$NOMI1.PERC <- G$NOMI1/mean(G$NOMI1,na.rm=T)*100
G$NOMI2.PERC <- G$NOMI2/mean(G$NOMI2,na.rm=T)*100
G$NOMI.PERC  <- G$NOMI/mean(G$NOMI,na.rm=T)*100

# Add standardize CPUE to summary table
G <- merge(G,OA,by="FYEAR",all.x=T)
setnames(G,"CPUE","STAND")

G <- merge(G,OI,by="FYEAR",all.x=T)
setnames(G,c("LOGCPUE","LOGIT"),c("STAND1","STAND2"))

G$STAND1 <- exp(G$STAND1+G$LOGSDCPUE^2/2)
G$STAND2 <- inv.logit(G$STAND2)

G[STAND==Inf] <- NA

G$STAND1.PERC <- G$STAND1/mean(G$STAND1,na.rm=T)*100 
G$STAND2.PERC <- G$STAND2/mean(G$STAND2,na.rm=T)*100 
G$STAND.PERC  <- G$STAND/mean(G$STAND,na.rm=T)*100

CPUE_O1 <- ggplot(data=G,aes(x=FYEAR))+geom_line(aes(y=NOMI1.PERC,group=1),color="blue")+geom_point(aes(y=STAND1.PERC),color="red",size=2)+
  geom_smooth(aes(y=STAND1.PERC),span=0.3, color="black",size=0.5)+
  ylab("Standardized pos.-only CPUE")+ggtitle("Nominal (blue) vs standard. (red): 1948-2002")+
  theme_bw()

CPUE_O2 <- ggplot(data=G,aes(x=FYEAR))+geom_line(aes(y=NOMI2.PERC,group=1),color="blue")+geom_point(aes(y=STAND2.PERC),color="red",size=2)+
  geom_smooth(aes(y=STAND2.PERC),span=0.3, color="black",size=0.5)+
  ylab("Standardized pres. CPUE")+ggtitle("Nominal (blue) vs standard. (red): 1948-2002")+
  theme_bw()

CPUE_O3 <- ggplot(data=G,aes(x=FYEAR))+geom_line(aes(y=NOMI.PERC,group=1),color="blue")+geom_point(aes(y=STAND.PERC),color="red",size=2)+
  geom_smooth(aes(y=STAND.PERC),span=0.3, color="black",size=0.5)+
  ylab("Standardized total CPUE")+ggtitle("Nominal (blue) vs standard. (red): 1948-2002")+
  theme_bw()

ggsave(CPUE_O1,filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUEFIG1a.tiff"),unit="cm",width=12,height=8,dpi=300)
ggsave(CPUE_O2,filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUEFIG1b.tiff"),unit="cm",width=12,height=8,dpi=300)
ggsave(CPUE_O3,filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUEFIG1c.tiff"),unit="cm",width=12,height=8,dpi=300)



#=================== Compare standardized vs. nominal CPUE ===================
# Nominal CPUE and percent change from Year1
if(Gear.name=="DEEP_HANDLINE")    H <- data.table(FYEAR=seq(2003,2018))
if(Gear.name=="INSHORE_HANDLINE") H <- data.table(FYEAR=seq(2003,2018))
if(Gear.name=="TROLLING")         H <- data.table(FYEAR=seq(2003,2018))

NOMI1   <- R[UKUCPUE>0,list(NOMI1=mean(UKUCPUE)),by=list(FYEAR)]
NOMI2   <- R[,list(NOMI2=mean(PRES)),by=list(FYEAR)]
H       <- merge(H,NOMI1,by="FYEAR",all.x=T)
H       <- merge(H,NOMI2,by="FYEAR",all.x=T)
H$NOMI  <- H$NOMI1*H$NOMI2

H$NOMI1.PERC <- H$NOMI1/mean(H$NOMI1,na.rm=T)*100
H$NOMI2.PERC <- H$NOMI2/mean(H$NOMI2,na.rm=T)*100
H$NOMI.PERC  <- H$NOMI/mean(H$NOMI,na.rm=T)*100

# Add standardize CPUE to summary table
H <- merge(H,RA,by="FYEAR",all.x=T)
setnames(H,"CPUE","STAND")

H <- merge(H,RI,by="FYEAR",all.x=T)
setnames(H,c("LOGCPUE","LOGIT"),c("STAND1","STAND2"))

H$STAND1 <- exp(H$STAND1+H$LOGSDCPUE^2/2)
H$STAND2 <- inv.logit(H$STAND2)

H$STAND1.PERC <- H$STAND1/mean(H$STAND1,na.rm=T)*100 
H$STAND2.PERC <- H$STAND2/mean(H$STAND2,na.rm=T)*100 
H$STAND.PERC  <- H$STAND/mean(H$STAND,na.rm=T)*100

CPUE_R1 <- ggplot(data=H,aes(x=FYEAR))+geom_line(aes(y=NOMI1.PERC,group=1),color="blue")+geom_point(aes(y=STAND1.PERC),color="red",size=2)+
  geom_smooth(aes(y=STAND1.PERC),span=0.4, color="black",size=0.5)+
  ylab("Standardized pos.-only CPUE")+ggtitle("Nominal (blue) vs standard. (red): 2003-2018")+
  theme_bw()

CPUE_R2 <- ggplot(data=H,aes(x=FYEAR))+geom_line(aes(y=NOMI2.PERC,group=1),color="blue")+geom_point(aes(y=STAND2.PERC),color="red",size=2)+
  geom_smooth(aes(y=STAND2.PERC),span=0.4, color="black",size=0.5)+
  ylab("Standardized pres. CPUE")+ggtitle("Nominal (blue) vs standard. (red): 2003-2018")+
  theme_bw()

CPUE_R3 <- ggplot(data=H,aes(x=FYEAR))+geom_line(aes(y=NOMI.PERC,group=1),color="blue")+geom_point(aes(y=STAND.PERC),color="red",size=2)+
  geom_smooth(aes(y=STAND.PERC),span=0.4, color="black",size=0.5)+
  ylab("Standardized total CPUE")+ggtitle("Nominal (blue) vs standard. (red): 2003-2018")+
  theme_bw()

ggsave(CPUE_R1,filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUEFIG1d.tiff"),unit="cm",width=12,height=8,dpi=300)
ggsave(CPUE_R2,filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUEFIG1e.tiff"),unit="cm",width=12,height=8,dpi=300)
ggsave(CPUE_R3,filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUEFIG1f.tiff"),unit="cm",width=12,height=8,dpi=300)


#======================Model diagnostics========================================

#=========================================================Old Positive-only=======================
Residuals=residuals(BestOldPos,type="pearson")
Year    <- O[UKUCPUE>0]$FYEAR
Area    <- O[UKUCPUE>0]$AREA_A
Month   <- O[UKUCPUE>0]$MONTH
Cum_exp <- log(O[UKUCPUE>0]$CUM_EXP)
PC1     <- O[UKUCPUE>0]$PC1
PC2     <- O[UKUCPUE>0]$PC2
sqPC1   <- O[UKUCPUE>0]$PC1^2
sqPC2   <- O[UKUCPUE>0]$PC2^2
data    <- data.table(predicted=predict(BestOldPos),Residuals,Year,Area,Month,Cum_exp,PC1,PC2,sqPC1,sqPC2) 

glist <- list()
glist[[1]] <- ggplot(data=G,aes(x=FYEAR))+geom_line(aes(y=NOMI1.PERC,group=1),color="black",linetype="dotted",size=0.3)+geom_point(aes(y=STAND1.PERC),color="black",size=0.5)+
  geom_smooth(aes(y=STAND1.PERC),span=0.3, color="black",size=0.3)+xlab("Year")+ylab("CPUE % change")+theme_bw()
glist[[2]] <- ggplot(data=data,aes(x=predicted,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[3]] <- ggplot(data=data,aes(x=Residuals))+geom_histogram(col="black",fill="white")+theme_bw()
glist[[4]] <- ggplot(data=data,aes(sample=Residuals))+stat_qq(size=1)+stat_qq_line()+theme_bw()
glist[[5]] <- ggplot(data=data,aes(x=Year,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[6]] <- ggplot(data=data,aes(x=Area,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[7]] <- ggplot(data=data,aes(x=Month,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[8]] <- ggplot(data=data,aes(x=PC1,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[9]] <- ggplot(data=data,aes(x=sqPC1,y=Residuals))+geom_point(size=0.1)+theme_bw()


if(Gear.name=="DEEP_HANDLINE"){
gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[6]],glist[[7]],glist[[8]],glist[[9]])

filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUE_DIAG_OldPos.tiff")
tiff(filename=filename, type="cairo", units="cm", compression = "lzw",
     width=17,
     height=14,
     res=400)
grid.draw(gPage)
dev.off()
}
#==========================================================Old Presence============================
Residuals<-qresid(BestOldPres)
Year     <- O$FYEAR
Area     <- O$AREA_A
Cum_exp  <- log(O$CUM_EXP)
Month    <- O$MONTH
PC1      <- O$PC1
PC2      <- O$PC2
sqPC1    <- O$PC1^2
sqPC2    <- O$PC2^2
data     <- data.table(predicted=predict(BestOldPres),Residuals,Year,Area,Month,Cum_exp,PC1,PC2,sqPC1,sqPC2) 

glist <- list()
glist[[1]] <- ggplot(data=G,aes(x=FYEAR))+geom_line(aes(y=NOMI2.PERC,group=1),color="black",linetype="dotted",size=0.3)+geom_point(aes(y=STAND2.PERC),color="black",size=0.5)+
  geom_smooth(aes(y=STAND2.PERC),span=0.3, color="black",size=0.3)+xlab("Year")+ylab("CPUE % change")+theme_bw()
glist[[2]] <- ggplot(data=data,aes(x=Residuals))+geom_histogram(col="black",fill="white")+theme_bw()
glist[[3]] <- ggplot(data=data,aes(x=Year,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[4]] <- ggplot(data=data,aes(x=Area,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[5]] <- ggplot(data=data,aes(x=Month,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[6]] <- ggplot(data=data,aes(x=PC1,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[7]] <- ggplot(data=data,aes(x=PC2,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[8]] <- ggplot(data=data,aes(x=sqPC1,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[9]] <- ggplot(data=data,aes(x=sqPC2,y=Residuals))+geom_point(size=0.1)+theme_bw()


if(Gear.name=="DEEP_HANDLINE"){
gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[6]],glist[[7]],glist[[8]],glist[[9]])

filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUE_DIAG_OldPres.tiff")
tiff(filename=filename, type="cairo", units="cm", compression = "lzw",
     width=17,
     height=14,
     res=400)
grid.draw(gPage)
dev.off()
}

#=========================================================Recent Positive-only=======================
Residuals=residuals(BestRecPos,type="pearson")
Year    <- R[UKUCPUE>0]$FYEAR
Area    <- R[UKUCPUE>0]$AREA_A
Month   <- R[UKUCPUE>0]$MONTH
Cum_exp <- log(R[UKUCPUE>0]$CUM_EXP)
Speed   <- R[UKUCPUE>0]$SPEED
PC1     <- R[UKUCPUE>0]$PC1
PC2     <- R[UKUCPUE>0]$PC2
sqPC1   <- R[UKUCPUE>0]$PC1^2
sqPC2   <- R[UKUCPUE>0]$PC2^2
data    <- data.table(predicted=predict(BestRecPos),Residuals,Year,Area,Month,Cum_exp,Speed,PC1,PC2,sqPC1,sqPC2) 

glist       <- list()
glist[[1]]  <- ggplot(data=H,aes(x=FYEAR))+geom_line(aes(y=NOMI1.PERC,group=1),color="black",linetype="dotted",size=0.3)+geom_point(aes(y=STAND1.PERC),color="black",size=0.5)+
  geom_smooth(aes(y=STAND1.PERC),span=0.5, color="black",size=0.3)+xlab("Year")+ylab("CPUE % change")+theme_bw()
glist[[2]]  <- ggplot(data=data,aes(x=predicted,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[3]]  <- ggplot(data=data,aes(x=Residuals))+geom_histogram(col="black",fill="white")+theme_bw()
glist[[4]]  <- ggplot(data=data,aes(sample=Residuals))+stat_qq(size=1)+stat_qq_line()+theme_bw()
glist[[5]]  <- ggplot(data=data,aes(x=Year,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[6]]  <- ggplot(data=data,aes(x=Area,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[7]]  <- ggplot(data=data,aes(x=Month,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[8]]  <- ggplot(data=data,aes(x=Speed,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[9]]  <- ggplot(data=data,aes(x=PC1,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[10]] <- ggplot(data=data,aes(x=PC2,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[11]] <- ggplot(data=data,aes(x=sqPC1,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[12]] <- ggplot(data=data,aes(x=sqPC2,y=Residuals))+geom_point(size=0.1)+theme_bw()


if(Gear.name=="DEEP_HANDLINE"){
  gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[6]],glist[[7]],glist[[8]],glist[[9]],glist[[11]])
}else if(Gear.name=="INSHORE_HANDLINE"){
  gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[6]],glist[[7]],glist[[8]],glist[[9]],glist[[10]],glist[[11]],glist[[12]])
}else if(Gear.name=="TROLLING"){
  gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[6]],glist[[8]],glist[[9]],glist[[10]],glist[[11]],glist[[12]])
}


filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUE_DIAG_RecPos.tiff")
tiff(filename=filename, type="cairo", units="cm", compression = "lzw",
     width=17,
     height=14,
     res=400)
grid.draw(gPage)
dev.off()

#==========================================================Recent Presence============================
Residuals<-qresid(BestRecPres)
Year    <- R$FYEAR
Area    <- R$AREA_A
Cum_exp <- log(R$CUM_EXP)
Month   <- R$MONTH
Speed   <- R$SPEED
PC1     <- R$PC1
PC2     <- R$PC2
sqPC1   <- R$PC1^2
sqPC2   <- R$PC2^2
data    <- data.table(predicted=predict(BestRecPres),Residuals,Year,Area,Month,Speed,Cum_exp,PC1,PC2,sqPC1,sqPC2) 

glist <- list()
glist[[1]] <- ggplot(data=H,aes(x=FYEAR))+geom_line(aes(y=NOMI2.PERC,group=1),color="black",linetype="dotted",size=0.3)+geom_point(aes(y=STAND2.PERC),color="black",size=0.5)+
  geom_smooth(aes(y=STAND2.PERC),span=0.5, color="black",size=0.3)+xlab("Year")+ylab("CPUE % change")+theme_bw()
glist[[2]]  <- ggplot(data=data,aes(x=Residuals))+geom_histogram(col="black",fill="white")+theme_bw()
glist[[3]]  <- ggplot(data=data,aes(x=Year,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[4]]  <- ggplot(data=data,aes(x=Area,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[5]]  <- ggplot(data=data,aes(x=Month,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[6]]  <- ggplot(data=data,aes(x=Speed,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[7]]  <- ggplot(data=data,aes(x=Cum_exp,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[8]]  <- ggplot(data=data,aes(x=PC1,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[9]]  <- ggplot(data=data,aes(x=PC2,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[10]] <- ggplot(data=data,aes(x=sqPC1,y=Residuals))+geom_point(size=0.1)+theme_bw()
glist[[11]] <- ggplot(data=data,aes(x=sqPC2,y=Residuals))+geom_point(size=0.1)+theme_bw()



if(Gear.name=="DEEP_HANDLINE"){
  gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[7]],glist[[8]],glist[[9]],glist[[10]],glist[[11]])
}else if(Gear.name=="INSHORE_HANDLINE"){
  gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[8]],glist[[9]],glist[[10]],glist[[11]])
}else if(Gear.name=="TROLLING"){
  gPage <- arrangeGrob(ncol=3, glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]],glist[[6]],glist[[7]],glist[[8]],glist[[9]],glist[[10]],glist[[11]])
}



filename=paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUE_DIAG_RecPres.tiff")
tiff(filename=filename, type="cairo", units="cm", compression = "lzw",
     width=17,
     height=14,
     res=400)
grid.draw(gPage)
dev.off()







