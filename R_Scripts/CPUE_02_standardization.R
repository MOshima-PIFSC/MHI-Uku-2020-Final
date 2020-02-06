require(data.table); require(ggplot2); require(mgcv);  require(MASS); require(dplyr); require(emmeans); require(lme4); require(boot); require(glmmTMB)

Gear.name <- c("DEEP_HANDLINE","INSHORE_HANDLINE","TROLLING")[3]
RES_LIK   <- T

C         <- readRDS(paste0("Outputs/CPUE_",Gear.name,"_StepC.rds"))

C$FYEAR           <- as.character(C$FYEAR)
C$PRES            <- 0
C[UKUCPUE>0]$PRES <- 1

# Arrange datasets for CPUE standardization
O <- C[DATE<"2003-01-01"]
R <- C[DATE>="2003-01-01"&!is.na(SPEED)]

O <- dplyr::select(O,TRIP,FYEAR,MONTH,CUM_EXP,FISHER,LAT,LONG,AREA,AREA_A,AREA_B,AREA_C,PC1,PC2,UKUCPUE,PRES)                 # Old data
R <- dplyr::select(R,TRIP,FYEAR,MONTH,CUM_EXP,FISHER,LAT,LONG,SPEED,XDIR,YDIR,AREA,AREA_A,AREA_B,AREA_C,PC1,PC2,UKUCPUE,PRES) # Recent data


# =============================================================================================================
# =========================FIT STANDARDIZATION GLMs==================================================
# =============================================================================================================
# =============================================================================================================

#==========Old dataset - Positive-only data===============
Models.OldPos  <- list()
Models.OldPos  <- append(Models.OldPos,list(glmmTMB(data=O[UKUCPUE>0],log(UKUCPUE)~1,REML=RES_LIK)         )) 
Models.OldPos  <- append(Models.OldPos,list(glmmTMB(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR,REML=RES_LIK)         )) 
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER),REML=RES_LIK)         ))  
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A,REML=RES_LIK)  ))
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH),REML=RES_LIK)  ))
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A*factor(MONTH),REML=RES_LIK)  ))
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+log(CUM_EXP),REML=RES_LIK)  ))
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+log(CUM_EXP)+PC1,REML=RES_LIK)    ))
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+log(CUM_EXP)+poly(PC1,2,raw=T),REML=RES_LIK)    ))
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+log(CUM_EXP)+poly(PC1,2,raw=T)+PC2,REML=RES_LIK)    ))
Models.OldPos  <- append(Models.OldPos,list(lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+log(CUM_EXP)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),REML=RES_LIK)    ))

# Best model for each CPUE index
if(Gear.name=="DEEP_HANDLINE"){
  BestOldPos     <- lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A*factor(MONTH)+poly(PC1,2,raw=T),REML=RES_LIK)
  Models.OldPos  <- append(Models.OldPos,list(BestOldPos)) 
} else if(Gear.name=="INSHORE_HANDLINE"){
  BestOldPos     <- lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+PC1,REML=RES_LIK)
  Models.OldPos  <- append(Models.OldPos,list(BestOldPos)) 
} else if(Gear.name=="TROLLING"){
  BestOldPos     <- lmer(data=O[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A,REML=RES_LIK)
  Models.OldPos  <- append(Models.OldPos,list(BestOldPos)) 
}

NUM_MOD              <- length(Models.OldPos)
Results.OldPos       <- data.table(TYPE=character(NUM_MOD),MODEL=character(NUM_MOD),AIC=numeric(NUM_MOD),DELTA_AIC=numeric(NUM_MOD),AIC_CHANGE=numeric(NUM_MOD),stringsAsFactors = F)
for(i in 1:NUM_MOD){
  
         Results.OldPos$MODEL[i]      <- paste(formula(Models.OldPos[[i]])[2],"~",formula(Models.OldPos[[i]])[3])
         Results.OldPos$AIC[i]        <- AIC(Models.OldPos[[i]])
  if(i>1)Results.OldPos$AIC_CHANGE[i] <- round((AIC(Models.OldPos[[i-1]])-AIC(Models.OldPos[[i]]))/AIC(Models.OldPos[[i-1]])*100,5)
}
Results.OldPos[1:NUM_MOD]$DELTA_AIC  <- Results.OldPos[1:NUM_MOD]$AIC-min(Results.OldPos[1:NUM_MOD]$AIC,na.rm=T)
Results.OldPos$TYPE <- "Old - Positive"

#=========Old dataset - Presence data=====================
Models.OldPres      <- list()
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~1,family=binomial("logit"))    ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR,family=binomial("logit"))    ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A,family=binomial("logit"))   ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH),family=binomial("logit"))   ))
#Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A*factor(MONTH),family=binomial("logit"))   ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP),family=binomial("logit"))   ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP)+PC1,family=binomial("logit"))   ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP)+poly(PC1,2,raw=T),family=binomial("logit"))   ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP)+poly(PC1,2,raw=T)+PC2,family=binomial("logit"))   ))
Models.OldPres      <- append(Models.OldPres,list(glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),family=binomial("logit"))   ))

if(Gear.name=="DEEP_HANDLINE"){
  BestOldPres     <- glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),family=binomial("logit"))
  Models.OldPres  <- append(Models.OldPres,list(BestOldPres)) 
} else if(Gear.name=="INSHORE_HANDLINE"){
  BestOldPres     <- glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP)+PC1+PC2,family=binomial("logit"))
  Models.OldPres  <- append(Models.OldPres,list(BestOldPres)) 
} else if(Gear.name=="TROLLING"){
  BestOldPres     <- glm(data=O,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP)+PC1+PC2,family=binomial("logit"))
  Models.OldPres  <- append(Models.OldPres,list(BestOldPres)) 
}

NUM_MOD              <- length(Models.OldPres)
Results.OldPres       <- data.table(TYPE=character(NUM_MOD),MODEL=character(NUM_MOD),AIC=numeric(NUM_MOD),DELTA_AIC=numeric(NUM_MOD),AIC_CHANGE=numeric(NUM_MOD),stringsAsFactors = F)
for(i in 1:NUM_MOD){
  
  Results.OldPres$MODEL[i]      <- paste(formula(Models.OldPres[[i]])[2],"~",formula(Models.OldPres[[i]])[3])
  Results.OldPres$AIC[i]        <- AIC(Models.OldPres[[i]])
  if(i>1)Results.OldPres$AIC_CHANGE[i] <- round((AIC(Models.OldPres[[i-1]])-AIC(Models.OldPres[[i]]))/AIC(Models.OldPres[[i-1]])*100,5)
}
Results.OldPres[1:NUM_MOD]$DELTA_AIC  <- Results.OldPres[1:NUM_MOD]$AIC-min(Results.OldPres[1:NUM_MOD]$AIC,na.rm=T)
Results.OldPres$TYPE <- "Old - Presence"

#===========Recent dataset - Positive-only data=====================

Models.RecPos  <- list()
Models.RecPos  <- append(Models.RecPos,list(glmmTMB(data=R[UKUCPUE>0],log(UKUCPUE)~1,REML=RES_LIK)         ))  
Models.RecPos  <- append(Models.RecPos,list(glmmTMB(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR,REML=RES_LIK)         ))  
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER),REML=RES_LIK)         ))    
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A,REML=RES_LIK)  ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH),REML=RES_LIK)  ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A*factor(MONTH),REML=RES_LIK)  ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED,REML=RES_LIK)  ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP),REML=RES_LIK)  ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+PC1,REML=RES_LIK)    ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+poly(PC1,2,raw=T),REML=RES_LIK)    ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+poly(PC1,2,raw=T)+PC2,REML=RES_LIK)    ))
Models.RecPos  <- append(Models.RecPos,list(lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),REML=RES_LIK)    ))


if(Gear.name=="DEEP_HANDLINE"){
  BestRecPos     <- lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED+poly(PC1,2,raw=T),REML=RES_LIK)
  Models.RecPos  <- append(Models.RecPos,list(BestRecPos)) 
} else if(Gear.name=="INSHORE_HANDLINE"){
  BestRecPos     <- lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+factor(MONTH)+SPEED+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),REML=RES_LIK)
  Models.RecPos  <- append(Models.RecPos,list(BestRecPos)) 
} else if(Gear.name=="TROLLING"){
  BestRecPos     <- lmer(data=R[UKUCPUE>0],log(UKUCPUE)~FYEAR+(1|FISHER)+AREA_A+SPEED+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),REML=RES_LIK)
  Models.RecPos  <- append(Models.RecPos,list(BestRecPos)) 
}

NUM_MOD              <- length(Models.RecPos)
Results.RecPos       <- data.table(TYPE=character(NUM_MOD),MODEL=character(NUM_MOD),AIC=numeric(NUM_MOD),DELTA_AIC=numeric(NUM_MOD),AIC_CHANGE=numeric(NUM_MOD),stringsAsFactors = F)
for(i in 1:NUM_MOD){
  
  Results.RecPos$MODEL[i]      <- paste(formula(Models.RecPos[[i]])[2],"~",formula(Models.RecPos[[i]])[3])
  Results.RecPos$AIC[i]        <- AIC(Models.RecPos[[i]])
  if(i>1)Results.RecPos$AIC_CHANGE[i] <- round((AIC(Models.RecPos[[i-1]])-AIC(Models.RecPos[[i]]))/AIC(Models.RecPos[[i-1]])*100,5)
}
Results.RecPos[1:NUM_MOD]$DELTA_AIC  <- Results.RecPos[1:NUM_MOD]$AIC-min(Results.RecPos[1:NUM_MOD]$AIC,na.rm=T)
Results.RecPos$TYPE <- "Recent - Positive"

#================Recent dataset - Presence data======================
Models.RecPres      <- list()
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~1,family=binomial("logit"))    ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR,family=binomial("logit"))    ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A,family=binomial("logit"))   ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH),family=binomial("logit"))   ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+SPEED,family=binomial("logit"))   ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP),family=binomial("logit"))   ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+PC1,family=binomial("logit"))   ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+poly(PC1,2,raw=T),family=binomial("logit"))   ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+poly(PC1,2,raw=T)+PC2,family=binomial("logit"))   ))
Models.RecPres      <- append(Models.RecPres,list(glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),family=binomial("logit"))   ))

if(Gear.name=="DEEP_HANDLINE"){
  BestRecPres     <- glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+log(CUM_EXP)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),family=binomial("logit"))
  Models.RecPres  <- append(Models.RecPres,list(BestRecPres)) 
} else if(Gear.name=="INSHORE_HANDLINE"){
  BestRecPres     <- glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),family=binomial("logit"))
  Models.RecPres  <- append(Models.RecPres,list(BestRecPres)) 
} else if(Gear.name=="TROLLING"){
  BestRecPres     <- glm(data=R,PRES~FYEAR+AREA_A+factor(MONTH)+SPEED+log(CUM_EXP)+poly(PC1,2,raw=T)+poly(PC2,2,raw=T),family=binomial("logit")) 
  Models.RecPres  <- append(Models.RecPres,list(BestRecPres)) 
}

NUM_MOD              <- length(Models.RecPres)
Results.RecPres       <- data.table(TYPE=character(NUM_MOD),MODEL=character(NUM_MOD),AIC=numeric(NUM_MOD),DELTA_AIC=numeric(NUM_MOD),AIC_CHANGE=numeric(NUM_MOD),stringsAsFactors = F)
for(i in 1:NUM_MOD){
  
  Results.RecPres$MODEL[i]      <- paste(formula(Models.RecPres[[i]])[2],"~",formula(Models.RecPres[[i]])[3])
  Results.RecPres$AIC[i]        <- AIC(Models.RecPres[[i]])
  if(i>1)Results.RecPres$AIC_CHANGE[i] <- round((AIC(Models.RecPres[[i-1]])-AIC(Models.RecPres[[i]]))/AIC(Models.RecPres[[i-1]])*100,5)
}
Results.RecPres[1:NUM_MOD]$DELTA_AIC  <- Results.RecPres[1:NUM_MOD]$AIC-min(Results.RecPres[1:NUM_MOD]$AIC,na.rm=T)
Results.RecPres$TYPE <- "Recent - Presence"

# Put results together and define the best model for each CPUE component
Results.Models <- rbind(Results.OldPos,Results.OldPres,Results.RecPos,Results.RecPres)
write.csv(Results.Models,paste0("Outputs/Graphs/CPUE/",Gear.name,"/CPUE models_",Gear.name,".csv"),row.names=F)

# Save best models
M <- list(BestOldPos,BestOldPres,BestRecPos,BestRecPres)
saveRDS(M,paste0("Outputs","/CPUE models_",Gear.name,".rds"))

