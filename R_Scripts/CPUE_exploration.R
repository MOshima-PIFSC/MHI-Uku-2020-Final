require(ggplot2); require(data.table); require(dplyr); require(gridExtra)

Gear.name <- c("DEEP_HANDLINE","INSHORE_HANDLINE","TROLLING")[1]

P <- readRDS(paste0("Outputs/CPUE_",Gear.name,"_StepC.rds"))

# Arrange datasets for CPUE standardization
O <- P[DATE<"2003-01-01"]
R <- P[DATE>="2003-01-01"]

O <- dplyr::select(O,TRIP,FYEAR,MONTH,CUM_EXP,LAT,LONG,AREA_B,AREA_C,PC1,PC2,PC3,PC4,UKUCPUE)                 # Old data
R <- dplyr::select(R,TRIP,FYEAR,MONTH,CUM_EXP,LAT,LONG,AREA_B,AREA_C,PC1,PC2,PC3,PC4,UKUCPUE) # Recent data

# Nominal CPUE exploration by sector
aTheme <- theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=8),legend.title=element_blank())

graph.list <- list()
for(ar in 1:5){
  
  area.name <- unique(O$AREA_C)[ar]
  
  OldData      <- O[AREA_C==area.name,list(UKUCPUE=mean(UKUCPUE)),by=list(FYEAR)]
  OldData$PERC <- OldData$UKUCPUE/mean(OldData$UKUCPUE)*100
  RecData      <- R[AREA_C==area.name,list(UKUCPUE=mean(UKUCPUE)),by=list(FYEAR)]
  RecData$PERC <- RecData$UKUCPUE/mean(RecData$UKUCPUE)*100  #Recent data tied to last years of old data, for continuity.
  aData <- rbind(OldData,RecData)
  
  graph.list[[ar]] <- ggplot(data=aData,aes(x=FYEAR,y=PERC))+geom_point()+
    geom_smooth(span=0.6)+ggtitle(paste0("Nominal uku CPUE at ",area.name))+aTheme
}

# Explore overall pattern
OldData      <- O[,list(UKUCPUE=mean(UKUCPUE)),by=list(FYEAR)]
OldData$PERC <- OldData$UKUCPUE/mean(OldData$UKUCPUE)*100
RecData      <- R[,list(UKUCPUE=mean(UKUCPUE)),by=list(FYEAR)]
RecData$PERC <- RecData$UKUCPUE/mean(RecData$UKUCPUE)*100  #Recent data tied to last years of old data, for continuity.
aData        <- rbind(OldData,RecData)

graph.list[[6]] <- ggplot(data=aData,aes(x=FYEAR,y=PERC))+geom_point()+
  geom_smooth(span=0.6)+ggtitle(paste0("Nominal uku CPUE all areas "))+aTheme

# Explore MONTH patterns
GR15 <- ggplot(data=O[,list(UKUCPUE=mean(UKUCPUE)),by=list(AREA_C,MONTH)])+geom_line(aes(x=MONTH,y=UKUCPUE,color=AREA_C))+
  ggtitle("Nominal CPUE by month and area 1948-2002: uku")+aTheme
GR16 <- ggplot(data=R[,list(UKUCPUE=mean(UKUCPUE)),by=list(AREA_C,MONTH)])+geom_line(aes(x=MONTH,y=UKUCPUE,color=AREA_C))+
  ggtitle("Nominal CPUE by month and area 2003-2018: uku")+aTheme

# Cumulative experience patterns

range(O$CUM_EXP)
range(R$CUM_EXP)
hist(O$CUM_EXP)
hist(R$CUM_EXP)

O$CUM_EXP_CAT <- character()
O[CUM_EXP>=0&CUM_EXP<50]$CUM_EXP_CAT    <- "025"
O[CUM_EXP>=50&CUM_EXP<100]$CUM_EXP_CAT  <- "075"
O[CUM_EXP>=100&CUM_EXP<200]$CUM_EXP_CAT <- "150"
O[CUM_EXP>=200&CUM_EXP<300]$CUM_EXP_CAT <- "250"
O[CUM_EXP>=300&CUM_EXP<400]$CUM_EXP_CAT <- "350"
O[CUM_EXP>=400]$CUM_EXP_CAT             <- "450"

R$CUM_EXP_CAT <- character()
R[CUM_EXP>=0&CUM_EXP<50]$CUM_EXP_CAT    <- "025"
R[CUM_EXP>=50&CUM_EXP<100]$CUM_EXP_CAT  <- "075"
R[CUM_EXP>=100&CUM_EXP<200]$CUM_EXP_CAT <- "150"
R[CUM_EXP>=200&CUM_EXP<300]$CUM_EXP_CAT <- "250"
R[CUM_EXP>=300&CUM_EXP<400]$CUM_EXP_CAT <- "350"
R[CUM_EXP>=400]$CUM_EXP_CAT             <- "450"

GR17 <- ggplot(data=O[AREA_C=="Penguin"&UKUCPUE>0],aes(x=CUM_EXP_CAT,y=UKUCPUE))+geom_boxplot(outlier.shape=NA)+ylim(0,50)+
  ggtitle("Pos. CPUE by experience 1948-2002: Penguin")+aTheme
GR18 <- ggplot(data=R[AREA_C=="Penguin"&UKUCPUE>0],aes(x=CUM_EXP_CAT,y=UKUCPUE))+geom_boxplot(outlier.shape=NA)+ylim(0,10)+
  ggtitle("Pos. CPUE by experience 2002-2018: Penguin")+aTheme

#=======Print everything out===============================

for(i in 15:18){  
  
  fig      <- paste0("GR",i) 
  filename <- file.path(paste0("Outputs/Graphs/CPUE/",Gear.name),paste0("FIG",formatC(i,width=2,flag="0"),".tiff"))
  ggsave(eval(parse(text=fig)), file=filename, width = 14, height = 8, units = "cm",dpi=150)  
}


for(i in 1:6){  
  
  filename <- file.path(paste0("Outputs/Graphs/CPUE/",Gear.name),paste0("NOMCPUEFIG",formatC(i,width=2,flag="0"),".tiff"))
  ggsave(graph.list[[i]], file=filename, width = 14, height = 8, units = "cm",dpi=150)  
}



test      <- P
test$TRIP <- paste0(test$DATE,test$FISHER)

test <- P[UKUCPUE>0,list(UKUCPUE=sum(UKUCPUE)),by=list(FYEAR,TRIP)]
test <- P[UKUCPUE>0,list(UKUCPUE=mean(UKUCPUE)),by=list(FYEAR)]

ggplot(data=test)+geom_line(aes(x=FYEAR,y=UKUCPUE))




