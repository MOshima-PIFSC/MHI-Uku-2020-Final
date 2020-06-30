#============Catch exploration=============================
require(ggplot2); require(data.table); require(reshape2); require(dplyr); require(gridExtra); require(grid); require(RColorBrewer)

# Load catch data and process
C <- readRDS("Outputs/CATCH_processed.rds")
C <- C[AREA_D=="MHI"] # Filter invalid and NWHI areas
C$TRIP <- paste(C$DATE,C$LICENSE,sep=".")

C[GEAR_A=="NET"|GEAR_A=="OTHER_LINE"|GEAR_A=="SPEAR"]$GEAR_A <- "OTHER"

# Sum LBS in the dataset so that species are only mentioned once per record
C <- C[,list(LBS=sum(LBS)),by=list(DATE,MONTH,FYEAR,FISHER,LICENSE,TRIP,AREA,AREA_A,AREA_B,AREA_C,AREA_D,GEAR,GEAR_A,HOURS,PORT_LAND,SPECIES)]

# Classify all offhsore area together
C[AREA_C!="Oahu"&AREA_C!="Hawaii"&AREA_C!="MauiNui"&AREA_C!="Penguin"&AREA_C!="Kauai"]$AREA_C <- "Offshore" # Classify offshore areas
sum(C[AREA_C=="Offshore"&SPECIES==20]$LBS)/sum(C[SPECIES==20]$LBS) # Offshore catch are <1% of the uku catch

C <- C[AREA_C!="Offshore"] # Remove offshore areas to simplify spatial analysis

E <- C                # Full dataset (all species and gear)
C <- C[SPECIES==20]   # Just uku dataset

# Quick exploration of effort data to calculate E/E'
EFFORT1 <- E[,list(HOURS=sum(HOURS)),by=list(FYEAR, GEAR_A,TRIP)]
EFFORT2 <- EFFORT1[FYEAR>=2003,list(HOURS=sum(HOURS)),by=list(FYEAR,GEAR_A)]

CPUE1 <- E[SPECIES==20&FYEAR>=2003,list(UKULBS=sum(LBS)),by=list(FYEAR,GEAR_A,TRIP)]

trips <- E[FYEAR>=2003,list(LBS=sum(LBS)),by=list(FYEAR,GEAR_A,TRIP)]
trips$LBS <- 0

test <- merge(CPUE1,trips,by=c("FYEAR","GEAR_A","TRIP"),all.y=T)
test <- test[,list(UKULBS=sum(UKULBS)),by=list(FYEAR,GEAR_A)]

# Global effort patterns
E  <- E[,list(LBS=sum(LBS)),by=list(FYEAR,MONTH,DATE,TRIP,GEAR_A,AREA_C)]
E  <- E[,list(NUM_REP=.N),by=list(FYEAR,MONTH,GEAR_A,AREA_C)]

aTheme <- theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=6),legend.title=element_blank())

GR1 <- ggplot(data=E[,list(NUM_REP=sum(NUM_REP)),by=list(FYEAR,GEAR_A)],aes(x=FYEAR,y=NUM_REP,fill=GEAR_A))+scale_fill_brewer(palette="Paired")+
  geom_area(position="stack")+ggtitle("Gear-specific effort by year: all species")+aTheme

GR2 <- ggplot(data=E[,list(NUM_REP=sum(NUM_REP)),by=list(FYEAR,GEAR_A)],aes(x=FYEAR,y=NUM_REP,fill=GEAR_A))+scale_fill_brewer(palette="Paired")+
  geom_area(position=position_fill())+ggtitle("Gear-specific prop. effort by year: all species")+aTheme

GR3  <- ggplot(data=E[GEAR_A=="DEEP_HANDLINE",list(NUM_REP=sum(NUM_REP)),by=list(FYEAR,AREA_C)],aes(x=FYEAR,y=NUM_REP,fill=AREA_C))+
  scale_fill_brewer(palette="Paired")+geom_area(position="stack")+ggtitle("Deep handline effort by year: all species")+aTheme

GR4 <- ggplot(data=E[GEAR_A=="DEEP_HANDLINE",list(NUM_REP=sum(NUM_REP)),by=list(FYEAR,AREA_C)],aes(x=FYEAR,y=NUM_REP,fill=AREA_C))+
  scale_fill_brewer(palette="Paired")+geom_area(position=position_fill())+ggtitle("Deep handline effort by year: all species")+aTheme


# Uku-specific catch and effort patterns
# (note: the effort metric could be impacted by trends in uku abundance since I'm filtering for non-zero trip only)

EU  <- C[,list(LBS=sum(LBS)),by=list(FYEAR,MONTH,DATE,TRIP,GEAR_A,AREA_C)]
EU  <- EU[,list(NUM_REP=.N,LBS=sum(LBS)),by=list(FYEAR,MONTH,GEAR_A,AREA_C)]

TOT <- EU[,list(NUM_REP_T=sum(NUM_REP),LBS_T=sum(LBS)),by=list(FYEAR)]
TOT <- TOT[order(FYEAR)]

# GEAR exploration
G <- EU[,list(NUM_REP=sum(NUM_REP),LBS=sum(LBS)),by=list(FYEAR,GEAR_A)]

GR5 <- ggplot(data=G)+geom_area(aes(x=FYEAR,y=NUM_REP,fill=GEAR_A),position="stack")+
  scale_fill_brewer(palette="Paired")+ggtitle("Gear-specific effort by year: uku")+aTheme
GR6 <- ggplot(data=G)+geom_area(aes(x=FYEAR,y=NUM_REP,fill=GEAR_A),position=position_fill())+
  scale_fill_brewer(palette="Paired")+ggtitle("Gear-specific prop. effort by year: uku")+aTheme

GR7 <- ggplot(data=G)+geom_area(aes(x=FYEAR,y=LBS,fill=GEAR_A),position="stack")+
  scale_fill_brewer(palette="Paired")+ggtitle("Gear-specific catch by year: uku")+aTheme
GR8 <- ggplot(data=G)+geom_area(aes(x=FYEAR,y=LBS,fill=GEAR_A),position=position_fill())+
  scale_fill_brewer(palette="Paired",labels = c("Deep-sea handline","Inshore handline","Other","Trap","Trolling"))+ggtitle("Gear-specific prop. catch by year: uku")+
  xlab("Year")+ylab("Prop. commercial catch")+
  scale_x_continuous(limits=c(1948,2018),expand=c(0,0),breaks=seq(1950,2010,10))+scale_y_continuous(expand=c(0,0))+aTheme

# AREA exploration
AR <- EU[,list(NUM_REP=sum(NUM_REP),LBS=sum(LBS)),by=list(FYEAR,AREA_C)]

GR9 <- ggplot(data=AR)+geom_area(aes(x=FYEAR,y=LBS,fill=AREA_C),position="stack")+scale_fill_brewer(palette="Paired")+
  ggtitle("Area-specific catch by year: uku")+aTheme
GR10 <- ggplot(data=AR)+geom_area(aes(x=FYEAR,y=LBS,fill=AREA_C),position=position_fill())+
  scale_fill_brewer(palette="Paired",labels = c("Hawaii","Kauai-Niihau","Maui Nui - Others","Oahu","Maui Nui - Penguin"))+
  ggtitle("Area-specific prop. catch by year: uku")+
  xlab("Year")+ylab("Prop. commercial catch")+
  scale_x_continuous(limits=c(1948,2018),expand=c(0,0),breaks=seq(1950,2010,10))+scale_y_continuous(expand=c(0,0))+aTheme

# Some Penguin Bank exploration plots
GR11 <- ggplot(data=E[AREA_C=="Penguin",list(NUM_REP=sum(NUM_REP)),by=list(FYEAR,GEAR_A)],aes(x=FYEAR,y=NUM_REP,color=GEAR_A))+
  geom_line()+ggtitle("Gear-specifc effort by year at Penguin Bank: all species")+aTheme

GR12 <- ggplot(data=EU[AREA_C=="Penguin",list(NUM_REP=sum(NUM_REP)),by=list(FYEAR,GEAR_A)],aes(x=FYEAR,y=NUM_REP,color=GEAR_A))+
  geom_line()+ggtitle("Gear-specific effort by year at Penguin Bank: uku")+aTheme

#==========What happened in 1987-1991?===============

H <- C[FYEAR>=1980&FYEAR<=2000]

# Penguin and Maui Nui are where the peak in catch was:
GR13 <- ggplot(data=H[AREA_C=="Penguin"|AREA_C=="MauiNui",list(LBS=sum(LBS)),by=list(FYEAR,AREA_B)])+geom_line(aes(x=FYEAR,y=LBS,color=AREA_B))+
  ggtitle("Catch around Maui Nui and Penguin Bank: uku")+aTheme

# Who where the top fishers?
TOP.FISHERS <- H[AREA_C=="Penguin"&(FYEAR>=1985&FYEAR<=1992),list(LBS=sum(LBS)),by=list(FISHER)]
TOP.FISHERS <- TOP.FISHERS[order(-LBS)][1:20,]$FISHER

FINFO <- H[(AREA_C=="Penguin")&FISHER%in%TOP.FISHERS,list(LBS=sum(LBS)),by=list(FISHER,FYEAR)]
FINFO <- FINFO[order(FISHER,FYEAR)]

GR14 <- ggplot(data=FINFO,aes(x=FYEAR,y=LBS,fill=FISHER))+ggtitle("Catch of top 20 uku fishers on Penguin")+
  geom_bar(position="stack",stat="identity")+aTheme+theme(legend.position = "none")

for(i in 1:14){  
  
  fig      <- paste0("GR",i) 
  filename <- file.path("Outputs/Graphs/Catch",paste0("FIG",formatC(i,width=2,flag="0"),".tiff"))
  ggsave(eval(parse(text=fig)), file=filename, width = 14, height = 8, units = "cm",dpi=150)  
}





