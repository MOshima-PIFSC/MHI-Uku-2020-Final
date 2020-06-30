require(ggplot2)

INP <- read.delim("C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\SS3\\026_Base\\Report.SSO", 
                  fill=FALSE, stringsAsFactors = FALSE, header=FALSE)

SSB <- matrix(   unlist(strsplit(INP[641:713,],split=" ")),ncol=4,nrow=73,byrow=T    )
SSB <- as.data.frame( SSB[,-1], stringsAsFactors=F )
colnames(SSB) <- c("Year","SSB","SD")
SSB[,2] <- as.numeric(SSB[,2])

REC <- matrix(   unlist(strsplit(INP[714:786,],split=" ")),ncol=4,nrow=73,byrow=T    )
REC <- as.data.frame( REC[,-1], stringsAsFactors=F )
colnames(REC) <- c("Year","REC","SD")
REC[,2] <- as.numeric(REC[,2])

SSB_VIRG <- SSB[1,2]
REC_VIRG <- REC[1,2]

SSB <- SSB[3:nrow(SSB),1:2]
SSB$Year <- seq(1948,2018,1)

REC <- REC[3:nrow(REC),1:2]
REC$Year <- seq(1948,2018,1)

DAT <- merge(SSB,REC,by="Year")

alpha <- 81.1
beta  <- 78.6

SR_plot <- ggplot(data=DAT)+scale_x_continuous(limits=c(0,SSB_VIRG+20),expand=c(0,0))+scale_y_continuous(limits=c(0,150),expand=c(0,0))+
  stat_function(fun=function(x) alpha*x/(beta+x),xlim=c(0,SSB_VIRG))+
  geom_point(aes(x=SSB,y=REC,col=Year),size=2)+geom_point(aes(x=SSB_VIRG,y=REC_VIRG),size=4,col="red",shape=18)+
  scale_color_gradientn(colors=rainbow(4))+theme_bw()+xlab("Spawning biomass (SSB; metric tons)")+ylab("Recruitment (1000 recruits)")

ggsave(filename="SR relation.png",plot=SR_plot,width=18,height=10,units="cm",dpi=300)

