Get_KobeScenarios <- function(Model.List,Label.Vect,plotdir,NModels){

library(ggplot2);library(ggsci);
  
# Kobe plot layout setting
## Adjust as needed

max_yr=2018

Results       <- setNames(data.frame(matrix(ncol = 3, nrow = NModels)), c("Model", "B_Bmsy", "F_Fmsy"))
Results$Model <- Label.Vect

     rnames <- Model.List$quants$Label
     index_SSB_MSY = which(rnames==paste("SSB_MSY",sep=""))
     index_Fstd_MSY = which(rnames==paste("Fstd_MSY",sep=""))
     index_SSB_TermYr = which(rnames==paste("SSB_",max_yr,sep=""))
     index_Fstd_TermYr = which(rnames==paste("F_",max_yr,sep=""))
     SSB_MSY_est  = Model.List$quants[index_SSB_MSY:index_SSB_MSY,1:NModels]
     Fstd_MSY_est = Model.List$quants[index_Fstd_MSY:index_Fstd_MSY,1:NModels]
     SSB_TermYr_est  = Model.List$quants[index_SSB_TermYr:index_SSB_TermYr,1:NModels]
     Fstd_TermYr_est = Model.List$quants[index_Fstd_TermYr:index_Fstd_TermYr,1:NModels]
     Results$B_Bmsy <- melt(SSB_TermYr_est/SSB_MSY_est)[,"value"]
     Results$F_Fmsy <- melt(Fstd_TermYr_est/Fstd_MSY_est)[,"value"]

     x_max = 3
     x_min = 0
     y_max = max(max(Results$F_Fmsy)+0.1,1.45)
     y_min = 0
     MSST_x = 0.9

## Create Kobe plot
## Overfished triangles/trapezoids
tri_y<-c(y_min,1,y_min)  ##currently MSST is set to 0.9Bmsy, but would be different depending on rebuilding projections
tri_x<-c(x_min,MSST_x,MSST_x)

poly_y<-c(y_min,y_max,y_max,1)
poly_x<-c(x_min,x_min,MSST_x,MSST_x)

shapes <- c(16,17,18,4,3)

aPlot <- ggplot()+ylab(expression(F/F[MSY]))+xlab(expression(SSB/SSB[MSY]))+
  scale_x_continuous(expand=c(0,0),limits=c(x_min,x_max))+scale_y_continuous(expand=c(0,0),limits=c(y_min,y_max))+
  geom_polygon(aes(x=tri_x,y=tri_y),fill="khaki1",col="black",size=1)+
  geom_polygon(aes(x=c(MSST_x,x_max,x_max,MSST_x), y=c(1,1,y_min,y_min)),fill="palegreen",col="black",size=1)+
  geom_polygon(aes(x=poly_x, y=poly_y),fill="salmon",col="black",size=1)+
  geom_polygon(aes(x=c(MSST_x,x_max,x_max,MSST_x), y=c(1,1,y_max,y_max)),fill="khaki1",col="black",size=1)+
  geom_segment(aes(x=1,y=0,xend=1,yend=1),size=1)+annotate("text",x=MSST_x-0.2,y=0.1,label="MSST")+
  annotate("text",x=1.3,y=0.1,label=expression(SSB[MSY]))+
  geom_line(aes(x=Results$B_Bmsy,y=Results$F_Fmsy,shape=Results$Model,col=Results$Model),size=1.5)+ # To have lines on the legend which is used by 4 graphs
  geom_point(aes(x=Results$B_Bmsy,y=Results$F_Fmsy,shape=Results$Model,col=Results$Model),size=4)+
  scale_shape_manual(values=shapes)+
  scale_color_jco()+
  theme_bw(base_size=20)+theme(legend.title=element_blank(),legend.position="right")

#ggsave(paste0(plotdir,"\\KobePlot.png"),height=5,width=5,units="in",dpi=300)
return(aPlot)
}
