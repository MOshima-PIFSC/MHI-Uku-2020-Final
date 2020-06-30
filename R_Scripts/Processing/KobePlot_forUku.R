library(r4ss) 
 
 ##Needs to be the raw F model
 #model_dir <- "C:\\Users\\michelle.sculley\\Documents\\Uku\\021_Base\\rawF"    #<----------------
 model_dir <- "C:\\Users\\Marc.Nadon\\Documents\\Work docs\\01_Projects\\002_Uku assessment\\0_R_Uku\\SS3\\026_Base"    #<----------------
 
 
 
 setwd(model_dir)
 base.model<-SS_output(getwd())
 # Kobe plot layout setting
 ## Adjust as needed
 x_max = 3.5
 x_min = 0
 y_max = 1.5
 y_min = 0
 MSST_x = 0.9
 max_yr=2018
 
 ## Overfished triangles/trapezoids
 
tri_y<-c(y_min,1,y_min)  ##currently MSST is set to 0.9Bmsy, but would be different depending on rebuilding projections
tri_x<-c(x_min,MSST_x,MSST_x)

poly_y<-c(y_min,y_max,y_max,1)
poly_x<-c(x_min,x_min,MSST_x,MSST_x)
   
 rnames <- base.model$derived_quants$Label

 index_SSB_MSY = which(rnames==paste("SSB_MSY",sep=""))
 index_Fstd_MSY = which(rnames==paste("Fstd_MSY",sep=""))
 
 year_vec = min(base.model$sprseries$Yr):max_yr
 
 index_SSB_TermYr = which(rnames==paste("SSB_",max_yr,sep=""))
 index_Fstd_TermYr = which(rnames==paste("F_",max_yr,sep=""))
 
 ##mean and SD of MSY values
 SSB_MSY_est = base.model$derived_quants[index_SSB_MSY:index_SSB_MSY,2]
 Fstd_MSY_est = base.model$derived_quants[index_Fstd_MSY:index_Fstd_MSY,2]
 
 SSB_MSY_SD = base.model$derived_quants[index_SSB_MSY:index_SSB_MSY,3]
 Fstd_MSY_SD = base.model$derived_quants[index_Fstd_MSY:index_Fstd_MSY,3]
 
 #mean and SD of terminal year values
 SSB_TermYr_est = base.model$derived_quants[index_SSB_TermYr:index_SSB_TermYr,2]
 Fstd_TermYr_est = base.model$derived_quants[index_Fstd_TermYr:index_Fstd_TermYr,2]
 
 SSB_TermYr_SD = base.model$derived_quants[index_SSB_TermYr:index_SSB_TermYr,3]
 Fstd_TermYr_SD = base.model$derived_quants[index_Fstd_TermYr:index_Fstd_TermYr,3]
 
 #Time series of ratios
 SSBratio = base.model$sprseries$SSB/SSB_MSY_est
 Fratio = base.model$sprseries$F_report/Fstd_MSY_est
 
 nyears=length(Fratio)-1
 
##SD of ratios in the terminal year
  ### Fration terminal year * sqrt((sd terminal year/F terminal year)^2+(sd Fmsy/Fmsy)^2)
 ### Fration terminal year * sqrt((sd terminal year/B terminal year)^2+(sd Bmsy/Bmsy)^2)

 Fstd<-Fratio[nyears]*sqrt((Fstd_TermYr_SD/Fstd_TermYr_est)^2+(Fstd_MSY_SD/Fstd_MSY_est)^2)
 SSBstd<-SSBratio[nyears]*sqrt((SSB_TermYr_SD/SSB_TermYr_est)^2+(SSB_MSY_SD/SSB_MSY_est)^2)
 
 Fratio_95<-Fratio[nyears]+1.96*Fstd
 Fratio_05<-Fratio[nyears]-1.96*Fstd
 SSBratio_95<-SSBratio[nyears]+1.96*SSBstd
 SSBratio_05<-SSBratio[nyears]-1.96*SSBstd
 
 
png("KobePlot.png",height=10,width = 10, units="in",res=300) 
 plot(c(x_min,x_max),c(y_min,y_max),type="n", ylab="", xlab="")
 mtext(side=1, expression(SSB/SSB[MSY]),line=2.5, cex=1)  
 mtext(side=2, expression(F/F[MSY]),line=2.5, cex=1)  
 
 polygon(tri_x, tri_y,col="khaki1") 
 polygon(c(MSST_x,x_max,x_max,MSST_x), c(1,1,y_min,y_min),col="palegreen")
 polygon(poly_x, poly_y,col="salmon") 
 polygon(c(MSST_x,x_max,x_max,MSST_x), c(1,1,y_max,y_max),col="khaki1")
 segments(1,0,1,1)

 points(SSBratio[1:nyears],Fratio[1:nyears],type="o",bg="black",pch=21,col="black",cex=1.2)
 
 points(SSBratio[1],Fratio[1],type="o",bg="white",pch=21,col="white",cex=1.2)
 points(SSBratio[nyears],Fratio[nyears],type="o",bg="orange",pch=21,col="orange",cex=1.2)
 #points(SSBratio[length(year_vec)],Fratio[length(year_vec)],type="o",bg="blue",pch=21,col="blue",cex=1.2)
 
 
 
 #SSBratio 
 #Fratio 0.693362522	1.034111657

 
 
 points(c(SSBratio[nyears],SSBratio[nyears])
 ,c(Fratio_05,Fratio_95)
 ,type="l",lwd=2,lty=3)
 
 
 points(c(SSBratio_95,SSBratio_05)
 ,c(Fratio[nyears],Fratio[nyears])
 ,type="l",lwd=2,lty=3)
 
 points(SSBratio[nyears],Fratio[nyears],type="o",bg="orange",pch=21,col="orange",cex=1.2)
 
 ##adjust as necessary
  text(SSBratio[1],Fratio[1],labels=year_vec[1],cex = 1,adj = c(0.1,-0.5))
  text(SSBratio[nyears],Fratio[nyears],labels=year_vec[nyears],cex = 1,adj = c(-0.3,-0.5))
  text(MSST_x,0.1,labels=c("MSST"),cex=1,adj=c(1.1,0))
  text(1,0.1,labels=c(expression(SSB[MSY])),cex=1,adj=c(-0.2,0))
 # text(SSBratio[19],Fratio[19],labels=year_vec[19],cex = 1, pos=3)
  
 dev.off()
 # legend
 # legend(x=x_max-1,y=y_max, 
 #        legend=c(min(year_vec),max(year_vec)-1),
 #        col=c(1), lwd=1, lty=c(1,1), 
 #        pch=c(NA,NA),border=NULL,box.lty=0,bg=NULL)
 # 
 # legend(x=x_max-1,y=y_max, 
 #        legend=c(min(year_vec),max(year_vec)-1),
 #        col=c("white","orange"), lwd=1, lty=c(0,0), 
 #        pch=c(19,19),fill=NULL)
 # 
 # 
