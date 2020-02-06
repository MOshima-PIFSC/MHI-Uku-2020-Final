
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# ss3runsFun.R
# Residual diagnostic plots with Runs Tests for Stock Sythesis
# Designed for r4ss objects of report.sso files
# Developed by Henning Winker and Felipe Carvalho 2019
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>


# Function to install and/load load missing packages
get_runslibs = function(){
  list.of.packages <- c("reshape2","kableExtra","tseries","dplyr","devtools")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  if("r4ss" %in% installed.packages()[,"Package"]==FALSE) devtools::install_github("r4ss/r4ss")
  library(r4ss); library(tseries); library(dplyr);library(reshape2)}

#---------------------
# Index color palette
#---------------------
jabba.colors = as.character(c('#e6194b', "#3cb44b", "#ffe119",
                              "#0082c8","#f58231", "#911eb4",
                              "#46f0f0", "#f032e6", "#d2f53c",
                              "#fabebe", "#008080","#e6beff", "#aa6e28",rainbow(12)[seq(1,12,3)],rainbow(12)[seq(2,12,3)],rainbow(12)[seq(3,12,3)]))
cols = jabba.colors

#-------------------------------------------------
# Function to do runs.test and 3 x sigma limits  
#------------------------------------------------

runs.sig3 <- function(x,type="resid") {
  if(type=="resid"){mu = 0}else{mu = mean(x, na.rm = TRUE)} 
  # Average moving range
  mr  <- abs(diff(x - mu))
  amr <- mean(mr, na.rm = TRUE)
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128
  # Calculate control limits
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  if(nlevels(factor(sign(x)))>1){ pvalue = round(runs.test(factor(sign(x)))$p.value,3)} else {
    pvalue = 0.001  
  }
  
  return(list(sig3lim=c(lcl,ucl),p.runs= pvalue))
}

#-------------------------------------------------------------------
# runs.tests plot function 
#------------------------------------------------------------------

plot_ss3runs=function(res_dat,plotname="my.model",Width=NULL,Height=NULL,Res=NULL,cex.leg=0.7){
# save graphic parameters
par.save = par
# Extract residual for by scenario and index 
d = res_dat
names(d)
d$residual = ifelse(is.na(d$Obs),NA,log(d$Obs)-log(d$Exp))
if(is.null(d$Fleet_name)){ # Deal with Version control
d$Fleet_name = d$Name
} 
agg = aggregate(residual~Yr+Fleet_name,d,mean)

#combine all years in one vector
idd = paste0(agg[,2])
yall = 1:length(idd)
iall = levels(factor(idd))
yrs = agg[,1]

if(is.null(Width)) Width = 6
if(is.null(Height)) Height = 3.5
if(is.null(Res)) Res = 200

Par = list(mfrow=c(1,1),mar = c(1.2, 1.2, 1., 0.1),oma=c(0, 0, 0, 6), mgp =c(2.5,1,0), tck = -0.02,cex=0.8)
png(file = paste0("runstest_",plotname,".png"), width = Width, height = Height, 
    res = Res, units = "in")
par(Par)
  ds = d
  jdd = paste0(ds$Yr,".",ds$Fleet_name)
  yj = which(idd %in% jdd)
  ylim = c(-max(abs(d$residual)[abs(d$residual)<3],na.rm=T)*1.1,max(abs(d$residual)[abs(d$residual)<3],na.rm=T)*1.1)
  plot(yall,yall,type="n",xlab="Year",xlim=c(1,max(yall)),ylim=ylim,axes=F,xaxs = "i",yaxs="i")  
  axis(1,at=yall,labels=yrs,cex.axis=0.6,mgp=c(0.1,0.,0))
  axis(2,cex.axis=0.6,mgp=c(0.9,0.4,0))
  title(main=paste(ds$scenario)[1], mgp=c(0.1,0,0),cex.main=0.8,line=0.2)
  abline(v=1)
  
  i = 1
  ni = length(unique(ds$Fleet_name))
  indices = levels(factor(ds$Fleet_name))
  # Save runs results
  runs.out = NULL
  for(i in 1:ni){
    di = ds[ds$Fleet_name==indices[i],]
    yi = yall[which(agg$Fleet_name==indices[i])]
    get_runs = runs.sig3(di$residual)
    lims = get_runs$sig3lim
    if(median(abs(di$residual))<3){
    polygon(c(yi,rev(yi)),c(rep(ylim[1],nrow(di)),rev(rep(ylim[2],nrow(di)))),col=ifelse(get_runs$p.runs<0.05,rgb(1,0,0,0.5),rgb(0,1,0,0.5)),border=0)   
    polygon(c(yi,rev(yi)),c(rep(lims[1],nrow(di)),rev(rep(lims[2],nrow(di)))),col=grey(0.5,0.5),border=0)   
    lines(yi,di$residual)
    points(yi,di$residual,pch=21,bg=ifelse(abs(di$residual)>abs(lims[1]),2,0),cex=0.7)  
    text(mean(yi),ylim[2]*0.9,which(indices[i]==iall),cex=0.8)
    }
    runs.out = rbind(runs.out,data.frame(index=indices[i],pvalue=get_runs$p.runs,sig3low=lims[1],sig3high=lims[2]))
  }
  legend(par('usr')[2]*0.97, quantile(par('usr')[3:4],0.6), bty='n', xpd=NA,
                    c(paste0(1:length(iall),": ",iall)),cex=cex.leg)
  legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,
                  c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=0.7)
  
  
  abline(h=0,lty=2)
  abline(h=max(abs(d$residual))*1.1)
  for(i in 1:length(iall)){
    abline(v=0.5+max(which(agg$Fleet_name==levels(factor(agg$Fleet_name))[i])))
  }
dev.off()
par=par.save
return(runs.out)
} # End of function


#---------------------------------------
# Make JABBA-Residual Plot
#---------------------------------------

jabba_resids_plot=function(res_dat,plotname="my.model",Width=NULL,Height=NULL,Res=NULL){
if(is.null(Width)) Width = 6
if(is.null(Height)) Height = 3.5
if(is.null(Res)) Res = 200
d = res_dat
names(d)
d$residual = log(d$Obs)-log(d$Exp)
if(is.null(d$Fleet_name)){ # Deal with Version control
  d$Fleet_name = d$Name
} 

resids = dcast(d,Yr~Fleet_name,value.var="residual")
# JABBA-residual plot
Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
png(file = paste0("JABBAresiduals_",plotname,".png"), width =Width, height = Height, 
    res = Res, units = "in")
par(Par)
Resids = t(resids[,-1])
Yr = resids[,1]
n.indices = nrow(Resids)
n.years = length(Yr)
ylim=ifelse(rep(max(ifelse(abs(Resids)>3,0,Resids),na.rm = T),2)>0.5,range(1.1*ifelse(abs(Resids)>3,0,Resids),na.rm = T),range(c(-0.7,0.5)))

Resids = ifelse(abs(Resids)>3,NA,Resids)
plot(Yr,Yr,type = "n",ylim=ylim,xlim=range(Yr),ylab="log residuals",xlab="Year")
boxplot(as.matrix(Resids),add=TRUE,at=c(Yr),xaxt="n",col=grey(0.8,0.5),notch=FALSE,outline = FALSE)
abline(h=0,lty=2)
positions=runif(nrow(Resids),-0.2,0.2)

for(i in 1:n.indices){
  for(t in 1:n.years){
    lines(rep((Yr+positions[i])[t],2),c(0,Resids[i,t]),col=jabba.colors[i])}
  points(Yr+positions[i],Resids[i,],col=1,pch=21,bg=jabba.colors[i])}
mean.res = apply(Resids,2,mean,na.rm =TRUE)
smooth.res = predict(loess(mean.res~Yr),data.frame(Yr))
lines(Yr,smooth.res,lwd=2)
# get degree of freedom
Nobs =length(as.numeric(Resids)[is.na(as.numeric(Resids))==FALSE])
RMSE = round(100*sqrt(sum(Resids^2,na.rm =TRUE)/Nobs),1)
legend('topright',c(paste0("RMSE = ",RMSE,"%")),bty="n")
legend('bottomleft',c(paste(rownames(Resids)),"Loess"),bty="n",col=1,pt.cex=1.1,cex=0.75,pch=c(rep(21,n.indices),-1),pt.bg=c(jabba.colors,1),lwd=c(rep(-1,n.indices),2))
dev.off()
return(list(Nobs=Nobs, RMSE=RMSE/100))
} #end of functions

#----------------------------------------
# Runs plot for RecDevs 
#----------------------------------------

plot_ss3runsrec=function(rec_dat,plotname="my.model",Width=NULL,Height=NULL,Res=NULL){
rec.est = rec_dat[rec_dat$era=="Main",]
# Make Plot
if(is.null(Width)) Width = 6
if(is.null(Height)) Height = 3.5
if(is.null(Res)) Res = 200
Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
png(file = paste0("RecDev_",plotname,".png"), width = Width, height = Height, 
    res = Res, units = "in")
par(Par)
years=rec.est$Yr
get_runs = runs.sig3(rec.est$dev)
xlim =range(rec_dat$Yr) 
ylim = c(min(-0.22,abs(rec.est$dev)*-1.1),max(0.22,abs(rec.est$dev)*1.1))#range(proc.dev)*1.1
lims = get_runs$sig3lim
cord.x <- c(years,rev(years))
cord.y <- c(rep(get_runs$sig3lim[1],length(years)),rev(rep(get_runs$sig3lim[2],length(years))))
# Process Error
plot(years,years,ylab="Recruiment Deviates",xlab="Year",ylim=ylim,xlim=xlim,type="n")

polygon(cord.x,cord.y,col=ifelse(get_runs$p.runs<0.05,rgb(1,0,0,0.5),rgb(0,1,0,0.5)),border=0)
rec_dat$dev[is.na(rec_dat$dev)] = 0
lines(rec_dat$Yr,rec_dat$dev,lwd=2,lty=2)
abline(h=0,lty=2)
lines(years,rec.est$dev,lwd=2,col=1)
outliers = rec.est[rec.est$dev<get_runs$sig3lim[1] |rec.est$dev>get_runs$sig3lim[2],]
points(years,rec.est$dev,bg="white",pch=21)
points(outliers$Yr,outliers$dev,bg="red",pch=21,cex=1.1)

pvalue = round(get_runs$p.runs,3)
#legend("topright",c(paste0("runs.p ", ifelse(pvalue<0.05,"< 0.05",paste0(" = ",pvalue)))),lwd=c(-1,1,2),col=c(0,1,4),bty="n",y.intersp = -0.2)
legend("topleft", bty='n', xpd=NA,
       c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=0.8)
legend("bottomleft",c("Fixed","Estimated"),lwd=c(2,2),col=c(1,1),lty=c(2,1),bty="n")
dev.off()
return(get_runs)
} # End of function

#--------------------------------------------
# Function to generate mean length residuas
## Code to prep data as used in SS3 mean length plot
## Adapted from https://rdrr.io/cran/r4ss/src/R/SSplotMnwt.R
## and https://rdrr.io/cran/r4ss/src/R/SSMethod.TA1.8.R
#--------------------------------------------

get_ss3compsTA1.8 <- function(ss3rep,type=c('len','age','size','con'),fleet=NULL,
                              part = 0:2,pick.gender = 0:3,seas = NULL,method = NULL,
                              plotit = TRUE,maxpanel = 1000){
  
  # Check the type is correct and the pick.gender is correct
  is.in <- function (x, y)!is.na(match(x, y))
  if(!is.in(type[1],c('age','len','size','con'))){
    stop('Composition type incorrectly speficied')
  }else{
    if(sum(!is.in(pick.gender,c(0:3)))>0){
      stop('Unrecognised value for pick.gender')
    }
  }
  
  # Select the type of datbase
  dbase <- ss3rep[[paste0(type[1],'dbase')]]
  if(is.null(fleet)) fleet = unique(dbase$Fleet)
  sel <-  is.in(dbase$Fleet,fleet) & is.in(dbase$Part,part)
  if(type[1]!='con')sel <- sel & is.in(dbase$Sexes,pick.gender)
  if(type[1]=='size' & !is.null(method)) sel <- sel & is.in(dbase$method,method)
  if(sum(sel)==0) return()
  dbase <- dbase[sel,]
  if(is.null(seas)){
    seas <- 'comb'
    if(length(unique(dbase$Seas))>1)
      cat('Warning: combining data from multiple seasons\n')
  }
  # create label for partitions
  partitions <- sort(unique(dbase$Part)) # values are 0, 1, or 2
  partition.labels <- c("whole","discarded","retained")[partitions+1]
  partition.labels <- paste("(",paste(partition.labels,collapse="&")," catch)",sep="")
  gender.flag <- type[1]!='con' & max(tapply(dbase$Sexes,
                                          dbase$Fleet,function(x)length(unique(x))))>1
  indx <- paste(dbase$Fleet,dbase$Yr,if(type[1]=='con')dbase$'Lbin_lo' else
    '',if(seas=='sep')dbase$Seas else '')
  if(gender.flag)indx <- paste(indx,dbase$Sexes)
  method.flag <- if(type[1]=='size') length(unique(dbase$method))>1 else FALSE
  if(method.flag)
    indx <- paste(indx,dbase$method)
  uindx <- unique(indx)
  if(length(uindx)==1){
    # presumably the method is meaningless of there's only 1 point,
    # but it's good to be able to have the function play through
    cat('Warning: only one point to plot\n')
    return()
  }
  
  pldat <- matrix(0,length(uindx),10,
                  dimnames=list(uindx,
                                c('Obsmn','Obslo','Obshi','semn','Expmn','Std.res',
                                  'ObsloAdj','ObshiAdj','Fleet','Yr')))
  if(type[1]=='con')pldat <- cbind(pldat,Lbin=0)
  if(gender.flag)pldat <- cbind(pldat,pick.gender=0)
  if(method.flag)pldat <- cbind(pldat,method=0)
  
  # Find the weighting factor for this combination of factors
  for(i in 1:length(uindx)){  # each row of pldat is an individual comp
    subdbase <- dbase[indx==uindx[i],]
    xvar <- subdbase$Bin
    pldat[i,'Obsmn'] <- sum(subdbase$Obs*xvar)/sum(subdbase$Obs)
    pldat[i,'Expmn'] <- sum(subdbase$Exp*xvar)/sum(subdbase$Exp)
    pldat[i,'semn'] <- sqrt((sum(subdbase$Exp*xvar^2)/sum(subdbase$Exp)-
                               pldat[i,'Expmn']^2)/mean(subdbase$N))
    pldat[i,'Obslo'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']
    pldat[i,'Obshi'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']
    pldat[i,'Std.res'] <- (pldat[i,'Obsmn']-pldat[i,'Expmn'])/pldat[i,'semn']
    pldat[i,'Fleet'] <- mean(subdbase$Fleet)
    pldat[i,'Yr'] <- mean(if(seas=='comb')subdbase$Yr else subdbase$Yr.S)
    if(type[1]=='con')pldat[i,'Lbin'] <- mean(subdbase$'Lbin_lo')
    if(gender.flag)
      pldat[i,'pick.gender'] <- mean(subdbase$'Pick_gender')
    if(method.flag)
      pldat[i,'method'] <- mean(subdbase$method)
  }
  Nmult <- 1/var(pldat[,'Std.res'],na.rm=TRUE)
  
  # Find the adjusted confidence intervals
  for(i in 1:length(uindx)){
    pldat[i,'ObsloAdj'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']/sqrt(Nmult)
    pldat[i,'ObshiAdj'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']/sqrt(Nmult)
  }
  
  Nfleet <- length(unique(pldat[,'Fleet']))
  # make plot if requested
  if(plotit){
    plindx <- if(type[1]=='con'){
      paste(pldat[,'Fleet'],pldat[,'Yr'])
    }else{
      pldat[,'Fleet']
    }
    if(gender.flag)plindx <- paste(plindx,pldat[,'pick.gender'])
    if(method.flag)plindx <- paste(plindx,pldat[,'method'])
    uplindx <- unique(plindx)
    
    # Select number of panels
    Npanel <- length(uplindx)
    ## Ian T. 9/25/14: changing from having at least 4 panels to no minimum
    #NpanelSet <- max(4,min(length(uplindx),maxpanel))
    NpanelSet <- min(length(uplindx),maxpanel)
    Nr <- ceiling(sqrt(NpanelSet))
    Nc <- ceiling(NpanelSet/Nr)
    # save current graphical parameters
    par_current <- par()
    # set new parameters
    par(mfrow=c(Nr,Nc),mar=c(2,2,1,1)+0.1,mgp=c(0,0.5,0),oma=c(1.2,1.2,0,0),
        las=1)
    par(cex=1)
    for(i in 1:Npanel){
      subpldat <- pldat[plindx==uplindx[i],,drop=FALSE]
      x <- subpldat[,ifelse(type[1]=='con','Lbin','Yr')]
      plot(x,subpldat[,'Obsmn'],pch='-',
           xlim=if(length(x)>1)range(x) else c(x-0.5,x+0.5),
           ylim=range(subpldat[,c('Obslo','Obshi','ObsloAdj','ObshiAdj','Expmn')],
                      na.rm=TRUE),
           xlab='',ylab='')
      segments(x,subpldat[,'Obslo'],x,subpldat[,'Obshi'],lwd=3)
      arrows(x,subpldat[,'ObsloAdj'],x,subpldat[,'ObshiAdj'],lwd=1,
             length=0.04, angle=90, code=3)
      points(x,subpldat[,'Obsmn'],pch=21,bg='grey80')
      ord <- order(x)
      if(length(x)>1){
        lines(x[ord],subpldat[ord,'Expmn'],col=4)
      }else{
        lines(c(x-0.5,x+0.5),rep(subpldat[,'Expmn'],2),col=4)
      }
      # Lines
      fl <- ss3rep$FleetNames[subpldat[1,'Fleet']]
      yr <- paste(subpldat[1,'Yr'])
      lab <- if(type[1]=='con')ifelse(Nfleet>1,paste(yr,fl),yr) else fl
      if(gender.flag)lab <-
        paste(lab,ifelse(subpldat[1,'pick.gender']==0,'comb','sex'))
      if(method.flag)lab <- paste(lab,'meth',subpldat[1,'method'])
      lab <- paste(lab,partition.labels)
      mtext(lab,side=3,at=mean(x))
    }
    mtext(paste('Mean',ifelse(is.in(type[1],c('len','size')),'length','age')),
          side=2,las=0,outer=TRUE)
    mtext(ifelse(type[1]=='con','Length','Year'),side=1,outer=TRUE)
    # restore previous graphics parameters
    par(mfrow=par_current$mfrow, mar=par_current$mar, mgp=par_current$mgp,
        oma=par_current$oma, las=par_current$las)
  }
  tmp <- matrix(sample(pldat[,'Std.res'],1000*nrow(pldat),replace=TRUE),nrow(pldat))
  confint <- as.vector(quantile(apply(tmp,2,function(x)1/var(x,na.rm=TRUE)),
                                c(0.025,0.975),na.rm=TRUE))
  Output <- c(w=Nmult,lo=confint[1],hi=confint[2])
  Outs <- paste("Francis Weights - ", type[1], ": ", ss3rep$FleetNames[fleet],": ",
                round(Nmult,4), " (",round(confint[1],4),"-",round(confint[2],4),")",
                sep="")
  print(Outs)
  pldat=data.frame(pldat)
  yrs=pldat$Yr
  
  comps_out  = list(ss_out = pldat ,runs_dat = data.frame(Fleet=pldat$Fleet,Fleet_name=ss3rep$FleetNames[pldat$Fleet],Yr=yrs,
                                                           Obs=pldat$Obsmn,Exp=pldat$Expmn))
  
  
  # return(Output)
  return(comps_out)
}


#------------------------------------------------------------------
# End of Functions
#------------------------------------------------------------------


