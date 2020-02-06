SSsplitdat <-
  function(inpath     = 'working_directory',
           outpath    = 'working_directory',
           inname     = 'data.ss_new',
           outpattern = 'BootData',
           number     = FALSE,
           verbose    = TRUE,
           fillblank  = TRUE,
           MLE        = TRUE,
           inputs     = FALSE,
           notes      = ""
  )
  {
    # this is a function to split bootstrap aggregated in the data.ss_new file
    # which is output from Stock Synthesis into individual data files.
    if(MLE & inputs) stop("can't have both 'MLE' and 'inputs' = TRUE")
    
    if(inpath=="working_directory") inpath=getwd()
    if(outpath=="working_directory") outpath=getwd()
    
    infile    <- paste(inpath,inname,sep='/')
    filelines <- readLines(infile)
    if(fillblank)  filelines[filelines==""] <- "#"
    
    string    <- '#_bootstrap file'
    starts    <- grep(string, filelines)
    ends      <- c(starts[-1]-1,length(filelines)-1)
    
    MLEstring <- '#_expected values with no error added'
    MLEstart  <- grep(MLEstring, filelines)
    MLEend    <- starts[1]-1
    
    if(MLE & length(MLEstart)==0) stop("no MLE values in ",inname,"\n  change 'N bootstrap datafiles' in starter.ss to 2 or greater")
    inputstring <- '#_observed data'
    inputstart  <- grep(inputstring, filelines)
    if(length(MLEstart)==0) inputend <- length(filelines) else inputend <- MLEstart-1
    if(length(inputstart)==0) stop("no values in ",inname,"\n  change 'N bootstrap datafiles' in starter.ss to 1 or greater")
    
    
    if(!MLE & !inputs){
      if(length(starts)==0) stop("no bootstrap values in ",inname,"\n  change 'N bootstrap datafiles' in starter.ss to 3 or greater")
      for(i in 1:length(starts)) {
        outfile <- paste(outpath,'/',outpattern,ifelse(number,i,''),'.dat',sep='')
        outline <- paste('# Data file created from',infile,'to',outfile)
        if(verbose) cat(outline,"\n")
        writeLines(c(outline,filelines[starts[i]:ends[i]]),outfile)
      }
    }else{
      if(MLE){
        outfile <- paste(outpath,'/',outpattern,'.dat',sep='')
        if(notes!="") notes <- paste("#C",notes) else notes <- NULL
        notes <- c(notes,paste('#C MLE data file created from',infile,'to',outfile))
        if(verbose) cat('MLE data file created from',infile,'to',outfile,"\n")
        writeLines(c(notes,filelines[MLEstart:MLEend]),outfile)
      }
      if(inputs){
        outfile <- paste(outpath,'/',outpattern,'.dat',sep='')
        if(notes!="") notes <- paste("#C",notes) else notes <- NULL
        notes <- c(notes,paste('#C data file created from',infile,'to',outfile))
        if(verbose) cat('file with copies of input data created from',infile,'to',outfile,"\n")
        writeLines(c(notes,filelines[inputstart:inputend]),outfile)
      }
    }
  }



SS_bootstrap <- function(){
    # this is not yet a generalized function, just some example code for how to do
    # a parametric bootstrap such as was done for the Pacific hake model in 2006
    # See http://www.pcouncil.org/wp-content/uploads/2006_hake_assessment_FINAL_ENTIRE.pdf
    # A description is on page 41 and Figures 55-56 (pg 139-140) show some results.
    
    # Written by Ian Taylor on 10/11/2012 after disucussion with Nancie Cummings
    
    # first set "Number of datafiles to produce" in starter.ss = 100 or some large number
    # re-run model to get data.ss_new file concatenating all bootstrap data files
    
    # Directory where bootstrap will be run.
    # You probably want to use a copy of the directory where you ran it,
    # so as not to overwrite the true results.
    inpath <- 'G:\\Uku\\Bootstrap'
    
    #setwd(inpath) # change working directory (commented out to avoid violating CRAN policy)
    
    # split apart data.ss_new into multiple data files with names like "BootData1.ss"
    SSsplitdat(inpath=inpath, outpath=inpath, number=TRUE, MLE=FALSE)
    
    N <- 100 # number of bootstrap models to run (less than or equal to setting in starter)
    
    starter <- SS_readstarter(file="starter.ss") # read starter file file="starter.ss"
    file.copy("starter.ss","starter_backup.ss") # make backup
    
    # loop over bootstrap files
    for(iboot in 1:N){
        # note what's happening
        cat("\n##### Running bootstrap model number",iboot," #########\n") 
        
        # change data file name in starter file
        starter$datfile <- paste("BootData",iboot,".dat",sep="")
        # replace starter file with modified version
        SS_writestarter(starter, overwrite=TRUE)
        
        # delete any old output files
        file.remove("Report.sso")
        file.remove("CompReport.sso")
        file.remove("covar.sso")
        
        # run model
        shell("ss")
        # for some computers or versions of R, "shell" works better than "system"
       # system("ss")
        
        # copy output files (might be good to use "file.exists" command first to check if they exist
        file.copy("Report.sso",paste("Report_",iboot,".sso",sep=""))
        file.copy("CompReport.sso",paste("CompReport_",iboot,".sso",sep=""))
        file.copy("covar.sso",paste("covar_",iboot,".sso",sep=""))
        # other .sso files could be copied as well
        
    }
    
    # read and summarize all models
    # (setting getcomp=FALSE will produce warning
    #  about missing comp file, but use less memory)
    bootmodels <- SSgetoutput(keyvec=paste("_",1:N,sep=""), dirvec=inpath, getcomp=FALSE)
    bootsummary <- SSsummarize(bootmodels)
    
    # a bunch of plots that won't work well if there are lots of models
   # SSplotComparisons(bootsummary,png=TRUE)
    # histogram of a single quantity
    hist(as.numeric(bootsummary$quants[bootsummary$quants$Label=="SSB_Virgin",1:N]), main="Bootstrapped Virgin SSB estimates",xlab="SSB")
    hist(as.numeric(bootsummary$pars[grep("R0",bootsummary$pars$Label),1:N]),main="Bootstrapped R0 estimates",xlab="ln(R0)")
   return(bootmodels) 
    
}
install.packages("devtools")
devtools::install_github("r4ss/r4ss")
library(r4ss)
setwd("G:\\Uku\\Bootstrap")
boot_results<-SS_bootstrap()

#bootmodels <- SSgetoutput(keyvec=paste("_",1:100,sep=""), dirvec=inpath, getcomp=FALSE)
NatAge<-as.data.frame(matrix(NA,ncol=45,nrow=100))

for (i in 1:100){
    tempmodel<-boot_results[[i]]
    NatAge[i,]<-tempmodel$natage[143,]
}

write.table(NatAge[,14:45],"Uku.bsn", row.names=FALSE,col.names = FALSE)
