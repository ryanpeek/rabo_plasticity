## FORMAT RAW CSV FILES FROM LOGGERS
## 2013-May-18

## Set working directory before running function
## file will output to current dir.

format_solinst<-function(skip){

  library(lubridate, warn.conflicts = F)
  library(readr, warn.conflicts = F)
  library(dplyr, warn.conflicts = F)
  
  skip=skip
  
  cat("formatting as solinst logger...\n\n")
  
  # choose a file, use the raw csv logger output
  inputfile <- ifelse(Sys.info()[[1]]=="Darwin",file.choose(), choose.files())
  file<-read.csv(inputfile,stringsAsFactors=FALSE,skip=skip) # 16 for old solinsts
  
  file$datetime<-paste(file$Date," ",file$Time,sep="")
  print(head(file))
  
  cat("\n","Enter date format: MDY or YMD?","\n\n") # prompt 
  z<-scan(what="character",n=1)
  cat("\n","Enter time format: HMS or HM?","\n\n")
  y<-scan(what="character",n=1)
  if(z=="MDY"& y=="HMS"){
    ## use lubridate with MDY
    file$datetime<-mdy_hms(file$datetime) # convert to POSIXct  
  } else{
    if(z=="MDY" & y=="HM"){
      file$datetime<-mdy_hm(file$datetime) # convert to POSIXct  
    } else {
      if(z=="YMD" & y=="HM"){
        file$datetime<-ymd_hm(file$datetime) # convert to POSIXct
      } else{
        file$datetime<-ymd_hms(file$datetime) # convert to POSIXct
      }
    }
  }
  cat("date converted \n")
  
  # select data
  cols_to_keep <- c("datetime", "Level", "Temperature")
  df<-as.data.frame(select(file, one_of(cols_to_keep)))
  colnames(df)<-c("datetime","level","temp_C")
  summary(df)
  
  require(tools) 
  filename<-basename(file_path_sans_ext(inputfile))
  getwd()
  
  cat("writing to rds ...\n")
  cat(getwd())
  write_rds(df, path=paste("data/",filename,"_formatted.rds",sep=""))
  cat(paste("\n file saved as: ","data/",filename,"_formatted.rds", sep=""))
}

## TO RUN EXAMPLE:
# format_solinst(13)
