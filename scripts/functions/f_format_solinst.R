## FORMAT RAW CSV FILES FROM LOGGERS

# Wed Apr 12 22:44:16 2017 ------------------------------

source("./scripts/functions/f_doy.R")

format_solinst<-function(skip, site, compensated=FALSE){

  library(lubridate, warn.conflicts = F)
  library(readr, warn.conflicts = F)
  library(dplyr, warn.conflicts = F)
  
  skip = skip
  rivsite = site # for adding a site name
  comps<-compensated
  cat("formatting as solinst logger...\n\n")
  
  # choose a file, use the raw csv logger output
  inputfile <- ifelse(Sys.info()[[1]]=="Darwin",file.choose(), choose.files())
  file<-read.csv(inputfile,stringsAsFactors=FALSE,skip=skip) # 16 for old solinsts
  
  file$datetime<-paste(file$Date," ",file$Time,sep="")
  print(head(file))
  
  cat("\n","Enter date format: 1=MDY or 2=YMD?","\n\n") # prompt 
  z<-scan(what="integer",n=1)
  cat("\n","Enter time format: 3=HMS or 4=HM?","\n\n")
  y<-scan(what="integer",n=1)
  if(z==1 & y==3){
    ## use lubridate with MDY
    file$datetime<-mdy_hms(file$datetime) # convert to POSIXct  
  } else{
    if(z==1 & y==4){
      file$datetime<-mdy_hm(file$datetime) # convert to POSIXct  
    } else {
      if(z==2 & y==4){
        file$datetime<-ymd_hm(file$datetime) # convert to POSIXct
      } else{
        file$datetime<-ymd_hms(file$datetime) # convert to POSIXct
      }
    }
  }
  cat("date converted \n")
  
  # select data
  df<-as.data.frame(file)
  colnames(df) <- tolower(colnames(df))
  cols_to_keep <- c("datetime", "level", "temperature")
  df<-as.data.frame(select(df, one_of(cols_to_keep)))
  colnames(df)<-c("datetime","level","temp_C")
  
  # add cols for site and compensation
  df <- df %>% 
    mutate(site=rivsite,
           compensated = ifelse(comps==TRUE, "Y", "N"))
  
  df <- add_WYD(df, "datetime")
  
  print(summary(df))
  
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
