## WUNDERGROUND DAILY DATA FROM (from Dylan, http://casoilresource.lawr.ucdavis.edu/drupal/node/991)
## see 'download_wunderground.r' script

#   TUO: KCAGROVE6, MTS026, MD2740
#   COLFAX/NFA: KCAGOLDR3, KCAWEIMA5, 
#   MFA: KCACOOL6
#   MFY: KCAALLEG2
#   RUB/RALSTON: KCAFORES14
#   FORESTHILL: KCAFORES6, KCAFORES12, KCAFORES21, KCAFORES22
#   MALAKOFF: MNVYC1
#   NFY: Saddleback, CA Headwaters of Goodyear Ck: MSLEC1

wunder_daily <- function(station, date)
{
  
  # load packages
  #if(!require(plyr)) { install.packages("plyr"); require(plyr)}
  if(!require(lubridate)) { install.packages("lubridate"); require(lubridate, warn.conflicts = F)}
  
  # get base web address
  base_url <- 'https://www.wunderground.com/weatherstation/WXDailyHistory.asp?'

  # parse date
  m <- as.integer(month(date))
  d <- as.integer(day(date))
  y <- year(date)
  
  # compose final url
  final_url <- paste(base_url,
                     'ID=', station,
                     '&month=', m,
                     '&day=', d, 
                     '&year=', y,
                     '&format=1', sep='')
  
  # reading in as raw lines from the web server
  # contains <br> tags on every other line
  u <- url(final_url)
  the_data <- readLines(u)
  close(u)
  
  # only keep records with more than 5 rows of data
  if(length(the_data) > 5 )
  {
    # remove the first and last lines
    the_data <- the_data[-c(1, length(the_data))]
    
    # remove odd numbers starting from 3 --> end
    the_data <- the_data[-seq(3, length(the_data), by=2)]
    
    # extract header and cleanup
    the_header <- the_data[1]
    the_header <- make.names(strsplit(the_header, ',')[[1]])
    
    # convert to CSV, without header
    tC <- textConnection(paste(the_data, collapse='\n'))
    the_data <- read.csv(tC, as.is=TRUE, row.names=NULL, header=FALSE, skip=1)
    close(tC)
    
    # remove the last column, created by trailing comma
    the_data <- the_data[, -ncol(the_data)]
    
    # assign column names
    names(the_data) <- the_header
    
    # convert Time column into properly encoded date time
    the_data$Time <- as.POSIXct(strptime(the_data$Time, format='%Y-%m-%d %H:%M:%S'))
    
    # remove UTC and software type columns
    the_data$DateUTC.br. <- NULL
    the_data$SoftwareType <- NULL
        
    # sort and fix rownames
    the_data <- the_data[order(the_data$Time), ]
    row.names(the_data) <- 1:nrow(the_data)

    # done
    return(the_data)
  }
}