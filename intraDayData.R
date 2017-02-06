intraDayData <- function(interval = 60, days = 1, symbol = "SPY", 
                         extended= FALSE){
      
      # Function to retrieve intraday stock data from google finance
      # Requires user to provide the time interval in seconds, number of
      # days and symbol. days are limited to 10 and interval must be
      # divisible by 60
      # if extended is TRUE; will also return extended hours data
      # Returns the date/time close high low open and volume for each period
      # IMPORTANT - FUNCTION RETURNS A LIST (DF, XTS)
       
       
      
      # Converting symbol to upper case
      require(xts)
      symbol = toupper(symbol)
      
      # Google only has the past 10 days available
      if (days>10){
            days = 10
            message("days cannot be greater than 10")
            message("days defaulted to 10")
      }
      
      # Interval must be divisible by 60
      if (interval%%60 != 0){
            interval = 60
            # Will return data with interval not divisible by 60
            # but does not appear to be correct or easily parsed
            message("Interval must be divisible by 60")
            message("Interval defaulted to 60")
      }
      
      partial.url = 'http://www.google.com/finance/getprices?i='
      url = paste(partial.url,interval,'&p=', days,
                  'd&f=d,o,h,l,c,v&df=cpct&q=',symbol ,sep="")
      
      if (extended == TRUE){
            # running extended during trading day throws error
            # when days = 1
            oldDays = days
            if (days < 2){
                  days = 2
            }
            url2 = paste(partial.url,interval,'&sessions=ext_hours&p=', days,
                        'd&f=d,o,h,l,c,v&df=cpct&q=',symbol ,sep="")
            days = oldDays
      }
      
      if (extended == TRUE){
            data1 = getGoogleData(url= url, interval, extended= FALSE)
            data2 = getGoogleData(url= url2, interval,extended)
            if (days == 1){
                  data2 <- data2[-1,]
            }
            data = rbind(data1,data2)
            data = data[order(data$PERIOD),]
            # Find and remove duplicates s/b 2 per day (8:30 & 15:00)
            data<- data[!duplicated(data[,"PERIOD"]),]
      }else{
            data <- getGoogleData(url, interval, extended)
      }
      
      data_xts <- xts(data[,-1], order.by=data[,1])
      data_list <- list(data = data, data_xts = data_xts)
      return (data_list)
      
      
}

getGoogleData <-function(url, interval, extended){
      
      require(stringr)
      #Read data from Google
      # needs error handling
      allLines <- readLines(url)
      
      #Strip out meta data
      #Could Return a list from function which includes meta Data
      if (extended == FALSE){
            metaData = allLines[1:7] 
            # read remaining data into table
            data = read.table(textConnection(allLines[8:length(allLines)]))
      }else{
            metaData = allLines[1:8] 
            # read remaining data into table
            data = read.table(textConnection(allLines[9:length(allLines)]))
      }
      
      # Split data into columns
      out <- strsplit(as.character(data$V1),',') 
      data <- do.call(rbind, out)
      
      # Rename columns
      names <- unlist(strsplit(as.character(str_sub(metaData[5],
                                                    14, str_length(metaData[5]))),','))
      names<- c("PERIOD", names)
      colnames(data) <- names
      
      #Strip leading a from first period entry
      data[,1]= ifelse(substring(data[,1], 1, 1)=="a"
                       ,str_sub(data[,1], 2, str_length(data[,1])),
                       data[,1])
      
      # Convert to dataframe
      data = as.data.frame(data)
      
      # Convert columns from factor to numeric
      indx <- sapply(data, is.factor)
      data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))
      
      # Following line necessary because GOOG inserts meta data on the first day
      # after the time change indicating a change in offset from GMT
      # The following error may appear in console:
      # In FUN(X[[i]], ...) : NAs introduced by coercion
      # Ignore it
      data <- data[complete.cases(data),]
      
      # Convert PERIOD to UNIX time
      data$PERIOD = convertUnixDate(data$PERIOD,interval)
      data$PERIOD <- as.POSIXct(data[,1], origin="1970-01-01")
      
      return (data)
}

convertUnixDate <- function(dat, interval){
      # Convert Date time column to UNIX Time
      # Number of seconds since Jan 1 1970 12:00:00 AM
      val = dat[1]
      newDat <- vector(mode="numeric", length=length(dat))
      # first element is known to be UNIX time
      newDat[1] = val
      for (i in 2:length(dat)){
            # 100000 in if statement is large but arbitrary
            if (dat[i]>100000){
                  val = dat[i]
                  newDat[i] = val
            }else{
                  newDat[i] = val + dat[i]*interval   
            } 
      }
      
      return(newDat)
}

# The following function adds indicators of interest to the dataset
# 
addIndicators <- function(df){
      
      # Adds additional indicators to the data
      # Specifically: 10, 20 and 50 period MA
      # 20 period SD
      # Bollinger bands (using 1.96)
      # Price change and log diff price
      # df is expected to be an xts object
      
      #Add MA's
      df$ma10  <- rollapply(df$CLOSE, width = 10,
                            FUN = mean, na.rm = TRUE)
      df$ma20  <- rollapply(df$CLOSE, width = 20,
                            FUN = mean, na.rm = TRUE)
      df$ma50  <- rollapply(df$CLOSE, width = 50,
                            FUN = mean, na.rm = TRUE)
      # Add 20 SD
      df$sd20  <- rollapply(df$CLOSE, width = 20,
                            FUN = sd, na.rm = TRUE)
      # Add Bolinger Bands
      df$bb1   <- df$ma20 + df$sd20 * -1.96
      df$bb2   <- df$ma20 + df$sd20 * 1.96
      # Recalculate change because it is blown up upon xts convert
      df$Change <- diff(df$CLOSE, lag = 1, differences = 1)
      # diff log
      df$log_Change <- diff(log(df$CLOSE), lag = 1, differences = 1)
      
     
      
      
      return(df)
}