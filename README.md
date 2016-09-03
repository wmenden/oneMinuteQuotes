# oneMinuteQuotes
#Pulls one minute stock/etf quotes from google finance

      # Function to retrieve intraday stock data from google finance
      # Requires user to provide the time interval in seconds, number of
      # days and symbol. days are limited to 10 and interval must be
      # divisible by 60
      # if extended is TRUE; will also return extended hours data
      # Returns the date/time close high low open and volume for each period
