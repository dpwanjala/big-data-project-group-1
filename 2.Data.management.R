# === Data Management ==========================================================

# note 1.Main.R is required to run first to access folder paths
# this script manages and formats raw data files 
# such that later scripts can perform analyses 

# === 1) read raw data =========================================================

# read csv files for stock exchange indices 
# read files from data.raw folder
# csv files must be in data.raw folder for code to work!

csv.SP500 <- read.csv(paste(p.data.raw, "SP500.5Years.csv", sep = ""))
csv.NASDAQ <- read.csv(paste(p.data.raw, "NASDAQ.5Years.csv", sep = ""))
csv.DJI <- read.csv(paste(p.data.raw, "DowJones.5Years.csv",sep = ""))
csv.TSX <- read.csv(paste(p.data.raw,
                          "tsx_composite_index_canada.csv", sep = ""))
csv.DAX <- read.csv(paste(p.data.raw, "DAX.5Years.csv", sep = ""))

# dataframe check for each csv file
head(csv.SP500)
str(csv.SP500)
head (csv.NASDAQ)
str(csv.NASDAQ)
head(csv.DJI)
str(csv.DJI)
head(csv.TSX)
str(csv.TSX)
head(csv.DAX)
str(csv.DAX)


# read csv files for covid19 data 
# reading from data.raw folder
csv.CVD.CAN <- read.csv(paste(p.data.raw,
                              "covid19canada-download.csv", sep = ""))
csv.CVD.USA <- read.csv(paste(p.data.raw,
                              "CoronaUSDec12.csv", sep = ""))
csv.CVD.DE <- read.csv(paste(p.data.raw,
                             "cases-rki-by-state_germany.csv", sep = ""))

# dataframe check for each csv file
head (csv.CVD.CAN)
str(csv.CVD.CAN)
head (csv.CVD.USA)
str(csv.CVD.USA)
head(csv.CVD.DE)
str(csv.CVD.DE)

# === 2) format raw data =======================================================


# create function to alter format of dates
## I need to fix the function such that it runs format 1 and then if it gets
## a NA it then runs format 2, we need this for the DJI index
alter.date.fmt <- function(date){
  dates.cr.fmt.1 <- as.Date.character(date, format = "%m/%d/%Y")
  return(dates.cr.fmt.1) 
}
# function for second date format
alter.date.fmt.2 <- function(date){
  dates.cr.fmt.2 <- as.Date.character(date, format = "%b %d,%Y")
  return(dates.cr.fmt.2)
}
# remove comma from DJI and DAX values
csv.DJI$Price <- as.numeric(gsub(",","",csv.DJI$Price))
csv.DJI$Low <- as.numeric(gsub(",","",csv.DJI$Low))
csv.DJI$High <- as.numeric(gsub(",","",csv.DJI$High))
csv.DJI$Open <- as.numeric(gsub(",","",csv.DJI$Open))
csv.DAX$Price <- as.numeric(gsub(",","",csv.DAX$Price))
csv.DAX$Low <- as.numeric(gsub(",","",csv.DAX$Low))
csv.DAX$High <- as.numeric(gsub(",","",csv.DAX$High))
csv.DAX$Open <- as.numeric(gsub(",","",csv.DAX$Open))

# sorting covid data from Canada by whole country
CAN.CVD.data <- csv.CVD.CAN[csv.CVD.CAN$prname == "Canada",]

# create data frame from Canada covid daily cases and dates 
CAN.CVD.df <- cbind.data.frame(CAN.CVD.data$date, CAN.CVD.data$numtoday)


# replace / in submitted dates with - for correct reading of calendar values
USA.covid.cr.dates <- alter.date.fmt(csv.CVD.USA$submission_date)
csv.CVD.USA$submission_date <- USA.covid.cr.dates

# create dataframe with daily covid cases for all states in USA
csv.CVD.USA$new_case <- as.numeric(gsub(",","",csv.CVD.USA$new_case))
USA.CVD.df <- aggregate(csv.CVD.USA['new_case'], 
                        by=csv.CVD.USA['submission_date'], sum)


# remove time from dates in Germany covid data
DE.covid.cr.dates <- gsub(csv.CVD.DE$time_iso8601,pattern="T17:00:00+0000",
                          replacement="",fixed=T)
csv.CVD.DE$time_iso8601 <- DE.covid.cr.dates


# create vector for daily cases 
# by finding difference between total cases day (n) - (n-1)
DE.daily.cases <- ave(csv.CVD.DE$sum_cases, FUN = function(x) c(0,diff(x)))
# replace first value in vector with original total case value from csv file
DE.daily.cases <- replace(DE.daily.cases, DE.daily.cases==0,
                          csv.CVD.DE$sum_cases[1])
# create dataframe with daily covid cases from all states in Germany
DE.CVD.df <- cbind.data.frame(csv.CVD.DE$time_iso8601,DE.daily.cases)


# replace / in submitted dates with - for correct reading of calendar values
# SP500
SP500.cr.fmt.date <- alter.date.fmt(csv.SP500$Date)
csv.SP500$Date <- SP500.cr.fmt.date

# DJI
DJI.cr.fmt.date <- alter.date.fmt.2(csv.DJI$Date)
csv.DJI$Date <- DJI.cr.fmt.date

# DAX
DAX.cr.fmt.date <- alter.date.fmt.2(csv.DAX$Date)
csv.DAX$Date <- DAX.cr.fmt.date

# === 3) write new csv files with clean data ===================================


# write csv files for each countries with cleaned and formatted data
# Canada
write.csv(CAN.CVD.df, paste(p.data.clean, "csv.CVD.CAN.clean.csv", sep = ""), 
          row.names = FALSE)
# USA
write.csv(USA.CVD.df, paste(p.data.clean, "csv.CVD.USA.clean.csv", sep = ""), 
          row.names = FALSE)
# Germany 
write.csv(DE.CVD.df, paste(p.data.clean, "csv.CVD.DE.clean.csv", sep = ""), 
          row.names = FALSE)

# write csv files for each index with cleaned and formatted data
#SP500
write.csv(csv.SP500, paste(p.data.clean, "csv.SP500.clean.csv", sep = ""), 
          row.names = FALSE)
# NASDAQ 
write.csv(csv.NASDAQ, paste(p.data.clean, "csv.NASDAQ.clean.csv", sep = ""), 
          row.names = FALSE)

#DJI
write.csv(csv.DJI, paste(p.data.clean, "csv.DJI.clean.csv", sep = ""), 
          row.names = FALSE)

# TSX
write.csv(csv.TSX, paste(p.data.clean, "csv.TSX.clean.csv", sep = ""), 
          row.names = FALSE)

# DAX
write.csv(csv.DAX, paste(p.data.clean, "csv.DAX.clean.csv", sep = ""), 
          row.names = FALSE)


#___ end _______________________________________________________________________

