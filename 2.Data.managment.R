# read csv files for stock exchange indices 
## we have to change this so it reads from the RAWdata Folder, 
## but I couldnt figure it out


list.files()
csv.SP500 <- read.csv("SP500.5Years.csv")
csv.NASDAQ <- read.csv("NASDAQ.5Years.csv")
csv.DJI <- read.csv("DowJones.5Years.csv")
csv.TSX <- read.csv("tsx_composite_index_canada.csv")
csv.DAX <- read.csv("DAX.5Years.csv")

# data frame check for each csv file
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
list.files()
csv.CVD.CAN <- read.csv("covid19canada-download.csv")
csv.CVD.USA <- read.csv("CoronaUSDec12.csv")
csv.CVD.DE <- read.csv("cases-rki-by-state_germany.csv")

# data frame check for each csv file
head (csv.CVD.CAN)
str(csv.CVD.CAN)
head (csv.CVD.USA)
str(csv.CVD.USA)
head(csv.CVD.DE)
str(csv.CVD.DE)

# create function to find start and stop dates for data 
find.dates <- function(date) {
  max.date <- max(date)
  min.date <- min(date)
  min.max <- c(min.date, max.date)
  return(min.max)
}
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
csv.DAX$Price <- as.numeric(gsub(",","",csv.DAX$Price))



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


# Finding start and stop dates for covid data
# Canada
CAN.CVD.dates.min.max <- find.dates(CAN.CVD.df$`CAN.CVD.data$date`)

# USA
USA.CVD.dates.min.max <- find.dates(USA.CVD.df$submission_date)

# Germany
DE.CVD.dates.min.max <- find.dates(DE.CVD.df$`csv.CVD.DE$time_iso8601`)



# replace / in submitted dates with - for correct reading of calendar values
# SP500
SP500.cr.fmt.date <- alter.date.fmt(csv.SP500$Date)
csv.SP500$Date <- SP500.cr.fmt.date

# NASDAQ 

# DJI
DJI.cr.fmt.date <- alter.date.fmt.2(csv.DJI$Date)
csv.DJI$Date <- DJI.cr.fmt.date

# TSX

# DAX
DAX.cr.fmt.date <- alter.date.fmt.2(csv.DAX$Date)
csv.DAX$Date <- DAX.cr.fmt.date

# create vectors for daily closing values from each index by USA covid dates
# SP500
SP500.dates <- csv.SP500$Date[csv.SP500$Date >= USA.CVD.dates.min.max[1] 
                              & csv.SP500$Date <= USA.CVD.dates.min.max[2]]
SP500.close <- csv.SP500$Close.Last[csv.SP500$Date >= USA.CVD.dates.min.max[1] 
                                    & csv.SP500$Date <= USA.CVD.dates.min.max[2]]

# NASDAQ
NASDAQ.dates <- csv.NASDAQ$Date[csv.NASDAQ$Date >= USA.CVD.dates.min.max[1] 
                                & csv.NASDAQ$Date <= USA.CVD.dates.min.max[2]]
NASDAQ.close <- csv.NASDAQ$Close[csv.NASDAQ$Date >= USA.CVD.dates.min.max[1] 
                                 & csv.NASDAQ$Date <= USA.CVD.dates.min.max[2]]

# DJI
DJI.dates <- csv.DJI$Date[csv.DJI$Date >= USA.CVD.dates.min.max[1] 
                          & csv.DJI$Date <= USA.CVD.dates.min.max[2]]
DJI.close <- csv.DJI$Price[csv.DJI$Date >= USA.CVD.dates.min.max[1] 
                           & csv.DJI$Date <= USA.CVD.dates.min.max[2]]

# create vectors for daily closing values from each index by Canada covid dates
# TSX
TSX.dates <- csv.TSX$Date[csv.TSX$Date >= CAN.CVD.dates.min.max[1] 
                          & csv.TSX$Date <= CAN.CVD.dates.min.max[2]]
TSX.close <- csv.TSX$Close[csv.TSX$Date >= CAN.CVD.dates.min.max[1] 
                           & csv.TSX$Date <= CAN.CVD.dates.min.max[2]]

# DAX
DAX.dates <- csv.DAX$Date[csv.DAX$Date >= DE.CVD.dates.min.max[1] 
                          & csv.DAX$Date <= DE.CVD.dates.min.max[2]]
DAX.close <- csv.DAX$Price[csv.DAX$Date >= DE.CVD.dates.min.max[1] 
                           & csv.DAX$Date <= DE.CVD.dates.min.max[2]]

# create data frame for daily closing values from each index 
# by associated covid data
# SP500
SP500.df <- cbind.data.frame(SP500.dates, SP500.close)

# NASDAQ
NASDAQ.df <- cbind.data.frame(NASDAQ.dates, NASDAQ.close)

# DJI
DJI.df <- cbind.data.frame(DJI.dates, DJI.close)

# TSX
TSX.df <- cbind.data.frame(TSX.dates, TSX.close)

# DAX
DAX.df <- cbind.data.frame(DAX.dates, DAX.close)

# rename column names for all data frame dates to "Date"
# countries 
names(CAN.CVD.df)[names(CAN.CVD.df) == "CAN.CVD.data$date"] <- "Date"
names(USA.CVD.df)[names(USA.CVD.df) == "submission_date"] <- "Date"
names(DE.CVD.df)[names(DE.CVD.df) == "csv.CVD.DE$time_iso8601"] <- "Date"

# indices 
names(SP500.df)[names(SP500.df) == "SP500.dates"] <- "Date"
names(NASDAQ.df)[names(NASDAQ.df) == "NASDAQ.dates"] <- "Date"
names(DJI.df)[names(DJI.df) == "DJI.dates"] <- "Date"
names(TSX.df)[names(TSX.df) == "TSX.dates"] <- "Date"
names(DAX.df)[names(DAX.df) == "DAX.dates"] <- "Date"

# rename column names for country data frames to "Daily cases"
names(CAN.CVD.df)[names(CAN.CVD.df) == "CAN.CVD.data$numtoday"] <- "Daily cases"
names(USA.CVD.df)[names(USA.CVD.df) == "new_case"] <- "Daily cases"
names(DE.CVD.df)[names(DE.CVD.df) == "DE.daily.cases"] <- "Daily cases"

# rename column names for index data frames to "Closing price"
names(SP500.df)[names(SP500.df) == "SP500.close"] <- "Closing price"
names(NASDAQ.df)[names(NASDAQ.df) == "NASDAQ.close"] <- "Closing price"
names(DJI.df)[names(DJI.df) == "DJI.close"] <- "Closing price"
names(TSX.df)[names(TSX.df) == "TSX.close"] <- "Closing price"
names(DAX.df)[names(DAX.df) == "DAX.close"] <- "Closing price"

# cast values in Date columns as Dates 
# countries 
CAN.CVD.df$Date <- as.Date(CAN.CVD.df$Date)
USA.CVD.df$Date <- as.Date(USA.CVD.df$Date)
DE.CVD.df$Date <- as.Date(DE.CVD.df$Date)
# indices
SP500.df$Date <- as.Date(SP500.df$Date)
NASDAQ.df$Date <- as.Date(NASDAQ.df$Date)
DJI.df$Date <- as.Date(DJI.df$Date)
TSX.df$Date <- as.Date(TSX.df$Date)
DAX.df$Date <- as.Date(DAX.df$Date)


# inner join merge of data frames for each country 
# such that equal dates with values match
# USA
USA.SP500.merged <- merge(x= USA.CVD.df, y=SP500.df, by = 'Date')
USA.NASDAQ.merged <- merge(x= USA.CVD.df, y=NASDAQ.df, by = 'Date')
USA.DJI.merged <- merge(x= USA.CVD.df, y=DJI.df, by = 'Date')
# Canada
CAN.TSX.merged <- merge(x= CAN.CVD.df, y=TSX.df, by = 'Date')
# Germany
DE.DAX.merged <- merge(x= DE.CVD.df, y=DAX.df, by = 'Date')


# test correlation 
# USA
cor(USA.SP500.merged$`Closing price`, USA.SP500.merged$`Daily cases`)
cor(USA.NASDAQ.merged$`Closing price`, USA.NASDAQ.merged$`Daily cases`)
cor(USA.DJI.merged$`Closing price`, USA.NASDAQ.merged$`Daily cases`)
# Canada
cor(CAN.TSX.merged$`Closing price`, CAN.TSX.merged$`Daily cases`)
# Germany
cor(DE.DAX.merged$`Closing price`, DE.DAX.merged$`Daily cases`)





