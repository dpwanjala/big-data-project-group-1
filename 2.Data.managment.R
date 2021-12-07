# read csv files for stock exchange indices 
## we have to change this so it reads from the RAWdata Folder, 
## but I couldnt figure it out


list.files()
csv.SP500 <- read.csv("SP500.5Years.csv")
csv.NASDAQ <- read.csv("NASDAQ.5Years.csv")
csv.DJI <- read.csv("DowJones.5Years.csv")
csv.TSX <- read.csv("tsx_composite_index_canada.csv")
csv.DAX <- read.csv("DAX.5Years.csv")


#Commented this for now
# Reads files from raw data folders
# Gotta have files in raw data folder for it to work!

# csv.SP500 <- read.csv(paste(p.data.raw, "SP500.5Years.csv", sep = ""))
# csv.NASDAQ <- read.csv(paste(p.data.raw, "NASDAQ.5Years.csv", sep = ""))
# csv.DJI <- read.csv(paste(p.data.raw, "DowJones.5Years.csv",sep = ""))
# csv.TSX <- read.csv(paste(p.data.raw, "tsx_composite_index_canada.csv", sep = ""))
# csv.DAX <- read.csv(paste(p.data.raw, "DAX.5Years.csv", sep = ""))

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

#SAME SHIT reading from folders
#commented for now

# csv.CVD.CAN <- read.csv(paste(p.data.raw, "covid19canada-download.csv", sep = ""))
# csv.CVD.USA <- read.csv(paste(p.data.raw, "CoronaUSDec12.csv", sep = ""))
# csv.CVD.DE <- read.csv(paste(p.data.raw, "cases-rki-by-state_germany.csv", sep = ""))

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
csv.DJI$Low <- as.numeric(gsub(",","",csv.DJI$Low))
csv.DJI$High <- as.numeric(gsub(",","",csv.DJI$High))
csv.DAX$Price <- as.numeric(gsub(",","",csv.DAX$Price))
csv.DAX$Low <- as.numeric(gsub(",","",csv.DAX$Low))
csv.DAX$High <- as.numeric(gsub(",","",csv.DAX$High))




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







