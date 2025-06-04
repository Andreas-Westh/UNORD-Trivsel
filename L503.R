# Main goal of this script
# Feature engineer the valid contract time period based on:
# 1. Start date
# 2. Contract signed date
# 3. 


# currently testing from local file, will later come from data lake 
# and go into d ata warehouse

library(readxl)


L503 <- read_xlsx("data/Odin_L503_GF-elever_ med_aftale.xlsx")
# Date format is Y-m-d

# converts into unix timecode
L503$Startdato_unix <- as.numeric(as.POSIXct(L503$Startdato, tz = "UTC"))
L503$Underskriftsdato_unix<- as.numeric(as.POSIXct(L503$Underskriftsdato, tz = "UTC"))

# calcualte 90 days into unix timecode
unix_value_90_days <- 90*24*60*60

# find earliest date for valid contract
L503$Unix_90_days <- L503$Startdato_unix - unix_value_90_days
# converts back to actual readable date format
L503$Earliest_date <- format(as.POSIXct(L503$Unix_90_days, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")

# get a boolean, to check, startdato can be changed for a different date
L503$Kontrakt_rettidigt <- L503$Underskriftsdato_unix >= L503$Unix_90_days &
  L503$Underskriftsdato_unix <= L503$Startdato_unix

