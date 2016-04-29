library(readr)
library(dplyr)

raw <- read_csv("data-raw/On_Time_On_Time_Performance_2015_1.csv.gz")

vars <- c("FlightDate", "UniqueCarrier", "FlightNum", "CRSDepTime",
"DepTime", "CRSArrTime", "ArrTime", "TaxiOut", "TaxiIn", "ArrDelay",
"DepDelay", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay",
"LateAircraftDelay")

ontime <- raw %>%
  select(one_of(vars))

devtools::use_data(ontime, overwrite = TRUE, compress = "xz")
