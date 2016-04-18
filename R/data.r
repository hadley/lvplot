#' Ontime Flight Data
#'
#' Data set detailing on-time performance of national US flights in
#' January 2015. This data is a subset of the data provided by the US Department of
#' Transportation and is available for download from
#' \url{http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time}.
#'
#' @references \url{http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time}
#' @format A data frame consisting of the variables
#' \itemize{
#' \item FlightDate a date variable of the day of the flight
#' \item UniqueCarrier factor variable of the carrier (using the two letter abbreviation)
#' \item TailNum character variable of the tail number
#' \item FlightNum numeric variable of the flight number
#' \item CRSDepTime scheduled departure time in hhmm format
#' \item DepTime actual departure time in hhmm format
#' \item CRSArrTime scheduled arrival time in hhmm format
#' \item ArrTime actual arrival time in hhmm format
#' \item TaxiOut numeric variable of the taxi out time in minutes
#' \item TaxiIn numeric variable of the taxi in time in minutes
#' \item  CarrierDelay	Carrier Delay, in Minutes
#' \item  WeatherDelay	Weather Delay, in Minutes
#' \item  NASDelay	National Air System Delay, in Minutes
#' \item  SecurityDelay	Security Delay, in Minutes
#' \item  LateAircraftDelay	Late Aircraft Delay, in Minutes
#' }
#' @examples
#' data(ontime)
#' library(ggplot2)
#' library(RColorBrewer)
#' greys <- rev(brewer.pal(9, "Greys"))
#' oranges <- brewer.pal(3, "Oranges")
#' colors <- c("white", greys[2:4], oranges[3], greys[4:6], oranges[2], greys[6:8], oranges[1], greys[8:9])
#'
#' p <- ggplot(data = ontime, aes(UniqueCarrier, sqrt(TaxiIn+TaxiOut)))
#' p + geom_lvplot(aes(fill=..LV..)) + scale_fill_manual(values=colors) +theme_bw()
"ontime"

#' County demographics based on 1980 US Census
#'
#' data frame based on the 1980 US Census
#' @format A data frame with 17 variables
#' \itemize{
#' \item name
#' }
"census"
