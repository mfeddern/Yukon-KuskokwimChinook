


# comments ----------------------------------------------------------------

#author: Michael Folkes, Canadian Department of Fisheries and Oceans, Nanaimo, BC, Canada (michael.folkes@dfo-mpo.gc.ca)

#this is an import of the Yukon River water temperature data series produced by Al von Finster (Whitehorse, Yukon) funded by the Yukon River Panel's R&E fund.

#this script merges two data files: the yukon river water temperature data series with station meta data to then allow data grouping by additional variables. 
#In particular, note the variable: station_seriesName, which aggregates stations of a common river-reach when one station was decommissioned but is located close to the currently active station. The metadata also includes geographic coordinates, basin, subbasin, and subsubbasin groupings (as developed by the Water Survey of Canada)

#when complete, the series will run to the end of 2022. Currently there is no plan for additional data collection past 2022.

#revised R scripts and data updates can be obtained from the website of the Pacific Salmon Commission (www.psc.org) or by contacting Michael Folkes


# setup -------------------------------------------------------------------

rm(list=ls())


# data import -------------------------------------------------------------

data.station.df <- read.csv("station.metadata.csv", stringsAsFactors = FALSE)

data.long <- read.csv(file = "yukon_water_temperature_vonfinster.csv", stringsAsFactors = FALSE,comment.char = "#")
data.long$datetime <- as.POSIXct(data.long$datetime_gmt , tz="GMT")

data.long.complete <- merge(data.long, data.station.df, by="stationID", sort = FALSE)

#row count should match:
nrow(data.long)
nrow(data.long.complete)
str(data.long.complete)

