library(readxl)


deathsxl <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"
setwd("~/Corona/epidemicResearch")
if (!file.exists(file.path(getwd(), "Corona", "Germany", "Data", "GermanyDeaths2016_2020.xlsx"))){
  download.file(deathsxl, file.path(getwd(), "Corona", "Germany", "Data", "GermanyDeaths2016_2020.xlsx"))
}

library(data.table)
library(ggplot2)

data <- list()

for (i in c(2016:2020)){
  data[[i-2015]]<- data.table(read_excel(file.path(getwd(), "Corona", "Germany", "Data", "GermanyDeaths2016_2020.xlsx"),
                                         sheet = paste0("D_",i,"_Tage"),skip = 8))
  
}

germanDeathsByDay <- lapply(data, function(x){
  output <- melt(x, id.vars = 1L, variable.name = "Date", value.name = "Deaths")
  output <- output[!is.na(output[[1]])]
  output[,Deaths := as.integer(Deaths)]
  output
})

germanDeathsByDay <- rbindlist(germanDeathsByDay, use.names = FALSE)

germanDeathsByDay[,':='(Date = as.Date(as.numeric(as.character(Date)), origin = "1899-12-30"))]

# next step finds the week end related to each day, then can sum up to something comparable 
# to the England/Wales weekly data

# first week end will be 08-Jan-2016
# easiest method is to create a matching table with dates and make a join

lastDays <- germanDeathsByDay[,.(lastDay=max(Date,na.rm=TRUE),lastSunday=max(Date[wday(Date)==7],na.rm=TRUE))]

weekLookup <- data.table(Day=seq.Date(from = as.Date("2016-01-02"), to = lastDays$lastSunday - 1, by="1 day") ,
                         WeekEnd=do.call("c",lapply(seq.Date(from = as.Date("2016-01-08"), to = lastDays$lastSunday, by="1 week"), function(x){rep(x, 7)})))

germanDeathsByWeek <- merge(germanDeathsByDay, weekLookup, by.x = "Date", by.y = "Day")
germanDeathsByWeek <- germanDeathsByWeek[,.(Deaths = sum(Deaths, na.rm = TRUE)), keyby = .(`unter … Jahren`, WeekEnd)]

# assign a number to each week in the year
weeks <- germanDeathsByWeek[,.(weeks=unique(WeekEnd)),keyby=year(WeekEnd)][,':='(WeekNo=c(1:.N)),by=year(weeks)]

germanDeathsByWeek <- merge(germanDeathsByWeek,weeks, by.x="WeekEnd", by.y = "weeks", all.x=TRUE)

averageDeaths2016_2019 <- germanDeathsByWeek[year < 2020,.(MaxDeaths = max(Deaths),
                                 MinDeaths = min(Deaths),
                                 AveDeaths = mean(Deaths)),
                   keyby=.(`unter … Jahren`,WeekNo)]

comparisonData <- merge(averageDeaths2016_2019, germanDeathsByWeek[year == 2020, .(`unter … Jahren`, Deaths2020 = Deaths, WeekNo)],by=c("unter … Jahren", "WeekNo"), all.x = TRUE)
