library(readxl)

excelLinks <- c(
  wd2010 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2010/publishedweek2010.xls",
  wd2011 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2011/publishedweek2011.xls",
  wd2012 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2012/publishedweek2012.xls",
  wd2013 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2013/publishedweek2013.xls",
  wd2014 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2014/publishedweek2014.xls",
  wd2015 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2015/publishedweek2015.xls",
  wd2016 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2016/publishedweek522016.xls",
  wd2017 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2017/publishedweek522017.xls",
  wd2018 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2018/publishedweek522018withupdatedrespiratoryrow.xls",
  wd2019 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2019/publishedweek522019.xls",
  wd2020 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek152020corrected.xlsx"
)


for (i in c(2010:2020)){
  if (i <= 2019){type <- ".xls"} else {type <- ".xlsx"}
  download.file(excelLinks[i-2009], file.path(getwd(),paste0("WeeklyDeaths",i,type)))
}

library(data.table)
data <- list()
for (i in c(2010:2020)){
  if (i <= 2019){
    type <- ".xls"
  } else {type <- ".xlsx"}
  if (i <= 2015){
    sheetname <- "Weekly Figures "
  } else {sheetname <- "Weekly figures "}
  
  data[[i - 2009]] <- data.table(read_excel(file.path(getwd(),paste0("WeeklyDeaths",i,type)), sheet = paste0(sheetname, i), skip=2))
} 

rm(i,type, sheetname)

# there is an inconsistency in how the dates are stored in the file for 2011
# until April cells have '7-Jan-11, then 29/04/2011
problemRow <- grep("Week ended",data[[2]][[1]])

# easiest solution takes the first correct excel date and takes multiples of seven from that date
problemCells <- unlist(data[[2]][problemRow, drop=TRUE])
numericCells<- grep("[0-9]{5}",problemCells)
firstValid <- as.numeric(problemCells[min(numericCells)])

for (i in 2:(min(numericCells)-1)){
  data[[2]][problemRow, i] <- as.character(firstValid - (min(numericCells)-i)*7)
}

# from all Excel sheets just need to take the "Week ended" row and the "Total deaths, all ages" row
# at least as a starting point

deathsByWeek_dt <- rbindlist(lapply(data, function(x){
  WeekEnded <- as.integer(unlist(x[grep("Week ended",x[[1]]),.SD,.SDcols=c(2:ncol(x))]))
  NoDeaths <- as.integer(unlist(x[grep("^Total deaths, all ages",x[[1]]),.SD,.SDcols=c(2:ncol(x))]))
  data.table(WeekEnded = WeekEnded, NoDeaths = NoDeaths)
}))

# remove rows where the number of deaths is missing
deathsByWeek_dt <- deathsByWeek_dt[!is.na(NoDeaths)]

# convert the Excel date to an R date
deathsByWeek_dt[ , WeekEnded := as.Date(WeekEnded, origin = "1899-12-30")]

# create an indicator for weeks at the end of March and start of April
deathsByWeek_dt[(as.numeric(format(WeekEnded,"%d"))>15 & as.numeric(format(WeekEnded,"%m"))==3)|
                  (as.numeric(format(WeekEnded,"%d"))<=15 & as.numeric(format(WeekEnded,"%m"))==4),
                endMarchStartApril := TRUE]
set(deathsByWeek_dt, which(is.na(deathsByWeek_dt$endMarchStartApril)),"endMarchStartApril",FALSE)

library(ggplot2)

ggplot(deathsByWeek_dt, aes(x = WeekEnded, y = NoDeaths, size=endMarchStartApril, shape=endMarchStartApril, col=endMarchStartApril)) + geom_point(aes(group=1)) +
  xlab("Week End Date") + ylab("No. of Deaths") + labs(title = "No. of Deaths by Week 2010-2020, England & Wales") + 
  scale_shape_manual(values=c(15,16))+
  scale_size_manual(values = c(2,4))+
  scale_color_manual(values = c("red","black"))+
  coord_cartesian(xlim = c(as.Date("2010-01-01"),as.Date("2020-12-31")), # This focuses the x-axis on the range of interest
                  clip = 'off')+
  theme_minimal() + annotate("text",label="Source: ONS",x=as.Date("2022-03-01"),y=5000)# + geom_line(aes(group=1))

