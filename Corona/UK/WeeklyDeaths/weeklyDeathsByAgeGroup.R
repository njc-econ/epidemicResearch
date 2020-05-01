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
  wd2020 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek1620201.xlsx"
)


for (i in c(2010:2020)){
  if (i <= 2019){type <- ".xls"} else {type <- ".xlsx"}
  if (!file.exists(file.path(getwd(),"Corona","UK","WeeklyDeaths",paste0("WeeklyDeaths",i,type))) | i == 2020){
    download.file(excelLinks[i-2009], file.path(getwd(),"Corona","UK","WeeklyDeaths",paste0("WeeklyDeaths",i,type)))
  }
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
  
  data[[i - 2009]] <- data.table(read_excel(file.path(getwd(),"Corona","UK","WeeklyDeaths",paste0("WeeklyDeaths",i,type)), sheet = paste0(sheetname, i), skip=2))
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

# deaths by age group are reported at a finer level for the most recent data than the older data
# can consider a comparison over just recent years to plot every available age group or constructing
# the common age groups to get a ten year comparison 

# only 2020 is more detailed

ageCategories <- c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+")

collectData <- function(exceldata){
  
  # first step: identify the column where the category labels are
  column <- 0
  labelIdentified <- FALSE
  while (!labelIdentified){
    column <- column + 1
    labelIdentified <- ageCategories[1] %in% exceldata[[column]]
  }
  
  WeekEnded <- as.integer(unlist(exceldata[grep("Week ended",exceldata[[1]]),.SD,.SDcols=c((column+1):ncol(exceldata))]))
  
  # second step: identify the row where each category is
  rows <- lapply(ageCategories, function(x){grep(x, exceldata[[column]])})
  
  # third step: extract the data
  relevantData <- lapply(rows, function(x){data.table(t(exceldata[x,.SD,.SDcols = c((column+1):ncol(exceldata))]))})
  for (i in c(1:length(relevantData))){
    colnames(relevantData[[i]])<- c("Persons", "Males", "Females")
    relevantData[[i]][,':='(WeekEnded = WeekEnded, AgeGroup = ageCategories[i], WeekNo = c(1:nrow(relevantData[[i]])))]
  }
  
  relevantData <- rbindlist(relevantData)
  return(relevantData)
}

data2010_2019 <- rbindlist(lapply(data[c(1:10)], collectData))

data2010_2019[,':='(WeekEnded = as.Date(WeekEnded, origin = "1899-12-30"))]
data2010_2019[,':='(Persons = as.integer(Persons),
                    Males = as.integer(Males),
                    Females = as.integer(Females))]

ageCategories <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
                       "80-84", "85-89", "90+")

data2020 <- collectData(data[[11]])
data2020[,':='(WeekEnded = as.Date(WeekEnded, origin = "1899-12-30"))]

combinedCategories <- rbindlist(list(
  data.table(oldCategory = "Under 1 year", newCategory = c("<1")),
  data.table(oldCategory = "01-14", newCategory = c("1-4", "5-9", "10-14")),
  data.table(oldCategory = "15-44", newCategory = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")),
  data.table(oldCategory = "45-64", newCategory = c( "45-49", "50-54", "55-59", "60-64")),
  data.table(oldCategory = "65-74", newCategory = c("65-69", "70-74")),
  data.table(oldCategory = "75-84", newCategory = c("75-79", "80-84")),
  data.table(oldCategory = "85+", newCategory = c("85-89", "90+"))
))

data2020 <- merge(data2020, combinedCategories, by.x = "AgeGroup", by.y = "newCategory")

data2020_comparable <- data2020[ , lapply(.SD,sum),keyby=c("oldCategory","WeekNo","WeekEnded"), .SDcols=c("Persons", "Males", "Females")]
setnames(data2020_comparable, "oldCategory", "AgeGroup")


compareWeeks <- data2010_2019[ ,.(maxPersons=max(Persons),
                  minPersons=min(Persons),
                  meanPersons=mean(Persons),
                  maxMales = max(Males),
                  minMales=min(Males),
                  meanMales=mean(Males),
                  maxFemales = max(Females),
                  minFemales=min(Females),
                  meanFemales=mean(Females)), keyby = c("WeekNo","AgeGroup")]

library(ggplot2)

compareWeeksLong <- melt(compareWeeks, id.vars=c("WeekNo","AgeGroup"))

# get three week moving averages of deaths every year
# summarise those 3 month series with mean max and min for the past 10 years

# plot against the same series for 2020





ggplot(compareWeeksLong[AgeGroup == "65-74" & variable %in%c("maxPersons","minPersons","meanPersons")], aes(x=WeekNo, y=value, linetype = variable)) +
  geom_line()
