library(readxl)

deathsxl <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"

if (!file.exists(file.path(getwd(), "Corona", "Germany", "Data", "GermanyDeaths2016_2020.xls"))){
  download.file(deathsxl, file.path(getwd(), "Corona", "Germany", "Data", "GermanyDeaths2016_2020.xls"))
}