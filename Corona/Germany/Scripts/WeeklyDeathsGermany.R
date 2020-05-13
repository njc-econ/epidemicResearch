library(readxl)


deathsxl <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"

if (!file.exists(file.path(getwd(), "Corona", "Germany", "Data", "GermanyDeaths2016_2020.xlsx"))){
  download.file(deathsxl, file.path(getwd(), "Corona", "Germany", "Data", "GermanyDeaths2016_2020.xlsx"))
}

library(data.table)

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

weekLookup <- data.table(Day=seq.Date(from = as.Date("2016-01-02"), to = as.Date("2020-04-10"), by="1 day") ,
                         WeekEnd=do.call("c",lapply(seq.Date(from = as.Date("2016-01-08"), to = as.Date("2020-04-12"), by="1 week"), function(x){rep(x, 7)})))

germanDeathsByWeek <- merge(germanDeathsByDay, weekLookup, by.x = "Date", by.y = "Day")
germanDeathsByWeek <- germanDeathsByWeek[,.(Deaths = sum(Deaths, na.rm = TRUE)), keyby = .(`unter … Jahren`, WeekEnd)]

germanDeathsByWeek[(as.numeric(format(WeekEnd,"%d"))>15 & as.numeric(format(WeekEnd,"%m"))==3)|
                  (as.numeric(format(WeekEnd,"%d"))<=22 & as.numeric(format(WeekEnd,"%m"))==4),
                endMarchStartApril := TRUE]
set(germanDeathsByWeek, which(is.na(germanDeathsByWeek$endMarchStartApril)),"endMarchStartApril",FALSE)

weeklyDeathsPlotGermany <- ggplot(germanDeathsByWeek[`unter … Jahren`=="Insgesamt"], aes(x = WeekEnd, y = Deaths, size=endMarchStartApril, shape=endMarchStartApril, col=endMarchStartApril)) + geom_point(aes(group=1)) +
  xlab("Week End Date") + ylab("No. of Deaths") + labs(title = "No. of Deaths by Week 2016-2020, Germany") + 
  scale_shape_manual(values=c(15,16), labels=c("Any Other Time","15th Mar - 22nd Apr"))+
  scale_size_manual(values = c(1,1.5), labels=c("Any Other Time","15th Mar - 22nd Apr"))+
  scale_color_manual(values = c("red","black"), labels=c("Any Other Time","15th Mar - 22nd Apr"))+
  coord_cartesian(xlim = c(as.Date("2016-01-01"),as.Date("2020-12-31")), # This focuses the x-axis on the range of interest
                  ylim = c(15000, max(germanDeathsByWeek$Deaths)*1.05),
                  clip = 'off')+
  annotate("text",label="Source: Destatis",x=as.Date("2021-03-01"),y=14500, size = 1)+
  theme_minimal(base_size = 4) +
  theme(legend.spacing.y = unit(0.1,"in"),
        legend.key.size = unit(0.1, "in"))  +
  guides(shape=guide_legend(title = "", reverse = TRUE),size=guide_legend(title = "", reverse = TRUE), color = guide_legend(title = "", reverse = TRUE))

ggsave(paste0("weeklyDeathsGermany",format(Sys.Date(),"%Y_%m_%d"),".png"), weeklyDeathsPlotGermany, units = "in", dpi = 300, width = 1200/300, height=630/300)


