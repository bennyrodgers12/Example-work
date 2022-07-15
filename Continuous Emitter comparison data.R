library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)

dataFile <- file.choose()
rawdata <-read.csv(dataFile)  #rawdata=original data set
date=rawdata$Date           #Sets each column to a specific variable. Ifil easier to access later than Filament..A. in my opinion
Ifil=rawdata$Filament..A.
Vext=rawdata$Extractor..V.
Itot=rawdata$Total.current..nA.
Icup=rawdata$Cup..nA
Iprime=rawdata$I...mA.sr
Temp=rawdata$Temperature..K.
Log=rawdata$Log
srcage=rawdata$Source.age..hrs.
reduced_columns=data.frame(date,srcage,Ifil,Vext,Itot,Icup,Iprime,Temp,Log)  #Puts the desired columns in a new data frame for viewing simplicity
View(reduced_columns)


#### Routine to get 8hr data from a continuously running emitter ####

# Need to manually change the start date, and the range for the for-loop.
# This script assumes that the emitter starts stabilized for each 9 hr chunk of data.
startdate = "2021-07-08 08:00:00" #must be 8am so the day counter works, choose the day

dframecontinuous = reduced_columns %>%
  mutate(`15 minute pct change (%)` = ((lead(Iprime,15)/Iprime) - 1)*100)%>%
  mutate(`datetime` = mdy_hms(reduced_columns$date))%>%
  mutate(`on.off` = ifelse(hour(datetime) >= 8 & hour(datetime) < 17 & Vext > 4000, 1, 0))%>%
  mutate(`Day` = interval(startdate, datetime)  %/% days(1))%>%
  filter(datetime > startdate)
View(dframecontinuous)

dailystable = c()
stdev = c()
avgIprime = c()
for (i in (1:14)) #probably need to manually change this depending on emitter too
{
  
  dailystable[i] = filter(dframecontinuous, Day == i & on.off == 1)$Iprime[match(1, ifelse(filter(dframecontinuous, Day == i & on.off == TRUE)$`15 minute pct change (%)` < 1, 1, 0))]
  
  stdev[i] = sd(filter(dframecontinuous, Day == i & on.off == 1)$Iprime, na.rm=TRUE)
  # don't have to filter as much data here, because emitter starts stable basically every time
  avgIprime[i] = mean(filter(dframecontinuous, Day == i & on.off == 1)$Iprime, na.rm=TRUE)
  
}
dailystable
stdev
avgIprime

stablecurrentdata.cont = data.frame( stable.iprime = dailystable,
                                avg.iprime = avgIprime,
                                stdev.iprime = stdev,
                                stable.avg.difference = dailystable - avgIprime,
                                day.counter = c(1:length(dailystable)),
                                ID = "sn500113")
View(stablecurrentdata.cont)
write.csv(stablecurrentdata.cont, "sn500113 stability data.csv")
