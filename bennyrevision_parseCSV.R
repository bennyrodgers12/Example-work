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
#View(reduced_columns)


#### SORT OUT "BAD" STARTS  ####
#For example, chD had to be reconnected during its 2nd official day in TEM mode,causing another "Schedule TEM started" message to occur
#This code lets you choose the most recent "Schedule TEM started" log message to start from

findbeginning=apply(reduced_columns,1,function(lR) {any(grepl(lR,pattern="Schedule TEM started"))})
scheduleTEMmodebeginning=reduced_columns[findbeginning,]
beginningrow=as.numeric(rownames(scheduleTEMmodebeginning))

max(beginningrow)           
scheduleTEMmode = slice(reduced_columns, -(1:max(beginningrow)))
#View(scheduleTEMmode)

#Now the dataframe should not be interrupted by any more log messages saying "Schedule TEM mode started"

#### end ####

# Search through to find the log entry that says the emitter is starting up
started=apply(scheduleTEMmode,1,function(started) {any(grepl(started,pattern="Entering Mode 2"))})  #started is a boolean list, with TRUE for rows that have the log message we're searching for
scheduleTEMmodestarted=scheduleTEMmode[started,]   #Is only the rows with the rampdown.
rnstart=as.numeric(rownames(scheduleTEMmodestarted))      #Gives list of row numbers where we want to break.
startedntimes=length(rnstart)
rnstart #print row indices where the log message says entering mode 2
startedntimes #print the number of days that the emitter has entered its "on" mode


# Search through to find the log entry that says the emitter is ramping down
ended=apply(scheduleTEMmode,1,function(ended) {any(grepl(ended,pattern="Entering Mode 1"))})  #ended is a boolean list, with TRUE for rows that have the log message we're searching for
scheduleTEMmodeended=scheduleTEMmode[ended,]   #Is only the rows with the rampdown.
rnend=as.numeric(rownames(scheduleTEMmodeended))      #Gives list of row numbers where we want to break.
endedntimes=length(rnend)
rnend #print row indices where the log message says entering mode 2
endedntimes #print the number of days that the emitter has been entered its "on" mode

#need to drop the first move to mode 1 and the last move to mode 2... these will always be incomplete due to workday & when mode was started!
adj_start = rnstart[-length(rnstart)] #removes the most recent startup from the list of startup rownames
adj_end = rnend[-1] #removes the first shutdown
adj_start
adj_end
dframe = scheduleTEMmode %>%
  slice(adj_start[1]:adj_end[length(adj_end)]) %>%
  mutate(`15 minute pct change (%)` = ((lead(Iprime,15)/Iprime) - 1)*100) %>%
  mutate(`Day` = cumsum(ifelse(Log == "Entering Mode 2", 1, 0)))%>%
  mutate( on.off = ifelse(Vext>max(Vext, na.rm = TRUE)-500 & Vext < max(Vext, na.rm = TRUE)+500 & Ifil>2.2 , TRUE, FALSE))%>%
  mutate(`datetime` = mdy_hms(date))%>%
  mutate(`Weekday` = wday(datetime, label = TRUE))
  #mutate(`Normalized time (hrs)`, ...)  want to add another column that tracks time during day of operation so that multiple days can be plotted with eachother
  #on/off column is brute forced with vext and Ifil. Need it to calculate daily avgs and stdevs. 

View(dframe)
#NOTE: weekday is Sunday =1, Monday =2, etc




#### Calculate the daily stable current ####

# We chose the first instance where the current (net) changes by <1% in the next 15 minutes
# Could improve this by requiring >1% net 15 minute change for say 5 minutes ? Or not

dailystable = c()
timetostable = c()
stdev = c()
avgIprime = c()
weekday = c()
for (i in unique(dframe$Day))
  {
  timetostable[i] = filter(dframe, Day == i)$srcage[match(1, ifelse(filter(dframe, Day == i)$`15 minute pct change (%)` < 1, 1, 0))]-
    filter(dframe, Day == i)$srcage[1]
  dailystable[i] = filter(dframe, Day==i)$Iprime[match(1, ifelse(filter(dframe, Day ==i)$`15 minute pct change (%)`<1, 1, 0))]
  stdev[i] = sd(filter(dframe, Day == i & on.off == TRUE & srcage > filter(dframe, Day == i)$srcage[match(1, ifelse(filter(dframe, Day == i)$`15 minute pct change (%)` < 1, 1, 0))])$Iprime)
  avgIprime[i] = mean(filter(dframe, Day == i & on.off == TRUE & srcage > filter(dframe, Day == i)$srcage[match(1, ifelse(filter(dframe, Day == i)$`15 minute pct change (%)` < 1, 1, 0))])$Iprime)
  weekday[i] = filter(dframe, Day == i)$Weekday[1]
}
dailystable
timetostable
stdev
avgIprime
weekday







tdata <- select(dframe, Day, Temp) %>% na.omit() #give me just the temperature recordings
tdata1 = tdata[order(nrow(tdata):1),] #rverse the order of the dataset in order to get most recent temp measurement on that day (some were rerecorded)
temps = distinct(tdata1, Day, .keep_all = TRUE)[order(nrow(distinct(tdata1, Day, .keep_all = TRUE)):1),] #use distinct by "day", then flip the rows again to be chronological

stablecurrentdata = data.frame( stable.iprime = dailystable,
                                avg.iprime = avgIprime,
                                stdev.iprime = stdev,
                                stable.avg.difference = dailystable - avgIprime,
                                timetostable.minutes = timetostable*60,
                                Day = c(1:length(dailystable)),
                                dayofweek = weekday)
#View(stablecurrentdata)

stablecurrentdatatemp = merge(stablecurrentdata, temps, by = "Day", all.x = TRUE)
View(stablecurrentdatatemp)
write.csv(stablecurrentdatatemp,"sn500005-stabilitydata.csv")
#### end ####


