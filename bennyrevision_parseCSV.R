setwd("C:/dev/R") #changes the working directory to a local drive

library(dplyr)
library(ggplot2)
library(ggthemes)



dataFile <- file.choose()
rawdata <-read.csv(dataFile)  #rawdata=original data set
date=rawdata$Date           #Sets each column to a specific variable
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

#Now discard the data from before the experiment started with turning on standby mode, maybe not super necessary
findbeginning=apply(reduced_columns,1,function(lR) {any(grepl(lR,pattern="Schedule TEM started"))})
scheduleTEMmodebeginning=sreduced_columns[findbeginning,]
beginningrow=as.numeric(rownames(scheduleTEMmodebeginning))
max(beginningrow)           
scheduleTEMmode = slice(reduced_columns, -(1:max(beginningrow)))
View(scheduleTEMmode)


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
adj_start = rnstart[-length(rnstart)]
adj_end = rnend[-1]
View(scheduleTEMmode)
count(scheduleTEMmode, Log)


#example of what I want to do to each day
onedaytest = slice(scheduleTEMmode, adj_start[1]:adj_end[1])
View(onedaytest)

#percent change function between two Iprime values
percentchange = function(Iprime1, Iprime2)
  {
  return ((Iprime2-Iprime1)/Iprime1)*100
  }

percentchanges = c()
for (i in 1:length(onedaytest$Iprime)){
  percentchanges[i] = percentchange(onedaytest$Iprime[i], onedaytest$Iprime[i+10])
}

onedaytest$percentchanges <- percentchanges
View(onedaytest)

onedaytest %>% ggplot()+
  geom_point(aes(x=srcage, y=log(percentchanges)))+
  ggsave("Log_percent_changes.png")









Ip_list = pull(scheduleTEMmode, Iprime) #pull the Iprime data so we can chop it up into equal sized pieces of ON data
lengths = c()
for (i in 1:length(adj_start))
  {
  lengths[i] = length(Ip_list[adj_start[i]:adj_end[i]])
  }
lengths #each day will likely have a different number of data points during mode 2

for (i in 1:length(adj_start))
  {
  
  }


















listofIprimes = c()#Initialize empty dataframe 
df = data.frame()
for (i in 1:length(adj_start)){   #iterate over the number of starts
    onedayIprime <- a[adj_start[i]:(adj_start[i]+535)]
    df[, ncol(df)+1] <- onedayIprime
    colnames(df)[ncol(df)] <- paste0("day", i)
}










#Quick inspection routine to see if the search method above missed any of the log messages.
# This is to ensure we continue through to the end of our data set, instead of ending at a break line.
# If the last row of rn is not equal to the number of rows in the lean dataset, then make a new dataframe with the two columns being 
if (max(rn) != nrow(scheduleTEMmode)) {
  rn= c(rn,nrow(scheduleTEMmode))
}
rn
lrn
# Finding the max length of the "ON" or Mode 2 case. (We'll need each chunk to have the same number of rows to write to a data frame.)
k=1
dif=c()
for (i in 2:length(rnstart)) {
  intdif = rnstart[i] - rnstart[k]
  dif=c(dif,intdif)
  maxdif=max(dif)
  k=k+1
}
dif
