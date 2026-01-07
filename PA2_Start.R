# Reproducible Research: Peer Assessment 2

library(plyr)
library(knitr)
library(ggplot2)
library(grid)

setwd("/testx")
NOAA <- read.csv("FStormData.csv", stringsAsFactors = FALSE, strip.white=TRUE, header=TRUE)

FilteredNOAA <-NOAA[ , c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES",
                         "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

FilteredNOAA$BGN_DATE <- as.POSIXct(FilteredNOAA$BGN_DATE,format="%m/%d/%Y %H:%M:%S")

TransformPropDamage <- mapvalues(FilteredNOAA$PROPDMGEXP,
                               c("K","M","", "B","m","+","0","5","6","?","4","2","3","h","7","H","-","1","8"), 
                               c(10^3,10^6, 1, 10^9,10^6, 1, 1,10^5,10^6, 1,10^4,10^2,10^3, 1,10^7,10^2, 1, 10,10^8))

TransformCropDamage <- mapvalues(FilteredNOAA$CROPDMGEXP,
                               c("","M","K","m","B","?","0","k","2"),
                               c( 1,10^6,10^3,10^6,10^9,1,1,10^3,10^2))

FilteredNOAA$PROPTOTALDMG <- as.numeric(TransformPropDamage) * FilteredNOAA$PROPDMG

FilteredNOAA$CROPTOTALDMG <- as.numeric(TransformCropDamage) * FilteredNOAA$CROPDMG

FilteredNOAA$TOTALDMG <- FilteredNOAA$PROPTOTALDMG + FilteredNOAA$CROPTOTALDMG

dataf <- data.frame(tolower(gsub(" ","", NOAA$EVTYPE, fixed=TRUE)),
                    NOAA$FATALITIES,NOAA$INJURIES,
                    NOAA$PROPDMG,tolower(NOAA$PROPDMGEXP),
                    NOAA$CROPDMG,tolower(NOAA$CROPDMGEXP)
                    )
                
names(dataf) <- c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")

fai = dataf[dataf$FATALITIES > 0 | dataf$INJURIES > 0 , ]

FAI <- aggregate ( x = list ( fai$FATALITIES, fai$INJURIES ),
                   by= list ( fai$EVTYPE), FUN=sum, na.rm = TRUE)
                   
names(FAI) <- c("EVTYPE","FATALITIES","INJURIES")

FAIplot <- head(FAI[order(-FAI$FATALITIES, -FAI$INJURIES) , ])
 
barplot(FAIplot$FATALITIES, 
        main="Fatalities per event", 
        col=c("dark green","green"),
        xlab="Event type", ylab="Fatalities", 
        cex.axis=0.6, cex.names=0.7,
        names.arg = FAIplot$EVTYPE, 
        las= 1)

barplot(FAIplot$INJURIES, 
        main="Injuries per event", 
        col=c("dark green","green"),
        xlab="Event type", ylab="Fatalities", 
        cex.axis=0.6, cex.names=0.7,
        names.arg = FAIplot$EVTYPE, 
        las= 1)
  
EconImpact = dataf[dataf$PROPDMGEXP %in% c("h","k","m","b") & 
             dataf$PROPDMG > 0 & 
             dataf$CROPDMGEXP %in% c("h","k","m","b") & 
             dataf$CROPDMG > 0, ]
        
EconImpact$CROPDMGEXP <- as.numeric( 
                      lapply(EconImpact$CROPDMGEXP, 
                             function(x) if      (x=="h") 100 
                                         else if (x=="k") 1000 
                                         else if (x=="m") 1e+06
                                         else if (x=="b") 1e+09
                                         else 0 ))
                                         
EconImpact$PROPDMGEXP <- as.numeric(
                      lapply(EconImpact$PROPDMGEXP, 
                             function(x) if      (x=="h") 100 
                                         else if (x=="k") 1000
                                         else if (x=="m") 1e+06
                                         else if (x=="b") 1e+09 
                                         else 0 ))
                                         
ECONIMPACT <- aggregate(x = list(EconImpact$PROPDMG*EconImpact$PROPDMGEXP,EconImpact$CROPDMG*EconImpact$CROPDMGEXP ),
                         by=list(EconImpact$EVTYPE), FUN=sum, na.rm = TRUE)

names(ECONIMPACT) <- c("EVTYPE","PRODMGs","CROPDMGs")

ECONIMPACTplot <- head(ECONIMPACT[order(-ECONIMPACT$PRODMGs, -ECONIMPACT$PRODMGs) , ])

barplot( ( ECONIMPACTplot$PRODMGs+ECONIMPACTplot$CROPDMGs) / 1000000, 
         main="Economic consequences per event", 
         col=c("dark green","dark blue"),
         xlab="Event", 
         ylab="Damage (Million Dollars)",
         cex.axis=0.5, cex.names=0.6, 
         names.arg = ECONIMPACTplot$EVTYPE, las= 1, beside = TRUE)  