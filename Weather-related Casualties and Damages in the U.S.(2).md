# Weather-related Casualties and Damages in the U.S.
###==================================================================
## Reading data

```r
library(stringr)
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.1
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.1.1
```

```r
library(ggplot2)

if (!file.exists("stormData.csv.bz2")){
   setInternet2(use = TRUE)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormData.csv.bz2")     
}

rawStorm = read.csv(bzfile("StormData.csv.bz2"), as.is = TRUE, header = TRUE)
```

```
## Warning: EOF within quoted string
```

```r
# let's take a peek at the data:
summary(rawStorm)
```

```
##    STATE__            BGN_DATE           BGN_TIME        
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##   TIME_ZONE            COUNTY           COUNTYNAME       
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##     STATE              EVTYPE           BGN_RANGE        
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##    BGN_AZI           BGN_LOCATI          END_DATE        
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##    END_TIME          COUNTY_END         COUNTYENDN       
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##   END_RANGE           END_AZI           END_LOCATI       
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##     LENGTH             WIDTH                F            
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##      MAG             FATALITIES          INJURIES        
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##    PROPDMG           PROPDMGEXP          CROPDMG         
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##   CROPDMGEXP            WFO             STATEOFFIC       
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##   ZONENAMES           LATITUDE          LONGITUDE        
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##   LATITUDE_E         LONGITUDE_          REMARKS         
##  Length:692288      Length:692288      Length:692288     
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##     REFNUM         
##  Length:692288     
##  Class :character  
##  Mode  :character
```

```r
# convert crop and property amounts into total dollars based on EXP fields
# first change all char values to lower case
rawStorm$PROPDMGEXP = tolower(rawStorm$PROPDMGEXP)

rawStorm$PROPDMGEXP = gsub('-', 0, rawStorm$PROPDMGEXP)
rawStorm$PROPDMGEXP = gsub('h', 2, rawStorm$PROPDMGEXP)
rawStorm$PROPDMGEXP = gsub('k', 3, rawStorm$PROPDMGEXP)
rawStorm$PROPDMGEXP = gsub('m', 6, rawStorm$PROPDMGEXP)
rawStorm$PROPDMGEXP = gsub('b', 9, rawStorm$PROPDMGEXP)

rawStorm$PROPDMGEXP = as.numeric(rawStorm$PROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
rawStorm$PROPDMGEXP[is.na(rawStorm$PROPDMGEXP) == TRUE] = 0

# calculate new property damage
rawStorm$propDamage = ( rawStorm$PROPDMG * ( 10 ^ rawStorm$PROPDMGEXP ) )
```

```
## Error: non-numeric argument to binary operator
```

## Results
### Which events are most harmful to pop. health?

```r
# create data frame of total and average population harms by event type
harmByEvent = rawStorm %.%
  group_by(EVTYPE) %.%
  summarize(cntEvents = n(), totalFatalities = sum(FATALITIES), totalInjuries = sum(INJURIES), 
            avgFatalities = totalFatalities / cntEvents, avgInjuries = totalInjuries / cntEvents )
```

```
## Error: invalid 'type' (character) of argument
```

```r
# order by occurence of event
harmByEvent = arrange(harmByEvent, -cntEvents)
```

```
## Error: object 'harmByEvent' not found
```

```r
# convert event types to lowercase format
harmByEvent$EVTYPE = paste0( tolower(harmByEvent$EVTYPE), " (", harmByEvent$cntEvents, ")" )
```

```
## Error: object 'harmByEvent' not found
```

```r
# plot top ten
# create plot data frame and melt (for easier plotting)
dfPlot = melt(harmByEvent[1:10,][c('EVTYPE', 'avgFatalities', 'avgInjuries')], id.vars='EVTYPE')
```

```
## Error: object 'harmByEvent' not found
```

```r
dfPlot = arrange(dfPlot, variable, -value)
```

```
## Error: object 'dfPlot' not found
```

```r
# plot top ten events
ggplot(dfPlot, aes(x = EVTYPE, y = value, color = variable)) + 
  geom_point(stat = 'identity', size = 3) + 
  scale_x_discrete(limits = dfPlot$EVTYPE[1:10]) + 
  coord_flip() +
  xlab('event type (n)') + ylab('average harms per event') + 
  theme_bw() + 
  ggtitle('average harms (injuries/fatalities)\nby event type')
```

```
## Error: object 'dfPlot' not found
```
Of the most prevalent weather event types, tornadoes not only have the highest average deaths per event, but also the highest average injuries per event - and by a wide margin.
### Which events are most harmful to economy?

```r
# create data frame of property damage by event type  
costByEvent = rawStorm %.%
  group_by(EVTYPE) %.%
  summarize(cntEvents = n(), totalPropDamage = sum(propDamage), avgPropDamage = totalPropDamage / cntEvents )
```

```
## Error: object 'propDamage' not found
```

```r
# order by occurence of event
costByEvent = arrange(costByEvent, -cntEvents)
```

```
## Error: object 'costByEvent' not found
```

```r
# convert event types to lowercase format
costByEvent$EVTYPE = paste0( tolower(costByEvent$EVTYPE), " (", costByEvent$cntEvents, ")" )
```

```
## Error: object 'costByEvent' not found
```

```r
# plot top ten
# create plot data frame
dfCostPlot = costByEvent[1:10, ]
```

```
## Error: object 'costByEvent' not found
```

```r
dfCostPlot = arrange(dfCostPlot, -avgPropDamage)
```

```
## Error: object 'dfCostPlot' not found
```

```r
# plot top ten events
ggplot(dfCostPlot, aes(x = EVTYPE, y = avgPropDamage) ) +
  geom_bar(stat = 'identity') + 
  scale_x_discrete(limits = dfCostPlot$EVTYPE[1:10]) + 
  coord_flip() +
  xlab('event type (n)') + ylab('average property damage per event') + 
  theme_bw() + ggtitle('average property damage\nby event type')
```

```
## Error: object 'dfCostPlot' not found
```
Of the most prevalent weather event types, floods have the highest average property damage per event. Tornadoes come in a distance second, followed by flash floods 
