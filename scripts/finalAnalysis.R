library(dplyr)

stormData <- read.csv("data/raw/repdata_data_StormData.csv")

## ====================
## data cleaning
## ====================

## get only the columns we need (the file goes down to 2.9 MB instead of 500+MB)
stormData <- subset(stormData, select = c(
  BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG,
  PROPDMGEXP, CROPDMG, CROPDMGEXP
))

## set event types and exponents to lower case
stormData$EVTYPE <- tolower(stormData$EVTYPE)
stormData$PROPDMGEXP <- tolower(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <- tolower(stormData$CROPDMGEXP)

## replace BGN_DATE with YEAR
stormData$YEAR <- format(as.Date(stormData$BGN_DATE, "%m/%d/%Y"), "%Y")
stormData <- subset(stormData, select = -BGN_DATE)

## reformat property and crop damage
## combine property and crop damage
getValueFromMantissaAndExponent <- function(mantissa, exponent) {
  if (exponent == "k") {
    return(mantissa * 1000)
  }
  if (exponent == "m") {
    return(mantissa * 1000000)
  }
  if (exponent == "b") {
    return(mantissa * 1000000000)
  }
  return(mantissa)
}

stormData$PROPDMGNEW <- mapply(
  getValueFromMantissaAndExponent,
  stormData$PROPDMG,
  stormData$PROPDMGEXP
)
stormData$CROPDMGNEW <- mapply(
  getValueFromMantissaAndExponent,
  stormData$CROPDMG,
  stormData$CROPDMGEXP
)

stormData$DMG <- stormData$PROPDMGNEW + stormData$CROPDMGNEW
stormData <- subset(stormData, select = -c(
  PROPDMG, PROPDMGEXP, PROPDMGNEW, CROPDMG, CROPDMGEXP, CROPDMGNEW))

## only look at data from 1996 onward (all event types examined from 1996 onward)
stormData <- subset(stormData, YEAR >= 1996) # reduces event types from 897 to 438

## remove all rows with day summary events (i.e. all summaries aside from "blizzard summary")
summaryEventNames <- stormData$EVTYPE[grepl("summary", stormData$EVTYPE)]
stormData <- subset(stormData, !grepl("summary", stormData$EVTYPE) |
                      grepl("^blizzard summary$", stormData$EVTYPE)) # reduces event types down to 372

## cleaning specific to fatality data
fatalityData <- stormData
fatalityData <- subset(fatalityData, FATALITIES > 0) # down to 102 event types

## cleaning specific to injury data
injuryData <- stormData
injuryData <- subset(injuryData, INJURIES > 0) # down to 100 event types

## cleaning specific to damage
damageData <- stormData
damageData <- subset(damageData, DMG > 0) # down to 155 event types

## buckets of events that will be counted together
tornadoEvents <- c("tornado")
flashFloodEvents <- c("flash flood", " flash flood")
extremeColdWindChillEvents <- c("extreme cold/wind chill", "extreme cold", "extreme windchill", "hypothermia/exposure")
tStormWindEvents <- c("thunderstorm wind", "tstm wind", "thunderstorm wind (g40)", "tstm wind (g40)", "tstm wind (g45)", "tstm wind 40", "tstm wind 45", "tstm wind (41)", " tstm wind (g45)", "tstm wind  (g45)", " tstm wind", "tstm wind g45")
lightningEvents <- c("lightning")
excessiveHeatEvents <- c("excessive heat", "hyperthermia/exposure", "record heat", "heat wave")
ripCurrentEvents <- c("rip currents", "rip current")
heavySnowEvents <- c("heavy snow", "snow squalls", "heavy snow shower")
blizzardEvents <- c("blizzard")
iceStormEvents <- c("ice storm")
highWindEvents <- c("high wind", "high winds", "high wind (g40)")
heavyRainEvents <- c("heavy rain", "torrential rainfall")
winterStormEvents <- c("winter storm")
floodEvents <- c("flood")
strongWindEvents <- c("strong winds", "strong wind")
hurricaneEvents <- c("hurricane", "hurricane/typhoon", "hurricane edouard", "typhoon")
highSurfEvents <- c("high surf", "heavy surf/high surf", "   high surf advisory")
coldWindChillEvents <- c("cold/wind chill", "extended cold", "cold", "cold temperature", "cold weather")
avalancheEvents <- c("avalanche")
tropicalStormEvents <- c("tropical storm")
hailEvents <- c("hail", "small hail")
coastalFloodEvents <- c("coastal flooding", "coastal flood", "coastal flooding/erosion")
heatEvents <- c("heat")
frostFreezeEvents <- c("frost/freeze", "freezing drizzle", "frost", "freeze", "damaging freeze", "early frost", "hard freeze", "agricultural freeze")
wildfireEvents <- c("wildfire", "wild/forest fire", "brush fire")
dustStormEvents <- c("dust storm")
stormSurgeTideEvents <- c("storm surge/tide", "storm surge")
waterspoutEvents <- c("waterspout")
denseFogEvents <- c("dense fog")
dustDevilEvents <- c("dust devil")
marineTStormWindEvents <- c("marine thunderstorm wind", "marine tstm wind")
marineStrongWindEvents <- c("marine strong wind")
marineHighWindEvents <- c("marine high wind")
tsunamiEvents <- c("tsunami")
funnelCloudEvents <- c("funnel cloud")
winterWeatherEvents <- c("winter weather mix")
droughtEvents <- c("drought")
lakeEffectSnowEvents <- c("lake effect snow")
seicheEvents <- c("seiche")
volcanicAshEvents <- c("volcanic ash")
tropicalDepressionEvents <- c("tropical depression")
lakeEffectSnowEvents <- c("lake-effect snow")
lakeshoreFloodEvents <- c("lakeshore flood")
astronomicalLowTideEvents <- c("astronomical low tide")
denseSmokeEvents <- c("dense smoke")
marineHailEvents <- c("marine hail")
freezingFogEvents <- c("freezing fog")

allEventsToKeep <- c(
  tornadoEvents,
  flashFloodEvents,
  extremeColdWindChillEvents,
  tStormWindEvents,
  lightningEvents,
  excessiveHeatEvents,
  ripCurrentEvents,
  heavySnowEvents,
  blizzardEvents,
  iceStormEvents,
  highWindEvents,
  heavyRainEvents,
  winterStormEvents,
  floodEvents,
  strongWindEvents,
  hurricaneEvents,
  highSurfEvents,
  coldWindChillEvents,
  avalancheEvents,
  tropicalStormEvents,
  hailEvents,
  coastalFloodEvents,
  heatEvents,
  frostFreezeEvents,
  wildfireEvents,
  dustStormEvents,
  stormSurgeTideEvents,
  waterspoutEvents,
  denseFogEvents,
  dustDevilEvents,
  marineTStormWindEvents,
  marineStrongWindEvents,
  marineHighWindEvents,
  tsunamiEvents,
  funnelCloudEvents,
  winterWeatherEvents,
  droughtEvents,
  lakeEffectSnowEvents,
  seicheEvents,
  volcanicAshEvents,
  tropicalDepressionEvents,
  lakeEffectSnowEvents,
  lakeshoreFloodEvents,
  astronomicalLowTideEvents,
  denseSmokeEvents,
  marineHailEvents,
  freezingFogEvents
)

eventBuckets <- list(
  tornadoEvents,
  flashFloodEvents,
  extremeColdWindChillEvents,
  tStormWindEvents,
  lightningEvents,
  excessiveHeatEvents,
  ripCurrentEvents,
  heavySnowEvents,
  blizzardEvents,
  iceStormEvents,
  highWindEvents,
  heavyRainEvents,
  winterStormEvents,
  floodEvents,
  strongWindEvents,
  hurricaneEvents,
  highSurfEvents,
  coldWindChillEvents,
  avalancheEvents,
  tropicalStormEvents,
  hailEvents,
  coastalFloodEvents,
  heatEvents,
  frostFreezeEvents,
  wildfireEvents,
  dustStormEvents,
  stormSurgeTideEvents,
  waterspoutEvents,
  denseFogEvents,
  dustDevilEvents,
  marineTStormWindEvents,
  marineStrongWindEvents,
  marineHighWindEvents,
  tsunamiEvents,
  funnelCloudEvents,
  winterWeatherEvents,
  droughtEvents,
  lakeEffectSnowEvents,
  seicheEvents,
  volcanicAshEvents,
  tropicalDepressionEvents,
  lakeEffectSnowEvents,
  lakeshoreFloodEvents,
  astronomicalLowTideEvents,
  denseSmokeEvents,
  marineHailEvents,
  freezingFogEvents
)

## remove events we're not using
fatalityData <- subset(fatalityData, EVTYPE %in% allEventsToKeep)
injuryData <- subset(injuryData, EVTYPE %in% allEventsToKeep)
damageData <- subset(damageData, EVTYPE %in% allEventsToKeep)

getNewEventLabels <- function(data) {
  return(mapply(
    function(eventType) {
      for (eventBucket in eventBuckets) {
        if (eventType %in% eventBucket) {
          return(eventBucket[[1]])
        }
      }
      return(eventType)
    },
    data$EVTYPE
  ))
}

fatalityData$EVTYPE <- getNewEventLabels(fatalityData)
injuryData$EVTYPE <- getNewEventLabels(injuryData)
damageData$EVTYPE <- getNewEventLabels(damageData)

## get yearly cumulative totals; events with top 20% yearly cumulative totals
fatalitiesByEvent <- fatalityData %>%
  group_by(EVTYPE) %>%
  summarize(CUMULATIVEFATALITIES = sum(FATALITIES))
fatalitiesByEvent <- subset(fatalitiesByEvent, 
                            fatalitiesByEvent$CUMULATIVEFATALITIES >= quantile(fatalitiesByEvent$CUMULATIVEFATALITIES, c(0.8))[[1]]
)
fatalitiesByEvent <- fatalitiesByEvent[order(fatalitiesByEvent$CUMULATIVEFATALITIES, decreasing = TRUE),]

injuriesByEvent <- injuryData %>%
  group_by(EVTYPE) %>%
  summarize(CUMULATIVEINJURIES = sum(INJURIES))
injuriesByEvent <- subset(injuriesByEvent, 
                          injuriesByEvent$CUMULATIVEINJURIES >= quantile(injuriesByEvent$CUMULATIVEINJURIES, c(0.8))[[1]]
)
injuriesByEvent <- injuriesByEvent[order(injuriesByEvent$CUMULATIVEINJURIES, decreasing = TRUE),]

damageByEvent <- damageData %>%
  group_by(EVTYPE) %>%
  summarize(CUMULATIVEDMG = sum(DMG))
damageByEvent <- subset(damageByEvent, 
                        damageByEvent$CUMULATIVEDMG >= quantile(damageByEvent$CUMULATIVEDMG, c(0.8))[[1]]
)
damageByEvent <- damageByEvent[order(damageByEvent$CUMULATIVEDMG, decreasing = TRUE),]

## graph
png("fatalitiesByEvent.png", width = 1200)
with(fatalitiesByEvent, barplot(
  CUMULATIVEFATALITIES,
  names.arg = EVTYPE,
  ylim = c(0, 2000),
  main = "Total Fatalities (Direct and Indirect) by Disaster, 1996 - 2011",
  width = 1200
))
dev.off()
png("injuriesByEvent.png", width = 1200)
with(injuriesByEvent, barplot(
  CUMULATIVEINJURIES,
  names.arg = EVTYPE,
  ylim = c(0, 25000),
  main = "Total Injuries (Direct and Indirect) by Disaster, 1996 - 2011",
  width = 1200
))
dev.off()
png("damageByEvent.png", width = 1200)
with(damageByEvent, barplot(
  CUMULATIVEDMG,
  names.arg = EVTYPE,
  ylim = c(0, 180000000000),
  main = "Total Property and Crop Damage ($) by Disaster, 1996 - 2011",
  width = 1200
))
dev.off()
