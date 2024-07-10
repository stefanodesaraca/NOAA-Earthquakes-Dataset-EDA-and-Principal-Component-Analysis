require(readr)
require(tidyverse)
require(dplyr)
require(webr)
require(plotly)
require(FactoMineR)
require(FactoInvestigate)
require(Factoshiny)
require(factoextra)
require(corrplot)



earthquakes <- read.csv("earthquakes.csv", sep="\t")
earthquakes <- earthquakes[-1,-1]


#NOTES:
# 1. Tsunami: it contains the intensity of the tsunami event using the Soloviev-Go Intensity Scale
# 2. Volcano: it may contain various data, but the most probable is the VEI Index


names(earthquakes) <- c("year", "hour", "minute", "second", "month", "day", "tsunami", "volcano", "location", "latitude", "longitude", "focalDepth", "magnitude", "MMIntensity", "deaths", "deathDescription", "missing", "missingDescription", "injuries", "injuriesDescription", "damage", "damageDescription", "housesDestroyed", "housesDestroyedDescription", "housesDamaged", "housesDamagedDescription", "totalDeaths", "totalDeathDescription", "totalInjuries", "totalInjuriesDescription", "totalDamage", "totalDamageDescription", "totalHousesDestroyedDescription", "totalHousesDamaged", "totalHousesDamagedDescription")


earthquakes$Latitude <- as.character(earthquakes$latitude)
earthquakes$Longitude <- as.character(earthquakes$longitude)

earthquakes$Year <- as.character(earthquakes$year)
earthquakes$hour <- as.character(earthquakes$hour)
earthquakes$minute <- as.character(earthquakes$minute)
earthquakes$second <- as.character(earthquakes$second)
earthquakes$month <- as.character(earthquakes$month)
earthquakes$day <- as.character(earthquakes$day)


earthquakes <- separate(earthquakes, "location", into = c("region", "locName"), sep = ":")


earthquakes <- data.frame(earthquakes)





# ---------------------------------------------- ANALYSIS ----------------------------------------------


summary(earthquakes)

var(earthquakes$magnitude, na.rm = T)
sd(earthquakes$magnitude, na.rm = T)


max(earthquakes$magnitude, na.rm = T)
subset(earthquakes, earthquakes$magnitude == max(earthquakes$magnitude, na.rm = T)) #Maximum earthquake intensity gotten from the max() function above


min(earthquakes$magnitude, na.rm = T)
subset(earthquakes, earthquakes$magnitude == min(earthquakes$magnitude, na.rm = T)) #Minimum earthquake intensity gotten from the max() function above




earthquakesPCA <- PCA(select_if(earthquakes, is.numeric))
earthquakesPCA$var$contrib
earthquakesPCA$var$cos2


fviz_eig(earthquakesPCA, col.var="blue") 

var <- get_pca_var(earthquakesPCA)
var

corrplot(var$cos2, is.corr=FALSE)

fviz_pca_var(earthquakesPCA,
             col.var = "cos2", 
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)

fviz_pca_var(earthquakesPCA,
             col.var = "contrib", 
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)



MMIDepth = na.omit(earthquakes[, c("MMIntensity", "focalDepth")])
cor(MMIDepth)
cor.test(MMIDepth$MMIntensity, MMIDepth$focalDepth)



MagnitudeHousesDestroyed = na.omit(earthquakes[, c("magnitude", "housesDestroyed")])
cor(MagnitudeHousesDestroyed)
cor.test(MagnitudeHousesDestroyed$magnitude, MagnitudeHousesDestroyed$housesDestroyed)



InjuriesHousesDestroyed = na.omit(earthquakes[, c("injuries", "housesDestroyed")])
cor(InjuriesHousesDestroyed)
cor.test(InjuriesHousesDestroyed$injuries, InjuriesHousesDestroyed$housesDestroyed)
#Strong correlation between injuries and houses destroyed





#Earthquakes World Map Plot#Earthquakes World Map PlotTRUE
fig <- earthquakes[, c("latitude", "longitude", "magnitude")]
fig <- fig %>%
  plot_ly(
    lat = ~latitude,
    lon = ~longitude,
    marker = list(color = "red"),
    type = 'scattermapbox',
    hovertext = ~magnitude) 
fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -88, lat = 34))) 

fig


#Selecting the first 10 countries by average deaths caused by earthquakes
avgDeathsByRegion = earthquakes[, c("deaths", "region")] %>% group_by(region) %>%
  summarise(avgDeaths = median(deaths, na.rm=T)) %>% 
  arrange(desc(avgDeaths)) %>%
  top_n(10, avgDeaths)

na.omit(avgDeathsByRegion)


PieDonut(avgDeathsByRegion, aes(avgDeaths, region), title = "First 10 regions by average deaths caused by earthquakes", showRatioPie = F, showRatioDonut = F)















