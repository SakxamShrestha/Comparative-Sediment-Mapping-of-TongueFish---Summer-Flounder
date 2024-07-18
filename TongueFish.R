# Install required packages
install.packages("ridigbio")
install.packages("sp")
install.packages("raster")

# Load necessary libraries
library(ridigbio)
library(sp)
library(sf)
library(ggplot2)
library(raster)

# Search for Tongue Fish records in iDigBio
tonguefish.idigbio <- idig_search_records(list("scientificname" = "Paralichthys dentatus"))
View(tonguefish.idigbio)

# Check the class of the data
class(tonguefish.idigbio)

# Filter out records with missing latitude
tonguefish.lat <- tonguefish.idigbio[!is.na(tonguefish.idigbio$geopoint.lat),]

# Filter out records with missing year data
tonguefish.has.year <- tonguefish.lat[!is.na(tonguefish.lat$`data.dwc:year`),]

# Filter records from the year 1970 to 2024
tonguefish.1970.2024 <- tonguefish.has.year[tonguefish.has.year$`data.dwc:year` >= 1970,]
View(tonguefish.1970.2024)

# Prepare data for mapping
tonguefish.map <- tonguefish.1970.2024
coordinates(tonguefish.map) = ~geopoint.lon + geopoint.lat
proj4string(tonguefish.map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
tonguefish.shape <- spTransform(tonguefish.map, CRS("+proj=longlat"))
raster::shapefile(tonguefish.shape, "tonguefishShapeFile.shp", overwrite = TRUE)

# Read shapefile and coastline data
tonguefish.shp <- read_sf("tonguefishShapeFile.shp")
coastline <- read_sf("us_medium_shoreline.shp")

# Set working directory to the location of the coastline shapefile
setwd("C:/Users/hp/OneDrive/Documents/us_medium_shoreline (1)-20240716T194158Z-001/us_medium_shoreline (1)")

# Plot map of Summer Flounder
ggplot() + 
  geom_sf(data = coastline[,1], col = "#999999") +
  geom_sf(aes(col = as.numeric(data.dwc.y)), data = tonguefish.shp) + 
  scale_y_continuous(position = 'right') +
  coord_sf(xlim = c(-78, -69), ylim = c(34, 43)) + 
  ggtitle("Summer Flounder Map")

# Plot latitude over time
plot(as.numeric(tonguefish.1970.2024$`data.dwc:year`), tonguefish.1970.2024$geopoint.lat, xlab = "Year", ylab = "Latitude", col = "orange")
tonguefish.reg <- lm(tonguefish.1970.2024$geopoint.lat ~ as.numeric(tonguefish.1970.2024$`data.dwc:year`))
abline(tonguefish.reg)
summary(tonguefish.reg)

# Read sediment data
sediment <- read.csv(file = "US9_EXT.csv")
View(sediment)

# Create a dataframe for combining fish and sediment data
tonguefish.all <- data.frame()

# Loop through each Summer Flounder record to match sediment data
for (i in 1:length(tonguefish.1970.2024$uuid)) {
  lat <- round(tonguefish.1970.2024$geopoint.lat[i], digits = 2)
  lon <- round(tonguefish.1970.2024$geopoint.lon[i], digits = 2)
  
  # Subset sediment dataframe to match latitude and longitude
  sed.sub.lat <- sediment[round(sediment$Latitude, digits = 2) == lat, ]
  sed.sub.lat.lon <- sed.sub.lat[round(sed.sub.lat$Longitude, digits = 2) == lon, ]
  sed.sub.final <- sed.sub.lat.lon[!sed.sub.lat.lon$Grainsze == -99, ]
  
  # Calculate maximum and minimum percentage of sand for that location
  grain.max <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA, max(sed.sub.final$Grainsze))
  grain.min <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA, min(sed.sub.final$Grainsze))
  
  # Store information in a new data table
  tonguefish.all[i, 1] <- tonguefish.1970.2024$catalognumber[i]
  tonguefish.all[i, 2] <- lat
  tonguefish.all[i, 3] <- lon
  tonguefish.all[i, 4] <- grain.max
  tonguefish.all[i, 5] <- grain.min
  tonguefish.all[i, 6] <- tonguefish.1970.2024$`data.dwc:year`[i]
}

# View combined data
View(tonguefish.all)

# Set column names for the combined data
colnames(tonguefish.all) <- c("Catalog Number", "Latitude", "Longitude", "Grain.Max", "Grain.Min", "Year")

# Filter out rows with missing grain size data
tonguefish.complete <- tonguefish.all[!is.na(tonguefish.all$Grain.Max),]
View(tonguefish.complete)

# Plot maximum grain size over time
plot(tonguefish.complete$Year, tonguefish.complete$Grain.Max, col = "orange", xlab = "Year", ylab = "Grain Size Max")
grain.max.reg <- lm(tonguefish.complete$Grain.Max ~ as.numeric(tonguefish.complete$Year))
abline(grain.max.reg)
summary(grain.max.reg)
