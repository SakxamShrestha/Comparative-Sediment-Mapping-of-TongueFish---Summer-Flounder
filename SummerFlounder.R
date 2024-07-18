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

# Search for Summer Flounder records in iDigBio
summerfish.idigbio <- idig_search_records(list("scientificname" = "Paralichthys dentatus"))
View(summerfish.idigbio)

# Check the class of the data
class(summerfish.idigbio)

# Filter out records with missing latitude
summerfish.lat <- summerfish.idigbio[!is.na(summerfish.idigbio$geopoint.lat),]

# Filter out records with missing year data
summerfish.has.year <- summerfish.lat[!is.na(summerfish.lat$`data.dwc:year`),]

# Filter records from the year 1970 to 2024
summerfish.1970.2024 <- summerfish.has.year[summerfish.has.year$`data.dwc:year` >= 1970,]
View(summerfish.1970.2024)

# Prepare data for mapping
summerfish.map <- summerfish.1970.2024
coordinates(summerfish.map) = ~geopoint.lon + geopoint.lat
proj4string(summerfish.map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
summerfish.shape <- spTransform(summerfish.map, CRS("+proj=longlat"))
raster::shapefile(summerfish.shape, "SummerfishShapeFile.shp", overwrite = TRUE)

# Read shapefile and coastline data
summerfish.shp <- read_sf("SummerfishShapeFile.shp")
coastline <- read_sf("us_medium_shoreline.shp")

# Set working directory to the location of the coastline shapefile
setwd("C:/Users/hp/OneDrive/Documents/us_medium_shoreline (1)-20240716T194158Z-001/us_medium_shoreline (1)")

# Plot map of Summer Flounder
ggplot() + 
  geom_sf(data = coastline[,1], col = "#999999") +
  geom_sf(aes(col = as.numeric(data.dwc.y)), data = summerfish.shp) + 
  scale_y_continuous(position = 'right') +
  coord_sf(xlim = c(-78, -69), ylim = c(34, 43)) + 
  ggtitle("Summer Flounder Map")

# Plot latitude over time
plot(as.numeric(summerfish.1970.2024$`data.dwc:year`), summerfish.1970.2024$geopoint.lat, xlab = "Year", ylab = "Latitude", col = "orange")
summerfish.reg <- lm(summerfish.1970.2024$geopoint.lat ~ as.numeric(summerfish.1970.2024$`data.dwc:year`))
abline(summerfish.reg)
summary(summerfish.reg)

# Read sediment data
sediment <- read.csv(file = "US9_EXT.csv")
View(sediment)

# Create a dataframe for combining fish and sediment data
summerfish.all <- data.frame()

# Loop through each Summer Flounder record to match sediment data
for (i in 1:length(summerfish.1970.2024$uuid)) {
  lat <- round(summerfish.1970.2024$geopoint.lat[i], digits = 2)
  lon <- round(summerfish.1970.2024$geopoint.lon[i], digits = 2)
  
  # Subset sediment dataframe to match latitude and longitude
  sed.sub.lat <- sediment[round(sediment$Latitude, digits = 2) == lat, ]
  sed.sub.lat.lon <- sed.sub.lat[round(sed.sub.lat$Longitude, digits = 2) == lon, ]
  sed.sub.final <- sed.sub.lat.lon[!sed.sub.lat.lon$Grainsze == -99, ]
  
  # Calculate maximum and minimum percentage of sand for that location
  grain.max <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA, max(sed.sub.final$Grainsze))
  grain.min <- ifelse(is.na(sed.sub.final$Grainsze[1]), NA, min(sed.sub.final$Grainsze))
  
  # Store information in a new data table
  summerfish.all[i, 1] <- summerfish.1970.2024$catalognumber[i]
  summerfish.all[i, 2] <- lat
  summerfish.all[i, 3] <- lon
  summerfish.all[i, 4] <- grain.max
  summerfish.all[i, 5] <- grain.min
  summerfish.all[i, 6] <- summerfish.1970.2024$`data.dwc:year`[i]
}

# View combined data
View(summerfish.all)

# Set column names for the combined data
colnames(summerfish.all) <- c("Catalog Number", "Latitude", "Longitude", "Grain.Max", "Grain.Min", "Year")

# Filter out rows with missing grain size data
summerfish.complete <- summerfish.all[!is.na(summerfish.all$Grain.Max),]
View(summerfish.complete)

# Plot maximum grain size over time
plot(summerfish.complete$Year, summerfish.complete$Grain.Max, col = "orange", xlab = "Year", ylab = "Grain Size Max")
grain.max.reg <- lm(summerfish.complete$Grain.Max ~ as.numeric(summerfish.complete$Year))
abline(grain.max.reg)
summary(grain.max.reg)
