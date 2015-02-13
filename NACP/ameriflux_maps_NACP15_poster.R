# Mapping Spatial Points, Basic

library(maps)
library(maptools)

###############
# Read in data table
dave.sites <- read.csv("sites_dave.csv", header=T) 

evergreen <- dave.sites[dave.sites$type == "Evergreen Needle-leaf Forest",]
deciduous <- dave.sites[dave.sites$type == "Deciduous Broad-leaf Forest",]
mixed <- dave.sites[dave.sites$type == "Mixed Forest",]

mmf.map <- dave.sites[dave.sites$site.name == "Morgan Monroe State Forest",]
niwot.map <- dave.sites[dave.sites$site.name == "Niwot Ridge",]

# making a spatial points data frame, guessing on coordinate system
dave.sites <- SpatialPointsDataFrame(coords=dave.sites[,c("long", "lat")], dave.sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
evergreen <- SpatialPointsDataFrame(coords=evergreen[,c("long", "lat")], evergreen, proj4string=CRS("+proj=longlat +datum=WGS84"))
deciduous <- SpatialPointsDataFrame(coords=deciduous[,c("long", "lat")], deciduous, proj4string=CRS("+proj=longlat +datum=WGS84"))
mixed <- SpatialPointsDataFrame(coords=mixed[,c("long", "lat")], mixed, proj4string=CRS("+proj=longlat +datum=WGS84"))
mmf.map <- SpatialPointsDataFrame(coords=mmf.map[,c("long", "lat")], mmf.map, proj4string=CRS("+proj=longlat +datum=WGS84"))
niwot.map <- SpatialPointsDataFrame(coords=niwot.map[,c("long", "lat")], niwot.map, proj4string=CRS("+proj=longlat +datum=WGS84"))


map("state", plot=T, lty="solid", col="gray50", lwd=2)
legend("bottomleft", legend=c("Active Sites", "Summer 2014", "Sampled Sites", "Summer 2015"), pch=19, col=c("red", "blue", "green", "orange"), cex+.5)



pdf("NACP15_sitemap.pdf", height=8, width=11)

map("state", plot=T, lty="solid", col="gray50", lwd=2)
#plot(dave.sites, pch=19, add=T, cex=1)
plot(evergreen, pch=15, add=T, cex=2, col = "dark green")
plot(deciduous, pch=19, add=T, cex=2, col = "green")
plot(mixed, pch=18, add=T, cex=2, col = "blue")
plot(mmf.map, pch=15, add=T, cex=3, col = "red")
plot(niwot.map, pch=19, add=T, cex=3, col= "deeppink")

legend("bottomleft", legend=c("Evergreen", "Deciduous", "Mixed", "Morgan Monroe", "Niwot Ridge"), pch=c(15,19,18,15,19), col=c("dark green", "green", "blue", "red", "deeppink"), cex=2)

dev.off()

