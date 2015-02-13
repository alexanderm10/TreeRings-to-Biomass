# Mapping Spatial Points, Basic

library(maps)
library(maptools)

###############
# Read in data table
dave.sites <- read.csv("sites_dave.csv")

evergreen <- dave.sites[dave.sites$type == "Evergreen Needle-leaf Forest",]
deciduous <- dave.sites[dave.sites$type == "Deciduous Broad-leaf Forest",]
mixed <- dave.sites[dave.sites$type == "Mixed Forest",]

done <- dave.sites[dave.sites$done == 1,]
neil.amy <- dave.sites[dave.sites$neil.amy == 1,]

# making a spatial points data frame, guessing on coordinate system
dave.sites <- SpatialPointsDataFrame(coords=dave.sites[,c("long", "lat")], dave.sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
evergreen <- SpatialPointsDataFrame(coords=evergreen[,c("long", "lat")], evergreen, proj4string=CRS("+proj=longlat +datum=WGS84"))
deciduous <- SpatialPointsDataFrame(coords=deciduous[,c("long", "lat")], deciduous, proj4string=CRS("+proj=longlat +datum=WGS84"))
mixed <- SpatialPointsDataFrame(coords=mixed[,c("long", "lat")], mixed, proj4string=CRS("+proj=longlat +datum=WGS84"))
done <- SpatialPointsDataFrame(coords=done[,c("long", "lat")], done, proj4string=CRS("+proj=longlat +datum=WGS84"))
neil.amy <- SpatialPointsDataFrame(coords=neil.amy[,c("long", "lat")], neil.amy, proj4string=CRS("+proj=longlat +datum=WGS84"))


map("state", plot=T, lty="solid", col="gray50", lwd=2)
legend("bottomleft", legend=c("Active Sites", "Summer 2014", "Sampled Sites", "Summer 2015"), pch=19, col=c("red", "blue", "green", "orange"), cex+.5)


map("state", plot=T, lty="solid", col="gray50", lwd=2)
#plot(dave.sites, pch=19, add=T, cex=1)
plot(evergreen, pch=15, add=T, cex=1, col = "dark green")
plot(deciduous, pch=19, add=T, cex=1, col = "green")
plot(mixed, pch=18, add=T, cex=1, col = "blue")
plot(done, pch=2, add=T, cex=2, lwd=2, col = "red")
plot(neil.amy, pch=1, add=T, cex=2, lwd=4, col= "deeppink")

legend("bottomleft", legend=c("Evergreen", "Deciduous", "Mixed", "Sampled", "Neil/Amy"), pch=c(15,19,18,2,1), col=c("dark green", "green", "blue", "red", "deeppink"), cex+.5)