library(ncdf)
library(timeSeries)
library(Hmisc)
# NOTE: Run site level detrending script and load objects into the workspace

#load any additional chronologies (In this case Flurin's Niwot Chronologies)
niwot.b.chrono <- ts(read.table("plotB_detrended_chronos.txt", header=T), end = 2012, frequency = 1)
niwot.c.chrono <- ts(read.table("plotC_detrended_chronos.txt", header=T), end = 2012, frequency = 1)


#mmf chronologies consolodated

mmf.chrono <- ts(read.csv("chron_all_mmfspp.csv"), end=2014, frequency =1)

# load Flux data (for now both Niwot and MMF are monthly)
#The niwot flux record spans 1998-2013
niwot.nep.m <- ts(read.table("monthly_nep_niwot.txt", header=T), end = 20013, frequency =1)

#The Morgan Monroe flux record spans 1999-2005
mmf.nep.m <- ts(read.table("monthly_nep_morgan.txt", header=T), end = 2005, frequency = 1)

# this code comes from Flurin's climate_response.r file

###load climate data
months <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
clim.niwot <- vector(mode = "list", length = 4); names(clim.niwot) <- c("temp", "precip", "radiation","airhumidity")
clim.morgan <- vector(mode = "list", length = 4); names(clim.morgan) <- c("temp", "precip", "radiation","airhumidity")

for(i in 1:length(clim.niwot)){
  clim.niwot[[i]] <- ts(matrix(data = NA, ncol = 12, nrow = 110), start = 1901, frequency = 1); colnames(clim.niwot[[i]]) <- months
  clim.morgan[[i]] <- ts(matrix(data = NA, ncol = 12, nrow = 110), start = 1901, frequency = 1); colnames(clim.morgan[[i]]) <- months
}

nNiwot <- 100; ewNiwot <- 149; nMorgan <- 102; ewMorgan <- 188  #grid cells

#NOTE: need to get the CRU-nCEP data from Flurin so that I can draw off of the source code.
#Note: This will be useful for later


for(i in 1:110){
  
  
  #setwd("C:/Users/babst/Desktop/data/data_climate/cruncep_data/tair")
  setwd("~/PhD/CRUNCEP_monthly/tair")
  clim.niwot[[1]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewNiwot,nNiwot,1), count = c(1,1,12))-272.15   #transform to Celsius
  clim.morgan[[1]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewMorgan,nMorgan,1), count = c(1,1,12))-272.15
  
  
  #setwd("C:/Users/babst/Desktop/data/data_climate/cruncep_data/rain")
  setwd("~/PhD/CRUNCEP_monthly/rain")
  clim.niwot[[2]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewNiwot,nNiwot,1), count = c(1,1,12))
  clim.morgan[[2]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewMorgan,nMorgan,1), count = c(1,1,12))
  
  #setwd("C:/Users/babst/Desktop/data/data_climate/cruncep_data/swdown_total")
  setwd("~/PhD/CRUNCEP_monthly/swdown_total")
  clim.niwot[[3]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewNiwot,nNiwot,1), count = c(1,1,12))
  clim.morgan[[3]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewMorgan,nMorgan,1), count = c(1,1,12))
  
  #setwd("C:/Users/babst/Desktop/data/data_climate/cruncep_data/qair")
  setwd("~/PhD/CRUNCEP_monthly/qair")
  clim.niwot[[4]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewNiwot,nNiwot,1), count = c(1,1,12))
  clim.morgan[[4]][i,] <- get.var.ncdf(open.ncdf(list.files()[i]), start = c(ewMorgan,nMorgan,1), count = c(1,1,12))
  
}
setwd("~/PhD/Conferences/2015/NACP15/NACP_gapfilling")

clim.morgan

write.csv(clim.morgan, "mmf.CRUNCEP.climate.csv")

clim.morgan <- read.csv

### correlation per species in tree rings

months <- c("pJan", "pFeb","pMar","pApr","pMay","pJun","pJul","pAug","pSep","pOct","pNov","pDec","Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","pYear","pJJA","pAMJJAS","MAM","MJ","AMJJAS","MJJ","JJA","JJ","JA")

first <- c(1901,1901,1956,1901,1910,1920,1930,1940,1950,1960,1970,1980)
last <- c(2010,1955,2010,1930,1940,1950,1960,1970,1980,1990,2000,2010)

clim.cor.niwot.tr <- vector(mode = "list", length = length(first))
clim.cor.morgan.tr <- vector(mode = "list", length = length(first))
names(clim.cor.niwot.tr) <- c("1901-2010","1901-1955","1956-2010","1901-1930","1910-1940", "1920-1950", "1930-1960", "1940-1970", "1950-1980", "1960-1990", "1970-2000", "1980-2010")
names(clim.cor.morgan.tr) <- names(clim.cor.niwot.tr)

for(x in 1:length(clim.cor.niwot.tr)){
  
  periodN <- vector(mode = "list", length = 4); names(periodN) <- c("temp", "precip", "radiation","airhumidity")
  periodM<- vector(mode = "list", length = 4); names(periodM) <- c("temp", "precip", "radiation","airhumidity")
  
  
  for(i in 1:length(periodN)){
    periodN[[i]] <- matrix(data = NA, ncol = 34, nrow = ncol(niwot.b.chrono)); colnames(periodN[[i]]) <- months; rownames(periodN[[i]]) <- colnames(niwot.b.chrono)
    periodM[[i]] <- matrix(data = NA, ncol = 34, nrow = (ncol(mmf.chrono)-1)); colnames(periodM[[i]]) <- months; rownames(periodM[[i]]) <- colnames(mmf.chrono[,2:ncol(mmf.chrono)])
  }
  
  for(i in 1:length(periodN)){
    for(a in 1:ncol(niwot.b.chrono)){
      periodN[[i]][a,] <- as.numeric(fancymonthlycorrels(niwot.b.chrono[,a],clim.niwot[[i]],first[x],last[x]))
    }
  }
  
  for(i in 1:length(periodM)){
    for(a in 2:ncol(mmf.chrono)){
      periodM[[i]][(a-1),] <- as.numeric(fancymonthlycorrels(mmf.chrono[,a],clim.morgan[[i]],first[x],last[x]))
    }
  }
  
  
  clim.cor.niwot.tr[[x]] <- periodN
  clim.cor.morgan.tr[[x]] <- periodM
  
}



#plotting graphs for Tree RIng climate response

#Niwot

par(mfrow = c(2,2), las=2)
barplot(clim.cor.niwot.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "niwot temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[3]][[2]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "niwot precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[3]][[3]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "niwot SW radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "niwot abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")

par(mfrow = c(2,2), las=2)
barplot(clim.cor.niwot.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "niwot temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[2]][[2]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "niwot precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[2]][[3]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "niwot SW radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "niwot abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")


#Morgan

par(mfrow = c(2,2), las=2)
barplot(clim.cor.morgan.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "mmf temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed") # 0.235 is the significance threshhold for 50 years of data
barplot(clim.cor.morgan.tr[[3]][[2]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "mmf precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[3]][[3]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "mmf SW radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "mmf abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")

par(mfrow = c(2,2), las=2)
barplot(clim.cor.morgan.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "mmf temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[2]][[2]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "mmf precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[2]][[3]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "mmf radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "mmf abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")

# Correlate climate with fluxes
#need to aggregate the fluxes up to the annual level

#had to make my own correlation tables because I couldnt' get Dave Frank's to work.
mmf.nep.m
mmf.nep.a <- as.data.frame(rowMeans(mmf.nep.m))
names(mmf.nep.a) <- "nep"
mmf.nep.a$year<-row.names(mmf.nep.a)



clim.morgan2<- as.data.frame(clim.morgan)
clim.morgan2$year <- 1901:2010
head(clim.morgan2)

clim.morgan3 <- clim.morgan2[clim.morgan2$year %in% 1999:2005,]
summary(clim.morgan3)

mmf.cor.flux <- rcorr(as.matrix(mmf.nep.a), as.matrix(clim.morgan3), type='pearson')


write.csv(mmf.cor.flux$r, "mmf_flux-climate_corr.csv")
write.csv(mmf.cor.flux$P, "mmf_flux_climate_pvalues.csv")

niwot.nep.m
niwot.nep.a <- as.data.frame(rowMeans(niwot.nep.m))
names(niwot.nep.a) <- "nep"
niwot.nep.a$year <-row.names(niwot.nep.a)
summary(clim.niwot)

clim.niwot2<- as.data.frame(clim.niwot)
clim.niwot2$year <-1901:2010
summary(clim.niwot2)

clim.niwot3 <- clim.niwot2[clim.niwot2$year %in% 1998:2010,]
niwot.cor.flux <- rcorr(as.matrix(niwot.nep.a[niwot.nep.a$year %in% 1998:2010,]), as.matrix(clim.niwot3), type='pearson')

write.csv(niwot.cor.flux$r, "niwot_flux-climate_corr.csv")
write.csv(niwot.cor.flux$P, "niwot_flux_climate_pvalues.csv")

# nifty plotting script
par(mfrow = c(2,2), las=2)
# Niwot
# Temperature
barplot(niwot.cor.flux$r[3:14,"nep"], col= ifelse(niwot.cor.flux$P[3:14,"nep"] < 0.05, "blue", "white"), ylim=c(-1,1), main="Niwot Temp:flux Pearson")

# precipitation
barplot(niwot.cor.flux$r[15:26,"nep"], col= ifelse(mmf.cor.flux$P[15:26,"nep"] < 0.05, "blue", "white"), ylim=c(-1,1), main="Niwot Precip:flux Pearson")

# SW Radiation
barplot(niwot.cor.flux$r[27:38,"nep"], col= ifelse(niwot.cor.flux$P[27:38,"nep"] < 0.05, "blue", "white"), ylim=c(-1,1), main="Niwot SW Rad:flux Pearson")

# Humidity
barplot(niwot.cor.flux$r[39:50,"nep"], col= ifelse(niwot.cor.flux$P[39:50,"nep"] < 0.05, "blue", "white"), ylim=c(-1,1), main="Niwot Humidity:flux Pearson")

# MMF
# Temperature
barplot(mmf.cor.flux$r[3:14,"nep"], col= ifelse(mmf.cor.flux$P[3:14,"nep"] < 0.05, "blue", "white"), ylim=c(-1,1), main="MMF Temp:flux Pearson")

# precipitation
barplot(mmf.cor.flux$r[15:26,"nep"], col= ifelse(mmf.cor.flux$P[15:26,"nep"] < 0.05, "blue", "white"),  ylim=c(-1,1),main="MMF Precip:flux Pearson")

# SW Radiation
barplot(mmf.cor.flux$r[27:38,"nep"], col= ifelse(mmf.cor.flux$P[27:38,"nep"] < 0.05, "blue", "white"),  ylim=c(-1,1),main="MMF SW Rad:flux Pearson")

# Humidity
barplot(mmf.cor.flux$r[39:50,"nep"], col= ifelse(mmf.cor.flux$P[39:50,] < 0.05, "blue", "white"),  ylim=c(-1,1),main="MMF Humidity:flux Pearson")


#correlating the tree rings to the tower record
niwot.b.chrono2 <- read.csv("plotB_detrended_chronos.csv", header=T)

summary(niwot.b.chrono2)

mmf.chrono2 <- read.csv("chron_all_mmfspp_with_year_column.csv", header=T)

summary(mmf.chrono2)
#niwot

niwot.nep.m

niwot.nep.m <- as.data.frame(niwot.nep.m)
summary(niwot.nep.m)

niwot.b.chrono2
clim.niwot2[clim.niwot2$year %in% 1998:2010,]

niwot.flux.tr.corr <- rcorr(as.matrix(niwot.b.chrono2[niwot.b.chrono2$year %in% 1998:2012,]), as.matrix(niwot.nep.m[niwot.nep.m$year %in% 1998:2012,]), type="pearson")



# Flurins fancy script
months <- c("pJan", "pFeb","pMar","pApr","pMay","pJun","pJul","pAug","pSep","pOct","pNov","pDec","Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","pYear","pJJA","pAMJJAS","MAM","MJ","AMJJAS","MJJ","JJA","JJ","JA")

first <- c(1998)
last <- c(2012)

clim.cor.niwot.flux <- vector(mode = "list", length = length(first))
clim.cor.morgan.flux <- vector(mode = "list", length = length(first))
names(clim.cor.niwot.flux) <- c("1901-2010","1901-1955","1956-2010","1901-1930","1910-1940", "1920-1950", "1930-1960", "1940-1970", "1950-1980", "1960-1990", "1970-2000", "1980-2010")
names(clim.cor.morgan.flux) <- names(clim.cor.niwot.flux)

for(x in 1:length(clim.cor.morgan.flux)){
  
  periodN <- vector(mode = "list", length = 4); names(periodN) <- c("temp", "precip", "radiation","airhumidity")
  periodM<- vector(mode = "list", length = 4); names(periodM) <- c("temp", "precip", "radiation","airhumidity")
  
  
  for(i in 1:length(periodN)){
    periodN[[i]] <- matrix(data = NA, ncol = 34, nrow = ncol(niwot.nep.m)); colnames(periodN[[i]]) <- months; rownames(periodN[[i]]) <- colnames(niwot.nep.m)
    periodM[[i]] <- matrix(data = NA, ncol = 34, nrow = (ncol(mmf.nep.m)-1)); colnames(periodM[[i]]) <- months; rownames(periodM[[i]]) <- colnames(mmf.nep.m[,2:ncol(mmf.nep.m)])
  }
  
  for(i in 1:length(periodN)){
    for(a in 1:ncol(niwot.nep.m)){
      periodN[[i]][a,] <- as.numeric(fancymonthlycorrels(niwot.nep.m[,a],clim.niwot[[i]],first[x],last[x]))
    }
  }
  
  for(i in 1:length(periodM)){
    for(a in 2:ncol(mmf.nep.m)){
      periodM[[i]][(a-1),] <- as.numeric(fancymonthlycorrels(mmf.nep.m[,a],clim.morgan[[i]],first[x],last[x]))
    }
  }
  
  
  clim.cor.morgan.flux[[x]] <- periodN
  clim.cor.morgan.flux[[x]] <- periodM
  
}


#plotting graphs for Tree RIng climate response

#Niwot

par(mfrow = c(2,2))
barplot(clim.cor.niwot.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[3]][[2]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[3]][[3]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "SW radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")

par(mfrow = c(2,2))
barplot(clim.cor.niwot.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[2]][[2]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[2]][[3]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "SW radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.niwot.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")


#Morgan

par(mfrow = c(2,2))
barplot(clim.cor.morgan.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed") # 0.235 is the significance threshhold for 50 years of data
barplot(clim.cor.morgan.tr[[3]][[2]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[3]][[3]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "SW radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[3]][[1]][,1:24], beside = T, ylim = c(-0.6,0.6), main = "abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")

par(mfrow = c(2,2))
barplot(clim.cor.morgan.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "temperature")  #temp
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[2]][[2]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "precipitation")  #precip
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[2]][[3]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "SW radiation")  #rad
abline(h = c(-0.235,0.235), lty = "dashed")
barplot(clim.cor.morgan.tr[[2]][[1]][,25:34], beside = T, ylim = c(-0.6,0.6), main = "abs. humidity")  #hum
abline(h = c(-0.235,0.235), lty = "dashed")
