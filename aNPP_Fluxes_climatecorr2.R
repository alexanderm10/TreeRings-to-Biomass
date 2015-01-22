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
niwot.nep.m <- ts(read.table("monthly_nep_niwot.txt", header=T), end = 2013, frequency =1)

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


# Reset the working directory to the current folder
setwd("~/PhD/Conferences/2015/NACP15/NACP_gapfilling")

clim.morgan
clim.niwot
###########################################
### correlation between climate and tree rings
###########################################

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
    periodM[[i]] <- matrix(data = NA, ncol = 34, nrow = (ncol(mmf.chrono))); colnames(periodM[[i]]) <- months; rownames(periodM[[i]]) <- colnames(mmf.chrono)
  }
  
  for(i in 1:length(periodN)){
    for(a in 1:ncol(niwot.b.chrono)){
      periodN[[i]][a,] <- as.numeric(fancymonthlycorrels(niwot.b.chrono[,a],clim.niwot[[i]],first[x],last[x]))
    }
  }
  
  for(i in 1:length(periodM)){
    for(a in 1:ncol(mmf.chrono)){
      periodM[[i]][(a),] <- as.numeric(fancymonthlycorrels(mmf.chrono[,a],clim.morgan[[i]],first[x],last[x]))
    }
  }
  
  
  clim.cor.niwot.tr[[x]] <- periodN
  clim.cor.morgan.tr[[x]] <- periodM
  
}




###########################################
# Correlate climate with fluxes
# going to need to generate a matrix to correlaate
###########################################

niwot.climate.fluxes <- ts.union(niwot.nep.m, clim.niwot)

###########################################
# Using Flurns script to correlated climate and fluxes together
###########################################

months <- c("pJan", "pFeb","pMar","pApr","pMay","pJun","pJul","pAug","pSep","pOct","pNov","pDec","Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","pYear","pJJA","pAMJJAS","MAM","MJ","AMJJAS","MJJ","JJA","JJ","JA")

first <- c(1998)
last <- c(2010)

clim.cor.niwot.flux <- vector(mode = "list", length = length(first))
clim.cor.morgan.flux <- vector(mode = "list", length = length(first))
names(clim.cor.niwot.flux) <- c("1998-2010")
names(clim.cor.morgan.flux) <- names(clim.cor.niwot.flux)

for(x in 1:length(clim.cor.niwot.flux)){
  
  periodN <- vector(mode = "list", length = 4); names(periodN) <- c("temp", "precip", "radiation","airhumidity")
  periodM<- vector(mode = "list", length = 4); names(periodM) <- c("temp", "precip", "radiation","airhumidity")
  
  
  for(i in 1:length(periodN)){
    periodN[[i]] <- matrix(data = NA, ncol = 34, nrow = ncol(niwot.nep.m)); colnames(periodN[[i]]) <- months; rownames(periodN[[i]]) <- colnames(niwot.nep.m)
    periodM[[i]] <- matrix(data = NA, ncol = 34, nrow = (ncol(mmf.nep.m))); colnames(periodM[[i]]) <- months; rownames(periodM[[i]]) <- colnames(mmf.nep.m)
  }
  
  for(i in 1:length(periodN)){
    for(a in 1:ncol(niwot.nep.m)){
      periodN[[i]][a,] <- as.numeric(fancymonthlycorrels(niwot.nep.m[,a],clim.niwot[[i]],first[x],last[x]))
    }
  }
  
  for(i in 1:length(periodM)){
    for(a in 1:ncol(mmf.nep.m)){
      periodM[[i]][(a-1),] <- as.numeric(fancymonthlycorrels(mmf.nep.m[,a],clim.morgan[[i]],first[x],last[x]))
    }
  }
  
  
  clim.cor.niwot.flux[[x]] <- periodN
  clim.cor.morgan.flux[[x]] <- periodM
  
}





###########################################
#correlating the tree rings to the fluxes
###########################################


# Flurins fancy script
months <- c("pJan", "pFeb","pMar","pApr","pMay","pJun","pJul","pAug","pSep","pOct","pNov","pDec","Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","pYear","pJJA","pAMJJAS","MAM","MJ","AMJJAS","MJJ","JJA","JJ","JA")

first <- c(1998)
last <- c(2012)

flux.cor.niwot.tr <- vector(mode = "list", length = length(first))
flux.cor.morgan.tr <- vector(mode = "list", length = length(first))
names(flux.cor.niwot.tr) <- c("1998-2012")
names(flux.cor.morgan.tr) <- names(flux.cor.niwot.tr)

for(x in 1:length(flux.cor.niwot.tr)){
  
  #   periodN <- vector(mode = "list", length = 1); names(periodN) <- c("NEP")
  #   periodM<- vector(mode = "list", length = 1); names(periodM) <- c("NEP")
  
  
  #   for(i in 1:length(periodN)){
  #     periodN[[i]] <- matrix(data = NA, ncol = 34, nrow = ncol(niwot.b.chrono)); colnames(periodN[[i]]) <- months; rownames(periodN[[i]]) <- colnames(niwot.b.chrono)
  #     periodM[[i]] <- matrix(data = NA, ncol = 34, nrow = (ncol(mmf.chrono))); colnames(periodM[[i]]) <- months; rownames(periodM[[i]]) <- colnames(mmf.chrono)
  #   }
  #   
  #   for(i in 1:length(periodN)){
  periodN <- matrix(data = NA, ncol = 34, nrow = ncol(niwot.b.chrono)); colnames(periodN) <- months; rownames(periodN) <- colnames(niwot.b.chrono)
  periodM <- matrix(data = NA, ncol = 34, nrow = (ncol(mmf.chrono))); colnames(periodM) <- months; rownames(periodM) <- colnames(mmf.chrono)
  #  }
  
  
  #   for(i in 1:length(periodN)){
  #     for(a in 1:ncol(niwot.b.chrono)){
  #       periodN[[i]][a,] <- as.numeric(fancymonthlycorrels(niwot.b.chrono[,a],niwot.nep.m[[i]],first[x],last[x]))
  #     }
  #   }
  #   
  
  #for(i in 1:length(periodN)){
  for(a in 1:ncol(niwot.b.chrono)){
    periodN[a,] <- as.numeric(fancymonthlycorrels(niwot.b.chrono[,a],niwot.nep.m,first[x],last[x]))
  }
  #  }
  
  #   
  #   for(i in 1:length(periodM)){
  for(a in 1:ncol(mmf.chrono)){
    periodM[(a),] <- as.numeric(fancymonthlycorrels(mmf.chrono[,a],mmf.nep.m,first[x],last[x]))
  }
  #   }
  
  
  flux.cor.niwot.tr[[x]] <- periodN
  flux.cor.morgan.tr[[x]] <- periodM
  
}
  
##################################################
# Correlating the Climate with the Climate
##################################################
#niwot
niwot.cor.t.vs.rad <- ts.cor(clim.niwot[[1]], clim.niwot[[3]])
summary(niwot.cor.t.vs.rad)

summary(niwot.cor.t.vs.rad)
barplot(niwot.cor.t.vs.rad)

par(mar=c(5,4.5,4,7))
image(niwot.cor.t.vs.rad, zlim=c(-1,1), axes=F, col=tim.colors(64), main = "Niwot Ridge Temp vs. SW Rad")
axis(1, at = seq(0,1, length.out=12), labels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
axis(2, at = seq(0,1, length.out=12), labels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las=2)
image.plot(niwot.cor.t.vs.rad, legend.only=T, zlim=c(-1,1))

#MMF

morgan.cor.t.vs.rad <- ts.cor(clim.morgan[[1]], clim.morgan[[3]])

summary(morgan.cor.t.vs.rad)
barplot(morgan.cor.t.vs.rad)
par(mar=c(5,4.5,4,7))
image(morgan.cor.t.vs.rad, zlim=c(-1,1), axes=F, col=tim.colors(64), main= "MMF Temp vs. SW Rad")
axis(1, at = seq(0,1, length.out=12), labels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
axis(2, at = seq(0,1, length.out=12), labels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las=2)
image.plot(morgan.cor.t.vs.rad, legend.only=T, zlim=c(-1,1))

# months <- c("pJan", "pFeb","pMar","pApr","pMay","pJun","pJul","pAug","pSep","pOct","pNov","pDec","Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","pYear","pJJA","pAMJJAS","MAM","MJ","AMJJAS","MJJ","JJA","JJ","JA")
# 
# first <- c(1901,1901,1956,1901,1910,1920,1930,1940,1950,1960,1970,1980)
# last <- c(2010,1955,2010,1930,1940,1950,1960,1970,1980,1990,2000,2010)
# 
# clim.cor.niwot.clim <- vector(mode = "list", length = length(first))
# clim.cor.morgan.clim <- vector(mode = "list", length = length(first))
# names(clim.cor.niwot.clim) <- c("1901-2010","1901-1955","1956-2010","1901-1930","1910-1940", "1920-1950", "1930-1960", "1940-1970", "1950-1980", "1960-1990", "1970-2000", "1980-2010")
# names(clim.cor.morgan.clim) <- names(clim.cor.niwot.clim)
# 
# for(x in 1:length(clim.cor.niwot.tr)){
#   
#   periodN <- vector(mode = "list", length = 4); names(periodN) <- c("temp", "precip", "radiation","airhumidity")
#   periodM<- vector(mode = "list", length = 4); names(periodM) <- c("temp", "precip", "radiation","airhumidity")
#   
#   
#   for(i in 1:length(periodN)){
#     periodN[[i]] <- matrix(data = NA, ncol = 34, nrow = ncol(clim.niwot)); colnames(periodN[[i]]) <- months; rownames(periodN[[i]]) <- colnames(clim.niwot)
#     periodM[[i]] <- matrix(data = NA, ncol = 34, nrow = (ncol(clim.morgan))); colnames(periodM[[i]]) <- months; rownames(periodM[[i]]) <- colnames(clim.niwot)
#   }
#   
#   for(i in 1:length(periodN)){
#     for(a in 1:ncol(clim.niwot)){
#       periodN[[i]][a,] <- as.numeric(fancymonthlycorrels(clim.niwot[,a],clim.niwot[[i]],first[x],last[x]))
#     }
#   }
#   
#   for(i in 1:length(periodM)){
#     for(a in 1:ncol(mmf.chrono)){
#       periodM[[i]][(a),] <- as.numeric(fancymonthlycorrels(clim.niwot[,a],clim.morgan[[i]],first[x],last[x]))
#     }
#   }
#   
#   
#   clim.cor.niwot.tr[[x]] <- periodN
#   clim.cor.morgan.tr[[x]] <- periodM
#   
# }






#Creating rDatafile for Flurin

save(clim.cor.morgan.flux, file= "climate_vs_flux_mmf.RData")
save(clim.cor.niwot.flux, file= "climate_vs_flux_niwot.RData")

save(clim.cor.morgan.tr, file= "climate_vs_tr_mmf.RData")
save(clim.cor.niwot.tr, file= "climate_vs_tr_niwot.RData")

save(flux.cor.morgan.tr, file= "flux_vs_tr_mmf.RData")
save(flux.cor.niwot.tr, file= "flux_vs_tr_niwot.RData")
