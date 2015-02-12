##################################################################################
## Basic Components necessary for data management for doing dendro modelling
##################################################################################

# clear memory
rm(list=ls())

# importing libraries
library(dplR)
library(lattice)

# Getting Libraries
library(reshape)
library(car)
library(mgcv)
library(nlme)
library(lmeSplines)
#library(lme4)
library(splines)
library(MASS)
library(MuMIn)
library(ggplot2)
library(grid)
se <- function(x){
	sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))


#################################################################################################
# STEP 1: Gap-filling measured trees
# STEP 1b: Pith-correction in measured trees (useful for stand dynamics; won't do unless you ask for it)
# STEP 2: Gap-filling missing trees
#
# BIG SELLING POINT OF THIS APPROACH: we can quantify different levels of uncertainty & variability
# Caveats: fitting the initial GAMM is very time-intensive (it may take hours with your full data) because current form fits a spline to each tree in a mixed model framework

#################################################################################################
# Previous workflow
# 1) Read in RWL, QA/QC
# 3) Aggregate to tree (factoring in whether cores were dated) level & decide if an entire tree is dated or not -- WRITE THIS AS A FILE!
# 4) Stack RWL & Merge with metadata (becomes "ring.data" or named equivalent)


# Ring.data format: stack all of the core BAI, so that data frame with a SIGNLE BAI column, and then all of the factors in other columns
ring.data <- read.csv("TreeRWL_Valles_stacked.csv")
ring.data$tree <- as.factor(ring.data$tree) 
summary(ring.data)

tree.data <- read.csv("TreeData.csv")
summary(tree.data)

# We're going to run 2 sets of fillin models:
# 1) Model based on only DATED trees (m1d)
# 2) Model based on both DATED and UNDATED trees (m1u)

trees.dated <- ring.data[ring.data$Dated=="Y","TreeID"]

# using the gamm allows us to fit a smoothing spline to each tree, which allows us to basically gapfill taking into account unique tree temporal trends
#	current spline parameter: shrinkage version of cubic spline with 3 knots (stiff CR spline)
#	when we fit a generalized version for missing trees, we'll have to decide what to fit it to instead of TreeID; I think probably species|plot
# m1 <- gamm(RW ~ s(Year, bs="cs", k=3) + species + dbh, random=list(site=~1, PlotID=~1, TreeID=~1), data=ring.data, na.action=na.omit)

# ----------------------------------------------------------------
# IDEAL MODEL FORM (it won't work for many reasons)
#	-- won't predict outside range of years observed on each core
#	-- end up with singularity issues
#	-- would take FOREVER to fit even if it did work
# m1d <- gamm(RW ~ s(Year, bs="cs", k=3, by=TreeID) + species*dbh*canopy.class, random=list(site=~1, PlotID=~1, TreeID=~1), data=trees.dated.full, na.action=na.omit)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# The spline doesn't fit outside the range of observed values, so we need to give it a "null" guess
# As a very very rough guess right now, filling missing with the measurement from the oldest ring
# 1) Create rough size-age relationships to give a narrow window of rings to fill (i.e. don't fill a 10 cm oak back to 1900 if our rings stop in 1980)
# 2) fill the modeling window with non-0 values... perhaps mean growth from last decade or past observed trend?

ring.data$RW0 <- ring.data$RW
for(i in unique(ring.data$TreeID)){
	yr <- min(ring.data[ring.data$TreeID==i & !is.na(ring.data$RW), "Year"])
	ring.data[ring.data$TreeID==i & is.na(ring.data$RW), "RW0"] <- ring.data[ring.data$TreeID==i & ring.data$Year==yr, "RW"]
}
summary(ring.data)
# ----------------------------------------------------------------

# A generalized additive mixed model (gamm) allows us to fit splines in a mixed model context
# we can let these splines vary by tree which essentially detrends the core
# here's we're using our dummy-filled ring widths as a response so that the spline will fit over the whole time period of interest
# NOTE: for this to work with canopy class, we'll need to figure out what to do about dead trees 
m1 <- gamm(RW0 ~ s(Year, bs="cs", k=3, by=TreeID) + spp + dbh, random=list(site=~1, PlotID=~1), data=ring.data, na.action=na.omit)

# This isn't a great way of doing it, but it'll let you see the splines for each of the cores
par(mfrow=c(4,5), mar=c(2,2,0,0)+0.1)
plot(m1$gam)

ring.data$RW.modeled <- predict(m1, ring.data)
summary(ring.data)

# Getting rid of negative ring width because they're impossible --> replacing with 0 to help with later
ring.data$RW.modeled <- ifelse(ring.data$RW.modeled < 0, 0, ring.data$RW.modeled)
summary(ring.data)

# VERY rough graphing of the modeled rings we will use to fill the data
par(new=F, mfrow=c(1,1), mar=c(5,5,0,0)+0.1)
for(i in unique(ring.data$TreeID)){
	plot(RW ~ Year, data=ring.data[ring.data$TreeID==i,], type="l", lwd=0.5, xlim=range(ring.data$Year, na.rm=T), ylim=range(ring.data$RW, na.rm=T))
	par(new=T)
}
for(i in unique(ring.data$TreeID)){
	plot(RW.modeled ~ Year, data=ring.data[ring.data$TreeID==i & is.na(ring.data$RW),], type="p", cex=0.4, pch=19, col="red", xlim=range(ring.data$Year, na.rm=T), ylim=range(ring.data$RW, na.rm=T), ylab="")
	par(new=T)
}
# for(i in unique(ring.data$TreeID)){
	# plot(RW.m1b ~ Year, data=ring.data[ring.data$TreeID==i & is.na(ring.data$RW),], type="p", cex=0.5, pch=19, col="blue", xlim=range(ring.data$Year, na.rm=T), ylim=range(ring.data$RW, na.rm=T))
	# par(new=T)
# }
par(new=F)

plot(RW.modeled ~ RW, pch=19, xlim=c(0,1), ylim=c(0,1), data=ring.data)
abline(a=0, b=1, col="red")


# Saving the GAMM From above so we can load it without having to refit it
save(m1, file="GapFilling_gamm_mmf_2015.01.rData")


############
# Creating a data frame with just predicted
predict0 <- ring.data[,c("TreeID", "Year", "RW.modeled")]
predict0$Year <- as.factor(predict0$Year)
summary(predict0)

trees.predict <- recast(predict0, Year ~ TreeID)
#summary(trees.predict)
trees.predict [1:10,1:10]
trees.predict [(length(trees.predict [,1])-10):length(trees.predict[,1]),1:10]

write.csv(trees.predict, "Trees_RW_modeled.csv", row.names=F)

############
# Creating a data frame with just observed
trees.obs <- ring.data[,c("TreeID", "Year", "RW")]
trees.obs $Year <- as.factor(trees.obs $Year)
summary(trees.obs)

trees.obs <- recast(trees.obs, Year ~ TreeID)
#summary(trees.obs)
head(trees.obs[,1:10])
trees.obs[(length(trees.obs[,1])-10):length(trees.obs[,1]),1:10]

summary(trees.obs)
write.csv(trees.obs, "Trees_RW_measured.csv", row.names=F)

####################################################
# replacing missing ring widths with predicted values (positive or 0)
####################################################
trees.obs <- read.csv("Trees_RW_measured.csv", row.names=1)
trees.predict <- read.csv("Trees_RW_modeled.csv", row.names=1)

# replacing observed with predicted where I don't have ring measurements
trees.gapfill <- trees.obs

for(j in 1:ncol(trees.gapfill)){
	trees.gapfill[is.na(trees.gapfill[,j]),j] <- trees.predict[is.na(trees.gapfill[,j]),j]
	}

#min(trees.gapfill[,2:ncol(trees.gapfill)])
trees.gapfill[1:10,1:10]
trees.gapfill[(length(trees.gapfill[,1])-20):length(trees.gapfill[,1]),1:10]
min(trees.gapfill)

# checking dimensions, adding year row names
dim(trees.gapfill)

write.csv(trees.gapfill, "Trees_RW_gapfilled.csv", row.names=T)









# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# Haven't been through the rest of this script yet!
# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------








###################################
# Reconstructing Basal Area working from the outside in
###################################

# Reading in core data
cores.data <- read.csv("Cores_Data_Measured.csv")
cores.data$Plot <- as.factor(cores.data$Plot)
cores.data$Tree <- as.factor(cores.data$Tree)
summary(cores.data)



##############
# Actual BA reconstruction
cores.ba.cum <- as.data.frame(array(dim=dim(trees.gapfill)))
names(cores.ba.cum) <- names(trees.gapfill)
rownames(cores.ba.cum) <- rownames(trees.gapfill)

# original to try to do without resorting
#for(j in seq_along(trees.gapfill)){
#	test[length(trees.gapfill[,j]),j] <- pi*((cores.data[cores.data$TreeID==names(trees.gapfill[j]),"DBH"]*10)^2)
#	for(i in 1:(length(test[,j])-1)){
#	test[i,j] <- test[i-1,j] - trees.gapfill[i,j]
#}
#}

for(j in 1:ncol(trees.gapfill)){
	# inserting 2013 BA (pi*(DBH/2)^2) for FLT (sampled in 2013) & NA for everthing else
	cores.ba.cum[1,j] <- ifelse(substr(names(trees.gapfill[j]), 1, 3)=="FLT", 
	pi*(((cores.data[cores.data$TreeID==names(trees.gapfill[j]),"DBH"]*10)/2)^2), NA)
	
	# calculating 2012 BA for FLT only
	cores.ba.cum[2,j] <- ifelse(substr(names(trees.gapfill[j]), 1, 3)=="FLT", 
	 cores.ba.cum[1,j] - trees.gapfill[1,j], # For FLT calculate BA by subtracting 2013
	pi*(((cores.data[cores.data$TreeID==names(trees.gapfill[j]),"DBH"]*10)/2)^2)) # for non-FLT, insert 2012 BA
	
	# subtracting BAI measurement from diamBAeter of previous year to get end-of-season BA 
	for(i in 3:(length(cores.ba.cum[,j]))){
	cores.ba.cum[i,j] <- ifelse(trees.gapfill[i,j]>0, cores.ba.cum[i-1,j] - trees.gapfill[i-1,j], 0) 
	}
	}

# checking the data frame: check to make sure numbers get smaller through time
#summary(cores.ba.cum)
#head(cores.ba.cum)
dim(cores.ba.cum)
cores.ba.cum[1:10,1:10]
cores.ba.cum[103:113,1:10]
cores.ba.cum[(length(cores.ba.cum[,1])-10):length(cores.ba.cum[,1]),1:10]

min(cores.ba.cum, na.rm=T)

# Removing negative Basal Areas (become 0)
for(j in 1:ncol(cores.ba.cum)){
	for(i in 1:(length(cores.ba.cum[,j]))){
	cores.ba.cum[i,j] <- ifelse(cores.ba.cum[i,j]>0, cores.ba.cum[i,j], 0)
}
}

cores.ba.cum[1:10,1:10]
cores.ba.cum[103:113,1:10]

min(cores.ba.cum, na.rm=T)


#summary(cores.ba.cum)
row.names(cores.ba.cum)

write.csv(cores.ba.cum, "AllCores_BA_Cumulative_NoCorrection.csv", row.names=T)

########################################################################################################
########################################################################################################
# Calculating Pith
########################################################################################################
########################################################################################################
cores.ba.cum <- read.csv("AllCores_BA_Cumulative_NoCorrection.csv", row.names=1)
cores.ba.cum[1:10,1:10]
cores.ba.cum[103:113,1:10]
cores.ba.cum[(length(cores.ba.cum[,1])-10):length(cores.ba.cum[,1]),1:10]

# Reading in core data
cores.data <- read.csv("Cores_Data_Measured.csv")
cores.data$Plot <- as.factor(cores.data$Plot)
cores.data$Tree <- as.factor(cores.data$Tree)
summary(cores.data)
dim(cores.data)

# Extract last non-0 year (calculated Pith)
cores.pith <- as.data.frame(array(dim=c(ncol(cores.ba.cum),2)))
dim(cores.pith)
names(cores.pith) <- c("TreeID", "pith.calc")
summary(cores.pith)

cores.pith$TreeID <- as.factor(colnames(cores.ba.cum))
summary(cores.pith)

# BAI-calculated pith year = outer year + 1 - number of non-NA values
# NOTE: will need to change this when FLT gets added in
for(j in 1:ncol(cores.ba.cum)){
	cores.pith[cores.pith$TreeID==names(cores.ba.cum[j]),"pith.calc"] <- ifelse(substr(names(cores.ba.cum[j]), 1, 3)=="FLT",
	2014-sum(cores.ba.cum[,j]>0, na.rm=T), 
	2013-sum(cores.ba.cum[,j]>0, na.rm=T))
	}

# Finding the Basal Area at the innermost year
for(j in 1:ncol(cores.ba.cum)){
	cores.pith[cores.pith$TreeID==names(cores.ba.cum[j]),"Inner.BA"] <- cores.ba.cum[2014-cores.data[cores.data$TreeID==names(cores.ba.cum[j]),"Inner"],j]
}


summary(cores.pith)
cores.pith[is.na(cores.pith$Inner.BA),]
head(cores.pith)
head(data.cores)
dim(cores.pith)
dim(data.cores)

cores.pith[,4:27] <- cores.data[,c(3:8,10:27)]
summary(cores.pith)

cores.pith[1:10,]
cores.pith[(length(cores.pith[,1])-10):length(cores.pith[,1]),]

summary(cores.pith)

########################################################################################################
# Acutal Pith Model
########################################################################################################
cores.ba.cum <- read.csv("AllCores_BA_Cumulative_NoCorrection.csv", row.names=1)
cores.ba.cum[1:10,1:10]
cores.ba.cum[103:113,1:10]
cores.ba.cum[(length(cores.ba.cum[,1])-10):length(cores.ba.cum[,1]),1:10]

cores.pith <- read.csv("Establishment_AllCores.csv")
cores.pith$est.calc <- cores.pith$Pith.Yr - cores.pith$pith.calc
summary(cores.pith)
hist(cores.pith$est.calc)


# Visulalizing estimated (BA reconstruction) & observed pith
plot(pith.calc ~ Pith.Yr, data=cores.pith, main="Modeled vs. Estmiated Pith", pch=19)

# Modeling pith year based on the 2012/2013 year DBH and the BA at the inner most year
m.pith <- lme(Pith.Yr ~ pith.calc + DBH, data=cores.pith[!is.na(cores.pith$Inner.BA),], random=list(Site=~1, Trans=~1, PlotID=~1), na.action=na.omit)
summary(m.pith)
m.pith.R2 <- r.squaredGLMM(m.pith)
m.pith.R2 # R2m = 0.804291, R2c = 0.8357807

# Modeling Pith based on above equation
cores.pith$pith.modeled <- predict(m.pith, newdata=cores.pith)
summary(cores.pith)

# Finding the Basal Area at the modeled pith
for(j in 1:ncol(cores.ba.cum)){
	cores.pith[cores.pith$TreeID==names(cores.ba.cum[j]),"Pith.Mod.BA"] <- cores.ba.cum[2014-cores.pith[cores.pith$TreeID==names(cores.ba.cum[j]),"pith.modeled"],j]
}
summary(cores.pith)

# looking at estimated - modeled 
cores.pith$est.mod <- cores.pith$Pith.Yr - cores.pith$pith.modeled
summary(cores.pith) # range of visual estimate & model: -15 - 5

plot(Pith.Yr ~ pith.modeled, data=cores.pith, subset=Pith.Yr>1900)

# double checking how model compares to Pith
m.pith0 <- lm(Pith.Yr ~ pith.modeled, data=cores.pith, subset=Pith.Yr>1900)
summary(m.pith0) #0.8419



summary(cores.pith)
hist(cores.pith$Pith.offset)
hist(cores.pith$est.mod)
hist(cores.pith$Inner.BA)
hist(cores.pith[cores.pith$Inner.BA<10000, "Inner.BA"])
hist(cores.pith[cores.pith$Inner.BA<1000, "Inner.BA"])
hist(cores.pith$Pith.Mod.BA)
hist(cores.pith[cores.pith$Pith.Mod.BA<10000, "Pith.Mod.BA"])
plot(cores.pith$est.mod ~ cores.pith$Pith.offset)
plot(cores.pith$Pith.Yr ~ cores.pith$pith.modeled)

hist(cores.pith$Pith.offset)

# NOTE: COULD ADD LAYER HERE WHERE WE ONLY USE PITH DATES FROM DATED TREES OR ALL TREES
# If there's a pith estimate & offset is <= 10 OR modeled > inner, use core pith est
# If there's a pith estimate AND ((offset is >10 years AND modeled !< inner) OR inner Basal Area > 5,000), average core & modeled
# If no pith estimate AND modeled < inner, use modeled 
# IF no pith estimate AND modeled > inner, use inner
for(i in 1:length(cores.pith$Pith.Yr)){
cores.pith$pith.use[i] <- ifelse(!is.na(cores.pith$Pith.Yr[i]) & (cores.pith$Pith.offset[i]<=10 |  cores.pith$Inner<cores.pith$pith.modeled), cores.pith$Pith.Yr[i], 
                                 ifelse(!is.na(cores.pith$Pith.Yr[i]) & cores.pith$Pith.offset[i]>10 & cores.pith$Inner[i] > cores.pith$pith.modeled[i], mean(cores.pith$Pith.Yr[i], cores.pith$pith.modeled[i]), 
                                 ifelse(is.na(cores.pith$Pith.Yr[i]) & cores.pith$Inner[i] >= cores.pith$pith.modeled[i], cores.pith$pith.modeled[i], cores.pith$Inner[i])))
}

summary(cores.pith)
summary(cores.pith$pith.use)

sum(!is.na(cores.pith$Pith.Yr) & (cores.pith$Pith.offset<=10 | cores.pith$Inner < cores.pith$pith.modeled)) # number of visual estimates used: 913
sum(!is.na(cores.pith$Pith.Yr) & cores.pith$Pith.offset>10 & cores.pith$Inner > cores.pith$pith.modeled) # number of averaged used: 28
sum(is.na(cores.pith$Pith.Yr) & cores.pith$Inner>=cores.pith$pith.modeled) # number of modeled pith used: 202
sum(is.na(cores.pith$Pith.Yr) & cores.pith$Inner<cores.pith$pith.modeled) # had to go with inner year as best guess: 45

dim(cores.pith)
913 + 28 + 202 + 45


# Finding the Basal Area at the Pith I'm using
for(j in 1:ncol(cores.ba.cum)){
	cores.pith[cores.pith$TreeID==names(cores.ba.cum[j]),"Pith.Use.BA"] <- cores.ba.cum[2014-cores.pith[cores.pith$TreeID==names(cores.ba.cum[j]),"pith.use"],j]
}

# converting BA to DBH
cores.pith$pith.dbh <- sqrt(cores.pith$Pith.Use.BA/pi)*.1

summary(cores.pith$pith.dbh) # note, despite the range, most of the data goes to 0
summary(cores.pith[cores.pith$Pith=="Y", "pith.dbh"])

hist(cores.pith$pith.dbh)

write.csv(cores.pith, "Establishment_AllCores.csv", row.names=F)

##################################################################################
##################################################################################
# Plotting establishment --> see separate script
##################################################################################
##################################################################################


##################################################################################
##################################################################################

##################################################################################
# Removing bai earlier than predicted pith date & replacing with 0 (important to ensure no decreases in average basal area through time)
##################################################################################
# reading in establishment data
cores.pith <- read.csv("Establishment_AllCores.csv")
summary(cores.pith)
## Make sure no cores have missing Pith Estimates

# if need to get rid of missing pith estimates
#cores.pith2 <- cores.pith[!is.na(cores.pith$pith.use),]
#summary(cores.pith2)
#dim(cores.pith)

# reading in calculated core data
trees.obs.mix <- read.csv("AllCores_BAI_meas_predicted.csv", row.names=1)
#summary(trees.obs.mix)
trees.obs.mix[1:10, 1:10]
trees.obs.mix[100:110, 1:10]
trees.obs.mix[200:210, 1:10]
dim(trees.obs.mix)

names1 <- names(trees.obs.mix)
names2 <- unique(cores.pith$TreeID)
length(names1)
length(names2)


cores.pith[cores.pith$TreeID==colnames(trees.obs.mix["BLDA101"]),"pith.use"]


dim(trees.obs.mix)

# creating an object with the range of years
years <- row.names(trees.obs.mix)

# trees.obs.filled = filled data with Pith Correction
# A faster version with help from John
trees.obs.filled <- trees.obs.mix
nrow(trees.obs.filled)
nrow(trees.obs.mix)

trees.obs.filled[1:10, 1:10]
trees.obs.filled[100:110, 1:10]
trees.obs.filled[233:243, 1:10]
trees.obs.filled[(nrow(trees.obs.filled)-10):nrow(trees.obs.filled), 1:10]


summary(cores.pith)

for(j in 1:ncol(trees.obs.filled)){
	# create a value with pith year for each core (j)
	temp.row <- 2015 - cores.pith[cores.pith$TreeID==colnames(trees.obs.filled[j]),"pith.use"]
	if(temp.row < nrow(trees.obs.filled)) 
	trees.obs.filled[temp.row:nrow(trees.obs.filled), j] <- 0 # 2015 means keep pith year)
	}

trees.obs.filled[1:10, 1:10]
trees.obs.filled[100:110, 1:10]
trees.obs.filled[(nrow(trees.obs.filled)-10):nrow(trees.obs.filled), 1:10]
min(trees.obs.filled, na.rm=T)
sum(is.na(trees.obs.filled))
sum(is.na(trees.obs.mix))
dim(trees.obs.mix)

#summary(trees.obs.filled)
row.names(trees.obs.filled)

write.csv(trees.obs.filled, "AllCores_BAI_filledtopith.csv", row.names=T)

#par(new=F)
#for(j in seq_along(trees.obs.filled)){
#		plot(trees.obs.filled[,j] ~ as.numeric(row.names(trees.obs.filled[1])), xlim=range(as.numeric(row.names(trees.obs.filled[1]))), ylim=range(trees.obs.filled, na.rm=T), xlab="Year", ylab="mm2", type="l", lwd=0.1)
#	par(new=T)
#}

##################################################################################
# Correcting BA calcuations
##################################################################################
cores.pith <- read.csv("Establishment_AllCores.csv")
summary(cores.pith)
## Make sure no cores have missing Pith Estimates

# reading in calculated core data
cores.ba.cum <- read.csv("AllCores_BA_Cumulative_NoCorrection.csv", row.names=1)

cores.ba.cum[1:10,1:10]
cores.ba.cum[103:113,1:10]
cores.ba.cum[(length(cores.ba.cum[,1])-10):length(cores.ba.cum[,1]),1:10]


names1 <- names(cores.ba.cum)
names2 <- unique(cores.pith$TreeID)
length(names1)
length(names2)

dim(cores.ba.cum)

# creating an object with the range of years
years <- row.names(cores.ba.cum)

# cores.ba.corr = basal area data with Pith Correction
cores.ba.corr <- cores.ba.cum
for(j in 1:ncol(cores.ba.corr)){
	# create a value with pith year for each core (j)
	temp.row <- 2015 - cores.pith[cores.pith$TreeID==colnames(cores.ba.corr[j]),"pith.use"]
	if(temp.row < nrow(cores.ba.corr)) cores.ba.corr[temp.row:nrow(cores.ba.corr), j] <- 0 # 2015 means keep pith year)
	}

cores.ba.corr[1:10, 1:10]
cores.ba.corr[100:110, 1:10]
cores.ba.corr[200:210, 1:10]
min(cores.ba.corr, na.rm=T)
sum(is.na(cores.ba.corr))
sum(is.na(cores.ba.cum))
dim(cores.ba.corr)

#summary(trees.obs.filled)
row.names(cores.ba.corr)

write.csv(cores.ba.corr, "AllCores_BA_Cumulative_PithCorrected1.csv", row.names=T)

# Plotting Basal Area through time
par(new=F)
for(j in seq_along(cores.ba.corr)){
		plot(cores.ba.corr[,j] ~ as.numeric(row.names(cores.ba.corr[1])), xlim=range(as.numeric(row.names(cores.ba.corr[1]))), ylim=range(cores.ba.corr, na.rm=T), xlab="Year", ylab="m2/Ha", type="l", lwd=0.1)
	par(new=T)
}

##########################

####################################################
# Smoothing out cumulative Basal Area at Pith by averaging in-out and out-in calculations of BA
####################################################
cores.pith <- read.csv("Establishment_AllCores.csv")
cores.pith$est.calc <- cores.pith$Pith.Yr - cores.pith$pith.calc
summary(cores.pith$pith.dbh)
cores.pith[is.na(cores.pith$pith.dbh),]
hist(cores.pith$pith.use)
hist(cores.pith$pith.dbh)

# Subsetting all cores >2 cm DBH at pith
cores.check <- cores.pith[cores.pith$pith.dbh>2,c ("TreeID",  "Spp", "DBH",  "Inner", "Pith.Yr", "pith.calc", "est.calc", "pith.modeled", "pith.use", "pith.dbh")]
cores.check <- cores.check[!is.na(cores.check$pith.dbh),]
summary(cores.check)
dim(cores.check)
dim(cores.pith)
nrow(cores.check)/nrow(cores.pith) # Proportion of cores with pith >2 CM DBH
length(cores.check[!is.na(cores.check$Pith.Yr),1]) # 188 trees have a pith estimate; only 85 are missing pith est

##################
# Looking at the distributions
par(new=F)
hist(cores.pith$pith.dbh, main="Basal Area at Pith, All Trees")
hist(cores.pith[cores.pith$pith.dbh>2, "pith.dbh"], main="", xlab="DBH (cm)")
hist(cores.pith[cores.pith$pith.dbh<2, "pith.dbh"], main="", xlab="DBH (cm)")

ggplot(data=cores.pith) + q.blank + facet_grid(Site ~ .) + geom_histogram(aes(x=pith.dbh), binwidth=.5) + ggtitle("DBH at pith, all trees") 
ggplot(data=cores.pith[cores.pith$pith.dbh>=2 & !is.na(cores.pith$pith.dbh),]) + q.blank + facet_grid(Site ~ .) + geom_histogram(aes(x=pith.dbh), binwidth=.5) + ggtitle("DBH at pith, trees >= 2 cm dbh at pith") 
ggplot(data=cores.pith[cores.pith$pith.dbh<2 & !is.na(cores.pith$pith.dbh),]) + q.blank + facet_grid(Site ~ .) + geom_histogram(aes(x=pith.dbh), binwidth=.5) + ggtitle("DBH at pith, trees < 2 cm dbh at pith") 
##################
cores.ba.corr <- read.csv("AllCores_BA_Cumulative_PithCorrected1.csv", row.names=1)
cores.ba.corr[1:10, 1:10]
cores.ba.corr[100:110, 1:10]
cores.ba.corr[200:210, 1:10]
min(cores.ba.corr, na.rm=T)

# Reading in BAI file that will be necessary for corrections
trees.obs.filled <- read.csv("AllCores_BAI_filledtopith.csv", row.names=1)
trees.obs.filled[1:10, 1:10]
trees.obs.filled[100:110, 1:10]
trees.obs.filled[(nrow(trees.obs.filled)-10):nrow(trees.obs.filled), 1:10]
min(trees.obs.filled, na.rm=T)
sum(is.na(trees.obs.filled))


########################
# Making data frame for new corrected data
#cores.ba.corr2 <- as.data.frame(array(dim=dim(cores.ba.corr)))
#row.names(cores.ba.corr2) <- cores.ba.corr$Year
#names(cores.ba.corr2) <- names(cores.ba.corr)

# making a data frame with only the trees that need to be fixed
cores.ba.corr2 <- as.data.frame(array(dim=c(nrow(cores.ba.corr), nrow(cores.check))))
row.names(cores.ba.corr2) <- row.names(cores.ba.corr)
names(cores.ba.corr2) <- unique(cores.check$TreeID)
dim(cores.ba.corr2)
dim(cores.ba.corr)

cores.ba.corr2[1:10, 1:10]
cores.ba.corr2[100:110, 1:10]
cores.ba.corr2[200:210, 1:10]
names(cores.ba.corr2)
#for(j in 1:ncol(cores.ba.corr2)){
	# if the core is not in the list that needs to be fixed, just punk it into the data frame
#	ifelse(!(names(cores.ba.corr2[j]) %in% cores.check$TreeID),  cores.ba.corr2[,j] <- cores.ba.corr[,j], 

for(j in unique(cores.check$TreeID)){	
	pith.row <- ifelse(2014 - cores.check[cores.check$TreeID==j, "pith.use"] < nrow(cores.ba.corr2), 2014 - cores.check[cores.check$TreeID==j, "pith.use"], nrow(cores.ba.corr2))

	cores.ba.corr2[pith.row,j] <- trees.obs.filled[pith.row, j]
	cores.ba.corr2[(pith.row+1):nrow(cores.ba.corr2),j] <- 0

	for(i in (pith.row-1):1){
		cores.ba.corr2[i,j] <- cores.ba.corr2[i+1, j] + trees.obs.filled[i,j]
		}
	}

cores.ba.corr2[1:10, 1:10]
cores.ba.corr2[100:110, 1:10]
cores.ba.corr2[200:210, 1:10]

######################
cores.ba.corr3 <- as.data.frame(array(dim=dim(cores.ba.corr)))
row.names(cores.ba.corr3) <- row.names(cores.ba.corr)
names(cores.ba.corr3) <- names(cores.ba.corr)

cores.ba.corr3[1:10, 1:10]
cores.ba.corr3[100:110, 1:10]
cores.ba.corr3[(nrow(cores.ba.corr3)-10):nrow(cores.ba.corr3), 1:10]

dim(cores.ba.corr)
dim(cores.ba.corr2)

# Writing BA reconstructions that were fine
for(j in unique(names(cores.ba.corr))){
	if(!(j %in% cores.check$TreeID)) 
	cores.ba.corr3[,j] <- cores.ba.corr[,j]

# taking mean of in-out and original BA reconstructions	
for(j in unique(names(cores.ba.corr2))){
	if(j %in% cores.check$TreeID)
	temp.ba <- as.data.frame(cores.ba.corr[,j])
	temp.ba[,2] <- cores.ba.corr2[,j]
	cores.ba.corr3[,j] <- rowMeans(temp.ba)
}


cores.ba.corr3[1:10, 1:10]
cores.ba.corr3[100:110, 1:10]
cores.ba.corr3[200:210, 1:10]

write.csv(cores.ba.corr3, "AllCores_BA_Cumulative_PithCorrected_Final.csv", row.names=T)

# Plotting Basal Area through time
par(new=F)
for(j in seq_along(cores.ba.corr3)){
		plot(cores.ba.corr3[,j] ~ as.numeric(row.names(cores.ba.corr3[1])), xlim=range(as.numeric(row.names(cores.ba.corr3[1]))), ylim=range(cores.ba.corr3, na.rm=T), xlab="Year", ylab="m2/Ha", type="l", lwd=0.1)
	par(new=T)
}


for(j in unique(cores.pith$TreeID)){
	cores.pith[cores.pith$TreeID==j, "pith.ba.corr"] <- cores.ba.corr3[2014-cores.pith[cores.pith$TreeID==j, "pith.use"], j]
	}
cores.pith$pith.dbh.corr <- sqrt(cores.pith$pith.ba.corr/pi)*.1 
summary(cores.pith)
hist(cores.pith$pith.dbh.corr)
hist(cores.pith$pith.dbh)

cores.check2 <- cores.pith[cores.pith$pith.dbh.corr>2,c ("TreeID",  "Spp", "DBH",  "Inner", "Pith.Yr", "pith.calc", "est.calc", "pith.modeled", "pith.use", "pith.dbh.corr")]
dim(cores.check2)
dim(cores.check)
summary(cores.check2)
summary(cores.check)

## NOTE: Some cores still have relatively large DBH at pith, but it's a lot better; worst offenders are all pre-1900





##################################################################################
##################################################################################



################################
# Adjusting Tree BA to a per hectare basis
# To do this: multiple tree BA by stand density (for each tree)
cores.data <- read.csv("Cores_Data_Measured.csv")
cores.data$Plot <- as.factor(cores.data$Plot)
cores.data$Tree <- as.factor(cores.data$Tree)
summary(cores.data)

cores.ba.corr <- read.csv("AllCores_BA_Cumulative_PithCorrected_Final.csv", row.names=1)
cores.ba.corr[1:10, 1:10]
cores.ba.corr[100:110, 1:10]
cores.ba.corr[(nrow(cores.ba.corr)-10):nrow(cores.ba.corr), 1:10]

cores.ba.ha <- cores.ba.corr
# *.01 mm2 to cm2 *.0001
for(j in unique(names(cores.ba.ha))){
	density <- cores.data[cores.data$TreeID==j, "Density.ha"]
	cores.ba.ha[,j] <- cores.ba.ha[,j] * density * .000001 # coverts mm2 to m2/ha
}

cores.ba.ha[1:10,1:10]

min(cores.ba.ha, na.rm=T)

write.csv(cores.ba.ha, "AllCores_BA_Cumulative_PithCorrected_perHA.csv", row.names=T)



########################
cores.ba.ha <- read.csv("AllCores_BA_Cumulative_PithCorrected_perHA.csv", row.names=1)
cores.ba.ha[1:10,1:10]
min(cores.ba.ha, na.rm=T)

# Plotting Basal Area through time
par(new=F)
for(j in seq_along(cores.ba.ha)){
		plot(cores.ba.ha[,j] ~ as.numeric(row.names(cores.ba.ha[1])), xlim=range(as.numeric(row.names(cores.ba.ha[1]))), ylim=range(cores.ba.ha, na.rm=T), xlab="Year", ylab="m2/Ha", type="l", lwd=0.1)
	par(new=T)
}


##################################################################################
##################################################################################
# Adjusting Tree BA to a per hectare basis
# To do this: multiple tree BA by stand density (for each tree)
cores.pith <- read.csv("Establishment_AllCores.csv")
summary(cores.pith)

cores.ba.corr <- read.csv("AllCores_BA_Cumulative_PithCorrected_Final.csv", row.names=1)
cores.ba.corr[1:10, 1:10]
cores.ba.corr[100:110, 1:10]
cores.ba.corr[(nrow(cores.ba.corr)-10):nrow(cores.ba.corr), 1:10]


age.df <- data.frame(array(dim=dim(cores.ba.corr)))
row.names(age.df) <- row.names(cores.ba.corr)
names(age.df) <- names(cores.ba.corr)

age.df[1:10, 1:10]
age.df[100:110, 1:10]
age.df[(nrow(age.df)-10):nrow(age.df), 1:10]

for(j in unique(names(age.df))){
	pith.row <- 2014-cores.pith[cores.pith$TreeID==j, "pith.use"]
	
	for(i in pith.row:1){
		age.df[i,j] <- pith.row - i 
	}
	}

# replacing 2013 for non-FLT with NA
for(j in unique(names(age.df))){
	age.df[1,j] <- ifelse(substr(j, 1, 3)=="FLT", age.df[1,j], NA)
	}



age.df[1:10, 1:10]
age.df[100:110, 1:10]
age.df[(nrow(age.df)-10):nrow(age.df), 1:10]


write.csv(age.df, "AllCores_Age.csv", row.names=T)

##################################################################################
##################################################################################
# Merging BAI & cumulative BA data sets

ring.data <- read.csv("Cores_FullData_AllYrs.csv")
ring.data$Plot <- as.factor(ring.data$Plot)
ring.data$Tree <- as.factor(ring.data$Tree)
summary(ring.data)
dim(ring.data)

ring.data <- ring.data[,c(1:21)]
summary(ring.data)
dim(ring.data)

age <- read.csv("AllCores_Age.csv", row.names=1)
age[1:10, 1:10]
age.stack <- stack(age)
names(age.stack) <- c("Age", "TreeID")
age.stack$Year <- as.numeric(row.names(age))
summary(age.stack)

cores.ba.ha <- read.csv("AllCores_BA_Cumulative_PithCorrected_perHA.csv", row.names=1)
cores.ba.ha[1:10, 1:10]
cores.ba.ha.stack <- stack(cores.ba.ha)
names(cores.ba.ha.stack) <- c("BA.m2ha", "TreeID")
cores.ba.ha.stack$Year <- as.numeric(row.names(cores.ba.ha))
summary(cores.ba.ha.stack)


trees.obs.filled <- read.csv("AllCores_BAI_filledtopith.csv", row.names=1)
#summary(trees.obs.filled)
trees.obs.filled[1:10, 1:10]
trees.obs.stack <- stack(trees.obs.filled)
names(trees.obs.stack) <- c("BAI.filled", "TreeID")
trees.obs.stack$Year <- as.numeric(row.names(trees.obs.filled))
summary(trees.obs.stack)

cores.ba.corr <- read.csv("AllCores_BA_Cumulative_PithCorrected_Final.csv", row.names=1)
#summary(cores.ba.cum)
cores.ba.corr[1:10, 1:10]
cores.ba.tree <- stack(cores.ba.corr)
names(cores.ba.tree) <- c("BA.tree", "TreeID")
cores.ba.tree$Year <- as.numeric(row.names(cores.ba.corr))
summary(cores.ba.tree)

ring.data1 <- merge(ring.data, age.stack, all.x=T)
summary(ring.data1)
dim(ring.data1)
dim(ring.data)

ring.data2 <- merge(ring.data1, cores.ba.ha.stack, all.x=T)
summary(ring.data2)
dim(ring.data2)
dim(ring.data1)

ring.data3 <- merge(ring.data2, trees.obs.stack, all.x=T)
summary(ring.data3)
dim(ring.data3)
dim(ring.data2)

ring.data4 <- merge(ring.data3, cores.ba.tree, all.x=T)
summary(ring.data4)
dim(ring.data4)
dim(ring.data3)

# Adding Tree pres/abs
ring.data4$Stems <- ifelse(ring.data4$BAI.filled>0 & !(is.na(ring.data4$BAI.filled)), 1, 0)
summary(ring.data4)

# Adding tree density per HA
plot.data <- read.csv("PlotData.csv")
plot.data$plot <- as.factor(plot.data$plot)
summary(plot.data)

for(j in unique(plot.data$plotID)){
	plot.area <- plot.data[plot.data$plotID==j, "plot.canopy.area"]
	ring.data4[ring.data4$PlotID==j,"Density.Ha"] <- ring.data4[ring.data4$PlotID==j,"Stems"]/plot.area * 10000
}

summary(ring.data4)

write.csv(ring.data4, "Cores_FullData2_AllYrs.csv", row.names=F)

ring.data <- read.csv("Cores_FullData2_AllYrs.csv")
ring.data$Plot <- as.factor(ring.data$Plot)
ring.data$Tree <- as.factor(ring.data$Tree)
summary(ring.data)

##################################################################################
# see next script for reconstructing basal area of trees with no samples 
##################################################################################
