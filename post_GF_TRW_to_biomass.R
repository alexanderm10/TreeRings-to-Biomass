library(dplR)
# Run this script after the gap filling process scripts have been run
# For the NACP15 abstract run Tree_rw_gapfilled.csv

g.filled.rw <- read.csv("Trees_RW_gapfilled.csv", header=T, row.names=1)


# read in years as rown names

summary(g.filled.rw)
head(g.filled.rw)

# ordering the dataset so that the most recent years are at the top.
# This helps when we go to do the diameter reconstrcution
g.filled.rw <- g.filled.rw[order(row.names(g.filled.rw), decreasing=T),order(names(g.filled.rw))]
head(g.filled.rw)


#making dataframe of tree diameter
tree.deets <- data.frame(names(g.filled.rw))
names(tree.deets) <- "ID"
summary(tree.deets)

names(tree.data)

for(i in unique(tree.deets$ID)){
  tree.deets[tree.deets$ID==i, "DBH"] <- tree.data[tree.data$TreeID==i,"dbh"]  
  
  tree.deets[tree.deets$ID==i, "SPP"] <- tree.data[tree.data$TreeID==i, "species"]
  
  tree.deets[tree.deets$ID==i, "plot"] <- tree.data[tree.data$TreeID==i, "plot"]
}


summary(tree.deets)


#diameter reconstructions of each tree from the trees that dated??
gf.dbh.recon <- g.filled.rw
summary(gf.dbh.recon)
summary(tree.deets)
for(j in seq_along(gf.dbh.recon)){
  # inserting 2012 DBH
  gf.dbh.recon[1,j] <- tree.deets[tree.deets$ID==names(gf.dbh.recon[j]),"DBH"] 
  for(i in 2:(length(gf.dbh.recon[,j]))){
    gf.dbh.recon[i,j] <- ifelse(!is.na(g.filled.rw[i,j]), gf.dbh.recon[i-1,j] - g.filled.rw[i-1,j]*2, g.filled.rw[i,j]*2) # subtracting the previous year's growth from DBH to get that year's DBH
  }
}


summary(gf.dbh.recon)
min(gf.dbh.recon, na.rm=T)

write.csv(gf.dbh.recon, "gap_filled_dbh.recon.csv")

#quick plot
spag.plot(gf.dbh.recon)

# We have some negative diameters.  For the poster we are just going to make them NAs.  But in the future we need to make a hard decision on this

gf.dbh.recon[gf.dbh.recon < 0] <- NA
min(gf.dbh.recon, na.rm=T)

##########################################################################
#using ring widths to caclulate a Basal Area Increment (BAI)
# FOR CHRISTY: IS THIS WHERE THIS SHOULD GO?
##########################################################################
g.filled.rw <- g.filled.rw[order(row.names(g.filled.rw), decreasing=F),order(names(g.filled.rw))]
head(g.filled.rw)
gf.tree.bai.out <- bai.out(g.filled.rw, diam=tree.deets)
summary(tree.bai.out)
min(tree.bai.out, na.rm=T)
spag.plot(gf.tree.bai.out)

# FOR CHRISTY: I think I have things working.  Would you mind doind a quick double check over these scripts before you use them to do the next step?

##########################################################################
# Allometric Equations
##########################################################################


#Convert to biomass with the allometric equation
#using generalized jenkins format from the paper
jenkins.general <- function(dbh, a, b){
  exp(a + b*log(dbh))
}

mmf.jenkins.recon <- jenkins.general(gf.dbh.recon, -2.0127, 2.4342)
summary(mmf.jenkins.recon)
#now in kilograms of biomass per tree

names(tree.data)
#need to incorporate the density into things
jenkins.bm.density <- data.frame(array(NA, dim=c(nrow(mmf.jenkins.recon), ncol(mmf.jenkins.recon))))
row.names(jenkins.bm.density) <- row.names(mmf.jenkins.recon)  #CRR Added
names(jenkins.bm.density)<- names(mmf.jenkins.recon)

for(i in unique(names(mmf.jenkins.recon))){
  jenkins.bm.density[,i] <- mmf.jenkins.recon[,i]*tree.data[tree.data$TreeID==i,"dens.stem.ha"]
}
summary(jenkins.bm.density)

##########################################################################
#we are now in kg of biomass per Ha
##########################################################################
jenkins.bm.density.meter <- jenkins.bm.density/10000
summary(jenkins.bm.density.meter)

##########################################################################
#now we are in kg of biomass per meter square meter
##########################################################################

#now need to aggregate the biomass per tree up to the plot level
# Dr. Rollinson Fix:
plots <- unique(substr(names(jenkins.bm.density.meter), 1, 3)) # You had the right idea, but it was throwing errors because you were trying to evaluate plots you haven't gotten to yet
jenkins.plot.meter <- data.frame(array(NA, dim=c(nrow(jenkins.bm.density.meter), length(plots))))
row.names(jenkins.plot.meter) <- row.names(jenkins.bm.density.meter)  #CRR Added
names(jenkins.plot.meter) <- plots

for(i in unique(plots)){
  cols <- which(substr(names(jenkins.bm.density.meter),1,3)==i)
  ifelse(length(cols) > 1, jenkins.plot.meter[,which(plots==i)] <- rowSums(jenkins.bm.density.meter[,cols], na.rm=T), jenkins.plot.meter[,which(plots==i)] <- jenkins.bm.density.meter[,cols])
}
summary(jenkins.plot.meter)
head(jenkins.plot.meter)

write.csv(jenkins.plot.meter, "MMF_plot_BM_cum.csv")

plot(jenkins.plot.meter$MMA~row.names(jenkins.plot.meter), type="l", col="red")
par(new = T)
plot(jenkins.plot.meter$MMB~row.names(jenkins.plot.meter), type="l", col="Green", ylim=range(jenkins.plot.meter$MMA))
par(new=T)
plot(jenkins.plot.meter$MMC~row.names(jenkins.plot.meter), type="l", col="Blue",ylim=range(jenkins.plot.meter$MMA))


##########################################################################
# Adding up the plots to get a site level biomass load
# Need to streamline this so that when we add more sites it can be automatic
##########################################################################
# names(tree.data)
# sites<- unique(tree.data$site)
# jenkins.site.meter <- data.frame(array(NA, dim=c(nrow(jenkins.plot.meter), length(sites))))
# row.names(jenkins.site.meter) <- row.names(jenkins.plot.meter)
# names(jenkins.site.meter) <- sites
# summary(jenkins.site.meter)
 

jenkins.site.meter <- data.frame(array(NA, dim=c(nrow(jenkins.plot.meter), 2)))
names(jenkins.site.meter) <- c("MMF", "MMF.SD")
row.names(jenkins.site.meter) <- row.names(jenkins.plot.meter)

jenkins.site.meter$MMF <- rowMeans(jenkins.plot.meter)
jenkins.site.meter$MMF.SD <- apply(jenkins.plot.meter, 1, sd)
summary(jenkins.site.meter)
head(jenkins.site.meter)
write.csv(jenkins.site.meter, "MMF_site_BM_cum.csv")
##########################################################################
# Now need to get the Biomass increment for the years for the plots then go to the site
##########################################################################
jenkins.plot.bm.inc <- data.frame(array(NA, dim=c(nrow(jenkins.plot.meter), length(jenkins.plot.meter))))
names(jenkins.plot.bm.inc) <- names(jenkins.plot.meter)
row.names(jenkins.plot.bm.inc) <- row.names(jenkins.plot.meter)
summary(jenkins.plot.bm.inc)

for(j in seq_along(jenkins.plot.bm.inc)){
  # inserting oldest biomass
  jenkins.plot.bm.inc[nrow(jenkins.plot.bm.inc),j] <- NA
  for(i in (length(jenkins.plot.bm.inc[,j])-1):1){
    jenkins.plot.bm.inc[i,j] <- jenkins.plot.meter[i,j] - jenkins.plot.meter[i+1,j] # subtracting the previous year's growth from DBH to get that year's DBH
  }
}
head(jenkins.plot.bm.inc)
summary(jenkins.plot.bm.inc)
write.csv(jenkins.plot.bm.inc, "MMF_plot_BM_inc.csv")

plot(jenkins.plot.bm.inc$MMA~row.names(jenkins.plot.bm.inc), type="l", col="red", ylim=c(0,1))
par(new = T)
plot(jenkins.plot.bm.inc$MMB~row.names(jenkins.plot.bm.inc), type="l", col="Green",ylim=c(0,1))
par(new=T)
plot(jenkins.plot.bm.inc$MMC~row.names(jenkins.plot.bm.inc), type="l", col="Blue",ylim=c(0,1))

##########################################################################
# We have now an BM increment for each plot
# Now need to get the Biomass increment for the site as a whole
##########################################################################

jenkins.site.bm.inc <- data.frame(array(NA, dim=c(nrow(jenkins.plot.bm.inc), 2)))
names(jenkins.site.bm.inc) <- names(jenkins.site.meter)
row.names(jenkins.site.bm.inc) <- row.names(jenkins.site.meter)
summary(jenkins.site.bm.inc)
jenkins.site.bm.inc$MMF <- rowMeans(jenkins.plot.bm.inc) 
jenkins.site.bm.inc$MMF.SD <- apply(jenkins.plot.bm.inc, 1, sd)
summary(jenkins.site.bm.inc)
head(jenkins.site.bm.inc)


plot(jenkins.plot.bm.inc$MMA~row.names(jenkins.plot.bm.inc), type="l", col="red", ylim=c(0,1))
par(new = T)
plot(jenkins.plot.bm.inc$MMB~row.names(jenkins.plot.bm.inc), type="l", col="Green",ylim=c(0,1))
par(new=T)
plot(jenkins.plot.bm.inc$MMC~row.names(jenkins.plot.bm.inc), type="l", col="Blue",ylim=c(0,1))
par(new=T)
plot(jenkins.site.bm.inc$MMF~row.names(jenkins.site.bm.inc), type="l", lwd=3, ylim=c(0,1))

write.csv(jenkins.site.bm.inc, "MMF_site_BM_inc.csv")


##########################################################################
# Going to aggregate the biomass to the species level
##########################################################################

#now need to aggregate the biomass per tree up to the plot level
# Dr. Rollinson Fix:
summary(tree.deets)
spp <- unique(tree.deets$SPP)
spp.plot <- unique(tree.deets$plot)

tree.deets[1:10,]; jenkins.bm.density.meter[1:10, 1:10]
summary(spp)

# Making a 3-D array that's years x species x plots
jenkins.spp.meter <- array(NA, dim=c(nrow(jenkins.bm.density.meter), length(spp), length(spp.plot)))
row.names(jenkins.spp.meter) <- row.names(jenkins.bm.density.meter)  #CRR Added
#names(jenkins.spp.meter) <- spp # Skipping this for now because I don't remember how to do names of 3-D arrays
dim(jenkins.spp.meter)


for(i in 1:length(spp.plot)){
  for(j in 1:length(spp)){ # dim #2!
    cols <- which(tree.deets$SPP==spp[j] & tree.deets$plot==spp.plot[i])
    if(length(cols) > 1){ jenkins.spp.meter[,j,i] <- rowSums(jenkins.bm.density.meter[,cols], na.rm=T)
    } else if (length(cols) == 1) { jenkins.spp.meter[,j,i] <- jenkins.bm.density.meter[,cols] 
    } else jenkins.spp.meter[,j,i] <- NA
  }
}
dim(jenkins.spp.meter)
summary(jenkins.spp.meter[,,1])
summary(jenkins.spp.meter[,,2])
summary(jenkins.spp.meter[,,3])
head(jenkins.spp.meter)

test.a <- jenkins.spp.meter[,,1:2]
summary(test.a)

# test <- data.frame(array(NA, dim=c(nrow(jenkins.bm.density.meter), length(spp))))
# names(test) <- spp
# row.names(test) <- row.names(jenkins.bm.density.meter)
# summary(test)
# dim(test)

test <- apply(jenkins.spp.meter, c(1:2), FUN=mean, na.rm=T)
test <- data.frame(test); names(test) <- spp
summary(test)
summary(jenkins.site.meter)
head(jenkins.spp.meter)

test2 <- apply(jenkins.spp.meter, c(1:2), FUN=sd, na.rm=T)
test2 <- data.frame(test2); names(test2) <- spp
summary(test2)
summary(jenkins.site.meter)
head(jenkins.spp.meter)
write.csv(jenkins.spp.meter[order(row.names(jenkins.spp.meter), decreasing=F),order(names(jenkins.spp.meter))], "MMF_bm_spp_cum.csv")

# Now need toget an increment for each species, just like we did for the whole site

jenkins.spp.bm.inc <- data.frame(array(NA, dim=c(nrow(jenkins.spp.meter), length(jenkins.spp.meter))))
names(jenkins.spp.bm.inc) <- names(jenkins.spp.meter)
row.names(jenkins.spp.bm.inc) <- row.names(jenkins.spp.meter)
summary(jenkins.spp.bm.inc)

for(j in seq_along(jenkins.spp.bm.inc)){
  # inserting oldest biomass
  jenkins.spp.bm.inc[nrow(jenkins.spp.bm.inc),j] <- NA
  for(i in (length(jenkins.spp.bm.inc[,j])-1):1){
    jenkins.spp.bm.inc[i,j] <- jenkins.spp.meter[i,j] - jenkins.spp.meter[i+1,j] # subtracting the previous year's growth from DBH to get that year's DBH
  }
}
head(jenkins.spp.bm.inc)
summary(jenkins.spp.bm.inc)


write.csv(jenkins.spp.bm.inc[order(row.names(jenkins.spp.bm.inc), decreasing=F),order(names(jenkins.spp.bm.inc))], "MMF_bm_spp_inc.csv")
for(j in 2:ncol(vlf.bm.avg)){
par(new=T)
for(j in 1:ncol(jenkins.spp.bm.inc)){ 
  plot(jenkins.spp.bm.inc[,j] ~ row.names(jenkins.spp.bm.inc), type="l", col=rainbow(12), ylim= c(0,1) )
  par(new=T)
}
par(new=F)
plot(jenkins.site.bm.inc$MMF~ row.names(jenkins.site.bm.inc), type="l", col="red", lwd=2, ylim=c(0,1))

#Swizzle! Things are looking good.  we are now in kg of biomass per meter squared at the plot level.