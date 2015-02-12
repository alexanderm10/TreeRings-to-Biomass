# looking at which cores for which we have measurements
# FOR CHRISTY: what about the cores where we don't have any data?  Will that be added later?
cores.meas <- as.vector(colnames(core.rw))
cores.meas[1:10] # checking names
length(cores.meas) # checking size
#this is looking at which trees we have measurements for (1,6) translates to AAA000 six characters


#This is commented out becasue we have a dated column in our core.data csv file
# Making a binary column of whether the tree was dated or not
#for(i in 1:length(diam.data$TreeID)){
#  diam.data$Dated[i] <- ifelse(diam.data$TreeID[i] %in% mmf.id2, "YES", "NO")
#}
#diam.data$Dated <- as.factor(diam.data$Dated) # making a factor (because sometimes goes weird)
#diam.data$Spp.Dated <- as.factor(paste(diam.data$spp, diam.data$Dated, sep=".")) # don't worry about this (something I"m playing with)
#summary(diam.data)

names(core.data)
summary(core.data)
core.deets <- data.frame(names(core.rw)) 
names(core.deets) <- "ID"
summary(core.deets)

names(core.data)

# Associating dbh with coreID
for(i in unique(core.deets$ID)){
  core.deets[core.deets$ID==i, "DBH"] <- core.data[core.data$CoreID==i,"dbh"]
}
summary(core.deets)



#subsetting the DBH list to match what cores were actually dated
#core.names <- names(all.dated2)

#ID2 <- ID[ID$ID %in% core.names,]

#order both Core details object we just made, and the uploaded TR measurements
core.deets <- core.deets[order(core.deets$ID),]
head(core.deets)
#Flipping the dataset so that it has the most recent year at the top
core.rw <- core.rw[order(row.names(core.rw), decreasing=T),order(names(core.rw))]
head(core.rw)
ncol(core.rw)

#order both tree details object we just made, and the uploaded TR measurements
tree.deets <- tree.deets[order(tree.deets$ID),]
head(tree.deets)
#Flipping the dataset so that it has the most recent year at the top
tree.rw <- tree.rw[order(row.names(tree.rw), decreasing=T),order(names(tree.rw))]
head(tree.rw)
ncol(tree.rw)

# ----------------------------------------------------------------------------

#making dataframe of tree diameter
tree.deets <- data.frame(names(tree.rw)) 
names(tree.deets) <- "ID"
summary(tree.deets)

names(core.data)

# Getting tree level data
for(i in unique(tree.deets$ID)){
  tree.deets[tree.deets$ID==i, "DBH"] <- tree.data[tree.data$TreeID==i,"dbh"]
}
summary(tree.deets)


#diameter reconstructions of each tree from the trees that dated??
dbh.recon <- tree.rw
summary(dbh.recon)
summary(tree.deets)
for(j in seq_along(dbh.recon)){
  # inserting 2012 DBH
  dbh.recon[1,j] <- tree.deets[tree.deets$ID==names(dbh.recon[j]),"DBH"] 
  for(i in 2:(length(dbh.recon[,j]))){
    dbh.recon[i,j] <- ifelse(!is.na(tree.rw[i,j]), dbh.recon[i-1,j] - tree.rw[i-1,j]*2, tree.rw[i,j]*2) # subtracting the previous year's growth from DBH to get that year's DBH
  }
}
summary(dbh.recon)
min(dbh.recon, na.rm=T)


#quick plot
spag.plot(dbh.recon)
plot.rwl(dbh.recon)



#checking for negative diameters that will need to be removed or switch to inside out orientation (call Christy)
#RA: WE NOW HAVE A NEGATIVE NUMBER IN THE DIAMETER RECONSTRCUTIONS

min(dbh.recon, na.rm=T)
summary(dbh.recon)
write.csv(dbh.recon, "DOE_dbh_recon.csv")

##########################################################################
#using ring widths to caclulate a Basal Area Increment (BAI)
# FOR CHRISTY: IS THIS WHERE THIS SHOULD GO?
##########################################################################

tree.bai.out <- bai.out(tree.rw, diam=tree.deets)
summary(tree.bai.out)
min(tree.bai.out, na.rm=T)

# FOR CHRISTY: I think I have things working.  Would you mind doind a quick double check over these scripts before you use them to do the next step?