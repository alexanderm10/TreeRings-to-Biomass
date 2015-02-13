library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


#################################################################################################
# Loading up .csv file that has meta data and RWL files for ring widths
# Also doing some housekeeping (unit conversions, name formats) up front to make the workflow smoother
#################################################################################################

#importing the diameter files of all trees sampled: includes tree id, spp, plot assignment, and DBH 
#loading the dplR to use the basal area reconstruction functions.

core.data <- read.csv("Core_data_DOE_summer_2014.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the site the trees belong to
names(core.data)
core.data$plot <- substr(core.data$plot.id, 3, 3)
core.data$plot <- as.factor(core.data$plot)

summary(core.data)

tree.data <- read.csv("Copy of DOE_field_notes_2014_updated_MMB spp.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the site the trees belong to
names(tree.data)
tree.data$plot <- substr(tree.data$PlotID, 3, 3)
tree.data$plot <- as.factor(tree.data$plot)

summary(tree.data)


#load in core details data sheet.  Has living/dead, pith info, measurement info.


#importing ring widths of dated samples as an object and making plot a factor since there were two distinct plots.  We may remove this for the nested design.  
#Removing NA's from the files
# FUTURE NOTE: need to create a generalizeable file to house all RW measurements
core.rw <- read.rwl("mmf_all_trees.rwl")
head(core.rw)
summary(core.rw)

#removing the extra character that tellervo adds
names(core.rw)<-substr(names(core.rw), 1, 7)
names(core.rw)

# NOTE: Unit Conversion Step
#we divide by 10 here because we are in mm going to cm (as long as you upload with dplR) 
summary(core.rw)
core.rw <- core.rw/10
summary(core.rw)



# ----------------------------------------------------------------------------
#add zeros to the outside if the tree is dead.  We do not want to generate modeled values for dead or zombie trees!
# Separating Dead vs. Missing rings
# What we need to know:
#1) Dead trees -- fill missing years with 0
#2) Live Trees, no growth in year (Zombie Trees) -- fill with 0
#3) Live Trees, missing part of core -- model growth

# CRR Note: This sets up what data gets gapfilled on the outside vs. which should have 0 growth in the most recent years

for(j in colnames(core.rw)){ # rather than going by number, we're using names to make things a bit clearer

	# If the core is a zombie or is dead, fill missing outer rings with 0s, otherwise it gets left alone
	# NOTE: We may need to add another level here if you have cores from multiple years so that things that were cored in a prior year also get 0s (so they don't get gapfilled outsides)
	if(!is.na(core.data[core.data$CoreID==j, "zombie"]) | core.data[core.data$CoreID==j, "live.dead"]=="DEAD"){ 
		last.meas <- as.numeric(core.data[core.data$CoreID==j, "outer.measured"]) # last ring which was mesured
	 	last.yr <- as.numeric(max(row.names(core.rw))) # oldest year in the data frame (this makes it flexible in case you add 2015 cores)
		if(!(last.meas = last.yr)){ # only do the 0 replacement if there are rows that need to be filled
			inner.fill <- which(row.names(core.rw)== last.meas+1)
			outer.fill <- which(row.names(core.rw)==last.yr)
			core.rw[inner.fill:outer.fill,j] <- 0
		}
	}
}
summary(core.rw)
# ----------------------------------------------------------------------------


##########################################################################
#AGGREGATION STEP
#taking ring widths and aggregating from the core level to the tree level
##########################################################################
# ----------------------------------------------------------------------------
# aggregate to the tree level using only dated trees where possible
trees <- unique(substr(names(core.rw), 1, 6)) # listing trees we have measurements for
tree.rw <- data.frame(array(NA, dim=c(nrow(core.rw), length(trees)))) # a blank data frame to put everything in
row.names(tree.rw) <- row.names(core.rw)  # labeling the rows with the years from our rwl
names(tree.rw)<-unique(substr(names(core.rw), 1, 6)) # labeling the columns as trees
# summary(tree.rw) # this will get really big very quickly
dim(tree.rw) # 90 trees, 112 years of data

# The Aggregation Loop
for(i in unique(trees)){
  cols <- which(substr(names(core.rw),1,6)==i) # getting the columns we're working with
  cores <- names(core.rw)[cols] # getting the name of the cores we're working with
  
  # -----------------------
  # if there's only one core, we just take that regardless of wheter it's dated or not
  if(length(cols) == 1){ 
   	tree.rw[,which(trees==i)] <- core.rw[,cols]

	# if that single core is dated, list the tree as dated ("Y"); if not, list as not ("N")
   	ifelse(core.data[core.data$CoreID==cores, "dated"]=="Y", tree.data[tree.data$TreeID==i, "Dated"] <- "Y", tree.data[tree.data$TreeID==i, "Dated"] <- "N")
	# Finding a (best-guess) pith date for the tree
   	tree.data[tree.data$TreeID==i, "Pith"] <- core.data[core.data$CoreID==cores, "pith.yr"]
  # -----------------------

  	} else { 

    # -----------------------
  	# if there's more than 1 core, we need to figure out which if any were dated
  	use <- vector(length=length(cols))
	for(x in 1:length(cols)){
		ifelse(core.data[core.data$CoreID==cores[x], "dated"]=="Y", use[x] <- "TRUE", use[x] <- "FALSE")
	 }	
    # -----------------------

    # -----------------------
	# now we know which were dated, so we can use that to figure out which cores to average
	if(length(use[use=="TRUE"])==1) { 
		# if only 1 core is dated, use only that core and call the tree dated
  	 	tree.rw[,which(trees==i)] <- core.rw[,cols[which(use=="TRUE")]]
  	 	tree.data[tree.data$TreeID==i, "Dated"] <- "Y"
   		tree.data[tree.data$TreeID==i, "Pith"] <- core.data[core.data$CoreID==cores[which(use=="TRUE")], "pith.yr"]

    # -----------------------

	} else if(length(use[use=="TRUE"])>1) { 
    # -----------------------
    # If there's greater than one dated core, take the mean of the dated cores and call the tree dated
		tree.rw[,which(trees==i)] <- rowMeans(core.rw[,cols[which(use=="TRUE")]], na.rm=T)
		tree.data[tree.data$TreeID==i, "Dated"] <- "Y"
   		tree.data[tree.data$TreeID==i, "Pith"] <- mean(core.data[core.data$CoreID==cores[which(use=="TRUE")], "pith.yr"], na.rm=T)

	} else { 
    # -----------------------
    # If no cores are dated, take the mean of whatever we have and call the tree undated
		tree.rw[,which(trees==i)] <- rowMeans(core.rw[,cols], na.rm=T) 
		tree.data[tree.data$TreeID==i, "Dated"] <- "N"
   		tree.data[tree.data$TreeID==i, "Pith"] <- mean(core.data[core.data$CoreID==cores, "pith.yr"], na.rm=T)
		
    # -----------------------
	}
  }
}
# Note: There are some warnings, but I think it's okay

# summary(tree.rw)
min(tree.rw, na.rm=T); max(tree.rw, na.rm=T)
dim(tree.rw)

# We've updated the tree.data file, so lets save our changes before we move any further
# We only added a new column and didn't change anything that was original, so it should be okay, but lets just double check before moving forward
tree.data$Dated <- as.factor(tree.data$Dated)
summary(tree.data)
# NOTE: right now you have a ridculously long name for your tree data spreadsheet, so I'm going to call it something different for my own sanity right now :-P
write.csv(tree.data, "TreeData.csv", row.names=F)

# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# CRR: writing our tree RWL to a csv formatted for gap filling before we calculate BAI (bc some have missing outsides and that's bad if we're working outside-in in out calculations)

# stacking the RWL so that we have a single column of ring widths & an identifying column with TreeID
tree.stack <- stack(tree.rw)
names(tree.stack) <- c("RW", "TreeID")
tree.stack$Year <- as.numeric(row.names(tree.rw)) # adding in the years
summary(tree.stack)

# attaching all of our useful tree data
summary(tree.data)
tree.stack <- merge(tree.stack, tree.data, all.x=T, all.y=F)
summary(tree.stack)
dim(tree.stack)

write.csv(tree.stack, "TreeRWL_stacked.csv", row.names=F)


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# I think this next block can get deleted
# ----------------------------------------------------------------------------
# Subsetting only the sites & species I have full data for right now
#sites <- unique(establishment$Site)
#sites

#species <- unique(establishment$Spp)
#species

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
# ----------------------------------------------------------------------------

#making dataframe of tree diameter
tree.deets <- data.frame(names(tree.rw)) 
names(tree.deets) <- "ID"
summary(tree.deets)

names(core.data)

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