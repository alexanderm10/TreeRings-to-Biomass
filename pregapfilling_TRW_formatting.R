library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


#################################################################################################
# Loading up .csv file that has meta data and RWL files for ring widths
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



#here we are telling the program that NA's in the most recent time are actual zero's.  Either the tree has not grown yet or it has been standing dead.  
#The issue is that if we really do get a tree that has been standing dead or not put on radial growth in the past 15 years or so it will mess with the value
#will work for the MMF samples, need to double check all the measured trees when we get done
for(j in 1:ncol(core.rw)){
  for(i in which(row.names(core.rw)=="2000"):which(row.names(core.rw)=="2014"))
    core.rw[i,j] <- ifelse(is.na(core.rw[i,j]), 0, core.rw[i,j])
}


# NOTE: Unit Conversion Step
#we divide by 10 here because we are in mm going to cm (as long as you upload with dplR) 
summary(core.rw)
core.rw <- core.rw/10
summary(core.rw)

#add zeros to the outside if the tree is dead.  We do not want to generate modeled values for dead trees! Do this later.
# Separating Dead vs. Missing rings
# What we need to know:
#1) Dead trees -- fill missing years with 0
#2) Live Trees, no growth in year (Zombie Trees) -- fill with 0
#3) Live Trees, missing part of core -- model growth


#removing the extra character that tellervo adds
names(core.rw)<-substr(names(core.rw), 1, 7)
head(core.rw)
# Subsetting only the sites & species I have full data for right now
#sites <- unique(establishment$Site)
#sites

#species <- unique(establishment$Spp)
#species




# looking at which cores for which we have measurements
# FOR CHRISTY: what about the cores where we don't have any data?  Will that be added later?
mmf.id <- as.vector(colnames(core.rw))
mmf.id[1:10] # checking names
length(mmf.id) # checking size
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

##########################################################################
#AGGREGATION STEP
#taking ring widths and aggregating from the core level to the tree level
##########################################################################
#aggregate to the tree level
#in the future we need to add a hierarchical component so that if a core dated then it trumps a non-dated core
trees <- unique(substr(names(core.rw), 1, 6)) #Dr. Christy, PhD in being Ninja is smart
tree.rw <- data.frame(array(NA, dim=c(nrow(core.rw), length(trees))))
row.names(tree.rw) <- row.names(core.rw)  #CRR Added
names(tree.rw)<-unique(substr(names(core.rw), 1, 6))
summary(tree.rw)
ncol(tree.rw)
for(i in unique(trees)){
  cols <- which(substr(names(core.rw),1,6)==i)
  ifelse(length(cols) > 1, tree.rw[,which(trees==i)] <- rowMeans(core.rw[,cols], na.rm=T), tree.rw[,which(trees==i)] <- core.rw[,cols])
}
summary(tree.rw)
min(tree.rw, na.rm=T)
ncol(tree.rw)

# FOR CHRISTY: Could you please check this to be sure that I have done the aggregation correctly?

#making dataframe of tree diameter
tree.deets <- data.frame(names(tree.rw)) 
names(tree.deets) <- "ID"
summary(tree.deets)

names(core.data)

for(i in unique(tree.deets$ID)){
  tree.deets[tree.deets$ID==i, "DBH"] <- tree.data[tree.data$TreeID==i,"dbh"]
}
summary(tree.deets)

# FOR CHRISTY: Could you please check this to be sure that I have done the aggregation correctly?


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