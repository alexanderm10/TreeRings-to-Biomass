library(dplR)
library(ggplot2)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


#################################################################################################
# Graphing Size class distributions of species & dated stems
#################################################################################################

#importing the diameter files of all trees sampled: includes tree id, spp, plot assignment, and DBH 
#loading the dplR to use the basal area reconstruction functions.

core.data <- read.csv("Core_data_DOE_summer_2014.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)
#adding a column include which plot at the site the trees belong to
core.data$plot <- substr(core.data$PlotID, 3, 3)
core.data$plot <- as.factor(core.data$plot)

summary(core.data)

#load in core details data sheet.  Has living/dead, pith info, measurement info.


#importing ring widths of dated samples as an object and making plot a factor since there were two distinct plots.  We may remove this for the nested design.  
#Removing NA's from the files
mmf.tr <- read.rwl("mmf_all_trees.rwl")
head(mmf.tr)
summary(mmf.tr)

#we divide by 10 here because we are in mm going to cm (as long as you upload with dplR) 
summary(mmf.tr)
mmf.tr <- mmf.tr/10
summary(mmf.tr)


#here we are telling the program that NA's in the most recent time are actual zero's.  Either the tree has not grown yet or it has been standing dead.  
#The issue is that if we really do get a tree that has been standing dead or not put on radial growth in the past 15 years or so it will mess with the value
#will work for the MMF samples, need to double check all the measured trees when we get done
for(j in 1:ncol(mmf.tr)){
  for(i in which(row.names(mmf.tr)=="2000"):which(row.names(mmf.tr)=="2014"))
    mmf.tr[i,j] <- ifelse(is.na(mmf.tr[i,j]), 0, mmf.tr[i,j])
}



#add zeros to the outside if the tree is dead.  We do not want to generate modeled values for dead trees! Do this later.
# Separating Dead vs. Missing rings
# What we need to know:
#1) Dead trees -- fill missing years with 0
#2) Live Trees, no growth in year (Zombie Trees) -- fill with 0
#3) Live Trees, missing part of core -- model growth


#removing the extra character that tellervo adds
names(mmf.tr)<-substr(names(mmf.tr), 1, 7)
head(mmf.tr)

# Subsetting only the sites & species I have full data for right now
#sites <- unique(establishment$Site)
#sites

#species <- unique(establishment$Spp)
#species

############
# subsetting trees belonging to completed sites
#tree.data2 <- tree.data[tree.data$Site %in% sites,]
#summary(tree.data2)
#dim(tree.data)
#dim(tree.data2)

# making a vector of trees that were dated (have stablishment)
#tree.id <- as.vector(unique(establishment$TreeID))
#tree.id[1:10]
#length(tree.id)

# Alternative approach depending on your data
# looking at which cores we have measurements for
mmf.id <- as.vector(colnames(mmf.tr))
mmf.id[1:10] # checking names
length(mmf.id) # checking size
#this is looking at which trees we have measurements for (1,6) translates to AAA000 six characters
mmf.id2<- unique(substr(mmf.id,1,6))
mmf.id2[1:10]

#This is commented out becasue we have a dated column in our core.data csv file
# Making a binary column of whether the tree was dated or not
#for(i in 1:length(diam.data$TreeID)){
#	diam.data$Dated[i] <- ifelse(diam.data$TreeID[i] %in% mmf.id2, "YES", "NO")
#}
#diam.data$Dated <- as.factor(diam.data$Dated) # making a factor (because sometimes goes weird)
#diam.data$Spp.Dated <- as.factor(paste(diam.data$spp, diam.data$Dated, sep=".")) # don't worry about this (something I"m playing with)
#summary(diam.data)



###########
#I don't know if we need this anymore
# merging sites together into one file
#we might not have to do this for the nested sampling design, I could put the measurements and trees from the whole site into one file
#we would just have to change the plot ID to reflext either the A, B, or C plots.  Depending on how the data is structured.
#summary(vlf.data)
#summary(vuf.data)

#rbind combines objets by either rows or columns.
#We have two separate object here and we have merged them into one new object
#all.valles <- rbind(vlf.data,vuf.data)
#all.valles$site <- as.factor(substr(all.valles$id,1,3))
#summary(all.valles)

#CHRISTY NEEDS TO EXPLAIN THIS...NOT SURE WHAT'S GOING ON.
#all.valles$bin.dated <- as.factor(paste(all.valles$bin2, all.valles$Dated, sep=".")) # don't worry about this (something I"m playing with)
#summary(all.valles)
#summary(all.valles$bin.dated)
 

#should we remove this section about the group colors?
############
# Reading in a file that has species names and colors
#group.col <- read.csv("GroupColors.csv")
#summary(group.col)

#what is going on here?  We have set a sequence from 0 - max DBH, but what does the 2 mean?  Are we doing this by years or cm?  I think we just have some annotation issues here.
# making bins for your distribution
head(core.data)
dbh.bins1 <- seq(10, max(core.data$dbh[core.data$site == "Morgan Monroe State Park"], na.rm=T), 5) # 5 cm bins based on the range of your trees
#dbh.bins2 <- c(seq(0, 40, 5), Inf) # 5 year bins that stop at 40 cm

#do we need this?

# making subsetting species from master lsit that are actually in the data (gets off otherwise)
#spp.list.tree <- unique(tree.data2[!tree.data2$Site=="IRN", "Spp"])
#spp.col.tree <- spp.col[spp.col$Spp %in% spp.list.tree,]
#length(spp.list.tree)
#dim(spp.col.tree)

#for the nested sampling design we might want to diagram how much dead stuff we have.  I think this would be easy enough to do, since it is already included in our fieldnotes.
# Plotting species by size distribution
qplot(x=dbh, data=diam.data[diam.data$site == "Morgan Monroe State Park",], geom="histogram", breaks=dbh.bins1, fill=spp) + facet_grid(site ~ .) + 
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + 
  scale_x_continuous(name="DBH") + ggtitle("Size Distribution") #+ scale_fill_manual(values=as.vector(spp.col.tree$Color))

# Plotting species by Dated or Not
#right now this is just for plot B at MMF
qplot(x=dbh, data=core.data[core.data$site == "Morgan Monroe State Park" ,], geom="histogram", breaks=dbh.bins1, fill=dated) + facet_grid(site ~ .) + 
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + 
  scale_x_continuous(name="DBH") + ggtitle("Size Distribution") + scale_fill_manual(values=c("gray80", "blue"))
  poster.theme

#I think this is stuff for Christy's work.  Can we remove it?
# Plotting species by Dated or Not, removing saplings
#qplot(x=DBH, data=tree.data2[!tree.data2$Site=="IRN" & tree.data2$DBH>5,], geom="histogram", breaks=dbh.bins1, fill=Dated) + facet_grid(Site ~ .) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution") + scale_fill_manual(values=c("gray80", "gray30"))

# Plotting species by Dated or Not, removing saplings & dead stuff
#qplot(x=DBH, data=tree.data2[!tree.data2$Site=="IRN" & tree.data2$DBH>5 & tree.data2$Live=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Dated) + facet_grid(Site ~ .) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution") + scale_fill_manual(values=c("gray80", "gray30"))

#I'm not sure what this is doing.  Breaking things down into bins, but I don't know what we are cutting.
#calculating the percent dated vs. undated in each bin of histogram
#bins.2 <- seq(from=0, to=60, by=2) #2 indicates the length of the bin, in this case 2cm 
core.data$bin <- cut(core.data$dbh, breaks=c(dbh.bins1))#we need to look into this

summary(core.data)

#what are we substringing and what is it doing.
#all.valles$bin2b <- substr(paste(all.valles$bin2),2,10)

#This is where I start to get lost.  Not quite sure waht we are doing here.  Is this where we have figured out dating percentages by diameter class?
#we might relable it as something different than test
#we can come back to this after NACP
#l1 <- length(unique(all.valles$site))*length(unique(all.valles$bin2))
#test <- as.data.frame(vector(length=l1))
#test[,1] <- unique(all.valles$site)
#test <- as.data.frame(test[sort(test[,1]),])
#test[,2] <- unique(paste(all.valles$bin2))
#names(test) <- c("site", "bin2")
#for(i in unique(all.valles$site)){
#  for(j in unique(all.valles$bin2)){
#    test[test$bin2==j & test$site==i,"perc.dated"] <- length(all.valles[all.valles$bin2==j & all.valles$site==i & all.valles$Dated=="YES","Dated"])/length(all.valles[all.valles$bin2==j & all.valles$site==i,"Dated"])
#  }
#}

#summary(test)
#print(test)

library(reshape2)

#fancy.table1 <- melt(test, na.rm=F)

#fancy.table2 <- dcast(fancy.table1, bin2~site)

#why did we write this as it's own special CSV?
#write.csv(fancy.table2, "valles_perc_dated_bins.csv")

###################################
#biomass calculations
#merge tree rings from the individual towers from VLF and VUF into one object

#vlf.dated2 <- vlf.dated
#vlf.dated2$year <- row.names(vlf.dated)

#vuf.dated2 <- vuf.dated
#vuf.dated2$year <- row.names(vuf.dated)

#all.dated <- merge(vlf.dated2, vuf.dated2, all.x=T, all.y=T)
#all.dated[all.dated == 0] <- NA
#row.names(all.dated) <- all.dated$year
#converting the Tsap measurements (currently in 1/100mm) to cm to match the DBH file we will use later


#again why the separate file?  
#write.csv(all.dated2, "all.dated.check.csv")



names(core.data)
core.deets <- data.frame(names(mmf.tr)) 
names(core.deets) <- "ID"
summary(core.deets)

names(core.data)

for(i in unique(core.deets$ID)){
  core.deets[core.deets$ID==i, "DBH"] <- core.data[core.data$core.id==i,"dbh"]
}
summary(core.deets)

#subsetting the DBH list to match what cores were actually dated
#core.names <- names(all.dated2)

#ID2 <- ID[ID$ID %in% core.names,]

#order both Core details object we just made, and the uploaded TR measurements
core.deets <- core.deets[order(core.deets$ID),]
#Flipping the dataset so that it has the most recent year at the top
mmf.tr <- mmf.tr[order(row.names(mmf.tr), decreasing=T),order(names(mmf.tr))]
head(mmf.tr)

###########################################################
#this is where we need to jump to the gap filling part
###########################################################

#diameter reconstructions of each tree from the cores that dated??
dbh.recon <- mmf.tr
summary(dbh.recon)
for(j in seq_along(dbh.recon)){
  # inserting 2012 DBH
  dbh.recon[1,j] <- core.deets[core.deets$ID==names(dbh.recon[j]),"DBH"] 
  for(i in 2:(length(dbh.recon[,j]))){
    dbh.recon[i,j] <- ifelse(!is.na(mmf.tr[i,j]), dbh.recon[i-1,j] - mmf.tr[i-1,j]*2, mmf.tr[i,j]*2) # subtracting the previous year's growth from DBH to get that year's DBH
  }
}
summary(dbh.recon)




#checking for negative diameters that will need to be removed or switch to inside out orientation (call Christy)

min(dbh.recon, na.rm=T)
summary(dbh.recon)
write.csv(dbh.recon, "DOE_dbh_recon.csv")


#aggregate to the tree level
#in the future we need to add a hierarchical component so that if a core dated then it trumps a non-dated core
trees <- unique(substr(names(dbh.recon), 1, 6)) #Dr. Christy is smart
diam.tree <- data.frame(array(NA, dim=c(nrow(dbh.recon), length(trees))))
row.names(diam.tree) <- row.names(dbh.recon)  #CRR Added
names(diam.tree)<-unique(substr(names(dbh.recon), 1, 6))
for(i in unique(trees)){
  cols <- which(substr(names(bai.density),1,6)==i)
  ifelse(length(cols) > 1, diam.tree[,which(trees==i)] <- rowMeans(dbh.recon[,cols], na.rm=T), diam.tree[,which(trees==i)] <- dbh.recon[,cols])
}
summary(diam.tree)

# Now the format is diameter per tree
#need to convert to biomass
###########################################################
#Biomass Conversions
###########################################################
#Convert to biomass with the allometric equation
#using generalized jenkins format from the paper
jenkins.general <- function(dbh, a, b){
  exp(a + b*log(dbh))
}

mmf.jenkins.recon <- jenkins.general(diam.tree, -2.0127, 2.4342)
summary(mmf.jenkins.recon)
#now in kilograms of biomass per tree

#need to incorporate the density into things
jenkins.bm.density <- data.frame(array(NA, dim=c(nrow(mmf.jenkins.recon), ncol(mmf.jenkins.recon))))
row.names(jenkins.bm.density) <- row.names(mmf.jenkins.recon)  #CRR Added
names(jenkins.bm.density)<- names(mmf.jenkins.recon)

for(i in unique(names(mmf.jenkins.recon))){
    jenkins.bm.density[,i] <- mmf.jenkins.recon[,i]*diam.data[diam.data$TreeID==i,"density"]
}
summary(jenkins.bm.density)
#we are now in kg of biomass per Ha
jenkins.bm.density.meter <- jenkins.bm.density/10000
summary(jenkins.bm.density.meter)
#now we are in kg of biomass per meter squared

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
plot(jenkins.plot.meter$MMB~row.names(jenkins.plot.meter), type="l")
#Swizzle! Things are looking good.  we are now in kg of biomass per meter squared at the plot level.

write.csv(jenkins.plot.meter, "mmf_biomass_kgm2.csv")




#creating the basal area increment reconstruction
cores.bai.out <- bai.out(mmf.tr2, diam=core.deets)
summary(cores.bai.out)

plot(cores.bai.out[,1]~row.names(cores.bai.out), type="l")


#I think we need to aggregate up to the tree level before we do this.  Not quite sure how we did this in the past.


#Basal Area Increment for the plot summed across the whole plot.  We will not be able to do this when we have a whole suite of sites.  Needs to be changed
#use the code we used to aggregate up to the tree to aggregate up to the plot level here.  Lines 287-290.
bai.plot <- data.frame(row.names(cores.bai.out))
names(bai.plot) <- ("Year")


bai.plot$A <- rowSums(cores.bai.out[,substr(names(cores.bai.out), 3, 3) == "A"], na.rm=T)
bai.plot$B <- rowSums(cores.bai.out[,substr(names(cores.bai.out), 3, 3) == "B"], na.rm=T)
bai.plot$C <- rowSums(cores.bai.out[,substr(names(cores.bai.out), 3, 3) == "C"], na.rm=T)

summary(bai.plot)
#making year a numeric thing
bai.plot$Year <- as.numeric(paste(bai.plot$Year))
plot(B ~ Year, data=bai.plot, type="l")

#adding in the density component to put things on a per hectare basis
which(names(cores.bai.out)=="MMB001A")
which(substr(names(cores.bai.out),1,6)=="MMB001")

trees <- unique(substr(names(cores.bai.out), 1, 6)) #Dr. Christy is smart
bai.density <- cores.bai.out

for(i in unique(trees)){
  cols <- which(substr(names(cores.bai.out),1,6)==i)
  bai.density[,cols] <- cores.bai.out[,cols]*diam.data[diam.data$TreeID==i,"density"]
}
summary(bai.density)
#now BAI is in a per HECTARE format!
#now aggregate the BAI calculations up from the core level to the tree level

bai.tree.ha <- data.frame(array(NA, dim=c(nrow(cores.bai.out), length(trees))))
names(bai.tree.ha) <- trees

for(i in unique(trees)){
  cols <- which(substr(names(bai.density),1,6)==i)
  ifelse(length(cols) > 1, bai.tree.ha[,which(trees==i)] <- rowMeans(bai.density[,cols], na.rm=T), bai.tree.ha[,which(trees==i)] <- bai.density[,cols])
}

summary(bai.tree.ha)
write.csv(bai.tree.ha, "bai.tree.ha.csv")

#aggregating the diameter reconstruction up to the tree level

dbh.tree <- data.frame(array(NA, dim=c(nrow(dbh.recon), length(trees))))
names(dbh.tree)<-trees

for(i in unique(trees)){
  cols <- which(substr(names(dbh.recon),1,6)==i)
  ifelse(length(cols) > 1, dbh.tree[,which(trees==i)] <- rowMeans(dbh.recon[,cols], na.rm=T), dbh.tree[,which(trees==i)] <- dbh.recon[,cols])
}
summary(dbh.tree)

#calculating the biomass for the current year.  In the non Valles stuff it is 2014.  In the Valles Stuff it is 2012 and in Niwot it is 2013
#calc the biomass for 2012 from the measured dbh and making points for each model type
mmf.current<- diam.data[substr(diam.data$TreeID, 1, 2)=="MM",]
summary(mmf.current)

#using just the general hardwood equation from the jenkins paper for now.
mmf.current.bm<-mmf.current[, c("TreeID", "spp", "canopy.class", "live.dead", "dbh")]
mmf.current.bm$current.bm<- jenkins.general(mmf.current$dbh, -2.0127, 2.4342)
summary(mmf.current.bm)

#need to put in the graphing scripts at some point.  Show graphs before and afte the gapfilling tries to show the differences between what was dated and the gapfilled

##############################################
#gapfilling try
#going to use a mixed model
#christy helped me with this step
#need to gapfill to the pith first using the pith offsets

summary(core.data)
summary(diam.data)

vlf.bin.dated <- all.valles$bin.dated[substr(all.valles$id,1,3)=="VLF"]
summary(vlf.bin.dated)

vuf.bin.dated <- all.valles$bin.dated[substr(all.valles$id,1,3)=="VUF"]
summary(vuf.bin.dated)


#Randomly choosing samples from the different diameter classes to insert in teh diameter classes.  We need to think up a better way to do this.
#last time I used this to identify the random samples to be used, but I had to insert the measurements in excel.
vlf.ten <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIPO" & all.valles$bin2=="(10,12]" & all.valles$Dated=="YES" ], 3, replace=T ))
vlf.twelve <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIPO" & all.valles$bin2=="(12,14]" & all.valles$Dated=="YES" ], 3, replace=T ))
vlf.fourteen <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIPO" & all.valles$bin2=="(14,16]" & all.valles$Dated=="YES"], 3, replace=T )) 
vlf.sixteen <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIPO" & all.valles$bin2=="(16,18]" & all.valles$Dated=="YES"], 3, replace=T )) 
vlf.twenty <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIPO" & all.valles$bin2=="(20,22]" & all.valles$Dated=="YES"], 1, replace=T )) 
vlf.twentyfour <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIPO" & all.valles$bin2=="(22,24]" & all.valles$Dated=="YES"], 1, replace=T )) 

vuf.six <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIEN" & all.valles$bin2=="(6,8]" & all.valles$Dated=="YES"], 1, replace=T )) 
vuf.eight <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIEN" & all.valles$bin2=="(8,10]" & all.valles$Dated=="YES"], 4, replace=T )) 
vuf.ten <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIEN" & all.valles$bin2=="(10,12]" & all.valles$Dated=="YES"], 5, replace=T )) 
vuf.twelve <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIEN" & all.valles$bin2=="(12,14]" & all.valles$Dated=="YES"], 1, replace=T )) 
vuf.eighteen <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIEN" & all.valles$bin2=="(18,20]" & all.valles$Dated=="YES"], 1, replace=T )) 
vuf.twenty <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIEN" & all.valles$bin2=="(20,22]" & all.valles$Dated=="YES"], 1, replace=T )) 
vuf.twentytwo <- as.data.frame(sample(all.valles$id[all.valles$spp=="PIEN" & all.valles$bin2=="(22,24]" & all.valles$Dated=="YES"], 1, replace=T )) 

write.csv(dbh.recon.vlf, "dbh.recon.vlf2.csv")
write.csv(dbh.recon.vuf, "dbh.recon.vuf2.csv")

#this is the file containing the replicated measurements
dbh.recon.vlf2<-read.csv("dbh.recon.vlf3.csv", header=T)
dbh.recon.vuf2<-read.csv("dbh.recon.vuf3.csv", header=T)

#stacking for the lower site
summary(dbh.recon.vlf2)
row.names(dbh.recon.vlf2)<-dbh.recon.vlf2$year

dbh.recon.vlf.stack2 <- stack(dbh.recon.vlf2[,2:ncol(dbh.recon.vlf2)])
summary(dbh.recon.vlf.stack2)
names(dbh.recon.vlf.stack2)<-c("dbh", "id")

dbh.recon.vlf.stack2$year <- as.numeric(row.names(dbh.recon.vlf))
dbh.recon.vlf.stack2$tree<- as.factor(substr(dbh.recon.vlf.stack2$id,1,6))
summary(dbh.recon.vlf.stack2)

#stacking for the upper site
summary(dbh.recon.vuf2)
row.names(dbh.recon.vuf2)<-dbh.recon.vuf2$year
dbh.recon.vuf.stack2 <- stack(dbh.recon.vuf2[,2:ncol(dbh.recon.vuf2)])

summary(dbh.recon.vuf.stack2)
names(dbh.recon.vuf.stack2)<-c("dbh", "id")

dbh.recon.vuf.stack2$year <- as.numeric(row.names(dbh.recon.vuf))
dbh.recon.vuf.stack2$tree<- as.factor(substr(dbh.recon.vuf.stack2$id,1,6))
summary(dbh.recon.vuf.stack2)
summary(dbh.recon.vuf.stack)

#not really sure what happened here.  Just two lines form.  Weird.  I think we were checking to see if there were any visual weirdness in the analysis up to this point.
par(new=F)
for(i in unique(dbh.recon.vuf.stack2$tree)){
  plot(dbh.recon.vuf.stack2[dbh.recon.vuf.stack2$tree==i, "dbh"] ~ dbh.recon.vuf.stack2[dbh.recon.vuf.stack2$tree==i, "year"], 
       xlim=range(dbh.recon.vuf.stack2$year, na.rm=T), ylim=range(dbh.recon.vuf.stack2$dbh, na.rm=T), lwd=.75, type="l", xlab="year", ylab="dbh")
  par(new=F)
}

par(new=F)
for(i in unique(dbh.recon.vlf.stack2$tree)){
  plot(dbh.recon.vlf.stack2[dbh.recon.vlf.stack2$tree==i, "dbh"] ~ dbh.recon.vlf.stack2[dbh.recon.vlf.stack2$tree==i, "year"], 
       xlim=range(dbh.recon.vlf.stack2$year, na.rm=T), ylim=range(dbh.recon.vlf.stack2$dbh, na.rm=T), lwd=.75, type="l", xlab="year", ylab="dbh")
  par(new=T)
}

#attempting MCMC with 2012
library(boot)
DBH <- all.valles$dbh[all.valles$spp=="PIPO"]
length(all.valles$dbh)
summary(all.valles$dbh)
#DBH <- rnorm(50, mean=10, sd=2)
#DBH

#using the mean and SD for the different parameters of hte allometric equation.  I forget which one was used, but I have a file of them somewhere.  I think this one is Tyson's.
beta0 <- rnorm(200, mean=0.054, sd=0.004)
beta1 <- rnorm(200, mean=2.651, sd=0.0175)
summary(beta0)
summary(beta1)

#CHRISTY!! what did I do here?  I cannot remember for the life of me.
test1 <- rep(NA,10000)
for(i in 1:10000){test1[i] <- mean(sample(beta0, size=1, replace=T)*DBH^sample(beta1, size=1, replace=T))}

plot(density(test1))
summary(test1)
sd(test1)
1.96*sd(test1)

#plotted the mean and SD of the mini bootstrapping thing I just did above.  I think I wanted to see if the data would come out 
#even slightly normal looking.
plot(density(test1))
abline(v=mean(test1), col="red") 
abline(v=mean(test1)+1.96*sd(test1), lty="dashed", col="red")
abline(v=mean(test1)-1.96*sd(test1), lty="dashed", col="red")
#this looks like it worked

#################################################################
#Getting average DBH at each site per year
#need to make a table or list or somethign of the files that we are using as the source data.
dbh.recon.vlf.tree <- aggregate(dbh.recon.vlf.stack2$dbh, by=list(dbh.recon.vlf.stack2$tree, dbh.recon.vlf.stack2$year), FUN="mean", na.rm=T)
names(dbh.recon.vlf.tree)<-c("tree", "year", "dbh")
summary(dbh.recon.vlf.tree)

vlf.dbh.year <- aggregate(dbh.recon.vlf.tree$dbh, by=list(dbh.recon.vlf.tree$year), FUN="mean", na.rm=T)
names(vlf.dbh.year)<-c("year", "dbh")

dbh.recon.vuf.tree <- aggregate(dbh.recon.vuf.stack2$dbh, by=list(dbh.recon.vuf.stack2$tree, dbh.recon.vuf.stack2$year), FUN="mean", na.rm=T)
names(dbh.recon.vuf.tree)<-c("tree", "year", "dbh")
vuf.dbh.year <- aggregate(dbh.recon.vuf.tree$dbh, by=list(dbh.recon.vuf.tree$year), FUN="mean", na.rm=T)
names(vuf.dbh.year)<-c("year", "dbh")

##################################
#generating bootstraps for each allometric equation

#year <- all.valles$Year
vlf.year <- unique(dbh.recon.vlf.tree$year)
vlf.year <- data.frame(vlf.year)
names(vlf.year)<- c("year")
# Navar Pipo
beta0 <- rnorm(1000, mean=0.054, sd=0.004)
beta1 <- rnorm(1000, mean=2.651, sd=0.0175)
summary(beta0)
summary(beta1)

for(j in unique(dbh.recon.vlf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vlf.tree[!is.na(dbh.recon.vlf.tree$dbh) & dbh.recon.vlf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$pipo.mean"] <- mean(test, na.rm=T)
  vlf.year[vlf.year$year==j, "n.t$pipo.sd"] <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$pipo.se"]<- se(test) 
}

summary(vlf.year)
class(vlf.year)
plot(n.t$pipo.mean~year, data=vlf.year, type="l", lwd=2)

# current Navar Pipo
beta0 <- rnorm(1000, mean=0.054, sd=0.004)
beta1 <- rnorm(1000, mean=2.651, sd=0.0175)


DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.pipo.mean <- mean(test, na.rm=T)
current.nt.pipo.sd <- sd(test)
current.nt.pipo.se <-se(test)

#######################
#n/t$piaz
beta0 <- 0.0527
beta1 <- rnorm(1000, mean=2.5569, sd=0.0275)

for(j in unique(dbh.recon.vlf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vlf.tree[!is.na(dbh.recon.vlf.tree$dbh) & dbh.recon.vlf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(beta0*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$piaz.mean"] <- mean(test, na.rm=T)
  vlf.year[vlf.year$year==j, "n.t$piaz.sd"] <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$piaz.se"] <- se(test)

}

summary(vlf.year)
# current n/t$piaz
beta0 <- 0.0527
beta1 <- rnorm(1000, mean=2.5569, sd=0.0275)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.piaz.mean <- mean(test, na.rm=T)
current.nt.piaz.sd <- sd(test)
current.nt.piaz.se <- se(test)

#######################
#n/t$pine.spp
beta0 <- 0.0597
beta1 <- rnorm(1000, mean=2.5741, sd=0.013)

for(j in unique(dbh.recon.vlf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vlf.tree[!is.na(dbh.recon.vlf.tree$dbh) & dbh.recon.vlf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(beta0*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$pine.spp.mean"] <- mean(test, na.rm=T)
  vlf.year[vlf.year$year==j, "n.t$pine.spp.sd"] <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$pine.spp.se"] <- se(test)
}

summary(vlf.year)

# current n/t$pine.spp
beta0 <- 0.0597
beta1 <- rnorm(1000, mean=2.5741, sd=0.013)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.pine.mean <- mean(test, na.rm=T)
current.nt.pine.sd <- sd(test)
current.nt.pine.se <- se(test)
#######################
#n/t$vcnp
beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

for(j in unique(dbh.recon.vlf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vlf.tree[!is.na(dbh.recon.vlf.tree$dbh) & dbh.recon.vlf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$vcnp.mean"] <- mean(test, na.rm=T)
  vlf.year[vlf.year$year==j, "n.t$vcnp.sd"] <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$vcnp.se"] <- se(test)
}

summary(vlf.year)

#current n/t$vcnp
beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.vcnp.mean <- mean(test, na.rm=T)
current.nt.vcnp.sd <- sd(test)
current.nt.vcnp.se <- se(test)


#######################
#n/tpine.dom
beta0 <-rnorm(1000, mean=0.0546, sd=0.00425)
beta1 <- rnorm(1000, mean=2.64, sd=0.0185)

for(j in unique(dbh.recon.vlf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vlf.tree[!is.na(dbh.recon.vlf.tree$dbh) & dbh.recon.vlf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$pine.dom.mean"] <- mean(test, na.rm=T)
  vlf.year[vlf.year$year==j, "n.t$pine.dom.sd"] <- sd(test)
  vlf.year[vlf.year$year==j, "n.t$pine.dom.se"] <- se(test)
}

summary(vlf.year)
names(vlf.year)<- c("year","nt.pipo.mean", "nt.pipo.sd", "nt.pipo.se",
                    "nt.piaz.mean", "nt.piaz.sd", "nt.piaz.se",
                    "nt.pine.spp.mean", "nt.pine.spp.sd", "nt.pine.spp.se",
                    "nt.vcnp.mean", "nt.vcnp.sd", "nt.vcnp.se",
                    "nt.pine.dom.mean", "nt.pine.dom.sd", "nt.pine.dom.se")

#current n/tpine.dom
beta0 <-rnorm(1000, mean=0.0546, sd=0.00425)
beta1 <- rnorm(1000, mean=2.64, sd=0.0185)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.pine.dom.mean <- mean(test)
current.nt.pine.dom.sd <- sd(test)
current.nt.pine.dom.se <- se(test)


################################################
#bootstrap allometrics for upper flux site
###############################################
#n/t$spruce
vuf.year <- unique(dbh.recon.vuf.tree$year)
vuf.year <- data.frame(vuf.year)
names(vuf.year)<- c("year")


beta0 <-rnorm(1000, mean=0.155, sd=0.0195)
beta1 <- rnorm(1000, mean=2.334, sd=0.0315)

for(j in unique(dbh.recon.vuf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vuf.tree[!is.na(dbh.recon.vuf.tree$dbh) & dbh.recon.vuf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vuf.year[vuf.year$year==j, "n.t$spruce.mean"] <- mean(test)
  vuf.year[vuf.year$year==j, "n.t$spruce.sd"] <- sd(test)
  vuf.year[vuf.year$year==j, "n.t$spruce.se"] <- se(test)
}

summary(vuf.year)

#current n/t$spruce
beta0 <-rnorm(1000, mean=0.155, sd=0.0195)
beta1 <- rnorm(1000, mean=2.334, sd=0.0315)

DBH.list <- vuf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.spruce.mean <- mean(test)
current.nt.spruce.sd <- sd(test)
current.nt.spruce.se <- se(test)

#n/t$vcnp

beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

for(j in unique(dbh.recon.vuf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vuf.tree[!is.na(dbh.recon.vuf.tree$dbh) & dbh.recon.vuf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vuf.year[vuf.year$year==j, "n.t$vcnp.mean"] <- mean(test)
  vuf.year[vuf.year$year==j, "n.t$vcnp.sd"] <- sd(test)
  vuf.year[vuf.year$year==j, "n.t$vcnp.se"] <- se(test)
}

summary(vuf.year)

#current n/t$vcnp
beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

DBH.list <- vuf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.vcnp.mean <- mean(test)
current.nt.vcnp.sd <- sd(test)
current.nt.vcnp.se <- se(test)

#n/t$mixed.con

beta0 <-rnorm(1000, mean=0.0961, sd=0.01)
beta1 <- rnorm(1000, mean=2.493, sd=0.024)

for(j in unique(dbh.recon.vuf.tree$year)){
  #for(j in 2010){
  #  DBH.list <- 10
  DBH.list <- dbh.recon.vuf.tree[!is.na(dbh.recon.vuf.tree$dbh) & dbh.recon.vuf.tree$year==j, "dbh"]
  test <- rep(NA,1000)
  for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)}
  #  a <- mean(test)
  #  b <- sd(test)
  vuf.year[vuf.year$year==j, "nt.mixed.con.mean"] <- mean(test)
  vuf.year[vuf.year$year==j, "nt.mixed.con.sd"] <- sd(test)
  vuf.year[vuf.year$year==j, "nt.mixed.con.se"] <- se(test)
}

summary(vuf.year)
names(vuf.year)<- c("year", "nt.spruce.mean", "nt.spruce.sd", "nt.spruce.se", 
                    "nt.vcnp.mean", "nt.vcnp.sd", "nt.vcnp.se",
                    "nt.mixed.con.mean", "nt.mixed.con.sd", "nt.mixed.con.se")

#current n/t$mixed.con

beta0 <-rnorm(1000, mean=0.0961, sd=0.01)
beta1 <- rnorm(1000, mean=2.493, sd=0.024)

DBH.list <- vuf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- mean(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.mixed.con.mean <- mean(test)
current.nt.mixed.con.sd <- sd(test)
current.nt.mixed.con.se <- se(test)




###########################
#data for simple line graphs with gapfilled data

#applying allometric equations to individual cores in dbh.recon.(site)
dbh.recon.vlf.stack3$jenkins.pine <- exp(equations[equations$model=="jenkins" & equations$spp=="pine", "beta0"] 
                                         + equations[equations$model=="jenkins" & equations$spp=="pine", "beta1"]
                                         * log(dbh.recon.vlf.stack3$dbh))
summary(dbh.recon.vlf.stack3)


dbh.recon.vlf.stack2$jenkins.pine <- exp(equations[equations$model=="jenkins" & equations$spp=="pine", "beta0"] 
                                        + equations[equations$model=="jenkins" & equations$spp=="pine", "beta1"]
                                        * log(dbh.recon.vlf.stack2$dbh))
dbh.recon.vlf.stack2$nt.piaz <- equations[equations$model=="n/t" & equations$spp=="piaz", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="piaz", "beta1"])
dbh.recon.vlf.stack2$nt.pine <- equations[equations$model=="n/t" & equations$spp=="pine.spp", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="pine.spp", "beta1"])
dbh.recon.vlf.stack2$nt.pipo <- equations[equations$model=="n/t" & equations$spp=="pipo", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="pipo", "beta1"])
dbh.recon.vlf.stack2$nt.vcnp <- equations[equations$model=="n/t" & equations$spp=="vcnp", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="vcnp", "beta1"])
dbh.recon.vlf.stack2$nt.pine.dom <- equations[equations$model=="n/t" & equations$spp=="pine.dom", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="pine.dom", "beta1"])

summary(dbh.recon.vlf.stack2)    

vlf.bm.tree.gf <- aggregate(dbh.recon.vlf.stack2[,c("jenkins.pine","nt.piaz", "nt.pine", "nt.pipo", "nt.vcnp","nt.pine.dom")], by=list(dbh.recon.vlf.stack2$tree, dbh.recon.vlf.stack2$year), FUN=mean, na.rm=T)
summary(vlf.bm.tree.gf)

names(vlf.bm.tree.gf) <- c("tree","year",names(vlf.bm.tree.gf[,3:8]))

summary(vlf.bm.tree.gf)

names(vlf.bm.tree.gf)
vlf.bm.avg.gf <- aggregate(vlf.bm.tree.gf[,c("jenkins.pine","nt.piaz", "nt.pine", "nt.pipo", "nt.vcnp","nt.pine.dom")], by=list(vlf.bm.tree.gf$year), FUN=mean, na.rm=T)
vlf.bm.avg.sd.gf <- aggregate(vlf.bm.tree.gf[,c("jenkins.pine","nt.piaz", "nt.pine", "nt.pipo", "nt.vcnp","nt.pine.dom")], by=list(vlf.bm.tree.gf$year), FUN=sd, na.rm=T)
summary(vlf.bm.avg.sd.gf)
summary(vlf.bm.avg.gf)
names(vlf.bm.avg.gf) <- c("year", names(vlf.bm.avg.gf[,2:7]))
names(vlf.bm.avg.sd.gf) <- c("year", names(vlf.bm.avg.sd.gf[,2:7]))



dbh.recon.vuf.stack2$jenkins.spruce <- exp(equations[equations$model=="jenkins" & equations$spp=="spruce", "beta0"] 
                                          + equations[equations$model=="jenkins" & equations$spp=="spruce", "beta1"]
                                          * log(dbh.recon.vuf.stack2$dbh))
summary(dbh.recon.vuf.stack2)


dbh.recon.vuf.stack2$nt.spruce <- equations[equations$model=="n/t" & equations$spp=="spruce", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="spruce", "beta1"])
dbh.recon.vuf.stack2$nt.vcnp <- equations[equations$model=="n/t" & equations$spp=="vcnp", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="vcnp", "beta1"])
dbh.recon.vuf.stack2$nt.mixed.con <- equations[equations$model=="n/t" & equations$spp=="mixed.con", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="mixed.con", "beta1"])
dbh.recon.vuf.stack2$nt.psme <- equations[equations$model=="n/t" & equations$spp=="psme", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="psme", "beta1"])

summary(dbh.recon.vuf.stack2)

vuf.bm.tree.gf <- aggregate(dbh.recon.vuf.stack2[,c("jenkins.spruce","nt.spruce", "nt.vcnp", "nt.mixed.con", "nt.psme")], by=list(dbh.recon.vuf.stack2$tree, dbh.recon.vuf.stack2$year), FUN=mean, na.rm=T)
summary(vuf.bm.tree.gf)

names(vuf.bm.tree.gf) <- c("tree","year",names(vuf.bm.tree[,3:7]))

vuf.bm.avg.gf <- aggregate(vuf.bm.tree.gf[,c("jenkins.spruce","nt.spruce", "nt.vcnp", "nt.mixed.con","nt.psme")], by=list(vuf.bm.tree.gf$year), FUN=mean, na.rm=T)
vuf.bm.avg.sd.gf <- aggregate(vuf.bm.tree.gf[,c("jenkins.spruce","nt.spruce", "nt.vcnp", "nt.mixed.con","nt.psme")], by=list(vuf.bm.tree.gf$year), FUN=sd, na.rm=T)
summary(vuf.bm.avg.sd.gf)
summary(vuf.bm.avg.gf)
names(vuf.bm.avg.gf) <- c("year", names(vuf.bm.avg.gf[,2:6]))
names(vuf.bm.avg.sd.gf) <- c("year", names(vuf.bm.avg.sd.gf[,2:6]))
########################################################################################
#trying to plot kg/tree with error ribbons
########################################################################################
#vlf.year
#vuf.year
#objects with the data I need

summary(vlf.year)
summary(vuf.year)


qplot(x=year, y= nt.pipo.mean,data=vlf.year, geom="line") + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="Year") + scale_y_continuous(name="kg Biomass / tree") + ggtitle("kg.BM/Tree") +
  geom_ribbon(aes(ymin=nt.pipo.mean - nt.pipo.sd, ymax=nt.pipo.mean + nt.pipo.sd), alpha=0.5)

vlf.plot<- ggplot()  +
  # plotting total site basal area
  
  geom_ribbon(data=vlf.year, aes(x=year, ymin=nt.pipo.mean - 1.96*nt.pipo.sd, ymax=nt.pipo.mean + 1.96*nt.pipo.sd), alpha=0.15, fill="red")+
  geom_ribbon(data=vlf.year, aes(x=year, ymin=nt.piaz.mean - 1.96*nt.piaz.sd, ymax=nt.piaz.mean + 1.96*nt.piaz.sd), alpha=0.15, fill="orange")+
  geom_ribbon(data=vlf.year, aes(x=year, ymin=nt.pine.spp.mean - 1.96*nt.pine.spp.sd, ymax=nt.pine.spp.mean + 1.96*nt.pine.spp.sd), alpha=0.15, fill="green")+
  #geom_ribbon(data=vlf.year, aes(x=year, ymin=nt.vcnp.mean - 1.96*nt.vcnp.sd, ymax=nt.vcnp.mean + 1.96*nt.vcnp.sd), alpha=0.15, fill="purple")+
  geom_ribbon(data=vlf.year, aes(x=year, ymin=nt.pine.dom.mean - 1.96*nt.pine.dom.sd, ymax=nt.pine.dom.mean + 1.96*nt.pine.dom.sd), alpha=0.15, fill="blue")+
  
  geom_line(data=vlf.year,  aes(x=year, y=nt.pipo.mean), size=1.5, colour="red") +
  geom_line(data= vlf.year, aes(x=year, y=nt.piaz.mean), size=1.5, colour="orange") +
  geom_line(data= vlf.year, aes(x=year, y=nt.pine.spp.mean), size=1.5,colour="green") +
  #geom_line(data= vlf.year, aes(x=year, y=nt.vcnp.mean), size=1.5,colour="purple") +
  geom_line(data= vlf.year, aes(x=year, y=nt.pine.dom.mean), size=1.5,colour="blue") +
  geom_line(data= vlf.bm.avg.gf, aes(x=year, y=jenkins.pine), size=1.5,colour="black") +
  
  
  geom_point(aes(x=2012, y=current.nt.pipo.mean), size=4, colour="red")+
  geom_point(aes(x=2012, y=current.nt.piaz.mean), size=4, colour="orange")+
  geom_point(aes(x=2012, y=current.nt.pine.mean), size=4, colour="green")+
  #geom_point(aes(x=2012, y=current.nt.vcnp.mean), size=4, colour="purple")+
  geom_point(aes(x=2012, y=current.nt.pine.dom.mean), size=4, colour="blue")+
  geom_point(data=vlf.bm.means, aes(x=2012, y=biomass[1]), size=4, colour="black")+
  
  
  geom_errorbar(aes(x=2012, ymin=current.nt.pipo.mean-1.96*current.nt.pipo.sd, ymax=current.nt.pipo.mean+1.96*current.nt.pipo.sd), width=0.25, colour="red") +
  geom_errorbar(aes(x=2012, ymin=current.nt.piaz.mean-1.96*current.nt.piaz.sd, ymax=current.nt.piaz.mean+1.96*current.nt.piaz.sd), width=0.25, colour="orange") +
  geom_errorbar(aes(x=2012, ymin=current.nt.pine.mean-1.96*current.nt.pine.sd, ymax=current.nt.pine.mean+1.96*current.nt.pine.sd), width=0.25, colour="green") +
 # geom_errorbar(aes(x=2012, ymin=current.nt.vcnp.mean-1.96*current.nt.vcnp.sd, ymax=current.nt.vcnp.mean+1.96*current.nt.vcnp.sd), width=0.25, colour="purple") +
  geom_errorbar(aes(x=2012, ymin=current.nt.pine.dom.mean-1.96*current.nt.pine.dom.sd, ymax=current.nt.pine.dom.mean+1.96*current.nt.pine.dom.sd), width=0.25, colour="blue") +
  # all of that theme stuff you can just pre-set
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12))+
  scale_fill_discrete(name="Model", labels = c("nt.pipo.mean", "nt.piaz.mean", "nt.pine.spp", "nt.vcnp.mean", "nt.pine.dom.mean"))

# telling what colors to make the lines for species
#scale_color_manual(values=c("red", "blue", "orange", "green")) 
vlf.plot+ggtitle("Lower Flux Tower")+scale_y_continuous("kg Biomass per Tree")

vuf.plot<- ggplot()  +
  # plotting error ribbons
  
  geom_ribbon(data=vuf.year, aes(x=year, ymin=nt.spruce.mean - 1.96*nt.spruce.sd, ymax=nt.spruce.mean + 1.96*nt.spruce.sd, fill="nt.spruce"), alpha=0.15,fill="red")+
  geom_ribbon(data=vuf.year, aes(x=year, ymin=nt.vcnp.mean - 1.96*nt.vcnp.sd, ymax=nt.vcnp.mean + 1.96*nt.vcnp.sd, fill="nt.vcnp"), alpha=0.15, fill="orange")+
  geom_ribbon(data=vuf.year, aes(x=year, ymin=nt.mixed.con.mean - 1.96*nt.mixed.con.sd, ymax=nt.mixed.con.mean + 1.96*nt.mixed.con.sd, fill="mixed.con"), alpha=0.15, fill="green")+
  
  geom_line(data=vuf.year,  aes(x=year, y=nt.spruce.mean), size=1.5,colour="red") +
  geom_line(data= vuf.year, aes(x=year, y=nt.vcnp.mean), size=1.5, colour="orange") +
  geom_line(data= vuf.year, aes(x=year, y=nt.mixed.con.mean), size=1.5, colour="green") +
  geom_line(data= vuf.bm.avg.gf, aes(x=year, y=jenkins.spruce), size=1.5, colour="black") +
  
  geom_point(aes(x=2012, y=current.nt.spruce.mean), size=4,, colour="red")+
  geom_point(aes(x=2012, y=current.nt.vcnp.mean), size=4,, colour="orange")+
  geom_point(aes(x=2012, y=current.nt.mixed.con.mean), size=4,, colour="green")+
  geom_point(data= vuf.bm.means, aes(x=2012, y=biomass[1]), size=4,, colour="black")+
  
  geom_errorbar(aes(x=2012, ymin=current.nt.spruce.mean-1.96*current.nt.spruce.sd, ymax=current.nt.spruce.mean+1.96*current.nt.spruce.sd, colour="red"), width=0.25,, colour="red")+
  geom_errorbar(aes(x=2012, ymin=current.nt.vcnp.mean-1.96*current.nt.vcnp.sd, ymax=current.nt.vcnp.mean+1.96*current.nt.vcnp.sd,colour="orange"), width=0.25, colour="orange")+
  geom_errorbar(aes(x=2012, ymin=current.nt.mixed.con.mean-1.96*current.nt.mixed.con.sd, ymax=current.nt.mixed.con.mean+1.96*current.nt.mixed.con.sd, colour="green"), width=0.25,, colour="green")+
  
  # all of that theme stuff you can just pre-set
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12,))+
  scale_colour_manual(values=c("red", "orange", "green"),name= "Model",labels=c("nt.spruce", "nt.vcnp", "nt.mixed.con"))
vuf.plot+ ggtitle("UpperFlux Tower")+scale_y_continuous("kg Biomass per Tree")

#plotting sample size of each site
counts<- read.csv("dated.counts.csv", header=T)

par(mfcol=c(1,1))
plot(counts$vuf.count~counts$year, type="l", lwd=2, xlab="year", ylab="Count", main="Sample Size", col="blue", ylim=range(counts$vlf.count, na.rm=T))
par(new=T)
plot(counts$vlf.count~counts$year, type="l", lwd=2, xlab="year", ylab="Count", col="red",ylim=range(counts$vlf.count, na.rm=T))
legend("bottomright", legend=c("Spruce Site", "PIPO Site"), lty="solid", lwd="2", col=c("blue", "red"), bty="n", cex=1.5) 



############################################################################################################
############################################################################################################
# Running a whole plot with the same allometric equation, taking the sum of the entire plot
############################################################################################################
############################################################################################################

vlf.year2<-as.data.frame(vlf.year$year) 
names(vlf.year2)<-c("year")

###############################################
###############################################
# Navar Pipo
###############################################
###############################################
beta0 <- rnorm(1000, mean=0.054, sd=0.004)
beta1 <- rnorm(1000, mean=2.651, sd=0.0175)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vlf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vlf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vlf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vlf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data

vlf.year2[, "n.t$pipo.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vlf.year2[i, "n.t$pipo.sd"] <-  sd(year.sd, na.rm=T)
}


# current Navar Pipo
beta0 <- rnorm(1000, mean=0.054, sd=0.004)
beta1 <- rnorm(1000, mean=2.651, sd=0.0175)


DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.pipo.sum <- mean(test)
current.nt.pipo.sd <- sd(test)
current.nt.pipo.se <-se(test)

###############################################
###############################################
#n/t$piaz
###############################################
###############################################
beta0 <- 0.0527
beta1 <- rnorm(1000, mean=2.5569, sd=0.0275)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vlf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vlf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vlf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vlf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data

vlf.year2[, "n/t$piaz.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vlf.year2[i, "n/t$piaz.sd"] <-  sd(year.sd, na.rm=T)
}

# current n/t$piaz
beta0 <- 0.0527
beta1 <- rnorm(1000, mean=2.5569, sd=0.0275)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.piaz.sum <- mean(test)
current.nt.piaz.sd <- sd(test)
current.nt.piaz.se <- se(test)

###############################################
###############################################
#n/t$pine.spp
###############################################
###############################################

beta0 <- 0.0597
beta1 <- rnorm(1000, mean=2.5741, sd=0.013)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vlf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vlf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vlf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vlf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data

vlf.year2[, "n/t$pine.spp.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vlf.year2[i, "n/t$pine.spp.sd"] <-  sd(year.sd, na.rm=T)
}

# current n/t$pine.spp
beta0 <- 0.0597
beta1 <- rnorm(1000, mean=2.5741, sd=0.013)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.pine.sum <- mean(test)
current.nt.pine.sd <- sd(test)
current.nt.pine.se <- se(test)



###############################################
###############################################
#n/t$vcnp
###############################################
###############################################

beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vlf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vlf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vlf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vlf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data

vlf.year2[, "n/t$vcnp.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vlf.year2[i, "n/t$vcnp.sd"] <-  sd(year.sd, na.rm=T)
}


#current n/t$vcnp
beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.vcnp.sum <- mean(test)
current.nt.vcnp.sd <- sd(test)
current.nt.vcnp.se <- se(test)


###############################################
###############################################
#n/tpine.dom
###############################################
###############################################
beta0 <-rnorm(1000, mean=0.0546, sd=0.00425)
beta1 <- rnorm(1000, mean=2.64, sd=0.0185)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vlf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vlf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vlf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vlf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data

vlf.year2[, "n/tpine.dom.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vlf.year2[i, "n/tpine.dom.sd"] <-  sd(year.sd, na.rm=T)
}

summary(vlf.year2)
names(vlf.year2)<- c("year","nt.pipo.sum", "nt.pipo.sd","nt.piaz.sum", "nt.piaz.sd",
                     "nt.pine.spp.sum", "nt.pine.spp.sd", "nt.vcnp.sum", "nt.vcnp.sd",
                     "nt.pine.dom.sum", "nt.pine.dom.sd")

#current n/tpine.dom
beta0 <-rnorm(1000, mean=0.0546, sd=0.00425)
beta1 <- rnorm(1000, mean=2.64, sd=0.0185)

DBH.list <- vlf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.pine.dom.sum <- mean(test)
current.nt.pine.dom.sd <- sd(test)
current.nt.pine.dom.se <- se(test)


################################################
#bootstrap allometrics for upper flux site
###############################################
###############################################
#n/t$spruce
###############################################
###############################################
vuf.year2<- as.data.frame(vuf.year2$year)
names(vuf.year2)<-c("year")

vuf.year2 <- unique(dbh.recon.vuf.tree$year)
vuf.year2 <- data.frame(vuf.year2)
names(vuf.year2)<- c("year")


beta0 <-rnorm(1000, mean=0.155, sd=0.0195)
beta1 <- rnorm(1000, mean=2.334, sd=0.0315)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vuf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vuf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vuf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vuf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data
vuf.year2<-as.data.frame(vuf.year$year) 
names(vuf.year2)<-c("year")

vuf.year2[, "n/t$spruce.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vuf.year2[i, "n/t$spruce.sd"] <-  sd(year.sd, na.rm=T)
}


#current n/t$spruce
beta0 <-rnorm(1000, mean=0.155, sd=0.0195)
beta1 <- rnorm(1000, mean=2.334, sd=0.0315)

DBH.list <- vuf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.spruce.sum <- mean(test)
current.nt.spruce.sd <- sd(test)
current.nt.spruce.se <- se(test)

###############################################
###############################################
#n/t$vcnp
###############################################
###############################################
beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vuf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vuf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vuf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vuf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data

vuf.year2[, "n/t$vcnp.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vuf.year2[i, "n/t$vcnp.sd"] <-  sd(year.sd, na.rm=T)
}


#current n/t$vcnp
beta0 <-rnorm(1000, mean=0.063, sd=0.0035)
beta1 <- rnorm(1000, mean=2.615, sd=0.014)

DBH.list <- vuf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.vcnp.sum <- mean(test)
current.nt.vcnp.sd <- sd(test)
current.nt.vcnp.se <- se(test)

###############################################
###############################################
#n/t$mixed.con
###############################################
###############################################
beta0 <-rnorm(1000, mean=0.0961, sd=0.01)
beta1 <- rnorm(1000, mean=2.493, sd=0.024)

# Creating a data frames where runs will get stored; using year to get the right number of rows (a crude way of doing it)
all.runs<-as.data.frame(vuf.year$year) 
all.runs[,1] <- NA #overwriting year with NA because I think it's easier if we don't have a year column
row.names(all.runs) <- vuf.year$year #just to help, adding rownames that match the year
names(all.runs)<-c("run1")

# starting the loop with the number of iterations you want
for(i in 1:1000){
  # creating a data frame where the Biomass will go; this will get overwritten in each run
  boot.run <- dbh.recon.vuf.tree 
  
  # running the allometric equation pulling randomly from the beta distribution; remember to check before you run that these are not impossible numbers
  boot.run$biomass <- sample(beta0, size=1, replace=T)*dbh.recon.vuf.tree$dbh^sample(beta1, size=1, replace=T)
  
  # Summing biomass for each year via aggregate
  boot.sum <- aggregate(boot.run$biomass, by=list(boot.run$year), FUN=sum, na.rm=T)
  names(boot.sum) <- c("year", "biomass.total")
  
  # storing each run into a data frame; i+1 shoudl mean first run gets stored in column 2, etc; this may not work and will need ot be tweaked
  all.runs[,i] <- boot.sum$biomass.total
} # close loop; final product we're interested in is the all.runs

dim(all.runs) # check the dimensions of all.runs; rows should equal number of years; columns should be number of iterations (1000)

# Creating your final data frame with the summarized bootstrap data

vuf.year2[, "n/t$mixed.con.sum"] <- rowMeans(all.runs, na.rm=T)

for(i in 1:length(all.runs[,1])){
  year.sd <- as.vector(all.runs[i,])
  vuf.year2[i, "n/t$mixed.con.sd"] <-  sd(year.sd, na.rm=T)
}
summary(vuf.year2)
names(vuf.year2)<- c("year", "nt.spruce.sum", "nt.spruce.sd", 
                     "nt.vcnp.sum", "nt.vcnp.sd", 
                     "nt.mixed.con.sum", "nt.mixed.con.sd")

#current n/t$mixed.con

beta0 <-rnorm(1000, mean=0.0961, sd=0.01)
beta1 <- rnorm(1000, mean=2.493, sd=0.024)

DBH.list <- vuf.current$dbh
test <- rep(NA,1000)
for(i in 1:1000){test[i] <- sum(sample(beta0, size=1, replace=T)*DBH.list^sample(beta1, size=1, replace=T), na.rm=T)
}
current.nt.mixed.con.sum <- mean(test)
current.nt.mixed.con.sd <- sd(test)
current.nt.mixed.con.se <- se(test)




###########################
#data for simple line graphs with gapfilled data

#applying allometric equations to individual cores in dbh.recon.(site)
dbh.recon.vlf.stack3$jenkins.pine <- exp(equations[equations$model=="jenkins" & equations$spp=="pine", "beta0"] 
                                         + equations[equations$model=="jenkins" & equations$spp=="pine", "beta1"]
                                         * log(dbh.recon.vlf.stack3$dbh))
summary(dbh.recon.vlf.stack3)


dbh.recon.vlf.stack2$jenkins.pine <- exp(equations[equations$model=="jenkins" & equations$spp=="pine", "beta0"] 
                                         + equations[equations$model=="jenkins" & equations$spp=="pine", "beta1"]
                                         * log(dbh.recon.vlf.stack2$dbh))
dbh.recon.vlf.stack2$nt.piaz <- equations[equations$model=="n/t" & equations$spp=="piaz", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="piaz", "beta1"])
dbh.recon.vlf.stack2$nt.pine <- equations[equations$model=="n/t" & equations$spp=="pine.spp", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="pine.spp", "beta1"])
dbh.recon.vlf.stack2$nt.pipo <- equations[equations$model=="n/t" & equations$spp=="pipo", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="pipo", "beta1"])
dbh.recon.vlf.stack2$nt.vcnp <- equations[equations$model=="n/t" & equations$spp=="vcnp", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="vcnp", "beta1"])
dbh.recon.vlf.stack2$nt.pine.dom <- equations[equations$model=="n/t" & equations$spp=="pine.dom", "beta0"] * (dbh.recon.vlf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="pine.dom", "beta1"])

summary(dbh.recon.vlf.stack2)    

vlf.bm.tree.gf <- aggregate(dbh.recon.vlf.stack2[,c("jenkins.pine","nt.piaz", "nt.pine", "nt.pipo", "nt.vcnp","nt.pine.dom")], by=list(dbh.recon.vlf.stack2$tree, dbh.recon.vlf.stack2$year), FUN=sum, na.rm=T)
summary(vlf.bm.tree.gf)

names(vlf.bm.tree.gf) <- c("tree","year",names(vlf.bm.tree.gf[,3:8]))

summary(vlf.bm.tree.gf)

names(vlf.bm.tree.gf)
vlf.bm.avg.gf <- aggregate(vlf.bm.tree.gf[,c("jenkins.pine","nt.piaz", "nt.pine", "nt.pipo", "nt.vcnp","nt.pine.dom")], by=list(vlf.bm.tree.gf$year), FUN=sum, na.rm=T)
vlf.bm.avg.sd.gf <- aggregate(vlf.bm.tree.gf[,c("jenkins.pine","nt.piaz", "nt.pine", "nt.pipo", "nt.vcnp","nt.pine.dom")], by=list(vlf.bm.tree.gf$year), FUN=sd, na.rm=T)
summary(vlf.bm.avg.sd.gf)
summary(vlf.bm.avg.gf)
names(vlf.bm.avg.gf) <- c("year", names(vlf.bm.avg.gf[,2:7]))
names(vlf.bm.avg.sd.gf) <- c("year", names(vlf.bm.avg.sd.gf[,2:7]))



dbh.recon.vuf.stack2$jenkins.spruce <- exp(equations[equations$model=="jenkins" & equations$spp=="spruce", "beta0"] 
                                           + equations[equations$model=="jenkins" & equations$spp=="spruce", "beta1"]
                                           * log(dbh.recon.vuf.stack2$dbh))
summary(dbh.recon.vuf.stack2)


dbh.recon.vuf.stack2$nt.spruce <- equations[equations$model=="n/t" & equations$spp=="spruce", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="spruce", "beta1"])
dbh.recon.vuf.stack2$nt.vcnp <- equations[equations$model=="n/t" & equations$spp=="vcnp", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="vcnp", "beta1"])
dbh.recon.vuf.stack2$nt.mixed.con <- equations[equations$model=="n/t" & equations$spp=="mixed.con", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="mixed.con", "beta1"])
dbh.recon.vuf.stack2$nt.psme <- equations[equations$model=="n/t" & equations$spp=="psme", "beta0"] * (dbh.recon.vuf.stack2$dbh)^(equations[equations$model=="n/t" & equations$spp=="psme", "beta1"])

summary(dbh.recon.vuf.stack2)

vuf.bm.tree.gf <- aggregate(dbh.recon.vuf.stack2[,c("jenkins.spruce","nt.spruce", "nt.vcnp", "nt.mixed.con", "nt.psme")], by=list(dbh.recon.vuf.stack2$tree, dbh.recon.vuf.stack2$year), FUN=sum, na.rm=T)
summary(vuf.bm.tree.gf)

names(vuf.bm.tree.gf) <- c("tree","year",names(vuf.bm.tree[,3:7]))

vuf.bm.avg.gf <- aggregate(vuf.bm.tree.gf[,c("jenkins.spruce","nt.spruce", "nt.vcnp", "nt.mixed.con","nt.psme")], by=list(vuf.bm.tree.gf$year), FUN=sum, na.rm=T)
vuf.bm.avg.sd.gf <- aggregate(vuf.bm.tree.gf[,c("jenkins.spruce","nt.spruce", "nt.vcnp", "nt.mixed.con","nt.psme")], by=list(vuf.bm.tree.gf$year), FUN=sd, na.rm=T)
summary(vuf.bm.avg.sd.gf)
summary(vuf.bm.avg.gf)
names(vuf.bm.avg.gf) <- c("year", names(vuf.bm.avg.gf[,2:6]))
names(vuf.bm.avg.sd.gf) <- c("year", names(vuf.bm.avg.sd.gf[,2:6]))

########################################################################################
#trying to plot kg/tree with error ribbons
########################################################################################
#vlf.year2
#vuf.year2
#objects with the data I need

summary(vlf.year2)
summary(vuf.year2)


qplot(x=year, y= nt.pipo.sum,data=vlf.year2, geom="line") + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="Year") + scale_y_continuous(name="kg Biomass / tree") + ggtitle("kg.BM/Tree") +
  geom_ribbon(aes(ymin=nt.pipo.sum - nt.pipo.sd, ymax=nt.pipo.sum + nt.pipo.sd), alpha=0.5)

poster.theme<-theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
              panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=20),
              axis.text.y=element_text(angle=0, color="black", size=20), axis.title.x=element_text(face="bold", size=24),
              axis.title.y=element_text(face="bold", size=24), strip.text=element_text(face="bold", size=rel(1.75)),
              title=element_text(face="bold", size=28))


#adjusted biomass in kg to account for plot area
#units now in kg of Biomass per m^2
vlf.year3<- vlf.year2[,2:ncol(vlf.year2)]/768
vlf.year3$year<- vlf.year2$year

vlf.plot<- ggplot()  +
  # plotting total site basal area
  
  geom_ribbon(data=vlf.year3, aes(x=year, ymin=nt.pipo.sum - 1.96*nt.pipo.sd, ymax=nt.pipo.sum + 1.96*nt.pipo.sd), alpha=0.15, fill="red")+
  geom_ribbon(data=vlf.year3, aes(x=year, ymin=nt.piaz.sum - 1.96*nt.piaz.sd, ymax=nt.piaz.sum + 1.96*nt.piaz.sd), alpha=0.15, fill="orange")+
  geom_ribbon(data=vlf.year3, aes(x=year, ymin=nt.pine.spp.sum - 1.96*nt.pine.spp.sd, ymax=nt.pine.spp.sum + 1.96*nt.pine.spp.sd), alpha=0.15, fill="green")+
  #geom_ribbon(data=vlf.year2, aes(x=year, ymin=nt.vcnp.sum - 1.96*nt.vcnp.sd, ymax=nt.vcnp.sum + 1.96*nt.vcnp.sd), alpha=0.15, fill="purple")+
  geom_ribbon(data=vlf.year3, aes(x=year, ymin=nt.pine.dom.sum - 1.96*nt.pine.dom.sd, ymax=nt.pine.dom.sum + 1.96*nt.pine.dom.sd), alpha=0.15, fill="blue")+
  
  geom_line(data=vlf.year3,  aes(x=year, y=nt.pipo.sum), size=1.5, colour="red") +
  geom_line(data= vlf.year3, aes(x=year, y=nt.piaz.sum), size=1.5, colour="orange") +
  geom_line(data= vlf.year3, aes(x=year, y=nt.pine.spp.sum), size=1.5,colour="green") +
  #geom_line(data= vlf.year2, aes(x=year, y=nt.vcnp.sum), size=1.5,colour="purple") +
  geom_line(data= vlf.year3, aes(x=year, y=nt.pine.dom.sum), size=1.5,colour="blue") +
  #geom_line(data= vlf.bm.avg.gf, aes(x=year, y=jenkins.pine), size=1.5,colour="black") +
  
  
  geom_point(aes(x=2012, y=current.nt.pipo.sum/768), size=4, colour="red")+
  geom_point(aes(x=2012, y=current.nt.piaz.sum/768), size=4, colour="orange")+
  geom_point(aes(x=2012, y=current.nt.pine.sum/768), size=4, colour="green")+
  #geom_point(aes(x=2012, y=current.nt.vcnp.sum), size=4, colour="purple")+
  geom_point(aes(x=2012, y=current.nt.pine.dom.sum/768), size=4, colour="blue")+
  #geom_point(data=vlf.bm.sums, aes(x=2012, y=biomass[1]), size=4, colour="black")+
  
  
geom_errorbar(aes(x=2012, ymin=(current.nt.pipo.sum/768)-1.96*(current.nt.pipo.sd/768), ymax=(current.nt.pipo.sum/768)+1.96*(current.nt.pipo.sd/768)), width=0.25, colour="red") +
geom_errorbar(aes(x=2012, ymin=(current.nt.piaz.sum/768)-1.96*(current.nt.piaz.sd/768), ymax=(current.nt.piaz.sum/768)+1.96*(current.nt.piaz.sd/768)), width=0.25, colour="orange") +
geom_errorbar(aes(x=2012, ymin=(current.nt.pine.sum/768)-1.96*(current.nt.pine.sd/768), ymax=(current.nt.pine.sum/768)+1.96*(current.nt.pine.sd/768)), width=0.25, colour="green") +
# geom_errorbar(aes(x=2012, ymin=current.nt.vcnp.sum-1.96*current.nt.vcnp.sd, ymax=current.nt.vcnp.sum+1.96*current.nt.vcnp.sd), width=0.25, colour="purple") +
geom_errorbar(aes(x=2012, ymin=(current.nt.pine.dom.sum/768)-1.96*(current.nt.pine.dom.sd/768), ymax=(current.nt.pine.dom.sum/768)+1.96*(current.nt.pine.dom.sd/768)), width=0.25, colour="blue") +
# all of that theme stuff you can just pre-set
poster.theme
  #theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12))+
 # scale_fill_discrete(name="Model", labels = c("nt.pipo.sum", "nt.piaz.sum", "nt.pine.spp", "nt.vcnp.sum", "nt.pine.dom.sum"))

# telling what colors to make the lines for species
#scale_color_manual(values=c("red", "blue", "orange", "green")) 
vlf.plot+ggtitle("Lower Flux Tower")+scale_y_continuous("kg Biomass m-2")+scale_x_continuous("Year")

#adjusted biomass in kg to account for plot area
#units now in kg of Biomass per m^2
vuf.year3<-as.data.frame(vuf.year2[,2:ncol(vuf.year2)]/1152)
vuf.year3$year<-vuf.year2$year

vuf.plot<- ggplot()  +
  # plotting error ribbons
  
  geom_ribbon(data=vuf.year3, aes(x=year, ymin=nt.spruce.sum - 1.96*nt.spruce.sd, ymax=nt.spruce.sum + 1.96*nt.spruce.sd, fill="nt.spruce"), alpha=0.15,fill="red")+
  geom_ribbon(data=vuf.year3, aes(x=year, ymin=nt.vcnp.sum - 1.96*nt.vcnp.sd, ymax=nt.vcnp.sum + 1.96*nt.vcnp.sd, fill="nt.vcnp"), alpha=0.15, fill="orange")+
  geom_ribbon(data=vuf.year3, aes(x=year, ymin=nt.mixed.con.sum - 1.96*nt.mixed.con.sd, ymax=nt.mixed.con.sum + 1.96*nt.mixed.con.sd, fill="mixed.con"), alpha=0.15, fill="green")+
  
  geom_line(data=vuf.year3,  aes(x=year, y=nt.spruce.sum), size=1.5,colour="red") +
  geom_line(data= vuf.year3, aes(x=year, y=nt.vcnp.sum), size=1.5, colour="orange") +
  geom_line(data= vuf.year3, aes(x=year, y=nt.mixed.con.sum), size=1.5, colour="green") +
  #geom_line(data= vuf.bm.avg.gf, aes(x=year, y=jenkins.spruce), size=1.5, colour="black") +
  
  geom_point(aes(x=2012, y=current.nt.spruce.sum/1152), size=4,, colour="red")+
  geom_point(aes(x=2012, y=current.nt.vcnp.sum/1152), size=4,, colour="orange")+
  geom_point(aes(x=2012, y=current.nt.mixed.con.sum/1152), size=4, colour="green")+
  #geom_point(data= vuf.bm.sums, aes(x=2012, y=biomass[1]), size=4,, colour="black")+
  
  geom_errorbar(aes(x=2012, ymin=(current.nt.spruce.sum/1152)-1.96*(current.nt.spruce.sd/1152), ymax=(current.nt.spruce.sum/1152)+1.96*(current.nt.spruce.sd/1152), colour="red"), width=0.25,, colour="red")+
  geom_errorbar(aes(x=2012, ymin=(current.nt.vcnp.sum/1152)-1.96*(current.nt.vcnp.sd/1152), ymax=(current.nt.vcnp.sum/1152)+1.96*(current.nt.vcnp.sd/1152),colour="orange"), width=0.25, colour="orange")+
  geom_errorbar(aes(x=2012, ymin=(current.nt.mixed.con.sum/1152)-1.96*(current.nt.mixed.con.sd/1152), ymax=(current.nt.mixed.con.sum/1152)+1.96*(current.nt.mixed.con.sd/1152), colour="green"), width=0.25,, colour="green")+
  
# all of that theme stuff you can just pre-set
poster.theme
  #theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12,))+
 # scale_colour_manual(values=c("red", "orange", "green"),name= "Model",labels=c("nt.spruce", "nt.vcnp", "nt.mixed.con"))
vuf.plot+ ggtitle("Upper Flux Tower")+scale_y_continuous("kg Biomass m-2")+scale_x_continuous("Year")

#plotting sample size of each site
counts<- read.csv("dated.counts.csv", header=T)

par(mfcol=c(1,1))
plot(counts$vuf.count~counts$year, type="l", lwd=2, xlab="year", ylab="Count", main="Sample Size", col="red", ylim=range(counts$vlf.count, na.rm=T))
par(new=T)
plot(counts$vlf.count~counts$year, type="l", lwd=2, xlab="year", ylab="Count", col="blue",ylim=range(counts$vlf.count, na.rm=T))
legend("bottomright", legend=c("VUF", "VLF"), lty="solid", lwd="2", col=c("red", "blue"), bty="n", cex=1.5) 

################################################
#taking the mean of all the models of each site
################################################
names(vlf.year2)
names(vuf.year2)

#VLF
model.mean<-as.data.frame(vlf.year2$year)
names(model.mean)<- c("year")

for(i in 1:length(vlf.year2$year)){
  model.mean[i, "vlf.mean"] <- mean(vlf.year2[i,"nt.pipo.sum"], vlf.year2[i,"nt.piaz.sum"], vlf.year2[i,"nt.pine.spp.sum"],vlf.year2[i,"nt.pine.dom.sum"], na.rm=T)
  model.mean[i, "vlf.sd"] <- sqrt(vlf.year2[i,"nt.pipo.sd"]^2+ vlf.year2[i,"nt.piaz.sd"]^2+ vlf.year2[i,"nt.pine.spp.sd"]^2+ vlf.year2[i,"nt.pine.dom.sd"]^2)
}


#VUF

for(i in 1:length(vuf.year2$year)){
  model.mean[i, "vuf.mean"] <- mean(vuf.year2[i,"nt.spruce.sum"], vuf.year2[i,"nt.vcnp.sum"], vuf.year2[i,"nt.mixed.con.sum"], na.rm=T)
  model.mean[i, "vuf.sd"] <- sqrt(vuf.year2[i,"nt.spruce.sd"]^2+vuf.year2[i,"nt.vcnp.sd"]^2+ vuf.year2[i,"nt.mixed.con.sd"]^2)
}

model.mean

current.mean<- as.data.frame(mean(current.nt.pipo.sum, current.nt.piaz.sum, current.nt.pine.sum, current.nt.pine.dom.sum))
names(current.mean)<- c("vlf.mean")

current.mean$vlf.sd <- sqrt(mean(current.nt.pipo.sd^2+ current.nt.piaz.sd^2+ current.nt.pine.sd^2+current.nt.pine.dom.sd^2))

current.mean$vuf.mean<- mean(current.nt.spruce.sum, current.nt.vcnp.sum, current.nt.mixed.con.sum)
current.mean$vuf.sd<- sqrt(current.nt.spruce.sd^2+ current.nt.vcnp.sd^2+ current.nt.mixed.con.sd^2)

#plotsof mean of plots

mean.plot<- ggplot()  +
  # plotting total site basal area
  
  geom_ribbon(data=model.mean, aes(x=year, ymin=vlf.mean - 1.96*vlf.sd, ymax=vlf.mean + 1.96*vlf.sd), alpha=0.15, fill="red")+
  geom_ribbon(data=model.mean, aes(x=year, ymin=vuf.mean - 1.96*vuf.sd, ymax=vuf.mean + 1.96*vuf.sd), alpha=0.15, fill="blue")+
  
  geom_line(data=model.mean,  aes(x=year, y=vlf.mean), size=1.5, colour="red") +
  geom_line(data= model.mean, aes(x=year, y=vuf.mean), size=1.5, colour="blue") +
  
  
  geom_point(data=current.mean, aes(x=2012, y=vlf.mean), size=4, colour="red")+
  geom_point(data=current.mean, aes(x=2012, y=vuf.mean), size=4, colour="blue")+
  
  
  geom_errorbar(data=current.mean,aes(x=2012, ymin=vlf.mean - 1.96*vlf.sd, ymax=vlf.mean + 1.96*vlf.sd), color="red")+
  geom_errorbar(data=current.mean, aes(x=2012, ymin=vuf.mean - 1.96*vuf.sd, ymax=vuf.mean + 1.96*vuf.sd), color="blue")+
  
  # all of that theme stuff you can just pre-set
  poster.theme
  #theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12))
  #scale_fill_discrete(name="Model", labels = c("nt.pipo.sum", "nt.piaz.sum", "nt.pine.spp", "nt.vcnp.sum", "nt.pine.dom.sum"))

# telling what colors to make the lines for species
#scale_color_manual(values=c("red", "blue", "orange", "green")) 
mean.plot+ggtitle("Ensemble Biomass")+scale_y_continuous("kg Biomass")+scale_x_continuous("Year")

#Plot biomass in kilograms
#divide all biomass by the area of the respective plots
#upper site: 24m x 24m plots = 1152 m2 combined plot area
#lower site: p1=12m x 12m; p2= 26m x 24 m; 768m2 combined plot area

current.mean
head(model.mean)

current.mean$vlf.mean.area <- current.mean$vlf.mean/768
current.mean$vlf.sd.area <- current.mean$vlf.sd/768
current.mean$vuf.mean.area <- current.mean$vuf.mean/1152
current.mean$vuf.sd.area <- current.mean$vuf.sd/1152

model.mean$vlf.mean.area <- model.mean$vlf.mean/768
model.mean$vlf.sd.area <- model.mean$vlf.sd/768
model.mean$vuf.mean.area <- model.mean$vuf.mean/1152
model.mean$vuf.sd.area <- model.mean$vuf.sd/1152

mean.area.plot<- ggplot()  +
  # plotting total site basal area
  
  geom_ribbon(data=model.mean, aes(x=year, ymin=(vlf.mean.area - 1.96*vlf.sd.area), ymax=(vlf.mean.area + 1.96*vlf.sd.area)), alpha=0.15, fill="red")+
  geom_ribbon(data=model.mean, aes(x=year, ymin=(vuf.mean.area - 1.96*vuf.sd.area), ymax=(vuf.mean.area + 1.96*vuf.sd.area)), alpha=0.15, fill="blue")+
  
  geom_line(data=model.mean,  aes(x=year, y=vlf.mean.area), size=1.5, colour="red") +
  geom_line(data= model.mean, aes(x=year, y=vuf.mean.area), size=1.5, colour="blue") +
  
  
  geom_point(data=current.mean, aes(x=2012, y=vlf.mean.area), size=4, colour="red")+
  geom_point(data=current.mean, aes(x=2012, y=vuf.mean.area), size=4, colour="blue")+
  
  
  geom_errorbar(data=current.mean,aes(x=2012, ymin=(vlf.mean.area - 1.96*vlf.sd.area), ymax=(vlf.mean.area + 1.96*vlf.sd.area)), color="red")+
  geom_errorbar(data=current.mean, aes(x=2012, ymin=(vuf.mean.area - 1.96*vuf.sd.area), ymax=(vuf.mean.area + 1.96*vuf.sd.area)), color="blue")+
  
  # all of that theme stuff you can just pre-set
  poster.theme
  #theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12))
#scale_fill_discrete(name="Model", labels = c("nt.pipo.sum", "nt.piaz.sum", "nt.pine.spp", "nt.vcnp.sum", "nt.pine.dom.sum"))

# telling what colors to make the lines for species
#scale_color_manual(values=c("red", "blue", "orange", "green")) 
mean.area.plot+ggtitle("Modeled Biomass")+scale_y_continuous("kg Biomass m-2")+scale_x_continuous("Year")

##getting differences between modeled and measured biomass for 2012
difference.bm<- as.data.frame(current.mean$vlf.mean.area - model.mean$vlf.mean.area[model.mean$year=="2011"])
names(difference.bm)<- c("vlf.diff")

difference.bm$vuf.diff <- current.mean$vuf.mean.area - model.mean$vuf.mean.area[model.mean$year=="2011"]
names(difference.bm)<- c("vlf.diff", "vuf.diff")

difference.bm$vlf.perc <- difference.bm$vlf.diff/current.mean$vlf.mean.area
difference.bm$vuf.perc <- difference.bm$vuf.diff/current.mean$vuf.mean.area

vuf.bm.means$perc.diff <- (vuf.bm.means$difference/vuf.bm.means$biomass)

########################################################################################
#Calculating the Interannual differences in the modeled biomass means
########################################################################################
head(model.mean)
inter.diff<-as.data.frame(model.mean)
#names(inter.diff)<-c("year")

model.mean[model.mean == 0] <- NA

inter.diff[,2:ncol(inter.diff)]<- NA

for(j in 2:ncol(model.mean)) {
    for( i in 2:length(inter.diff[,j])){
      inter.diff[i,j] <- ifelse(i-1 > 0, model.mean[i,j] - model.mean[i-1,j], NA) 
    }
         }
model.mean    
inter.diff2<-inter.diff[,2:ncol(inter.diff)]*1000   
inter.diff2$year <- inter.diff$year


plot(inter.diff2$vlf.mean.area ~ inter.diff2$year, type="l", xlim=c(2000,2011))
plot(inter.diff2$vuf.mean.area ~ inter.diff2$year, type="l", xlim=c(2000,2011))

##############################
#need interannual differences for each site
#to do this need to take the difference of each cumulative model and THEN take the mean

vlf.year2[vlf.year2==0]<-NA
vuf.year2[vuf.year2==0]<-NA

#vlf.year2[vlf.year2==0] <-NA
vlf.model.diff<- as.data.frame(vlf.year2)

vlf.model.diff[,2:ncol(vlf.model.diff)]<-NA



for(j in 2:ncol(vlf.year2)) {
  for( i in 2:length(vlf.model.diff[,j])){
    vlf.model.diff[i,j] <- ifelse(i-1 > 0, vlf.year2[i,j] - vlf.year2[i-1,j], NA) 
  }
}

model.mean.diff<-as.data.frame(model.mean)
model.mean.diff[,2:ncol(model.mean.diff)]<-NA

for(i in 1:length(vlf.model.diff$year)){
  model.mean.diff[i, "vlf.mean"] <- mean(vlf.model.diff[i,"nt.pipo.sum"], vlf.model.diff[i,"nt.piaz.sum"], vlf.model.diff[i,"nt.pine.spp.sum"],vlf.model.diff[i,"nt.pine.dom.sum"], na.rm=T)
  model.mean.diff[i, "vlf.sd"] <- sqrt(vlf.model.diff[i,"nt.pipo.sd"]^2+ vlf.model.diff[i,"nt.piaz.sd"]^2+ vlf.model.diff[i,"nt.pine.spp.sd"]^2+ vlf.model.diff[i,"nt.pine.dom.sd"]^2)
}
summary(model.mean.diff)



vuf.model.diff<- as.data.frame(vuf.year2)

vuf.model.diff[,2:ncol(vuf.model.diff)]<-NA



for(j in 2:ncol(vuf.year2)) {
  for( i in 2:length(vuf.model.diff[,j])){
    vuf.model.diff[i,j] <- ifelse(i-1 > 0, vuf.year2[i,j] - vuf.year2[i-1,j], NA) 
  }
}

for(i in 1:length(vlf.model.diff$year)){
  model.mean.diff[i, "vuf.mean"] <- mean(vuf.model.diff[i,"nt.spruce.sum"], vuf.model.diff[i,"nt.vcnp.sum"], vuf.model.diff[i,"nt.mixed.con.sum"], na.rm=T)
  model.mean.diff[i, "vuf.sd"] <- sqrt(vuf.model.diff[i,"nt.spruce.sd"]^2+vuf.model.diff[i,"nt.vcnp.sd"]^2+ vuf.model.diff[i,"nt.mixed.con.sd"]^2)
}

summary(model.mean.diff)

model.mean.diff$vlf.mean.area <- model.mean.diff$vlf.mean/768
model.mean.diff$vlf.sd.area <- model.mean.diff$vlf.sd/768
model.mean.diff$vuf.mean.area <- model.mean.diff$vuf.mean/1152
model.mean.diff$vuf.sd.area <- model.mean.diff$vuf.sd/1152

summary(model.mean.diff)
summary(inter.diff2)
plot(model.mean.diff$vlf.mean.area ~ inter.diff2$year, type="l", xlim=c(2000,2011))
plot(model.mean.diff$vuf.mean.area ~ inter.diff2$year, type="l", xlim=c(2000,2011))

###ROSS START HERE NEXT TIME FOR PROOF READING AND ANNOTATION!!!
#######################################
#writing csv's for the files used in the bootstrapping process, and the files produced by the bootstrap
#######################################
write.csv(vlf.year2, "vlf.boot.biomass.csv")
write.csv(vuf.year2, "vlf.boot.biomass.csv")
write.csv(dbh.recon.vuf.tree, "vuf.stacked.dbh.csv")
write.csv(dbh.recon.vlf.tree, "vlf.stacked.dbh.csv")