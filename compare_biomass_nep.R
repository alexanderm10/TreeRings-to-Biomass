library(Hmisc)
library(vioplot)
library(lattice)
library(ggplot2)

nep.niwot <- ts(read.table("monthly_nep_niwot.txt", header = T), end = 2013, frequency = 1)

nep.morgan <- ts(read.table("monthly_nep_morgan.txt", header = T), end = 2005, frequency = 1)



bioB.niwot <- ts(read.table("site_biomass_increment_recon_plotB_kg_per_ha.txt",header = T)/10,end = 2012, frequency = 1)    #/10 to convert kg/ha into g/m2
bioC.niwot <- ts(read.table("site_biomass_increment_recon_plotC_kg_per_ha.txt",header = T)/10,end = 2012, frequency = 1)
bio.niwot.all <- ts(apply(ts.union(bioB.niwot[,1],bioC.niwot[,1]),1,mean,na.rm = T),end = 2012,frequency = 1)
# bio.niwot.fir <- bio.niwot.all <- ts(apply(ts.union(all.niwot[,2],bioC.niwot[,2]),1,mean,na.rm = T),end = 2012,frequency = 1)
#  bio.niwot.pine <- bio.niwot.all <- ts(apply(ts.union(all.niwot[,3],bioC.niwot[,3]),1,mean,na.rm = T),end = 2012,frequency = 1) 
#  bio.niwot.spruce <- bio.niwot.all <- ts(apply(ts.union(all.niwot[,4],bioC.niwot[,4]),1,mean,na.rm = T),end = 2012,frequency = 1)

 bio.niwot.all<- ts.union(bio.niwot.all, bio.niwot.fir, bio.niwot.spruce, bio.niwot.pine, end= 2012, frequency =1)
 summary(bio.niwot.all)
 
 
 all.morgan.increment <- ts(read.csv("MMF_site_BM_inc2.csv", header = T)[,2], end = 2014, frequency = 1)
summary(all.morgan.increment)

mmf.spp.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header=T) [,2:12], end=2014, frequency =1)

summary(mmf.spp.increment[,"ACSA"])

mmf.spp.increment[,"ACSA"]
 
 


# all.morgan.increment <- all.morgan
# all.morgan.increment[1] = 0
# 
# for(i in 2:length(all.morgan)){
# 
# all.morgan.increment[i] <- all.morgan[i]-all.morgan[(i-1)] #biomass increment from cumulative biomass
# 
# }
# bio.mmf.all <- ts.union(acsa.morgan.increment, saal.morgan.increment, frax.morgan.increment, quru.morgan.increment, 
#                     qual.morgan.increment, fagr.morgan.increment, ulru.morgan.increment, litu.morgan.increment,
#                     tiam.morgan.increment, pogr.morgan.increment, astr.morgan.increment, all.morgan.increment, end= 2014, frequency= 1)
# summary(bio.mmf.all) 
# bio.mmf.all <- bio.mmf.all*1000 #gets us into grams for the comparison with the tower

 all.morgan.increment <- all.morgan.increment*1000
 mmf.spp.increment <-mmf.spp.increment*1000
summary(all.morgan.increment) 
 summary(mmf.spp.increment) 



ts.plot(all.morgan.increment, col="red", lwd="4", ylim=c(0,800))
par(new=T)
ts.plot(mmf.spp.increment[,1:11], ylim=c(0,800))

mmf.plotting.inc <-ts.union(mmf.spp.increment, all.morgan.increment, end=2014) 
mmf.plotting.inc <-data.frame(mmf.plotting.inc) 
mmf.plotting.inc <-mmf.plotting.inc[,1:12]
names(mmf.plotting.inc)<- c("ACSA", "ASTR","FAGR", "FRAX", "LITU", "POGR", "QUAL", "QURU", "SAAL", "TIAM", "ULRU", "ALL")
row.names(mmf.plotting.inc) <- c(1903:2014)
head(mmf.plotting.inc)
################################################

nep.niwot.annual <- ts(apply(nep.niwot,1,sum,na.rm = T),end = 2012, frequency = 1)
nep.morgan.annual <- ts(apply(nep.morgan,1,sum,na.rm = T),end = 2005, frequency = 1)

forcomp.niwot <- window(ts.union(nep.niwot.annual, bio.niwot.all*0.5),start = 1999,end = 2012)  # multiplying the TR BMinc. to get the amount of carbon; lets us compare with the tower.
forcomp.morgan.all <- window(ts.union(nep.morgan.annual, all.morgan.increment*0.5),start = 1999,end = 2005)
forcomp.morgan.spp <- window(ts.union(nep.morgan.annual, mmf.spp.increment*0.5),start = 1999,end = 2005)
 
dim(forcomp.morgan.all) 
dim(forcomp.morgan.spp)
 
 summary(forcomp.niwot)

par(new=F)
par(cex=2, tck=0.03, mar=c(4, 4.25, 2.5, 1)) 
plot(forcomp.niwot[,2], type="l", lty="dotdash", col="darkgreen", xlim=c(1999,2012),ylim=c(0,600), ylab="NEP (gC/m2)", lwd=3, yaxt="n") 
 axis(2,las=1)
 par(new=T)
 plot(forcomp.morgan.all[,2], type="l", col="red1",lty="dotdash", xlim=c(1999,2012), ylim=range(0,600), axes=F, ylab="", xlab="", lwd=3)
 par(new=T)
 plot(forcomp.niwot[,1], type="l",  col="darkgreen",xlim=c(1999,2012), ylim=c(0,600), axes=F, ylab="", xlab="", lwd=3)
 par(new=T)
 plot(forcomp.morgan.all[,1], type="l", col="red1", xlim=c(1999,2012), ylim=range(0,600), axes=F, ylab="", xlab="", lwd=3, main="NEP Measurements and Estimates")
 legend(x= 2007.5,y=600, legend= c("Niwot Tower", "Niwot Tree Rings", "MMF Tower", "MMF Tree Rings"), col= c("darkgreen","darkgreen","red1","red1"), cex=0.75, lty=c(1,4,1,4),lwd=3, bty="n")
 
 
ratio.niwot <- forcomp.niwot[,2:5]/forcomp.niwot[,1]
ratio.morgan.all <- ts(forcomp.morgan.all[,2]/forcomp.morgan.all[,1], start=1999, end=2005)
ratio.morgan.spp <- ts(forcomp.morgan.spp[,2:12]/forcomp.morgan.spp[,1], start = 1999, end = 2005)

ratio.morgan.combo <- window(ts.union(ratio.morgan.all, ratio.morgan.spp), start = 1999, end =2005)

ratio.morgan.combo<-as.data.frame(ratio.morgan.combo)
head(ratio.morgan.combo)
names(ratio.morgan.combo)<- c(" All", "ACSA", "ASTR","FAGR", "FRAX", "LITU", "POGR", "QUAL", "QURU", "SAAL", "TIAM", "ULRU")
summary(ratio.morgan.combo)

# ratio.morgan.spp <- as.data.frame(ratio.morgan.spp)
# #names(ratio.morgan.spp)<- c("ACSA", "ASTR","FAGR", "FRAX", "LITU", "POGR", "QUAL", "QURU", "SAAL", "TIAM", "ULRU")
# head(ratio.morgan.spp )
# ratio.morgan.combo <- ratio.morgan.spp
# ratio.morgan.combo$all <- ratio.morgan.all
# head(ratio.morgan.combo)
# names(ratio.morgan.combo)<- c("ACSA", "ASTR","FAGR", "FRAX", "LITU", "POGR", "QUAL", "QURU", "SAAL", "TIAM", "ULRU", "ALL")
# head(ratio.morgan.combo)
# row.names(ratio.morgan.combo) <- c(1999:2005)
# head(ratio.morgan.combo)
# summary(ratio.morgan.combo) 

min(ratio.morgan.combo)

test <- data.frame(ratio.morgan.combo)
test <- stack(test)
dim(test)
ratio.morgan.stack <-stack(ratio.morgan.combo)
summary(ratio.morgan.stack)
names(ratio.morgan.stack)<- c("ratio", "Species")
summary(ratio.morgan.stack)

ratio.niwot <- as.data.frame(ratio.niwot)
row.names(ratio.niwot) <- c(1999:2012)
names(ratio.niwot) <- c("All SPP", "Fir", "Spruce", "Pine")
head(ratio.niwot)

ratio.niwot.stack <- stack(ratio.niwot)
head(ratio.niwot.stack)
names(ratio.niwot.stack)<-c("ratio", "Species")

dim(ratio.morgan.combo)
dim(ratio.morgan.stack)



# Making violin plots of the ratios to show the percentage of NEP each spp contributes
large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines"))

poster.theme<-theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                    panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=21),
                    axis.text.y=element_text(angle=0, color="black", size=24), axis.title.x=element_text(face="bold", size=28),
                    axis.title.y=element_text(face="bold", size=28), strip.text=element_text(face="bold", size=rel(1.75)),
                    title=element_text(face="bold", size=30))
niwot.shapes <- c(15:18)

ggplot(data=ratio.niwot.stack, aes(Species, ratio)) + 
  geom_violin(adjust=2, scale="width") + 
  scale_shape_manual(values=niwot.shapes) +
  geom_point(position=position_jitter(width=0.15, height=0.0001), aes(shape=Species, color=Species), size=8) + 
  large.axes +
  poster.theme + 
  theme(legend.position=c(0.95,0.8), legend.text=element_text(size=24), legend.title=element_text(size=28), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + 
  labs(title="Niwot Ridge") +
  theme(axis.title.y=element_text(vjust=2))+
  #theme(axis.title.x=element_text(vjust=0))+
  scale_y_continuous(name="NEP:aNPP") + scale_x_discrete(name="Species") + 
  stat_summary(fun.y=mean, geom="point", shape="_", size=15, color="black") 
  

# mmf.colors <- read.csv("SpeciesColors.csv", header=T)
mmf.shapes <- read.csv("SpeciesShapes.csv", header=T)

mmf.shapes <- c(10, 15:20, 15:19)
mmf.colors <- c("dodgerblue", "purple", "wheat3", "cadetblue3", "orange4", "darkgreen", "hotpink1", "black", "red1", "brown", "goldenrod1", "medium purple1")
# dodgerblue
# purple
# wheat3
# cadetblue3
# orange4
# darkolivegreen2
# hotpink1
# black
# red2
# brown
# goldenrod1
# mediumpurple1


dim(mmf.colors)
#MMF
large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines"))

ggplot(data=ratio.morgan.stack, aes(Species, ratio)) + 
  geom_violin(adjust=1, scale="width") + 
  scale_shape_manual(values=mmf.shapes) +
  scale_color_manual(values=mmf.colors) +
  geom_point(position=position_jitter(width=0.25, height=0.0001), aes(shape=Species, color=Species), size=8) + 
  poster.theme + 
  theme(legend.position=c(0.95,0.8), legend.text=element_text(size=20), legend.title=element_text(size=20), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + 
  theme(axis.title.y=element_text(vjust=1.5))+
  theme(axis.title.x=element_text(vjust=-0.1))+
  labs(title="Morgan-Monroe")+
  scale_y_continuous(name="aNPP : NEP") + scale_x_discrete(name="Species") + 
  stat_summary(fun.y=mean, geom="point", shape="_", size=15, color="black") 

#################################################################
# Plotting out time series for graphics
#################################################################

df.bio.niwot.all <- data.frame(bio.niwot.all*0.5)
names(df.bio.niwot.all)<- c("all.spp", "fir", "spruce", "pine")
row.names(df.bio.niwot.all) <- c(1700:2012)

df.all.morgan.inc <-data.frame(all.morgan.increment*0.5)
names(df.all.morgan.inc) <- c("all.spp")
row.names(df.all.morgan.inc) <- c(1903:2014)

par(new=F)
par(cex=2, tck=0.03, mar=c(4, 4.25, 2.5, 1)) 
plot(df.bio.niwot.all[,1] ~ row.names(df.bio.niwot.all), type="l", col="darkgreen", xlim=c(1900,2014),ylim=c(0,600),  lwd=3, yaxt="n", xlab="Year",ylab="aNPP (gC/m^2", main="aNPP estimates from Tree Rings") 
axis(2,las=1)
par(new=T)
plot(df.all.morgan.inc[,1], type="l", col="red1", xlim=c(1900,2014), ylim=range(0,600), axes=F, ylab="", xlab="", lwd=3)

legend("topright", legend= c("Niwot Ridge", "Morgan-Monroe"), col= c("darkgreen","red1"), cex=0.75, lty=c(1,4,1,4),lwd=3, bty="n")

