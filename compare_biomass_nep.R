 "MMF_site_BM_inc.csv")
"MMF_site_BM_cum.csv")


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

#  acsa.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,2], end = 2014, frequency = 1)
#  saal.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,3], end = 2014, frequency = 1)
#  frax.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,4], end = 2014, frequency = 1)
#  quru.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,5], end = 2014, frequency = 1)
#  qual.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,6], end = 2014, frequency = 1)
#  fagr.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,7], end = 2014, frequency = 1)
#  ulru.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,8], end = 2014, frequency = 1)
#  litu.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,9], end = 2014, frequency = 1)
#  tiam.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,10], end = 2014, frequency = 1)
#  pogr.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,11], end = 2014, frequency = 1)
#  astr.morgan.increment <- ts(read.csv("MMF_bm_spp_inc.csv", header = T)[,12], end = 2014, frequency = 1)
 


 
 
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
 
 
head(bio.mmf.all)
################################################

nep.niwot.annual <- ts(apply(nep.niwot,1,sum,na.rm = T),end = 2012, frequency = 1)
nep.morgan.annual <- ts(apply(nep.morgan,1,sum,na.rm = T),end = 2005, frequency = 1)

forcomp.niwot <- window(ts.union(nep.niwot.annual, bio.niwot.all*0.5),start = 1999,end = 2012)  # multiplying the TR BMinc. to get the amount of carbon; lets us compare with the tower.
forcomp.morgan <- window(ts.union(nep.morgan.annual, all.morgan.increment*0.5),start = 1999,end = 2005)

dim(forcomp.morgan)
 
 summary(forcomp.niwot)
par(new=F)
par(cex=2, tck=0.03) 
plot(forcomp.niwot[,2], type="l", lty="dotdash", col="darkgreen", xlim=c(1999,2012),ylim=c(0,600), ylab="NEP (gC/m2)", lwd=3, yaxt="n") 
 axis(2,las=1)
 par(new=T)
 plot(forcomp.morgan[,2], type="l", col="red1",lty="dotdash", xlim=c(1999,2012), ylim=range(0,600), axes=F, ylab="", xlab="", lwd=3)
 par(new=T)
 plot(forcomp.niwot[,1], type="l",  col="darkgreen",xlim=c(1999,2012), ylim=c(0,600), axes=F, ylab="", xlab="", lwd=3)
 par(new=T)
 plot(forcomp.morgan[,1], type="l", col="red1", xlim=c(1999,2012), ylim=range(0,600), axes=F, ylab="", xlab="", lwd=3)
 legend("topright", legend= c("Niwot NEP", "Niwot Tree Rings", "MMF NEP", "MMF Tree Rings"), col= c("darkgreen","darkgreen","red1","red1"), cex=0.75, lty=c(1,4,1,4))
 
 
ratio.niwot <- forcomp.niwot[,2]/forcomp.niwot[,1]
ratio.morgan <- forcomp.morgan[,2]/forcomp.morgan[,1]

ratio.all <- window(ts.union(ratio.niwot, ratio.morgan))
 
plot(ratio.niwot, main="Niwot Ridge NEP:aNPP", ylim=c(0,1))
par(new=T)
 boxplot(ratio.morgan, main="MMF Ridge NEP:aNPP", ylim=c(0,1))

 
par(cex=1,mar=c(3,6,2,1))
boxplot(ratio.all, col=c("darkgreen","Red"), names =c("Niwot Ridge", "MMF"), ylab="NEP : aNPP", cex.axis = 2, cex.names=2, ylim=c(0,1), cex.lab=2)



plot(nep.niwot.annual, type="l", col="darkgreen", xlim=c(1998,2012), ylim=c(0, 1000), ylab="gC/m^2", cex.axis =2, cex.names=2, lwd=3)
par(new=T)
plot(nep.morgan.annual, type="l", col="red", xlim=c(1998, 2012), ylim=c(0, 1000), axes=F, ylab="", xlab="")
 
 
#  plot(bio.niwot*0.5, type="l", col="darkgreen", ylim=c(0, 350), xlim= c(1800, 2014),ylab="")
#  par(new=T)
#  plot(all.morgan.increment*0.5, type="l", col="red", xlim= c(1800, 2014), ylim=c(0, 350), ylab="gC/m^2")
#  
#  plot(all.morgan.increment*1000*0.5, type="l", col="black", xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(acsa.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(saal.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(frax.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(quru.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(qual.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(ulru.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(tiam.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(pogr.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  plot(astr.morgan.increment*1000*0.5, type="l", col=rainbow(11), xlim= c(1900, 2014), ylim=c(0, 500), ylab="gC/m^2")
#  par(new=T)
#  
 


