

nep.niwot <- ts(read.table("monthly_nep_niwot.txt", header = T), end = 2013, frequency = 1)



nep.morgan <- ts(read.table("monthly_nep_morgan.txt", header = T), end = 2005, frequency = 1)



bioB.niwot <- ts(read.table("site_biomass_increment_recon_plotB_kg_per_ha.txt",header = T)/10,end = 2012, frequency = 1)    #/10 to convert kg/ha into g/m2
bioC.niwot <- ts(read.table("site_biomass_increment_recon_plotC_kg_per_ha.txt",header = T)/10,end = 2012, frequency = 1)
bio.niwot <- ts(apply(ts.union(bioB.niwot[,1],bioC.niwot[,1]),1,mean,na.rm = T),end = 2012,frequency = 1)

setwd( "C:/Users/babst/Desktop/post_doc_Tucson/ameriflux_sites/morgan_monroe")

bioB.morgan <- ts(read.csv("mmf_biomass_kgm2.csv", header = T)[,3], end = 2014, frequency = 1)
bioB.morgan.increment <- bioB.morgan
bioB.morgan.increment[1] = 0

for(i in 2:length(bioB.morgan)){
  
  bioB.morgan.increment[i] <- bioB.morgan[i]-bioB.morgan[(i-1)] #biomass increment from cumulative biomass
  
}

bioB.morgan.increment <- bioB.morgan.increment*1000


################################################

nep.niwot.annual <- ts(apply(nep.niwot,1,sum,na.rm = T),end = 2012, frequency = 1)
nep.morgan.annual <- ts(apply(nep.morgan,1,sum,na.rm = T),end = 2005, frequency = 1)

forcomp.niwot <- window(ts.union(nep.niwot.annual, bio.niwot*0.5),start = 1999,end = 2012)  # multiplying the TR BMinc. to get the amount of carbon; lets us compare with the tower.
forcomp.morgan <- window(ts.union(nep.morgan.annual, bioB.morgan.increment*0.5),start = 1999,end = 2005)

ratio.niwot <- forcomp.niwot[,2]/forcomp.niwot[,1]
ratio.morgan <- forcomp.morgan[,2]/forcomp.morgan[,1]

par(new=F)
boxplot(ratio.niwot, main="NEP:aNPP", ylim=c(0,1))




