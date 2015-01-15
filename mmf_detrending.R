#standardizing MMF chronologies for climate
#uploading files
library(dplR)

acsa <- read.rwl("mmf_dated_maple.rwl")
fagr <-read.rwl("mmf_dated_beech.rwl")
red <-read.rwl("mmf_dated_redoak.rwl")
white <- read.rwl("mmf_dated_whiteoak.rwl")
litu <- read.rwl("mmf_dated_litu.rwl")
saal <- read.rwl("mmf_dated_sass.rwl")
frax <- read.rwl("mmf_dated_ash.rwl")

#detrend with 20 yr. spline to match Flurin

i.acsa <- detrend(acsa, method="Spline", nyrs= 20, f=0.5)
i.fagr <- detrend(fagr, method="Spline",nyrs= 20, f=0.5)
i.red <- detrend(red, method="Spline",nyrs= 20, f=0.5)
i.white <- detrend(white, method="Spline",nyrs= 20, f=0.5)
i.litu <- detrend(litu, method="Spline",nyrs= 20, f=0.5)
i.saal <- detrend(saal, method="Spline",nyrs=20, f=0.5)
i.frax <- detrend(frax, method="Spline",nyrs= 20, f=0.5)

chron.acsa<-chron(i.acsa)
chron.fagr<-chron(i.fagr)
chron.red<-chron(i.red)
chron.white<-chron(i.white)
chron.litu<-chron(i.litu)
chron.saal<-chron(i.saal)
chron.frax<-chron(i.frax)

write.csv(chron.acsa, "chron_maple.csv")
write.csv(chron.fagr, "chron_beech.csv")
write.csv(chron.red, "chron_redoak.csv")
write.csv(chron.white, "chron_whiteoak.csv")
write.csv(chron.saal, "chron_sassafras.csv")
write.csv(chron.frax, "chron_ash.csv")
write.csv(chron.litu, "chron_tulip.csv")
