#standardizing MMF chronologies for climate
#uploading files
library(dplR)

acsa <- read.rwl("mmbacsa03.rwl")
fagr <-read.rwl("mmfbeech04.rwl")
red <-read.rwl("mmfred02.rwl")
white <- read.rwl("mmfwhite08.rwl")
litu <- read.rwl("mmflitu01.rwl")
saal <- read.rwl("mmfsass01.rwl")
frax <- read.rwl("mmfash02.rwl")

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
