library(fields)

mmf.colors2 <- c("orange4", "purple", "black", "darkgreen", "red1", "cadetblue3", "goldenrod")
  
  
niwot.colors2 <- c("olivedrab", "darkturquoise", "mediumorchid")
setwd("C:/Users/babst/Desktop/post_doc_Tucson/ameriflux_sites/niwot_ridge/correlations_Ross")

data <- vector(mode = "list", length = length(list.files()))
for(i in 1:length(data)){load(list.files()[i])}


setwd("C:/Users/babst/Desktop/post_doc_Tucson/ameriflux_sites/niwot_ridge/preliminary_results")

### 1) Clim vs. flux



title <- c("MMF monthly T vs. NEP", "MMF monthly P vs. NEP","MMF monthly rad vs. NEP","MMF monthly Q vs. NEP")
par(mfrow = c(2,2))
for(i in 1:length(clim.cor.morgan.flux[[1]])){
image.plot(clim.cor.morgan.flux[[1]][[i]][,13:24], zlim = c(-1,1), main = title[i])
}





title <- c("Niwot monthly T vs. NEP", "Niwot monthly P vs. NEP","Niwot monthly rad vs. NEP","Niwot monthly Q vs. NEP")
par(mfrow = c(2,2))
for(i in 1:length(clim.cor.niwot.flux[[1]])){
image.plot(clim.cor.niwot.flux[[1]][[i]][,13:24], zlim = c(-1,1), main = title[i])
}





### 2) Clim vs. tree rings

names(clim.cor.morgan.tr) #this shows you the different objects in the list (the way I named them when I created it)

# Morgan Monroe Climate correlations with tree ring index
# monthly of current growing season had most interesting results
title <- c("Temperature", "Precipitation","SW Radiation","Air Humidity")
par(mfrow = c(4,1))

for(i in 1:length(clim.cor.morgan.tr[[12]])){             #the 12 stands for the 1980-2010 period, we could also take another one...
barplot(clim.cor.morgan.tr[[12]][[i]][,c(26,28:29,31:32)], beside = T, ylim = c(-0.6,0.6), 
        col = ifelse(clim.cor.morgan.tr[[12]][[i]][,c(26,28:29,31:32)] > 0.355 | clim.cor.morgan.tr[[12]][[i]][,c(26,28:29,31:32)] < -0.355, 
                     mmf.colors2, "white"), 
        border = mmf.colors2,  
        main = title[i], cex.main=2, cex.axis = 1.25, cex.names=2, yaxt="n")
        axis(2, las=2)
#not sure about the color, maybe you wanna play with this yourself a bit

#abline(v = c(72.5,144.5), lwd=3)
}



barplot(clim.cor.morgan.tr[[12]][[1]], col="white", border="white")
ifelse(i == 4, legend(x=-1,y=1, legend= c("FRAX", "ACSA", "QUAL", "LITU",
                                         "QURU", "FAGR", "SAAL"), col= mmf.colors2, 
                      pch=15, horiz=T, cex=1.5),"")




# c(4:21,26,28:29,31:32)], full two yearbreak down plus seasons
#niwot ridge, climate vs. ring width index, 1980-2010
# seasonal correlations were most interesting
title <- c("Temperature", "Precipitation","SW Radiation","Air Humidity")
par(mfrow = c(4,1), mar=c(6,4,2,0))

for(i in 1:length(clim.cor.niwot.tr[[12]])){
  barplot(clim.cor.niwot.tr[[12]][[i]][2:4,c(26,28:29,31:32)], beside = T, ylim = c(-0.6,0.6), 
          col = ifelse(clim.cor.niwot.tr[[12]][[i]][2:4,c(26,28:29,31:32)] > 0.355 | clim.cor.niwot.tr[[12]][[i]][2:4,c(26,28:29,31:32)] < -0.355,niwot.colors2, "white"),
          border = niwot.colors2,  
          main = title[i], cex.main=2, cex.axis = 1.25, cex.names=2, yaxt="n")
          axis(2, las=2)  #not sure about the color, maybe you wanna play with this yourself a bit
  abline(v = c(36.5,72.5), lwd=3)
  
}
# clim.cor.niwot.tr[[12]][[i]][2:4,c(4:21,26,28:29,31:32)]
barplot(clim.cor.niwot.tr[[12]][[1]], col="white", border="white")
ifelse(i == 4, legend(x=-1,y=1, legend= c("Fir", "Pine", "Spruce"), col= niwot.colors2, pch=15, ncol=4, cex=1.75),"")





### 3) Flux vs. tree rings

names(flux.cor.morgan.tr) #this shows you the different objects in the list (the way I named them when I created it)



par(mfrow=c(1,1))
title <- c("MMF NEP vs. TR")
barplot(flux.cor.morgan.tr[[1]][,c(4:21,26,28:29,31:32)], beside = T, ylim = c(-1,1),  
        col = ifelse(flux.cor.morgan.tr[[1]][,c(4:21,26,28:29,31:32)] > 0.755 | flux.cor.morgan.tr[[1]][,c(4:21,26,28:29,31:32)] < -0.755, 
                     c("orange3", "purple3", "black", "green3", "red1", "dodgerblue", "hotpink"), "white"), 
        border = c("orange3", "purple3", "black", "green3", "red1", "dodgerblue", "hotpink"),
        main = title)  #not sure about the color, maybe you wanna play with this yourself a bit
abline(v = c(72.5,144.5))




nameofpicturefile = "correls_flux_TR_niwot.pdf"
pdf(file = nameofpicturefile, width = 15, height = 5)

title <- c("Niwot NEP vs. TR")
barplot(flux.cor.niwot.tr[[1]][2:4,c(4:21,26,28:29,31:32)], beside = T, ylim = c(-1,1), col = ifelse(flux.cor.niwot.tr[[1]][2:4,c(4:21,26,28:29,31:32)] > 0.514 | flux.cor.niwot.tr[[1]][2:4,c(4:21,26,28:29,31:32)] < -0.514,c("red", "blue", "darkorange1"), "white"),
        border = c("red", "blue", "darkorange1"), main = title)  #not sure about the color, maybe you wanna play with this yourself a bit
abline(v = c(36.5,72.5))








