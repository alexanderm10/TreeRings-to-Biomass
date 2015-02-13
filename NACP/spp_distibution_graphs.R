
niwot.tree.data <- read.csv("niwot.tree.data.csv", header=T)



dbh.bins1 <- seq(10, max(tree.data$dbh[tree.data$site == "Morgan Monroe State Park"], na.rm=T), 5) # 5 cm bins based on the range of your trees

dbh.bins2 <- seq(10, max(niwot.tree.data$dbh), 5) # 5 cm bins based on the range of your trees

poster.theme<-theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                    panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=21),
                    axis.text.y=element_text(angle=0, color="black", size=24), axis.title.x=element_text(face="bold", size=28),
                    axis.title.y=element_text(face="bold", size=28), strip.text=element_text(face="bold", size=rel(1.75)),
                    
mmf.colors2 <- c("purple", "wheat3", "cadetblue3", "orange4", "darkgreen", "hotpink1", "black", "red1", "brown", "goldenrod1", "medium purple1")
                    #for the nested sampling design we might want to diagram how much dead stuff we have.  I think this would be easy enough to do, since it is already included in our fieldnotes.
                    # Plotting species by size distribution
                    
qplot(x=dbh, data=tree.data[tree.data$site == "Morgan Monroe State Park",], geom="histogram", breaks=dbh.bins1, fill=species) + facet_grid(site ~ .) + 
                      scale_fill_manual(values=mmf.colors2) +
                      theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + 
                      scale_x_continuous(name="DBH (cm)", limit=c(0,100)) + ggtitle("Morgan-Monroe")+ scale_y_continuous(name="Count")+ #+ scale_fill_manual(values=as.vector(species.col.tree$Color))
                      poster.theme
                    
niwot.colors2 <- c("olivedrab", "darkturquoise", "mediumorchid")

qplot(x=dbh, data=niwot.tree.data, geom="histogram", breaks=dbh.bins2, fill=species) +  
  scale_fill_manual(values=niwot.colors2) +
  theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + 
  scale_x_continuous(name="DBH (cm)", limit=c(0,100)) + ggtitle("Niwot Ridge")+ scale_y_continuous(name="Count")+ #+ scale_fill_manual(values=as.vector(species.col.tree$Color))
  poster.theme
