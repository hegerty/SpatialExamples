#Get any packages you might need
#install.packages("sp")
#install.packages("rgdal")
#install.packages("GISTools")
#install.packages("classInt")
#install.packages("RColorBrewer")

#Load the packages
library(sp)
library(rgdal)
library(GISTools)
library(classInt)
library(RColorBrewer)

#Read and create data; be sure to change your directory here

setwd("I:/MKE_R_GEO")
Robberies<- readOGR("Robberies.shp")
CityLine<- readOGR("MKECityLine2.shp")
data<- readOGR("MKEDATA2.shp")

#Check the data
head(data@data)
colnames(data@data)

#Plot maps individually
plot(CityLine)
plot(Robberies)
plot(data)

#Or all three in one; 3 columns x 1 row
par(mfrow=c(1,3))
plot(CityLine)
par(new=FALSE)
plot(Robberies)
par(new=FALSE)
plot(data)

#check projections
data@proj4string
Robberies@proj4string
CityLine@proj4string

#Plot Deprivation Index--it is column 7; name it "depr" as well
depr<-data$DEPRIVATIO
depr[is.nan(depr)]<-0
depr[is.na(depr)]<-0

#Also check the distribution of the variable; make it 1x1 again instead of 3 columns
par(mfrow=c(1,1))
hist(depr)

#Gives you a basic choropleth map
spplot(data[,7])

#make it a little nicer
#9 colors with 8 breaks in between
#Choose a ramp from white-->black
colors<-brewer.pal(9,"Greys")
spplot(data[,7],col.regions=colors,cuts=8, main="Economic Deprivation, Milwaukee, 2014")

#Now the robberies, including the city borders
plot(CityLine,col="black",lwd=4,lty=1,main="Milwaukee Robberies, 2014")
plot(Robberies,pch=16,cex=0.5,add=TRUE)

#Change the shape of Milwaukee; copy a projection
CityLine@proj4string<-Robberies@proj4string
plot(CityLine,col="black",lwd=4,lty=1,main="Milwaukee Robberies, 2014")
plot(Robberies,pch=16,cex=0.5,add=TRUE)

#Add block groups
plot(data[,7],main="Milwaukee Robberies, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)
plot(Robberies,pch=16,cex=0.5,add=TRUE)

#Now make a choropleth map of economic deprivation
#and add the robberies

#Need colors and breaks
#Colors are done above; now do breaks
#Natural breaks are used here
#Need to separate the values out from the results
breaks1<-classIntervals(data@data[,7],n=8,style="jenks")
breaks1
breaks1<-breaks1$brks
breaks1

#First just make the map print in RStudio
library(GISTools)
choropleth(sp=data,v = depr, shading(breaks1,cols = colors),  main="Milwaukee Deprivation Index and Robberies, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)
points(Robberies,col="red",pch=16,cex=0.01)

################################################################################
#Print to file: The only change is the point size on the robberies
jpeg("MKEMAP.jpg",width=9,height=9,units="in",res=300)
choropleth(sp=data,v = depr, shading(breaks1,cols = colors),  main="Milwaukee Deprivation Index and Robberies, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)
points(Robberies,col="red",pch=16,cex=0.3)
dev.off()








