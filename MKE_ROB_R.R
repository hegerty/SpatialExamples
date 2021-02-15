#Creating counts of points in polygons
#Here: Robberies per block group in Milwaukee, 2014

#You can also get multiple packages at once
#install.packages("ctv")
#library(ctv)
#install.views("Spatial")

#Run the first script if you have to
source("I:/MKE_R_GEO/R_Script.R")
dev.off()
setwd("I:/")

###Check lines 17 and 46 for hidden errors
#Overlay Robberies over data to get totals in each block group
#over() gives point-->Polygon
#over(Robberies,data)

#CRS needs to be the same: 
#Make same CRS and redo
Robberies@proj4string
data@proj4string
data@proj4string<-Robberies@proj4string
count<-over(Robberies,data)

#extract and add to dataframe
head(count)
table(count$GEOID)
as.numeric(table(count$GEOID))
data@data$RobCount<-as.numeric(table(count$GEOID))

#check your results
head(data@data)

#Check colnames: RobCount should be #10 
colnames(data@data)
#Make a choropleth map for count: Make breaks here too
rc<-data$RobCount
breaks2<-classIntervals(data@data[,10],n=8,style="jenks")
breaks2<-breaks2$brks
choropleth(sp=data,v = rc, shading(breaks2,cols = colors),  main="Number of Milwaukee Robberies, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)
#Notice large block groups = high counts

#Now get density per unit of land area
#data@data$RobDens<-data@data$RobCount/data@data$ALAND
#data@data$RobDens<-NULL

#Turn integer-->numeric
#as.numeric() does not turn the integer into the correct values
head(data@data$ALAND)
head(as.numeric(data@data$ALAND))

#So here is a workaround
data@data$ALAND2<-as.numeric(as.character(data@data$ALAND))
#Now it works
data@data$RobDens<-data@data$RobCount/data@data$ALAND2
head(data@data)

#Check histograms: Density and Count
hist(data$RobDens)
hist(data$RobCount)

#check column position--RobDens should be column 12
colnames(data@data)

#make breaks for RobDens
breaks3<-classIntervals(data@data[,12],n=8,style="jenks")
breaks3<-breaks3$brks

#Choropleth map for density
rd<-data$RobDens
choropleth(sp=data,v = rd, shading(breaks3,cols = colors),  main="Density of Milwaukee Robberies, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)

jpeg("MKEROBDENS.jpg",width=9,height=9,units="in",res=300)
choropleth(sp=data,v = rd, shading(breaks3,cols = colors),  main="Density of Milwaukee Robberies, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)
dev.off()


