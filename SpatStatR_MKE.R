##More advanced analysis: Weight matrix, Spatial autocorrelation, Getis-Ord G*, Spatial Regression

#If you need to, run MKE_ROB_R.R
#I intentionally include two errors, but have hidden them in that file
#Change your folders as necessary
source("F:/MKE_ROB_R.R")
dev.off()
setwd("F:/")

#Make a weights matrix
#Simple matrices are recommended: Here, Queen contiguity of order 1 (row-normalized)
#install.packages("spdep")
library(spdep)
q1<-poly2nb(data,queen = TRUE)
q1<-nb2listw(q1,style = "W",zero.policy = TRUE)

#Moran's I for Robbery Density using weights
moran.test(rd,q1)
moran.plot(rd,q1,labels = FALSE)

#Calculate Getis-Ord "Hot Spots"
data@data$Gstat<-localG(rd,q1)
gstat<-data$Gstat
head(data@data)

#Set breaks at standard confidence levels
breaksg<-c(-1.96,-1.645,1.645,1.96)
colorsg<-c("blue","cyan","white","magenta","red")

#make a choropleth map
choropleth(sp=data,v = gstat, shading(breaksg,cols = colorsg),  main="Robbery Hot/Cold Spots in Milwaukee, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)
legend("left",title="Getis-Ord G-statistic",legend=c("Sig. Neg. (5%)","Sig. Neg. (10%)","Insignificant","Sig. Pos. (10%)","Sig. Pos. (5%)"),fill=colorsg)

#A Simple bivariate spatial lag regression
#Robbery density on deprivation
#reg1<-lagsarlm(data@data$RobDens~data@data$DEPRIVATIO,listw = q1)
reg1<-lagsarlm(RobDens~DEPRIVATIO,data = data,listw = q1)
summary(reg1)

#Maybe add an additional variable?
#Check for highly correlated variables
#(You would really need more data)
colnames(data@data)
data@data[,9]<-as.numeric(data@data[,9])
cor(data@data[,7:10])

reg2<-lagsarlm(RobDens~DEPRIVATIO+PERCVAC,data = data,listw = q1)
summary(reg2)

#Hi-Res "Hot Spot" Map
jpeg("MKE_Rob_G.jpg",width=9,height=9,units="in",res=300)
choropleth(sp=data,v = gstat, shading(breaksg,cols = colorsg),  main="Robbery Hot/Cold Spots in Milwaukee, 2014")
plot(CityLine,col="black",lwd=4,lty=1,add=TRUE)
legend("left",title="Getis-Ord G-statistic",legend=c("Sig. Neg. (5%)","Sig. Neg. (10%)","Insignificant","Sig. Pos. (10%)","Sig. Pos. (5%)"),fill=colorsg)
dev.off()