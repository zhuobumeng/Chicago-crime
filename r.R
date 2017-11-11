A <- matrix(0,6,5)
A[,1] = c(35,31,33,34,28,34)
A[,2] = c(25,39,67,41,39,54)
A[,3] = c(26,37,64,44,28,53)
A[,4] = c(132,243,280,375,337,299)# mean difference
A[,5] = sqrt(A[,2]^2+A[,3]^2)
A[,5] = sqrt(A[,2]^2+A[,3]^2+2*sqrt(A[,2]*A[,3]))
A[,5] = 1.96*A[,5]

yu = c(610,685,694,750,732,710)
yse = c(25,39,67,41,39,54)
tu = c(478,442,414,375,395,411)
tse = c(26,37,64,44,28,53)
ni = c(16,14,17,14,9,14)

E = sum((yu-tu)*ni)/sum(ni)
n=sum(ni)
se = sqrt(sum((yse+tse)^2*ni^2)/n^2)

colnames(A) = c("Body size","se of Yellow","se of Trans.","mean diff.","95% CI")
kable(A,"markdown",digits=2,align="c")

x = rnorm(50)
y = rnorm(100)
boxplot(x,y,names=c("x","y"))


###########################
## Problem 2
###########################

library(ggplot2)
library(plotrix)
library(knitr)
library(ggmap)
library(lubridate)
library(proj4) 
library(maptools) 
library(scales)
library(gridExtra)


crime = read.csv("ChicagoTheft.csv")
IUCR.name = read.csv("http://www.stat.uchicago.edu/~nicolae/prelims/2014/IUCRCodes.csv")
hw = data.frame(crime,count=rep(1,dim(crime)[1]))

######### a

daytmp = substring(as.character(hw$Date),1,10)
day = as.Date(daytmp,"%m/%d/%Y")
month = as.Date(format(day,"%Y-%m-01"))
# h2tmp = data.frame(hw,day,month,mday)
# h2 = merge(h2tmp,IUCR.name[,1:2],by="IUCR",all.x=T)
h2 = merge(hw,IUCR.name[,1:2],by="IUCR",all.x=T)
colnames(h2)[colnames(h2)=="PRIMARY.DESCRIPTION"] = "descr"

daymatrix = aggregate(count~day+descr,data=h2,sum)
# mday, describe the temporal trend in one year
monthmatrix = aggregate(count~month+descr,data=h2,sum)
mday = as.Date(format(daymatrix$day,"2016-%m-%d"))
mdaytmp = data.frame(daymatrix,mday)
mdaymat = aggregate(count~mday+descr,data=mdaytmp,mean)

pa1 = ggplot(daymatrix,aes(x=day,y=count,color=factor(descr))) + geom_point() +
  scale_colour_manual("Primary Description", values=c("#2b83ba", "#abdda4","#d7191c","#fdae61")) +
  xlab("day") + ylab("occurrence number in each day")
pa1


pa2 = ggplot(monthmatrix,aes(x=month,y=count,color=factor(descr))) + geom_line(size=1.2) +
  scale_colour_manual("Primary Description", values=c("#2b83ba", "#abdda4","#d7191c","#fdae61")) +
  xlab("month") + ylab("occurrence number in each month") #+ scale_x_date(date_breaks = "6 month", date_labels = "%y%m")
pa2

pdf(file="2a.pdf",width=10,height=4)
pa2
dev.off()

pa3 = ggplot(mdaymat,aes(x=mday,y=count,color=factor(descr))) + geom_point() +
  scale_colour_manual("Primary Description", values=c("#2b83ba", "#abdda4","#d7191c","#fdae61")) +
  xlab("mday") + ylab("occurrence number in each day") + scale_x_date(date_breaks="1 month",date_labels = "%b")
pa3

## Describe temporal trend in one year


############### b

side = ifelse(h2$Y.Coordinate<=1895000,"south","north")
matb = data.frame(h2,side)
b1plot = aggregate(count~side+day+descr,data=matb,sum)
b2plot = aggregate(count~side+month+descr,data=matb,sum)

pb1 = ggplot(b1plot,aes(x=day,y=count,color=factor(side))) + geom_jitter() +
  scale_colour_manual("Primary Description", values=c("#2b83ba","#d7191c")) + facet_wrap(~descr,nrow=2) +
  xlab("day") + ylab("occurence number in each day")
pb1

pb2 = ggplot(b2plot,aes(x=month,y=count,color=factor(side))) + geom_line(size=1.2) +
  scale_colour_manual("Primary Description", values=c("#2b83ba","#d7191c")) + facet_wrap(~descr,nrow=2) +
  xlab("month") + ylab("average occurence number in each month") #+ scale_x_date(date_labels = "%b %d")
pb2

#################### c

matc = aggregate(count~X.Coordinate+Y.Coordinate+descr,data=h2,sum)
matc2 = h2[,c("X.Coordinate","Y.Coordinate","descr")]

chicago <- readShapePoly("Boundary/City_Boundary.shp")
community <- readShapePoly("CCA/CCA.shp")
chicago <- fortify(chicago)
ggplot(chicago) + geom_polygon(aes(x=long, y=lat, group=group))
ggplot(chicago) + geom_path(aes(x=long, y=lat, group=group))

as.character(unique(h2$descr)) -> cri

pc = ggplot(chicago) +  geom_path(aes(x=long, y=lat, group=group)) + geom_point(data=matc2, aes(x=X.Coordinate, y=Y.Coordinate),alpha = 1/500) +
  facet_wrap(~descr,nrow=2)
pc


#### try
pdf(file="2c.pdf",height=10,width=10)

i=3
pc3.3.2 = ggplot(chicago) +  geom_path(aes(x=long, y=lat, group=group)) +
  stat_bin2d(data=matc2[matc2$descr==cri[i],],aes(X.Coordinate,Y.Coordinate),binwidth = c(1000,1000)) + 
  scale_fill_gradient(limit=c(0,200),low = "white", high = "black") +geom_path(aes(x=long, y=lat, group=group)) +
  facet_wrap(~descr,nrow=1)
pc3.3.2
grid.arrange(pc3.1,pc3.2,pc3.4,pc3.4.2,pc3.3,pc3.3.2,ncol=2)
dev.off()

pc3.1 pc3.2 pc3.4 pc3.3 

library(hexbin)
pc2 = ggplot(chicago) +  geom_path(aes(x=long, y=lat, group=group)) +
  stat_binhex(data=matc2,aes(X.Coordinate,Y.Coordinate),binwidth = c(1000,1000)) + 
  scale_fill_gradient(limits = c(0, 100),low = "white", high = "black") +geom_path(aes(x=long, y=lat, group=group)) +
  facet_wrap(~descr,nrow=2)
pc2

#########################################

require(rgdal)
require(ggplot2)
require(rgeos)
require(ggmap)

myshp = readShapeSpatial("Comm_20Areas/CommAreas.shp")
matry = aggregate(count~Community.Area+descr,data=h2,sum)
colnames(matry)[1] = "AREA_NUMBE"
myshp@data$id = rownames(myshp@data)
newtmp = merge(myshp@data,matry,by='AREA_NUMBE',all.x=T)
shp = fortify(myshp)
new = merge(shp,newtmp,by="id")

i=4
cri = unique(new$descr)
p4 = ggplot(new[new$descr==cri[i],], aes(x=long, y=lat, group=group))+geom_path(color="black")+
  geom_polygon(aes(fill=count))+facet_wrap(~descr,ncol=2)+ scale_fill_gradient(low='grey',high='#d7191c')+
  coord_fixed()
p4

pdf(file="2c.pdf",height=10,width=10)
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()
