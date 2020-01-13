library(raster)
library(sp)
library(sf)
library(rgdal)
library(tidyverse)
library(ggplot2)

# get current directoy
getwd()
# set current directory
setwd('D:/UCL_CASA/GIS/assignment/R_Assignment/Data')
# read weather data
weather <- read_csv('weather_data.csv')
# transform weather data into dataframe
weather <- as.data.frame(weather)


###plot monthly average rainfall and temperature
# rename columns' names
names(weather) <- c("Month","Rainfall","Temperature")  
# set month level
weather$Month <- factor(weather$Month, levels=weather$Month)

# plot weather information
ggplot(weather)+
  geom_bar(aes(Month,Rainfall),stat = "identity",size=1, col='blue', fill='blue')+
  geom_line(aes(weather[,1],2*weather[,3]),size=1.5,col="red", group=1)+
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Temp.(°C)"))+
  ylab("Rainfall(mm)")



#### processing red, greed, and near-infrared band of landsat 8 from April to August for calculating NDVI & NDWI 
### processing April data
## April red band
# read landsat 8 April red band data, two images contain the ranges we want
Apr_red76 <- raster('LC08_L1TP_169076_20160404_20170327_01_T1_B4.tif')
Apr_red77 <- raster('LC08_L1TP_169077_20160404_20170327_01_T1_B4.tif')

# remove the black background(value=0) of landsat by using mask
maskAprRed76 <- mask(Apr_red76, Apr_red76!=0, maskvalue=FALSE)
maskAprRed77 <- mask(Apr_red77, Apr_red77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_apr_red <- merge(maskAprRed76, maskAprRed77, all.x=TRUE)

# read migration rages shapefile
migra_range <- readOGR('union_movement.shp')

# transform crs preparing for masking
m_range_crs <- spTransform(migra_range, crs(mer_apr_red))

# masking migration ranges, and get April red band for calculating NDVI  
AprRed_c <- crop(mer_apr_red, m_range_crs)
AprRed_f <- mask(AprRed_c, m_range_crs)
plot(AprRed_f)
writeRaster(AprRed_f, filename="AprRed_f.tif", overwrite=TRUE) # save result

## April Green band
# read landsat 8 April NIR band data, two images contain the ranges we want
Apr_green76 <- raster('LC08_L1TP_169076_20160404_20170327_01_T1_B3.tif')
Apr_green77 <- raster('LC08_L1TP_169077_20160404_20170327_01_T1_B3.tif')

# remove the black background(value=0) of landsat by using mask
maskAprGreen76 <- mask(Apr_green76, Apr_green76!=0, maskvalue=FALSE)
maskAprGreen77 <- mask(Apr_green77, Apr_green77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_apr_green <- merge(maskAprGreen76, maskAprGreen77, all.x=TRUE)

# masking migration ranges, and get April green band for calculating NDVI  
AprGreen_c <- crop(mer_apr_green, m_range_crs)
AprGreen_f <- mask(AprGreen_c, m_range_crs)

plot(AprGreen_f)
writeRaster(AprGreen_f, filename="AprGreen_f.tif", overwrite=TRUE) # save result

## April NIR band
# read landsat 8 April NIR band data, two images contain the ranges we want
Apr_NIR76 <- raster('LC08_L1TP_169076_20160404_20170327_01_T1_B5.tif')
Apr_NIR77 <- raster('LC08_L1TP_169077_20160404_20170327_01_T1_B5.tif')

# remove the black background(value=0) of landsat by using mask
maskAprNIR76 <- mask(Apr_NIR76, Apr_NIR76!=0, maskvalue=FALSE)
maskAprNIR77 <- mask(Apr_NIR77, Apr_NIR77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_apr_NIR <- merge(maskAprNIR76, maskAprNIR77, all.x=TRUE)

# masking migration ranges, and get April green band for calculating NDVI 
AprNIR_c <- crop(mer_apr_NIR, m_range_crs)
AprNIR_f <- mask(AprNIR_c, m_range_crs)
plot(AprNIR_f)
writeRaster(AprNIR_f, filename="AprNIR_f.tif", overwrite=TRUE) # save result



### processing May data
## May red band
# read landsat 8 May red band data, two images contain the ranges we want
May_red76 <- raster('LC08_L1TP_169076_20160506_20170325_01_T1_B4.tif')
May_red77 <- raster('LC08_L1TP_169077_20160506_20170325_01_T1_B4.tif')

# remove the black background(value=0) of landsat by using mask
maskMayRed76 <- mask(May_red76, May_red76!=0, maskvalue=FALSE)
maskMayRed77 <- mask(May_red77, May_red77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_may_red <- merge(maskMayRed76, maskMayRed77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
MayRed_c <- crop(mer_may_red, m_range_crs)
MayRed_f <- mask(MayRed_c, m_range_crs)
plot(MayRed_f)
writeRaster(MayRed_f, filename="MayRed_f.tif", overwrite=TRUE) # save result


## May green band
# read landsat 8 May green band data, two images contain the ranges we want
May_green76 <- raster('LC08_L1TP_169076_20160506_20170325_01_T1_B3.tif')
May_green77 <- raster('LC08_L1TP_169077_20160506_20170325_01_T1_B3.tif')

# remove the black background(value=0) of landsat by using mask
maskMayGreen76 <- mask(May_green76, May_green76!=0, maskvalue=FALSE)
maskMayGreen77 <- mask(May_green77, May_green77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_may_green <- merge(maskMayGreen76, maskMayGreen77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
MayGreen_c <- crop(mer_may_green, m_range_crs)
MayGreen_f <- mask(MayGreen_c, m_range_crs)
plot(MayGreen_f)
writeRaster(MayGreen_f, filename="MayGreen_f.tif", overwrite=TRUE) # save result


## May NIR band
# read landsat 8 May green band data, two images contain the ranges we want
May_NIR76 <- raster('LC08_L1TP_169076_20160506_20170325_01_T1_B5.tif')
May_NIR77 <- raster('LC08_L1TP_169077_20160506_20170325_01_T1_B5.tif')

# remove the black background(value=0) of landsat by using mask
maskMayNIR76 <- mask(May_NIR76, May_NIR76!=0, maskvalue=FALSE)
maskMayNIR77 <- mask(May_NIR77, May_NIR77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_may_NIR <- merge(maskMayNIR76, maskMayNIR77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
MayNIR_c <- crop(mer_may_NIR, m_range_crs)
MayNIR_f <- mask(MayNIR_c, m_range_crs)
plot(MayNIR_f)
writeRaster(MayNIR_f, filename="MayNIR_f.tif", overwrite=TRUE) # save result



### processing June data
## June red band
# read landsat 8 June red band data, two images contain the ranges we want
Jun_red76 <- raster('LC08_L1TP_169076_20160607_20170324_01_T1_B4.tif')
Jun_red77 <- raster('LC08_L1TP_169077_20160607_20170324_01_T1_B4.tif')

# remove the black background(value=0) of landsat by using mask
maskJunRed76 <- mask(Jun_red76, Jun_red76!=0, maskvalue=FALSE)
maskJunRed77 <- mask(Jun_red77, Jun_red77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_jun_red <- merge(maskJunRed76, maskJunRed77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
JunRed_c <- crop(mer_jun_red, m_range_crs)
JunRed_f <- mask(JunRed_c, m_range_crs)
plot(JunRed_f)
writeRaster(JunRed_f, filename="JunRed_f.tif", overwrite=TRUE) # save result



## June green band
# read landsat 8 June green band data, two images contain the ranges we want
Jun_green76 <- raster('LC08_L1TP_169076_20160607_20170324_01_T1_B3.tif')
Jun_green77 <- raster('LC08_L1TP_169077_20160607_20170324_01_T1_B3.tif')

# remove the black background(value=0) of landsat by using mask
maskJunGreen76 <- mask(Jun_green76, Jun_green76!=0, maskvalue=FALSE)
maskJunGreen77 <- mask(Jun_green77, Jun_green77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_jun_green <- merge(maskJunGreen76, maskJunGreen77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
JunGreen_c <- crop(mer_jun_green, m_range_crs)
JunGreen_f <- mask(JunGreen_c, m_range_crs)
plot(JunGreen_f)
writeRaster(JunGreen_f, filename="JunGreen_f.tif", overwrite=TRUE) # save result



## June NIR band
# read landsat 8 June green band data, two images contain the ranges we want
Jun_NIR76 <- raster('LC08_L1TP_169076_20160607_20170324_01_T1_B5.tif')
Jun_NIR77 <- raster('LC08_L1TP_169077_20160607_20170324_01_T1_B5.tif')

# remove the black background(value=0) of landsat by using mask
maskJunNIR76 <- mask(Jun_NIR76, Jun_NIR76!=0, maskvalue=FALSE)
maskJunNIR77 <- mask(Jun_NIR77, Jun_NIR77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_jun_NIR <- merge(maskJunNIR76, maskJunNIR77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
JunNIR_c <- crop(mer_jun_NIR, m_range_crs)
JunNIR_f <- mask(JunNIR_c, m_range_crs)
plot(JunNIR_f)
writeRaster(JunNIR_f, filename="JunNIR_f.tif", overwrite=TRUE) # save result



### processing July data
## July red band
# read landsat 8 July red band data, two images contain the ranges we want
July_red76 <- raster('LC08_L1TP_169076_20160709_20170323_01_T1_B4.tif')
July_red77 <- raster('LC08_L1TP_169077_20160709_20170323_01_T1_B4.tif')

# remove the black background(value=0) of landsat by using mask
maskJulRed76 <- mask(July_red76, July_red76!=0, maskvalue=FALSE)
maskJulRed77 <- mask(July_red77, July_red77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_jul_red <- merge(maskJulRed76, maskJulRed77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
JulRed_c <- crop(mer_jul_red, m_range_crs)
JulRed_f <- mask(JulRed_c, m_range_crs)
plot(JulRed_f)
writeRaster(JulRed_f, filename="JulRed_f.tif", overwrite=TRUE) # save result



## July green band
# read landsat 8 July green band data, two images contain the ranges we want
July_green76 <- raster('LC08_L1TP_169076_20160709_20170323_01_T1_B3.tif')
July_green77 <- raster('LC08_L1TP_169077_20160709_20170323_01_T1_B3.tif')

# remove the black background(value=0) of landsat by using mask
maskJulGreen76 <- mask(July_green76, July_green76!=0, maskvalue=FALSE)
maskJulGreen77 <- mask(July_green77, July_green77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_jul_green <- merge(maskJulGreen76, maskJulGreen77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
JulGreen_c <- crop(mer_jul_green, m_range_crs)
JulGreen_f <- mask(JulGreen_c, m_range_crs)
plot(JulGreen_f)
writeRaster(JulGreen_f, filename="JulGreen_f.tif", overwrite=TRUE) # save result



## July NIR band
# read landsat 8 July NIR band data, two images contain the ranges we want
July_NIR76 <- raster('LC08_L1TP_169076_20160709_20170323_01_T1_B5.tif')
July_NIR77 <- raster('LC08_L1TP_169077_20160709_20170323_01_T1_B5.tif')

# remove the black background(value=0) of landsat by using mask
maskJulNIR76 <- mask(July_NIR76, July_NIR76!=0, maskvalue=FALSE)
maskJulNIR77 <- mask(July_NIR77, July_NIR77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_jul_NIR <- merge(maskJulNIR76, maskJulNIR77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
JulNIR_c <- crop(mer_jul_NIR, m_range_crs)
JulNIR_f <- mask(JulNIR_c, m_range_crs)
plot(JulNIR_f)
writeRaster(JulNIR_f, filename="JulNIR_f.tif", overwrite=TRUE) # save result



### processing August data
## Aug. red band
# read landsat 8 Aug red band data, two images contain the ranges we want
Aug_red76 <- raster('LC08_L1TP_169076_20160810_20170322_01_T1_B4.tif')
Aug_red77 <- raster('LC08_L1TP_169077_20160810_20170322_01_T1_B4.tif')

# remove the black background(value=0) of landsat by using mask
maskAugRed76 <- mask(Aug_red76, Aug_red76!=0, maskvalue=FALSE)
maskAugRed77 <- mask(Aug_red77, Aug_red77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_aug_red <- merge(maskAugRed76, maskAugRed77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
AugRed_c <- crop(mer_aug_red, m_range_crs)
AugRed_f <- mask(AugRed_c, m_range_crs)
plot(AugRed_f)
writeRaster(AugRed_f, filename="AugRed_f.tif", overwrite=TRUE) # save result



## Aug. green band
# read landsat 8 Aug green band data, two images contain the ranges we want
Aug_green76 <- raster('LC08_L1TP_169076_20160810_20170322_01_T1_B3.tif')
Aug_green77 <- raster('LC08_L1TP_169077_20160810_20170322_01_T1_B3.tif')

# remove the black background(value=0) of landsat by using mask
maskAugGreen76 <- mask(Aug_green76, Aug_green76!=0, maskvalue=FALSE)
maskAugGreen77 <- mask(Aug_green77, Aug_green77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_aug_green <- merge(maskAugGreen76, maskAugGreen77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
AugGreen_c <- crop(mer_aug_green, m_range_crs)
AugGreen_f <- mask(AugGreen_c, m_range_crs)
plot(AugGreen_f)
writeRaster(AugGreen_f, filename="AugGreen_f.tif", overwrite=TRUE) # save result


## Aug. NIR band
# read landsat 8 Aug green band data, two images contain the ranges we want
Aug_NIR76 <- raster('LC08_L1TP_169076_20160810_20170322_01_T1_B5.tif')
Aug_NIR77 <- raster('LC08_L1TP_169077_20160810_20170322_01_T1_B5.tif')

# remove the black background(value=0) of landsat by using mask
maskAugNIR76 <- mask(Aug_NIR76, Aug_NIR76!=0, maskvalue=FALSE)
maskAugNIR77 <- mask(Aug_NIR77, Aug_NIR77!=0, maskvalue=FALSE)

# merge two images to form the whole ranges
mer_aug_NIR <- merge(maskAugNIR76, maskAugNIR77, all.x=TRUE)

# masking migration ranges, and get April red band for calculating NDVI  
AugNIR_c <- crop(mer_aug_NIR, m_range_crs)
AugNIR_f <- mask(AugNIR_c, m_range_crs)
plot(AugNIR_f)
writeRaster(AugNIR_f, filename="AugNIR_f.tif", overwrite=TRUE) # save result



#### Calculate NDVI
### April NDVI
# create NDVI function
par(mfrow=c(1,1))
plot(m_range_crs, add=TRUE)
ndvi_function <-function(x,y){
  vi<-(x-y)/(x+y)
  return(vi)
}

# April NDVI plot
April_ndvi <- overlay(AprNIR_f,AprRed_f, fun=ndvi_function)
plot(April_ndvi, col=rev(terrain.colors(10)), main="April NDVI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Recalssify April NDVI
Apr_ndvi_rc <- reclassify(April_ndvi, c(-Inf,0, NA, 0,0.15,1, 0.15,0.3,2, 0.3, Inf, 3))
plot(Apr_ndvi_rc, col=rev(terrain.colors(3)), main="April NDVI")

# April NDVI histogram
hist(April_ndvi,
     main="Distribution April NDVI Values ",
     xlab= NA,
     ylab= "Frequency",
     col = "yellowgreen",
     xlim = c(-0.1, 0.5),
     breaks = 10,
     xaxt = 'n',
     yaxt="n")
axis(side=1, at = seq(-0.1,0.5, 0.05), labels = seq(-0.1,0.5, 0.05))
axis(side=2,at=seq(0,10000000,200000),labels=format(seq(0,10000000,200000),scientific=FALSE))


# May NDVI plot
May_ndvi <- overlay(MayNIR_f,MayRed_f, fun=ndvi_function)
plot(May_ndvi, col=rev(terrain.colors(10)), main="May-NDVI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Recalssify May NDVI
May_ndvi_rc <- reclassify(May_ndvi, c(-Inf,0, NA, 0,0.15,1, 0.15,0.3,2, 0.3, Inf, 3))
plot(May_ndvi_rc, col=rev(terrain.colors(3)), main="May NDVI")

# May NDVI histogram
hist(May_ndvi,
     main="Distribution May NDVI Values ",
     xlab= NA,
     ylab= "Frequency",
     col = "yellowgreen",
     xlim = c(-0.1, 0.5),
     breaks = 10,
     xaxt = 'n',
     yaxt='n')
axis(side=1, at = seq(-0.1,0.5, 0.05), labels = seq(-0.1,0.5, 0.05))
axis(side=2,at=seq(0,12000000,200000),labels=format(seq(0,12000000,200000),scientific=FALSE))


# June NDVI plot
Jun_ndvi <- overlay(JunNIR_f,JunRed_f, fun=ndvi_function)
plot(Jun_ndvi, col=rev(terrain.colors(10)), main="Jun-NDVI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Reclassify June NDVI
Jun_ndvi_rc <- reclassify(Jun_ndvi, c(-Inf,0, NA, 0,0.15,1, 0.15,0.3,2, 0.3, Inf, 3))
plot(Jun_ndvi_rc, col=rev(terrain.colors(3)), main="Jun NDVI")

# June NDVI Histogram 
hist(Jun_ndvi,
     main="Distribution June NDVI Values ",
     xlab= NA,
     ylab= "Frequency",
     col = "yellowgreen",
     xlim = c(-0.1, 0.5),
     ylim = c(0, 1000000),
     breaks = 10,
     xaxt = 'n',
     yaxt= 'n')
axis(side=1, at = seq(-0.1,0.5, 0.05), labels = seq(-0.1,0.5, 0.05))
axis(side=2,at=seq(0,10000000,200000),labels=format(seq(0,10000000,200000),scientific=FALSE))

#July NDVI plot
Jul_ndvi <- overlay(JulNIR_f,JulRed_f, fun=ndvi_function)
plot(Jul_ndvi, col=rev(terrain.colors(10)), main="July-NDVI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

#Reclassify July NDVI
Jul_ndvi_rc <- reclassify(Jul_ndvi, c(-Inf,0, NA, 0,0.15,1, 0.15,0.3,2, 0.3, Inf, 3))
plot(Jul_ndvi_rc, col=rev(terrain.colors(3)), main="July NDVI")

# June NDVI Histogram 
hist(Jul_ndvi,
     main="Distribution July NDVI Values ",
     xlab= NA,
     ylab= "Frequency",
     col = "yellowgreen",
     xlim = c(-0.1, 0.5),
     ylim = c(0, 1000000),
     breaks = 10,
     xaxt = 'n',
     yaxt= 'n')
axis(side=1, at = seq(-0.1,0.5, 0.05), labels = seq(-0.1,0.5, 0.05))
axis(side=2,at=seq(0,10000000,200000),labels=format(seq(0,10000000,200000),scientific=FALSE))

# August NDVI plot
Aug_ndvi <- overlay(AugNIR_f,AugRed_f, fun=ndvi_function)
plot(Aug_ndvi, col=rev(terrain.colors(10)), main="Aug-NDVI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Reclassify August NDVI
Aug_ndvi_rc <- reclassify(Aug_ndvi, c(-Inf,0, NA, 0,0.15,1, 0.15,0.3,2, 0.3, Inf, 3))
plot(Aug_ndvi_rc, col=rev(terrain.colors(3)), main="August NDVI")

# August NDVI Histogram 
hist(Aug_ndvi,
     main="Distribution August NDVI Values ",
     xlab= NA,
     ylab= "Frequency",
     col = "yellowgreen",
     xlim = c(-0.1, 0.5),
     ylim = c(0, 1000000),
     breaks = 10,
     xaxt = 'n',
     yaxt='n')
axis(side=1, at = seq(-0.1,0.5, 0.05), labels = seq(-0.1,0.5, 0.05))
axis(side=2,at=seq(0,10000000,200000),labels=format(seq(0,10000000,200000),scientific=FALSE))

#### Calculate NDWI
### April NDWI
# create NDWI function 
ndwi_function <-function(x,y){
  wi<-(x-y)/(x+y)
  return(wi)
}

# April NDWI plot
April_ndwi <- overlay(AprGreen_f,AprNIR_f, fun=ndwi_function)
plot(April_ndwi, col=rev(topo.colors(5)), main="April NDWI")

text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Reclassify April NDWI 
Apr_ndwi_rc <- reclassify(April_ndwi, c(-Inf,0, NA, 0,Inf,1))
plot(Apr_ndwi_rc, col='red', main="April NDWI")
plot(m_range_crs, add=TRUE)
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# May NDWI plot
May_ndwi <- overlay(MayGreen_f,MayNIR_f, fun=ndwi_function)

plot(May_ndwi, col=rev(topo.colors(5)), main="May NDWI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Reclassify May NDWI
May_ndwi_rc <- reclassify(May_ndwi, c(-Inf,0, NA, 0,Inf,1))
plot(May_ndwi_rc, col='red', main="May NDWI")
plot(m_range_crs, add=TRUE)
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# June NDWI plot
Jun_ndwi <- overlay(JunGreen_f,JunNIR_f, fun=ndwi_function)
plot(Jun_ndwi, col=rev(topo.colors(5)), main="June NDWI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Reclassify June NDWI
Jun_ndwi_rc <- reclassify(Jun_ndwi, c(-Inf,0, NA, 0,Inf,1))
plot(Jun_ndwi_rc, col='red', main="June NDWI")
plot(m_range_crs, add=TRUE)
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# July NDWI plot
Jul_ndwi <- overlay(JulGreen_f,JulNIR_f, fun=ndwi_function)
plot(Jul_ndwi, col=rev(topo.colors(5)), main="July NDWI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Reclassify June NDWI
Jul_ndwi_rc <- reclassify(Jul_ndwi, c(-Inf,0, NA, 0,Inf,1))
plot(Jul_ndwi_rc, col='red', main="July NDWI")
plot(m_range_crs, add=TRUE)
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# August NDWI plot
Aug_ndwi <- overlay(AugGreen_f,AugNIR_f, fun=ndwi_function)

plot(Aug_ndwi, col=rev(topo.colors(5)), main="August NDWI")
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)

# Reclassify August NDWI
Aug_ndwi_rc <- reclassify(Aug_ndwi, c(-Inf,0, NA, 0,Inf,1))
plot(Aug_ndwi_rc, col='red', main="August NDWI")
plot(m_range_crs, add=TRUE)
text(300000, -2590000, "A", cex=2)
text(300000, -2650000, "B", cex=2)



