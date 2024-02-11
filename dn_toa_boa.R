## Preperation

library(raster)

setwd("~/Desktop/EarthObservation/eo_data_s01-02")

## 1: DN to TOA conversion

# metadata using read.delim()
metadataL1TP <- read.delim("~/Desktop/EarthObservation/eo_data_s01-02/L1TP/LC08_L1TP_189025_20140716_20200911_02_T1/LC08_L1TP_189025_20140716_20200911_02_T1_MTL.txt",sep = '=', stringsAsFactors = F, header = FALSE)

# adding header
names(metadataL1TP) <- c("VAR", "VAL") 
head(metadataL1TP)

#The following information is required for DN to TOA conversion:
#REFLECTANCE_MULT_BAND
#REFLECTANCE_ADD_BAND
#SUN_ELEVATION


# extract conversion factors
REFLECTANCE_MULT_BAND <- as.numeric(metadataL1TP[grep("REFLECTANCE_MULT_BAND_[2-7]", 
                                                 metadataL1TP$VAR),][,2])
REFLECTANCE_ADD_BAND <- as.numeric(metadataL1TP[grep("REFLECTANCE_ADD_BAND_[2-7]", 
                                                      metadataL1TP$VAR),][,2])
SUN_ELEVATION <- as.numeric(metadataL1TP[grep("SUN_ELEVATION", 
                                                     metadataL1TP$VAR),][,2])

# Helper-function to convert degrees to radians
deg2rad <- function(deg){ (deg * pi) / (180) }
SUN_ELEVATION_radiants <- deg2rad(SUN_ELEVATION)

# listing band-files
list_20140716 <- list.files(path = "~/Desktop/EarthObservation/eo_data_s01-02/L1TP/LC08_L1TP_189025_20140716_20200911_02_T1", 
                            pattern = ".*B[234567]\\.TIF$", ignore.case=TRUE, full.names = TRUE)
stack_20140716 <- stack(list_20140716)

# crop image to study area
study_area <- c(317355, 382545, 5475465, 5536035)  # xmin, xmax, ymin, ymax (WGS84, UTM 34N)
crop_20140716 <- crop(stack_20140716, study_area)

# Conversion to TOA reflectance

TOA <- REFLECTANCE_MULT_BAND*crop_20140716 + REFLECTANCE_ADD_BAND 

# Correction for the sun angle 

TOA <- TOA/ sin(SUN_ELEVATION_radiants)

# reconverting to integer
TOA <- TOA*10000

# plot
plot(TOA)

writeRaster(TOA, filename = "L8_TOA_20140716.tif", overwrite = T, datatype = 'INT2U')


## Compare TOA with BOA

# metadata using read.delim()
metadataL2SP <- read.delim("~/Desktop/EarthObservation/eo_data_s01-02/L2SP/LC08_L2SP_189025_20140716_20200911_02_T1/LC08_L2SP_189025_20140716_20200911_02_T1_MTL.txt", sep = '=', stringsAsFactors = F, header = FALSE)

# adding header
names(metadataL2SP) <- c("VAR", "VAL") 

# extract conversion factors
REFLECTANCE_MULT_BAND <- as.numeric(metadataL2SP[grep("REFLECTANCE_MULT_BAND_[2-7]", 
                                                      metadataL2SP$VAR),][,2])
REFLECTANCE_ADD_BAND <- as.numeric(metadataL2SP[grep("REFLECTANCE_ADD_BAND_[2-7]", 
                                                     metadataL2SP$VAR),][,2])

# listing band-files and stack

list_20140716_L2SP <- list.files(path = "~/Desktop/EarthObservation/eo_data_s01-02/L2SP/LC08_L2SP_189025_20140716_20200911_02_T1", 
                            pattern = ".*B[234567]\\.TIF$", ignore.case=TRUE, full.names = TRUE)
stack_20140716_L2SP <- stack(list_20140716_L2SP)

crop_20140716_L2SP <- crop(stack_20140716_L2SP, study_area)

# Conversion to reflectance

BOA <- REFLECTANCE_MULT_BAND*crop_20140716_L2SP + REFLECTANCE_ADD_BAND 

# reconverting to integer
BOA <- BOA*10000

writeRaster(BOA, filename = "L8_BOA_20140716.tif", overwrite = T, datatype = 'INT2U')

## Briefly summarize the spectral appearance of the different land cover types.
## What are the most evident differences between TOA and SR reflectance spectra?

# forest: no huge difference; higher reflectance for TOA in bands 1,2,3; apart from that pretty similar spectra 
# water: higher reflectance for TOA in bands 1-5; BOA peak in band 2 at 400 ish; TOA peak in band 1
# generally higher reflectance for TOA than BOA
# bare soil: similar; higher reflectance for TOA in bands 1-3 


## 2: Data Quality
# Investigating the QA band

#Read the QA band into R and identify the three most frequent values using freq().
qa <- stack("~/Desktop/EarthObservation/eo_data_s01-02/L2SP/LC08_L2SP_189025_20140716_20200911_02_T1/LC08_L2SP_189025_20140716_20200911_02_T1_QA_PIXEL.tif")
fre <- freq(qa)
fre <- data.frame(fre)
fre_values <- fre[order(-fre$LC08_L2SP_189025_20140716_20200911_02_T1_QA_PIXEL.count),][1:3,]
fre_values

#value  count
#  1 24928182
# 21824 19892015
# 22280  8331238

# What do the most frequent values mean? Decode each integer value into bits and 
# describe their meaning here: 

# Most frequent value + meaning as comment
rev(as.numeric(intToBits(1)[1:16]))
# Most frequent value is 1, fill data pixel in the empty areas around the actual observations; pixels that are not part of the actual image but make up the 'background'
# those pixel exist because the image is slightly tilted to the right 

# Second most frequent value + meaning as comment
rev(as.numeric(intToBits(21824)[1:16]))
#Second most frequent value is 21824. Clear-sky pixels; low confidence for water, clouds or cloud shadows
# most of the pixels in our image do not include clouds and cloud shadows

# Third most frequent value + meaning as comment
rev(as.numeric(intToBits(22280)[1:16]))
#Third frequent value is 22280. Cloudy pixels are being detected; no clear, water, snow or cloud shadow pixels
# high cloud confidence

# Creating a cloud mask

# high confidence clouds OR high confidence cloud shadows OR fill values
high_conf <- function(x){
  return((intToBits(x)[1] == T) | (intToBits(x)[4] == T) | (intToBits(x)[5] == T))
}

# high AND medium confidence clouds OR high AND medium confidence cloud shadows OR fill values

high_med_conf <- function(x){
  return((intToBits(x)[1] == T) | (intToBits(x)[9] == T) | (intToBits(x)[11] == T))
}

# Create a binary mask from each function

high_conf_mask <- calc(QA_PIXEL, high_conf)
plot(high_conf_mask)
writeRaster(high_conf_mask, filename = "high_conf_mask.tif", overwrite = T, datatype = 'LOG1S')

high_med_conf_mask <- calc(QA_PIXEL, high_med_conf)
plot(high_med_conf_mask)
writeRaster(high_med_conf_mask, filename = "high_med_conf_mask.tif", overwrite = T, datatype = 'LOG1S')

# Discuss and decide on the appropriate data type for binary masks and write them to disk.
# as we are dealing with binary masks and all observations either have the value 0 or 1 the LOG1S datatype seems to be appropriate 
# however, R shows the warning that data type "LOG" is not available in GDAL and the data type is therefore changed to INT1U 

# Which mask is more reliable in your opinion?
# the second mask only returns 1 values (not sure what was done wrong)
# In our case only the first mask (high confidence mask) can be used

# =============================================================================
# 3) Masking images

BOA <- stack("L8_BOA_20140716.tif")

crop_high_conf_mask <- crop(high_conf_mask, study_area)

BOA_masked <- mask(BOA, crop_high_conf_mask, maskvalue = 1)

writeRaster(BOA_masked, filename = "masked_image.tif", overwrite = T, datatype = 'INT2U')
