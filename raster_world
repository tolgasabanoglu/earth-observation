##  Rasters in R ##
#   Tolga Sabanoglu  # 

## 1: Welcome to the raster world

# packages

install.packages('raster')
install.packages('rgdal')

library(raster)
library(rgdal)

## 2: Reading data

list_20140310 <- list.files(path ="~/Desktop/EarthObservation/eo_data_s01-02/L1TP/LC08_L1TP_189025_20140310_20200911_02_T1",
                            pattern = ".*B[234567]\\.TIF$", ignore.case=TRUE, full.names = TRUE)

list_20140716 <- list.files(path = "~/Desktop/EarthObservation/eo_data_s01-02/L1TP/LC08_L1TP_189025_20140716_20200911_02_T1", 
                            pattern = ".*B[234567]\\.TIF$", ignore.case=TRUE, full.names = TRUE)
# stack

stack_20140310 <- stack(list_20140310)
stack_20140716 <- stack(list_20140716)

## 3: Manipulating data

# crs

projection(stack_20140310)
projection(stack_20140716)

# In which projection is the data delivered (EPSG-code / proj-string)?
utm18 <- "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"
# sutm <- projectRaster(stack, utm18)

# crop

roi_extent <- c(327945, 380325, 5472105, 5521095)
crop_20140310 <- crop(stack_20140310, roi_extent)
crop_20140716 <- crop(stack_20140716, roi_extent)
plot(crop_20140310)
plot(crop_20140716)

## 4: Writing data

writeRaster(crop_20140310, filename = "landsat_crop_20140310.tif", overwrite = T, datatype = 'INT2U')
writeRaster(crop_20140716, filename = "landsat_crop_20140716.tif", overwrite = T, datatype = 'INT2U')

# Which datatype(s) is/are suitable and why?
crop_20140310
crop_20140716
# INT2U is the suitable datatype
# positive integer values
# landsat 8 OLI quantized over a 12-bit radiometric range, but rescaled to 16-bit integers 
# INT2U - 16 bit, maximum possible value 65,534

## 5: Plotting image data

# remove the filename from each band name for pretty plotting

names(crop_20140310) <- gsub(pattern = "LC08_L1TP_189025_20140310_20200911_02_T1_", replacement = "", names(crop_20140310))
plot(crop_20140310, col = gray(20:100 / 100))

names(crop_20140716) <- gsub(pattern = "LC08_L1TP_189025_20140716_20200911_02_T1_", replacement = "", names(crop_20140716))
plot(crop_20140716, col = gray(20:100 / 100))

# plot true color 

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crop_20140310,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image")
box(col = "white")

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crop_20140716,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image")
box(col = "white")

# plot false color 

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crop_20140310,
        r = 4, g = 3, b = 2,
        stretch = "hist",
        axes = TRUE,
        main = "RGB composite image")
box(col = "white")

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crop_20140716,
        r = 4, g = 3, b = 2,
        stretch = "hist",
        axes = TRUE,
        main = "RGB composite image")
box(col = "white")

## 6: Extracting and visualizing spectral profiles

# extract values at 'x' = 355623, 'y' = 5486216

stack <- stack(crop_20140310, crop_20140716)
location <- c(355623, 5486216)
cellvals<- extract(stack, location)
cellvals <- na.omit(cellvals)

# Load ggplot2 package (or install if needed)
library(ggplot2)

# Create a data frame combining extracted values with 
# band wavelengths and acquisition date 
profile <- data.frame('wavelength'=rep(c(482, 561, 655, 865, 1609, 2201),2), 
                      'date'=as.factor(c(rep('10 March', 6), rep('10 July', 6))),
                      'values'=as.numeric(cellvals))

print(profile)

# Plot spectral profiles as line plot, grouped by factor date
ggplot(profile, aes(x=wavelength, y=values, group=date)) + 
  geom_line(aes(color=date)) + 
  # Add axis & legend names
  scale_y_continuous(name='DN') + 
  scale_x_continuous(name='Wavelength (nm)') + 
  scale_colour_manual(name='Acquisition DOY', values=c('darkgreen', 'brown')) +
  theme_bw() # Chose black/white layout

# What type of surface do the spectra likely represent?

# type of surface: vegetation 
# green peak recognizable for spectral profile (July)
# no/smaller green peak in March, not as much vegetation yet
