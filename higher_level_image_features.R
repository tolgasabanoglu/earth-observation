# load required packages
l_packages <- c("raster", "rgdal", "lubridate")
for (p in l_packages){
  if(! p %in% installed.packages()){
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = T)
}

# =============================================================================
# Part I - Best-Available-Pixel compositing
# =============================================================================

dir_main <- '~/Desktop/EarthObservation/session_03-05'
setwd(dir_main)

# =============================================================================
# 1) Prepare input

# create list of desired image band files
l_bands <- c("BLU", "GRN", "RED", "NIR", "SW1", "SW2", "NDV", "EVI", "TCG", 
             "TCB", "TCW")
l_files <- sapply(l_bands, function(x){
  paste0("imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_", x, "_TSS.tif")
})
print(l_files)

# read raster stacks
l_stacks <- lapply(l_files, raster::stack)

# retrieve necessary variables for compositing (DOYs and year vectors)
bandnames <- names(l_stacks$BLU)
datestring <- lapply(bandnames, function(x) as.Date(substr(x, 2, 9), "%Y%m%d"))
doys <- as.numeric(lapply(datestring, yday))
years <- as.numeric(lapply(datestring, year))

# read cloud distance and valid pixels (masks) stacks
cdists <- raster::stack('imagery/bap/2014-2016_001-365_HL_TSA_LNDLG_CDS_TSS.tif')
vld <- raster::stack('imagery/bap/2014-2016_001-365_HL_TSA_LNDLG_VLD_TSS.tif')


# =============================================================================
# 2) Parameterization

# path to the bap_compositing-function including .R suffix
source('~/Desktop/EarthObservation/Assignments /Assignment4_BAP.R')

# Define the parameterization for two composites and briefly comment 
# on your decisions.

# Composite 1
# parameters
target_year <- 2015
target_doy <- 180

w_year <- 0.1
w_doy <- 0.6
w_cdist <- 0.3

max_doff <- 50
max_yoff <- 4

min_cdist <- 100
max_cdist <- 3000

# call bap_score-function for 1st composite
bapscore1 <- bap_score(doy=doys, year=years, cloud_dist=cdists, 
                       target_doy=target_doy, target_year=target_year, 
                       w_doy=w_doy, w_year=w_year, w_cloud_dist=w_cdist, 
                       max_doy_offset=max_doff, max_year_offset=max_yoff, 
                       min_cloud_dist=min_cdist, max_cloud_dist=max_cdist,
                       valid_pixels=vld)

# create composites for each band from index object
l_composites <- lapply(l_stacks, create_bap, idx_raster=bapscore1$idx)

# create stack of composite rasters + DOY and YEAR info and write to disc
comp <- raster::brick(c(l_composites, bapscore1$doy, bapscore1$year))
print(comp)

outname <- paste0('imagery/bap/LNDLG_BAP_Y', toString(target_year), '-', 
                  toString(max_yoff), '_DOY', toString(target_doy), '-', 
                  toString(max_doff), '.tif')
writeRaster(comp, outname, format='GTiff', datatype = 'INT2U', overwrite=T,
            progress='text')

# Composite 2

# parameters
target_year2 <- 2015
target_doy2 <- 330

w_year <- 0.1
w_doy <- 0.6
w_cdist <- 0.3

max_doff <- 80
max_yoff <- 5

min_cdist <- 100
max_cdist <- 2000

# call bap_score-function for 1st composite
bapscore2 <- bap_score(doy=doys, year=years, cloud_dist=cdists, 
                       target_doy=target_doy2, target_year=target_year2, 
                       w_doy=w_doy, w_year=w_year, w_cloud_dist=w_cdist, 
                       max_doy_offset=max_doff, max_year_offset=max_yoff, 
                       min_cloud_dist=min_cdist, max_cloud_dist=max_cdist,
                       valid_pixels=vld)

# create composites for each band from index object
l_composites2 <- lapply(l_stacks, create_bap, idx_raster=bapscore2$idx)

# create stack of composite rasters + DOY and YEAR info and write to disc
comp2 <- raster::brick(c(l_composites2, bapscore2$doy, bapscore2$year))
print(comp2)

outname2 <- paste0('imagery/bap/LNDLG_BAP_Y2', toString(target_year2), '-', 
                  toString(max_yoff), '_DOY', toString(target_doy2), '-', 
                  toString(max_doff), '.tif')
writeRaster(comp2, outname2, format='GTiff', datatype = 'INT2U', overwrite=T,
            progress='text')

# Composite 1 
# peak-of-season composite
# seasonal consistency is more important than annual consistency
# observations from all years are suitable

# Composite 2
# winter composite 
# again prioritizing seasonal consistency 
# range of suitable days higher than before


### Visual inspection and evaluation of results
# What worked out well. What did not? How could the quality of the composites potentially be improved?

# in some areas choosing pixels without clouds worked very well
# in other areas there are still clouds and very bright pixel 
# in general, the summer composite worked quite well   
# composite 2 (winter composite) included a lot of pixel from landsat-7 making it very hard to interpret
# also composite 2 seemed to be really patchy


# =============================================================================
# Part II - Spectral-Temporal-Metrics
# =============================================================================
# 

#1) Inspect the input data

ndvi <- brick('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_NDV_TSS.tif')
tcg <- brick('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_TCG_TSS.tif') 
tcw <- brick('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_TCW_TSS.tif')
evi <- stack('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_EVI_TSS.tif')
tcb <- brick('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_TCB_TSS.tif')

names(ndvi) <- names(tcb)  # rename bands (raster::writeRaster unfortunately does not preserve band-names)
names(evi) <- names(tcb)

# read training point locations
file_training_pints <- "vector/s03_training_data.gpkg"
train <- readOGR(file_training_pints)

# put imagery in list to extract points iteratively 
l_imgs <- list(ndvi, evi, tcb, tcg, tcw)

# extract time series at training locations for each img in list
l_values <- lapply(l_imgs, FUN = raster::extract, train, sp = TRUE)

# convert (SpatialPoints)objects to dataframes
l_df <- lapply(l_values, as.data.frame)

# add name of spectral feature to associated dataframe
index_name <- c("NDVI", "EVI", "TCG", "TCB", "TCW")
for (i in 1:length(index_name)){
  l_df[[i]]$feature <- index_name[i]
}

# prepare DFs with helper function
prep <- function(x){
  return(
    x %>%
      gather(time, value, -c(classID, feature, coords.x1, coords.x2)) %>%
      mutate(classID = factor(classID), 
             time = as.Date(substr(time, 2, 9), format = "%Y%m%d"),
             value = value)
  )
}

l_df <- lapply(l_df, prep)  # apply helper function to each df

# merge dfs into one df (long format)
df <- do.call(rbind, l_df)

# retrieve averages for each feature and class over time
df_mean <- df %>%
  group_by(feature, classID, time) %>%  # group to calculate for each separately
  summarise(mn = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%  # get temporal mean and std
  mutate(roll_mn = rollapply(
    mn, width=6, FUN=function(x) mean(x, na.rm=TRUE), by=1, partial=TRUE, 
    fill=NA, align="center"
  ),
  roll_sd = rollapply(
    sd, width=6, FUN=function(x) mean(x, na.rm=TRUE), by=1, partial=TRUE, 
    fill=NA, align="center"))  # apply rolling mean to "smooth" trajectories

# specify colours
colours <- c("green", "red", "blue", "grey")
scale_custom <- list(
  scale_color_manual(values = colours),
  scale_fill_manual(values = colours)
)

# plot e.g. mean TCW (+-1sd) over time for each class
feat <- "EVI"
ggplot(data = df_mean %>% filter(feature == feat)) +
  geom_line(aes(x=time, y=roll_mn, color=classID), size=1.25) +
  geom_ribbon(aes(x=time, ymin = roll_mn - roll_sd,
                  ymax = roll_mn + roll_sd, fill=classID), alpha=0.1) +
  scale_custom +
  labs(x ="Time", y = feat) +
  theme_bw()

# add month column 
df_monthly <- df %>%
  group_by(feature, classID, time) %>% 
  mutate(month = format(time, "%m"))
# visualize monthly distributions irrespective of year

feat <- "TCB"
ggplot(data = df_monthly %>% filter(feature == feat)) +
  geom_boxplot(aes(x=as.factor(month), y=value, fill=classID)) +
  scale_custom +
  labs(x ="Time", y = feat) +
  theme_bw()

# Temporal behavior of the four classes

# the different classes show different monthly variability
# the class "other" has larger monthly variability than the three forest classes
# monthly class-wise boxplots of the TCB shows larger means in the late spring/ early summer months for all forest classes but especially deciduous forest 
# those boxplots also make the differentiation between the forest classes and the non-forest class possible 

# =============================================================================
# 2) Compute additional spectral-temporal metrics

ndvi <- stack('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_NDV_TSS.tif')
tcg <- stack('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_TCG_TSS.tif') 
tcw <- stack('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_TCW_TSS.tif')
evi <- stack('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_EVI_TSS.tif')
tcb <- stack('imagery/time-series/2014-2016_001-365_HL_TSA_LNDLG_TCB_TSS.tif')


#converting into SpatRaster
library(terra)

rast(ndvi)
rast(evi)
rast(tcb)
rast(tcg)
rast(tcw)

# retrieve temporal information from bandnames
library(lubridate)
datestring <- unlist(lapply(names(tcb), function(x) substr(x, 2, 9)))
dates <- as.POSIXlt(datestring, format = "%Y%m%d")

#  STM 1
#  Standard deviation; summer months

# on this date-object, we can perform logical operations
# may - september
condition <- (year(dates) %in% 2014:2016) & (month(dates) %in% 5:9)

# convert raster subset to matrix
#tcb
tcb_matrix <- as.matrix(tcb[[which(condition)]])

# calculate sd
tcb_sd <- apply(tcb_matrix, 1, FUN=sd, na.rm=T)

# write results to empty raster
tcb_sd_raster <- raster(nrows=tcb@nrows, 
                        ncols=tcb@ncols, 
                        crs=tcb@crs, 
                        vals=tcb_sd,
                        ext=extent(tcb))

# plot result
plot(tcb_sd_raster)

#save
outname <- paste0('imagery/stm/tcb_sd_2014-2016_5-9', '.tif')
writeRaster(tcb_sd_raster, outname, format='GTiff', datatype = 'INT2U', overwrite=T,
            progress='text')

# STM 2
# range year 2015-2016 tcg

# on this date-object, we can perform logical operations
condition <- (year(dates) %in% 2015:2016) & (month(dates) %in% 1:12)

# convert raster subset to matrix
#tcb
tcg_matrix <- as.matrix(tcg[[which(condition)]])

# calculate sd
tcg_range <- apply(tcg_matrix, 1, FUN=range, na.rm=T)

# write results to empty raster
tcg_range_raster <- raster(nrows=tcg@nrows, 
                           ncols=tcg@ncols, 
                           crs=tcg@crs, 
                           vals=tcg_range,
                           ext=extent(tcg))

# plot result
plot(tcg_range_raster)

#save
outname <- paste0('imagery/stm/tcb_sd_2015-2016_5-9', '.tif')
writeRaster(tcb_sd_raster, outname, format='GTiff', datatype = 'INT2U', overwrite=T,
            progress='text')
