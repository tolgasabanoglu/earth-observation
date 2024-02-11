#libraries
library(raster)
library(lubridate)
library(rgdal)
library(ggplot2)

dir_main <- '~/Desktop/EarthObservation/session_03-05'
setwd(dir_main)

# 1) Producing reference data

map <- raster("~/Desktop/EarthObservation/session_03-05/imagery/stm/test.tif")
samp <- sampleStratified(x=map, size=25, sp=T, na.rm=T)
plot(samp)
writeOGR(samp, ".", "sample", driver= "ESRI Shapefile")

# 2) Area-adjusted accuracy assessment

freq(map, na.omit=T)
freq <- as.data.frame(freq)
freq

#confusion matrix 

sample <- readOGR("~/Desktop/EarthObservation/session_03-05/sample/sample.shp")
sample_df <- as.data.frame(newsample)
conf_matrix <- table(sample$classID, sample$test)
conf_matrix

## Which class has the highest / lowest user?s accuracy?

# highest user's accuracy = class non-forest (69.7 %)
# lowest user's accuracy = class coniferous forest (44.44 %)

## Which class has the highest / lowest producer?s accuracy?

# highest producer's accuracy = class non-forest (92.00 %)
# lowest producer's accuracy = class coniferous forest (32.00 %)

## How does the overall accuracy differ after area-adjustment? Why?

# overall accuracy (not adjusted) = 60.00 %
# overall accuracy (area-adjusted calculation) = 64.31 %
# see assignment: unadjusted overall accuracy is independent of the true area proportions of the map classes
# unadjusted accuracy does not account for the sampling bias introduced by the stratified sampling
# with adjusted overall accuracy: proportion of classes taken into account, area distribution, 
# weighing to occurrence of class, account to bias of using same sample size for every class,
# irradicate bias

## How do the map-based area estimates differ from those obtained using the reference data?

#                 Map area	Adj area
# Deciduous	      15911	    19688,39377
# Mixed	          12835	    13192,70143
# Coniferous	    7171	    18458,07662
# Non-Forest	    54083	    38660,82818

# map-based area estimates seem to be smaller than the adjusted area
# map-based area estimates: derived directly from the map by multiplying 
# the map?s class proportions with the total size of the study area
# but: reference data better quality
# omission of specific class in map was taken into account, 
# which in turn largely increases the area estimate for this class

# 3) Knowledge transfer

## Generate the confusion matrix containing probabilities.

conf_matrix <- as.data.frame.matrix(conf_matrix)
classnames <- c("deciduous forest", "mixed forest", "coniferous forest", "non-forest")
conf_matrix$classnames <- classnames

freq <- as.data.frame(freq)
freq$classnames <- classnames
prob <- merge(conf_matrix, freq, by= "classnames")
prob

prob$sum <- rowSums(prob[ , c(2,3,4,5)], na.rm=TRUE)

# adjusted class probabilities (classification/reference)
prob$adj_class_prob_deciduous   <- (prob$"1" / prob$sum) * prob$proportion
prob$adj_class_prob_mixed       <- (prob$"2" / prob$sum) * prob$proportion
prob$adj_class_prob_coniferous  <- (prob$"3" / prob$sum) * prob$proportion
prob$adj_class_prob_nonforest   <- (prob$"4" / prob$sum) * prob$proportion
prob$adj_class_prob_sum         <- rowSums(prob[ , c(10,11,12,13)], na.rm=TRUE)

# adjusted class probabilities (classification/proportions belonging to class)
prob$proportion_class_deciduous <- (prob$"1" / prob$sum)
prob$proportion_class_mixed     <- (prob$"2" / prob$sum)
prob$proportion_class_coniferous<- (prob$"3" / prob$sum)
prob$proportion_class_nonforest <- (prob$"4" / prob$sum)

# EPS
prob$eps_deciduous    <- sum(prob$adj_class_prob_deciduous)
prob$eps_mixed        <- sum(prob$adj_class_prob_mixed)
prob$eps_coniferous   <- sum(prob$adj_class_prob_coniferous)
prob$eps_nonforest    <- sum(prob$adj_class_prob_nonforest)

# EV

# SE

## Calculate overall accuracy and class-wise user?s and producer?s accuracy from the confusion matrix.

# overall accuracy (not adjusted)
prob$overall_accuracy <- sum(prob$"1"[1]) + sum(prob$"2"[2]) + sum(prob$"3"[3]) + sum(prob$"4"[4])

# adjusted overall accuracy
prob <- prob[order(prob$value),]
prob$adj_overall_accuracy <- sum(prob$adj_class_prob_deciduous[1]) + sum(prob$adj_class_prob_mixed[2]) +
  sum(prob$adj_class_prob_coniferous[3]) + sum(prob$adj_class_prob_nonforest[4])

# producer's accuracy (not adjusted)
prob$producers_accuracy_deciduous  <- (prob$"1"[1]) / sum(prob$`1`)
prob$producers_accuracy_mixed      <- (prob$"2"[2]) / sum(prob$`2`)
prob$producers_accuracy_coniferous <- (prob$"3"[3]) / sum(prob$`3`)
prob$producers_accuracy_nonforest  <- (prob$"4"[4]) / sum(prob$`4`)

# user's accuracy (not adjusted)
prob$users_accuracy_deciduous  <- (prob$"1"[1]) / prob$sum[1]
prob$users_accuracy_mixed      <- (prob$"2"[2]) / prob$sum[2]
prob$users_accuracy_coniferous <- (prob$"3"[3]) / prob$sum[3]
prob$users_accuracy_nonforest  <- (prob$"4"[4]) / prob$sum[4]

# producer's accuracy (adjusted)

prob$adj_producers_accuracy_deciduous  <- prob$adj_class_prob_deciduous[1] / prob$eps_deciduous
prob$adj_producers_accuracy_mixed      <- prob$adj_class_prob_mixed[2] / prob$eps_mixed
prob$adj_producers_accuracy_coniferous <- prob$adj_class_prob_coniferous[3] / prob$eps_coniferous
prob$adj_producers_accuracy_nonforest  <- prob$adj_class_prob_nonforest[4] / prob$eps_nonforest

# user's accuracy (adjusted)

prob$adj_users_accuracy_deciduous  <- prob$adj_class_prob_deciduous[1] / prob$adj_class_prob_sum[1]
prob$adj_users_accuracy_mixed      <- prob$adj_class_prob_mixed[2] / prob$adj_class_prob_sum[2]
prob$adj_users_accuracy_coniferous <- prob$adj_class_prob_coniferous[3] / prob$adj_class_prob_sum[3]
prob$adj_users_accuracy_nonforest  <- prob$adj_class_prob_nonforest[4] / prob$adj_class_prob_sum[4]

## Produce error-adjusted area estimates from the confusion matrix.

# calculate area in ha by using pixel count
prob$area <- prob$count*30^2/10000

prob$adj_area_deciduous  <- sum(prob$area) * prob$eps_deciduous
prob$adj_area_mixed      <- sum(prob$area) * prob$eps_mixed
prob$adj_area_coniferous <- sum(prob$area) * prob$eps_coniferous
prob$adj_area_nonforest  <- sum(prob$area) * prob$eps_nonforest
