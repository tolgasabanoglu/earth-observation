## Preparation

# load required packages
l_packages <- c("ggplot2", "Rcpp", "raster", "rgdal", "tidyverse", "zoo", 
                "lubridate", "randomForest", "mapac")
for (p in l_packages){
  if(! p %in% installed.packages()){
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = T)
}

## directory
dir_main <- "~/Desktop/EarthObservation"
setwd(dir_main)

# =============================================================================

# Exercise 1: Choosing an approach for change detection

# approach: Integrated multi-temporal change classification
  # stack all features into a single file and collect training data for the change classes of interest
  # obtain the change map directly from your classification

# =============================================================================

# Exercise 2: Screening of input data and training data collection

# a: Visualize the Landsat images in QGIS. Test a true color (RGB), as well as a nIR-swIR1-red false color combination.
    # Optional: Derive additional features from your data. Do you want to classify based on the spectral bands? 
    # What about vegetation indices, principal components, tasseled cap components?

# load data

list <- list.files(path = "~/Desktop/EarthObservation/data 3/sr_data", 
                              pattern = ".*\\.tif$", ignore.case=TRUE, full.names = TRUE)
stack <- stack(list)

file_path <- "~/Desktop/EarthObservation/data 3/validation/validation_points.shp"
val <- readOGR(file_path)

gfc <- raster("~/Desktop/EarthObservation/data 3/gfc/gfc_reclassified.tif")

# training data collection in QGIS

train <- readOGR("~/Desktop/EarthObservation/data 3/train/train_07.shp")
plot(train)

# =============================================================================

# Exercise 3: Model training & classification

# create dataframe

df <- raster::extract(stack, df=T, train)
df$classID <- train$classID
df$classID

df <- df[-1]

# classID as factor

df$classID <- as.factor(df$classID)

# remove NAs

df <- na.omit(df)

# building random forest model with ntree = 1000 and mtry at default

rf <- randomForest(classID ~ ., data = df, importance = T, proximity = T, ntree = 1000)

# Investigating model performance

rf 
plot(rf)

# # Classification

# option 1) single core
plot(rf)
map <- raster::predict(stack, rf, filename = "test.tif", overwrite=T)
plot(map)

# =============================================================================

# Exercise 4: Change map validation & area estimates

# We provided reference data for the study area as a shapefile. Use it to perform an accuracy assessment. 
# Why is it challenging to perform an area-adjusted accuracy assessment in this case?
  
# Calculate map-based area estimates for your map and the Global Forest Change data using simple pixel counting. 
# Compare the estimates. What are the major differences?
  
val <- readOGR("~/Desktop/EarthObservation/data 3/validation/validation_points.shp")
map <- raster("~/Desktop/EarthObservation/data 3/test.tif")

# class proportions

freq <- freq(map)
freq <- as.data.frame(freq)
freq
freq$proportion <- proportions(freq$count)
sum(freq$proportion)

# confusion matrix

plot(val, add=T)
values <-raster::extract(map, df=T, val)
conf_matrix <- table(val$layer, values$test)
conf_matrix

## Generate the confusion matrix containing probabilities.

conf_matrix <- as.data.frame.matrix(conf_matrix)
classnames <- c("Stable forest", "Deforestation 2001-2005", "Deforestation 2006-2010", "Forest gain", "Stable non-forest")
conf_matrix$classnames <- classnames

freq <- as.data.frame(freq)
freq$classnames <- classnames
prob <- merge(conf_matrix, freq, by= "classnames")
prob

prob$sum <- rowSums(prob[ , c(2,3,4,5,6)], na.rm=TRUE)

prob <- prob[order(prob$value),]

prob

# adjusted class probabilities (classification/reference)
prob$adj_class_prob_forest          <- (prob$"1" / prob$sum) * prob$proportion
prob$adj_class_prob_def_2001_2005   <- (prob$"2" / prob$sum) * prob$proportion
prob$adj_class_prob_def_2006_2010   <- (prob$"3" / prob$sum) * prob$proportion
prob$adj_class_prob_forestgain      <- (prob$"4" / prob$sum) * prob$proportion
prob$adj_class_prob_nonforest       <- (prob$"5" / prob$sum) * prob$proportion
prob$adj_class_prob_sum             <- rowSums(prob[ , c(11,12,13,14,15)], na.rm=TRUE)

# adjusted class probabilities (classification/proportions belonging to class)
prob$proportion_class_forest             <- (prob$"1" / prob$sum)
prob$proportion_class_prob_def_2001_2005 <- (prob$"2" / prob$sum)
prob$proportion_class_def_2006_2010      <- (prob$"3" / prob$sum)
prob$proportion_class_forestgain         <- (prob$"4" / prob$sum)
prob$proportion_class_nonforest          <- (prob$"5" / prob$sum)

# EPS
prob$eps_forest    <- sum(prob$adj_class_prob_forest)
prob$eps_def_2001_2005        <- sum(prob$adj_class_prob_def_2001_2005)
prob$eps_def_2006_2010   <- sum(prob$adj_class_prob_def_2006_2010)
prob$eps_forestgain    <- sum(prob$adj_class_prob_forestgain)
prob$eps_nonforest    <- sum(prob$adj_class_prob_nonforest)

# EV

# SE

## Calculate overall accuracy and class-wise user?s and producer?s accuracy from the confusion matrix.

# overall accuracy (not adjusted)
prob$overall_accuracy <- sum(prob$"1"[1]) + sum(prob$"2"[2]) + sum(prob$"3"[3]) + sum(prob$"4"[4]) + sum(prob$"5"[5])

# adjusted overall accuracy
prob <- prob[order(prob$value),]
prob$adj_overall_accuracy <- sum(prob$adj_class_prob_forest[1]) + sum(prob$adj_class_prob_def_2001_2005[2]) +
  sum(prob$adj_class_prob_def_2006_2010[3]) + sum(prob$adj_class_prob_forestgain[4]) + sum(prob$adj_class_prob_nonforest[5])

# producer's accuracy (not adjusted)
prob$producers_accuracy_forest  <- (prob$"1"[1]) / sum(prob$`1`)
prob$producers_accuracy_def_2001_2005      <- (prob$"2"[2]) / sum(prob$`2`)
prob$producers_accuracy_def_2006_2010 <- (prob$"3"[3]) / sum(prob$`3`)
prob$producers_accuracy_forestgain  <- (prob$"4"[4]) / sum(prob$`4`)
prob$producers_accuracy_nonforest  <- (prob$"5"[5]) / sum(prob$`5`)

# user's accuracy (not adjusted)
prob$users_accuracy_forest  <- (prob$"1"[1]) / prob$sum[1]
prob$users_accuracy_def_2001_2005      <- (prob$"2"[2]) / prob$sum[2]
prob$users_accuracy_def_2006_2010 <- (prob$"3"[3]) / prob$sum[3]
prob$users_accuracy_forestgain  <- (prob$"4"[4]) / prob$sum[4]
prob$users_accuracy_nonforest  <- (prob$"5"[5]) / prob$sum[5]

# producer's accuracy (adjusted)

prob$adj_producers_accuracy_forest  <- prob$adj_class_prob_forest[1] / prob$eps_forest
prob$adj_producers_accuracy_def_2001_2005      <- prob$adj_class_prob_def_2001_2005[2] / prob$eps_def_2001_2005
prob$adj_producers_accuracy_def_2006_2010 <- prob$adj_class_prob_def_2006_2010[3] / prob$eps_def_2006_2010
prob$adj_producers_accuracy_forestgain  <- prob$adj_class_prob_forestgain[4] / prob$eps_forestgain
prob$adj_producers_accuracy_nonforest  <- prob$adj_class_prob_nonforest[5] / prob$eps_nonforest

# user's accuracy (adjusted)

prob$adj_users_accuracy_forest  <- prob$adj_class_prob_forest[1] / prob$adj_class_prob_sum[1]
prob$adj_users_accuracy_def_2001_2005      <- prob$adj_class_prob_def_2001_2005[2] / prob$adj_class_prob_sum[2]
prob$adj_users_accuracy_def_2006_2010 <- prob$adj_class_prob_def_2006_2010[3] / prob$adj_class_prob_sum[3]
prob$adj_users_accuracy_forestgain  <- prob$adj_class_prob_forestgain[4] / prob$adj_class_prob_sum[4]
prob$adj_users_accuracy_nonforest  <- prob$adj_class_prob_forestgain[5] / prob$adj_class_prob_sum[5]

## Produce error-adjusted area estimates from the confusion matrix.

# calculate area in ha by using pixel count
prob$area <- prob$count*30^2/10000

prob$adj_area_forest  <- sum(prob$area) * prob$eps_forest
prob$adj_area_def_2001_2005      <- sum(prob$area) * prob$eps_def_2001_2005
prob$adj_area_def_2006_2010 <- sum(prob$area) * prob$eps_def_2006_2010
prob$adj_area_forestgain  <- sum(prob$area) * prob$eps_forestgain
prob$adj_area_nonforest  <- sum(prob$area) * prob$eps_nonforest

