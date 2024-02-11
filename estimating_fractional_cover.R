# load required packages

pck_list <- c("assertthat","ggplot2", "tidyr", "dplyr","raster", "e1071", "rgdal", "viridisLite")
lapply(pck_list, require, character.only = TRUE)

dir_main <- "~/Desktop/EarthObservation/data_08"
setwd(dir_main)

boa <- raster("2018-2019_091-319_HL_TSA_SEN2L_TSS_20190726_SEN2A_crop.tif")
val <- readOGR("validation_20190726.gpkg")

# =============================================================================
# 1) Input spectra

slib <- read.table(file = "S2_EM.txt", header = T, sep = ",")

# visualisation

# add wavenlenghts to data.frame
slib$cwl <- c(492.7, 559.8, 664.6, 704.1, 740.5, 782.8, 832.8, 864.7, 1613.7, 2202.4)

# PV
ggplot(slib, aes(x=cwl, y=PV)) + 
  geom_line()

# NPV
ggplot(slib, aes(x=cwl, y=NPV)) + 
  geom_line()

# soil
ggplot(slib, aes(x=cwl, y=soil)) + 
  geom_line()

# shade
ggplot(slib, aes(x=cwl, y=shade)) + 
  geom_line()

#What are the main characteristics of the spectra?
# - PV shows the typical red edge
  # - wavelenghts 650 nm to 750nm probably most important
# - NPV increase in reflection up until ~1600nm; then sharp decrease again
  # - wavenlenghts 1500nm to 1700nm might be important for differentiation 
# - soil: steady increase in reflection
# - shade shows no change along the different wavelengths; stable at 100

#How do the spectra differ between the classes and which wavelength regions could be most important for differentiating the classes?

# =============================================================================
# 2) Linear spectral mixtures

# cover proportions
fraction_PV <- 0.1      # 10% PV
fraction_NPV <- 0.8     # 80% NPV
fraction_soil <- 0.05   # 5% Soil
fraction_shade <- 0.05  # 5% shade

# Do we violate the assumption that all surface components represent 100% of the surface area?
if((fraction_PV + fraction_NPV + fraction_soil+ fraction_shade) != 1) print('Fractions don´t sum to 1.')

# Create a linear mixture of the endmember spectra
model_spectrum <- fraction_PV * slib$PV + 
  fraction_NPV * slib$NPV +
  fraction_soil * slib$soil + 
  fraction_shade * slib$shade

# We could simulate imperfect measurements by adding random noise.
noise <- rnorm(10, mean=0, sd=0.02)

# Append the modeled spectrum to the endmembers data.frame
slib$model_spectrum <- model_spectrum + noise

# Convert the spectra into long format for plotting with ggplot2
slib_vis <- pivot_longer(slib, -c(band, cwl)) 

# Visualize the modeled spectrum in comparison to the endmember spectra
ggplot(slib_vis) + 
  geom_line(aes(x=cwl, y=value, color=name, linetype=name))+
  scale_color_manual(values=c("black", "steelblue", "darkgreen", "darkgrey","firebrick"), name="Spectrum")+
  scale_linetype_manual(values=c("dotdash", rep("solid", 4)), name="Spectrum")+
  scale_y_continuous(labels=seq(0,1,0.1), breaks=seq(0,10000,1000))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Wavelength (nm)", y="Reflectance")

# =============================================================================
# 3) Synthetic training data generation

# Why can it be beneficial to use not only one, but many endmember spectra for each class?

synthmix <- function(df, cl_target, cl_background, n_samples=1000,
                     mix_complexity=c(2, 3, 4), p_mix_complexity=c(.7, .2, .1),
                     within_class_mixture=TRUE, include_endmember=TRUE){
  
  "Function to generate synthetic training data mixtures from pure endmember
  spectra.
  
  df:                 (list) Input dataframe. First column must contain the
                      class-IDs in integer format. Remaining columns must 
                      contain the features to be mixed. 
  cl_target:          (int) Target class' integer ID value.
  cl_background:      (int) Background class' integer ID value(s). Vector for 
                      multiple classes, e.g. 'c(2, 3, 4)'.
  n_samples:          (int) Number of synthetic training points to generate.
  mix_complexity:     (int) Vector with desired number of possible mixtures
                      between different classes.
  p_mix_complexity:   (float) Vector containing desired occurence propabilities 
                      associated to the number of possible mixtures 
                      (i.e. mix_complexity). Must be of same length as 
                      'mix_complexity' argument.
  
  returns:            (list) Dataframe with linearily mixed features and 
                      corresponding fraction of target class (i.e. cl_target)
  "
  
  # total number of classes
  all_ems <- c(cl_target, cl_background)
  n_em <- length(all_ems)
  
  # create empty df to store training data
  df_mixture <- setNames(data.frame(matrix(ncol = ncol(df), nrow = 0)), 
                         c(names(df)[2:length(df)], "fraction")) 
  
  # index list of EMs for sampling
  idx_em <- list()
  for (em in all_ems){
    idx_em[[em]] <- which(df[,1] == em)
  }
  
  # vector for fraction calculation
  zero_one <- integer(nrow(df))
  zero_one[idx_em[[cl_target]]] <- 1
  
  # iterator for generating each synthetic mixture 
  for (i in 1:n_samples) {
    
    if (length(p_mix_complexity) == 1){
      complexity = mix_complexity
    } else {
      # sample mixing complexity based on mixing likelihoods
      complexity = sample(as.vector(mix_complexity), 
                          size = 1, 
                          prob = as.vector(p_mix_complexity)) 
    }
    
    # select background EMs which will be included in the mixture
    if (within_class_mixture){
      background <- sample(all_ems, complexity - 1, replace = TRUE)
    } else {
      background <- sample(cl_background, complexity - 1, replace = FALSE)
    }
    
    # sample indices of selected EMs
    response <- c(cl_target, background)      
    drawn_index <- c()
    for (r in response){
      drawn_index <- c(drawn_index, sample(idx_em[[r]], 1))
    }
    drawn_features <- df[drawn_index, 2:length(df)]
    drawn_fraction <- zero_one[drawn_index]
    
    # sample random weights
    drawn_weights <- c()
    for (j in 1:(complexity-1)){
      if (j == 1){
        weight <- runif(1)
      } else {
        weight <- runif(1) * (1. - sum(drawn_weights))
      }
      drawn_weights <- c(drawn_weights, weight)
    }
    drawn_weights <- c(drawn_weights, (1. - sum(drawn_weights)))
    
    # calculate mixtures and associated fractions
    calc_mixtures <- apply(drawn_features * drawn_weights, 2, FUN=sum)
    calc_fraction <- sum(drawn_fraction * drawn_weights)
    
    # append to df
    df_mixture[nrow(df_mixture)+1,] <- c(calc_mixtures, calc_fraction)
  }
  
  if (include_endmember){
    df_endmember <- cbind(df[,2:length(df)], zero_one)
    colnames(df_endmember) <- c(names(df)[2:length(df)], "fraction")
    df_mixture <- rbind(df_mixture, df_endmember)
  }
  
  return(df_mixture)
  
}

# Format your data.frame to meet the function’s criteria

head(slib_vis)

# reorder
slib_vis <- arrange(slib_vis, name)

# Create classID by group
df <- transform(slib_vis, ID = as.integer(factor(name)))

# classID as first column
colnames(df)
df <- df[, c(5, 3, 1, 2, 4)]

# classID 1: model_spectrum
# classID 2: NPV
# classID 3: PV
# classID 4: shade
# classID 5: soil

df <- df[-2]
df2 <- df[-3]

df2 <- reshape(df2, idvar = "ID", timevar = "band", direction = "wide")

# snthmix function
# target classes: 2,3,5

# PV (ID 3)
df_PV <- synthmix(df2 ,cl_target = 3,cl_background = c(1,2,4,5), n_samples = 1000,mix_complexity = c(2,3), 
         p_mix_complexity = c(.5 , .5))

#NPV (ID 2)
df_NPV <- synthmix(df2 ,cl_target = 2,cl_background = c(1,3,4,5), n_samples = 1000,mix_complexity = c(2,3), 
                  p_mix_complexity = c(.5 , .5))

#soil (ID 5)
df_soil <- synthmix(df2 ,cl_target = 5,cl_background = c(1,2,3,4), n_samples = 1000,mix_complexity = c(2,3), 
                   p_mix_complexity = c(.5 , .5))

# =============================================================================
# 4) Modeling fractional cover

## PV model buillding
# PV : RFR model 
PV_RF <- randomForest(as.factor(fraction) ~ .,df_PV,importance = T, proximity = T, ntree = 1000, type ="regression")

# PV : SVR model 
#PV_svm <- svm(as.factor(fraction) ~ ., data= df_PV)
cv <- tune.control(cross = 10) 
PV_svm_tune <- tune.svm(as.factor(fraction) ~ ., data = df_PV, 
                        gamma = 0.1, cost = 0.1, epsilon = 0.001, tunecontrol = cv)
PV_svm_best <- PV_svm_tune$best.model

## NPV model building
# NPV : RFR model 
NPV_RF <- randomForest(as.factor(fraction) ~ .,df_NPV,importance = T, proximity = T, ntree = 1000, type ="regression")

# NPV : SVR model 
#NPV_svm <- svm(as.factor(fraction) ~ ., data= df_NPV)
NPV_svm_tune <- tune.svm(as.factor(fraction) ~ ., data = df_NPV, 
                         gamma = 0.1, cost = 0.1, epsilon = 0.001, tunecontrol = cv)
NPV_svm_best <- NPV_svm_tune$best.model

## Soil model building
# Soil : RFR model 
Soil_RF <- randomForest(as.factor(fraction) ~ .,df_soil,importance = T, proximity = T, ntree = 1000, type ="regression")

# NPV : SVR model 
#Soil_svm <- svm(as.factor(fraction) ~ ., data= df_soil)
soil_svm_tune <- tune.svm(as.factor(fraction) ~ ., data = df_soil, 
                          gamma = 0.1, cost = 0.1, epsilon = 0.001, tunecontrol = cv)
soil_svm_best <- soil_svm_tune$best.model

# Prediction

#renaming the bands of the Sentinel-2 image
names(boa) <- paste("B", seq(1,10), sep="")

# renaming does not work

#Error in `names<-`(`*tmp*`, value = c("B1", "B2", "B3", "B4", "B5", "B6",  : 
#incorrect number of layer names

#from here on now the code is not working

library(parallel)

# predict() PV
pred_PV <- predict(PV_svm_best, boa)

#beginCluster(detectCores()-1)  # or any integer number smaller than the amount of cores you have available
#pred_PV <- clusterR(boa, raster::predict, args = list(model = PV_svm_best))
#endCluster()
writeRaster(pred_PV, "prediction_PV_svm", format='GTiff', overwrite=T)

pred_PV_RF <- predict(PV_RF, boa)
writeRaster(pred_PV_RF, "prediction_PV_RF", format='GTiff', overwrite=T)

# predict() NPV
pred_NPV <- predict(NPV_svm_best, boa)
writeRaster(pred_NPV, "prediction_NPV_svm", format='GTiff', overwrite=T)

pred_NPV_RF <- predict(NPV_RF, boa)
writeRaster(pred_NPV_RF, "prediction_NPV_RF", format='GTiff', overwrite=T)

# predict() Soil
pred_soil <- predict(soil_svm_best, boa)
writeRaster(pred_soil, "prediction_soil_svm", format='GTiff', overwrite=T)

pred_soil_RF <- predict(Soil_RF, boa)
writeRaster(pred_soil_RF, "prediction_soil_RF", format='GTiff', overwrite=T)

# =============================================================================
# 5) Evaluation of fractional cover

validation <- readOGR("validation_20190726.gpkg")

# Set negative values to 0
# Set values larger than 1 to 1

