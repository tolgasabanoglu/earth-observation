# 1- Training a Random Forest model

dir_main <- '~/Desktop/EarthObservation/session_03-05'
setwd(dir_main)

#libraries 
l_packages <- c("raster", "rgdal", "lubridate")
for (p in l_packages){
  if(! p %in% installed.packages()){
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = T)
}

# read training point locations
file_training_pints <- "vector/s03_training_data.gpkg"
train <- readOGR(file_training_pints)


pbc1 <- stack("~/Desktop/EarthObservation/session_03-05/imagery/bap/LNDLG_BAP_Y2015-4_DOY180-50.tif")
pbc2 <- stack("~/Desktop/EarthObservation/session_03-05/imagery/bap/LNDLG_BAP_Y22015-4_DOY200-110.tif")

list_stm <- list.files(path = "~/Desktop/EarthObservation/session_03-05/imagery/stm", ignore.case=TRUE, full.names = TRUE)

stack_stm <- stack(list_stm)

stm_own <- stack("~/Desktop/EarthObservation/session_03-05/imagery/stm/tcb_sd_2015-2016_5-9.tif")
all <- stack(pbc1, pbc2, stack_stm,stm_own)

#pb1
pbc1_df <- extract(pbc1, df=T, train)
#add classid
pbc1_df$classid <- train$classID
#remove doy and year columns
pbc1_df <- pbc1_df[ , -which(names(pbc1_df) %in% c("LNDLG_BAP_Y2015.4_DOY180.50.7","ID","LNDLG_BAP_Y2015.4_DOY180.50.8","LNDLG_BAP_Y2015.4_DOY180.50.11","LNDLG_BAP_Y2015.4_DOY180.50.12","LNDLG_BAP_Y2015.4_DOY180.50.13"))]


#pbc2
pbc2_df <- extract(pbc2, df=T, train)
#add classid
pbc2_df$classid <- train$classID
#remove doy and year columns
pbc2_df <- pbc2_df[ , -which(names(pbc2_df) %in% c("LNDLG_BAP_Y2015.4_DOY180.50.7","ID","LNDLG_BAP_Y2015.4_DOY180.50.8","LNDLG_BAP_Y2015.4_DOY180.50.11","LNDLG_BAP_Y2015.4_DOY180.50.12","LNDLG_BAP_Y2015.4_DOY180.50.13"))]

#stack_stm
stack_stm_df <- extract(stack_stm, df=T, train)
#add classid
stack_stm_df$classid <- train$classID
#remove ID
stack_stm_df <- stack_stm_df[ , -which(names(stack_stm_df) %in% c("ID"))]

#stm_own
stm_own_df <- extract(stm_own, df=T, train)
#add classid
stm_own_df$classid <- train$classID

#classid as factor
pbc1_df$classid <-as.factor(pbc1_df$classid)
pbc2_df$classid <-as.factor(pbc2_df$classid)
stack_stm_df$classid <-as.factor(stack_stm_df$classid)
stm_own_df$classid <-as.factor(stm_own_df$classid)


#omit NA values
pbc1_df <- na.omit(pbc1_df)
pbc2_df <- na.omit(pbc2_df)
stack_stm_df <- na.omit(stack_stm_df)
stm_own_df <- na.omit(stm_own_df)


#build randomForest models.
library(randomForest)
pbc1_forest <- randomForest(classid ~ ., data=pbc1_df, ntree=1000)
pbc2_forest <- randomForest(classid ~., data=pbc2_df, ntree=1000)
stack_stm_df_forest <- randomForest(classid ~., data=stack_stm_df, ntree=1000)
stm_own_forest <- randomForest(classid ~., data=stm_own_df, ntree=1000)


# 2- Investigating model performance

# a. Which model has the lowest OOB error? How large is the difference between 
# the best and second best model in terms of overall accuracy?

print(pbc1_forest)
print(pbc2_forest)
print(stack_stm_df_forest)
print(stm_own_forest)

#best: stack_stm_forest: %36.36
#second best: pbc1_forest %44.94

# b. How does the OOB behave when increasing the number of trees in your model (ntree)? 
# You can access the OOB per number of trees via err.rate. Use this information to determine a suitable value for ntree. From a model’s predictive performance ntree cannot be too high, 
# only the computational performance will decrease. The aim is to find the spot where the error rate stabilizes (converges) and adding more trees would no longer improve accuracy.

#model with 1000 trees
print(stack_stm_df_forest) #OOB error rate is 36.36%

#model with 1200 trees
stack_stm_df_forest2 <- randomForest(classid ~., data=stack_stm_df, ntree=1200)
print(stack_stm_df_forest2) #OBB error rate is 38.64%

#OOB error goes up when increasing the numbers of trees in the model 

# c. In the model with the lowest OOB error, which of the four classes has the highest OOB error?

print(stack_stm_df_forest)
#class 2 has the highest error (0.7619)


# d. In case you are not satisfied with your model performance, consider joining forces with another team and merge your training datasets. 
# Feel free to experiment and document your findings.


# 3) Final model parametrization and variable importances

#a. Train a final model with the best combination of images and ntrees.

stack_stm_df_forest <- randomForest(classid ~., data=stack_stm_df, ntree=1000)
print(stack_stm_df_forest)

# b. Investigate the variable importances using varImpPlot(). Use partialPlot() to produce partial dependence plots for your most important predictor and all four classes. 
# Can you explain the differences between classes?

importance(stack_stm_df_forest)        
varImpPlot(stack_stm_df_forest)

# 4) Classification
# Perform a classification of the image stack using the predict() function. 
#Write the resulting map to disk in GTiff format. When doing so, choose the appropriate datatype argument.

# option 1) single core

map <- predict(stack_stm, stack_stm_df_forest)

outname <- paste0('imagery/stm/stm_stack_prediction', '.tif')
writeRaster(map, outname, datatype="INT2S", overwrite=T)
