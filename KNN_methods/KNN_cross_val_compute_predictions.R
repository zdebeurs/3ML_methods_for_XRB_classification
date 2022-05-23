# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract

# This script performs the cross-validation described in Section 4.2 for the K-Nearest Neighbors Algorithm (Section 3.2)

# Be sure to install "caret" if not already installed with "install.packages("caret")"
library(caret)

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")

# Set directory for the training files. 
# Choose the subsample number (D1-D10). Here we choose subsample D1, but this can be changed to D2, D3, etc.
subsample_num = 'D2'
directory = paste("10_sampled20Perc_Datasets/",subsample_num,"_Sampled_20percent_Training_and_Testing/", sep="")
file_list_z = list.files(directory)
systems = strsplit(file_list_z, ".asc")
systems = unlist(systems)
systems

# shuffle the systems
systems_shuffled = sample(systems)

# define a not in function
`%notin%` <- function(x,y) !(x %in% y) 

# Choose the value of k. In the paper we consider k=2-50 and found that k=24 is the optimal choice for this dataset
k_choice =24

  
# #to do n-fold cross-validation
for (cross_iter in seq(1, length(systems))){
   print(cross_iter) 
   test_systems = systems_shuffled[cross_iter]
   print(test_systems)
  
  # Reading in data from MAXI files for Training
  fulldata <- {}
  for (i in 1:length(systems)){
    if (systems[i] %notin% test_systems){
      print(paste(directory, systems[i],".asc",sep=""))
      example = read.table(paste(directory,systems[i],".asc",sep=""))
      fulldata <- rbind(fulldata,example)
    }
  }
  #Add a column for star type
  compact <- rep(1,dim(fulldata)[1])
  hmbh <- which(fulldata[,2] == 'HMBH')
  lmbh <- which(fulldata[,2] == 'LMBH')
  bh <- c(hmbh,lmbh)
  atoll <- which(fulldata[,2] == 'Atoll')
  zsource <- which(fulldata[,2] == 'Zsource')
  bursters_as_np <- which(fulldata[,2] == 'Burster') 
  pulsars <- which(fulldata[,2] == 'Pulsar')
  nonpulsar <- c(atoll,zsource,bursters_as_np)
  compact[nonpulsar] <- rep(2,length(nonpulsar))
  compact[pulsars] <- rep(3,length(pulsars))
  fulldata <- cbind(compact,fulldata)
  names(fulldata) <- c("Class_num","System","Type","Date","CC3","CC1","CC2")
  
  head(fulldata)
  summary(fulldata)
  
  
  # load test.data
  # Predict each of the 44 test systems independently and write them to files
  for (i in 1:length(test_systems)){
    
    test_fulldata <- {}
    # Reading in data from MAXI files for Validation
    print(paste(directory, test_systems[i],".asc",sep=""))
    test_example = read.table(paste(directory,test_systems[i],".asc",sep=""))
    test_fulldata <- rbind(test_fulldata,test_example)
    
    #Add a column for star type
    compact <- rep("1",dim(test_fulldata)[1])
    hmbh <- which(test_fulldata[,2] == 'HMBH')
    lmbh <- which(test_fulldata[,2] == 'LMBH')
    bh <- c(hmbh,lmbh)
    atoll <- which(test_fulldata[,2] == 'Atoll')
    zsource <- which(test_fulldata[,2] == 'Zsource')
    bursters_as_np <- which(test_fulldata[,2] == 'Burster') 
    pulsars <- which(test_fulldata[,2] == 'Pulsar')
    nonpulsar <- c(atoll,zsource,bursters_as_np)
    compact[nonpulsar] <- rep("2",length(nonpulsar))
    compact[pulsars] <- rep("3",length(pulsars))
    test_fulldata <- cbind(compact,test_fulldata)
    names(test_fulldata) <- c("Class_num","System","Type","Date","CC3","CC1","CC2")
    
    head(test_fulldata)
    summary(test_fulldata)
    
  
    ##Run nomalization on first 4 coulumns of dataset because they are the predictors
    iris_norm <- as.data.frame(fulldata[,c(5,6,7)])
    test_iris_norm <- as.data.frame(test_fulldata[,c(5,6,7)])
    
    ##extract training set
    summary(iris_norm)
    summary(test_iris_norm)
    
    ##extract training set
    iris_train <- iris_norm
    
    ##extract testing set
    iris_test <- test_iris_norm
    
    ##extract 1st column of train dataset because it will be used as 'cl' argument in knn function.
    iris_target_category <- fulldata[,1]
    ##extract 1st column if test dataset to measure the accuracy
    iris_test_category <- test_fulldata[,1]
    
    ##run knn function
    prc_test_pred <- knn3Train(train=iris_train,test=iris_test,cl=iris_target_category,k=k_choice,prob = TRUE, use.all = TRUE)
    predicted_labels = {}
    predicted_errors = {}
    probabilities = attr(prc_test_pred, "prob")
    probabilities
    
    # Error Prediction by just taking sd(prob(bh))
    total_sum = sum(probabilities[,"1"])+sum(probabilities[,"2"])+sum(probabilities[,"3"])
    blackhole_prob = mean(probabilities[,"1"])
    non_pulsar_prob = mean(probabilities[,"2"])
    pulsar_prob = mean(probabilities[,"3"])
    
    
    #Save individual output file for each source
    predictions <- data.frame(test_systems[i],blackhole_prob,non_pulsar_prob,pulsar_prob)
    
    folder = paste("KNN_methods/Predictions/",subsample_num,"Predictions_k=",k_choice,"/", sep="")
    dir.create(paste(folder), showWarnings = FALSE)
    output_filename_asc = paste(folder, test_systems[i],'.asc', sep="")
    if (file.exists(output_filename_asc)){
      output_filename_asc = paste(folder, test_systems[i],runif(1),'.asc', sep="")
    }
    write.table(predictions, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
                row.names = FALSE, col.names = TRUE, quote = FALSE)
  } 
}  

# Put files in appropriate folders (BH, NS, Bursters, Pulsar)
pred_folders = list.files( paste("KNN_methods/Predictions/",sep=""))
for (current_pred_folder in pred_folders){
  #BHs
  pred_folder = paste("KNN_methods/Predictions/",current_pred_folder, sep="")
  print(pred_folder)
  dir.create(paste(pred_folder,"/BH",sep=""))
  BH_file_list <- list.files("Example_pred_files/BH")
  BH_file_list
  file.copy(paste(pred_folder,"/", BH_file_list, sep=""),paste(pred_folder,"/BH/", BH_file_list, sep=""))
  file.remove(paste(pred_folder,"/", BH_file_list, sep=""))
  
  # NSs
  dir.create(paste(pred_folder,"/NS",sep=""))
  NS_file_list <- list.files("Example_pred_files/NS")
  NS_file_list
  file.copy(paste(pred_folder,"/", NS_file_list, sep=""),paste(pred_folder,"/NS/", NS_file_list, sep=""))
  file.remove(paste(pred_folder,"/", NS_file_list, sep=""))
  
  # Pulsars
  dir.create(paste(pred_folder,"/Pulsar",sep=""))
  Pulsar_file_list <- list.files("Example_pred_files/Pulsar")
  Pulsar_file_list
  file.copy(paste(pred_folder,"/", Pulsar_file_list, sep=""),paste(pred_folder,"/Pulsar/", Pulsar_file_list, sep=""))
  file.remove(paste(pred_folder,"/", Pulsar_file_list, sep=""))
}



