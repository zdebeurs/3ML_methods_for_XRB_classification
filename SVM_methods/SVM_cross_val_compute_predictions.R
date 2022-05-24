# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract

# This script performs the cross-validation described in Section 4.2 for the Support Vector Machines Algorithm (Section 3.3)
# This script uses the SVM algorithm implemented in the R LIBSVM library (Chang & Lin 2011) and the latin hypercube sampling in the R lhs library (Carnell 2020).

# Be sure to install "e1071", "lhs", and "doFuture" if not already installed with "install.packages("e1071")", "install.packages("lhs")", and "install.packages("doFuture")"
library(e1071)
library(lhs)
library(doFuture)

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")

# Set directory for the training files location
# Choose the subsample number (D1-D10). Here we choose subsample D1, but this can be changed to D2, D3, etc.
directory = '10_sampled20Perc_Datasets/D'

# define a not in function
`%notin%` <- function(x,y) !(x %in% y) 

# Latin hypercube sampling of Cost(C) and Gamma (Y)
# 100 samples, 2 variables
A <- randomLHS(100, 2) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
# Define C range (2^-15 to 2^5)
B[,1] <-qunif(A[,1], min = -15, max = 5) 
C_list = 2**B[,1]
# Define Y range (2^-15 to 2^3)
B[,2] <-qunif(A[,2], min = -15, max = 3)
gamma_list = 2**B[,2]

# Plot the range of C, Y parameters from the latin hypercube sampling
plot(C_list, gamma_list)

# Choose whether to stick to the best values of C,Y from the paper or to try many different values of C, Y. To try different ones, changes this line to lhs_params_run = 'TRUE'
lhs_params_run = 'FALSE'#'TRUE'

# initialize parallelization of code
registerDoFuture()
plan(multiprocess)

# Run through subsampled datasets D1-D10 where x=1 corresponds to D1
x <- 1:10

# Run through subsamples D1-D10 in parallel
y <- foreach(num_rum = x, options(future.rng.onMisuse="ignore")) %dopar% {
  # Read in data
  directory_full = paste(directory,num_rum,'_Sampled_20percent_Training_and_Testing/', sep="") #'Sampled_20percent_Training_and_Testing/'
  file_list_z <- list.files(directory_full)#"Sampled_20percent_Training_and_Testing/")
  systems = strsplit(file_list_z, ".asc")
  systems = unlist(systems)
  systems
  systems_shuffled = sample(systems)
  # Check whether we want to try various C, Y params.
  if (lhs_params_run=="TRUE"){
    c_exp = C_list[num_rum]
    gamma_exp = gamma_list[num_rum]
  } else {
    # Stick to best parameters from paper
    c_exp = 0.655298078515272
    gamma_exp = 0.585144090708118
  }
  
  print(paste("C: ",c_exp, ", gamma: ", gamma_exp, sep=""))
 
  # Perform 44-fold cross-validation
  for (cross_iter in seq(1, length(systems), 1)){
    print(systems_shuffled[cross_iter]) 
    test_systems = systems_shuffled[cross_iter]
    print(test_systems)

    
    # Reading in data from MAXI files for Training
    fulldata <- {}
    for (i in 1:length(systems)){
      if (systems[i] %notin% test_systems){
        print(paste(directory_full, systems[i],".asc",sep=""))
        example = read.table(paste(directory_full,systems[i],".asc",sep=""))
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
    
    # classification mode
    # Load training data in data.frame:
    x <- fulldata[5:7]
    y <- fulldata[1]
    
    model <- svm(x, y[,1],type='C-classification', kernel="radial", gamma=gamma_exp,cost=c_exp, probability=TRUE)
    print(model)
    summary(model)
    
    # Predict the 1 system in the test set.
    for (i in 1:length(test_systems)){
      
      test_fulldata <- {}
      # Reading in data from MAXI files for Validation
      print(paste(directory_full, test_systems[i],".asc",sep=""))
      test_example = read.table(paste(directory_full,test_systems[i],".asc",sep=""))
      test_fulldata <- rbind(test_fulldata,test_example)
      
      # Add a column for star type
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
      
      # test with test example
      x_test <- test_fulldata[5:7]
      y_test <- test_fulldata[1]
      
      # Predict Output
      preds <- predict(model,x_test, probability = TRUE)
      table(preds, y_test[,1])
      
      probabilities = (attr(preds, "probabilities"))
      probabilities
      # New Error Prediction by just taking sd(prob(bh))
      
      total_sum = sum(probabilities[,"1"])+sum(probabilities[,"2"])+sum(probabilities[,"3"])
      blackhole_prob = mean(probabilities[,"1"])
      non_pulsar_prob = mean(probabilities[,"2"])
      pulsar_prob = mean(probabilities[,"3"])
      
      #Save individual output file for each source
      predictions <- data.frame(test_systems[i],blackhole_prob,non_pulsar_prob,pulsar_prob)
      
      dir.create(paste("SVM_methods/Predictions/D",num_rum,"_RBF_C=",round(c_exp,6),"_Y=",round(gamma_exp,6), "/", sep=""), showWarnings = FALSE)
      output_filename_asc = paste("SVM_methods/Predictions/D",num_rum,"_RBF_C=",round(c_exp,6),"_Y=",round(gamma_exp,6), "/",  test_systems[i],'.asc', sep="")
      if (file.exists(output_filename_asc)){
        output_filename_asc = paste(output_filename_asc,runif(1),'.asc', sep="")
      }
      write.table(predictions, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
                  row.names = FALSE, col.names = TRUE, quote = FALSE)
    } 
  }  
}

# Put files in appropriate folders (BH, NS, Bursters, Pulsar)
pred_folders = list.files( paste("SVM_methods/Predictions/",sep=""))
for (current_pred_folder in pred_folders){
  #BHs
  pred_folder = paste("SVM_methods/Predictions/",current_pred_folder, sep="")
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
