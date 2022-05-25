# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract

# This script performs the cross-validation described in Section 4.2 for the Bayesian Gaussian Process Algorithm (Section 3.3)
# This script uses the BGP algorithm implemented in the R kernlab library (Karatzoglou et al. 2004).

# Be sure to install "kernlab" and "doParallel" if not already installed with "install.packages("kernlab")" and "install.packages("doParallel")"
library(kernlab)
library(doParallel)

# THIS WILL NEED TO BE CHANGED based on the path on your supercomputer where you put the file
supercomputer_path_to_files = ("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification/")#"/home/gridsan/zdebeurs/SAO_Research"
setwd(supercomputer_path_to_files)

# Define kernel for Bayesian Gaussian Process. We choose laplacedot here but we also tried 'rbfdot' or 'anovadot' in the paper which you are welcome to try.
kernel_rbf = "laplacedot"

# Set directory for the training files. 
# Choose the subsample number (D1-D10). Here we choose subsample D1, but this can be changed to D2, D3, etc.
subsample_num = 'D1'
directory = paste(supercomputer_path_to_files,'10_sampled20Perc_Datasets/',subsample_num, "_Sampled_20percent_Training_and_Testing/",sep="")
file_list_z <- list.files(directory)
systems = strsplit(file_list_z, ".asc")
systems = unlist(systems)
systems_shuffled = sample(systems)
systems_shuffled = systems_shuffled #[1:3]

# Set an output folder for predictions
# First create folder "Predictions" on supercomputer (if it doesn't already exist)
dir.create(paste(supercomputer_path_to_files,"BGP_methods/Predictions", sep=""), showWarnings = FALSE)
# choose a filename for individual runs
todays_date = Sys.Date()
output_folder = paste(supercomputer_path_to_files,"BGP_methods/Predictions/",subsample_num, "_",kernel_rbf,"_",todays_date,"/", sep="") 
# create folders within the Predictions folder for each of runs (if they do not already exist)
dir.create(output_folder, showWarnings = FALSE)


# Initialize multiple cores. Here we choose 11 cores but this can be changed depending on your supercomputer's specifications.
num_cores = 11
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

# define a not-in list parameter
`%notin%` <- Negate(`%in%`)

# define number of cross-val folds to equal the number of XRB systems (44)
x <- seq(1,length(systems))

# Perform cross-validation
y <- foreach(cross_iter = x) %dopar% {
    library(kernlab)
    #for (cross_iter in seq(1, length(systems), 1)){
    #print(systems_shuffled[cross_iter]) 
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
    compact <- rep("1",dim(fulldata)[1])
    hmbh <- which(fulldata[,2] == 'HMBH')
    lmbh <- which(fulldata[,2] == 'LMBH')
    bh <- c(hmbh,lmbh)
    atoll <- which(fulldata[,2] == 'Atoll')
    zsource <- which(fulldata[,2] == 'Zsource')
    bursters_as_np <- which(fulldata[,2] == 'Burster') 
    pulsars <- which(fulldata[,2] == 'Pulsar')
    nonpulsar <- c(atoll,zsource,bursters_as_np)
    compact[nonpulsar] <- rep("2",length(nonpulsar))
    compact[pulsars] <- rep("3",length(pulsars))
    fulldata <- cbind(compact,fulldata)
    names(fulldata) <- c("Class_num","System","Type","Date","CC3","CC1","CC2")
      
    head(fulldata)
    summary(fulldata)
      
    ## classification mode
    
    # Load training data in data.frame:
    x <- fulldata[5:7]
    #x <- as.double[,1]
    y <- fulldata[1]
    #y <- as.integer(y[,1])
    
    # Create the data frame 
    train.data <- data.frame(
      #emp_name = c("MJD","2-3 keV","2-3 keV error","3-5 keV","3-5 keV error", "5-12 keV", "5-12 keV error"),
      CC1 = fulldata[5],
      CC2 = fulldata[6],
      CC3 = fulldata[7],
      Class = fulldata[1],
      stringsAsFactors = FALSE
    )
    
    
    # train model
    model <- gausspr(y=train.data$Class_num, x=t(train.data[1:3]), kernel=kernel_rbf,
                     kpar="automatic", var=1, variance.model = FALSE)
    
    # load test.data
    # Predict each of the 5 test systems independently and write them to files
    #for (i in 1:length(test_systems)){
    for (i in 1:length(test_systems)){
      test_fulldata <- {}
      # Reading in data from MAXI files for Validation
      print(paste(directory, test_systems[i],".asc",sep=""))
      test_example = read.table(paste(directory,test_systems[i],".asc",sep=""))
      test_fulldata <- rbind(test_fulldata,test_example)
      #print(test_fulldata)
      
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
      
      #head(test_fulldata)
      #summary(test_fulldata)
      
      # Create the data frame 
      test.data <- data.frame(
        #emp_name = c("MJD","2-3 keV","2-3 keV error","3-5 keV","3-5 keV error", "5-12 keV", "5-12 keV error"),
        CC1 = test_fulldata[5],
        CC2 = test_fulldata[6],
        CC3 = test_fulldata[7],
        Class = test_fulldata[1],
        stringsAsFactors = FALSE
      )
      
      # predict classification of test data
      preds = predict(model, test.data[,-4], type="probabilities")
      probabilities = preds
      
      print(colMeans(probabilities, na.rm = FALSE, dims = 1))
      
      # Compute mean probability over all data point belonging to XRB source
      total_sum = sum(probabilities[,1])+sum(probabilities[,2])+sum(probabilities[,3])
      blackhole_prob = mean(probabilities[,1])
      non_pulsar_prob = mean(probabilities[,2])
      pulsar_prob = mean(probabilities[,3])
      
      # Save individual output file for each source
      predictions <- data.frame(test_systems[i],blackhole_prob,non_pulsar_prob,pulsar_prob)
      
      output_filename_asc = paste(output_folder, test_systems[i],'.asc', sep="")
      if (file.exists(output_filename_asc)){
        output_filename_asc = paste(folder, test_systems[i],runif(1),'.asc', sep="")
      }
      write.table(predictions, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
                  row.names = FALSE, col.names = TRUE, quote = FALSE)
    
    }
}





