# BGP modified from kernlab package (Zoe de Beurs)
library(kernlab)

setwd("/Documents/Summer19/SAO_Research/simple_BGP/mit_supercloud_feb10_2022")#"/home/gridsan/zdebeurs/SAO_Research")

#Define kernel options
kernel_rbf = "laplacedot"

# Choose whether to apply the uniform cutoff or a specific cutoff
cutoff_type = '0.4' #'by_eye'#'1sigma'#'2sigma'#'0.4'# '1sigma_median'# 'by_eye_4U1916-053v2' # '2sigma'#'1sigma'# or '3sigma' or 'by_eye'

#For Training
directory = paste('Dec13_datasets_4U1916/Dec13_',cutoff_type,'cutoff_Sampled_20percent_Training_and_Testing/', sep="")#'/home/gridsan/zdebeurs/SAO_Research/Dec13_datasets_4U1916/Dec13_',cutoff_type,'cutoff_Sampled_20percent_Training_and_Testing/', sep="") #'Sampled_20percent_Training_and_Testing/'
file_list_z <- list.files(directory)
systems = strsplit(file_list_z, ".asc")
systems = unlist(systems)
systems_shuffled = sample(systems)
systems_shuffled = systems_shuffled[1:3]

# Set an output folder for predictions
output_folder = paste("Dec13_preds_4U1916/",cutoff_type,"cutoff_",kernel_rbf,"_kernel_preds_test/", sep="") #"/home/gridsan/zdebeurs/SAO_Research/Dec13_preds_4U1916/",cutoff_type,"cutoff_",kernel_rbf,"_kernel_preds_test/", sep="")
dir.create(output_folder, showWarnings = FALSE)

#idxs = (1:length(systems))
#randomize
#idxs = sample(idxs)

# run k=5 fold cross-validation (with 4 different kernels) in parallel
library(doParallel)
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

#registerDoParallel(cores=2)
#library(parallel)
#library(foreach)


# define a not-in list parameter
`%notin%` <- Negate(`%in%`)

# define number of cross-val folds (k=33)
x <- seq(1,2)#length(systems))

# parallelize the crossval component
#y <- foreach(cross_iter = x) %dopar% {print(cross_iter)}

# parallelize the kernel component, not the cross_val
#x_kernel <- seq(1,length(kernel_list))

#y <- foreach(kern = x_kernel) %dopar% {
#y <- foreach(cross_iter = x) %dopar% {
#for (cross_iter in x) {
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
    print(kernel_rbf)
    model <- gausspr(Class_num~., data=train.data, kernel=kernel_rbf,
                     kpar="automatic", var=1, variance.model = FALSE)
    
    # other version for supercloud
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
      
      # predict
      preds = predict(model, test.data[,-4], type="probabilities")
      probabilities = preds
      
      print(colMeans(probabilities, na.rm = FALSE, dims = 1))
      
      # New Error Prediction by just taking sd(prob(bh))
      total_sum = sum(probabilities[,1])+sum(probabilities[,2])+sum(probabilities[,3])
      blackhole_prob = mean(probabilities[,1])
      bh_se = sd(probabilities[,1])
      blackhole_prob
      bh_se
      non_pulsar_prob = mean(probabilities[,2])
      np_se = sd(probabilities[,2])
      non_pulsar_prob
      np_se
      pulsar_prob = mean(probabilities[,3])
      p_se = sd(probabilities[,3])
      pulsar_prob
      p_se
      
      # Save individual output file for each source
      predictions <- data.frame(test_systems[i],blackhole_prob,bh_se,non_pulsar_prob,np_se,pulsar_prob,p_se)
      
      output_filename_asc = paste(output_folder, test_systems[i],'.asc', sep="")
      if (file.exists(output_filename_asc)){
        output_filename_asc = paste(folder, test_systems[i],runif(1),'.asc', sep="")
      }
      write.table(predictions, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
                  row.names = FALSE, col.names = TRUE, quote = FALSE)
    
    }
}





