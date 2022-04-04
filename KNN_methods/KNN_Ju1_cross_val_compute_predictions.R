# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# Please cite our paper when using any part of this code. [INCLUDE CITATION LINK]

# This script performs the cross-validation described in Section 4.2 for the K-Nearest Neighbors Algorithm (Section 3.2)

# Be sure to install "caret" if not already installed with "install.packages("caret")". Please do the same for the packages "doFuture", "foreach", and "future"
library(caret)
library(doFuture)

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")

# Set directory to save the output results in
output_directory = 'KNN_methods/April4_resampled_10_runs/'
dir.create(output_directory, showWarnings = FALSE)

# define a not in function
`%notin%` <- function(x,y) !(x %in% y) 

# set the range of k to test for the cross-validation procedure. This can also be set to a single number if you only want to test a specific vaue of k.
k_range = 2:50 

# run the cross-validation across the ten subsampled datasets in parallel
registerDoFuture()
plan(multiprocess)

# parallelize code, runs through the 10 different subsamples
x <-  1:10 #1:4#1:33

y <- foreach(num_run = x) %dopar% {
  #For Training
  directory = paste('10_sampled20Perc_Datasets/D',num_run,'_Sampled_20percent_Training_and_Testing/', sep="") #'Sampled_20percent_Training_and_Testing/'
  file_list_z <- list.files(directory)#"Sampled_20percent_Training_and_Testing/")
  systems = strsplit(file_list_z, ".asc")
  systems = unlist(systems)
  systems
  systems_shuffled = sample(systems)
  
  
  k_choice = 24#k_range[num_run]
  print(paste("K = ",k_choice))
  
  # to do a 11-fold cross validation:
  #for (cross_iter in seq(1, length(systems), 3)){
  #  print(cross_iter)
  #  test_systems = systems_shuffled[cross_iter:(cross_iter+2)]
  #  print(test_systems)
  
  # #to do n-fold cross-val
  for (cross_iter in seq(1, length(systems))){
     print(cross_iter) 
     test_systems = systems_shuffled[cross_iter]
     print(test_systems)

    # 5-fold cross-validation
    #for (cross_iter in seq(1, length(systems), 5)){
    #  if (cross_iter != 31){
    #    print(systems_shuffled[cross_iter:(cross_iter+4)])
    #    test_systems = systems_shuffled[cross_iter:(cross_iter+4)]
    #    print(test_systems)
    #  }
    #  else{
    #    print(systems_shuffled[(cross_iter-2):(cross_iter+2)])
    #    test_systems = systems_shuffled[(cross_iter-2):(cross_iter+2)]
    #    print(test_systems)
    #  }
    
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
    # Predict each of the 5 test systems independently and write them to files
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
      #attributes(.Last.value)
      
      predicted_labels = {}
      predicted_errors = {}
      probabilities = attr(prc_test_pred, "prob")
      probabilities
      
      # New Error Prediction by just taking sd(prob(bh))
      total_sum = sum(probabilities[,"1"])+sum(probabilities[,"2"])+sum(probabilities[,"3"])
      blackhole_prob = mean(probabilities[,"1"])
      bh_se = sd(probabilities[,"1"])
      blackhole_prob
      bh_se
      non_pulsar_prob = mean(probabilities[,"2"])
      np_se = sd(probabilities[,"2"])
      non_pulsar_prob
      np_se
      pulsar_prob = mean(probabilities[,"3"])
      p_se = sd(probabilities[,"3"])
      pulsar_prob
      p_se
      
      #Save individual output file for each source
      predictions <- data.frame(test_systems[i],blackhole_prob,bh_se,non_pulsar_prob,np_se,pulsar_prob,p_se)
      
      folder = paste(output_directory,"D",num_run,"_Predictions_Dec_21_k=",k_choice,"/", sep="")
      dir.create(paste(folder), showWarnings = FALSE)
      output_filename_asc = paste(folder, test_systems[i],'.asc', sep="")
      if (file.exists(output_filename_asc)){
        output_filename_asc = paste(folder, test_systems[i],runif(1),'.asc', sep="")
      }
      write.table(predictions, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
                  row.names = FALSE, col.names = TRUE, quote = FALSE)
    } 
  }  
}


setwd("~/Documents/Summer19/SAO_Research/KNN_methods")
pred_folders = list.files("Jul1_resampled_10_runs")

for (pred_folder_a in pred_folders[2:10]){
  # Put files in appropriate folders (BH, NS, Bursters, Pulsar)
  # BHs
  #i = 1
  #pred_folder_a = pred_folders[1] #"Predictions_Dec_20_RBF_C=0.000129602894755257Y=7.33829867635422e-05"
  pred_folder = paste("Jul1_resampled_10_runs/",pred_folder_a, sep="")
  print(pred_folder)
  dir.create(paste(pred_folder,"/BH",sep=""))
  BH_file_list <- list.files("~/Documents/Summer19/SAO_Research/KNN_methods/Dec21_repeat_10_runs/Predictions_Dec_21_k=24run1/BH")
  BH_file_list
  file.copy(paste(pred_folder,"/", BH_file_list, sep=""),paste(pred_folder,"/BH/", BH_file_list, sep=""))
  file.remove(paste(pred_folder,"/", BH_file_list, sep=""))
  
  # NSs
  dir.create(paste(pred_folder,"/NS",sep=""))
  NS_file_list <- list.files("~/Documents/Summer19/SAO_Research/KNN_methods/Dec21_repeat_10_runs/Predictions_Dec_21_k=24run1/NS")
  NS_file_list
  file.copy(paste(pred_folder,"/", NS_file_list, sep=""),paste(pred_folder,"/NS/", NS_file_list, sep=""))
  file.remove(paste(pred_folder,"/", NS_file_list, sep=""))
  
  # Pulsars
  dir.create(paste(pred_folder,"/Pulsar",sep=""))
  Pulsar_file_list <- list.files("~/Documents/Summer19/SAO_Research/KNN_methods/Dec21_repeat_10_runs/Predictions_Dec_21_k=24run1/Pulsar")
  Pulsar_file_list
  file.copy(paste(pred_folder,"/", Pulsar_file_list, sep=""),paste(pred_folder,"/Pulsar/", Pulsar_file_list, sep=""))
  file.remove(paste(pred_folder,"/", Pulsar_file_list, sep=""))
}

# Fix the CygX-3 problem (move from NS -> BH)
setwd("~/Documents/Summer19/SAO_Research/KNN_methods")
pred_folders = list.files("Dec21_preds")

for (pred_folder_a in pred_folders){
  print(pred_folder_a)
  #pred_folder_a = "Predictions_Dec_21_k=24run10"
  file.copy(paste("Dec21_preds/",pred_folder_a,"/NS/", "CygX-3.asc", sep=""),paste("Dec21_preds/",pred_folder_a,"/BH/", "CygX-3.asc", sep=""))
  file.remove(paste("Dec21_preds/",pred_folder_a,"/NS/", "CygX-3.asc", sep=""))
}


file.copy("Dec21_repeat_10_runs/Predictions_Dec_21_k=24run2/NS/CygX-3.asc", "Dec21_repeat_10_runs/Predictions_Dec_21_k=24run2/BH/CygX-3.asc")

#______________old_______________________________

#For Training
directory = 'Training/'
file_list_z <- list.files("Training/")
systems = strsplit(file_list_z, ".asc")
systems = unlist(systems)
systems

# For a test case
test_directory = 'Validation/'
test_file_list_z <- list.files("Validation/")
test_systems = strsplit(test_file_list_z, ".asc")
test_systems = unlist(test_systems)
test_systems


# Reading in data from MAXI files for Training
fulldata <- {}
for (i in 1:length(systems)){
  print(paste("~/Documents/Summer19/SAO_Research/KNN_methods/",directory, systems[i],".asc",sep=""))
  example = read.table(paste(directory,systems[i],".asc",sep=""), skip =1)
  fulldata <- rbind(fulldata,example)
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
  
head(fulldata)
summary(fulldata)

# Reading in data from MAXI files for Validation
test_fulldata <- {}
for (i in 1:length(test_systems)){
  print(paste("~/Documents/Summer19/SAO_Research/KNN_methods/",test_directory, test_systems[i],".asc",sep=""))
  test_example = read.table(paste(test_directory,test_systems[i],".asc",sep=""))
  test_fulldata <- rbind(test_fulldata,test_example)
}
#Add a column for star type
compact <- rep(1,dim(test_fulldata)[1])
hmbh <- which(test_fulldata[,2] == 'HMBH')
lmbh <- which(test_fulldata[,2] == 'LMBH')
bh <- c(hmbh,lmbh)
atoll <- which(test_fulldata[,2] == 'Atoll')
zsource <- which(test_fulldata[,2] == 'Zsource')
bursters_as_np <- which(test_fulldata[,2] == 'Burster') 
pulsars <- which(test_fulldata[,2] == 'Pulsar')
nonpulsar <- c(atoll,zsource,bursters_as_np)
compact[nonpulsar] <- rep(2,length(nonpulsar))
compact[pulsars] <- rep(3,length(pulsars))
test_fulldata <- cbind(compact,test_fulldata)

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
prc_test_pred <- knn3Train(train=iris_train,test=iris_test,cl=iris_target_category,k=21,prob = TRUE, use.all = TRUE)
#attributes(.Last.value)

#Cross-Validation version
#prc_test_pred <- knn.cv(iris_train,cl=iris_target_category,k=7,prob = TRUE)

predicted_labels = {}
predicted_errors = {}
probabilities = attr(prc_test_pred, "prob")


total_sum = sum(probabilities[,"1"])+sum(probabilities[,"2"])+sum(probabilities[,"3"])
blackhole_prob = mean(probabilities[,"1"])
bh_se = sd(probabilities[,"1"])
blackhole_prob
bh_se
non_pulsar_prob = mean(probabilities[,"2"])
np_se = sd(probabilities[,"1"])
non_pulsar_prob
np_se
pulsar_prob = mean(probabilities[,"3"])
p_se = sd(probabilities[,"3"])
pulsar_prob
p_se


# for (i in 1:length(iris_test_category)){
#   #print(paste("actual: ",iris_test_category[i], ", pred:",prc_test_pred[i]))
#   predicted_labels = append(predicted_labels, prc_test_pred[i])
#   predicted_errors = append(predicted_errors, abs(prob_point_class[i]-1))
#   #print("   ")
# }
# 
# t <- data.frame(
#   predicted_labels,
#   predicted_errors,
#   stringsAsFactors = FALSE
# )
# 
# counts = length(predicted_labels)
# 
# # assigned probability to belonging to each class
# #Black holes
# if (length(subset(predicted_labels,t[,1]==1))!=0){
#   blackhole_prob = length(subset(predicted_labels,t[,1]==1))/counts
#   bh_se = sum(subset(predicted_errors,t[,1]==1))/counts
# } else{ 
#   blackhole_prob = 0.0
#   bh_se = 0.0}
# 
# #Non-pulsing NS
# if (length(subset(predicted_labels,t[,1]==2))!=0){
#   non_pulsar_prob = length(subset(predicted_labels,t[,1]==2))/counts
#   np_se = sum(subset(predicted_errors,t[,1]==2))/counts
# } else{ 
#   non_pulsar_prob = 0.0
#   np_se =0.0}
# 
# #Pulsars
# if (length(subset(predicted_labels,t[,1]==3))!=0){
#   pulsar_prob = length(subset(predicted_labels,t[,1]==3))/counts
#   p_se = sum(subset(predicted_errors,t[,1]==3))/counts
# } else{ 
#   pulsar_prob = 0.0
#   p_se = 0.0}

# Save individual output file for each source
predictions <- data.frame(test_systems,blackhole_prob,bh_se,non_pulsar_prob,np_se,pulsar_prob,p_se)
output_filename_asc = paste("Predictions/", test_systems,'.asc', sep="")
if (file.exists(output_filename_asc)){
  output_filename_asc = paste("Predictions/", test_systems,runif(1),'.asc', sep="")
}
write.table(predictions, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

##create confusion matrix
tab <- table(prc_test_pred,iris_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#CrossTable(x =iris_test_category, y = prc_test_pred, prop.chisq = FALSE)

#install.packages('rgl')
#library(rgl)
#plot3d(test_fulldata[,5], test_fulldata[,6], test_fulldata[,7], col = "blue", size = 2)
#surface3d(unique(newdata$age), unique(newdata$lwt), newdata$yhat, 
#          alpha = 0.3)

