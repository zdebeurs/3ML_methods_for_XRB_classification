# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract
# Script aims to take a sample such that each source has between 100 to 300 pts per sample. This is further described in Section 2.2 of the paper. 

# Be sure to install "ggplot2" if not already installed with "install.packages("ggplot2")"
library(ggplot2)

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")

# Name of folder which contains XRB sources 
training_files = "Training_and_Testing"

# Name of output folder for 10 sampled datasets and create folder (if it does not already exist)
output_folder = "10_sampled20Perc_Datasets"
dir.create(output_folder, showWarnings = FALSE)

# create folder for boxplot data to be used to generate some plots in the script Subsmapling_plotting.R
boxplot_folder = "10_boxplot_sampled20Perc_Datasets"
dir.create(boxplot_folder, showWarnings = FALSE)

for (sub_sam_num in 1:10){
  # Set a sub folder within the output folder for the sampled (20%) observations
  output_folder_sub = paste(output_folder,"/D",sub_sam_num,"_Sampled_20percent_Training_and_Testing/", sep="")
  dir.create(output_folder_sub, showWarnings = FALSE)
  
  # List the files in training and test set folder
  file_list <- list.files(training_files)
  test_systems = strsplit(file_list, ".asc")
  test_systems = unlist(test_systems)
  
  # Print the XRB systems
  test_systems
  
  # Create an empty list to store the data
  fulldata <- {}
  # Store the pre-sampled length of each XRB system
  orig_system_length <- {}
  
  # Loop through each of the XRB systems
  for(i in 1:length(test_systems)){
    # create a dummy variable to read in the systems (skip the first row which includes header info)
    dummy <- read.table(paste(training_files,"/",test_systems[i], ".asc",sep=""), skip = 1)
    
    # print the name of each test system and the number of observations
    print(paste(test_systems[i],": ",length(dummy[,1]), sep=""))
    
    # Store the original length of each system (before sampling)
    orig_system_length <- rbind(orig_system_length, length(dummy[,1]))
    
    # Store the data
    fulldata <- rbind(fulldata,dummy)
  }
  
  #Add a column for star type in numbers (1 = BH,2 = Non-pulsing NS,3 = Pulsar) for each type of source 
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
  
  # Add a column for star type written out ("BH","NS","Pulsar")
  compact_s <- rep("BH",dim(fulldata)[1])
  hmbh <- which(fulldata[,2] == 'HMBH')
  lmbh <- which(fulldata[,2] == 'LMBH')
  bh <- c(hmbh,lmbh)
  atoll <- which(fulldata[,2] == 'Atoll')
  zsource <- which(fulldata[,2] == 'Zsource')
  bursters_as_np <- which(fulldata[,2] == 'Burster') 
  pulsars <- which(fulldata[,2] == 'Pulsar')
  nonpulsar <- c(atoll,zsource,bursters_as_np)
  compact_s[nonpulsar] <- rep("NS",length(nonpulsar))
  compact_s[pulsars] <- rep("Pulsar",length(pulsars))
  fulldata <- cbind(compact,compact_s,fulldata)
  
  # redefine the column labels (CC1 = Soft Colors, CC2 = Hard Colors, CC3 = Relative Intensity)
  names(fulldata) <- c("Compact_Star","Star_Type","System","Type","Date","CC1","CC2","CC3")
  
  # Set an upper bound for number of data points to include in a training data set. This is set to 20%, but can be changed.
  # The KNN and SVM methods can handle a larger % of the observations to be included, but the BGP computation scales with O(N^3) where N is the number of observations.
  data_size <- .2*dim(fulldata)[1]
  
  #Determine vector of probabilities to sample from, which are inversely proportional to the XRB system's number of observations
  systems <- levels(fulldata$System)
  probs <- sapply(systems, function(x) 1/length(which(fulldata$System == x)))
  
  # Scale the probabilities by the sum of all probabilities and compute the sampling probabilities
  probs <- probs/sum(probs)
  sampling_probs <- sapply(seq(dim(fulldata)[1]), function(x) probs[fulldata[x,3]])
  
  
  # Sample each data point with the corresponding probability
  samples <- sample(dim(fulldata)[1],size=data_size,replace=FALSE,prob=sampling_probs)
  training <- fulldata[samples,]
  
  # Create a data frame including only  the sampled observations
  sampled.data <- data.frame(
    Classification = training["Compact_Star"],
    System = training["System"],
    Star_Type = training["Star_Type"],
    Type = training["Type"],
    Date = training["Date"],
    CC1 = training["CC1"], # <--- This is the Soft Colors
    CC2 = training["CC2"], # <--- This is the Hard Colors
    CC3 = training["CC3"],  # <--- This is the Relative Intensity
    stringsAsFactors = FALSE
  )
  
  # Define a variable for the list of 1s, 2s, 3s that correspond to the classifications of the XRBs
  class_box_list = training["Compact_Star"][,1]
  
  classification_plot_list = c()
  for(element in 1:length(class_box_list)){
    # classes for boxplot
    if (class_box_list[element] == "1" ){
      classification_plot_list = append(classification_plot_list, "BH")
    }
    if (class_box_list[element] == "2"){
      classification_plot_list = append(classification_plot_list, "NS")
    }
    if (class_box_list[element] == "3"){
      classification_plot_list = append(classification_plot_list, "Pulsar")
    }
  }
  
  boxplot.data <- data.frame(
    classification = classification_plot_list,
    relative_intensity = training["CC1"][,1],
    soft_colors = training["CC2"][,1],
    hard_colors= training["CC3"][,1],
    stringsAsFactors = FALSE
  )
  
  
  # write the resulting subsampled data to a csv 
  write.csv(boxplot.data,paste(boxplot_folder,'/boxplot_data_before_sampling.csv', sep=""))
  
  
  # Check the number of total observations in sampled data
  length(sampled.data[,3])
  
  # create a list to store the number of observations you ended up with after sampling
  simp_full_data_list = {}
  num_of_pts = c()
  
  # Loop through individual test systems to generate individual files for each XRB source
  for (i in 1:length(test_systems)){
    # Create a subset of observations from only that specific system
    sub = subset(sampled.data, System==test_systems[i])
    
    # Define the name of the output file
    output_filename_asc = paste(output_folder_sub,
                                test_systems[i],'.asc', sep="")
    
    # Create the data frame 
    clean.data <- data.frame(
      source_name = test_systems[i],
      classification = sub["Type"],
      MJD = sub["Date"],
      soft_colors = sub["CC1"],
      hard_colors = sub["CC2"],
      relative_intensity = sub["CC3"],
      stringsAsFactors = FALSE
    )
    
    # write the resulting subsampled data to a csv (This can be used later to make Figure 4)
    write.csv(boxplot.data,paste(boxplot_folder,'/boxplot_data_after_sampling',sub_sam_num,'.csv', sep=""))
    
    # Print the name of the system an the number of points after sampling
    print(paste((test_systems[i]),", number of data pts: ", length(sub["CC1"][,1]), sep=""))
    
    # count the number of points and append to the list "num_of_pts"
    num_of_pts = append(num_of_pts, c(length(sub["CC1"][,1])))
    
    #WRITE NEW FILES
    write.table(clean.data, file=output_filename_asc, append = FALSE, sep = "    ", dec = ".",
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    
    # Record how many observations remained for each XRB systems after sampling
    simp_full_data = c(test_systems[i], orig_system_length[i], length(sub["CC1"][,1]))
    names(simp_full_data) = c("System","Sign. Points","Sampled points")
    simp_full_data_list <- rbind(simp_full_data_list,simp_full_data)
  }
  
  
  # Print the mean and sd of the number of points across XRB systems
  mean(num_of_pts)
  sd(num_of_pts)
}


# check how many points were include for a specificic subsample, for example sample 1 (This can be changed to subsample 2, 3, etc by changing "D1" to "D2", "D3", etc.)
# chose subsampled set to investigate
sub_num =1

file_loc_sampled =  paste(output_folder,"/D",sub_num,"_Sampled_20percent_Training_and_Testing", sep="")
sampled_system_length <- {}
simp_samp_full_data_list  <- {}

# Loop through each of the XRB systems
for(i in 1:length(test_systems)){
  # create a dummy variable to read in the systems
  dummy <- read.table(paste(file_loc_sampled,"/",test_systems[i], ".asc",sep=""), skip = 1)
  print(paste(test_systems[i],": ",length(dummy[,1]), sep=""))
  
  # Store the original length of each system (before sampling)
  sampled_system_length <- rbind(sampled_system_length, length(dummy[,1]))

  # Record how many observations remained for this XRB systems after sampling
  simp_samp_full_data = c(test_systems[i], orig_system_length[i], length(dummy[,1]))
  names(simp_samp_full_data) = c("System","Sign. Points","Sampled points")
  simp_samp_full_data_list <- rbind(simp_samp_full_data_list,simp_samp_full_data)
}

simp_samp_full_data_list

# write the resulting subsampled stats to a csv (this can be used to make Figures 5, A2)
write.csv(simp_samp_full_data_list,paste('number_of_sampled_pts',sub_num,'.csv', sep=""))

print("End of Sampling Script")
