# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract

# This script computes the median and SD across the predictions of the 10 subsampled datasets.

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")
directory = "SVM_methods/Predictions/"
pred_folders = list.files(directory)

# Aggregate the predictions across ten subsample datasets into one list 'prediction_list'
categories = c("BH/", "NS/", "Pulsar/")
prediction_list <- {}
# run through each of the subsample predictions folders D1-D10
for (pred_folder in pred_folders){
  pred_folder
  # run through each of the compact object types (BH, NS, Pulsar)
  for (compact_obj_folder in categories){
    print(pred_folder)
    prediction_folder = paste(directory,pred_folder, sep="")
    category = compact_obj_folder
    file_list <- list.files(paste(prediction_folder,"/",category, sep=""))
    fulldata <- {}
    for(i in 1:length(file_list)){
      dummy <- read.table(paste(prediction_folder,"/",category, file_list[i], sep=""), skip = 1)
      fulldata <- rbind(fulldata,dummy)
    }
    names(fulldata) <- c("systems","blackhole_prob","non_pulsar_prob","pulsar_prob")
    predictions = fulldata
    predictions
    # append predictions to prediction_list
    prediction_list <- rbind(prediction_list,predictions)
  }
}

## Read in reference file that provides the XRB sources in the correct RA-dec order
# This order will allow you to format the data for tables in latex 
ref_order_dat = read.csv(file = 'ref_order.csv')

# Create data frame to store the predictions in so that they can be sorted in RA-dec order
pred.data <- data.frame(
  # reference order
  ref_order = rep(ref_order_dat[,1],10),
  # XRB system names from ref_order.csv
  sysname= rep(ref_order_dat[,2],10),
  
  # XRB system names from predictions
  system = prediction_list[,1],
  blackhole_prob = prediction_list[,2],
  non_pulsar_prob = prediction_list[,3],
  pulsar_prob = prediction_list[,4],
  stringsAsFactors = FALSE
)

# Order the observations based on ref_order.csv
ordered_pred.data = pred.data[order(pred.data$ref_order),]

source_type_list = c(rep("BH", 12),rep("NS", 20), rep("Pulsar", 12))

# Now let us FINALLY compute the median and sd across all the predictions
fulldata <- {}
for (index in 1:44){
  print(ordered_pred.data[ordered_pred.data$ref_order==index,])
  
  med_bh_prob = median(ordered_pred.data[ordered_pred.data$ref_order==index,'blackhole_prob'])
  sd_bh_prob = sd(ordered_pred.data[ordered_pred.data$ref_order==index,'blackhole_prob'])
  med_ns_prob = median(ordered_pred.data[ordered_pred.data$ref_order==index,'non_pulsar_prob'])
  sd_ns_prob = sd(ordered_pred.data[ordered_pred.data$ref_order==index,'non_pulsar_prob'])
  med_ps_prob = median(ordered_pred.data[ordered_pred.data$ref_order==index,'pulsar_prob'])
  sd_ps_prob = sd(ordered_pred.data[ordered_pred.data$ref_order==index,'pulsar_prob'])
  source_type = source_type_list[index]
  
  dummy <- data.frame(
    system = ordered_pred.data[ordered_pred.data$ref_order==index,'system'][1],
    sysname= ordered_pred.data[ordered_pred.data$ref_order==index,'sysname'][1],
    ref_order = ordered_pred.data[ordered_pred.data$ref_order==index,'ref_order'][1],
    
    blackhole_prob = med_bh_prob,
    bh_se =sd_bh_prob,
    
    non_pulsar_prob = med_ns_prob,
    np_se = sd_ns_prob,
    
    pulsar_prob = med_ps_prob,
    p_se = sd_ps_prob,
    source_type = source_type,
    stringsAsFactors = FALSE
  )
  dummy
  
  fulldata <- rbind(fulldata,dummy)
}
fulldata

# Assign names to each of the columns
names(fulldata) <- c("systems","sysname","ref_order","blackhole_prob","bh_se","non_pulsar_prob","np_se","pulsar_prob","p_se", "source_type")
fulldata

# choose a filename for writing these median and sd results to a single csv
todays_date = Sys.Date()

# write median_sd results to a file 
write.csv(fulldata, paste("SVM_methods/10_runs_med_sd_SVM_",todays_date,".csv", sep =""))
