# divide the sources into the correct NS, BH, Pulsar folders
setwd("~/Documents/Summer19/SAO_Research/SVM_methods")
directory = "Jul1_resampled_10_runs/"
pred_folders = list.files(directory)

# Compute SD across ten examples
categories = c("BH/", "NS/", "Pulsar/")
prediction_list <- {}
#pred_folder = pred_folders[1]
for (pred_folder in pred_folders){
  # remove this line later
  #pred_folder = pred_folders[1]
  pred_folder
  for (compact_obj_folder in categories){
    # remove this line later
    #compact_obj_folder = "BH/"
    print(pred_folder)
    prediction_folder = paste(directory,pred_folder, sep="")
    category = compact_obj_folder
    file_list <- list.files(paste(prediction_folder,"/",category, sep=""))
    fulldata <- {}
    for(i in 1:length(file_list)){
      dummy <- read.table(paste(prediction_folder,"/",category, file_list[i], sep=""), skip = 1)
      fulldata <- rbind(fulldata,dummy)
    }
    names(fulldata) <- c("systems","blackhole_prob","bh_se","non_pulsar_prob","np_se","pulsar_prob","p_se")
    #names(fulldata) <- c("systems","blackhole_prob","bh_se","non_pulsar_prob","np_se",
    #                     "pulsar_prob","p_se","burster_prob", "burster_se")
    predictions = fulldata
    predictions
    prediction_list <- rbind(prediction_list,predictions)
  }
}
#write.csv(prediction_list, "5_runs_BGP_Jul1.csv")

## format the data for tables in latex
ref_order_dat = read.csv(file = '../ref_order.csv')

# Create the data frame
pred.data <- data.frame(
  #emp_name = c("MJD","2-3 keV","2-3 keV error","3-5 keV","3-5 keV error", "5-12 keV", "5-12 keV error"),
  ref_order = rep(ref_order_dat[,1],10),
  sysname= rep(ref_order_dat[,2],10),
  
  system = prediction_list[,1],
  
  blackhole_prob = prediction_list[,2],
  bh_se = prediction_list[,3],
  
  non_pulsar_prob = prediction_list[,4],
  np_se =  prediction_list[,5],
  
  pulsar_prob = prediction_list[,6],
  stringsAsFactors = FALSE
)

ordered_pred.data = pred.data[order(pred.data$ref_order),]

fulldata <- {}
for (index in 1:44){
  # REMOVE THIS LINE
  #index=2
  
  print(ordered_pred.data[ordered_pred.data$ref_order==index,])
  
  med_bh_prob = median(ordered_pred.data[ordered_pred.data$ref_order==index,'blackhole_prob'])
  sd_bh_prob = sd(ordered_pred.data[ordered_pred.data$ref_order==index,'blackhole_prob'])
  med_ns_prob = median(ordered_pred.data[ordered_pred.data$ref_order==index,'non_pulsar_prob'])
  sd_ns_prob = sd(ordered_pred.data[ordered_pred.data$ref_order==index,'non_pulsar_prob'])
  med_ps_prob = median(ordered_pred.data[ordered_pred.data$ref_order==index,'pulsar_prob'])
  sd_ps_prob = sd(ordered_pred.data[ordered_pred.data$ref_order==index,'pulsar_prob'])
  
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
    stringsAsFactors = FALSE
  )
  dummy
  
  fulldata <- rbind(fulldata,dummy)
}
fulldata
names(fulldata) <- c("systems","sysname","ref_order","blackhole_prob","bh_se","non_pulsar_prob","np_se","pulsar_prob","p_se")
fulldata

# write median_sd results to a file 
write.csv(fulldata, "10_runs_med_sd_SVM_Jul1.csv")
