# Zoe de Beurs (working with Saku Vrtilek and Nazma Islam)

#K nearest neighbors algorithm, plots the results

setwd("~/Documents/Summer19/SAO_Research/")

library(ggplot2)
library(stringr)

k_n = 30
a = TRUE

full_mean_full_data <- {}

# Choose whether to apply the uniform cutoff or a specific cutoff
cutoff_type = '0.4all' #'top20' #2sigma_all'#'0.4all'#_4u1916'#'2sigma'# '0.4' #'1sigma'#'by_eye_4U1916-053v2' #'by_eye'#'by_eye_4U1916-053' #'1sigma_median'# or '3sigma' or 'by_eye'
folder_preds = paste("KNN_methods/Dec13_preds_4U1916/all/",cutoff_type,"cutoff_preds", sep="")

if (a = TRUE){
  #for (category in c("BH", "NS")){
  setwd(folder_preds)#"~/Documents/Summer19/SAO_Research/KNN_methods/Jul1_resampled_10_runs/Predictions_Dec_21_k=24run1","", sep=""))#,k_n,"/", sep=""))
  # Takes all predictions from k-fold cross_validation and plots them

  category = "BH"
  file_list <- list.files(paste(category, "/", sep=""))
  fulldata <- {}
  for(i in 1:length(file_list)) {
    dummy <-
      read.table(paste(category, "/", file_list[i], sep = ""), skip = 1)
    fulldata <- rbind(fulldata, dummy)
  }
  names(fulldata) <-
    c("systems",
      "blackhole_prob",
      "bh_se",
      "non_pulsar_prob",
      "np_se",
      "pulsar_prob",
      "p_se")
  predictions = fulldata
  predictions
  
  number_of_systems = length(predictions$systems)
  Source = c()
  for (i in number_of_systems) {
    name = rep(i, 3)
    Source = append(Source, name)
  }
  Source = c()
  Source = rep(predictions$systems, 3)
  Class = c()
  Class = rep(c("Black Hole"), number_of_systems)
  Class = append(Class, rep(c("Non-pulsing Neutron Star"), number_of_systems))
  Class = append(Class, rep(c("Pulsar"), number_of_systems))
  Probability = c(predictions$blackhole_prob,
                  predictions$non_pulsar_prob,
                  predictions$pulsar_prob)
  #Probability=rep(c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$pulsar_prob), number_of_systems)
  #error = rep(c(predictions$bh_se, predictions$np_se, predictions$p_se), number_of_systems)
  error = c(predictions$bh_se, predictions$np_se, predictions$p_se)
  data = data.frame(Source, Class, Probability, error)
  data
  
  #setwd("~/Documents/Summer19/SAO_Research/KNN_methods/")
  #write.csv(data, 'KNN_pred_new_colors.csv')
  
  # Grouped
  ggplot(data, aes(fill = Class, y = Probability, x = Source)) +
    geom_bar(position = "dodge", stat = "identity") +
    coord_cartesian(xlim = NULL, ylim = c(0.0, 1.)) +
    #geom_errorbar(aes(ymin=Probability-error, ymax=Probability+error), width=.1,
    #              position=position_dodge(.9)) +
    scale_fill_manual(values = c("purple", "orange", "mediumseagreen"),) +
    labs(y = "KNN Probability") +
    theme(legend.position = "bottom")+
    ggtitle(cutoff_type)
  
  # print mean probabilities
  if (category =="BH"){
  bh_mean_bh = mean(predictions[,2])
  bh_mean_ns = mean(predictions[,4])}
  if (category =="NS"){
    ns_mean_bh = mean(predictions[,2])
    ns_mean_ns = mean(predictions[,4])}
}
  #mean_full_data = c(k_n,bh_mean_bh, ns_mean_ns, bh_mean_bh+ns_mean_ns)
  #names(mean_full_data) = c("K", "BH_mean_BH", "NS_mean_NS", "Sum")
  #full_mean_full_data <- rbind(full_mean_full_data, mean_full_data)

  #full_mean_full_data

# write to csv correctly for latex tables
names(fulldata) <- c("systems","Prob(BH)","SD","Prob(NS)","SD","Prob(Pulsar)","SD")
#names(fulldata) <- c("systems","blackhole_prob","bh_se","non_pulsar_prob","np_se",
#                     "pulsar_prob","p_se","burster_prob", "burster_se")
predictions = fulldata
predictions

# WRITE FILES
#write.csv(predictions,'Jul1_KNN_20_pred_RBF_final.csv')

# simplified csv with number of correct classifications and hyperparameters for cross-val
# Check how many correct classifications
# Loop through all three folders (BH, NS, Pulsar)
setwd("~/Documents/Summer19/SAO_Research/KNN_methods/")
categories = c("BH/", "NS/", "Pulsar/")
simp_full_data_list <- {}

folder = "Dec13_preds_4U1916/all/"
pred_folders = list.files(folder)#"n-fold cross_val preds_20")
for (pred_folder in pred_folders){
  for (compact_obj_folder in categories){
    print(pred_folder)
    prediction_folder = paste(folder,"/",pred_folder, sep="")
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
    
    # for BHs
    if (category == "BH/"){
      z = predictions[,2]>predictions[,4] & predictions[,2]>predictions[,6]
      bh_correct_preds = sum(z, na.rm = TRUE)
      print(paste(bh_correct_preds, " out of ", length(predictions[,2]), " BHs are correctly classified", sep=""))
    }
    # for NS
    if (category == "NS/"){
      z = predictions[,4]>predictions[,2] & predictions[,4]>predictions[,6]
      ns_correct_preds = sum(z, na.rm = TRUE)
      print(paste(ns_correct_preds, " out of ", length(predictions[,2]), " BHs are correctly classified", sep=""))
    }
    # for Pulsar
    if (category == "Pulsar/"){
      z = predictions[,6]>predictions[,2] & predictions[,6]>predictions[,4]
      pulsar_correct_preds = sum(z, na.rm = TRUE)
      print(paste(pulsar_correct_preds, " out of ", length(predictions[,2]), " BHs are correctly classified", sep=""))
    }
  }
  # extract kernel and hyperparameters
  a<-paste("ext",pred_folder,"ext", sep="")
  res <- str_match(a, "ext\\s*(.*?)\\s*cutoff_predsext")
  cutoff_type = res[,2]
  cutoff_type
  k_num = 24
  
  simp_full_data = c(k_num, 
                     cutoff_type,
                     paste(bh_correct_preds,"/12", sep=""),
                     paste(ns_correct_preds,"/20", sep=""), 
                     paste(pulsar_correct_preds,"/12", sep=""), 
                     paste(bh_correct_preds+ns_correct_preds+pulsar_correct_preds,"/44", sep=""), 
                     paste((bh_correct_preds+ns_correct_preds+pulsar_correct_preds)/44, sep=""))
  names(simp_full_data) = c("k_value","cutoff_type","BH_acc", "NS_acc", "Pulsar_acc", "Total_acc")
  simp_full_data_list <- rbind(simp_full_data_list,simp_full_data)
}

simp_full_data_list
#write.csv(simp_full_data_list,'KNN_jun_1_21_cross_val_simplified_results.csv')#'KNN_mar_11_21_cross_val_simplified_results.csv')



