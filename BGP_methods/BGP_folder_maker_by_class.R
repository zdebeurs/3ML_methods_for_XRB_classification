# divide the sources into the correct NS, BH, Pulsar folders
setwd("Documents/Summer19/SAO_Research/simple_BGP/mit_supercloud_feb10_2022/")
folder = "Feb11_preds_4U1916/"
pred_folders = list.files(folder)#"Jul1_resampled_5_runs")

for (pred_folder_a in pred_folders){
  # Put files in appropriate folders (BH, NS, Bursters, Pulsar)
  # BHs
  #i = 1
  #pred_folder_a = pred_folders[i]
  pred_folder = paste(folder,pred_folder_a, sep="")
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



# Compute SD across ten examples
categories = c("BH/", "NS/", "Pulsar/")
prediction_list <- {}
pred_folders = list.files("files_from_tacc_march_2021")
#pred_folder = pred_folders[1]
for (pred_folder in pred_folders){
  # remove this line later
  #pred_folder = pred_folders[1]
  #pred_folder
  for (compact_obj_folder in categories){
    print(pred_folder)
    prediction_folder = paste("files_from_tacc_march_2021/",pred_folder, sep="")
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
write.csv(prediction_list, "5_runs_BGP_Dec21.csv")
