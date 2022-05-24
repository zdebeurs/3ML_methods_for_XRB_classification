# Zoe de Beurs (working with Saku Vrtilek and Nazma Islam)

#K nearest neighbors algorithm, plots the results

library(ggplot2)
library(stringr)

# divide the sources into the correct NS, BH, Pulsar folders
setwd("~/Documents/Summer19/SAO_Research/simple_BGP")
pred_folders = list.files("files_from_tacc_jan_2021")

for (pred_folder_a in pred_folders){
  # Put files in appropriate folders (BH, NS, Bursters, Pulsar)
  # BHs
  #i = 1
  pred_folder_a = "Dec3_NP20_BGP_Predictions_kernel=rbfdot"
  pred_folder = paste("files_from_tacc_jan_2021/",pred_folder_a, sep="")
  print(pred_folder)
  dir.create(paste(pred_folder,"/BH",sep=""))
  BH_file_list <- list.files("~/Documents/Summer19/SAO_Research/simple_BGP/files_from_tacc_jan_2021/Dec3_NP20_BGP_Predictions_kernel=laplacedot/BH")
  BH_file_list
  file.copy(paste(pred_folder,"/", BH_file_list, sep=""),paste(pred_folder,"/BH/", BH_file_list, sep=""))
  file.remove(paste(pred_folder,"/", BH_file_list, sep=""))
  
  # NSs
  dir.create(paste(pred_folder,"/NS",sep=""))
  NS_file_list <- list.files("~/Documents/Summer19/SAO_Research/simple_BGP/files_from_tacc_jan_2021/Dec3_NP20_BGP_Predictions_kernel=laplacedot/NS")
  NS_file_list
  file.copy(paste(pred_folder,"/", NS_file_list, sep=""),paste(pred_folder,"/NS/", NS_file_list, sep=""))
  file.remove(paste(pred_folder,"/", NS_file_list, sep=""))
  
  # Pulsars
  dir.create(paste(pred_folder,"/Pulsar",sep=""))
  Pulsar_file_list <- list.files("~/Documents/Summer19/SAO_Research/simple_BGP/files_from_tacc_jan_2021/Dec3_NP20_BGP_Predictions_kernel=laplacedot/Pulsar")
  Pulsar_file_list
  file.copy(paste(pred_folder,"/", Pulsar_file_list, sep=""),paste(pred_folder,"/Pulsar/", Pulsar_file_list, sep=""))
  file.remove(paste(pred_folder,"/", Pulsar_file_list, sep=""))
}


setwd("~/Documents/Summer19/SAO_Research/simple_BGP/files_from_tacc_jan_2021/")
# Takes all predictions from k-fold cross_validation and plots them
folders = "Feb11_preds_4U1916/"
pred_folders = list.files(folders)
folder = pred_folders[1]
category1 = "BH/"
category2 = "NS/"
category3 = "Pulsar/"
category = category1
file_list <- list.files(paste(folders,folder,"/",category, sep=""))
fulldata <- {}
for(i in 1:length(file_list)){
  dummy <- read.table(paste(folders,folder,"/",category, file_list[i], sep=""), skip = 1)
  fulldata <- rbind(fulldata,dummy)
}
names(fulldata) <- c("systems","blackhole_prob","bh_se","non_pulsar_prob","np_se","pulsar_prob","p_se")
#names(fulldata) <- c("systems","blackhole_prob","bh_se","non_pulsar_prob","np_se",
#                     "pulsar_prob","p_se","burster_prob", "burster_se")
predictions = fulldata
predictions


number_of_systems = length(predictions$systems)
Source = c()
for (i in number_of_systems){
  name = rep(i, 3)
  Source = append(Source, name)
}

Source = c()
Source = rep(predictions$systems, 3)
Class = c()
Class=rep(c("Black Hole"), number_of_systems)
Class = append(Class,rep(c("Non-pulsing Neutron Star"),number_of_systems))
Class = append(Class, rep(c("Pulsar"), number_of_systems))
#Class = append(Class, rep(c("Burster"), number_of_systems))

Probability=c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$burster_prob, predictions$pulsar_prob)
#Probability=rep(c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$pulsar_prob), number_of_systems)
#error = rep(c(predictions$bh_se, predictions$np_se, predictions$p_se), number_of_systems)
error = c(predictions$bh_se, predictions$np_se, predictions$burster_se, predictions$p_se)
data=data.frame(Source,Class,Probability,error)
data

#write.csv(data,'BGP_pred_jan13.csv')

# Grouped
ggplot(data, aes(fill=Class, y=Probability, x=Source)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_cartesian(xlim = NULL, ylim = c(0.0,1.)) +
  #ylim(-0.001,1.1)+
  #geom_errorbar(aes(ymin=Probability-error, ymax=Probability+error), width=.1,
  #              position=position_dodge(.9)) + 
  scale_fill_manual(values = c("purple", "orange", "mediumseagreen"), )+
  labs(y = "BGP Probability")+
  theme(legend.position="top")


# simplified csv with number of correct classifications and hyperparameters for cross-val
# Check how many correct classifications
# Loop through all three folders (BH, NS, Pulsar)
setwd("~/Documents/Summer19/SAO_Research/simple_BGP")
categories = c("BH/", "NS/", "Pulsar/")
simp_full_data_list <- {}
pred_folders = list.files("Feb11_preds_4U1916/")#"files_from_tacc_jan_2021")
pred_folder = pred_folders[1]
pred_folder
for (pred_folder in pred_folders){
  for (compact_obj_folder in categories){
    print(pred_folder)
    prediction_folder = paste("Feb11_preds_4U1916/",pred_folder, sep="")
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
    if (category == category1){
      z = predictions[,2]>predictions[,4] & predictions[,2]>predictions[,6]
      bh_correct_preds = sum(z, na.rm = TRUE)
      print(paste(bh_correct_preds, " out of ", length(predictions[,2]), " BHs are correctly classified", sep=""))
    }
    # for NS
    if (category == category2){
      z = predictions[,4]>predictions[,2] & predictions[,4]>predictions[,6]
      ns_correct_preds = sum(z, na.rm = TRUE)
      print(paste(ns_correct_preds, " out of ", length(predictions[,2]), " NSs are correctly classified", sep=""))
    }
    # for Pulsar
    if (category == category3){
      z = predictions[,6]>predictions[,2] & predictions[,6]>predictions[,4]
      pulsar_correct_preds = sum(z, na.rm = TRUE)
      print(paste(pulsar_correct_preds, " out of ", length(predictions[,2]), " Pulsars are correctly classified", sep=""))
    }
  }
  # extract kernel and hyperparameters
  a<-paste("t",pred_folder,"ext", sep="")
  kernel_type <- str_match(a, "t\\s*(.*?)\\s*cutoff")#"kernel=\\s*(.*?)\\s*ext")
  kernel_type = kernel_type[,2]
  kernel_type
  
  simp_full_data = c(kernel_type,
                     paste(bh_correct_preds,"/12", sep=""),
                     paste(ns_correct_preds,"/20", sep=""), 
                     paste(pulsar_correct_preds,"/12", sep=""), 
                     paste(bh_correct_preds+ns_correct_preds+pulsar_correct_preds,"/44", sep=""))
  names(simp_full_data) = c("cutoff","BH_acc", "NS_acc", "Pulsar_acc", "Total_acc")
  simp_full_data_list <- rbind(simp_full_data_list,simp_full_data)
}

simp_full_data_list
#write.csv(simp_full_data_list,'BGP_cross_val_simplified_results_fulldata.csv')

# write to csv correctly for latex tables
names(fulldata) <- c("systems","Prob(BH)","SD","Prob(NS)","SD","Prob(Pulsar)","SD")
#names(fulldata) <- c("systems","blackhole_prob","bh_se","non_pulsar_prob","np_se",
#                     "pulsar_prob","p_se","burster_prob", "burster_se")
predictions = fulldata
predictions

write.csv(predictions,'BGP_20_pred_laplace_final.csv')
