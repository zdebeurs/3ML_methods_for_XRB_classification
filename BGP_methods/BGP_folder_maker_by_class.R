# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract

# This script takes the prediction files from the supercomputer and sorts them into the appropriate folder based on XRB class

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")
folder = "BGP_methods/Predictions"
pred_folders = list.files(folder)#"Jul1_resampled_5_runs")

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


