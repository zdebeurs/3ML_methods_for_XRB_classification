# Zoe de Beurs (working with Saku Vrtilek and Nazma Islam)

#SVM algorithm, plots the results

library(ggplot2)
library(stringr)
library(RColorBrewer)

setwd("~/Documents/Summer19/SAO_Research/SVM_methods")
# Takes all predictions from k-fold cross_validation and plots them

# input csv that has averages and errors
ten_runs_data = read.csv(file = '10_runs_med_sd_SVM_Jul1_v2.csv')
ten_runs_data

# subset of bhs
bh_ten_runs_data = subset(ten_runs_data, ten_runs_data[,10]=="BH")
# subset of ns
ns_ten_runs_data = subset(ten_runs_data, ten_runs_data[,10]=="NS")
# subset of ps
ps_ten_runs_data = subset(ten_runs_data, ten_runs_data[,10]=="Pulsar")

# Choose which predictions to plot
predictions = ns_ten_runs_data[11:20,]#ns_ten_runs_data[1:11,] #ps_ten_runs_data #bh_ten_runs_data
predictions
names(predictions) <- c("ref_order","systems","blackhole_prob","bh_se",
                        "non_pulsar_prob","np_se",
                        "pulsar_prob","p_se", "pred",
                        "source_type", "correct")

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

Probability=c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$pulsar_prob)
#Probability=rep(c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$pulsar_prob), number_of_systems)
#error = rep(c(predictions$bh_se, predictions$np_se, predictions$p_se), number_of_systems)
error = c(predictions$bh_se, predictions$np_se,predictions$p_se)
error
data=data.frame(Source,Class,Probability,error)
data

#write.csv(data,'SVM_pred_final.csv')

# Remove negative errors and replace with 0.0
min_error = Probability-error
for (i in seq(1, length(min_error))){
  if (min_error[i] < 0.0){
    print(min_error[i])
    min_error[i]=0.0
    print(min_error[i])
  }
}

# Remove errors greater than 1.0 and replace with 1.0
max_error = Probability+error
#for (i in seq(1, length(min_error))){
#  if (max_error[i] > 1.0){
#    print(max_error[i])
#    max_error[i]=1.0
#    print(max_error[i])
#  }
#}

# Grouped
ggplot(data, aes(fill=Class, y=Probability, x=Source)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_cartesian(xlim = NULL, ylim = c(0.0,1.)) +
  geom_errorbar(aes(ymin=min_error, ymax=max_error), width=.1,
                position=position_dodge(.9)) + 
  scale_fill_manual(values = c("purple", "orange", "mediumseagreen"), )+
  labs(y = "SVM Probability")+
  theme(legend.position="top")


# old code below ______________ (prior to January, 2021)
#for (rand_var_x in c(0)){
pred_folders = list.files("Dec21_repeat_10_runs")
pred_folder = pred_folders[1]
print(pred_folder)
prediction_folder = paste("Dec21_repeat_10_runs/",pred_folder, sep="")
#prediction_folder = "PredictionsRBF_C=-0.728578078560531Y=0.276670657098293"
category1 = "BH/"
category2 = "NS/"
category3 = "Pulsar/"
category = category1
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

Probability=c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$pulsar_prob)
#Probability=rep(c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$pulsar_prob), number_of_systems)
#error = rep(c(predictions$bh_se, predictions$np_se, predictions$p_se), number_of_systems)
error = c(predictions$bh_se, predictions$np_se,predictions$p_se)
data=data.frame(Source,Class,Probability,error)
data

#write.csv(data,'SVM_pred_final.csv')

# Remove negative errors and replace with 0.0
min_error = Probability-error
for (i in seq(1, length(min_error))){
  if (min_error[i] < 0.0){
    print(min_error[i])
    min_error[i]=0.0
    print(min_error[i])
  }
}

# Remove errors greater than 1.0 and replace with 1.0
max_error = Probability+error
for (i in seq(1, length(min_error))){
  if (max_error[i] > 1.0){
    print(max_error[i])
    max_error[i]=1.0
    print(max_error[i])
  }
}

# Grouped
ggplot(data, aes(fill=Class, y=Probability, x=Source)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_cartesian(xlim = NULL, ylim = c(0.0,1.)) +
  #geom_errorbar(aes(ymin=min_error, ymax=max_error), width=.1,
  #              position=position_dodge(.9)) + 
  scale_fill_manual(values = c("purple", "orange", "mediumseagreen"), )+
  labs(y = "SVM Probability")+
  theme(legend.position="top")

# write to csv correctly for latex tables
names(fulldata) <- c("systems","Prob(BH)","SD","Prob(NS)","SD","Prob(Pulsar)","SD")
#names(fulldata) <- c("systems","blackhole_prob","bh_se","non_pulsar_prob","np_se",
#                     "pulsar_prob","p_se","burster_prob", "burster_se")
predictions = fulldata
predictions

write.csv(predictions,'SVM_pred_x_RBF_final.csv')

# Compute SD across ten examples
categories = c("BH/", "NS/", "Pulsar/")
prediction_list <- {}
pred_folders = list.files("Dec21_repeat_10_runs")
#pred_folder = pred_folders[1]
for (pred_folder in pred_folders){
  # remove this line later
  #pred_folder = pred_folders[1]
  #pred_folder
  for (compact_obj_folder in categories){
    print(pred_folder)
    prediction_folder = paste("Dec21_repeat_10_runs/",pred_folder, sep="")
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
write.csv(prediction_list, "10_runs_SVM_Dec21.csv")

# simplified csv with number of correct classifications and hyperparameters for cross-val
# Check how many correct classifications
# Loop through all three folders (BH, NS, Pulsar)
categories = c("BH/", "NS/", "Pulsar/")
simp_full_data_list <- {}
pred_folders = list.files("Dec20b_preds")
#pred_folder = pred_folders[1]
for (pred_folder in pred_folders){
  # remove this line later
  #pred_folder = pred_folders[4]
  #pred_folder
  for (compact_obj_folder in categories){
    print(pred_folder)
    prediction_folder = paste("Dec20b_preds/",pred_folder, sep="")
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
      print(paste(ns_correct_preds, " out of ", length(predictions[,2]), " BHs are correctly classified", sep=""))
    }
    # for Pulsar
    if (category == category3){
      z = predictions[,6]>predictions[,2] & predictions[,6]>predictions[,4]
      pulsar_correct_preds = sum(z, na.rm = TRUE)
      print(paste(pulsar_correct_preds, " out of ", length(predictions[,2]), " BHs are correctly classified", sep=""))
    }
  }
  # extract kernel and hyperparameters
  a<-paste(pred_folder,"ext", sep="")
  res <- str_match(a, "Predictions_Dec_20_\\s*(.*?)\\s*_C")
  kernel = res[,2]
  kernel
  res <- str_match(a, "RBF_C=\\s*(.*?)\\s*Y=")
  Cost_value = res[,2]
  Cost_value
  res <- str_match(a, "Y=\\s*(.*?)\\s*ext")
  Y_value = res[,2]
  Y_value
  
  simp_full_data = c(kernel,Cost_value, Y_value, 
                     paste(bh_correct_preds,"/11", sep=""),
                     paste(ns_correct_preds,"/21", sep=""), 
                     paste(pulsar_correct_preds,"/12", sep=""), 
                     paste(bh_correct_preds+ns_correct_preds+pulsar_correct_preds,"/44", sep=""), 
                     paste((bh_correct_preds+ns_correct_preds+pulsar_correct_preds)/44, sep=""))
  names(simp_full_data) = c("kernel","C","Y", "BH_acc", "NS_acc", "Pulsar_acc", "Total_acc")
  simp_full_data_list <- rbind(simp_full_data_list,simp_full_data)
}

simp_full_data_list
write.csv(simp_full_data_list,'SVM_Dec21_cross_val_simplified_results.csv')

results.data <- data.frame(
  Cost = as.numeric(simp_full_data_list[,2]),
  Gamma = as.numeric(simp_full_data_list[,3]),
  Classification_Accuracy = as.numeric(simp_full_data_list[,8]),
  stringsAsFactors = FALSE
)

library(scales)
mid <- median(results.data$Classification_Accuracy)
sp<-ggplot(results.data, aes(x = Cost, y = Gamma, color=Classification_Accuracy)) +
  geom_point() +scale_colour_gradient2(midpoint = mid, low = "red", mid = "white", high = "blue")+ #scale_colour_gradient(low = "yellow", high = "purple", na.value = NA)+
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2",function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  scale_x_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))

sp




#scale_fill_manual(values = c("purple", "blue", "orange", "mediumseagreen"), )+
