# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract

# This script performs the plotting for the results of the Bayesian Gaussian Process Algorithm (Section 3.1)

# Be sure to install "ggplot2" if not already installed with "install.packages("ggplot2")"
library(ggplot2)

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")

# create an empty list
full_mean_full_data <- {}

# Choose the subsample number (D1-D10) to plot. Here we choose subsample D1, but this can be changed to D2, D3, etc.
subsample_num = 'D1'
folder_preds = paste("BGP_methods/Predictions/",subsample_num,"_Predictions/", sep="")
setwd(folder_preds)

# choose a filename for writing these results to a single csv
todays_date = Sys.Date()
csv_filename = paste(todays_date,"_",subsample_num,"_Predictions_BGP.csv", sep="")

# choose whether to plot the BH, NPNS, or Pulsar predictions
category = "BH"
file_list <- list.files(paste(category, "/", sep=""))
fulldata <- {}
for(i in 1:length(file_list)) {
  dummy <-read.table(paste(category, "/", file_list[i], sep = ""), skip = 1)
  fulldata <- rbind(fulldata, dummy)
}
names(fulldata) <-
  c("systems",
    "blackhole_prob",
    "non_pulsar_prob",
    "pulsar_prob")
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
data = data.frame(Source, Class, Probability)
data

# Plot results
ggplot(data, aes(fill = Class, y = Probability, x = Source)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(xlim = NULL, ylim = c(0.0, 1.)) +
  scale_fill_manual(values = c("purple", "orange", "mediumseagreen"),) +
  labs(y = "KNN Probability") +
  theme(legend.position = "bottom")

# write results to csv
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification/BGP_methods/")
names(fulldata) <- c("systems","Prob(BH)","Prob(NS)","Prob(Pulsar)")
predictions = fulldata
predictions
write.csv(predictions,csv_filename)
