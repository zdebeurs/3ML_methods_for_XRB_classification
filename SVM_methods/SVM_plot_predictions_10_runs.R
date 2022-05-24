# Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). 
# If you use any part of this code, please cite our paper: 
# de Beurs, Z. L., Islam, N., Gopalan, G., & Vrtilek, S.D. (2020). A Comparative Study of Machine Learning Methods for X-ray Binary Classification. Accepted to the Astrophysical Journal. 
# https://ui.adsabs.harvard.edu/abs/2022arXiv220400346D/abstract

# This script takes in the median predictions and corresponding sd and plots the results.

# Be sure to install "ggplot2" if not already installed with "install.packages("ggplot2")"
library(ggplot2)

# Set the working directory (Adjust this path based on where you download your files )
setwd("~/Documents/Github/3ML_methods_for_XRB_classification/3ML_methods_for_XRB_classification")

# input csv that has median preds and sds (CHANGE THIS TO THE FILENAME OF YOUR FILE!)
ten_runs_data = read.csv(file = 'SVM_methods/10_runs_med_sd_SVM_2022-05-24.csv')
ten_runs_data

# subset of bhs
bh_ten_runs_data = subset(ten_runs_data, ten_runs_data[,11]=="BH")
# subset of ns
ns_ten_runs_data = subset(ten_runs_data, ten_runs_data[,11]=="NS")
# subset of ps
ps_ten_runs_data = subset(ten_runs_data, ten_runs_data[,11]=="Pulsar")

# Choose which predictions to plot.
# for BHs (Figure 10c from paper), set predictions = bh_ten_runs_data
# for NPNS (Figure 11c from paper), set predictions =  ns_ten_runs_data[11:20,] or predictions = ns_ten_runs_data[1:11,]
# for Pulsars (Figure 12c from paper), set predictions = ps_ten_runs_data
predictions = bh_ten_runs_data
predictions
names(predictions) <- c("x","systems","ref_order","system","blackhole_prob","bh_se",
                        "non_pulsar_prob","np_se",
                        "pulsar_prob","p_se",
                        "source_type")

# Read in the data in a way that ggplot2 can easily plot it.
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
Probability=c(predictions$blackhole_prob, predictions$non_pulsar_prob, predictions$pulsar_prob)
error = c(predictions$bh_se, predictions$np_se,predictions$p_se)
error
data=data.frame(Source,Class,Probability,error)
data


# Compute error bars
min_error = Probability-error
max_error = Probability+error

# Produce plot
plot_z <- ggplot(data, aes(fill=Class, y=Probability, x=Source)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_cartesian(xlim = NULL, ylim = c(0.0,1.)) +
  geom_errorbar(aes(ymin=min_error, ymax=max_error), width=.1,
                position=position_dodge(.9)) + 
  scale_fill_manual(values = c("purple", "orange", "mediumseagreen"), )+
  labs(y = "SVM Probability")+
  theme(legend.position="top")
plot_z

# Save plot as png
ggsave("SVM_methods/plot_z.png", width=12, height=4)


