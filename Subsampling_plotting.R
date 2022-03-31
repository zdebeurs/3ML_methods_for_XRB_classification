# Code written by Zoe de Beurs (working with Saku Vrtilek, Nazma Islam, and Giri Gopalan). Some of this code is adapted from https://github.com/ggopalan/XRay-Binary-Classification.
# Please cite our paper when using any part of this code. [INCLUDE CITATION LINK]

# This script produces the histograms before and after subsampling (Figure 5 and Figure A2 in the paper)

# Run code below to create histograms of the number of observations before and after subsampling


# Read in the csv that records the number of significant points and number of subsampled points of each XRB source
sub_num = 1
num_pts_data = read.csv(file =paste('number_of_sampled_pts',sub_num,'.csv', sep=""))
head(num_pts_data)

# Define variables for the number of points before and after subsampling
num_of_pts_before_sampling = num_pts_data$Sign..Points
num_of_pts_after_sampling = num_pts_data$Sampled.points


## _______________ Plot Histograms (Figures 5, 2A in paper)_______________
# Plot before sampling
par(mfrow=c(1,2))
barplot(num_of_pts_before_sampling, xlab="Sources", ylab="Number of observations", main="Before Subsampling")

minimum= min(num_of_pts_after_sampling)
maximum= max(num_of_pts_after_sampling)
# Plot After sampling
barplot(num_of_pts_after_sampling, xlab="Sources", ylab="Number of observations", main=paste("After Subsampling, Run: ", sub_num, sep=""))#, min:",minimum, "max:" , maximum))

## _______________ Plot 2D projections of CCI plots before and after subsampling_______________

# read in data before resampling for 2D projections
boxplot.data = read.csv(file ='boxplot_data_before_sampling.csv')

# 2D projections
# HC & Relint
p<-ggplot(data = boxplot.data, aes(x=relative_intensity, y=hard_colors, color=classification)) +#geom_boxplot()+
  xlab("Relative Intensity")+ylab("Hard Colors")+
  geom_point(alpha=0.5)+scale_color_manual(values=c("purple", "orange", "#02a131"))+
  ylim(0, 1)+ xlim(0,1)
p

# SC & Relint
p<-ggplot(data = boxplot.data, aes(x=relative_intensity, y=soft_colors, color=classification)) +#geom_boxplot()+
  xlab("Relative Intensity")+ylab("Soft Colors")+
  geom_point(alpha=0.5)+scale_color_manual(values=c("purple", "orange", "#02a131"))+
  ylim(0, 1)+ xlim(0,1)
p

# SC & HC
p<-ggplot(data = boxplot.data, aes(x=hard_colors, y=soft_colors, color=classification)) +#geom_boxplot()+
  xlab("Hard Colors")+ylab("Soft Colors")+
  geom_point(alpha=0.5)+scale_color_manual(values=c("purple", "orange", "#02a131"))+
  ylim(0, 1)+ xlim(0,1)
p


## _______________ Plot Boxplots of ranges of SC, HC, RelInt before and after subsampling_______________


boxplot.data = read.csv(file ='boxplot_data_before_sampling.csv')

# box plots
# RelInt
# boxplot
p<-ggplot(data = boxplot.data, aes(y=relative_intensity, x=classification,  showWarnings = FALSE)) +#geom_boxplot()+
  xlab("Classification")+ylab("Relative Intensity")+
  ggtitle("Before Subsampling: Range of Relative Intensity")+
  geom_jitter(aes(color=classification,
                  fill=classification),shape=16, position=position_jitter(0.2), alpha=0.5)+
  stat_summary(fun = median,
               geom = "crossbar", width = 0.5, color="black")+ #, aes(colour="Median"))
  ylim(c(0,1.0))
p+scale_color_manual(values=c("purple", "orange", "#02a131")) +scale_fill_manual(values=c("purple","orange", "#02a131"))


# SC
# boxplot
p<-ggplot(data = boxplot.data, aes(y=soft_colors, x=classification)) +#geom_boxplot()+
  xlab("Classification")+ylab("Soft Colors")+
  ggtitle("Before Subsampling: Range of Soft Colors")+
  geom_jitter(aes(color=classification,
                  fill=classification),shape=16, position=position_jitter(0.2), alpha=0.5)+
  stat_summary(fun = median,
               geom = "crossbar", width = 0.5, color="black")+ #, aes(colour="Median"))
  ylim(c(0,1.0))
p+scale_color_manual(values=c("purple", "orange", "#02a131")) +scale_fill_manual(values=c("purple","orange", "#02a131"))



# HC
# boxplot
p<-ggplot(data = boxplot.data, aes(y=hard_colors, x=classification)) +#geom_boxplot()+
  xlab("Classification")+ylab("Hard Colors")+
  ggtitle("Before Subsampling: Range of Hard Colors")+
  geom_jitter(aes(color=classification,
                  fill=classification),shape=16, position=position_jitter(0.2), alpha=0.5)+
  stat_summary(fun = median, 
               geom = "crossbar", width = 0.5, color="black")+ #, aes(colour="Median"))
  ylim(c(0,1.0))
p+scale_color_manual(values=c("purple", "orange", "#02a131")) +scale_fill_manual(values=c("purple","orange", "#02a131"))


# 2D projections
# HC & Relint
p<-ggplot(data = boxplot.data, aes(x=relative_intensity, y=hard_colors, color=classification)) +#geom_boxplot()+
  xlab("Relative Intensity")+ylab("Hard Colors")+
  geom_point(alpha=0.5)+scale_color_manual(values=c("purple", "orange", "#02a131"))+
  ylim(0, 1)+ xlim(0,1)
p

# SC & Relint
p<-ggplot(data = boxplot.data, aes(x=relative_intensity, y=soft_colors, color=classification)) +#geom_boxplot()+
  xlab("Relative Intensity")+ylab("Soft Colors")+
  geom_point(alpha=0.5)+scale_color_manual(values=c("purple", "orange", "#02a131"))+
  ylim(0, 1)+ xlim(0,1)
p

# SC & HC
p<-ggplot(data = boxplot.data, aes(x=hard_colors, y=soft_colors, color=classification)) +#geom_boxplot()+
  xlab("Hard Colors")+ylab("Soft Colors")+
  geom_point(alpha=0.5)+scale_color_manual(values=c("purple", "orange", "#02a131"))+
  ylim(0, 1)+ xlim(0,1)
p



# Run code below to create histograms of the number of observations before and after subsampling

# Counts number of points before sampling
num_of_pts_bf = c()
for (i in 1:length(test_systems)){
  sub_bf = subset(fulldata, System==test_systems[i])
  num_of_pts_bf = append(num_of_pts_bf, c(length(sub_bf["CC1"][,1]))) 
}


# Plot before sampling
par(mfrow=c(1,2))
barplot(num_of_pts, xlab="Sources", ylab="Number of observations", main="Before Subsampling")

minimum= min(num_of_pts)
maximum= max(num_of_pts)
# Plot After sampling
barplot(num_of_pts, xlab="Sources", ylab="Number of observations", main=paste("After Subsampling"))#, min:",minimum, "max:" , maximum))
