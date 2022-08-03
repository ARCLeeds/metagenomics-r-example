source("scripts/setup.R")

library("xlsx") 
#Alternatively, install just readxl from CRAN: install.packages("readxl")
library(ggplot2)
library(cowplot)
library(cowplot)
library(ggpubr)
library(ggsci)
library(tidyverse)


#library(readxl)
#dff=read_excel("Anonimised_data/otu_table-ex_All_normalised200000scale-ex-FamilyAnonimised_Trans-Reduced_Metadata_included.xlsx")
#dff=read_excel("Anonimised_data/OTU_Drug_Data_Bookchap.xlsx")
library("xlsx") 
dff=read.xlsx2("data/OTU_Drug_Data_Bookchap.xlsx",sheetIndex = 1)
head(dff)

#Diversity
install.packages("vegan")
install.packages("permute")
library(permute)
library(lattice)
library(vegan)
Sim.diversity<-diversity(dff[,-c(1:4)], "simpson") 
#head(dff)
#dim(dff)

#Simpson diversity
Simp.data<- data.frame(Sample=dff$SampleID, Group=dff$Group, Timepoint=dff$Timepoint, Sex=dff$DummySex, Sim.diversity)
head(Simp.data)
Simp.data$Timepoint <- factor(Simp.data$Timepoint, levels = c('D1','D2','D5','D7', 'D10','D15','D25','D30'),ordered = TRUE) #  For prefered order. Otherwise R puts D1 then D10, then D2 onwards 
Timepoint<-as.factor(Simp.data$Timepoint) #Cause we want to colour by timepoint
str(Timepoint)
# Ord.factor w/ 8 levels "D1"<"D2"<"D5"<..: 5 6 2 7 8 3 4 5 7 8 ...
#R Plot diversity
#Sort the data by timepoint
Simp.data<-Simp.data[order(Timepoint),] #Sort the whole based on Timepoint 
head(Simp.data)
Group<-Simp.data$Group
str(Simp.data$Group)
#chr [1:107] "X" "X" "X" "X" "X" "X" "X" "X" "X" "X" "X" "X" "X" ...
Group<-as.factor(Simp.data$Group)
str(Group)
#Factor w/ 2 levels "X","Y": 1 1 1 1 1 1 1 1 1 1 ...

#Now we need 8 separate cols. Making a new palette of 10 cols
colors <- c("black","red", "green3","blue","cyan","magenta","yellow","gray","orange","violet","brown","gold1")
Timepoint<-as.factor(Simp.data$Timepoint) #Cause we want to colour by timepoint

plot(Simp.data$Sim.diversity,col=colors[Timepoint], pch=as.numeric(Group))

plot(Simp.data$Sim.diversity, axes=FALSE, col=colors[Timepoint], pch=as.numeric(Group), main="Simpson's diversity distribution",ylab="Simpson",xlab="Timepoints")
seq(1,107,by=14)  #Trying to find exact position for labels
axis(1, at =seq(1,107,by=14), labels = levels(Timepoint), cex.axis = 0.65)
axis(2)
box()
legend(95,0.48, legend=levels(Timepoint), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       cex=0.65,lwd=c(4,4),col=colors) # gives the legend lines the correct color and width
legend(95,0.25,legend=levels(Group),cex=0.65, pch=16)

#Solution using ggplot2:
#Diversity scatter plot by ggplot
ggplot(data = Simp.data, aes(x=Timepoint, y=Sim.diversity,group=Group))+
  geom_point(aes( colour = Timepoint,  shape=Group))+ggtitle("Simpson's diversity distribution")+ theme(plot.title = element_text(hjust = 0.5))

#Boxplots
#Diversity box plot by ggplot Drugs
ggplot(data = Simp.data, aes(x=Timepoint, y=Sim.diversity)) + geom_boxplot(aes(fill=Group))
head(Simp.data)

# 
# > head(Simp.data)
# Sample Group Timepoint Sex Sim.diversity
# 12 S136_D1     X        D1   M     0.6707577
# 17 S141_D1     X        D1   F     0.6259074
# 22 S143_D1     X        D1   F     0.7555204
# 28 S144_D1     X        D1   M     0.6655613
# 35  S22_D1     X        D1   F     0.7366364
# 38 S231_D1     X        D1   M     0.7637540
# > 

#Diversity box plot by ggplot Sex
ggplot(data = Simp.data, aes(x=Timepoint, y=Sim.diversity)) + geom_boxplot(aes(fill=Sex))

#Now beta diversity
?vegdist()
head(dff)
dim(dff)

Taxa.bundance<-data.frame(dff[,-c(1:4)])
rownames(Taxa.bundance)<-dff$SampleID
distance.matrix <- vegdist(Taxa.bundance, method="bray")
#Now if we want to see the distance matrix we can use 
as.matrix(distance.matrix) # in full symmetric form
#Or if we want to see it graphically
heatmap(as.matrix(distance.matrix))
#Now if we subset our data and create heatmap for each drug group 

dff.X<-dff[dff$Group=='X',]
Taxa.bundance.X<-data.frame(dff.X[,-c(1:4)])
rownames(Taxa.bundance.X)<-dff.X$SampleID
heatmap(as.matrix(vegdist(Taxa.bundance.X,method="bray")))

dff.Y<-dff[dff$Group=='Y',]
Taxa.bundance.Y<-data.frame(dff.Y[,-c(1:4)])
rownames(Taxa.bundance.Y)<-dff.Y$SampleID
heatmap(as.matrix(vegdist(Taxa.bundance.Y,method="bray")))

install.packages("heatmaply")
library("heatmaply")
heatmap.X<-heatmaply(as.matrix(vegdist(Taxa.bundance.X,method="bray")),file = "heatmaply_plot_X.html")
 #name of the file(s) into which to save the heatmaply output. Should be a character vector of strings ending with ".html" for a dynamic output, or ".png", ".jpeg", ".pdf" for a static output. For example: heatmaply(x, file = "heatmaply_plot.html")
heatmaply(as.matrix(vegdist(Taxa.bundance.Y,method="bray")))

#par(mfrow=c(1,1))

#PCA
#head(dff)
#dim(dff)
pca<-prcomp(dff[,-c(1:4)], scale= TRUE) # removing first four columns for metadata included sample
?prcomp() 
## plot pc1 and pc2
plot(pca$x[,1],pca$x[,2])
## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
## now make a fancy looking plot that shows the PCs and the variation:
pca.data <- data.frame(Sample=dff$SampleID,
                       X=pca$x[,1],
                       Y=pca$x[,2])
#pca.data
require(ggplot2)
colData <- data.frame(Sample=dff$SampleID,
                      X=pca$x[,1],
                      Y=pca$x[,2],Group=dff$Group, Timepoint=dff$Timepoint, Sex=dff$DummySex)

colData$Timepoint <- factor(colData$Timepoint, levels = c('D1','D2','D5','D7', 'D10','D15','D25','D30'),ordered = TRUE) # Just to put the data in desired order.   
head(colData) # check before plotting

#ggplot by drug
ggplot(data=colData, aes(x=X, y=Y))+
  geom_point(aes(colour = Timepoint,  shape=Group),size=2)+
  #geom_text(size=1,nudge_y = 0.05,  check_overlap = FALSE) +
  
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +ggtitle("My PCA Graph")+ theme(plot.title = element_text(hjust = 0.5)) #to make plot center

#ggplot by male female
ggplot(data=colData, aes(x=X, y=Y))+
  # geom_point(aes(colour = Timepoint,  shape=Group))+
  geom_point(aes(colour = Timepoint,  shape=Sex), size=5)+
  scale_shape_manual(values = c(M = "\u2642", F = "\u2640"))+
  #geom_text(size=1,nudge_y = 0.05,  check_overlap = FALSE) +
  
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +ggtitle("My PCA Graph")+ theme(plot.title = element_text(hjust = 0.5)) #to make plot center



####Pie charts
##Please follow Piechart code. 
