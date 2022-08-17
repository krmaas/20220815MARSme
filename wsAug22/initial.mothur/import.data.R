# import mothur files and load libraries
# install.packages("ggplot2")
# install.packages("vegan")
# install.packages("dplyr")
# install.packages("tidyverse")

library(ggplot2)
library(vegan)
library(dplyr)
library(tidyverse)


parseDistanceDF = function(phylip_file) {
  
  # Read the first line of the phylip file to find out how many sequences/samples it contains
  temp_connection = file(phylip_file, 'r')
  len = readLines(temp_connection, n=1)
  len = as.numeric(len)
  len = len +1
  close(temp_connection)
  
  
  phylip_data = read.table(phylip_file, fill=T, row.names=1, skip=1, col.names=1:len)
  colnames(phylip_data) <- row.names(phylip_data)
  return(phylip_data)
}




otu <- read.table(file = "../wsAug22.trim.contigs.good.unique.good.filter.precluster.denovo.vsearch.pick.opti_mcc.0.03.subsample.shared", header=T, stringsAsFactors = FALSE, row.names=2)
otu <- select(otu, -label, -numOtus)


### reading in taxa data
## may come back to this tomorrow
# taxa <- read.table(textConnection(gsub("\\(.+?\\);", "\t", readLines("../TLP2021.trim.contigs.good.unique.good.filter.precluster.pick.pick.opti_mcc.0.03.cons.taxonomy"))), col.names=c("OTU", "Size", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus"), skip=1)
# taxa <- taxa[taxa$OTU %in% names(otu),]
# 
# # get OTU abundance for this subsampling
# sub.size <- data.frame(OTU = colnames(otu), size.sub = colSums(otu))
# 
# taxa <- full_join(taxa, sub.size, by = "OTU", copy=TRUE)
# 
# 
# maxab <- apply(otu, 2, max)
# n1 <- names(which(maxab < 50))
# otu.ab <- otu[,-which(names(otu) %in% n1)]
# taxa.ab <- taxa[-which(taxa$OTU %in% n1),]



### Alpha diversity

alpha <- read.table(file="../wsAug22.trim.contigs.good.unique.good.filter.precluster.denovo.vsearch.pick.opti_mcc.groups.ave-std.summary", header=T, stringsAsFactors = FALSE)
alpha <- filter(alpha, label== "0.03" & method == "ave")

# Beta diversity
jc <- parseDistanceDF("../wsAug22.trim.contigs.good.unique.good.filter.precluster.denovo.vsearch.pick.opti_mcc.jest.0.03.lt.ave.dist")
bc <- parseDistanceDF("../wsAug22.trim.contigs.good.unique.good.filter.precluster.denovo.vsearch.pick.opti_mcc.braycurtis.0.03.lt.ave.dist")
tyc <- parseDistanceDF("../wsAug22.trim.contigs.good.unique.good.filter.precluster.denovo.vsearch.pick.opti_mcc.thetayc.0.03.lt.ave.dist")

# experimental data ### DREW put your data in here
expdata <- read.table(file="../may18ws.env.txt",  header=T, stringsAsFactors = TRUE)

# make a file that is 2 column: the sample name in your exp data csv, and the group name in mothur files 
samples <- read.table(file="../may18ws.sample.txt", header=1, stringsAsFactors = FALSE)
expdata <- left_join(expdata, samples, on="Sample")

alpha.expdata <- left_join(alpha, expdata, on="group")

### if you have issues with this join, email me your expdata and samples.csv
alpha.expdata <- left_join(alpha, expdata, on="group")



##### initial look at alpha diversity

ggplot(data=alpha.expdata, mapping = aes(x = Type, y = sobs))+
  geom_boxplot()

ggplot(data=alpha.expdata, mapping = aes(x = Type, y = shannon))+
  geom_boxplot()

ggplot(data=alpha.expdata, mapping = aes(x = Type, y = invsimpson))+
  geom_boxplot()
S