dir_path <- "D:/UCL Data/FACS_data/quorum_17_02_16/"
require(gtools)
library(ggplot2)

dirs <- mixedsort(list.files(path=dir_path))
outflag <- "quorum_17_02_16"

patterns <- c("*Q_?_100.fcs", "*Q_?_1000.fcs", "*A_?_100.fcs", "*A_?_1000.fcs", "*N_?_100.fcs", "*N_?_1000.fcs")
#CHANGEME: strain identifying labels - just a label used to identify results
strains <- c("Quorum", "Quorum", "OXB20", "OXB20", "N", "N")

dilutions <- c(100, 1000, 100, 1000, 100, 1000)


all.data <- data.frame()
source("C:/Users/sandy/Documents/GitHub/plasmid_loss_flow/pre_process/getTrimmedFS.R", chdir=T)
for(i in 1:length(patterns)){
    fs.data <- get.trimmedFSets( dir_path, dirs, patterns[i] )
    print("Got Trimmed Data")

    fs <- fs.data[[1]][[1]]

    data <- data.frame(strain=NA, timepoint=fs.data[[2]], rept=fs.data[[3]])
    data$strain <- strains[i]
    data$dilution <- dilutions[i]

    data$meanFluor <- 0
    for(i in 1:length(fs) ){
        temp.summary <- summary(fs[[i]])
        data[i, "meanFluor"] <- temp.summary["Mean", "FL1-H"]
        data[i, "medFluor"] <- temp.summary["Median", "FL1-H"]
    }

    all.data <- rbind(all.data, data)
}

agg.data <- aggregate(all.data, by=list(Timepoint=all.data$timepoint, Strain=all.data$strain, dilution=all.data$dilution), FUN=mean)

ggplot() +
    geom_point(data=all.data, aes(x=timepoint, y=meanFluor, colour=strain, group=strain), shape=1) +
    geom_point(data=all.data, aes(x=timepoint, y=medFluor, colour=strain, group=paste(strain,dilution)), shape=3) +
    # geom_line(data=agg.data, aes(x=agg.data$Timepoint, y=agg.data$medFluor, group=paste(Strain,dilution), colour=Strain)) +
    stat_smooth(data=all.data, aes(x=timepoint, y=medFluor, colour=strain, fill=strain, group=paste(strain,dilution)))+
    #facet_wrap(~dilution, ncol=2) +
    #xlim(0, 9) +
    labs(x="Time (hours)", y="log10 Fluorescence", title="Median GFP Fluorescence Produced Constitutively or Through\n Quorum Sensing: Different Starting Dilutions") +
    theme(plot.title = element_text(size=20, face="bold", vjust=1, margin = margin(10, 0, 10, 0)))

ggplot() +
    geom_point(data=all.data, aes(x=timepoint, y=10^meanFluor, colour=strain, group=strain), shape=1) +
    geom_point(data=all.data, aes(x=timepoint, y=10^medFluor, colour=strain, group=paste(strain,dilution)), shape=3) +
    # geom_line(data=agg.data, aes(x=agg.data$Timepoint, y=10^agg.data$medFluor, group=paste(Strain,dilution), colour=Strain)) +
    stat_smooth(data=all.data, aes(x=timepoint, y=10^medFluor, colour=strain, group=paste(strain,dilution)))+
    #facet_wrap(~dilution, ncol=2) +
    #xlim(0, 9) +
    labs(x="Time (hours)", y="Fluorescence", title="Median GFP Fluorescence Produced Constitutively or Through\n Quorum Sensing: Different Starting Dilutions") +
    theme(plot.title = element_text(size=20, face="bold", vjust=1, margin = margin(10, 0, 10, 0)))
