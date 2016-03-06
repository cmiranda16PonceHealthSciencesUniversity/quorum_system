library(ggplot2)
library(reshape2)
library(gtools)

## Path to .csv files
dir_path <- "R:/Nayoon/plate reader/quorum/"
## Get list of csv filenames
files <- list.files(path=dir_path, pattern="*conv.csv")

## For each file
for(i in 1:length(files)){
    ## Extract the data
    data <- read.csv(file=paste(dir_path, files[i], sep=""), header=T)

    ## Manipulate it in to a dataframe that is easier to use for plotting
    mdf <- melt(data, id=c("Content", "Strain", "Culture", "Rept", "Dilution", "Antibiotic", "Well_Col", "Well_Row"))

    ## This code adds a time column to the melted data so we don't need to use the "variable" column.
    t.col <- c()
    timestep <- 10 ## CHANGEME: this is specific to the experiment - currently at 10 mins
    for (j in 0:(nrow(mdf)/nrow(data) - 1)){
        t.set <- c(rep(1, nrow(data)))
        t.set <- (t.set * j) * timestep
        t.col <- c(t.col, t.set)
    }
    mdf$time <- t.col

    ## Plot the value against time
    ##      - Make a differen plot for each Strain
    ##      - Group based on the combination of Culture and Rept so we get a different line for each well
    plt <- ggplot(data=mdf, aes(time, log10(value), group=paste(Content))) +
        geom_line(aes(color=as.factor(Dilution))) +
        #xlim(0,500) +
        #ylim(0.2, 1.2) +
        #geom_point(aes(color=as.factor(Culture), shape=as.factor(Rept)), size=0.1) +
        facet_wrap(~Strain, ncol=2) +
        xlab("Time")

    ## Save plot as a .png
    ggsave(filename=paste(substr(files[i], 1, nchar(files[i])-4), "_log.png", sep=""), plot=plt, path=dir_path)
}
