#
# SummariseAll function
# Combined code to import csv files and derive summary statistics from 
# trajectories in preparation for analysis
# NOT WORKING AT PRESENT - DO NOT USE
# read in files and run summary function (SummTraj2)
#
SummariseAll<-function(){

# load dependencies at the start
require(adehabitatLT)
require(circular)
require(reshape2)

# iterate through the list of files to be processed
datalist <- list()
files <- list.files(pattern="\\.csv$")
	for(file in files) {
	# pull out index from file name
        stem <- gsub("\\.csv$","",file)
	# read in file
	datalist[[stem]] <- read.csv(file)
	# add comment to use as label for subsequent function
	comment(datalist[[stem]]) <- stem
}

# apply SummTraj2 function to each file in datalist
summlist<-lapply(datalist, SummTraj2)

# convert each part of the list from SummTraj2 to dataframe for return
#
# need to finish this bit...perhaps think through how they are returned??
#
summdf<-do.call(rbind, summlist)
# create treatment group factor
treatment<-substr(summdf$replicate, 6, 8)
# add to summdf
summdf<-cbind(treatment, summdf)
# remove row names
row.names(summdf)<-NULL
return(summdf)

}

#
# SummTraj2
# function to convert csv data into ltraj object (adehabitatLT) then derives and stores summary stats from ltraj object
# requires adehabitatLT and circular package as CircStats has trouble with NAs
# label is df name, returns data frame containing mean and var of step lengths and abs/rel angles of turn
# uses id field from ltraj as grouping variable for deriving stats
# adding in code for extracting acf values and returning them in long format
# adds in additional dependency to reshape2
#
SummTraj2<-function(test){

# set up label
label<-comment(test)

# add column names
names(test)[1] <- "X"
names(test)[2] <- "Y"

# generate time sequence to use in ltraj:  this is specific to the current application and can be altered to suit
# or use existing timestamps within the csv file if they are present
times<-seq.POSIXt(ISOdate(2013,7,1), by="2 sec", length.out=length(test$X))

# convert to ltraj and to df
traj1<- as.ltraj(xy = test[,c("X","Y")], date = times, id = label, typeII=TRUE, slsp = "missing")
frame<-ld(traj1)

# extract acf values for steps and angles
acfd<-acfdist.ltraj(traj1, lag=10, plot=FALSE)
acfr<-acfang.ltraj(traj1, which="relative", lag=10, plot=FALSE)
acfa<-acfang.ltraj(traj1, which="absolute", lag=10, plot=FALSE)

# convert each to df, extract observed acf values and add replicate index
obs.acfd<-(cbind(label, as.data.frame(acfd)[1, ]))
obs.acfr<-(cbind(label, as.data.frame(acfr)[1, ]))
obs.acfa<-(cbind(label, as.data.frame(acfa)[1, ]))

# convert to long format for analysis using melt from reshape2
obs.acfd<-melt(obs.acfd, id=names(obs.acfd[1]))
obs.acfr<-melt(obs.acfr, id=names(obs.acfr[1]))
obs.acfa<-melt(obs.acfa, id=names(obs.acfa[1]))

# derive mean and var of step lengths from data using aggregate
meanstep<-aggregate(frame$dist, list(frame$id), mean, na.rm=T)
varstep<-aggregate(frame$dist, list(frame$id), var, na.rm=T)

# derive mean and variance of angles (abs and rel), coercing to circular class on the fly
meanabs<-aggregate(circular(frame$abs.angle), list(frame$id), mean.circular, na.rm=T)
meanrel<-aggregate(circular(frame$rel.angle), list(frame$id), mean.circular, na.rm=T)
varabs<-aggregate(circular(frame$abs.angle), list(frame$id), var.circular, na.rm=T)
varrel<-aggregate(circular(frame$rel.angle), list(frame$id), var.circular, na.rm=T)

# concatenate results to new frame @@ can simplfy here
sum.params<-data.frame()
sum.params<-meanstep
sum.params<-cbind(sum.params, varstep[2], meanabs[2], varabs[2], meanrel[2], varrel[2])
names(sum.params)<-c("replicate","step.mean", "step.var", "abs.mean","abs.var","rel.mean","rel.var")

# combine summary parameters with acf values in a list
return.list<-list(sum.params, obs.acfd, obs.acfr, obs.acfa)
names(return.list)<-c("sum.params", "obsacf.d", "obsacf.r", "obsacf.a")

return(return.list)

}
