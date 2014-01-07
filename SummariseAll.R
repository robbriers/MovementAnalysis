#
# Summary function
# Combined code to import and run analysis
# slightly hacked version, but getting better
# still issue with circular mean for some reason
# 7/1/14
#
# read in files and run summary function (SummTraj2)
#
SummariseAll<-function(){
datalist <- list()
files <- list.files(pattern="\\.csv$")
	for(file in files) {
	# pull out index from file name (first 4 characters)
        stem <- gsub("\\.csv$","",file)
	# read in file
	datalist[[stem]] <- read.csv(file)
	# add comment to use as label for subsequent function
	comment(datalist[[stem]]) <- stem
}
# apply SummTraj2 function to each file in datalist
summlist<-lapply(datalist, SummTraj2)
# convert to dataframe for return, removing row names
summdf<-do.call(rbind, summlist)
# create treatment group factor
treatment<-substr(summdf$replicate, 6, 8)
print (treatment)
# add to summdf
summdf<-cbind(treatment, summdf)
row.names(summdf)<-NULL
return(summdf)

}

#
# SummTraj2
# function to convert data into ltraj object (adehabitatLT)
# label is df name, then derives and stores summary stats from ltraj object
# requires adehabitatLT and circular package as CircStats has trouble with NAs
# returns data frame containing mean and var of step lengths and abs/rel angles of turn
# uses id field from ltraj as grouping variable for deriving stats
#
SummTraj2<-function(test){
# load dependencies
require(adehabitatLT)
require(circular)

# set up label
label<-comment(test)

# add column names
names(test)[1] <- "X"
names(test)[2] <- "Y"
names(test)[8] <- "Time"
# generate time sequence to use in ltraj
times<-seq.POSIXt(ISOdate(2013,7,1), by="2 sec", length.out=length(test$Time))
#convert to ltraj
traj1<- as.ltraj(xy = test[,c("X","Y")], date = times, id = label, typeII=TRUE, slsp = "missing")
frame<-ld(traj1)

# derive mean and var of step lengths from data using aggregate
meanstep<-aggregate(frame$dist, list(frame$id), mean, na.rm=T)
varstep<-aggregate(frame$dist, list(frame$id), var, na.rm=T)

# derive mean and variance of angles (abs and rel), coercing to circular class on the fly
meanabs<-aggregate(circular(frame$abs.angle), list(frame$id), mean.circular, na.rm=T)
meanrel<-aggregate(circular(frame$rel.angle), list(frame$id), mean.circular, na.rm=T)
varabs<-aggregate(circular(frame$abs.angle), list(frame$id), var.circular, na.rm=T)
varrel<-aggregate(circular(frame$rel.angle), list(frame$id), var.circular, na.rm=T)

# concatenate results to new frame
summ<-data.frame()

summ<-meanstep
summ<-cbind(summ, varstep[2], meanabs[2], varabs[2], meanrel[2], varrel[2])
names(summ)<-c("replicate","step.mean", "step.var", "abs.mean","abs.var","rel.mean","rel.var")

return(summ)

}