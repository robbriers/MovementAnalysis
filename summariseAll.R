#
# summarisePaths function
# Import csv files, derive summary statistics and ACF values from 
# adehabitatLT trajectories in preparation for analysis
#
summarisePaths<-function(){

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

# convert files and extract values
trajlist<-lapply(datalist, makeTraj)
acfdlist<-lapply(trajlist, extractACF, type="dist")
acfalist<-lapply(trajlist, extractACF, type="abs")
acfrlist<-lapply(trajlist, extractACF, type="rel")
paramlist<-lapply(trajlist, extractParams)

# convert outputlists to dataframes for return
summ.acfd<-do.call(rbind, acfdlist)
summ.acfa<-do.call(rbind, acfalist)
summ.acfr<-do.call(rbind, acfrlist)
summ.params<-do.call(rbind, paramlist)

# create treatment group factor: this is specific to the current setup and can be edited
treatment<-substr(summ.params$replicate, 6, 8)
summ.params<-cbind(treatment, summ.params)
treatment<-substr(summ.acfd$replicate, 6, 8)
summ.acfd<-cbind(treatment, summ.acfd)
summ.acfa<-cbind(treatment, summ.acfa)
summ.acfr<-cbind(treatment, summ.acfr)

# remove row names
row.names(summ.params)<-NULL
row.names(summ.acfd)<-NULL
row.names(summ.acfa)<-NULL
row.names(summ.acfr)<-NULL

# combine output to list for return
return.list<-list(summ.params, summ.acfd, summ.acfa, summ.acfr)
names(return.list)<-c("params", "acf.d", "acf.a", "acf.r")

return(return.list)

}

#
# Function to convert imported csv file to ltraj object
#
makeTraj<-function(df){

# add column names
names(df)[1] <- "X"
names(df)[2] <- "Y"

# generate time sequence to use in ltraj:  this is specific to the current setup and can be altered to suit
# or use existing timestamps within the csv file if they are present
times<-seq.POSIXt(ISOdate(2013,7,1), by="2 sec", length.out=length(df$X))

# convert to ltraj
traj<- as.ltraj(xy = df[,c("X","Y")], date = times, id = comment(df), typeII=TRUE, slsp = "missing")

# pass on comment for use later
comment(traj)<-comment(df)
return(traj)
}

#
# Function to extract observed ACF values and return
#
extractACF<-function(traj, type){

# extract appropriate acf value, depending on type
if (type =="dist") {acf<-acfdist.ltraj(traj, lag=10, plot=FALSE)}
if (type =="abs") {acf<-acfang.ltraj(traj, which="absolute", lag=10, plot=FALSE)}
if (type =="rel") {acf<-acfang.ltraj(traj, which="relative", lag=10, plot=FALSE)}

# convert to df, extract observed acf values and add replicate index
obs.acf<-(cbind(comment(traj), as.data.frame(acf)[1, ]))

# convert to long format for analysis using melt from reshape2
obs.acf<-melt(obs.acf, id=names(obs.acf[1]))

# add column headings
names(obs.acf)<-c("replicate","lag", "acf.val")

return(obs.acf)
}

#
# Function to extract summary parameters from trajectory
#
extractParams<-function(traj){

# convert ltraj to dataframe
frame<-ld(traj)

# derive mean and var of step lengths from data using aggregate
meanstep<-aggregate(frame$dist, list(frame$id), mean, na.rm=T)
varstep<-aggregate(frame$dist, list(frame$id), var, na.rm=T)

# get R2N value from final value of column
r2n<-frame$R2n[length(frame$R2n)]

# derive mean and variance of angles (abs and rel), coercing to circular class on the fly
meanabs<-aggregate(circular(frame$abs.angle), list(frame$id), mean.circular, na.rm=T)
meanrel<-aggregate(circular(frame$rel.angle), list(frame$id), mean.circular, na.rm=T)
varabs<-aggregate(circular(frame$abs.angle), list(frame$id), var.circular, na.rm=T)
varrel<-aggregate(circular(frame$rel.angle), list(frame$id), var.circular, na.rm=T)

# concatenate results to new frame
summ<-cbind(meanstep, varstep[2], meanabs[2], varabs[2], meanrel[2], varrel[2], r2n)
names(summ)<-c("replicate","step.mean", "step.var", "abs.mean","abs.var","rel.mean","rel.var", "R2N")

return(summ)
}
