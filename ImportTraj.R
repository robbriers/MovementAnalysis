#
# GetTraj
# function to import data into ltraj object (adehabitatLT)
# label is a string to identify treatment/replicate
#
GetTraj<-function(label){
require(adehabitatLT)
# read in file
test<-read.csv(file.choose(),header=F)
# add column names
names(test)[1] <- "X"
names(test)[2] <- "Y"
names(test)[8] <- "Time"
# generate time sequence to use in ltraj
times<-seq.POSIXt(ISOdate(2013,7,1), by="2 sec", length.out=length(test$Time))
#convert to ltraj
traj1<- as.ltraj(xy = test[,c("X","Y")], date = times, id = label, typeII=TRUE, slsp = "missing")
return(traj1)
}
