# testing functions for memory purposes!

# run analysis on all csv files in the path
test<-summarisePaths()

# view outputs

# first parameters
test$params

# next autocorrelation of step lengths
test$acf.d

# next autocorrelation of relative turning angles
test$acf.r


# next autocorrelation of absolute turning angles
test$acf.a
