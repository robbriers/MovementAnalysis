Movement Analysis
=================

R code for summary and analysis of animal movement patterns. Reads in csv files to ltraj objects (adehabitatLT) and produces summary statistics (mean and variance of step lengths, absolute and relative turning angles). Also returns autocorrelation values for movement parameters up to 10 lags, following the methods described in Dray et al. 2010.

Main function (summariseAll) is setup to read in all csv files in the working directory, but can call individual functions (makeTraj, extractParams and extractACF) to work on specific files, provided they have been read into dataframes. An example data file is given to demonstrate the format expected. The code can be easily adapted to other data formats.

Dependencies
------------
Calls the following packages:
  * adehabitatLT
  * circular
  * reshape2

Output
-------

Returns a list object containing four dataframes, with filename indices

  * params - means and var of lengths and angles for each path
  * acf.d - observed autocorrelation values for step lengths
  * acf.a - observed autocorrelation values for absolute turning angles
  * acf.r - observed autocorrelation values for relative turning angles
