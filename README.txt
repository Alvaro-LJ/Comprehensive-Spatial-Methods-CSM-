Comprehensive Spatial Methods (CSM)
CSM is a toolbox developed using R to analyze spatially resolved tissue data
To see a demo of CSM capabilities please see associated publication XXX

#Before you proceed
CSM is delivered to users through a script that contains all required functions (CSM VER_X.X.X SOURCE CODE.R)
Examples of use can be found in the CSM VER_X.X.X USER CODE.R script
In the CSM VER_X.X.X the user can find all the required R packages that must be installed before running CSM functions 








#
#A slide-wise cell X Y coordinates is required
#Features are expression marker data. They should be double-type vectors

#This script subdivides the analysis in 1+6 basic steps
#STEP 0 - Images are segmented. Data is imported and formatted to an adequate format
#STEP 1 - Feature expression data is normalized
#STEP 2 - Feature thresholding is performed
#STEP 3 - Cells are phenotyped according to marker expression
#STEP 4 - Global and spatially resolved heterogeneity analyses are performed
#STEP 5 - Single cell to cell interaction analysis is performed
#STEP 6 - Neighborhood analysis is carried out


#HOW DOES THIS SCRIPT WORKS
#If this is your first time working with this script, you need to execute the FIRST TIME INSTALLATION section
#By executing these lines of code you will download the required packages from CRAN, Bioconductor and GitHub repositories



#Thanks for using CSM!!
#Any bugs can be reported to A-LJ (alopezj@unav.es). We will try our best to improve the script