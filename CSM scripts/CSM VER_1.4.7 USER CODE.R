######################Comprehensive Spatial Methods (CSM) ############################

######################README############################
#CSM is a resource to perform analysis of spatially resolved data
#Basic input will ALWAYS be a matrix where cells are organized in rows and features are organized in columns
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

#RELEVANT INFOR FOR RSTUDIO USERS!!!!!
#CSM uses plenty of R packages (more than 50!). If you are using RStudio (which is a great idea), you will find that after installing all the required packages, these may be loaded
#by default in your namespace environment to test and avoid conflicts between pacakge functions. 
#As a consequence, sometimes a substantial amount of RAM memory is allocated to your R session even if the actual packages are not required to perform
#the desired computation. If this is your case, it is advisable to turn of this behavior. 
#You can do this trough: Tools > Global Options > Code > Diagnostics and deselect the "Show diagnostics for R" tickbox
#Afterwards you can re-start RStudio to see the improvement


#Thanks for using CSM!!
#Any bugs can be reported to A-LJ (alopezj@unav.es). We will try our best to improve the script

##############FIRST TIME INSTALLATION###############
#TO BE EXECUTED DURING THE FIRST INSTALLATION ONLY

#First install CRAN packages
Required_CRAN_packages <- c("alphahull", 
                            "ape", 
                            "autothresholdr", 
                            "benchmarkme", 
                            "berryFunctions",
                            "catsim",
                            "ClusterR", 
                            "cowplot", 
                            "corrplot", 
                            "dbscan", 
                            "deldir",
                            "dendextend",
                            "DescTools", 
                            "devtools", 
                            "doParallel", 
                            "elsa", 
                            "future",
                            "furrr",
                            "ggforce",
                            "ggiraph",
                            "ggpubr", 
                            "glcm",
                            "gt",
                            "igraph", 
                            "imager", 
                            "imagerExtra",
                            "lme4", 
                            "Matrix", 
                            "mmand",
                            "mxnorm", 
                            "pals",
                            "ParallelLogger", 
                            "partR2", 
                            "patchwork",
                            "philentropy",
                            "picante", 
                            "raster", 
                            "readr", 
                            "remotes",
                            "Rmixmod",
                            "sf", 
                            "shiny",
                            "shinyWidgets",
                            "spatstat", 
                            "survival", 
                            "survminer",
                            "svd",
                            "tabularaster", 
                            "tidyverse", 
                            "uwot",
                            "vegan",
                            "zeallot")

install.packages(Required_CRAN_packages)


#Second install Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install(c("bluster", 
                       "ComplexHeatmap", 
                       "ConsensusClusterPlus",
                       "cytomapper", 
                       "EBImage", 
                       "FlowSOM", 
                       "imcRtools", 
                       "lisaClust", 
                       "SingleCellExperiment", 
                       "simpleSeg", 
                       "snifter",
                       "SpatialExperiment",
                       "SPIAT")
)


#Finally install GitHub packages
library(remotes)
library(devtools)
remotes::install_github("akoyabio/rtree")
devtools::install_github("plevritis/CELESTA")


#####################################################
#####################################################

##############IMPORT THE CSM FUNCTIONS and CSM dependencies###############
#specify the file containing the CSM functions to load into your R session

source(file  = "Comprehensive-Spatial-Method-CSM-/CSM scripts/CSM VER_1.4.7 SOURCE CODE.R", # The file location of CSM functions
       echo = FALSE)

#######################################STEP 0 - DATA GENERATION#####################################################
#COLUMN ORDER SHOULD ALWAYS BE: 
#   Cell_no, 
#   X, 
#   Y, 
#   Subject_Names (Image ID)
#   MARKERS
#Check the column order of your DATA before finishing STEP 0

#First things first
#You may want to know some basic information of your computing resources
Describe_my_computing_unit()


#0 (OPTIONAL) You can obtain marker expression directly from images using the  Segmentation_tester and Cell_segmentator_quantificator functions
# Images must be multi-tiff (with each channel representing a single marker). All images must be saved in a single directory (folder)
# If you are working with color images (like the ones obtained with HE, IHC or other histochemical techniques) you can isolate channels of interest using
# the dedicated functions: Color_deconvolution_App_launcher (to explore deconvolution parameters) and Image_deconvolution_function (to isolate channels)

#use this function to decide the parameters required to separate color channels
Color_deconvolution_App_launcher(Directory = "My_images_directory" #path to the images that need to be explored
)

str(Deconvolution_Parameters)

Image_deconvolution_function(Directory = "My_images_directory", #path to the images that will be processed
                             Output_directory = "Processed_Images_directory", #path to the empty folder where the resulting multi-tiff images will be placed
                             Deconvolution_parameters = Deconvolution_Parameters, #A list of paramters to isolate every channel
                             N_cores = 1 #Number of cores to parallelize your computation
)

#0 (OPTIONAL) sometimes proyects involve working simultaneously with cells and extra-cellular molecules. Working with this images can be challenging. 
#CSM allows the user to work with images using various approaches. Images can be thresholded to select positive pixels that can then be used to analyze their relationship with cells

#Pixel thresholding parameters can be explored using the dedicated thresholding APP
Image_thresholding_app_launcher(Directory = "My_images_directory",
                                Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"))

#Pixels can be thresholded using the following functino
Pixel_Threshold_calculator(N_cores = 2, #Cores to parallelize computation
                           Directory = "My_images_directory", #Path to images
                           Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"), #Image channels in order
                           Channels_to_keep = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"), #Channels used in tissue mask generation
                           Target_channel = "PDL1", #Target channel to be analyzed
                           
                           Save_processed_images = TRUE, #Should thresholded images saved? These thresholded images can be used in further analyses
                           Output_Directory = "Processed_Images_directory", #Path where output images should be stored
                           
                           Local_thresholding = FALSE, #Thresholds should be calculated locally (per image) or using all images in the directory 
                           Threshold_type = "Multilevel", #Type of threshold should be one of the following: Arbitrary, Otsu or Multilevel
                           Threshold_value = c(0.001, 0.1, 0.3), #Values must be provided for Arbitrary and CAN be provided for Multilevel
                           Levels = 3, #Multilevel thresholding can also be performed automatically. The desired number of levesl can be provided if Threshold_value is NULL
                           
                           Threshold_type_tissueMask = "Arbitrary", #Type of threshold to performe tissue mask. Either Otsu, Arbitrary or Absolute
                           Threshold_value_tissueMask = 0.001, #Value used if Arbitrary is the threshold type of choice
                           Blurr_tissueMask = TRUE, #Should image blurring be performed before tissue mask generation?
                           Sigma_tissueMask = 0.5, #Sigma to perform image blurring
                           
                           Blurr_target = TRUE, #Should target image blurring be performed before thresholding
                           Sigma_target = 4 #Sigma to perform image blurring
)

#Mean Fluorescence Intensity for every image can also be calculated

#Mean Fluorescence Intensity is always calculated using a tissue mask, but can also take into account an overlay of masks

#Mask list. A lis of lists. Every marker list should have 5 elements.
#In this example we will try to calculate the MFI of PDL1 in macrophages that are located within the tumor compartment
Target_mask_list <- list(Tumor = list(Mask_name = "CK", 
                                      Threshold_type = "Arbitrary", 
                                      Threshold_value = 0.001, 
                                      Blurr = TRUE, 
                                      Sigma = 2),
                         MACROPHAGES = list(Mask_name = "CD68", 
                                            Threshold_type = "Arbitrary", 
                                            Threshold_value = 0.001, 
                                            Blurr = TRUE, 
                                            Sigma = 2)
)

#Afterwards, MFI can be calculated using the following function
MFI_Experimet_Calculator(
  N_cores = 2, #Cores to parallelize computation
  Directory = "My_images_directory", #Directory storing images
  Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"), #Image channels in order
  Channels_to_keep = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"), #Channels used in tissue mask generation
  Target_channel = "PDL1", #Target channel to be analyzed
  
  Target_masks = Target_mask_list, #This will be a list of lists features where each item will be a target mask 
  
  Threshold_type_tissueMask = "Arbitrary", #Type of threshold to performe tissue mask. Either Otsu, Arbitrary or Absolute
  Threshold_value_tissueMask = 0.01, #Value used if Arbitrary is the threshold type of choice
  Blurr_tissueMask = TRUE, #Should image blurring be performed before tissue mask generation?
  Sigma_tissueMask = 0.5 #Sigma to perform image blurring
)

#Now we can perform cell segmentation
#The following function allows to test different segmentation parameters
Segmentation_Parameters <- Segmentation_tester(Directory = "My_images_directory", #path to the folder containing the images
                                               Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"), #The order of channels in the multi-tiff
                                               Channels_to_keep = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK"), #Channels to be kept in the analysis
                                               Images_to_test = NULL, #Optionally the user may specify the names (a vector) of the images to use in the test
                                               Number_tests = 1, #Alternatively, a random number of images can be specified
                                               
                                               Nuclear_marker = "DAPI", #marker or list of markers corresponding to the nuclei
                                               Cell_body_method = "discModel", #Method of cytoplasm identification. Can be 'none', 'dilate', 'discModel'
                                               Min_pixel = 10, # Minimum pixels for an object to be recognized as a cell and not noise
                                               Smooth_amount = 1, #The amount of Gaussian smoothing to be applied to the image
                                               Normalization = "tissueMask", #Single value or vector specifying the transformations from "sqrt", "asinh", "norm99", "maxThresh" and "tissueMask"
                                               Watershed_type = "combine", #Method used to perform watersheding. Accepted values: "intensity", "distance" or "combine"
                                               Tolerance_value = NULL, #minimum height of the object in the units of image intensity between its highest point (seed) and the point where it contacts another object (MAY BE NULL)
                                               Neighborhood_distance = 1, #Radius of the neighborhood in pixels for the detection of neighboring objects. Higher value smooths out small objects.
                                               Disc_size = 15, #The size of dilation around nuclei to create cell disc or capture cytoplasm
                                               Tissue_mask_markers = c("CK", "CD68", "CD8"), #A vector specifying the channels to be used to create the tissue mask if specified in transforms
                                               Perform_PCA = FALSE #Whether to run PCA on aggregated nucleus markers in order to detect the cellular nucclei
)

#You can also used a dedicated function to explore segmentation parameters
segmentator_tester_app(Directory = "My_images_directory",
                       Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"))
str(Segmentation_Parameters)

#The outcome is a parameter list that can be used in the following function
DATA_RAW <- Cell_segmentator_quantificator(Directory = "My_images_directory", #path to the folder containing the images
                                           Parameter_list = Segmentation_Parameters, #List containing the parameters to perform segmentation (the product of the segmentation_tester_app or the Segmentation_tester function)
                                           
                                           N_cores = 38, #Number of cores to parallelize your computation
                                           
                                           quantiles_to_calculate = c(0.05, 0.25, 0.5, 0.75, 0.95), # quantiles to be calculated for each marker
                                           #By default the mean and sd for every cell and markers are calculated
                                           
                                           #Alternatively the parameters can be specified following the same rules as for Segmentation_tester function
                                           Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO"),
                                           Channels_to_keep = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK"),
                                           Nuclear_marker = "DAPI",
                                           Cell_body_method = "discModel",
                                           Min_pixel = 10,
                                           Smooth_amount = 1,
                                           Normalization = "tissueMask",
                                           Watershed_type = "combine",
                                           Tolerance_value = NULL,
                                           Neighborhood_distance = 1,
                                           Disc_size = 7,
                                           Tissue_mask_markers = c("CK", "CD68", "CD8"),
                                           Perform_PCA = FALSE
)

write.csv(DATA_normalized, "DATA_RAW.csv", row.names = F)
paste0("Your cell data file has been saved in: ", getwd())


#1 IMPORT DATA
DATA <- readRDS("Endometrial_cores_Dataset.rds")#Paste your data pathway and select delimiter ("," is the default)

#Let's see all the imageIDs from the DATA
unique(DATA$imageID)

#Let's see your DATA structure
str(DATA)

#Explore your variables. This step is required for further processing
View(tibble(Variables = names(DATA),
              Column_number = str_c("Column", 1:ncol(DATA), sep = " ")))


#2 We will keep quantile 95 marker quantifications
Markers_to_keep <- names(DATA)[str_detect(names(DATA), "q095")]

#Now let's rearrange the DATA in order to start working with it
DATA <- Data_arrange_function(DATA = DATA, #Name of data frame
                              X = 'm.cx',  #Name of column specifying X coordinate
                              Y = "m.cy", #Name of column specifying Y coordinate
                              Subject_Names = "imageID", #Name of column specifying the image name for each cell
                              Markers_to_keep = Markers_to_keep) #State markers by column name (Vector)

#Optionally we can remove cells in the border of the image (somtimes they contain artifacts)
DATA <- Cell_in_edge_remover(N_cores = 4, #Number of cores to parallelize your computation 
                             DATA = DATA, #Data frame
                             Hull_ratio = 0.5, #Define the hull ratio for Concave hull calculation
                             Distance_to_edge = 20, #Distance to edge to define a cell in the edge
                             Image_preview = NULL #Image to be previewed befor executing cell removal, it can also be NULL to select a random image
)

#3 Set aside some of the variables that wont be used immediately in subsequent analyses 
#(these may be functional markers that you are not willing to introduce in the cell labeling process, but may be used in the future)
#For this test we'll set aside PD1 and PD-L1

DATA_list <- Data_set_aside(DATA = DATA, #Choose your formatted data
                            Markers_to_set = c("PD1_q095", "PDL1_q095")) #Choose markers to set aside (can be a single marker or a vector)


#Original data without set aside variables will be stored $DATA, data with set apart variables will be stored in $Aside
DATA <- DATA_list$DATA
DATA_aside <- DATA_list$Aside

#FINAL DATA IS READY
DATA
DATA_aside

#Check the number of cells by sample
Cell_number_by_image <- DATA %>% dplyr::group_by(Subject_Names) %>% dplyr::count() %>% arrange(n) %>% dplyr::ungroup()

#We will remove samples with less than 1K cells
DATA <- DATA %>% dplyr::filter(Subject_Names %in% unlist(Cell_number_by_image[Cell_number_by_image$n >= 1000, 1]))


#4 APPLY QUALITY CHECKS (fix if necessary)
Data_QC_Check_function(DATA = DATA) #Specify the data to be checked for missing values and single-value variables


#6 PREPARE CLINICAL DATA

DATA_CLINICAL <- readRDS("Endometrial_data_clinical.rds")

#We will change certain variables to factors
DATA_CLINICAL <- DATA_CLINICAL %>% dplyr::mutate(RELAPSE = as.factor(RELAPSE),
                                                 DEATH = as.factor(DEATH),
                                                 LVI = as.factor(LVI),
                                                 MMRP_status = as.factor(MMRP_status))

#Now let's rearrange your clinical data
DATA_CLINICAL <- Clinical_Data_arrange_function(DATA = DATA_CLINICAL, #Specify the object that contains the clinical data
                                                Subject_Names = "Subject_Names", #Specify the column that contains the subject names
                                                Outcomes_to_keep = c("AGE", "RELAPSE", "DEATH", "OS_m", "LVI", "MMRP_status") #Specify clinical data to keep in for further analyses
)


#7 Calculate image size if necessary
DATA_AREA <- Image_size_calculator(DATA = DATA,#Specify the data source to calculate the areas it can be any DATA type that has X and Y
                                   
                                   Strategy = "Concave_hull", #Specify the area inference strategy (either Tiling or Concave_hull)
                                   Image_to_plot = NULL, #Specify the image to perform preview (if NULL the smallest sample will be chosen)
                                   
                                   Tile_accuracy = 50, #Lower values calculate the area in a more precise manner, with higher computational times
                                                 #In addition if the value is too low, it can detect areas of stroma as not being tissue 
                                   Hull_ratio = 0.5 #Define the hull ratio for Concave hull calculation
)


#Calculate the correlation between markers across cells if required. This can aid to remove highly correlated markers that may actually contain the same information
Plot_correlation_matrix(DATA = DATA, #Provide sample information data
                        Variables_included = names(DATA)[-c(1:4)], #Provide the markers to be correlated
                        Correlation_method = "pearson" #Select one of "pearson" or "spearman"
)


#######################################STEP 1 - NORMALIZATION#####################################################
#1 Decide your normalization approach either mxnorm or simpleSeg

#2 Decide Normalization Parameters according to the strategy chosen

#slide ID for TMA: We substract the TMA information from the Subject_Names variable to take into account the slide information in the normalization process
TMA_image <- substr(DATA$Subject_Names, start = 0, stop = nchar(DATA$Subject_Names)-7)

mxnorm_Parameters <- list(slide_id = "TMA_image", #The column name of the DATA that contains the slide ID (this can be the Subject_Names or a user supplied vector containing the slide identity of these cells)
                          image_id = "Subject_Names", #The column name of the DATA that contains the image ID
                          marker_cols = names(DATA[-c(1:4)]), #The columns of the DATA that should be normalized
                          transform = "None", # Choose between: "None", "log10", "mean_divide","log10_mean_divide"
                          method = "ComBat", # Choose between: "None", "ComBat","Registration"
                          method_override = NULL, # To specify a user defined method (contact A-LJ for alternatives)
                          method_override_name = NULL # To specify the name of the user defined method
)


simpleSeg_Parameters <- list(cells = DATA,
                             markers = names(DATA[-c(1:4)]), 
                             imageID = "Subject_Names", 
                             transformation = "sqrt", #Choose between:  NULL, "asinh", "sqrt"
                             method = c("trim99", "minMax"), # Choose between (can be multiple): NULL, 'mean', 'minMax', 'trim99', 'PC1'
                             cores = 2 #Number of cores to use in the computation (deprecated) (Set with N_cores argument in Normalization function)
)


#3 Normalize your data
#You can choose the first or the second approach. You can use mxnorm_Parameters or simpleSeg_Parameters in each

#First approach a classic normalization
DATA_normalized <- Normalization_function(DATA, #Specify your data
                                          Strategy = "simpleSeg", #choose between mxnorm or simpleSeg
                                          Parameters = simpleSeg_Parameters #Specify your parameters
)

#Second approach is parallelized and may be faster for datasets containing huge amount of cells (for example with more than 10^6 - 10^7 cells)
DATA_normalized <- Normalization_function_parallel(DATA, #Specify your data
                                                   Strategy = "simpleSeg", #choose between mxnorm or simpleSeg
                                                   Parameters = simpleSeg_Parameters, #Specify your parameters
                                                   N_cores = 2 #Number of cores to parallelize your computation 
)

#Let's see your DATA structure
str(DATA_normalized)


#4 Your data has been normalized
#You can compare now your original DATA vs your DATA_normalized with Normalization_diagnostics
Normalization_diagnostics(Original_DATA = DATA,
                          Normalized_DATA = DATA_normalized)

#You can check the correlation between markers after the normalization process
Plot_correlation_matrix(DATA = DATA_normalized, #Provide sample information data
                        Variables_included = names(DATA_normalized)[-c(1:4)], #Provide the markers to be correlated
                        Correlation_method = "pearson" #Select one of "pearson" or "spearman"
)

#You may need to export your normalized data
write.csv(DATA_normalized, "Normalized_cells.csv", row.names = F) #Write the path
paste0("Your normlized cell data file has been saved in: ", getwd())


#######################################STEP 2 - THRESHOLDING#####################################################
#1 Decide your thresholding strategy. 
#Available strategies are:
##"EBI_Otsu" => Classic Otsu thresholding from the EBImage package

##"Kmeans" => Find the threshold by clustering the markers using K means clustering

##"Kmeans_Otsu" => A variant of classical Otsu method

##"Autothreshold" => It's not a method but a package that includes several methods
#User should specify the Method_autothreshold argument (one of "IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean",
#"MinErrorI", "Minimum", "Moments", "Otsu", "RenyiEntropy", "Shanbhag", "Triangle" or "Yen")
#Some of these do not work very well with 0 enriched markers

##"TriClass_Otsu" => A variant of classic Otsu
#User should specify the number_iterations_TriClass argument (a number specifying the max number of iterations to be calculated)

##"Mean" => Threshold according to average intensity

##"Quantile" => Threshold according to quantile
#User should specify the Percentile argument (a number between 0 and 1 specifying the percentile to use in the thresholding procees)

##"Arbitrary" => Threshold based on an arbitrary limit stablished by the user

##"Multi_level" => Threshold in several levels
#User should specify the Levels argument (a number specifying the number of levels)


#2 Decide if you are will calculate a Local threshold (by slide) or global threshold (by experiment). 
#Local threshold are discouraged as they may be subjected to residual batch effect after normalization
#User should specify the Local_thresholding argument (TRUE if local and FALSE if global)

#In order to explore all these options, you can execute the following shiny APP
Thresholding_tester_app(DATA = DATA_normalized, #Source data to be analyzed
                        Directory = "My_images_directory", #Directory were Multi-Tiff images are stored
                        Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK") #Order of the image channels in the multi-TIFF
)

#3 Perform thresholding using the Thresholding_function. 
#This functions if you want to use the same strategy for every marker (which is not always the case)
DATA_thresholded <- Thresholding_function(DATA = DATA_normalized,
                                          Strategy = "EBI_Otsu", 
                                          Local_thresholding = FALSE, 
                                          Method_autothreshold = "Otsu",
                                          number_iterations_TriClass = 20,
                                          Percentile = 0.5,
                                          Defined_threshold = 0.1,
                                          Levels = 3
)


# You can also apply a specific thresholding method for each of the variables in your tibble
# First construct a tibble with 7 columns (variable, Strategy, Method_autothreshold, Local_thresholding, number_iterations_TriClass, Percentile and Levels)
# Each row specifies the variable to be thresholded and the method used to threshold it
Variables_tibble <- tibble(variable = names(DATA_normalized)[-(1:4)], #Specify the names of the variables of the analysis
                           Strategy = c("EBI_Otsu", "EBI_Otsu", "Arbitrary", "EBI_Otsu", "EBI_Otsu"), #Specify the strategy for each analysis
                           Local_thresholding = F, #Specify if the function is global or local for each variable
                           Method_autothreshold = F, #Specify the Method_autothreshold for each variable
                           number_iterations_TriClass = F, #Specify the number of iterations TriClass for each variable
                           Percentile = 0.5, #Specify the required percentile for each variable
                           Defined_threshold = c(FALSE, FALSE, 1, FALSE, FALSE), # Specify an absolute threshold if required for each variables
                           Levels = NA #Specify the levels of multilevel thresholding for each variable
)

View(Variables_tibble)

DATA_thresholded <- Thresholding_function_tailored(DATA = DATA_normalized, #Specify the data
                                                    Variables_tibble = Variables_tibble #Provide the tibble with the arguments
)



#4 Calculate the number of cells above the threshold and cell densities for each marker
#First We will remove the DAPI channel for the following analyses
DATA_thresholded <- DATA_thresholded %>% dplyr::select(-DAPI_q095)

#Now we calculate
Thresholding_results <- Thresholding_exploration_function(DATA = DATA_thresholded, #Specify the thresholded data
                                                          Calculate_Density = TRUE, #Select if densities need to be calculated
                                                          DATA_Area = DATA_AREA #Provide the data where areas are specified
)

#If you need to export your results to a CSV file execute the following code 
write.csv(Thresholding_exploration_function(DATA_thresholded), "N_cells_above_threshold.csv", row.names = F)
paste0("Your absolute cell number data file has been saved in: ", getwd())

#5 Calculate the actual threshold for each of the markers and plot the result (works with global thresholding)
Thresholding_summary_function(DATA = DATA_normalized, #Specify the original data
                              DATA_thresholded = DATA_thresholded #Specify the thresholded data
)

#6 Plot invididual marker intensisties and classified as positive
Thresholding_graphicator_function(DATA = DATA_normalized, #Provide Marker intensity data
                                  DATA_thresholded = DATA_thresholded, #Provide thresholded data 
                                  Marker_names = names(DATA_thresholded)[-c(1:4)], #Provide a list of markers to include
                                  Image_name = "Endo_IG09_T_Core[1,1,B]" #Provide the name of the image to analyze
)

#7 Correlate your data with clinical findings using the Clinical_Data_analyzer function
Clinical_Data_analyzer(DATA = Thresholding_results, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("PROP_CD68_q095", "PROP_CD8_q095", "PROP_FOXP3_q095"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "MMRP_status", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = F, #Specify if there is going to be a time to event analysis performed
                       Time_variable = NULL, #If time to event is going to be performed supply adequate time variable 
                       Event_variable = NULL #If time to event is going to be performed supply an outcome variable
)

Clinical_Data_analyzer(DATA = Thresholding_results, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("PROP_CD68_q095", "PROP_CD8_q095", "PROP_FOXP3_q095"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = NULL, #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #If time to event is going to be performed supply an outcome variable
)


#8 Correlate the number of different positive cells across samples
Plot_correlation_matrix(DATA = Thresholding_results, #Provide sample information data
                        Variables_included = c("PROP_CD68_q095", "PROP_CD8_q095", "PROP_FOXP3_q095", "PROP_CK_q095"),#Provide the markers to be correlated
                        Correlation_method = "pearson" #Select one of "pearson", or "spearman"
)


#######################################STEP 3 - PHENOTYPING#####################################################

#1 Decide whether you want to follow a user specified approach or a clustering approach 

##For a user specified approach first you need to explore all the combinations 
Phenotype_possibilities <- Marker_combinator_generator(DATA = DATA_thresholded, #Select your thresholded data 
                                                       Markers = names(DATA_thresholded)[-c(1:4)] #select the markers included
                                                       )

##Explore your results
View(Phenotype_possibilities)

##Complete the data frame with your desired phenotypes (given in order)
Phenotype_possibilities$Phenotype <- c("TUMOR", #Cell_type_1
                                       "OTHER", #Cell_type_2
                                       "MACROPHAGE", #Cell_type_3
                                       "MACROPHAGE", #Cell_type_4
                                       "CD8", #Cell_type_5
                                       "CD8", #Cell_type_6
                                       "CD8", #Cell_type_7
                                       "CD8", #Cell_type_8
                                       "MACROPHAGE", #Cell_type_9
                                       "CD8", #Cell_type_10
                                       "CD8", #Cell_type_11
                                       "MACROPHAGE", #Cell_type_12
                                       "TREG", #Cell_type_13
                                       "TREG", #Cell_type_14
                                       "CD8", #Cell_type_15
                                       "CD8") #Cell_type_16

##Explore your results
View(Phenotype_possibilities)

##Perform phenotyping
DATA_Phenotypes <- Phenotype_assigner_function(DATA = DATA_thresholded, #Specify your source data
                                               Phenotype_possibilities = Phenotype_possibilities #Specify your matched phenotypes
)


##Alternatively you can use a clustering approach straight from your marker intensity data. We will try a Graph-Based approach
DATA_Phenotypes_2 <- Clustering_Phenotyper(DATA = DATA_normalized %>% dplyr::select(-contains("DAPI")), #Marker intensity data as obtained from the 
                                         Strategy = "Graph_Based", #Decide one of the methods from Consensus_Clustering, SOM, Graph_Based
                                         #K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering
                                         
                                         #Denoising parameters
                                         Apply_Denoise = FALSE, #Specify if a denoising filtering is required before clustering
                                         Denoising = NULL, #Select denoising strategy from: Quantile, Standard_Deviation, Threshold, Otsu or DimRed_DBscan
                                         Percentile = NULL, #Select the adequate percentile for quantile threshold
                                         N_Standard_Deviations = NULL, #Select the number of standard deviations from mean for Standard_Deviation method
                                         Selected_threshold = NULL, #Select the absolute threshold for the Threshold method
                                         Min_cell_no = NULL, #Parameter for DBscan
                                         Distance_radius = NULL, #Parameter for DBscan
                                         
                                         #Dimension reduction
                                         Perform_Dimension_reduction = FALSE, #Should Dimension Reduction be performed
                                         Dimension_reduction = NULL, #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
                                         Dimension_reduction_prop = NULL, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
                                         Cluster_on_Reduced = F, #Should subsequent clustering be performed on reduced data
                                         
                                         #Parameters for Consensus Clustering
                                         Max_N_phenotypes = 5, #Number of maximum neighborhods that you desire to find
                                         Consensus_reps = 3, #Number of iterations of the algorithm to try to converge
                                         Consensus_p_Items = 0.9, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
                                         Consensus_Cluster_Alg = "km", #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
                                         Consensus_Distance = "euclidean", #Distance metric to be used (pearson(1 - Pearson correlation), spearman(1 - Spearman correlation), euclidean, binary, maximum, canberra, minkowski
                                         Consensus_Name = "ConsensusClustering", #Name of the folder that is going to be created in order to place the resulting graphs
                                         
                                         #Parameters for Self-Organizing Maps
                                         Max_SOM_phenotypes = 5, #Maximum number of clusters (phenotypes) to try in the algorithm
                                         
                                         #Parameters for Graph-Based approaches
                                         Nearest_neighbors_for_graph = 10, #Specify the number of closest neighbors to calculate the graph
                                         Graph_Method = "Leiden", #One of Louvain, Leiden, Greedy, WalkTrap, Spinglass, Leading_Eigen or Edge_Betweenness
                                         Graph_Resolution = 0.2, #Used for Louvain and Leiden. 1 is default. The smaller the value, the larger the clusters will be
                                         N_steps = 5, #Number of steps given in the WalkTrap algorithm
                                         
                                         #Parameters for K means Meta Clustering
                                         N_K_centroids = 200, #Number of centroids to perform K means
                                         Max_N_phenotypes_Meta = 5, #Number of maximum neighborhods that you desire to find
                                         Consensus_reps_Meta = 5, #Number of iterations of the algorithm to try to converge
                                         Consensus_p_Items_Meta = 0.95, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
                                         Consensus_Name_Meta = "Metaclustering", #Name of the folder that is going to be created in order to place the resulting graphs
                                         
                                         #Parameters for Batched K means
                                         Batch_size = 1000, #The number of cells to be included in each random batch
                                         Max_N_phenotypes_Batch = 5, #Number of maximum clusters (phenotypes) that you desire to find
                                         N_initiations = 5, #Number of times the algorithm is going to be tried to find the best clustering result
                                         Max_iterations = 10, #Max number of iterations in each try
                                         
                                         #Parameters for Gaussian Mixture Model
                                         Quality_metric = "AIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
                                         Max_N_phenotypes_GMM = 10, #Number of maximum clusters (phenotypes) that you desire to find
                                         Max_iterations_km = 15, #Number of max iterations in the K means clustering performed
                                         Max_iterations_em = 15, #Number of max iterations in the Expectation Maximization algorithm
                                         GMM_Distance = "eucl_dist", #Distance metric to use in the model ("eucl_dist" or "maha_dist")
                                         
                                         #Parameters for CLARA clustering
                                         Samples_CLARA = 5, #Number of samples the CLARA algorithm is going to use to be calculated
                                         Sample_per_CLARA = 0.5, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
                                         Max_N_phenotypes_CLARA = 5, #Number of maximum clusters (phenotypes) that you desire to find
                                         Distance_CLARA = "euclidean", #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
                                         #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
                                         N_cores = 2 #Number of cores to parallelize your computation
)


#We use the Data_Phenotype_renamer to assign a new name to these phenotypes (REQUIRED FOR CLUSTERING PHENOTYPING)
DATA_Phenotypes_2 <- DATA_Phenotype_renamer(DATA = DATA_Phenotypes_2, #Specify the data created previously
                                          New_names = c("TREG", "TUMOR", "CD8", "MACROPHAGE", "OTHER", "TUMOR", "OTHER", "TUMOR") #Provide the new names in order (Phenotype 1, 2, 3...)
)


#You can also apply the CELESTA algorithm (see https://github.com/plevritis-lab/CELESTA )
#In CELESTA you generate phenotypes based on expected marker expression
#First create a csv template to fulfill according to the markers desired to be used in the phenotyping process
CELESTA_template_generator(DATA = DATA_normalized,
                           Markers_to_keep = c("CD8_q095", "FOXP3_q095", "CD68_q095", "CK_q095"),
                           Template_name = "My_template")

#Then execute the CELESTA algorithm
DATA_Phenotypes_3 <- CELESTA_phenotyper(DATA = DATA_normalized, #DATA containing the marker expression data
                                      Template_path = "My_template2.csv", #Path to the template
                                      Alternative_CSV_locale = F, #Should a European locale be used when importing the template?
                                      N_cores = 4, #Number of cores to parallelize your computation
                                      
                                      Apply_filters = TRUE, #Should cells be filtered before runing the algorithm
                                      high_marker_threshold = 0.90, #Cells with expression above this quantile for all markers will not be phenotyped
                                      low_marker_threshold = 0.10, #Cells with expression below this quantile for all markers will not be phenotyped
                                      
                                      max_iteration = 10, #Max number of iterations to run the GMM algorithm unterlying CELESTA
                                      cell_change_threshold = 0.01 #when the iterative cell-type assignment stops. 
                                      #The default value is 0.01, 
                                      #which means that if the percentage of additional assigned cells is smaller than 1% of the unassigned cells, then cell-type assignment will stop. 
                                      #The recommended range is 0.01 - 0.05. 
                                      #Note that the higher the cell change threshold, the more cells are left unassigned.
)



###User can explore the phenotyping results using the following Shiny app louncher
Phenotyping_evaluator_shiny_app_launcher(DATA = DATA_Phenotypes, #Source data to be analyzed
                                         Directory = "My_images_directory", #Directory were Multi-Tiff images are stored
                                         Ordered_Channels = c("DAPI", "PDL1", "CD8", "FOXP3", "CD68", "PD1", "CK", "AUTO") #Order of the image channels in the multi-TIFF
)


##You can explore different phenotyping processes between them using the rand index
##You need to introduce as arguments DATA sources containing a variable to be compared with other DATA sources
##DATA sources must be named. User can work with as many DATA sources as desired
Concordance_calculator(Threshold = DATA_Phenotypes, 
                       GMM = DATA_Phenotypes_2,  
                       Variable = "Phenotype", #Choose the variable name that contains the phenotypes
                       Strategy = "Rand" #Choose betweeen Rand and FM (Fowlkes-Mallows)
)

##You may need to export your phenotype data
write.csv(DATA_Phenotypes, "ENDOMETRIAL_CANCER_CELLS_with_phenotypes.csv", row.names = F)
paste0("Your cells with assigned phenotypes data has been saved in: ", getwd())

##Quantify your phenotypes by sample
Phenotypes_by_Sample <- Phenotype_quantifier(DATA = DATA_Phenotypes, #Specify the data frame you want to use
                                             Calculate_Density = TRUE, #Specify if density of cell types should be calculated
                                             DATA_Area = DATA_AREA #Provide area data if cell density needs to be calculated
)

##Correlate your phenotypes with clinical data
Clinical_Data_analyzer(DATA = Phenotypes_by_Sample, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Density_CD8", "Density_MACROPHAGE", "Density_TREG"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "LVI", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = F, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "Months_until_Progression", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "outcome" #and outcome variable
)

##Correlate your phenotypes with clinical data
Clinical_Data_analyzer(DATA = Phenotypes_by_Sample, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Density_CD8", "Density_MACROPHAGE", "Density_TREG"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "DEATH", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)

#You may need to export your number of phenotypes by sample result
write.csv(Phenotypes_by_Sample, "DATA_counts_phenotypes.csv", row.names = F)
paste0("Your phenotype count data has been saved in: ", getwd())

##Graph your results
Barplot_generator(Phenotypes_by_Sample, 
                  Phenotypes_included = c("TUMOR", "MACROPHAGE", "CD8", "TREG"), #Which phenotypes should be included in the graph
                  Ordering_phenotype = "CD8")#Which phenotypes should guide the barplot ordering

##Graph individual images
Image_plotter(DATA = DATA_Phenotypes, #Your phenotype data source
              Image_name = "Endo_IG09_T_Core[1,1,B]", #Name of the image to be graphicated
              Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG") #which phenotypes should be included in the graph
)

#You may plot the image and the cells generating an overlay
Cell_image_plot_generator(Image_directory = "Endo_IG09_T_Core[1,1,B].tif", #Image path
                          Channel_to_display = 1, #Channel index to be displayed
                          Image_rotate = NULL, #Rotate image, either NULL or a numeric value between 0 and 360
                          Image_x_flip = FALSE, #Flip image horizontally
                          Image_y_flip = TRUE, #Flip image vertically
                          Gamma_level = 0, #Gamma level (between -3 and +3)
                          Equalize = FALSE, #Should image be equalized
                          Black_level = 0, #Value between 0 and 100
                          White_level = 100, #Value between 0 and 100
                          
                          DATA = DATA_Phenotypes, #Cell data
                          Image_name = "Endo_IG09_T_Core[1,1,B]", #Image to be displayed
                          Color_by = "Phenotype", #Either NULL or the column used to color the cells 
                          Point_size = 1, #Size of the points
                          Pixel_distance_ratio = NULL #Pixel to distance ratio to be used when cells coordinates are not in pixel units
)

#Analyze correlation of density from each cell type
Plot_correlation_matrix(DATA = Phenotypes_by_Sample, #Provide sample information data
                        Variables_included = c("Density_CD8", "Density_MACROPHAGE", "Density_TUMOR", "Density_TREG", "Density_OTHER"),#Provide the markers to be correlated
                        Correlation_method = "pearson" #Select one of "pearson", or "spearman"
)

#Analyze expression of functional markers (set aside in STEP 0) with your phenotypes
Cell_functional_assessment(DATA = DATA_Phenotypes, #Provide the DATA with data phenotypes
                           Target_Variable = "Phenotype", #Specify the name of the variable where phenotypes are included
                           Targets_Included = c("TUMOR"), #Specify the phenotypes to be analyzed
                           DATA_Aside = DATA_aside, #Provide the DATA with the functional markers
                           Threshold_functional_Markers = T, #Specify if multi-level thresholding should be performed on functional markers
                           Levels = 3 #Specify the number of levels to split the functional markers into
)

#Read your phenotypes from a local file
DATA_Phenotypes <- readr::read_delim("TEST_DATASETS/DATA_with_phenotypes.csv", delim = ",")#Paste your data pathway and select delimiter ("," is the default)

#######################################STEP 4 - HETEROGENEITY ANALYSIS#####################################################
#1 First you need to decide your approach

##2 Global approach
#Calculate global heterogeneity.
Global_Heterogeneity_by_sample <- Global_heterogeneity_calculator(DATA = DATA_Phenotypes,#Specify your data containing the phenotypes
                                  Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG", "OTHER")#Select the phenotypes that will be used in the calculation
  )

#Shannon index - The larger the more diverse
#Simpson - The smaller the more diverse
#Inverse Simpson - The larger the more diverse
#Renji entropy - The larger the more diverse
#Rao-Dkk Within-community diversity
#Gini index - The larger the more inequal (less diverse)
#KL_Sample - Kullback–Leibler divergence index compared to even cell distribution
#JS_Sample - Jensen_Shannon divergence index compared to even cell distribution
#KL_Experiment - Kullback–Leibler divergence index compared to experiment-wise cell distribution
#JS_Experiment - Jensen_Shannon divergence index compared to experiment-wise cell distribution

#Export your global heterogeneity results
write.csv(Global_Heterogeneity_by_sample, "Global_heterogeneity_results.csv", row.names = F)
paste0("Your global heterogeneity results have been saved in: ", getwd())

#Generate a Barplot of the desired result
Barplot_Heterogeneity_generator(DATA = Global_Heterogeneity_by_sample, #Specify your data source
                                Metric = "Shannon" #Specify the metric to be visualized
)
#Correlate with clinical data
Clinical_Data_analyzer(DATA = Global_Heterogeneity_by_sample, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Shannon", "Simpson", "Gini"),
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "DEATH", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)

Clinical_Data_analyzer(DATA = Global_Heterogeneity_by_sample, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Shannon", "Simpson", "Gini"),
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "LVI", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = F, #Specify if there is going to be a time to event analysis performed
                       Time_variable = F, #If time to event is going to be performed supply adequate time variable 
                       Event_variable = F #and outcome variable
)


##3 Local approach
#Obtain information on the image sizes of the experiment
Image_length_calculator(DATA = DATA_Phenotypes)

#Calculate an adequate tile size
Suggested_Tile_Size_Calculator(DATA_Phenotypes, #Provide a data phenotypes object 
                               N_cols = 15, #Provide the number of desired columns 
                               N_rows = 15, #Provide the number of desired rows
                               Based_on_smaller = T, #Whether to base calculation on the smallest or the largest image
                               Draw_preview = T #Specify if you want a preview to be graphicated
)

#Tile your images according to your required parameters
Tiled_Images_phenotypes <- Image_tiling_processing_function(N_cores = 2, #Number of cores to parallelize your computation
                                                 DATA = "DATA_Phenotypes", #Name of the DATA_phenotype object to be used in the analysis
                                                 Tile_width = 150, #Width of your tiles
                                                 Tile_height = 150, #height of your tiles
                                                 Variables_to_keep = "Phenotype" #Names of the variables to be preserved in the tiling process
)

#Calculate heterogeneity by Tile
Heterogeneity_by_tile <- Tiled_image_heterogeneity_calculator(Tiled_images = Tiled_Images_phenotypes, #Name of the object containing the tiled Images
                                                              Minimum_cell_no_per_tile = 5, #Minimum number of cells a tile should contain to be included in the analysis
                                                              Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG", "OTHER") #The phenotypes that must be taken into account
)

#Graph your Heterogeneity results
Tiled_image_heterogeneity_graph_maker(DATA = DATA_Phenotypes, #specify the Data to obtain your cell info
                                      Tiled_images = Heterogeneity_by_tile, #Specify the heterogeneity result DATA
                                      Image_name = "Endo_IG09_T_Core[1,1,B]", #Name of the image to be plotted
                                      Metric = "Shannon" #Name of the metric to be plotted
)

#Analyze your heterogeneity results
Heterogeneity_by_tile_Analysis <- Tiled_image_heterogeneity_analyzer(
  Tiled_heterogeneity_DATA = Heterogeneity_by_tile, #Specify the heterogeneity data to be analyzed
  Strategy = "Overall_Summary", #Decide between two different options
  #Quantify_by_Threshold => Calculates number and proportion of tiles above a pre-set threshold
  #                       as well as Spatial Auto Correlation of heterogeneous tiles (Morans I)
  #Overall_Summary => Calculates dispersion and central tendency metrics 
  #                   for each image as well as Spatial Auto Correlation metrics of heterogeneous tiles (Morans I)
  Metric = "Shannon", #Choose the heterogeneity measure to be analyzed 
  Threshold = 0.99 #For the quantify_by_Threshold approach select an adequate threshold to analyze the tiles
) 


#You can calculate the heterogeneity using texture analysis of the image
#First you may explore the distribution of your cell types accros samples
Tiled_images_graphicator(Tiled_images = Tiled_Images_phenotypes, #Provide a tiled image list
                         Image_name = "Endo_IG09_T_Core[1,1,B]", #Specify the image you want to analyze
                         Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG", "OTHER") #Specify the cell types to be analyzed
)

#Now calculate the texture features of a specific cell type accross images
Texture_features_DATA <- Texture_features_calculator(Tiled_images = Tiled_Images_phenotypes, #Provide a tiled image list
                                                     Phenotype_included = c("TUMOR") #Specify the phenotype to be analyzed (one per analysis)
)


#Correlate with clinical data
Clinical_Data_analyzer(DATA = Heterogeneity_by_tile_Analysis, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Average", "p50", "Max_value", "MoransI_obseved"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "MMRP_status", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = F, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "Outcome_time", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "Outcome_event" #and outcome variable
)

Clinical_Data_analyzer(DATA = Heterogeneity_by_tile_Analysis, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Average", "p50", "Max_value", "MoransI_obseved"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "DEATH", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)

#Alternatively perform SPIAT analysis. Its a local analysis based on tiling of the images
#First we need to edit our data into SPIAT type objects
DATA_SPIAT <- SPIAT_object_generator(DATA_Intensities = DATA, #Specify original data with marker intensity information
                                     DATA_Phenotypes = DATA_Phenotypes #Specify data with phenotype information
)

#Then we can calculate the spatial heterogeneity 
SPIAT_Heterogeneity_Analyzer(N_cores = 1, #Number of cores to parallelize your computation
                             DATA_SPIAT = DATA_SPIAT, #Provide the list of SPIAT objects
                             DATA_Phenotypes = DATA_Phenotypes, #Provide a data phenotype object
                             Tile_size = 150, #Provide the approximate size of the square tiles to be used
                             Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG", "OTHER"), #Specify the cell phenotypes to be included in the analysis
                             Entropy_threshold = 0.5, #Specify the heterogeneity threshold to use in the analysis
                             Autocorrelation_metric = "globalmoran") #Specify the autocorrelation metric "globalmoran" or "GearyC"



#######################################STEP 5 - SIMPLE CELL TO CELL ANALYSIS#####################################################
#Calculate the cell to cell distance matrix
DATA_Distances <- Distance_matrix_generator(N_cores = 2, #Number of cores to parallelize your computation
                                            DATA = "DATA_Phenotypes",#Name of the DATA_phenotype object to be used in the analysis
                                            Cell_Of_Origin = "TUMOR", #Name of the cell of origin of the analysis
                                            Target_Cell = "CD8", #Name of the target cell of the analysis
                                            Allow_Cero_Distance = F, #Specify if completely colocallized cells are allowed in the analysis (impact on self to self distance analysis)
                                            Perform_edge_correction = TRUE, #Should Cells of Origin close to sample borders be removed before analysis?
                                            Hull_ratio = 1, #Hull_ratio to calculate sample borders
                                            Distance_to_edge = 50 #Distance from the border
                                            )


#You may need to calculate a RANDOM background of cells
RANDOM_Distances <- Random_Distance_matrix_generator(N_cores = 2, #Number of cores to parallelize your computation
                                                     DATA = "DATA_Phenotypes", #Name of the DATA_phenotype object to be used in the analysis
                                                     Cell_Of_Origin = "TUMOR", #Name of the cell of origin of the analysis
                                                     Target_Cell = "CD8", #Name of the target cell of the analysis
                                                     Random_cells_per_sample = 10, #Number of random cells to be generated in the analysis
                                                     Allow_Cero_Distance = F, #Specify if completely colocallized cells are allowed in the analysis (impact on self to self distance analysis)
                                                     Perform_edge_correction = TRUE, #Should Cells of Origin close to sample borders be removed before analysis?
                                                     Hull_ratio = 1, #Hull_ratio to calculate sample borders
                                                     Distance_to_edge = 50 #Distance from the border
                                                     )

#Calulate your cumulative interaction data
DATA_Cumulative_Interaction <- Cumulative_Interaction_generator(N_cores = 2, #Number of cores to parallelize your computation
                                                                DATA = "DATA_Distances", #Name of the DATA_distance object to be used in the analysis
                                                                Start_from = 10, #Starting distance to calculate the cumulative interaction function
                                                                Stop_at = 100, #Final distance where cumulative interaction analysis will stop
                                                                Sampling_frequency = 10 #Sampling distance interval
)


#You may need to calculate a RANDOM cumulative interaction data
RANDOM_Cumulative_Interaction <- Cumulative_Interaction_generator(N_cores = 2, #Number of cores to parallelize your computation
                                                                  DATA = "RANDOM_Distances",#Name of the DATA_distance object to be used in the analysis
                                                                  Start_from = 0, #Starting distance to calculate the cumulative interaction function
                                                                  Stop_at = 50, #Final distance where cumulative interaction analysis will stop
                                                                  Sampling_frequency = 25 #Sampling distance interval
)

#Start analyzing your Cell to Cell data
DISTANCE_analysis <- Distance_analyzer(N_cores = 2, #Number of cores to parallelize your computation
                                       DATA = DATA_Distances, #Provide the data containing the distances
                                       DATA_RANDOM = RANDOM_Distances, #Provide data containing the random distances (optional)
                                       Metric = "Average_Distance", #Select the type of analysis between: Average_Distance, Min_Distance, Max_Distance
                                       Include_Random = T, #Decide if Random Data should be used to calculate significance level
                                       By_Sample_Random = T #Decide if Random distribution should be calculated by sample (T) or by experiment (F)
)

RADIUS_analysis <- Cells_in_Radius_analyzer(DATA = DATA_Cumulative_Interaction, #Provide the data containing the cumulative interaction distances
                                            DATA_RANDOM = RANDOM_Cumulative_Interaction, #Provide data containing the random cumulative interactions (optional)
                                            Radius = 50, #Select de radius of the analysis (must be contained in the Cumulative interaction sampling)
                                            Include_Random = T, #Decide if Random Data should be used to calculate significance level
                                            By_Sample_Random = T #Decide if Random distribution should be calculated by sample or by experiment
)


#Correlate with clinical data
Clinical_Data_analyzer(DATA = DISTANCE_analysis, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Average_Distance", "Random"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "LVI", #Specify the clinical outcome to be evaluated 
                       Perform_time_to_event = F, #Specify if there is going to be a time to event analysis performed
                       Time_variable = NULL, #If time to event is going to be performed supply adequate time variable 
                       Event_variable = NULL #and outcome variable
)


Clinical_Data_analyzer(DATA = RADIUS_analysis, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Average_cells", "Random"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = F, #Specify the clinical outcome to be evaluated 
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)

#Graph your results
Cell_to_Cell_graph_maker(Image_name = "Endo_IG09_T_Core[1,1,B]", #Provide the image name to be plotted
                         DATA_Phenotypes = DATA_Phenotypes, #Provide the data phenotypes information
                         Strategy = "Cells_in_Radius", #one of "Min_Distance", "Max_Distance", "Average_Distance", "Cells_in_Radius"
                         DATA_Distances = DATA_Distances, #Provide a DATA_Distances object if Min, Max or Average_Distance need to be calculated
                         DATA_Cumulative = DATA_Cumulative_Interaction, #Provide a DATA_Cumulative_Interaction object for Cells_in_Radius analysis
                         Radius = 50 #Specify the radius to be graphicated
)

#Alternatively you can perform certain cell to cell analyses without requiring the previous computations
#Gcross function calculates the probability of encountering a single target cells accross a radius of distance from the cell of origin
#The function returns AUC data of the function calculated
Gcross_Analysis <- Gcross_calculator(DATA = DATA_Phenotypes, #Provide the data containing the Cell Type information
                                     Cell_Of_Origin = "TUMOR", #Name of the cell of origin of the analysis
                                     Target_Cell = "CD8", #Name of the target cell of the analysis
                                     Stop_at = 150, #Final distance where cumulative interaction analysis will stop
                                     Sampling_frequency = 10, #Sampling distance interval
                                     Use_Clinical = T, #Specify if you desire to use clinical information in your pipeline
                                     DATA_Clinical = DATA_CLINICAL, #Provide clinical information if desired
                                     Clinical_var = "MMRP_status" #Specify the name of the variable to be used for clinical information analysis
)


Clinical_Data_analyzer(DATA = Gcross_Analysis$Simplified_DATA, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = "AUC", #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "MMRP_status", #Specify the clinical outcome to be evaluated 
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)



#Ripleys K and L function calculate the spatial aggregation of cells and compares it to Complete Spatial Randomness (CSR) situation
#This function can be troublesome to compute and therefore it may be necessary to try different parameters
Ripley_function_calculator(DATA = DATA_Phenotypes, #Provide the data containing the Cell Type information
                           Cell_type = "CD8", #Name the cell of origin of the analysis
                           Max_distance = 1,#Maximum distance to calculate Ripleys function
                           Strategy = "Ripleys_K", #Strategy, choose between Ripleys_K and Ripleys_L
                           N_simulations = 10 #Number of simulations to calculate the confidence intervals of Ripleys function
)

#We can fit a multi-level model to analyze the cumulative interaction patterns of individual COO with target cells according to a clinical variable
#The clinical variable should be a character vector
#The multilevel model is always adjusted by the target cell density
Results_interim <- Multi_level_modelling_function(DATA_cumulative = DATA_Cumulative_Interaction, #Provide data containing the cumulative interaction information of COO and Target cells
                                                  DATA_Clinical = DATA_CLINICAL, #Provide clinical data
                                                  Clinical_var = "MMRP_status", #Specify the clinical Variable to be analyzed
                                                  DATA_Densities = Phenotypes_by_Sample, #Provide data containing the cell densities
                                                  Cell_Of_Origin = "TUMOR", #Specify the COO
                                                  Target_cell = "CD8", #Specify the target cell
                                                  Calculate_R2 = FALSE, #Should R2 be calculated
                                                  N_bootstrap = 10 #If R2 is calculated, select the number of bootstrap sub samples to be used in the calculation
)


#TRIO ANALYSIS 
#For TRIO analysis the user first needs to select a single COO and two target cells
#The COO will serve as a reference and the analysis will be based on the simultaneous interaction of these COO with the two target cells
#Calculate the COO distance matrix to the 2 other target cells
Trio_DATA_Distances <- Trio_Distance_matrix_generator(N_cores = 2, #Number of cores to parallelize your computation
                                                      DATA = "DATA_Phenotypes", #Name of the DATA_phenotype object to be used in the analysis
                                                      Cell_Of_Origin = "CD8",#Name of the cell of origin of the analysis
                                                      Target_Cell_1 = "MACROPHAGE", #Name of the first target cell of the analysis
                                                      Target_Cell_2 = "TREG", #Name of the second target cell of the analysis
                                                      Perform_edge_correction = TRUE, #Should Cells of Origin close to sample borders be removed before analysis?
                                                      Hull_ratio = 1, #Hull_ratio to calculate sample borders
                                                      Distance_to_edge = 50 #Distance from the border
)


#Calculate the RANDOM COO distance matrix to the 2 other target cells
Trio_RANDOM_Distances <- Trio_Random_Distance_matrix_generator(N_cores = 4, #Number of cores to parallelize your computation
                                                               DATA = "DATA_Phenotypes", #Name of the DATA_phenotype object to be used in the analysis
                                                               Cell_Of_Origin = "CD8", #Name of the cell of origin of the analysis
                                                               Target_Cell_1 = "MACROPHAGE", #Name of the first target cell of the analysis
                                                               Target_Cell_2 = "TREG", #Name of the second target cell of the analysis
                                                               Random_cells_per_sample = 10, #Number of random cells to be generated in the analysis
                                                               Perform_edge_correction = TRUE, #Should Cells of Origin close to sample borders be removed before analysis?
                                                               Hull_ratio = 1, #Hull_ratio to calculate sample borders
                                                               Distance_to_edge = 50 #Distance from the border
)

#Calulate your cumulative interaction data
Trio_DATA_Cumulative_Interaction <- Trio_Cumulative_Interaction_generator(N_cores = 5, #Number of cores to parallelize your computation
                                                                          DATA = "Trio_DATA_Distances", #Name of the DATA_distance object to be used in the analysis
                                                                          Start_from = 10, #Starting distance to calculate the cumulative interaction function
                                                                          Stop_at = 100, #Final distance where cumulative interaction analysis will stop
                                                                          Sampling_frequency = 10 #Sampling distance interval
)


#You may need to calculate a RANDOM cumulative interaction data
Trio_RANDOM_Cumulative_Interaction <- Trio_Cumulative_Interaction_generator(N_cores = 2, #Number of cores to parallelize your computation
                                                                            DATA = "Trio_RANDOM_Distances", #Name of the DATA_distance object to be used in the analysis
                                                                            Start_from = 10, #Starting distance to calculate the cumulative interaction function
                                                                            Stop_at = 100, #Final distance where cumulative interaction analysis will stop
                                                                            Sampling_frequency = 10 #Sampling distance interval
)


#Start analyzing your Cell to Cell data
Trio_DISTANCE_analysis <- Trio_Min_Distance_analyzer(N_cores = 5, #Number of cores to parallelize your computation
                                                     DATA = Trio_DATA_Distances,#Provide the data containing the distances
                                                     DATA_RANDOM = Trio_RANDOM_Distances,#Provide data containing the random distances (optional)
                                                     Include_Random = T, #Decide if Random Data should be used to calculate significance level
                                                     By_Sample_Random = T #Decide if Random distribution should be calculated by sample or by experiment
)

Trio_Radius_analysis <- Trio_Cells_in_Radius_analyzer(DATA = Trio_DATA_Cumulative_Interaction,#Provide the data containing the distances
                                                      DATA_RANDOM = Trio_RANDOM_Cumulative_Interaction,#Provide data containing the random distances (optional)
                                                      Radius = 50, #Radius to include in the analysis
                                                      Include_Random = T, #Decide if Random Data should be used to calculate significance level
                                                      By_Sample_Random = F #Decide if Random distribution should be calculated by sample or by experiment
)


#Correlate with clinical data
Clinical_Data_analyzer(DATA = Trio_DISTANCE_analysis, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Min_dist_to_TRIO", "Random"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "MMRP_status", #Specify the clinical outcome to be evaluated 
                       Perform_time_to_event = F, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)

Clinical_Data_analyzer(DATA = Trio_DISTANCE_analysis, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c("Min_dist_to_TRIO", "Random"), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "MMRP_status", #Specify the clinical outcome to be evaluated 
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)


#Graph your TRIO results
Trio_graph_maker(Image_name = "Endo_IG09_T_Core[1,1,B]", #Provide the image name to be plotted
                 DATA_Phenotypes = DATA_Phenotypes, #Provide the data phenotypes information
                 Strategy = "TRIO_in_Radius", #one of "Min_Distance" or"TRIO_in_Radius"
                 TRIO_Distances = Trio_DATA_Distances, #Provide a TRIO_DATA_Distances object if Min_Distance needs to be calculated
                 TRIO_Cumulative = Trio_DATA_Cumulative_Interaction, #Provide a TRIO_DATA_Cumulative_Interaction object for TRIO_in_Radius analysis
                 Radius = 50 #Specify the radius to be graphicated
)

#We can use the SPIAT approach to calculate aggregated entropy between cell types
#First edit data into SPIAT type objects
DATA_SPIAT <- SPIAT_object_generator(DATA_Intensities = DATA, #Specify original data with marker intensity information
                                     DATA_Phenotypes = DATA_Phenotypes #Specify data with phenotype information
)

SPIAT_entropy_gradient_generator(DATA_SPIAT = DATA_SPIAT, #Provide the list of SPIAT objects
                                 Gradient_start = 10, #Provide the starting distance to calculate the gradient
                                 Gradient_stop = 60, #Provide the max distance to calculate the gradient
                                 Gradient_sampling = 25, #Provide the sampling frequency to calculate the gradient
                                 Phenotypes_included = c("TUMOR", "CD8") #Specify two cell populations to calculate the gradient (first COO, then Target cell)
)

#Distance from cells to pixels in images can also be calculated
DATA_Phenotypes_distance <- Cell_to_pixel_distance_calculator(N_cores = 1, #Cores to parallelize computation
                                                              Directory = "My_directory_with_thresholded_images", #Path to thresholded images
                                                              Image_rotate = 90, #Should images be rotated before analysis (Either NULL or a numeric value between 0 and 360)
                                                              Image_x_flip = FALSE, #Should images be horizontally flipped before analysis
                                                              Image_y_flip = TRUE, #Should image be vertically flipped before analysis
                                                              DATA = DATA_Phenotypes, #DATA to be used
                                                              Phenotypes_included = unique(DATA_Phenotypes$Phenotype), #Phenotypes included in the computation
                                                              Pixel_distance_ratio = NULL #Distance ratio to be used when cell coordinates are not specified in pixels
)
DATA_Phenotypes_distance_segmented <- Marker_segmentator(DATA = DATA_Phenotypes_distance, #DATA 
                                                         DATA_variable = "PDL1_DIST", #Variable to be segmented
                                                         DATA_cutoff = c(0, 5, 20, 1000), #Cut off values
                                                         DATA_labels = c("Inside", "Border", "Outside"), #New labels (must be N of cut off - 1)
                                                         Merge = FALSE, #Should the new variable be merged with another variable
                                                         Var_to_Merge = "Phenotype" #Variable to be merged with
)

Cell_image_plot_generator(Image_directory = "Endo_IG09_T_Core[1,1,B].tiff", #Image path
                          Channel_to_display = 1, #Channel index to be displayed
                          Image_rotate = NULL, #Rotate image, either NULL or a numeric value between 0 and 360
                          Image_x_flip = FALSE, #Flip image horizontally
                          Image_y_flip = TRUE, #Flip image vertically
                          Gamma_level = 0, #Gamma level (between -3 and +3)
                          Equalize = FALSE, #Should image be equalized
                          Black_level = 0, #Value between 0 and 100
                          White_level = 100, #Value between 0 and 100
                          
                          DATA = DATA_Phenotypes_distance_segmented, #Cell data
                          Image_name = "Endo_IG09_T_Core[1,1,B]", #Image to be displayed
                          Color_by = "PDL1_DIST", #Either NULL or the column used to color the cells 
                          Point_size = 1, #Size of the points
                          Pixel_distance_ratio = NULL #Pixel to distance ratio to be used when cells coordinates are not in pixel units
)

#######################################STEP 6 - NEIGHBORHOOD ANALYSIS#####################################################

###ALTERNATIVE 1 CLOSEST NEIGHBORS
#Compute your closest neighbors. This is the grounds of closest neighbor neighborhood identification (like the one of Gary Nolan) 
#This may be computationally intensive
#Neighbors can be defined by distance, number or both

#For this demo, we'll try CLOSEST NEIGHBORS in a smaller subset
DATA1 <- DATA_Phenotypes %>% dplyr::filter(Subject_Names %in% unique(DATA_Phenotypes$Subject_Names)[1:5])

DATA_Closest_Neighbors <- 
  Tailored_Closest_neighbor_calculator(N_cores = 4, #Number of cores to parallelize your computation
                                       DATA = DATA1, #Provide the data containing the Cell Type information
                                       Strategy = "Distance", #Specify the strategy to consider two cells to be neighbors between Number, Distance, Both
                                       N_neighbors = 10, #Specify the number of closest neighbors to include in the analysis
                                       Include_COO_in_neighborhood = TRUE, #Specify if the Cell of Origin should be included in neighbor counting
                                       Max_dist_allowed = 100, #Specify the max Distance to define neighborhoods
                                       Cell_Of_Origin = "TUMOR", #Specify the cells of origin to include in the analysis
                                       Target_Cell = c("CD8", "MACROPHAGE", "TREG")) #Specify the target cells to include in the analysis

#Decide wether you want to use the Percentage or the Absolute neighbor count
DATA_Closest_Neighbors <- DATA_Closest_Neighbors$Percentage
DATA_Closest_Neighbors <- DATA_Closest_Neighbors$Absolute_count

#Since this is a computationally intensive task you may need to write your results at the end of the computation
write.csv(DATA_Closest_Neighbors, "Closest_Neighbors_DATA.csv", row.names = F)
paste0("Closest neighbor data patterns have been saved in: ", getwd())

#You may import a DATA_Closest_Neighbors object if you already have it
DATA_Closest_Neighbors <- read_csv("Closest_Neighbors_DATA.csv")

#Now we try to find the neighborhoods present in our samples, we will do this by clustering
DATA_neighborhoods <- Neighborhood_discovery_function(
  DATA = DATA_Closest_Neighbors, #Provide the data containing the Closest neighbor information
  Allowed_max_Dist = 100, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_avg_Dist = 100, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_median_Dist = 100, #Provide de threshold to eliminate cells whose neighbors are too far away
  
  #Dimension reduction
  Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
  Dimension_reduction = "TSNE", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
  Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
  Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
  
  #Clustering strategy
  Strategy = "SOM", #Choose your clustering method (either Consensus_Clustering, SOM (Self-Organizing Maps), Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
  
  #Parameters for Consensus Clustering
  Max_N_neighborhoods = 8, #Number of maximum neighborhods that you desire to find
  Consensus_reps = 20, #Number of iterations of the algorithm to try to converge
  Consensus_p_Items = 0.90, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
  Consensus_Cluster_Alg = "km", #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
  Consensus_Distance = "euclidean", #Distance metric to be used (pearson(1 - Pearson correlation), spearman(1 - Spearman correlation), euclidean, binary, maximum, canberra, minkowski
  Consensus_Name = "Neighborhood_test1", #Name of the folder that is going to be created in order to place the resulting graphs
  
  #Parameters for Self-Organizing Maps
  Max_SOM_neighborhoods = 10, #Maximum number of clusters (neighborhoods) to try in the algorithm
  
  #Parameters for Graph Based Clustering
  Graph_type = "SNN", #Decide the type of graph to build: it can be complete (more accurate but computationally intensive), SNN (nearest neighbor)
  Nearest_neighbors_for_graph = 20, #If closest neighbor graphs are calculated decide the number of closest neighbors to include
  Graph_Method = "Leiden", #One of Louvain, Leiden, Optimal, Greedy, WalkTrap, Spinglass, Leading_Eigen or Edge_Betweenness
  Graph_Resolution = 1, #Used for Louvain and Leiden. 1 is default. The smaller the value, the larger the clusters will be
  Graph_Distance_method = "euclidean", #Distance used to calculate the graph weight (euclidean, maximum, manhattan, canberra, binary or minkowski)
  N_steps = NULL, #Number of steps given in the WalkTrap algorithm
  
  #Parameters for K means Meta Clustering
  N_K_centroids = 1000, #Number of centroids to perform K means
  Max_N_neighborhoods_Meta = 10, #Number of maximum clusters (neighborhoods) that you desire to find
  Consensus_reps_Meta = 2, #Number of iterations of the algorithm to try to converge
  Consensus_p_Items_Meta = 1, #Percentage of cells that you desire to sample in each iteration
  Consensus_Name_Meta = "TEST", #Name of the folder that is going to be created in order to place the resulting graphs
  
  #Parameters for Batched K means
  Batch_size = 50, #The number of cells to be included in each random batch
  Max_N_neighborhoods_Batch = 10, #Number of maximum clusters (neighborhoods) that you desire to find
  Percentage_centroid_initiation = 1,
  N_initiations = 2, #Number of times the algorithm is going to be tried to find the best clustering result
  Max_iterations = 10, #Max number of iterations in each try
  
  #Parameters for Gaussian Mixture Model
  Quality_metric = "AIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
  Max_N_neighborhoods_GMM = 10, #Number of maximum clusters (phenotypes) that you desire to find
  Max_iterations_km = 2, #Number of max iterations in the K means clustering performed
  Max_iterations_em = 2, #Number of max iterations in the Expectation Maximization algorithm
  GMM_Distance = "eucl_dist", #Distance metric to use in the model ("eucl_dist" or "maha_dist")
  
  #Parameters for CLARA clustering
  Samples_CLARA = 10, #Number of samples the CLARA algorithm is going to use to be calculated
  Sample_per_CLARA = 0.5, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
  Max_N_neighborhoods_CLARA = 10, #Number of maximum clusters (neighborhoods) that you desire to find
  Distance_CLARA = "euclidean", #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
  #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
  N_cores = 1 #Number of cores to parallelize your computation
)

DATA_neighborhoods <- DATA_neighborhoods_renamer(DATA = DATA_neighborhoods, #Provide the data containing the Neighborhood information
                                                 New_names = c("Neighborhood_name_1", #Specify the names of the different neighborhoods in Order (1, 2, 3...)
                                                               "Neighborhood_name_2",
                                                               "Neighborhood_name_3",
                                                               "Neighborhood_name_4"
                                                               )
)

####ALTERNATIVE 2 UTAG
#Alternatively UTAG algorithm can be applied. UTAG modifies each cell according to it´s surrounding neighbors in a process called message passing

#For this demo, we'll try UTAG in a smaller subset
DATA2 <- DATA_normalized %>% dplyr::filter(Subject_Names %in% unique(DATA_normalized$Subject_Names)[1:5])

#First perform message passing
DATA_UTAG <- UTAG_message_passing(DATA = "DATA2",#Provide the data with marker expression information (a character scalar)
                                  COO_to_visit = NULL, #Should the algorithm perform message passing in all the cells of the dataset?
                                                       #This can be NULL or it a can be a logical vector specifying which cells to visit
                                  Neighbor_strategy = "Both", #How are neighbors defined. They can be defined by Number, Distance or Both
                                  Message_strategy = "Sum", #Averaging (it will average the expression of markers between the COO and the neighbors),
                                                            #Sum (it will sum the marker expression data of the COO and the neighbors)
                                  N_neighbors = 10, #Number of closest neighbors to each COO to be considered
                                  Max_dist_allowed = 100, #Specify the max distance to consider two cells as neighbors
                                  
                                  Weighting_Strategy = "Proximity", #Should message passing be weighted (4 options: None, Proximity, Disregarded_minority or Both)
                                                                    #None: no weighting
                                                                    #Proximity: Neighbors closer to the COO will have higher influence compared to more distant neighbors
                                                                    #Disregarded_minority: Markers with low expression in the dataset will be overrepresented in their neighbors
                                                                    #Both: Perform both weighting strategies
                                  COO_weight = NULL, #Decide the weight to assign to the Cell of Origin (can be NULL (weight according to number of neighbors) or a numeric value from 0 to 1) when it mixes with their neighbors
                                  
                                  N_cores = 4 #The number of cores to perform the computation
)
#Since this is a computationally intensive task you may need to write your results at the end of the computation
write.csv(DATA_UTAG, "UTAG_CELLS_DATA.csv", row.names = F)
paste0("UTAG data cells have been saved in: ", getwd())

#You may import a DATA_Closest_Neighbors object if you already have it
DATA_UTAG <- read_csv("UTAG_CELLS_DATA.csv")

#Now cluster the resulting cells
#First we try SOM strategy
DATA_neighborhoods_UTAG <- UTAG_Neighborhood_identifier(DATA = DATA_UTAG, #Provide Message passed cells
                                                   Strategy = "SOM", #Decide one of the methods from Consensus_Clustering, SOM, Graph_Based
                                                   #K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering
                                                   Min_Neighbors = 1, #Select cells with at least a minimum number of neighbors
                                                   
                                                   #Denoising parameters
                                                   Apply_Denoise = TRUE, #Specify if a denoising filtering is required before clustering
                                                   Denoising = "DimRed_DBscan", #Select denoising strategy from: Quantile, Standard_Deviation, Threshold, Otsu or DimRed_DBscan
                                                   Percentile = NULL, #Select the adequate percentile for quantile threshold
                                                   N_Standard_Deviations = NULL, #Select the number of standard deviations from mean for Standard_Deviation method
                                                   Selected_threshold = NULL, #Select the absolute threshold for the Threshold method
                                                   Min_cell_no = 5, #Parameter for DBscan
                                                   Distance_radius = 0.05, #Parameter for DBscan
                                                   
                                                   #Dimension reduction
                                                   Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
                                                   Dimension_reduction = "TSNE", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
                                                   Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
                                                   Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
                                                   
                                                   #Parameters for Consensus Clustering
                                                   Max_N_neighborhoods = 10, #Number of maximum neighborhods that you desire to find
                                                   Consensus_reps = 5, #Number of iterations of the algorithm to try to converge
                                                   Consensus_p_Items = 0.9, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
                                                   Consensus_Cluster_Alg = "km", #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
                                                   Consensus_Distance = "euclidean", #Distance metric to be used (pearson(1 - Pearson correlation), spearman(1 - Spearman correlation), euclidean, binary, maximum, canberra, minkowski
                                                   Consensus_Name = "test", #Name of the folder that is going to be created in order to place the resulting graphs
                                                   
                                                   #Parameters for Self-Organizing Maps
                                                   Max_SOM_neighborhoods = 10, #Maximum number of clusters (neighborhoods) to try in the algorithm
                                                   
                                                   #Parameters for Graph-Based approaches
                                                   Nearest_neighbors_for_graph = 10, #Specify the number of closest neighbors to calculate the graph
                                                   Graph_Method = "Leiden", #Specify the clustering method
                                                   Graph_Resolution = 0.1, #Specify the graph resolution
                                                   N_steps = NULL, #Number of steps given in the WalkTrap algorithm
                                                   
                                                   #Parameters for K means Meta Clustering
                                                   N_K_centroids = 1000, #Number of centroids to perform K means
                                                   Max_N_neighborhoods_Meta = 10, #Number of maximum clusters (neighborhoods) that you desire to find
                                                   Consensus_reps_Meta = 5, #Number of iterations of the algorithm to try to converge
                                                   Consensus_p_Items_Meta = 0.9, #Percentage of cells that you desire to sample in each iteration
                                                   Consensus_Name_Meta = "TEST", #Name of the folder that is going to be created in order to place the resulting graphs
                                                   
                                                   #Parameters for Batched K means
                                                   Batch_size = 10000, #The number of cells to be included in each random batch
                                                   Max_N_neighborhoods_Batch = 10, #Number of maximum clusters (neighborhoods) that you desire to find
                                                   Percentage_centroid_initiation = 1,#Percentage of cells (from 0 to 1) that will be used in algorithm initiation
                                                   N_initiations = 1, #Number of times the algorithm is going to be tried to find the best clustering result
                                                   Max_iterations = 10, #Max number of iterations in each try
                                                   
                                                   #Parameters for Gaussian Mixture Model
                                                   Quality_metric = "BIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
                                                   Max_N_neighborhoods_GMM = 5, #Number of maximum clusters (phenotypes) that you desire to find
                                                   Max_iterations_km = 10, #Number of max iterations in the K means clustering performed
                                                   Max_iterations_em = 10, #Number of max iterations in the Expectation Maximization algorithm
                                                   GMM_Distance = "eucl_dist", #Distance metric to use in the model ("eucl_dist" or "maha_dist")
                                                   
                                                   #Parameters for CLARA clustering
                                                   Samples_CLARA = 100, #Number of samples the CLARA algorithm is going to use to be calculated
                                                   Sample_per_CLARA = 0.1, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
                                                   Max_N_neighborhoods_CLARA = 4, #Number of maximum clusters (neighborhoods) that you desire to find
                                                   Distance_CLARA = "euclidean", #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
                                                   #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
                                                   N_cores = 5 #Number of cores to parallelize your computation
)

#Now we try try Consensus_Clustering strategy
DATA_neighborhoods_UTAG_2 <- UTAG_Neighborhood_identifier(DATA = DATA_UTAG, #Provide Message passed cells
                                                        Strategy = "Consensus_Clustering", #Decide one of the methods from Consensus_Clustering, SOM, Graph_Based
                                                        #K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering
                                                        Min_Neighbors = 1, #Select cells with at least a minimum number of neighbors
                                                        
                                                        #Denoising parameters
                                                        Apply_Denoise = TRUE, #Specify if a denoising filtering is required before clustering
                                                        Denoising = "DimRed_DBscan", #Select denoising strategy from: Quantile, Standard_Deviation, Threshold, Otsu or DimRed_DBscan
                                                        Percentile = NULL, #Select the adequate percentile for quantile threshold
                                                        N_Standard_Deviations = NULL, #Select the number of standard deviations from mean for Standard_Deviation method
                                                        Selected_threshold = NULL, #Select the absolute threshold for the Threshold method
                                                        Min_cell_no = 5, #Parameter for DBscan
                                                        Distance_radius = 0.05, #Parameter for DBscan
                                                        
                                                        #Dimension reduction
                                                        Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
                                                        Dimension_reduction = "TSNE", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
                                                        Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
                                                        Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
                                                        
                                                        #Parameters for Consensus Clustering
                                                        Max_N_neighborhoods = 10, #Number of maximum neighborhods that you desire to find
                                                        Consensus_reps = 5, #Number of iterations of the algorithm to try to converge
                                                        Consensus_p_Items = 0.9, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
                                                        Consensus_Cluster_Alg = "hc", #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
                                                        Consensus_Distance = "euclidean", #Distance metric to be used (pearson(1 - Pearson correlation), spearman(1 - Spearman correlation), euclidean, binary, maximum, canberra, minkowski
                                                        Consensus_Name = "test", #Name of the folder that is going to be created in order to place the resulting graphs
                                                        
                                                        #Parameters for Self-Organizing Maps
                                                        Max_SOM_neighborhoods = 10, #Maximum number of clusters (neighborhoods) to try in the algorithm
                                                        
                                                        #Parameters for Graph-Based approaches
                                                        Nearest_neighbors_for_graph = 10, #Specify the number of closest neighbors to calculate the graph
                                                        Graph_Method = "Leiden", #Specify the clustering method
                                                        Graph_Resolution = 0.1, #Specify the graph resolution
                                                        N_steps = NULL, #Number of steps given in the WalkTrap algorithm
                                                        
                                                        #Parameters for K means Meta Clustering
                                                        N_K_centroids = 1000, #Number of centroids to perform K means
                                                        Max_N_neighborhoods_Meta = 10, #Number of maximum clusters (neighborhoods) that you desire to find
                                                        Consensus_reps_Meta = 5, #Number of iterations of the algorithm to try to converge
                                                        Consensus_p_Items_Meta = 0.9, #Percentage of cells that you desire to sample in each iteration
                                                        Consensus_Name_Meta = "TEST", #Name of the folder that is going to be created in order to place the resulting graphs
                                                        
                                                        #Parameters for Batched K means
                                                        Batch_size = 10000, #The number of cells to be included in each random batch
                                                        Max_N_neighborhoods_Batch = 10, #Number of maximum clusters (neighborhoods) that you desire to find
                                                        Percentage_centroid_initiation = 1,#Percentage of cells (from 0 to 1) that will be used in algorithm initiation
                                                        N_initiations = 1, #Number of times the algorithm is going to be tried to find the best clustering result
                                                        Max_iterations = 10, #Max number of iterations in each try
                                                        
                                                        #Parameters for Gaussian Mixture Model
                                                        Quality_metric = "BIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
                                                        Max_N_neighborhoods_GMM = 5, #Number of maximum clusters (phenotypes) that you desire to find
                                                        Max_iterations_km = 10, #Number of max iterations in the K means clustering performed
                                                        Max_iterations_em = 10, #Number of max iterations in the Expectation Maximization algorithm
                                                        GMM_Distance = "eucl_dist", #Distance metric to use in the model ("eucl_dist" or "maha_dist")
                                                        
                                                        #Parameters for CLARA clustering
                                                        Samples_CLARA = 100, #Number of samples the CLARA algorithm is going to use to be calculated
                                                        Sample_per_CLARA = 0.1, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
                                                        Max_N_neighborhoods_CLARA = 4, #Number of maximum clusters (neighborhoods) that you desire to find
                                                        Distance_CLARA = "euclidean", #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
                                                        #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
                                                        N_cores = 5 #Number of cores to parallelize your computation
)

#We now pick the DATA 
DATA_neighborhoods_UTAG_final <- DATA_neighborhoods_UTAG$DATA
DATA_neighborhoods_UTAG_final_2 <- DATA_neighborhoods_UTAG_2$DATA


#We now need to analyze our data
#First rename our neighborhoods (OPTIONAL)
DATA_neighborhoods_UTAG_final <- DATA_neighborhoods_renamer(DATA = DATA_neighborhoods_UTAG_final, #Provide the data containing the Neighborhood information
                                                 New_names = c("Neighborhood_name_1", #Specify the names of the different neighborhoods in Order (1, 2, 3...)
                                                               "Neighborhood_name_2",
                                                               "Neighborhood_name_3",
                                                               "Neighborhood_name_4",
                                                               "Neighborhood_name_5")
)

DATA_neighborhoods_UTAG_final_2 <- DATA_neighborhoods_renamer(DATA = DATA_neighborhoods_UTAG_final_2, #Provide the data containing the Neighborhood information
                                                            New_names = c("Neighborhood_name_1", #Specify the names of the different neighborhoods in Order (1, 2, 3...)
                                                                          "Neighborhood_name_2",
                                                                          "Neighborhood_name_3",
                                                                          "Neighborhood_name_4",
                                                                          "Neighborhood_name_5",
                                                                          "Neighborhood_name_6")
)

##You can explore concordance between neighborhood analysis processes using the rand index as in STEP 3
##You need to introduce as arguments DATA sources containing a variable to be compared with other DATA sources
##DATA sources must be named. User can work with as many DATA sources as desired
Concordance_calculator(SOM = DATA_neighborhoods_UTAG_final, 
                       Consensus_Clustering = DATA_neighborhoods_UTAG_final_2,
                       Variable = "Neighborhood_assignment", #Choose the variable name that contains the neighborhoods
                       Strategy = "Rand" #Choose betweeen Rand and FM (Fowlkes-Mallows)
)


#Now you can analyze using any neighborhood DATA you've created

#Quantify the number of neighborhoods in your samples
Neighborhoods_by_Sample <- Neighborhood_Quantifier(DATA = DATA_neighborhoods, #Provide the data containing the Neighborhood information
                                                   Calculate_Density = T, #Specify if density should be calculated
                                                   DATA_Area = DATA_AREA #If densities should be calculated provide tissue area estimations
)



#Correlate your neighborhoods with functional markers
Cell_functional_assessment(DATA = DATA_neighborhoods, #Provide the DATA with data phenotypes
                           Target_Variable = "Neighborhood_assignment", #Specify the name of the variable where phenotypes are included
                           Targets_Included = c("Neighborhood_name_1", 
                                                "Neighborhood_name_2",
                                                "Neighborhood_name_3",
                                                "Neighborhood_name_4"), #Specify the phenotypes to be analyzed
                           DATA_Aside = DATA_aside, #Provide the DATA with the functional markers
                           Threshold_functional_Markers = F, #Specify if multi-level thresholding should be performed on functional markers
                           Levels = 3 #Specify the number of levels to split the functional markers into
)

#Correlate your Quantified neighborhoods with clinical data
Clinical_Data_analyzer(DATA = Neighborhoods_by_Sample, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c('PROP_Neighborhood_name_1', 
                                    'PROP_Neighborhood_name_2', 
                                    'PROP_Neighborhood_name_3', 
                                    'PROP_Neighborhood_name_4'), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "MMRP_status", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = F, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "Outcome_time", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "Outcome_event" #and outcome variable
)

Clinical_Data_analyzer(DATA = Neighborhoods_by_Sample, #Provide the Data with the subject names and the summary information of the experiment
                       DATA_var = c('PROP_Neighborhood_name_1', 
                                    'PROP_Neighborhood_name_2', 
                                    'PROP_Neighborhood_name_3', 
                                    'PROP_Neighborhood_name_4'), #Provide names of the variables to include in the analysis
                       DATA_Clinical = DATA_CLINICAL, #Provide the Clinical data
                       Clinical_var = "MMRP_status", #Specify the clinical outcome to be evaluated
                       Perform_time_to_event = T, #Specify if there is going to be a time to event analysis performed
                       Time_variable = "OS_m", #If time to event is going to be performed supply adequate time variable 
                       Event_variable = "DEATH" #and outcome variable
)

#Tile your images and see the spatial arrangement of your neighborhoods
Image_length_calculator(DATA = DATA_neighborhoods)

#Calculate an adequate tile size
Suggested_Tile_Size_Calculator(DATA_Phenotypes, #Provide a data phenotypes object 
                               N_cols = 15, #Provide the number of suggested columns 
                               N_rows = 15, #Provide the number of suggested rows
                               Based_on_smaller = T, #Wether to base calculation on the smallest or the largest image
                               Draw_preview = T #Specify if you want a preview to be graphicated
)

#You may also take into account the size of the neighborhoods
quantile(DATA_neighborhoods$max_DIST, 0.95)

#Generate tiled images
Tiled_Images_neighborhoods <- Image_tiling_processing_function(N_cores = 4, #Number of cores to parallelize your computation
                                                 DATA = "DATA_neighborhoods", #Name of the DATA_phenotype object to be used in the analysis (A CHARACTER SCALAR)
                                                 Tile_width = 100, #Width of your tiles
                                                 Tile_height = 100, #height of your tiles
                                                 Variables_to_keep = "Neighborhood_assignment") #Names of the variables to be preserved in the tiling process




#Celebrate elections in each of your tiles
DATA_neighborhood_elections <- Neighborhood_voting_function(N_cores = 4, #Number of cores to parallelize your computation
                                                            Tiled_Images = Tiled_Images_neighborhoods, #Name of the object containing the tiled Images
                                                            Minimum_cell_no_per_tile = 1, #Minimum number of neighborhoods a tile should contain to be included in the analysis
                                                            Neighborhoods_included = c("Neighborhood_name_1", 
                                                                                       "Neighborhood_name_2", 
                                                                                       "Neighborhood_name_3",
                                                                                       "Neighborhood_name_4")
                                                            )

#Graph your data
Tiled_neighborhoods_graphicator(DATA_elections = DATA_neighborhood_elections, #Specify the object containing the data neighborhood elections
                                Image_name = "Endo_IG09_T_Core[1,1,F]", #Provide the name of the sample
                                Graph_only_winner_neighborhood = TRUE #Specify if only the winner neighborhood in each tile should be graphed
)


#### Alternative 3 HISTOCAT based (cluster image tiles to find recurrent patterns in tissue)
#First we need to calculate the size of our tiles
Suggested_Tile_Size_Calculator(DATA_Phenotypes, #Provide a data phenotypes object 
                               N_cols = 15, #Provide the number of suggested columns 
                               N_rows = 15, #Provide the number of suggested rows
                               Based_on_smaller = T, #Wether to base calculation on the smallest or the largest image
                               Draw_preview = T #Specify if you want a preview to be graphicated
)

#For this demo, we'll try HISTOCAT in a smaller subset
DATA_Phenotypes2 <- DATA_Phenotypes %>% dplyr::filter(Subject_Names %in% unique(DATA_Phenotypes$Subject_Names)[1:5])


#Generate tiled images
Tiled_Images_neighborhoods2 <- Image_tiling_processing_function(N_cores = 4, #Number of cores to parallelize your computation
                                                 DATA = "DATA_Phenotypes2", #Name of the DATA_phenotype object to be used in the analysis (A CHARACTER SCALAR)
                                                 Tile_width = 100, #Width of your tiles
                                                 Tile_height = 100, #height of your tiles
                                                 Variables_to_keep = "Phenotype" #Names of the variables to be preserved in the tiling process
)

#Find recurrent patterns in our tiles
Clustered_Tiled_Images <- Tiled_Image_Clustering_function(Tiled_images = Tiled_Images_neighborhoods2, #Provide a list with tiled images
                                                          Minimum_cell_no_per_tile = 10, #State the min number of cells per tile
                                                          Minimum_valid_tiles_per_image = 10, #State the min number of tiles per image
                                                          Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG", "OTHER"), #Provide the cell phenotypes included in the analysis
                                                          
                                                          #Cluster based on tile cell density or tile cell percentage
                                                          Cluster_Data = "Cell_Density", #Choose between Cell_Density or Cell_Percentage
                                                          
                                                          #Dimension reduction
                                                          Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
                                                          Dimension_reduction = "PCA", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
                                                          Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
                                                          Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
                                                          
                                                          #Clustering strategy
                                                          Strategy = "Consensus_Clustering", #Choose your clustering method (either Consensus_Clustering, SOM, Graph_Based
                                                          #K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
                                                          
                                                          
                                                          #Parameters for Consensus Clustering
                                                          Max_N_Clusters = 10, #Number of maximum neighborhods that you desire to find
                                                          Consensus_reps = 2, #Number of iterations of the algorithm to try to converge
                                                          Consensus_p_Items = 0.8, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
                                                          Consensus_Cluster_Alg = "pam", #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
                                                          Consensus_Distance = "euclidean", #Distance metric to be used (pearson(1 - Pearson correlation), spearman(1 - Spearman correlation), euclidean, binary, maximum, canberra, minkowski
                                                          Consensus_Name = "Clustering_test1", #Name of the folder that is going to be created in order to place the resulting graphs
                                                          
                                                          #Parameters for Self-Organizing Maps
                                                          Max_SOM_Clusters = 10, #Maximum number of clusters (neighborhoods) to try in the algorithm
                                                          
                                                          #Parameters for Graph Based Clustering
                                                          Graph_type = NULL, #Decide the type of graph to build: it can be complete (more accurate but computationally intensive), SNN (nearest neighbor) or Dimension_SNN (based on dimension reduction data)
                                                          Nearest_neighbors_for_graph = 10, #If closest neighbor graphs are calculated decide the number of closest neighbors to include
                                                          Graph_Method = "Leiden", #One of Louvain, Leiden, Optimal, Greedy, WalkTrap, Spinglass, Leading_Eigen or Edge_Betweenness
                                                          Graph_Resolution = 1, #Used for Louvain and Leiden. 1 is default. The smaller the value, the larger the clusters will be
                                                          Graph_Distance_method = "euclidean", #Distance used to calculate the graph weight (euclidean, maximum, manhattan, canberra, binary or minkowski)
                                                          N_steps = NULL, #Number of steps given in the WalkTrap algorithm
                                                          
                                                          #Parameters for K means Meta Clustering
                                                          N_K_centroids = 100, #Number of centroids to perform K means
                                                          Max_N_Clusters_Meta = 10, #Number of maximum clusters (neighborhoods) that you desire to find
                                                          Consensus_reps_Meta = 2, #Number of iterations of the algorithm to try to converge
                                                          Consensus_p_Items_Meta = 1, #Percentage of cells that you desire to sample in each iteration
                                                          Consensus_Name_Meta = "TEST", #Name of the folder that is going to be created in order to place the resulting graphs
                                                          
                                                          #Parameters for Batched K means
                                                          Batch_size = 50, #The number of cells to be included in each random batch
                                                          Max_N_Clusters_Batch = 10, #Number of maximum clusters (neighborhoods) that you desire to find
                                                          Percentage_centroid_initiation = 1,
                                                          N_initiations = 2, #Number of times the algorithm is going to be tried to find the best clustering result
                                                          Max_iterations = 10, #Max number of iterations in each try
                                                          
                                                          #Parameters for Gaussian Mixture Model
                                                          Quality_metric = "AIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
                                                          Max_N_Clusters_GMM = 10, #Number of maximum Clusters (neighboorhoods) that you desire to find
                                                          Max_iterations_km = 2, #Number of max iterations in the K means clustering performed
                                                          Max_iterations_em = 2, #Number of max iterations in the Expectation Maximization algorithm
                                                          GMM_Distance = "eucl_dist", #Distance metric to use in the model ("eucl_dist" or "maha_dist")
                                                          
                                                          #Parameters for CLARA clustering
                                                          Samples_CLARA = 10, #Number of samples the CLARA algorithm is going to use to be calculated
                                                          Sample_per_CLARA = 0.5, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
                                                          Max_N_Clusters_CLARA = 10, #Number of maximum clusters (neighborhoods) that you desire to find
                                                          Distance_CLARA = "euclidean", #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
                                                          #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
                                                          N_cores = 1 #Number of cores to parallelize your computation
)

#Rename the clusters if desired
Clustered_Tiled_Images <- Clustered_Tiled_Images_renamer(Tiled_images = Clustered_Tiled_Images,
                                                         New_names = c("Neighborhood_name_1", 
                                                                       "Neighborhood_name_2",
                                                                       "Neighborhood_name_3",
                                                                       "Neighborhood_name_4",
                                                                       "Neighborhood_name_5"))

#Analyze cluster abundance and heterogeneity
Clustered_Tiled_Images_analysis <- Clustered_Tiled_Images_analyzer(Tiled_images = Clustered_Tiled_Images, #Provide the clustered tiled images
                                                                   Perform_heterogeneity_analysis = T, #Select if heterogeneity analysis should be performed
                                                                   Graph_Modularity_resolution = 0.5 #Provide graph modularity index
)


#Graph individual images
Clustered_Tiled_Images_graphicator(Tiled_images = Clustered_Tiled_Images,
                                   Image_name = "Endo_IG09_T_Core[1,1,D]")


#### Alternative 4 find recurrent patterns of multiple cell to cell atraction/rejection patterns
#First calculate your cell-wise interaction scores

#For this demo, we'll try recurrent patterns of multiple cell to cell atraction/rejection patterns in a smaller subset
DATA_Phenotypes2 <- DATA_Phenotypes %>% dplyr::filter(Subject_Names %in% unique(DATA_Phenotypes$Subject_Names)[1:5])

DATA_Interaction <- Interaction_counter(DATA = "DATA_Phenotypes2", #Provide the a phenotypes data source
                                        Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG", "OTHER"), #Select phenotypes to calculate interactions1
                                        N_cores = 4, #Number of cores to parallelize your computation
                                        
                                        Graph_type = "knn", #one of "expansion", "knn", "delaunay"
                                        K_number = 20, #Max number of K for knn graphs
                                        Dist_threshold = NULL, #Distance threshold for expansion graphs
                                        
                                        Method = "classic", # one of "classic", "histocat", "patch"
                                        patch_size = 100, #size of the patch for patch-based interaction computing
                                        
                                        Perform_significance_testing = T, #Specify if significance test needs to be performed
                                        N_iterations = 25, #Number of iterations to test interaction score
                                        p_threshold = 0.05 #Threshold to consider the p value significan
)

#Analyze your interaction patterns
By_sample_interactions <- 
  Interaction_analyzer(DATA = DATA_Interaction, #Provide a list with the interaction data
                       Exclude_non_significant = F, #Decide if non significant interactions should be removed (if TRUE provide DATA with significance testing)
                       Cluster = T, #Decide wether clustering should be performed to summarize the results
                       
                       Max_N_Clusters = 4, #Number of maximum clusters that you desire to find
                       Consensus_reps = 15, #Number of iterations of the algorithm to try to converge
                       Consensus_p_Items = 1, #Percentage of the samples that you desire to use in each iteration
                       Consensus_Cluster_Alg = "km", #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
                       Consensus_Distance = "euclidean", #Distance metric to be used "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
                       Consensus_Name = "Clustering_test2" #Name of the folder that is going to be created in order to place the resulting graphs
  )









##Tissue segmentation can be performed using our tile based quick TUMOR / STROMA segmenting function
DATA_structured <- Quick_Tumor_Stroma_identifier(DATA_Phenotypes = "DATA_Phenotypes", #Provide name of a data containing the phenotype info
                                                 Index_phenotype = "TUMOR", #Provide the cell that will be considered as tumoral
                                                 Accuracy = 75, #Accuracy index (guides the tiling strategy)
                                                 Min_cell_no = 10, #Min number of cells to to be considered a tumor area
                                                 Image_preview = "Endo_IG09_T_Core[1,1,B]", #Image to be used in the preview
                                                 N_cores = 4 #Number of cores to parellelize the computation
)

#We can perform a slower but more precise algorithm that is based on DBSCAN clustering technique
DATA_structured1 <- DBSCAN__Tumor_Stroma_identifier(DATA_Phenotypes = "DATA_Phenotypes", #Provide name of a data containing the phenotype info
                                                   Index_phenotype = "TUMOR", #Provide the cell that will be considered as tumoral
                                                   Image_preview = "Endo_IG09_T_Core[1,1,B]", #Image to be used in the preview
                                                   Min_cells = 2, #Min number of cells to to be considered a tumor area
                                                   Distance_radius = 50, #Min distance to calculate spatial clusters
                                                   N_cores = 4 #Number of cores to parellelize the computation
)

#We can perform LISA clustering, it calculates Tumor, Stroma and border compartments (it requires plenty of RAM)
DATA_structured <- LISA__Tumor_Stroma_identifier(DATA_Phenotypes = "DATA_Phenotypes", #Provide name of a data containing the phenotype info
                                                 Index_phenotype = "TUMOR",#Provide the cell that will be considered as tumoral
                                                 Image_preview = "Endo_IG09_T_Core[1,1,B]", #Image to be used in the preview
                                                 Association_Dist_min = 10, #Minimum distance to test cell-cell spatial interaction
                                                 Association_Dist_max = 50, #Maximum distance to test cell-cell spatial interaction
                                                 Type_of_LISA_function = "L", #Select LISA function (L or K)
                                                 Window_type = "convex", #Select the interaction window shape (’square’, ’convex’ or ’concave’)
                                                 N_cores = 4 #Number of cores to parallelize your computation
)

#We can perform a mixture of tiling/DBSCAN filtering plus polygon inference with an advanced function
DATA_structured_advanced <- Advanced_Tumor_Stroma_identifier(DATA_Phenotypes = "DATA_Phenotypes", #Provide the name of the DATA_phenotypes tibble
                                                    Index_phenotype = "TUMOR",#Provide the cell-type that will be considered as tumoral
                                                    Filtering_Method = "DBSCAN", #Select the pre-processing method (Tiling or DBSCAN)
                                                    
                                                    Accuracy = 75, #Accuracy parameter for the Tiling method
                                                    Min_cell_no = 3, #Min number of cells parameter (for Tiling and DBSCAN methods)
                                                    Distance_radius = 100, #Min radius parameter for DBSCAN method
                                                    
                                                    Hull_ratio = 0.1, #Ratio to calculate the convex Hull (from 0 to 1) (the smaller the more precise)
                                                    Calculate_border = T, #Should bordering cells be computed, or just tumor / stromal cells
                                                    Dist_to_border = 10, #Distance that defines the bordering cells
                                                    Image_preview = "Endo_IG09_T_Core[1,1,B]", #Image to be used in the preview 
                                                    N_cores = 4 #Number of cores to parallelize your computation
)

#If data has been compartimentalized with Advanced_Tumor_stroma_identifier, we can calculate the proportion and density of the cells wihin each compartment
Phenotypes_by_sample_Compartment <- Compartment_Phenotype_quantifier(DATA = DATA_structured, #Select the source DATA
                                                                     Calculate_Density = TRUE, #Select if density should be calculated
                                                                     DATA_Area = DATA_AREA #Provide the DATA containing sample areas
)

#Alternatively we can use SPIAT
#First edit data into SPIAT type objects
DATA_SPIAT <- SPIAT_object_generator(DATA_Intensities = DATA, #Specify original data with marker intensity information
                                     DATA_Phenotypes = DATA_Phenotypes #Specify data with phenotype information
)

unique(DATA_Phenotypes$Phenotype)

#Find tissue structures (TUMOR, STROMA, BORDER...) and assign each cell to a tissue structure
DATA_SPIAT_structured <- 
  SPIAT_Tissue_structuring_function(N_cores = 4, #Number of cores to parallelize your computation
                                    DATA_SPIAT = "DATA_SPIAT", #a character SCALAR specifying the name of the list containing the SPIAT objects
                                    DATA_Phenotypes = "DATA_Phenotypes", #a character SCALAR specifying the name of DATA_phenotypes object
                                    Cell_type_to_define_cluster = "TUMOR", #Cell that defines the tumor cluster
                                    Minimum_number_cells_cluster = 10, #Minimum cells to generate a cluster
                                    Cell_types_of_interest = c("TUMOR", "MACROPHAGE", "CD8", "TREG"), #Cell types of interest
                                    Layers_margin = 5, #Cell layers that define the cluster margin
                                    Simplify_result = T #Specify if results should be simplified
  )

#You may use the Neighborhood identifier of SPIAT (most of these algorithms find neighborhoods by segmenting cells according to their spatial proximity)
SPIAT_neighborhood_identifier(N_cores = 4, #Number of cores to parallelize your computation
                              DATA_SPIAT = "DATA_SPIAT", #a character SCALAR specifying the name of the list containing the SPIAT objects
                              Strategy = "hierarchical", # Method to find the clusters one of "hierarchical" or "dbscan" 
                              Cell_types_of_interest = c("TUMOR", "MACROPHAGE", "CD8", "TREG"), #Cell types of interest
                              Radius = 50, #Radius used in the algorithm to define neighbors
                              Min_neighborhood_size = 10, #Min number of cells to constitute a neighborhood
                              No_Phenotype_name = "OTHER" #Name of the cells without a defined phenotype
)

