source(file  = "CSM VER_1.4.6 SOURCE CODE.R", # The file location of CSM functions
       echo = FALSE)

#Check your computing machine features
Describe_my_computing_unit()



#########ENDOMETRIAL CARCINOMA DATASET#########
#Load Cell feature matrix and format it adequately
DATA_Endometrial <- readRDS("Endometrial_Dataset.rds")

DATA_Endometrial <- Data_arrange_function(DATA = DATA_Endometrial, #Name of data frame
                                          X = 'X',  #Name of column specifying X coordinate
                                          Y = "Y", #Name of column specifying Y coordinate
                                          Subject_Names = "Subject_Names", #Name of column specifying the image name for each cell
                                          Markers_to_keep = "Phenotype") #State markers by column name (Vector)

#Check adequacy of data
Data_QC_Check_function(DATA = DATA_Endometrial)

#Load the inForm data
DATA_inForm <- readRDS("Endometrial_Inform_DATA.rds")

#We will keep cores for which we have inForm data
DATA_Endometrial <- DATA_Endometrial %>% dplyr::filter(Subject_Names %in% DATA_inForm$Subject_Names)

#Now we will calculate the surface of the sample
DATA_AREA <- Image_size_calculator(DATA = DATA_Endometrial,#Specify the data source to calculate the areas it can be any DATA type that has X and Y
                                   
                                   Strategy = "Tiling", #Specify the area inference strategy (either Tiling or Concave_hull)
                                   Image_to_plot = NULL, #Specify the image to perform preview (if NULL the smallest sample will be chosen)
                                   
                                   Tile_accuracy = 100, #Lower values calculate the area in a more precise manner, with higher computational times
                                   #In addition if the value is too low, it can detect areas of stroma as not being tissue 
                                   Hull_ratio = 0.1 #Define the hull ratio for Concave hull calculation
)
#Calculate the cell types by sample
Phenotypes_by_Sample <- Phenotype_quantifier(DATA = DATA_Endometrial, #Specify the data frame you want to use
                                             Calculate_Density = TRUE, #Specify if density of cell types should be calculated
                                             DATA_Area = DATA_AREA #Provide area data if cell density needs to be calculated
)

#(OPTIONAL STEP) We will calculate the cells by mm (currently is cells by squared pixel) taking into account that each pixel is roughly 0.2 squared microns
Phenotypes_by_Sample[str_detect(names(Phenotypes_by_Sample), "Density_")] <- 
  map_dfc(Phenotypes_by_Sample[str_detect(names(Phenotypes_by_Sample), "Density_")], 
          function(Variable){
            (Variable*10^7)/5
          })

#We will also calculate the Tumor compartment 
DATA_structured <- Advanced_Tumor_Stroma_identifier(DATA_Phenotypes = "DATA_Endometrial", #Provide the name of the DATA_phenotypes tibble
                                                    Index_phenotype = "TUMOR",#Provide the cell-type that will be considered as tumoral
                                                    Filtering_Method = "DBSCAN", #Select the pre-processing method (Tiling or DBSCAN)
                                                    
                                                    Accuracy = 50, #Accuracy parameter for the Tiling method
                                                    Min_cell_no = 30, #Min number of cells parameter (for Tiling and DBSCAN methods)
                                                    Distance_radius = 75, #Min radius parameter for DBSCAN method
                                                    
                                                    Hull_ratio = 0.175, #Ratio to calculate the convex Hull (from 0 to 1) (the smaller the more precise)
                                                    Calculate_border = T, #Should bordering cells be computed, or just tumor / stromal cells
                                                    Dist_to_border = 1, #Distance that defines the bordering cells
                                                    Image_preview = "Endo_TMA_IG08T_Core[1,4,A]", #Image to be used in the preview 
                                                    N_cores = 6 #Number of cores to parallelize your computation
)



#Calculate CD8 correlations
CD8_Den_tibble <- Phenotypes_by_Sample %>% dplyr::select(Subject_Names, Density_CD8) %>%
  left_join(DATA_inForm %>% dplyr::select(Subject_Names, CD8_Total), by = "Subject_Names")
cor.test(CD8_Den_tibble$Density_CD8, CD8_Den_tibble$CD8_Total, method = "spearman")
CD8_Den_tibble %>% ggplot(aes(x = log(Density_CD8, 10), y = log(CD8_Total, 10))) + geom_point()+
  geom_smooth(method = "lm")

#Check CD68 correlation
CD68_Den_tibble <- Phenotypes_by_Sample %>% dplyr::select(Subject_Names, Density_MACROPHAGE) %>%
  left_join(DATA_inForm %>% dplyr::select(Subject_Names, CD68_Total), by = "Subject_Names")
cor.test(CD68_Den_tibble$Density_MACROPHAGE, CD68_Den_tibble$CD68_Total, method = "spearman")
CD68_Den_tibble %>% ggplot(aes(x = log(Density_MACROPHAGE, 10), y = log(CD68_Total, 10))) + geom_point()+
  geom_smooth(method = "lm")

#Check Treg correlation
TREG_Den_tibble <- Phenotypes_by_Sample %>% dplyr::select(Subject_Names, Density_TREG) %>%
  left_join(DATA_inForm %>% dplyr::select(Subject_Names, FoxP3_Total), by = "Subject_Names")
cor.test(TREG_Den_tibble$Density_TREG, TREG_Den_tibble$FoxP3_Total, method = "spearman")
TREG_Den_tibble %>% ggplot(aes(x = log(Density_TREG, 10), y = log(FoxP3_Total, 10))) + geom_point() +
  geom_smooth(method = "lm")

#Check CK correlation
TUMOR_Den_tibble <- Phenotypes_by_Sample %>% dplyr::select(Subject_Names, Density_TUMOR) %>%
  left_join(DATA_inForm %>% dplyr::select(Subject_Names, CK_Total), by = "Subject_Names")
cor.test(TUMOR_Den_tibble$Density_TUMOR, TUMOR_Den_tibble$CK_Total, method = "spearman")
TUMOR_Den_tibble %>% ggplot(aes(x = log(Density_TUMOR, 10), y = log(CK_Total, 10))) + geom_point() +
  geom_smooth(method = "lm")

#Check Area correlation
Tumor_Area_tibble <- DATA_structured$DATA_Compartment_Area %>% left_join(DATA_inForm %>% dplyr::select(Subject_Names, Tissue_Area_Epithelium), by = "Subject_Names")
cor.test(Tumor_Area_tibble$Area, Tumor_Area_tibble$Tissue_Area_Epithelium)
Tumor_Area_tibble %>%
  ggplot(aes(x = Area, y = Tissue_Area_Epithelium)) + geom_point()+
  geom_smooth(method = "lm")


#Calculate Cell density association with image metadata
DATA_CLINICAL <- readRDS("Endometrial_Dataset_Metadata.rds")
DATA_CLINICAL <- Clinical_Data_arrange_function(DATA = DATA_CLINICAL, #Specify the object that contains the clinical data
                                                Subject_Names = "Subject_Names", #Specify the column that contains the subject names
                                                Outcomes_to_keep = c("AGE", "DEATH", "OS_m", "LVI", "MMRP_status") #Specify clinical data to keep in for further analyses
)
#OS and death must be numeric values in order to perform time to event analysis
DATA_CLINICAL$OS_m <- as.numeric(DATA_CLINICAL$OS_m)
DATA_CLINICAL$DEATH <- as.numeric(DATA_CLINICAL$DEATH)
#Analyze associations between cell types and image metadata
Clinical_Data_analyzer(DATA = Phenotypes_by_Sample,
                       DATA_var = c("Density_TUMOR", "Density_CD8", "Density_MACROPHAGE", "Density_TREG"),
                       DATA_Clinical = DATA_CLINICAL,
                       Clinical_var = "LVI",
                       Perform_time_to_event = FALSE)

Clinical_Data_analyzer(DATA = Phenotypes_by_Sample,
                       DATA_var = c("Density_TUMOR", "Density_CD8", "Density_MACROPHAGE", "Density_TREG"),
                       DATA_Clinical = DATA_CLINICAL,
                       Clinical_var = "AGE",
                       Perform_time_to_event = FALSE)

Clinical_Data_analyzer(DATA = Phenotypes_by_Sample,
                       DATA_var = c("Density_TUMOR", "Density_CD8", "Density_MACROPHAGE", "Density_TREG"),
                       DATA_Clinical = DATA_CLINICAL,
                       Event_variable = "DEATH",
                       Time_variable = "OS_m",
                       Perform_time_to_event = TRUE)


#Now we will fit a multi-level model testing CD8-CD8 homotypic spatial interactions
DATA_Distances <- Distance_matrix_generator(N_cores = 2, #Number of cores to parallelize your computation
                                            DATA = "DATA_Endometrial",#Name of the DATA_phenotype object to be used in the analysis
                                            Cell_Of_Origin = "CD8", #Name of the cell of origin of the analysis
                                            Target_Cell = "CD8", #Name of the target cell of the analysis
                                            Allow_Cero_Distance = FALSE, #Specify if completely colocallized cells are allowed in the analysis (impact on self to self distance analysis)
                                            Perform_edge_correction = FALSE, #Should Cells of Origin close to sample borders be removed before analysis?
                                            Hull_ratio = 1, #Hull_ratio to calculate sample borders
                                            Distance_to_edge = 10 #Distance from the border
)

DATA_Cumulative_Interaction <- Cumulative_Interaction_generator(N_cores = 4, #Number of cores to parallelize your computation
                                                                DATA = "DATA_Distances", #Name of the DATA_distance object to be used in the analysis
                                                                Start_from = 0, #Starting distance to calculate the cumulative interaction function
                                                                Stop_at = 200, #Final distance where cumulative interaction analysis will stop
                                                                Sampling_frequency = 25 #Sampling distance interval
)


Multi_level_modeling_function(DATA_cumulative = DATA_Cumulative_Interaction, #Provide data containing the cumulative interaction information of COO and Target cells
                              DATA_Clinical = DATA_CLINICAL, #Provide clinical data
                              Clinical_var = "LVI", #Specify the clinical Variable to be analyzed
                              DATA_Densities = Phenotypes_by_Sample, #Provide data containing the cell densities
                              Cell_Of_Origin = "CD8", #Specify the COO
                              Target_cell = "CD8" #Specify the target cell
)

#Lets calculate TRIO distances
Trio_DATA_Distances <- Trio_Distance_matrix_generator(N_cores = 10, #Number of cores to parallelize your computation
                                                      DATA = "DATA_Endometrial", #Name of the DATA_phenotype object to be used in the analysis
                                                      Cell_Of_Origin = "CD8",#Name of the cell of origin of the analysis
                                                      Target_Cell_1 = "MACROPHAGE", #Name of the first target cell of the analysis
                                                      Target_Cell_2 = "TREG", #Name of the second target cell of the analysis
                                                      Perform_edge_correction = TRUE, #Should Cells of Origin close to sample borders be removed before analysis?
                                                      Hull_ratio = 0.6, #Hull_ratio to calculate sample borders
                                                      Distance_to_edge = 100 #Distance from the border
)

Trio_RANDOM_Distances <- Trio_Random_Distance_matrix_generator(N_cores = 5, #Number of cores to parallelize your computation
                                                               DATA = "DATA_Endometrial", #Name of the DATA_phenotype object to be used in the analysis
                                                               Cell_Of_Origin = "CD8", #Name of the cell of origin of the analysis
                                                               Target_Cell_1 = "MACROPHAGE", #Name of the first target cell of the analysis
                                                               Target_Cell_2 = "TREG", #Name of the second target cell of the analysis
                                                               Random_cells_per_sample = 100, #Number of random cells to be generated in the analysis
                                                               Perform_edge_correction = TRUE, #Should Cells of Origin close to sample borders be removed before analysis?
                                                               Hull_ratio = 0.6, #Hull_ratio to calculate sample borders
                                                               Distance_to_edge = 100 #Distance from the border
)


Trio_DATA_Cumulative_Interaction <- Trio_Cumulative_Interaction_generator(N_cores = 10, #Number of cores to parallelize your computation
                                                                          DATA = "Trio_DATA_Distances", #Name of the DATA_distance object to be used in the analysis
                                                                          Start_from = 100, #Starting distance to calculate the cumulative interaction function
                                                                          Stop_at = 200, #Final distance where cumulative interaction analysis will stop
                                                                          Sampling_frequency = 50 #Sampling distance interval
)



Trio_RANDOM_Cumulative_Interaction <- Trio_Cumulative_Interaction_generator(N_cores = 5, #Number of cores to parallelize your computation
                                                                            DATA = "Trio_RANDOM_Distances", #Name of the DATA_distance object to be used in the analysis
                                                                            Start_from = 100, #Starting distance to calculate the cumulative interaction function
                                                                            Stop_at = 200, #Final distance where cumulative interaction analysis will stop
                                                                            Sampling_frequency = 50 #Sampling distance interval
)


Trio_Radius_analysis_200_by_sample <- Trio_Cells_in_Radius_analyzer(DATA = Trio_DATA_Cumulative_Interaction,#Provide the data containing the distances
                                                                    DATA_RANDOM = Trio_RANDOM_Cumulative_Interaction,#Provide data containing the random distances (optional)
                                                                    Radius = 200, #Radius to include in the analysis
                                                                    Include_Random = TRUE, #Decide if Random Data should be used to calculate significance level
                                                                    By_Sample_Random = TRUE #Decide if Random distribution should be calculated by sample or by experiment
)

Trio_Radius_analysis_200_Global <- Trio_Cells_in_Radius_analyzer(DATA = Trio_DATA_Cumulative_Interaction,#Provide the data containing the distances
                                                                 DATA_RANDOM = Trio_RANDOM_Cumulative_Interaction,#Provide data containing the random distances (optional)
                                                                 Radius = 200, #Radius to include in the analysis
                                                                 Include_Random = TRUE, #Decide if Random Data should be used to calculate significance level
                                                                 By_Sample_Random = FALSE #Decide if Random distribution should be calculated by sample or by experiment
)


Trio_DATA_Distances_Uncorrected <- Trio_Distance_matrix_generator(N_cores = 10, #Number of cores to parallelize your computation
                                                                  DATA = "DATA_Endometrial", #Name of the DATA_phenotype object to be used in the analysis
                                                                  Cell_Of_Origin = "CD8",#Name of the cell of origin of the analysis
                                                                  Target_Cell_1 = "MACROPHAGE", #Name of the first target cell of the analysis
                                                                  Target_Cell_2 = "TREG", #Name of the second target cell of the analysis
                                                                  Perform_edge_correction = FALSE, #Should Cells of Origin close to sample borders be removed before analysis?
                                                                  Hull_ratio = NULL, #Hull_ratio to calculate sample borders
                                                                  Distance_to_edge = NULL #Distance from the border
)

Trio_DISTANCE_analysis <- Trio_Min_Distance_analyzer(N_cores = 5,
                                                     DATA = Trio_DATA_Distances_Uncorrected,#Provide the data containing the distances
                                                     DATA_RANDOM = NULL,#Provide data containing the random distances (optional)
                                                     Include_Random = FALSE, #Decide if Random Data should be used to calculate significance level
                                                     By_Sample_Random = FALSE #Decide if Random distribution should be calculated by sample or by experiment
)

#Now we will calculate global interaction pattern analysis
DATA_Interaction <- Interaction_counter(DATA = "DATA_Endometrial", #Provide the a phenotypes data source
                                        Phenotypes_included = c("TUMOR", "CD8", "MACROPHAGE", "TREG"), #Select phenotypes to calculate interactions1
                                        N_cores = 5, #Number of cores to parallelize your computation
                                        
                                        Graph_type = "knn", #one of "expansion", "knn", "delaunay"
                                        K_number = 10, #Max number of K for knn graphs
                                        Dist_threshold = NULL, #Distance threshold for expansion graphs
                                        
                                        Method = "classic", # one of "classic", "histocat", "patch"
                                        patch_size = 100, #size of the patch for patch-based interaction computing
                                        
                                        Perform_significance_testing = T, #Specify if significance test needs to be performed
                                        N_iterations = 50, #Number of iterations to test interaction score
                                        p_threshold = 0.05 #Threshold to consider the p value significan
)

By_sample_interactions <- 
  Interaction_analyzer(DATA = DATA_Interaction, #Provide a list with the interaction data
                       Exclude_non_significant = F, #Decide if non significant interactions should be removed (if TRUE provide DATA with significance testing)
                       Cluster = T, #Decide wether clustering should be performed to summarize the results
                       
                       Max_N_Clusters = 10, #Number of maximum clusters that you desire to find
                       Consensus_reps = 100, #Number of iterations of the algorithm to try to converge
                       Consensus_p_Items = 1, #Percentage of the samples that you desire to use in each iteration
                       Consensus_Cluster_Alg = "pam", #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
                       Consensus_Distance = "euclidean", #Distance metric to be used "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
                       Consensus_Name = "Interaction_test1" #Name of the folder that is going to be created in order to place the resulting graphs
  )







#########BREAST CARCINOMA DATASET#########
DATA_Breast <- readRDS("Breast_Dataset.rds")

DATA_Breast <- Data_arrange_function(DATA = DATA_Breast, #Name of data frame
                                     X = 'X',  #Name of column specifying X coordinate
                                     Y = "Y", #Name of column specifying Y coordinate
                                     Subject_Names = "Subject_Names", #Name of column specifying the image name for each cell
                                     Markers_to_keep = "Phenotype") #State markers by column name (Vector)

#Check adequacy of data
Data_QC_Check_function(DATA = DATA_Breast)

#Calculate cell type composition
Phenotypes_by_Sample <- Phenotype_quantifier(DATA = DATA_Breast, #Specify the data frame you want to use
                                             Calculate_Density = FALSE
)

#Generate a barplot of CD8 distribution
Barplot_generator(Phenotypes_by_Sample, 
                  Phenotypes_included = names(Phenotypes_by_Sample)[str_detect(names(Phenotypes_by_Sample), "CD8") & 
                                                                      !str_detect(names(Phenotypes_by_Sample), "PROP")], #Which phenotypes should be included in the graph
                  Ordering_phenotype = "CD8_PD1neg_GZMneg")


#Analyze PD1 cell interaction patterns
#We will merge CD8 labels to PD1+/PD1- and Tumor and other cells to PDL1+/PDL1-
DATA_Breast_2 <- DATA_Breast %>% dplyr::mutate(Phenotype = case_when(Phenotype == "CD8_PD1low_GZMhigh" | 
                                                                       Phenotype == "CD8_PD1low_GZMlow" |
                                                                       Phenotype == "CD8_PD1low_GZMneg" |
                                                                       Phenotype == "CD8_PD1high_GZMhigh" | 
                                                                       Phenotype == "CD8_PD1high_GZMlow" |
                                                                       Phenotype == "CD8_PD1high_GZMneg" ~ "CD8_PD1pos",
                                                                     Phenotype == "TUMOR_PDL1high" |
                                                                       Phenotype == "TUMOR_PDL1low" |
                                                                       Phenotype == "OTHER_PDL1high" |
                                                                       Phenotype == "OTHER_PDL1low" ~ "PDL1_pos_cell",
                                                                     TRUE ~ Phenotype
))

#Not enough colors in the palette to be graphicated  
Gcross_Analysis <- Gcross_calculator(DATA = DATA_Breast_2, #Provide the data containing the Cell Type information
                                     Cell_Of_Origin = "CD8_PD1pos", #Name of the cell of origin of the analysis
                                     Target_Cell = "PDL1_pos_cell", #Name of the target cell of the analysis
                                     Stop_at = 300, #Final distance where cumulative interaction analysis will stop
                                     Sampling_frequency = 1, #Sampling distance interval
                                     Use_Clinical = F, #Specify if you desire to use clinical information in your pipeline
                                     DATA_Clinical = DATA_CLINICAL, #Provide clinical information if desired
                                     Clinical_var = "Clinical_benefit" #Specify the name of the variable to be used for clinical information analysis
)

ggplot(aes(x = r, y = km, group = Subject_Names, color = AUC), data = left_join(Gcross_Analysis[[1]],
                                                                                               Gcross_Analysis[[2]],
                                                                                               by = "Subject_Names")
) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous("Interaction probability \n PD1+/CD8+ => PDL1+ Cell") +
  scale_x_continuous("Distance (pixels)")+
  scale_color_viridis_c() +
  cowplot::theme_cowplot() +
  ggtitle("G-cross function analysis by sample")+ 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"),
        panel.grid = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 13, face = "bold"))


#Another approach based on cells in radius
DATA_Distances <- Distance_matrix_generator(N_cores = 1, 
                                            DATA = "DATA_Breast_2",
                                            Cell_Of_Origin = "CD8_PD1pos", 
                                            Target_Cell = "PDL1_pos_cell", 
                                            Allow_Cero_Distance = FALSE, 
                                            Perform_edge_correction = FALSE
                                            )  
DATA_Cumulative_Interaction <- Cumulative_Interaction_generator(N_cores = 3, #Number of cores to parallelize your computation
                                                                DATA = "DATA_Distances", #Name of the DATA_distance object to be used in the analysis
                                                                Start_from = 0, #Starting distance to calculate the cumulative interaction function
                                                                Stop_at = 300, #Final distance where cumulative interaction analysis will stop
                                                                Sampling_frequency = 25 #Sampling distance interval
)
RADIUS_analysis <- Cells_in_Radius_analyzer(DATA = DATA_Cumulative_Interaction, #Provide the data containing the cumulative interaction distances
                                            DATA_RANDOM = NULL, #Provide data containing the random cumulative interactions (optional)
                                            Radius = 100, #Select de radius of the analysis (must be contained in the Cumulative interaction sampling)
                                            Include_Random = F, #Decide if Random Data should be used to calculate significance level
                                            By_Sample_Random = F #Decide if Random distribution should be calculated by sample or by experiment
)

#Global heterogeneity analysis
Global_Heterogeneity_by_sample <- 
  Global_heterogeneity_calculator(DATA = DATA_Breast,#Specify your data containing the phenotypes
                                  Phenotypes_included = unique(DATA_Breast$Phenotype)#Select the phenotypes that will be used in the calculation
  )

Barplot_Heterogeneity_generator(DATA = Global_Heterogeneity_by_sample, #Specify your data source
                                Metric = "Shannon" #Specify the metric to be visualized
                                ) 


#We will perform a by tile heterogeneity analysis
#First calculate the potengial size of our tiles
Suggested_Tile_Size_Calculator(DATA_Breast, #Provide a data phenotypes object 
                               N_cols = 5, #Provide the number of desired columns 
                               N_rows = 5, #Provide the number of desired rows
                               Based_on_smaller = T, #Whether to base calculation on the smallest or the largest image
                               Draw_preview = T #Specify if you want a preview to be graphicated
)
#Tile your images according to your required parameters
Tiled_Images <- Image_tiling_processing_function(N_cores = 2, #Number of cores to parallelize your computation
                                                 DATA = "DATA_Breast", #Name of the DATA_phenotype object to be used in the analysis
                                                 Tile_width = 200, #Width of your tiles
                                                 Tile_height = 200, #height of your tiles
                                                 Variables_to_keep = "Phenotype" #Names of the variables to be preserved in the tiling process
)
#Calculate heterogeneity by Tile
Heterogeneity_by_tile <- Tiled_image_heterogeneity_calculator(Tiled_images = Tiled_Images, #Name of the object containing the tiled Images
                                                              Minimum_cell_no_per_tile = 3, #Minimum number of cells a tile should contain to be included in the analysis
                                                              Phenotypes_included = unique(DATA_Breast$Phenotype) #The phenotypes that must be taken into account
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


#Texture features analysis

#We will merge all Tumor cells into asingle label
DATA_Breast_2 <- DATA_Breast %>% 
  dplyr::mutate(Phenotype = case_when(Phenotype %in% c("TUMOR_PDL1low", "TUMOR_PDL1high", "TUMOR_PDL1neg") ~ "TUMOR",
                                      TRUE ~ Phenotype))


Tiled_Images_Tumor <- Image_tiling_processing_function(N_cores = 4, #Number of cores to parallelize your computation
                                                       DATA = "DATA_Breast_2", #Name of the DATA_phenotype object to be used in the analysis
                                                       Tile_width = 50, #Width of your tiles
                                                       Tile_height = 50, #height of your tiles
                                                       Variables_to_keep = "Phenotype" #Names of the variables to be preserved in the tiling process
)

#Now calculate the texture features of a specific cell type accross images
Texture_features_DATA <- Texture_features_calculator(Tiled_images = Tiled_Images_Tumor, #Provide a tiled image list
                                                     Phenotype_included = "TUMOR" #Specify the phenotype to be analyzed
)

Tiled_images_graphicator(Tiled_images = Tiled_Images_Tumor, #Provide a tiled image list
                         Image_name = "ABCM22001_B03.tif", #Specify the image you want to analyze
                         Phenotypes_included = "TUMOR" #Specify the cell types to be analyzed
)



#########Lymph Node DATASET#########
DATA_Lymphnode <- readRDS("Lymphnode_Dataset.rds")

#We will arrange our data keepin only quantile 0.975
DATA_Lymphnode <- Data_arrange_function(DATA = DATA_Lymphnode, #Name of data frame
                                        X = 'm.cx',  #Name of column specifying X coordinate
                                        Y = "m.cy", #Name of column specifying Y coordinate
                                        Subject_Names = "imageID", #Name of column specifying the image name for each cell
                                        Markers_to_keep = names(DATA_Lymphnode)[str_detect(names(DATA_Lymphnode), "q0975")]) 

#Check data quality
Data_QC_Check_function(DATA = DATA_Lymphnode)

#Generate a correlation matrix
Plot_correlation_matrix(DATA = DATA_Lymphnode, #Provide sample information data
                        Variables_included = names(DATA_Lymphnode)[-c(1:4)], #Provide the markers to be correlated
                        Correlation_method = "pearson" #Select one of "pearson" or "spearman"
)

#Select proteins that will be included in the analysis
DATA_list <- Data_set_aside(DATA = DATA_Lymphnode, #Choose your formatted data
                            Markers_to_set = 
                              names(DATA_Lymphnode)[!names(DATA_Lymphnode) %in% 
                                            c("Cell_no", "X", "Y", "Subject_Names",
                                              "CD3e_q0975", "CD8_q0975", "CD4_q0975", "CXCR5_q0975", #T cell markers
                                              "CD20_q0975", "CD138_q0975", #B cell and plasma cells
                                              "CD31_q0975", "Lyve1_q0975", #Vessels,
                                              "CD35_q0975", "CD107a_q0975", "CD21_q0975", #Dendritic cells 
                                              "FoxP3_q0975", #Tregs
                                              "CD11c_q0975", "CD15_q0975", #Myeloid
                                              "Ki67_q0975" #Other
                                            )]) 

DATA_use <- DATA_list$DATA
DATA_aside <- DATA_list$Aside

#tSNE is an stochastic method, therefore phenotype assignment may vary slightly between runs
DATA_Phenotypes <- Clustering_Phenotyper(DATA = DATA_use, #Marker intensity data as obtained from the 
                                         Strategy = "Graph_Based", #Decide one of the methods from Consensus_Clustering, SOM, Graph_Based
                                         #K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering
                                         
                                         #Denoising parameters
                                         Apply_Denoise = TRUE, #Specify if a denoising filtering is required before clustering
                                         Denoising = "DimRed_DBscan", #Select denoising strategy from: Quantile, Standard_Deviation, Threshold, Otsu or DimRed_DBscan
                                         Percentile = NULL, #Select the adequate percentile for quantile threshold
                                         N_Standard_Deviations = NULL, #Select the number of standard deviations from mean for Standard_Deviation method
                                         Selected_threshold = NULL, #Select the absolute threshold for the Threshold method
                                         Min_cell_no = 8, #Parameter for DBscan
                                         Distance_radius = 2, #Parameter for DBscan
                                         
                                         #Dimension reduction
                                         Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
                                         Dimension_reduction = "TSNE", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
                                         Dimension_reduction_prop = 0.5, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
                                         Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
                                         
                                         
                                         #Parameters for Graph-Based approaches
                                         Nearest_neighbors_for_graph = 40, #Specify the number of closest neighbors to calculate the graph
                                         Graph_Method = "Leiden", #One of Louvain, Leiden, Greedy, WalkTrap, Spinglass, Leading_Eigen or Edge_Betweenness
                                         Graph_Resolution = 0.75, #Used for Louvain and Leiden. 1 is default. The smaller the value, the larger the clusters will be
                                         N_steps = 5, #Number of steps given in the WalkTrap algorithm
                                         
                                         
                                         #Parameters for Batched K means
                                         Batch_size = 15000, #The number of cells to be included in each random batch
                                         Max_N_phenotypes_Batch = 25, #Number of maximum clusters (phenotypes) that you desire to find
                                         N_initiations = 100, #Number of times the algorithm is going to be tried to find the best clustering result
                                         Max_iterations = 100, #Max number of iterations in each try
                                         
                                         #Parameters for Gaussian Mixture Model
                                         Quality_metric = "AIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
                                         Max_N_phenotypes_GMM = 20, #Number of maximum clusters (phenotypes) that you desire to find
                                         Max_iterations_km = 500, #Number of max iterations in the K means clustering performed
                                         Max_iterations_em = 500, #Number of max iterations in the Expectation Maximization algorithm
                                         GMM_Distance = "eucl_dist"
)

#This was the original result from the associated paper
DATA_Phenotypes_CLUSTERING <- readRDS("Clustering_DATA_Phenotypes.rds")


#Rename the data according to marker expression
DATA_Phenotypes_CLUSTERING <- DATA_Phenotype_renamer(DATA = DATA_Phenotypes_CLUSTERING$DATA, #Specify the data created previously
                                                     New_names = c(
                                                       "1" = "OTHER",
                                                       "2" = "Bcells_GC", 
                                                       "3" = "Bcells_GC",
                                                       "4" = "T_regs",
                                                       "5" = "Bcells",
                                                       "6" = "T_helper",
                                                       "7" = "DC",
                                                       "8" = "Plasma_cells",
                                                       "9" = "Bcells",
                                                       "10" = "T_helper",
                                                       "11" = "Bcells",
                                                       "12" = "T_helper",
                                                       "13" = "T_cytotoxic",
                                                       "14" = "T_helper",
                                                       "15" = "Endothelial",
                                                       "16" = "T_regs",
                                                       "17" = "Bcells",
                                                       "18" = "T_cytotoxic",
                                                       "19" = "Myeloid",
                                                       "20" = "T_cytotoxic",
                                                       "21" = "Myeloid",
                                                       "22" = "T_helper",
                                                       "23" = "T_helper",
                                                       "24" = "T_cytotoxic",
                                                       "25" = "T_cytotoxic",
                                                       "26" = "T_helper",
                                                       "27" = "T_cytotoxic",
                                                       "28" = "OTHER",
                                                       "29" = "Myeloid",
                                                       "30" = "OTHER",
                                                       "31" = "OTHER")
)

Image_plotter(DATA = DATA_Phenotypes_CLUSTERING, #Your phenotype data source
              Image_name = "reg001", #Name of the image to be graphicated
              Phenotypes_included = unique(DATA_Phenotypes_CLUSTERING$Phenotype)[-which(unique(DATA_Phenotypes_CLUSTERING$Phenotype) == "OTHER")] #which phenotypes should be included in the graph
)

CELESTA_template_generator(DATA = DATA_Lymphnode,
                           Markers_to_keep = c("CD3e_q0975", "CD8_q0975", "CD4_q0975", "CXCR5_q0975", #T cell markers
                                               "CD20_q0975", "CD138_q0975", #B cell and plasma cells
                                               "CD31_q0975", "Lyve1_q0975", #Vessels,
                                               "CD35_q0975", "CD107a_q0975", "CD21_q0975", #Dendritic cells 
                                               "FoxP3_q0975", #Tregs
                                               "CD11c_q0975", "CD15_q0975", #Myeloid
                                               "Ki67_q0975"), #Other,
                           Template_name = "CODEX_LymphNode_template")

#Then execute the CELESTA algorithm
DATA_Phenotypes_CELESTA <- CELESTA_phenotyper(DATA = DATA_Lymphnode, #DATA containing the marker expression data
                                              Template_path = "CODEX_LymphNode_template.csv", #Path to the template
                                              Alternative_CSV_locale = TRUE, #Should a European locale be used when importing the template?
                                              N_cores = 1, #Number of cores to parallelize your computation
                                              
                                              Apply_filters = TRUE, #Should cells be filtered before runing the algorithm
                                              high_marker_threshold = 0.95, #Cells with expression above this quantile for all markers will not be phenotyped
                                              low_marker_threshold = 0.05, #Cells with expression below this quantile for all markers will not be phenotyped
                                              
                                              max_iteration = 100, #Max number of iterations to run the GMM algorithm unterlying CELESTA
                                              cell_change_threshold = 0.01 #when the iterative cell-type assignment stops. 
                                              #The default value is 0.01, 
                                              #which means that if the percentage of additional assigned cells is smaller than 1% of the unassigned cells, then cell-type assignment will stop. 
                                              #The recommended range is 0.01 - 0.05. 
                                              #Note that the higher the cell change threshold, the more cells are left unassigned.
)

Image_plotter(DATA = DATA_Phenotypes_CELESTA, #Your phenotype data source
              Image_name = "reg001", #Name of the image to be graphicated
              Phenotypes_included = unique(DATA_Phenotypes_CELESTA$Phenotype) #which phenotypes should be included in the graph
)


#To compare both samples we will change labels of CELESTA
DATA_Phenotypes_CELESTA <- DATA_Phenotypes_CELESTA %>% dplyr::mutate(Phenotype = case_when(Phenotype == "B_cell_GC" ~ "Bcells_GC",
                                                                                           
                                                                                           Phenotype == "B_cell_Non_GC" ~ "Bcells",
                                                                                           Phenotype == "T_reg" ~ "T_regs",
                                                                                           
                                                                                           Phenotype == "Plasma_cell" ~ "Plasma_cells",
                                                                                           Phenotype == "Unknown" ~ "OTHER",
                                                                                           Phenotype == "Endothelial_cells" ~ "Endothelial",
                                                                                           is.na(Phenotype) ~ "OTHER",
                                                                                           TRUE ~ Phenotype))

Concordance_calculator(Clustering = DATA_Phenotypes_CLUSTERING, 
                       CELESTA = DATA_Phenotypes_CELESTA,  
                       Variable = "Phenotype", #Choose the variable name that contains the phenotypes
                       Strategy = "Rand" #Choose betweeen Rand and FM (Fowlkes-Mallows)
)



#Neighborhood approach 1 => 20 Closest neighbors
Neighbors_20closest <- 
  Tailored_Closest_neighbor_calculator(N_cores = 5, 
                                       DATA = DATA_Phenotypes_CLUSTERING, 
                                       Strategy = "Number", 
                                       N_neighbors = 20, 
                                       Include_COO_in_neighborhood = TRUE, 
                                       Max_dist_allowed = NULL, 
                                       Cell_Of_Origin = unique(DATA_Phenotypes_CLUSTERING$Phenotype)[-which(unique(DATA_Phenotypes_CLUSTERING$Phenotype) == "OTHER")], 
                                       Target_Cell = unique(DATA_Phenotypes_CLUSTERING$Phenotype)[-which(unique(DATA_Phenotypes_CLUSTERING$Phenotype) == "OTHER")]
                                       )

Neighbors_20closest <- Neighbors_20closest$Absolute_count

#This process is also stochastic, results may vary slightly from run to run
Neighborhoods_20closest <- 
  Neighborhood_discovery_function(
    DATA = Neighbors_20closest, #Provide the data containing the Closest neighbor information
    Allowed_max_Dist = 90, #Provide de threshold to eliminate cells whose neighbors are too far away
    Allowed_avg_Dist = 60, #Provide de threshold to eliminate cells whose neighbors are too far away
    Allowed_median_Dist = 60, #Provide de threshold to eliminate cells whose neighbors are too far away
    
    #Dimension reduction
    Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
    Dimension_reduction = "UMAP", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
    Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
    Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
    
    #Clustering strategy
    Strategy = "Batch_K_means", #Choose your clustering method (either Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
    
    #Parameters for Batched K means
    Batch_size = 10000, #The number of cells to be included in each random batch
    Max_N_neighborhoods_Batch = 5, #Number of maximum clusters (neighborhoods) that you desire to find
    Percentage_centroid_initiation = 1,
    N_initiations = 200, #Number of times the algorithm is going to be tried to find the best clustering result
    Max_iterations = 100 #Max number of iterations in each try
  )
Neighborhoods_20closest$DATA %>% ggplot(aes(x = X, y = Y, color = Neighborhood_assignment)) + geom_point()


#Neighborhood approach 2 => 20 pixels Closest neighbors
Neighbors_20microns <- 
  Tailored_Closest_neighbor_calculator(N_cores = 10, #Number of cores to parallelize your computation
                                       DATA = DATA_Phenotypes_CLUSTERING, #Provide the data containing the Cell Type information
                                       Strategy = "Distance", #Specify the strategy to consider two cells to be neighbors between Number, Distance, Both
                                       N_neighbors = NULL, #Specify the number of closest neighbors to include in the analysis
                                       Include_COO_in_neighborhood = TRUE, #Specify if the Cell of Origin should be included in neighbor counting
                                       Max_dist_allowed = 20, #Specify the max Distance to define neighborhoods
                                       Cell_Of_Origin = unique(DATA_Phenotypes_CLUSTERING$Phenotype)[-which(unique(DATA_Phenotypes_CLUSTERING$Phenotype) == "OTHER")], #Specify the cells of origin to include in the analysis
                                       Target_Cell = unique(DATA_Phenotypes_CLUSTERING$Phenotype)[-which(unique(DATA_Phenotypes_CLUSTERING$Phenotype) == "OTHER")]
  ) 

Neighbors_20microns <- Neighbors_20microns$Absolute_count
#This process is also stochastic, results may vary slightly from run to run
Neighborhoods_20microns <- 
  Neighborhood_discovery_function(
    DATA = Neighbors_20microns, #Provide the data containing the Closest neighbor information
    Allowed_max_Dist = 30, #Provide de threshold to eliminate cells whose neighbors are too far away
    Allowed_avg_Dist = 30, #Provide de threshold to eliminate cells whose neighbors are too far away
    Allowed_median_Dist = 30, #Provide de threshold to eliminate cells whose neighbors are too far away
    
    #Dimension reduction
    Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
    Dimension_reduction = "UMAP", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
    Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
    Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
    
    #Clustering strategy
    Strategy = "Batch_K_means", #Choose your clustering method (either Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
    
    #Parameters for Batched K means
    Batch_size = 10000, #The number of cells to be included in each random batch
    Max_N_neighborhoods_Batch = 5, #Number of maximum clusters (neighborhoods) that you desire to find
    Percentage_centroid_initiation = 1,
    N_initiations = 200, #Number of times the algorithm is going to be tried to find the best clustering result
    Max_iterations = 100 #Max number of iterations in each try
  )
Neighborhoods_20microns$DATA %>% ggplot(aes(x = X, y = Y, color = Neighborhood_assignment)) + geom_point()



#Neighborhood approach 3 => UTAG 50 pixels
DATA_UTAG <- UTAG_message_passing(DATA = "DATA_use",#Provide the data with marker expression information (a character scalar)
                                  COO_to_visit = NULL, #Should the algorithm perform message passing in all the cells of the dataset?
                                  #This can be NULL or it a can be a logical vector specifying which cells to visit
                                  Neighbor_strategy = "Distance", #How are neighbors defined. They can be defined by Number, Distance or Both
                                  Message_strategy = "Sum", #Averaging (it will average the expression of markers between the COO and the neighbors),
                                  #Sum (it will sum the marker expression data of the COO and the neighbors)
                                  N_neighbors = NULL, #Number of closest neighbors to each COO to be considered
                                  Max_dist_allowed = 50, #Specify the max distance to consider two cells as neighbors
                                  
                                  Weighting_Strategy = "Proximity", #Should message passing be weighted (4 options: None, Proximity, Disregarded_minority or Both)
                                  #None: no weighting
                                  #Proximity: Neighbors closer to the COO will have higher influence compared to more distant neighbors
                                  #Disregarded_minority: Markers with low expression in the dataset will be overrepresented in their neighbors
                                  #Both: Perform both weighting strategies
                                  COO_weight = NULL, #Decide the weight to assign to the Cell of Origin (can be NULL (weight according to number of neighbors) or a numeric value from 0 to 1) when it mixes with their neighbors
                                  
                                  N_cores = 5 #The number of cores to perform the computation
)
#This process is also stochastic, results may vary slightly from run to run
Neighborhoods_UTAG <- UTAG_Neighborhood_identifier(DATA = DATA_UTAG, #Provide Message passed cells
                                                   Strategy = "Batch_K_means", #Decide one of the methods from Consensus_Clustering, SOM, Graph_Based
                                                   #K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering
                                                   Min_Neighbors = 0, #Select cells with at least a minimum number of neighbors
                                                   
                                                   #Denoising parameters
                                                   Apply_Denoise = FALSE, #Specify if a denoising filtering is required before clustering
                                                   Denoising = "DimRed_DBscan", #Select denoising strategy from: Quantile, Standard_Deviation, Threshold, Otsu or DimRed_DBscan
                                                   Percentile = NULL, #Select the adequate percentile for quantile threshold
                                                   N_Standard_Deviations = NULL, #Select the number of standard deviations from mean for Standard_Deviation method
                                                   Selected_threshold = NULL, #Select the absolute threshold for the Threshold method
                                                   Min_cell_no = 10, #Parameter for DBscan
                                                   Distance_radius = 0.25, #Parameter for DBscan
                                                   
                                                   #Dimension reduction
                                                   Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
                                                   Dimension_reduction = "UMAP", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
                                                   Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
                                                   Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
                                                   
                                                   #Parameters for Batched K means
                                                   Batch_size = 10000, #The number of cells to be included in each random batch
                                                   Max_N_neighborhoods_Batch = 5, #Number of maximum clusters (neighborhoods) that you desire to find
                                                   Percentage_centroid_initiation = 1,#Percentage of cells (from 0 to 1) that will be used in algorithm initiation
                                                   N_initiations = 200, #Number of times the algorithm is going to be tried to find the best clustering result
                                                   Max_iterations = 100 #Max number of iterations in each try
                                                   
)
Neighborhoods_UTAG$DATA %>% ggplot(aes(x = X, y = Y, color = Neighborhood_assignment)) + geom_point()


#Neighborhood approach 4 => Tiling
Tiled_Images <- Image_tiling_processing_function(N_cores = 1, #Number of cores to parallelize your computation
                                                 DATA = "DATA_Phenotypes_CLUSTERING", #Name of the DATA_phenotype object to be used in the analysis (A CHARACTER SCALAR)
                                                 Tile_width = 150, #Width of your tiles
                                                 Tile_height = 150, #height of your tiles
                                                 Variables_to_keep = "Phenotype" #Names of the variables to be preserved in the tiling process
)

#Find recurrent patterns in our tiles
Clustered_Tiled_Images <- Tiled_Image_Clustering_function(Tiled_images = Tiled_Images, #Provide a list with tiled images
                                                          Minimum_cell_no_per_tile = 5, #State the min number of cells per tile
                                                          Minimum_valid_tiles_per_image = 10, #State the min number of tiles per image
                                                          Phenotypes_included = unique(DATA_Phenotypes_CLUSTERING$Phenotype)[-which(unique(DATA_Phenotypes_CLUSTERING$Phenotype) == "OTHER")], #Provide the cell phenotypes included in the analysis
                                                          
                                                          #Cluster based on tile cell density or tile cell percentage
                                                          Cluster_Data = "Cell_Density", #Choose between Cell_Density or Cell_Percentage
                                                          
                                                          #Dimension reduction
                                                          Perform_Dimension_reduction = TRUE, #Should Dimension Reduction be performed
                                                          Dimension_reduction = "UMAP", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
                                                          Dimension_reduction_prop = 1, #For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
                                                          Cluster_on_Reduced = TRUE, #Should subsequent clustering be performed on reduced data
                                                          
                                                          #Clustering strategy
                                                          Strategy = "Batch_K_means", #Choose your clustering method (either Consensus_Clustering, SOM, Graph_Based
                                                          #K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
                                                          
                                                          #Parameters for Batched K means
                                                          Batch_size = 250, #The number of cells to be included in each random batch
                                                          Max_N_Clusters_Batch = 5, #Number of maximum clusters (neighborhoods) that you desire to find
                                                          Percentage_centroid_initiation = 1,
                                                          N_initiations = 200, #Number of times the algorithm is going to be tried to find the best clustering result
                                                          Max_iterations = 100 #Max number of iterations in each try
                                                          
)

Clustered_Tiled_Images_analysis <- Clustered_Tiled_Images_analyzer(Tiled_images = Clustered_Tiled_Images, #Provide the clustered tiled images
                                                                   Perform_heterogeneity_analysis = T, #Select if heterogeneity analysis should be performed
                                                                   Graph_Modularity_resolution = 0.5 #Provide graph modularity index
)

#Each cell is assigned to the label of its tile
Cells_by_Tiles <- Tiled_Images[[1]][[2]] %>% dplyr::left_join(Clustered_Tiled_Images[[1]] %>% dplyr::select(tile_id, Cluster_assignment), by = "tile_id")



#We will only compare cells that are present in all 4 analyses (Tiling drops cells in tiles with less than 5 cells)
Final_cells <- Reduce(intersect, list(Neighborhoods_20closest$DATA$Cell_no, 
                                      Neighborhoods_20microns$DATA$Cell_no,
                                      Neighborhoods_UTAG$DATA$Cell_no,
                                      Cells_by_Tiles$Cell_no))
Cells_by_Tiles <- Cells_by_Tiles %>% dplyr::rename("Neighborhood_assignment" = "Cluster_assignment")

  
#Calculate concordance between methods
Concordance_plot <- Concordance_calculator(K20_Neighborhoods = Neighborhoods_20closest$DATA %>% dplyr::filter(Cell_no %in% Final_cells), 
                                           micron20_Neighborhoods = Neighborhoods_20microns$DATA %>% dplyr::filter(Cell_no %in% Final_cells), 
                                           UTAG = Neighborhoods_UTAG$DATA %>% dplyr::filter(Cell_no %in% Final_cells),
                                           Tiling =  Cells_by_Tiles %>% dplyr::filter(Cell_no %in% Final_cells),
                                           Variable = "Neighborhood_assignment", #Choose the variable name that contains the phenotypes
                                           Strategy = "Rand" #Choose betweeen Rand and FM (Fowlkes-Mallows)
)

#########Colorectal Carcinoma DATASET#########

#Import original data
DATA_CRC <- readRDS("CRC_Dataset.rds")
#Format
DATA_CSM <- Data_arrange_function(DATA = DATA_CRC, 
                                  X = "X:X",  
                                  Y = "Y:Y", 
                                  Subject_Names = "File Name", 
                                  Markers_to_keep = c("ClusterName", "neighborhood10", "neighborhood name"))

#Select subset for neighbor matrix calculation
DATA_CSM_Phenotypes <- DATA_CSM %>% dplyr::select(1:4, ClusterName) %>% dplyr::rename("Phenotype" = "ClusterName")
#Calculate 10 closest neighbors matrix
DATA_Closest_Neighbors <- 
  Tailored_Closest_neighbor_calculator(N_cores = 8, #Number of cores to parallelize your computation
                                       DATA = DATA_CSM_Phenotypes, #Provide the data containing the Cell Type information
                                       Strategy = "Number", #Specify the strategy to consider two cells to be neighbors between Number, Distance, Both
                                       N_neighbors = 10, #Specify the number of closest neighbors to include in the analysis
                                       Include_COO_in_neighborhood = TRUE, #Specify if the Cell of Origin should be included in neighbor counting
                                       Max_dist_allowed = NULL, #Specify the max Distance to define neighborhoods
                                       Cell_Of_Origin = unique(DATA_CSM_Phenotypes$Phenotype), #Specify the cells of origin to include in the analysis
                                       Target_Cell = unique(DATA_CSM_Phenotypes$Phenotype)) #Specify the target cells to include in the analysis

#This process is stochastic, results may vary slightly from run to run
DATA_neighborhoods_CSM <- Neighborhood_discovery_function(
  DATA = DATA_Closest_Neighbors$Absolute_count, #Provide the data containing the Closest neighbor information
  Allowed_max_Dist = 10000, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_avg_Dist = 10000, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_median_Dist = 10000, #Provide de threshold to eliminate cells whose neighbors are too far away
  
  #Dimension reduction
  Perform_Dimension_reduction = FALSE, #Should Dimension Reduction be performed
  Dimension_reduction = "TSNE", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
  Dimension_reduction_prop = 0.1,#For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
  Cluster_on_Reduced = FALSE, #Should subsequent clustering be performed on reduced data
  
  #Clustering strategy
  Strategy = "Batch_K_means", #Choose your clustering method (either Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
  
  #Parameters for Batched K means
  Batch_size = 1024, #The number of cells to be included in each random batch
  Max_N_neighborhoods_Batch = 10, #Number of maximum clusters (neighborhoods) that you desire to find
  Percentage_centroid_initiation = 1,
  N_initiations = 250, #Number of times the algorithm is going to be tried to find the best clustering result
  Max_iterations = 100 #Max number of iterations in each try
)

#Generate a tibble with the original labels
Original_neighborhood <- DATA_CSM %>% rename("Neighborhood_assignment" = "neighborhood10")

#Calculate concordance
Concordance_calculator(ORIGINAL = Original_neighborhood, 
                       CSM = DATA_neighborhoods_CSM,  
                       Variable = "Neighborhood_assignment", #Choose the variable name that contains the phenotypes
                       Strategy = "Rand" #Choose betweeen Rand and FM (Fowlkes-Mallows)
)


#########Human skin DATASET#########
DATA_Phenotypes <- readRDS("SKIN_Dataset.rds")

DATA_Closest_Neighbors <-
  Tailored_Closest_neighbor_calculator(N_cores = 4, #Number of cores to parallelize your computation
                                       DATA = DATA_Phenotypes$DATA, #Provide the data containing the Cell Type information
                                       Strategy = "Distance", #Specify the strategy to consider two cells to be neighbors between Number, Distance, Both
                                       N_neighbors = NULL, #Specify the number of closest neighbors to include in the analysis
                                       Include_COO_in_neighborhood = TRUE, #Specify if the Cell of Origin should be included in neighbor counting
                                       Max_dist_allowed = 60, #Specify the max Distance to define neighborhoods
                                       Cell_Of_Origin = unique(DATA_Phenotypes$DATA$Phenotype)[-14], #Specify the cells of origin to include in the analysis (OTHER will be removed)
                                       Target_Cell = unique(DATA_Phenotypes$DATA$Phenotype)[-14]) #Specify the target cells to include in the analysis (OTHER will be removed)


DATA_neighborhoods <- Neighborhood_discovery_function(
  DATA = DATA_Closest_Neighbors$Absolute_count, #Provide the data containing the Closest neighbor information
  Allowed_max_Dist = 80, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_avg_Dist = 80, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_median_Dist = 80, #Provide de threshold to eliminate cells whose neighbors are too far away
  
  #Dimension reduction
  Perform_Dimension_reduction = FALSE, #Should Dimension Reduction be performed
  Dimension_reduction = "TSNE", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
  Dimension_reduction_prop = 0.5,#For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
  Cluster_on_Reduced = FALSE, #Should subsequent clustering be performed on reduced data
  
  #Clustering strategy
  Strategy = "GMM", #Choose your clustering method (either Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
  
  #Parameters for Gaussian Mixture Model
  Quality_metric = "AIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
  Max_N_neighborhoods_GMM = 25, #Number of maximum clusters (phenotypes) that you desire to find
  Max_iterations_km = 500, #Number of max iterations in the K means clustering performed
  Max_iterations_em = 500, #Number of max iterations in the Expectation Maximization algorithm
  GMM_Distance = "eucl_dist" #Distance metric to use in the model ("eucl_dist" or "maha_dist")
)

#########Human lung DATASET#########
DATA_Phenotypes <- readRDS("LUNG_Dataset.rds")


DATA_Closest_Neighbors <-
  Tailored_Closest_neighbor_calculator(N_cores = 4, #Number of cores to parallelize your computation
                                       DATA = DATA_Phenotypes$DATA, #Provide the data containing the Cell Type information
                                       Strategy = "Distance", #Specify the strategy to consider two cells to be neighbors between Number, Distance, Both
                                       N_neighbors = NULL, #Specify the number of closest neighbors to include in the analysis
                                       Include_COO_in_neighborhood = TRUE, #Specify if the Cell of Origin should be included in neighbor counting
                                       Max_dist_allowed = 40, #Specify the max Distance to define neighborhoods
                                       Cell_Of_Origin = unique(DATA_Phenotypes$DATA$Phenotype)[-3], #Specify the cells of origin to include in the analysis (OTHER will be removed)
                                       Target_Cell = unique(DATA_Phenotypes$DATA$Phenotype)[-3]) #Specify the target cells to include in the analysis (OTHER will be removed)

DATA_neighborhoods <- Neighborhood_discovery_function(
  DATA = DATA_Closest_Neighbors$Absolute_count, #Provide the data containing the Closest neighbor information
  Allowed_max_Dist = 80, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_avg_Dist = 80, #Provide de threshold to eliminate cells whose neighbors are too far away
  Allowed_median_Dist = 80, #Provide de threshold to eliminate cells whose neighbors are too far away
  
  #Dimension reduction
  Perform_Dimension_reduction = FALSE, #Should Dimension Reduction be performed
  Dimension_reduction = "TSNE", #Strategy for dimension reduction, one of the following PCA, TSNE, UMAP
  Dimension_reduction_prop = 0.5,#For TSNE and UMAP, reduction model can be calculated in a subset of the data and generalized to the whole dataset afterwrards. Improved efficiency for very large datasets
  Cluster_on_Reduced = FALSE, #Should subsequent clustering be performed on reduced data
  
  #Clustering strategy
  Strategy = "GMM", #Choose your clustering method (either Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering)
  
  #Parameters for Gaussian Mixture Model
  Quality_metric = "AIC", #The quality measure used to test the number of clusters ("AIC" or "BIC")
  Max_N_neighborhoods_GMM = 15, #Number of maximum clusters (phenotypes) that you desire to find
  Max_iterations_km = 500, #Number of max iterations in the K means clustering performed
  Max_iterations_em = 500, #Number of max iterations in the Expectation Maximization algorithm
  GMM_Distance = "eucl_dist" #Distance metric to use in the model ("eucl_dist" or "maha_dist")
)

  