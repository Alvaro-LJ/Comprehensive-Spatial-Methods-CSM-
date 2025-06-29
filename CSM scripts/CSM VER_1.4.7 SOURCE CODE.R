##############BASIC DEPENDENCIES###############
message("CSM - Comprehensive Spatial Methods - Version 1.4.7")
message("Loading required packages...")
#Are required to be executed every time the master sheet is used
library(tidyverse)
library(compiler)

############STEP 0 - DATA GENERATION / ARRANGE- REQUIRED FUNCTIONS###########
message("Importing functions: STEP 0 - DATA GENERATION")
Describe_my_computing_unit <- cmpfun(
  function() {
    if(stringr::str_detect(benchmarkme::get_platform_info()$OS.type, "windows")){
      list(
        CPU =  paste0("CPU: ", benchmarkme::get_cpu()$model_name, ". Number of available threads (cores): ", benchmarkme::get_cpu()$no_of_cores),
        RAM =  paste0("Total available RAM memory: approximately ", 
                      round(
                        as.double(substr(shell('wmic path Win32_ComputerSystem get TotalPhysicalMemory/value', translate = F, intern = TRUE)[[3]], 
                                         start = 21,
                                         stop = nchar(shell('wmic path Win32_ComputerSystem get TotalPhysicalMemory/value', translate = F, intern = TRUE)[[3]])-2))/104857600, 
                        digits = 2),
                      " Gb"),
        R_VERSION =  paste0(benchmarkme::get_r_version()$version.string, " - ", 
                            benchmarkme::get_r_version()$nickname, ". Version type: ", benchmarkme::get_r_version()$arch),
        OS = paste0("You are running R on ", benchmarkme::get_platform_info()$OS.type)
      )
    }
    
    else (
      benchmarkme::get_sys_details(byte_compiler = F, linear_algebra = F,  locale = F, installed_packages = F, machine = F)
    )
  },
  options = list(optimize = 3))

EBImage_color_thresholder <- cmpfun(function(Image = NULL, 
                                             Target = c(1, 1, 1),
                                             Color_Tolerance = NULL,
                                             Final_Tolerance = NULL){
  #Check arguments
  if(!EBImage::is.Image(Image)) stop("Image must be of class EBImage")
  if(!all(is.integer(as.integer(Target)), length(Target) == 3)) stop("Target must contain the target color information in RGB format")
  if(!all(map_lgl(Target, function(value) value >= 0 & value <= 255))) stop("Target must be of the 8-bit RGB format")
  if(!all(is.numeric(Color_Tolerance), length(Color_Tolerance) == 3, all(Color_Tolerance >= 0), all(Color_Tolerance <= 1))) stop("Color_Tolerance must be a numeric vector of length = 3 with numeric values between 0 and 1")
  if(!all(is.numeric(Final_Tolerance), Final_Tolerance >= 0, Final_Tolerance <= 1))stop("Final_Tolerance must be a single numeric value between 0 and 1")
  
  #Target colors are transformed to 0-1 scale
  RED <- Target[[1]]/255
  GREEN <- Target[[2]]/255
  BLUE <- Target[[3]]/255
  
  #Extract each channel
  RED_channel <- EBImage::channel(Image, "red")
  GREEN_channel <- EBImage::channel(Image, "green")
  BLUE_channel <- EBImage::channel(Image, "blue")
  
  #Each channel will be the difference between the value and the target according to tolerance
  RED_channel <- 1 - abs(RED_channel - RED)
  RED_channel[RED_channel < 1-Color_Tolerance[[1]]] <- 0
  
  GREEN_channel <- 1 - abs(GREEN_channel - GREEN)
  GREEN_channel[GREEN_channel < 1-Color_Tolerance[[2]]] <- 0
  
  BLUE_channel <- 1 - abs(BLUE_channel - BLUE)
  BLUE_channel[BLUE_channel < 1-Color_Tolerance[[3]]] <- 0
  
  
  #Merge channels to a grey colorscale
  Final_image <- 
    EBImage::channel(EBImage::rgbImage(red = RED_channel,
                                       green = GREEN_channel,
                                       blue = BLUE_channel),
                     mode = "grey") 
  Final_image[Final_image < 1-Final_Tolerance] <- 0
  
  #return the final EBImage object
  return(Final_image)
},
options = list(optimize = 3))

Channel_deconvolution_function <- cmpfun(function(Image = NULL,
                                                  Parameters = NULL){
  Photo <- Image
  
  #Apply required processing
  Photo <- Photo %>% magick::image_modulate(brightness = Parameters[["Brightness"]], saturation = Parameters[["Saturation"]], hue = Parameters[["Hue"]])
  if(Parameters[["Equalize"]]) Photo <- Photo %>% magick::image_equalize()
  if(Parameters[["Normalize"]]) Photo <- Photo %>% magick::image_normalize()
  if(Parameters[["Contrast"]]) Photo <- Photo %>% magick::image_contrast(sharpen = Parameters[["Sharpen"]])
  if(Parameters[["Reduce_color"]]) Photo <- Photo %>% magick::image_quantize(max = Parameters[["Max_color"]], dither = TRUE)
  
  #Exctract color
  #Transform to EBI object
  Photo <- Photo %>% magick::image_convert(type = NULL,
                                           colorspace = "RGB",
                                           depth = 8,
                                           antialias = NULL,
                                           matte = FALSE) %>% 
    magick::as_EBImage() %>% 
    EBImage::toRGB()
  #Extract Image
  Photo <- EBImage_color_thresholder(Image = Photo, 
                                     Target = c(Parameters[["RED_value"]], Parameters[["GREEN_value"]], Parameters[["BLUE_value"]]),
                                     Color_Tolerance = Parameters[["Color_Tolerance"]],
                                     Final_Tolerance = Parameters[["Final_Tolerance"]]
  )
  #Apply post processing if required
  #Apply transformations before erosion and dilation
  if(Parameters[["Post_normalize"]]) Photo <- magick::image_read(Photo) %>% magick::image_normalize() %>% magick::as_EBImage()
  if(Parameters[["Post_equalize"]]) Photo <- magick::image_read(Photo) %>% magick::image_equalize() %>% magick::as_EBImage()
  
  #if erosion is required then execute the rounds of erosion the user requires
  if(Parameters[["Erode"]]){
    Photo <- purrr::reduce(
      .x = seq_along(1:Parameters[["Erode_rounds"]]),
      .f = function(img, ...) EBImage::erode(img, kern = EBImage::makeBrush(Parameters[["Erode_kern"]], "disc")),
      .init = Photo
    )
  }
  
  
  #If dilation is required then execute the dilation rounds
  if(Parameters[["Dilate"]]){
    Photo <- purrr::reduce(
      .x = seq_along(1:Parameters[["Dilate_rounds"]]),
      .f = function(img, ...) EBImage::dilate(img, kern = EBImage::makeBrush(Parameters[["Dilate_kern"]], "disc")),
      .init = Photo
    )
  }
  
  return(Photo)
},options = list(optimize = 3))

Color_deconvolution_App_launcher <- cmpfun(
  function(Directory = NULL){
    
    #Run a gc after exiting because it consumes a load of RAM
    on.exit(gc())
    
    #Obtain image names and channel names
    Real_Images <- dir(Directory, full.names = FALSE)
    
    #BUILD THE USER INTERFACE
    {
      user_interface <- shiny::fluidPage(
        
        #Set the title
        shiny::titlePanel("Image color deconvolution APP"),
        
        #We want a two panel layout, one in the left containing the input parameters and the output in the right
        shiny::sidebarLayout(
          #Set the first column (which contains the user defined parameters)
          shiny::sidebarPanel(
            #ID and width
            id="sidebar",
            
            #Select the image to be tested  
            shiny::fluidRow(
              shiny::column(3, shiny::selectInput("Real_Image_name", "Image to display", sort(Real_Images), multiple = FALSE)),
              shiny::column(3, shiny::textInput("Channel_name", "Channel name", value = "Channel_1")),
              shiny::column(2, shiny::selectInput("Res", "Image Res", c(Low = 150, Mid = 300, high = 600, Original = 1000), selected = 150, multiple = FALSE)),
              shiny::column(2, shiny::actionButton("ADD_button", shiny::icon("plus", library = "font-awesome"), label = "Add")),
              shiny::column(2, shiny::actionButton("Remove", shiny::icon("minus", library = "font-awesome"), label = "Remove")),
            ),
            
            shiny::h4("Initial Pre-processing"),
            #Select basic image features
            shiny::fluidRow(
              shiny::column(4, shiny::sliderInput("Brightness", "Brightness", value = 100, min = 0, max = 100, step = 1)),
              shiny::column(4, shiny::sliderInput("Saturation", "Saturation", value = 100, min = 0, max = 100, step = 1)),
              shiny::column(4, shiny::sliderInput("Hue", "Hue", value = 100, min = 0, max = 100, step = 1))
            ),
            
            #Normalize, Equalize, Contrast and color reduction
            shiny::fluidRow(
              shiny::column(2, shinyWidgets::materialSwitch("Normalize", "Normalize", value = FALSE)),
              shiny::column(2, shinyWidgets::materialSwitch("Equalize", "Equalize", value = FALSE)),
              shiny::column(2, shinyWidgets::materialSwitch("Contrast", "Contrast", value = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Contrast == '1'",
                                                       shiny::numericInput("Sharpen", "Sharpen", value = 1, min = 1, max = NA, step = 1)
              )),
              shiny::column(2, shinyWidgets::materialSwitch("Quantize", "Colors", value = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Quantize == '1'",
                                                       shiny::numericInput("Max_colors", "Max", value = 100, min = 1, max = NA, step = 1)
              ))
              
            ),
            
            #Target color 
            shiny::h4("Color targeting"),
            shiny::fluidRow(
              shiny::column(3, shiny::textInput("Color_text", "Color", value = "#ffffff")),
              shiny::column(2, shiny::numericInput("RED", "RED", value = 255, min = 0, max = 255, step = 1)),
              shiny::column(2, shiny::numericInput("GREEN", "GREEN", value = 255, min = 0, max = 255, step = 1)),
              shiny::column(2, shiny::numericInput("BLUE", "BLUE", value = 255, min = 0, max = 255, step = 1)),
              shiny::column(1, shiny::plotOutput("Target", width = 45, height = 45, inline = FALSE))
            ),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("Tolerance_RED", "Red Tol", value = 0.1, min = 0, max = 1, step = 0.01)),
              shiny::column(3, shiny::numericInput("Tolerance_GREEN", "Green Tol", value = 0.1, min = 0, max = 1, step = 0.01)),
              shiny::column(3, shiny::numericInput("Tolerance_BLUE", "Blue Tol", value = 0.1, min = 0, max = 1, step = 0.01)),
              shiny::column(3, shiny::numericInput("Tolerance_Final", "End Tol", value = 0.1, min = 0, max = 1, step = 0.01))
            ),
            
            #Opening and closing
            shiny::h4("Final Processing"),
            #Normalize and equalize
            shiny::fluidRow(
              shiny::column(2, shinyWidgets::materialSwitch("Post_Normalize", "Normalize", value = FALSE)),
              shiny::column(2, shinyWidgets::materialSwitch("Post_Equalize", "Equalize", value = FALSE)),
            ),
            
            #Erode dilate
            shiny::fluidRow(
              shiny::column(2, shinyWidgets::materialSwitch("Erode", "Erode", value = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Erode == '1'",
                                                       shiny::numericInput("Erode_kern", "Kern", value = 1, min = 1, max = NA, step = 1)
              )),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Erode == '1'",
                                                       shiny::numericInput("Erode_round", "Rounds", value = 1, min = 1, max = NA, step = 1)
              )),
              shiny::column(2, shinyWidgets::materialSwitch("Dilate", "Dilate", value = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Dilate == '1'",
                                                       shiny::numericInput("Dilate_kern", "Kern", value = 1, min = 1, max = NA, step = 1)
              )),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Dilate == '1'",
                                                       shiny::numericInput("Dilate_round", "Rounds", value = 1, min = 1, max = NA, step = 1)
              ))
            ),
            
            shiny::h4("Parameter results"),
            #Buttons to add or remove
            shiny::fluidRow(
              shiny::column(3, shiny::actionButton("Download_Param", shiny::icon("download"), label = "Download Parameters"))
            ),
            
            #The current results
            shiny::fluidRow(
              shiny::column(12, shiny::verbatimTextOutput("Result_list"))
            )
          ),
          
          #Set the outcome columns
          shiny::mainPanel(
            #First row will have the Photo and the overview of marker intensity by cell
            shiny::fluidRow(
              shiny::column(5, shiny::plotOutput("Photo",
                                                 #Controls for zoom in
                                                 dblclick = "Photo_dblclick",
                                                 brush = shiny::brushOpts(id = "Photo_brush",
                                                                          resetOnNew = TRUE)
              )
              ),
              shiny::column(5, ggiraph::girafeOutput("Pre_Processed"))
            ),
            #Second row will contain the positive cells and the histogram
            shiny::fluidRow(
              shiny::column(5, shiny::plotOutput("Color")),
              shiny::column(5, shiny::plotOutput("Final_channel"))
            )
          )
        ),
        shiny::tags$head(shiny::tags$style(
          htmltools::HTML('
         #sidebar {
            background-color: #cee02f;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')))
      )
    }
    
    
    #BUILD THE SERVER
    server <- function(input, output, session){
      #BASIC reactives
      Image_dir <- shiny::reactive(str_c(Directory, "/", input$Real_Image_name))
      Channel_name <- shiny::reactive(input$Channel_name)
      Resolution <- shiny::reactive(input$Res)
      
      Brightness <- shiny::reactive(input$Brightness)
      Saturation <- shiny::reactive(input$Saturation)
      Hue <- shiny::reactive(input$Hue)
      Normalize <- shiny::reactive(input$Normalize)
      Equalize <- shiny::reactive(input$Equalize)
      Contrast <- shiny::reactive(input$Contrast)
      Sharpen <- shiny::reactive(input$Sharpen)
      Reduce_color <- shiny::reactive(input$Quantize)
      Max_color <- shiny::reactive(input$Max_colors)
      
      Color_name <- shiny::reactive(input$Color_text)
      RED_value <- shiny::reactive(input$RED)
      GREEN_value <- shiny::reactive(input$GREEN)
      BLUE_value <- shiny::reactive(input$BLUE)
      Tolerance_value_RED <- shiny::reactive(input$Tolerance_RED)
      Tolerance_value_GREEN <- shiny::reactive(input$Tolerance_GREEN)
      Tolerance_value_BLUE <- shiny::reactive(input$Tolerance_BLUE)
      Tolerance_value_FINAL <- shiny::reactive(input$Tolerance_Final)
      
      Normalize_post <- shiny::reactive(input$Post_Normalize)
      Equalize_post <- shiny::reactive(input$Post_Equalize)
      Erode <- shiny::reactive(input$Erode)
      Erode_kern <- shiny::reactive(input$Erode_kern)
      Erode_rounds <- shiny::reactive(input$Erode_round)
      Dilate <- shiny::reactive(input$Dilate)
      Dilate_kern <- shiny::reactive(input$Dilate_kern)
      Dilate_rounds <- shiny::reactive(input$Dilate_round)
      
      #SPECIAL reactives 
      #4 chained reactives to process the photo
      #Original photo
      Photo_original <- shiny::reactive({
        Photo <- magick::image_read(Image_dir())
        return(Photo)
      })
      Pre_processed_photo <- shiny::reactive({
        Photo <- Photo_original()
        
        #Apply required processing
        Photo <- Photo %>% magick::image_modulate(brightness = Brightness(), saturation = Saturation(), hue = Hue())
        if(as.logical(Equalize())) Photo <- Photo %>% magick::image_equalize()
        if(as.logical(Normalize())) Photo <- Photo %>% magick::image_normalize()
        if(as.logical(Contrast())) Photo <- Photo %>% magick::image_contrast(sharpen = Sharpen())
        if(as.logical(Reduce_color())) Photo <- Photo %>% magick::image_quantize(max = Max_color(), dither = TRUE)
        
        return(Photo)
        
      })
      Color_extracted_photo <- shiny::reactive({
        Photo <- Pre_processed_photo()
        
        #Transform to EBI object
        Photo <- Photo %>% magick::image_convert(type = NULL,
                                                 colorspace = "RGB",
                                                 depth = 8,
                                                 antialias = NULL,
                                                 matte = FALSE) %>% 
          magick::as_EBImage() %>% 
          EBImage::toRGB()
        #Extract Image
        Photo <- EBImage_color_thresholder(Image = Photo, 
                                           Target = c(RED_value(), GREEN_value(), BLUE_value()),
                                           Color_Tolerance = c(Tolerance_value_RED(), Tolerance_value_GREEN(), Tolerance_value_BLUE()),
                                           Final_Tolerance = Tolerance_value_FINAL())
        #Return to magick object
        Photo <- magick::image_read(Photo)
        return(Photo)
      })
      Color_processed_photo <- shiny::reactive({
        Photo <- Color_extracted_photo()
        #Apply transformations before erosion and dilation
        if(as.logical(Normalize_post())) Photo <- Photo %>% magick::image_normalize()
        if(as.logical(Equalize_post())) Photo <- Photo %>% magick::image_equalize()
        
        #Transform to EBI object
        Photo <- Photo %>% magick::as_EBImage()
        
        #if erosion is required then execute the rounds of erosion the user requires
        if(as.logical(Erode())){
          Photo <- purrr::reduce(
            .x = seq_along(1:Erode_rounds()),
            .f = function(img, ...) EBImage::erode(img, kern = EBImage::makeBrush(Erode_kern(), "disc")),
            .init = Photo
          )
        }
        
        #If dilation is required then execute the dilation rounds
        if(as.logical(Dilate())){
          Photo <- purrr::reduce(
            .x = seq_along(1:Dilate_rounds()),
            .f = function(img, ...) EBImage::dilate(img, kern = EBImage::makeBrush(Dilate_kern(), "disc")),
            .init = Photo
          )
        }
        
        #Return to magick object
        Photo <- magick::image_read(Photo)
        return(Photo)
      })
      
      #Zoom reactive
      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      
      
      #Bi-directional reactives (Color name and Channel values)
      #First add RGB values if color is specified
      shiny::observeEvent({
        input$Color_text #Observe if text is modified by writing
      }, 
      {
        shiny::req(input$Color_text)
        
        #If color is introduced then get the color and rgb vector
        if(!berryFunctions::is.error(grDevices::col2rgb(as.character(Color_name()), alpha = FALSE)[,1])){
          RGB_vector <- grDevices::col2rgb(as.character(Color_name()), alpha = FALSE)[,1]
          
          shiny::updateNumericInput(session, "RED", value = RGB_vector[["red"]])
          shiny::updateNumericInput(session, "GREEN", value = RGB_vector[["green"]])
          shiny::updateNumericInput(session, "BLUE", value = RGB_vector[["blue"]])
        }
      }, 
      ignoreInit = TRUE)
      #in addition specify what to do if the user clicks on the pre-processed image
      shiny::observeEvent({
        input$Pre_Processed_selected #Observe if text is modified by writing
      }, 
      {
        shiny::req(input$Pre_Processed_selected)
        Color_name <- str_split_i(input$Pre_Processed_selected, "_", i = 1)
        
        #If color is introduced then get the color and rgb vector
        if(!berryFunctions::is.error(grDevices::col2rgb(Color_name, alpha = FALSE)[,1])){
          RGB_vector <- grDevices::col2rgb(Color_name, alpha = FALSE)[,1]
          
          shiny::updateNumericInput(session, "RED", value = RGB_vector[["red"]])
          shiny::updateNumericInput(session, "GREEN", value = RGB_vector[["green"]])
          shiny::updateNumericInput(session, "BLUE", value = RGB_vector[["blue"]])
        }
      }, 
      ignoreInit = TRUE)
      #Second add color name if RGB colors are modified
      shiny::observeEvent(
        { #Observe any modification in RGB values
          input$RED
          input$GREEN
          input$BLUE
        },
        {
          
          #If color is introduced then get the color and rgb vector
          RED_value <- if(RED_value()/255 > 1) 1 else RED_value()/255
          GREEN_value <- if(GREEN_value()/255 > 1) 1 else GREEN_value()/255
          BLUE_value <- if(BLUE_value()/255 > 1) 1 else BLUE_value()/255
          
          Color_name <- grDevices::rgb(RED_value, GREEN_value, BLUE_value)
          session$sendCustomMessage(type = 'Pre_Processed_set', message = as.character(Color_name[1]))
          shiny::updateTextInput(session, "Color_text", value = Color_name[1])
          
        }, 
        ignoreInit = TRUE)
      #Color target mini-rectangle
      output$Target <- shiny::renderPlot({
        ggplot() +
          geom_rect(
            aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            fill = Color_name(),
            color = "black"
          ) +
          scale_x_continuous("", labels = NULL) +
          scale_y_continuous("", labels = NULL) +
          theme_minimal() +
          theme(axis.ticks = element_blank(),
                axis.line = element_blank(),
                plot.margin = margin(-100, -100, -100, -100, "pt"))
      })
      
      
      #PARAMETER LIST
      #generate an object that will be modified by the user when the Add button is clicked
      Parameters_object <- shiny::reactiveValues(Parameters_list = list()) #Parameter list will be a list of lists
      #If the user hits the add button, place all parameters into a list and add a new list item to the Parameters list ()
      shiny::observeEvent(input$ADD_button, 
                          {
                            #If name is present print a warning message
                            if(input$Channel_name %in% names(Parameters_object$Parameters_list)){
                              showModal(modalDialog(
                                paste0(input$Channel_name, " already present in Parameter list. ", input$Channel_name, " will be overwritten" ),
                                easyClose = TRUE,
                                footer = NULL
                              )
                              )
                            }
                            
                            #Generate parameter list
                            Deconv_parameters <-
                              list(Brightness = input$Brightness,
                                   Saturation = input$Saturation,
                                   Hue = input$Hue,
                                   Normalize = as.logical(input$Normalize),
                                   Equalize = as.logical(input$Equalize),
                                   Contrast = as.logical(input$Contrast),
                                   Sharpen = input$Sharpen,
                                   Reduce_color = as.logical(input$Quantize),
                                   Max_color = input$Max_colors,
                                   
                                   RED_value = input$RED,
                                   GREEN_value = input$GREEN,
                                   BLUE_value = input$BLUE,
                                   Color_Tolerance = c(input$Tolerance_RED, input$Tolerance_GREEN, input$Tolerance_BLUE),
                                   Final_Tolerance = input$Tolerance_Final,
                                   
                                   Post_normalize = as.logical(input$Post_Normalize),
                                   Post_equalize = as.logical(input$Post_Equalize),
                                   Erode = as.logical(input$Erode),
                                   Erode_kern = input$Erode_kern,
                                   Erode_rounds = input$Erode_round,
                                   Dilate = as.logical(input$Dilate),
                                   Dilate_kern = input$Dilate_kern,
                                   Dilate_rounds = input$Dilate_round
                              )
                            
                            #If name is present replace the exact element of the list
                            if(input$Channel_name %in% names(Parameters_object$Parameters_list)){
                              Name_index <- match(input$Channel_name, names(Parameters_object$Parameters_list))
                              Parameters_object$Parameters_list[[Name_index]] <- Deconv_parameters
                            }
                            
                            else{
                              Parameters_object$Parameters_list[[length(Parameters_object$Parameters_list)+1]] <- Deconv_parameters
                              names(Parameters_object$Parameters_list)[length(Parameters_object$Parameters_list)] <- as.character(input$Channel_name) 
                            }
                          })
      
      #If the user hits the remove button then look for that element in the Parameter_list and remove it
      shiny::observeEvent(input$Remove, 
                          {
                            Name_index <- match(input$Channel_name, names(Parameters_object$Parameters_list))
                            if(is.na(Name_index)){
                              showModal(modalDialog(
                                paste0(input$Channel_name, " not present in Parameter list. Please check channel name provided"),
                                easyClose = TRUE,
                                footer = NULL
                              )
                              )
                            }
                            else{
                              Parameters_object$Parameters_list <- Parameters_object$Parameters_list[-Name_index]
                            }
                          })
      #Generate the output
      output$Result_list <- shiny::renderPrint({
        if(length(Parameters_object$Parameters_list) == 0) "Awaiting initial parameters"
        else{
          Parameters_object$Parameters_list
        }
      })
      #If download parameter button is clicked, download paramater list
      shiny::observeEvent(input$Download_Param, 
                          {
                            if(length(Parameters_object$Parameters_list) == 0){
                              showModal(modalDialog(
                                "Channel deconvolution parameters not specified. Please add parameters to the list and retry",
                                easyClose = TRUE,
                                footer = NULL
                              )
                              )
                            }
                            else{
                              Deconvolution_Parameters <<- Parameters_object$Parameters_list
                              showModal(modalDialog(
                                "An object called 'Deconvolution_Parameters' has been created in the Global environment.",
                                easyClose = TRUE,
                                footer = NULL
                              )
                              )
                            }
                          })
      
      
      
      #Original photo (top left)
      #Print the photo
      output$Photo <- shiny::renderPlot({
        #Obtain the photo
        Photo <- Photo_original()
        
        #Modify resolution before printing
        if(as.numeric(Resolution()) != 1000){
          Image_Resolution <- str_c("X", Resolution())
          Photo <- magick::image_scale(Photo, Image_Resolution)
        }
        
        #Get image info
        Image_information <- magick::image_info(Photo)
        Width <- Image_information$width
        Height <- Image_information$height
        
        #Generate the plot as annotation_raster
        return(
          ggplot() +
            annotation_raster(Photo, xmin = 0, xmax = Width, ymin = 0, ymax = Height, interpolate = TRUE)+
            scale_x_continuous(limits = c(0, Width)) +
            scale_y_continuous(limits = c(0, Height)) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  panel.grid = element_blank())
        )
      }, res = 300)
      
      #Control the zoom in of the Photo and the other plots
      shiny::observeEvent(input$Photo_dblclick, {
        brush <- input$Photo_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      
      #Pre-processed photo (top right)
      #Generate the plot
      Pre_processed_plot <- 
        shiny::reactive({
          Photo <- Pre_processed_photo() #Get processed photo
          
          #If no zoom in required then low down the resolution of the image(always as geom_tile is very computationally slow)
          if(is.null(ranges$x)){
            Image_Resolution <- str_c("X", Resolution())
            Photo <- magick::image_scale(Photo, "X100")
            Photo <- Photo %>% magick::image_raster()
            #Reverse pixels to match annotate_raster system
            Photo$y <- rev(Photo$y)
          }
          
          #If zoom is required then keep original user defined resolution (required to match zoomed in areas)
          if(!is.null(ranges$x)){
            if(as.numeric(Resolution()) == 1000){
              Photo <- Photo %>% magick::image_raster()
              #Reverse pixels to match annotate_raster system
              Photo$y <- rev(Photo$y)
            }
            else{
              Image_Resolution <- str_c("X", Resolution())
              Photo <- magick::image_scale(Photo, Image_Resolution)
              Photo <- Photo %>% magick::image_raster()
              #Reverse pixels to match annotate_raster system
              Photo$y <- rev(Photo$y)
            }
            
            #remove pixels outside the scale
            Photo <- Photo %>% dplyr::filter(x >= ranges$x[[1]],
                                             x <= ranges$x[[2]],
                                             y >= ranges$y[[1]],
                                             y <= ranges$y[[2]])
            
          }
          
          #Generate an ID that contains the color code (used for tooltip data)
          Photo$Pixel_id <- str_c(Photo$col, 1:nrow(Photo), sep = "_")
          
          
          return(
            Photo %>%
              ggplot() +
              ggiraph::geom_tile_interactive(aes(x = x, y = y, fill = col,
                                                 data_id = Pixel_id, 
                                                 tooltip = as.character(col),
                                                 hover_nearest = FALSE
              ),
              size = 0) +
              scale_fill_identity() +
              cowplot::theme_cowplot()+
              guides(color = "none") +
              theme(axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.background = element_rect(fill = "black")) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) 
          )
        })
      #send the graph to the UI
      output$Pre_Processed <- ggiraph::renderGirafe({
        plot <- ggiraph::girafe(code = print(Pre_processed_plot()),
                                options = list(
                                  ggiraph::opts_hover(css = "stroke:black;cursor:pointer;", reactive = TRUE),
                                  ggiraph::opts_selection(type = "single", css = "fill:#FF3333;stroke:black;")
                                )
        )
        return(plot)
      })
      
      
      #Color extraction (bottom left)
      output$Color <- shiny::renderPlot({
        #Obtain the photo
        Photo <- Color_extracted_photo()
        
        #Modify resolution before printing
        if(as.numeric(Resolution()) != 1000){
          Image_Resolution <- str_c("X", Resolution())
          Photo <- magick::image_scale(Photo, Image_Resolution)
        }
        
        #Get image info
        Image_information <- magick::image_info(Photo)
        Width <- Image_information$width
        Height <- Image_information$height
        #Generate the plot as annotation_raster
        return(
          ggplot() +
            annotation_raster(Photo, xmin = 0, xmax = Width, ymin = 0, ymax = Height, interpolate = TRUE)+
            scale_x_continuous(limits = c(0, Width)) +
            scale_y_continuous(limits = c(0, Height)) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  panel.grid = element_blank())
        )
      }, res = 300)
      
      #Final color channel (bottom right)
      output$Final_channel <- shiny::renderPlot({
        #Obtain the photo
        Photo <- Color_processed_photo()
        
        #Modify resolution before printing
        if(as.numeric(Resolution()) != 1000){
          Image_Resolution <- str_c("X", Resolution())
          Photo <- magick::image_scale(Photo, Image_Resolution)
        }
        
        #Get image info
        Image_information <- magick::image_info(Photo)
        Width <- Image_information$width
        Height <- Image_information$height
        
        #Generate the plot as annotation_raster
        return(
          ggplot() +
            annotation_raster(Photo, xmin = 0, xmax = Width, ymin = 0, ymax = Height, interpolate = TRUE)+
            scale_x_continuous(limits = c(0, Width)) +
            scale_y_continuous(limits = c(0, Height)) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  panel.grid = element_blank())
        )
      }, res = 300)
      
      #If browser is closed end the app
      session$onSessionEnded(function() { shiny::stopApp() })
    }
    
    #Run the server
    message("Always stop current R execution if you want to continue with your R session")
    shiny::shinyApp(user_interface, server)
  },
  options = list(optimize = 3))

Image_deconvolution_function <- cmpfun(function(Directory = NULL,
                                                Output_directory = NULL,
                                                Deconvolution_parameters = NULL,
                                                N_cores = NULL){
  print("Checking supplied arguments")
  #Check arguments
  if(!length(dir(Directory)) >= 1) stop("Directory does not contain any files") #Check directory files
  if(berryFunctions::is.error(dir(Output_directory))) stop("Invalid output directory") #Check output directory
  if(length(dir(Output_directory)) != 0) stop("Output directory should be an empty folder") #Check that output directory is empty
  if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
  
  #The expected names in each element of the parameter list
  Adequate_Names <- c("Brightness","Saturation","Hue", "Normalize", "Equalize", "Contrast", "Sharpen", "Reduce_color",
                      "Max_color", "RED_value", "GREEN_value", "BLUE_value", "Color_Tolerance", "Final_Tolerance", 
                      "Post_normalize", "Post_equalize", "Erode", "Erode_kern", "Erode_rounds", "Dilate", "Dilate_kern", "Dilate_rounds")
  Deconvolution_Parameters <- Deconvolution_parameters
  Names_ok <- map_lgl(Deconvolution_Parameters, function(Channel) identical(names(Channel), Adequate_Names)) #Check that the names are equal
  if(!all(Names_ok)){
    stop(paste0("The following channels do not contain all the required deconvolution parameters: ",
                str_c(names(Deconvolution_Parameters)[!Names_ok], collapse = ", "),
                ". The required parameters should be the following in the following exact order: ",
                str_c(Adequate_Names, collapse = ", ")))
  } 
  
  #Check parameters list
  Error_messages <- map(Deconvolution_Parameters, function(Channel){
    #Conditions to test
    Testing_OK <- c(Brightness_OK = all(is.numeric(Channel[["Brightness"]]), Channel[["Brightness"]] >= 0, Channel[["Brightness"]] <= 100),
                    Saturation_OK = all(is.numeric(Channel[["Saturation"]]), Channel[["Saturation"]] >= 0, Channel[["Saturation"]] <= 100),
                    Hue_OK = all(is.numeric(Channel[["Hue"]]), Channel[["Hue"]] >= 0, Channel[["Hue"]] <= 100),
                    Normalize_OK = is.logical(Channel[["Normalize"]]),
                    Equalize_OK = is.logical(Channel[["Equalize"]]),
                    Contrast_OK = is.logical(Channel[["Contrast"]]),
                    Sharpen_OK = all(is.numeric(Channel[["Sharpen"]]), Channel[["Sharpen"]] > 0),
                    Reduce_color_OK = is.logical(Channel[["Reduce_color"]]),
                    Max_color_OK = all(is.numeric(Channel[["Max_color"]]), Channel[["Max_color"]] >= 1),
                    RED_value_OK = Channel[["RED_value"]] >= 0 & Channel[["RED_value"]] <= 255,
                    GREEN_value_OK = Channel[["GREEN_value"]] >= 0 & Channel[["GREEN_value"]] <= 255,
                    BLUE_value_OK = Channel[["BLUE_value"]] >= 0 & Channel[["BLUE_value"]] <= 255,
                    Color_Tolerance_value_OK = all(is.numeric(Channel[["Color_Tolerance"]]), length(Channel[["Color_Tolerance"]]) == 3, all(Channel[["Color_Tolerance"]] >= 0), all(Channel[["Color_Tolerance"]] <= 1)),
                    Final_Tolerance_value_OK = all(is.numeric(Channel[["Final_Tolerance"]]), Channel[["Final_Tolerance"]] >= 0, Channel[["Final_Tolerance"]] <= 1),
                    Post_normalize_OK = is.logical(Channel[["Post_normalize"]]),
                    Post_equalize_OK = is.logical(Channel[["Post_equalize"]]),
                    Erode_OK = is.logical(Channel[["Erode"]]),
                    Erode_kern_OK = all(is.numeric(Channel[["Erode_kern"]]), Channel[["Erode_kern"]]>0),
                    Erode_rounds_OK = all(is.numeric(Channel[["Erode_rounds"]]), Channel[["Erode_rounds"]]>0),
                    Dilate_OK = is.logical(Channel[["Dilate"]]),
                    Dilate_kern_OK = all(is.numeric(Channel[["Dilate_kern"]]), Channel[["Dilate_kern"]]>0),
                    Dilate_rounds_OK = all(is.numeric(Channel[["Dilate_rounds"]]), Channel[["Erode_rounds"]]>0)
    )
    
    #Messages to deploy
    Messages_to_return <- c(Brightness_OK = "Brightness must be a positive numeric value between 0 and 100",
                            Saturation_OK = "Saturation must be a positive numeric value between 0 and 100",
                            Hue_OK = "Hue must be a positive numeric value between 0 and 100",
                            Normalize_OK = "Normalize must be a logical value",
                            Equalize_OK = "Equalize must be a logical value",
                            Contrast_OK = "Contrast must be a logical value",
                            Sharpen_OK = "Sharpen must be a numeric value larger than 0",
                            Reduce_color_OK = "Reduce_color must be a logical value",
                            Max_color_OK = "Max_color must be a numeric value equal or larger than 1",
                            RED_value_OK = "RED_value must be a numeric value between 0 and 255",
                            GREEN_value_OK = "GREEN_value must be a numeric value between 0 and 255",
                            BLUE_value_OK = "BLUE_value must be a numeric value between 0 and 255",
                            Color_Tolerance_value_OK = "Tolerance value must be a numeric vector of length = 3, containing values between 0 and 1",
                            Final_Tolerance_value_OK = "Final_Tolerance must be a single numeric value between 0 and 1",
                            Post_normalize_OK = "Post_normalize must be a logical value",
                            Post_equalize_OK = "Post_equalize must be a logical value",
                            Erode_OK = "Erode must be a logical value",
                            Erode_kern_OK = "Erode_kern must be a numeric value equal to or higher than 1",
                            Erode_rounds_OK = "Erode_rounds must be a numeric value equal to or higher than 1",
                            Dilate_OK = "Dilate must be a logical value",
                            Dilate_kern_OK = "Dilate_kern must be a numeric value equal to or higher than 1",
                            Dilate_rounds_OK = "Dilate_rounds must be a numeric value equal to or higher than 1"
    )
    Messages_to_return[!Testing_OK]
  })
  #If badly specified then return an error with adequate messages
  if(any(map_lgl(Error_messages, ~length(.)>0))) {
    Error_messages <- Error_messages[map_lgl(Error_messages, ~length(.)>0)]
    Messages <- map_chr(1:length(Error_messages), function(Index){
      paste0(names(Error_messages)[Index], ": ", str_c(Error_messages[[Index]], sep = ". "))
    })
    stop(cat(Messages, fill = length(Messages)))
  }
  
  #Before proceeding print a summary and ask the user if they want to proceed
  print(paste0(length(dir(Directory)), " images will be processed"))
  print(paste0(length(Deconvolution_Parameters), " channels will be obtained: ", str_c(names(Deconvolution_Parameters), collapse = ", ")))
  print(paste0(N_cores, " cores will be used in the computation"))
  answer <- menu(c("Proceed", "Abort"), title = "Should the analysis proceed")
  
  #If user decides to stop then abort function and return stop message
  if(answer == 2) stop("The function has been stopped.")
  
  print("Obtaining individual channels for every image and writing results")
  #save exit function if parallelization fails
  on.exit({
    future::plan("future::sequential")
    gc()
  })
  
  #Now we calculate our distance matrix
  future::plan("future::multisession", workers = N_cores) 
  options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
  furrr::furrr_options(scheduling = Inf)
  
  
  #Ready to execute. It will be image based (image directory is the input and writing a file is the output)
  furrr::future_map(seq_along(1:length(dir(Directory))), function(Image_index){
    
    #Get the original image as a magick object
    Image <- magick::image_read(dir(Directory, full.names = TRUE)[Image_index])
    
    #Obtain a list with each element corresponding to each of the channels
    List_of_image_channel <- map(Deconvolution_Parameters, 
                                 function(Channel_Parameters){
                                   Channel <- Channel_deconvolution_function(Image = Image,
                                                                             Parameters = Channel_Parameters)
                                   return(Channel)
                                 })
    FINAL_Image <- EBImage::combine(List_of_image_channel)
    FINAL_Image_Name <- str_c(Output_directory, "/", dir(Directory, full.names = FALSE)[Image_index], "_Channels.tiff")
    
    EBImage::writeImage(FINAL_Image, FINAL_Image_Name, type = "tiff")
  },
  .progress = TRUE)
  
  future::plan("future::sequential")
  gc()
  
  #Finally also print some messages
  print(paste0("Processed images can be found at: ", Output_directory))
  print(tibble(Channel = str_c("Channel_", 1:length(Deconvolution_Parameters), sep = ""),
               Name = names(Deconvolution_Parameters)
  )
  )
},
options = list(optimize = 3))

Segmentation_tester <- cmpfun(
  function(Directory = NULL,
           Ordered_Channels = NULL,
           Channels_to_keep = NULL,
           Images_to_test = NULL,
           Number_tests = NULL,
           
           Nuclear_marker = NULL, #marker or list of markers corresponding to the nuclei
           Cell_body_method = NULL, #Method of cytoplasm identification. Can be 'none', dilate', 'discModel' or the name of a dedicated cytoplasm marker
           Min_pixel = NULL, # Minimum pixels for an object to be recognized as a cell and not noise
           Smooth_amount = NULL, #The amount of Gaussian smoothing to be applied to the image
           Normalization = NULL, #Single value or list specifying the transformations from "sqrt", "asinh", "norm99", "maxThresh" and "tissueMask
           Watershed_type = NULL, #Method used to perform watersheding. Accepted values: "intensity", "distance" or "combine"
           Tolerance_value = NULL, #minimum height of the object in the units of image intensity between its highest point (seed) and the point where it contacts another object (MAY BE NULL)
           Neighborhood_distance = NULL, #Radius of the neighborhood in pixels for the detection of neighboring objects. Higher value smooths out small objects.
           Disc_size = NULL, #The size of dilation around nuclei to create cell disc or capture cytoplasm
           Tissue_mask_markers = NULL, #A vector specifying the channels to be used to create the tissue mask if specified in transforms
           Perform_PCA = FALSE, #Whether to run PCA on aggregated nucleus markers in order to detect the cellular nucclei
           
           Perform_nuclear_channel_processing = NULL,
           Black_level = NULL,
           White_level = NULL,
           Gamma_level = NULL,
           Equalize = NULL,
           Opening_kernel_size = NULL,
           Closing_kernel_size = NULL
  ){
    #Specify what to do on exit
    on.exit(gc())
    
    #Check arguments by generating a argument check vector and message vector
    Argument_checker <- c(Empty_directory = length(dir(Directory)) >= 1,
                          Channels_OK = all(Channels_to_keep %in% Ordered_Channels),
                          Images_OK = if(!is.null(Images_to_test)) {
                            all(Images_to_test %in% dir(Directory, full.names = FALSE))
                          } else(TRUE),
                          Number_tests_OK = if(is.null(Images_to_test)) {
                            Number_tests <= length(dir(Directory)) & Number_tests >= 1
                          } else(TRUE),
                          Channels_to_keep_OK = all(Channels_to_keep %in% Ordered_Channels),
                          Nuclear_OK = unlist(Nuclear_marker) %in% Channels_to_keep,
                          Cell_body_OK = Cell_body_method %in% c("none", "dilate", "discModel"),
                          Min_pixel_OK = all(is.numeric(Min_pixel), Min_pixel%%1 == 0, Min_pixel >= 0),
                          Smooth_amount_OK = all(is.numeric(Smooth_amount), Smooth_amount >= 0),
                          Normalization_OK = Normalization %in% c("sqrt", "asinh", "norm99", "maxThresh", "tissueMask"),
                          Watershed_type_OK = Watershed_type %in% c("intensity", "distance", "combine"),
                          Tolerance_value_OK = if(!is.null(Tolerance_value)) {
                            all(is.numeric(Tolerance_value), Tolerance_value >= 0)
                          } else(TRUE),
                          Neighborhood_distance_OK = all(Neighborhood_distance%%1 == 0, Neighborhood_distance >= 0),
                          Disc_size_OK = all(Disc_size%%1 == 0, Disc_size >= 0),
                          Tissue_mask_markers_OK =  if(!is.null(Tissue_mask_markers)) {
                            all(Tissue_mask_markers %in% Channels_to_keep)
                          } else(TRUE),
                          Perform_PCA_OK = is.logical(Perform_PCA),
                          Perform_nuclear_channel_processing_OK = is.logical(Perform_nuclear_channel_processing)
    )
    
    Stop_messages <- c(Empty_directory = "No files found at the directory provided. Please check out the path.",
                       Channels_OK = str_c(
                         "The following channels are not present the channel names provided: ",
                         str_c(Channels_to_keep[!(Channels_to_keep %in% Ordered_Channels)], collapse = ", "),
                         sep = ""),
                       Images_OK = str_c(
                         "The following images are not present in the directory provided: ",
                         str_c(Images_to_test[!(Images_to_test %in% dir(Directory, full.names = FALSE))], collapse = ", "),
                         sep = ""),
                       Numer_tests_OK = str_c("Images to test must be between 1 and ", length(dir(Directory)), sep = ""),
                       Channels_to_keep_OK = "Channels_to_keep must be included in Ordered_channels",
                       Nuclear_OK = str_c("Nuclear marker specified not found in ", str_c(Channels_to_keep, collapse = ", ")),
                       Cell_body_OK = "Cell body method must be one of the following: none, dilate, discModel",
                       Min_pixel_OK = "Min pixel must be a positive integer value",
                       Smooth_amount_OK = "Smooth amount must be a positive numeric value",
                       Normalization_OK = "Normalization method must be one of the following: sqrt, asinh, norm99, maxThresh, tissueMask",
                       Watershed_type_OK = "Watershed type must be NULL or one of the following: intensity, distance, combine",
                       Tolerance_value_OK = "Tolerance value must be NULL or a positive numeric value",
                       Neighborhood_distance_OK = "Neighborhood distance must be a positive integer value",
                       Disc_size_OK = "Disc size must be a positive integer value",
                       Tissue_mask_markers_OK =  str_c("Tissue mask must be NULL or one of the following: ", str_c(Channels_to_keep, collapse = ", ")),
                       Perform_PCA_OK = "Perform PCA must be a logical value",
                       Perform_nuclear_channel_processing_OK = "Perform_nuclear_channel_processing must be a logical value")
    
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    
    #Check specifically nuclear processing arguments if Perform_nuclear_channel_processing is true
    if(Perform_nuclear_channel_processing){
      if(!all(is.numeric(Black_level), Black_level >= 0, Black_level <= 100, Black_level < White_level)) stop("Black_level must be a numeric value between 0 - 100 and smaller than White_level")
      if(!all(is.numeric(White_level), White_level >= 0, White_level <= 100)) stop("White_level must be a numeric value between 0 - 100")
      if(!all(is.numeric(Gamma_level), Gamma_level >= -3, Gamma_level <= 3)) stop ("Gamma_level must be a numeric value between -3 and +3")
      if(!is.logical(Equalize)) stop("Equalize must be a logical value")
      if(!all(Opening_kernel_size%%1 == 0, Opening_kernel_size >= 1)) stop("Opening_kernel_size must be a positive integer value >= 1")
      if(!all(Closing_kernel_size%%1 == 0, Closing_kernel_size >= 1)) stop("Closing_kernel_size must be a positive integer value >= 1")
    }
    
    
    
    #Obtain the channels and the Image names
    Channels <- Ordered_Channels
    Simple_Image_Names <- dir(Directory, full.names = FALSE)
    Complete_Image_Names <- dir(Directory, full.names = TRUE)
    
    #Obtain the Images to test
    #First obtain image names randomly if the user does not select explicitly the images
    if(is.null(Images_to_test)){
      message("No image names provided. Images will be selected randomly")
      Samples_to_test <- sample(1:length(Simple_Image_Names), size = Number_tests, replace = F)
      Images_to_use_Complete <- Complete_Image_Names[Samples_to_test]
      print(paste0("The following images will be tested: ", str_c(Simple_Image_Names[Samples_to_test], collapse = ", ")))
    }
    
    #Then select explicit images to be tested
    else {
      Samples_to_test <- match(Images_to_test, Simple_Image_Names, nomatch = NULL)
      Images_to_use_Complete <- Complete_Image_Names[Samples_to_test]
    }
    
    #Print images and channels to make the user certify that the channels are correctly arranged
    map(Images_to_use_Complete, function(Images){
      #Import the Images and scale them to make them more printable in the plot window
      Image <- magick::image_read(Images) %>% magick::image_scale('X600')
      
      if(nrow(magick::image_info(Image)) != length(Ordered_Channels)){
        stop(paste0("The number of channels should match the number of Ordered_Channels. ",
                    "Channels in image: ", nrow(magick::image_info(Image)), ", ",
                    "Channels provided: ", length(Ordered_Channels)))
      }
      
      #Plot the multi-tiff image with the labels
      map2(.x = seq_along(1:nrow(magick::image_info(Image))), 
           .y = Ordered_Channels, 
           
           function(.x, .y){
             plot(Image[.x] %>% magick::image_annotate(str_c("Channel ", as.character(.x), ": ", .y, collapse = ""),
                                                       size = 40, "North", color = "black", boxcolor = "white") %>% magick::image_ggplot())
           })
    }, .progress = list(clear = F,
                        name = "Obtaining Image Channels",
                        show_after = 2,
                        type = "iterator"))
    
    #Ask the user if channel names are adequate
    Channels_OK <- menu(choices = c("Proceed", "Abort"), title = "Check that image channels have been correctly specified. If channels are OK select proceed")
    if(Channels_OK == 2) stop("Procedure has been stopped. Please check channel ordering")
    
    #Test the modifications of the nuclear channels
    if(Perform_nuclear_channel_processing){
      #Get nuclear channels
      Nuclear_channels_number <- match(Nuclear_marker, Ordered_Channels)
      #Modify each channel according to user preferences and display side by side results, then as the user to proceed or not
      
      map(Images_to_use_Complete, function(Images){
        map(Nuclear_channels_number, function(Channel_number){
          Image_original <- magick::image_read(Images)[Channel_number] #Get original Image
          Image_Modified <- Image_original #Generate a new copy
          if(Equalize) Image_Modified <- Image_Modified %>% magick::image_equalize() #Equalize if necesary
          Image_Modified <- Image_Modified %>% magick::image_level(black_point = Black_level,
                                                                   white_point = White_level,
                                                                   mid_point = 10^Gamma_level) #Chane withe, black and gamma
          Image_Modified <- Image_Modified %>% magick::as_EBImage() #turn to EBImage object
          Image_Modified  <- 
            Image_Modified %>% EBImage::opening(EBImage::makeBrush(size = Opening_kernel_size, shape = "disc")) %>%
            EBImage::closing(EBImage::makeBrush(size = Closing_kernel_size, shape = "disc")) #opening and closing
          
          Image_Modified <- magick::image_read(Image_Modified) #again as Magick image
          
          #print both images
          plot(magick::image_append(c(Image_original, Image_Modified)))
          
        })
      })
      
      
      Processing_OK <- menu(choices = c("Proceed", "Abort"), title = "Check nuclear channel processing results. Should the algorithm proceed")
      if(Processing_OK == 2) stop("Procedure has been stopped. Please check nuclear channel processing parameters")
    }
    
    #If everything is OK proceed with segmentation and display the results
    gc()
    map(seq_along(1:length(Images_to_use_Complete)), function(Index){
      
      if(Perform_nuclear_channel_processing){
        Image <- magick::image_read(Images_to_use_Complete[Index]) #Import image
        
        Nuclear_channels_number <- match(Nuclear_marker, Ordered_Channels)
        
        #Apply changes to every nuclear channel
        for(index in Nuclear_channels_number){
          Image_Modified <- Image[index]
          
          if(Equalize) Image_Modified <- Image_Modified %>% magick::image_equalize() #Equalize if necesary
          Image_Modified <- Image_Modified %>% magick::image_level(black_point = Black_level,
                                                                   white_point = White_level,
                                                                   mid_point = 10^Gamma_level) #Chane withe, black and gamma
          Image_Modified <- Image_Modified %>% magick::as_EBImage() #turn to EBImage object
          Image_Modified  <- 
            Image_Modified %>% EBImage::opening(EBImage::makeBrush(size = Opening_kernel_size, shape = "disc")) %>%
            EBImage::closing(EBImage::makeBrush(size = Closing_kernel_size, shape = "disc")) #opening and closing
          
          Image_Modified <- magick::image_read(Image_Modified) #again as Magick image
          
          Image[index] <- Image_Modified
        }
        #Returna a EBImage object
        Image <- Image %>% magick::as_EBImage()
      }
      
      #Import image with the EBImage importer if no processing is required
      else{
        Image <- EBImage::readImage(Images_to_use_Complete[Index])
      }
      
      #Transform it to cytoImage object
      Image <- cytomapper::CytoImageList(Image)
      cytomapper::channelNames(Image) <- Ordered_Channels #define channel names
      S4Vectors::mcols(Image)$imageID <- as.character(Simple_Image_Names[Index])#Modify name
      Image <- cytomapper::getChannels(Image, Ordered_Channels[Ordered_Channels %in% Channels_to_keep]) #Keep only user defined channels
      
      #Perform cell segmentation
      Seg_results <- simpleSeg::simpleSeg(Image,
                                          nucleus = Nuclear_marker, 
                                          cellBody = Cell_body_method, #May be the name of a dedicated cytoplasm marker
                                          sizeSelection = Min_pixel,
                                          smooth = Smooth_amount,
                                          transform = Normalization,
                                          watershed = Watershed_type,
                                          tolerance = Tolerance_value,
                                          ext = Neighborhood_distance,
                                          discSize = Disc_size,
                                          tissue = Tissue_mask_markers,
                                          pca = Perform_PCA,
                                          cores = 1
      )
      S4Vectors::mcols(Seg_results)$imageID <- as.character(Simple_Image_Names[Index])
      
      #Prepare the color list and plot the first image
      color_list <- list(A = c(alpha("black", 0), "#0549fc"))
      names(color_list) <- Nuclear_marker
      cytomapper::plotPixels(image = Image, 
                             mask = Seg_results, 
                             img_id = "imageID",
                             colour_by = Nuclear_marker, 
                             colour = color_list,
                             display = "single",
                             legend = NULL) 
      
      #Display the second type of image
      EBImage::image(EBImage::colorLabels(Seg_results[[1]]))
      
      #Plot the side by side comparison between nuclear marker and virtual cells
      
      #First obtain the cell locations
      Position <- cytomapper::measureObjects(mask = Seg_results,
                                             image = Image,
                                             img_id = "imageID",
                                             feature_types = c("basic", "moment"),
                                             moment_feature = c("cx", "cy"),
                                             basic_feature = "mean")
      Position_tibble <- as_tibble(cbind(as_tibble(SummarizedExperiment::colData(Position)), 
                                         as_tibble(t(SummarizedExperiment::assays(Position)[[1]])))) %>% dplyr::select(-objectNum)
      
      #Import the image as a magick object
      Image_magick <- magick::image_read(Images_to_use_Complete[Index]) %>% magick::image_scale('X800')
      
      #Get only the nuclear marker
      DAPI_Image <- Image_magick[which(Ordered_Channels == Nuclear_marker)[[1]]] %>% magick::image_ggplot()
      
      #Obtain the virtual cells
      Virtual_cells <- Position_tibble %>% ggplot(aes(x = m.cx, y = -m.cy)) + geom_point(color = "white", size = 0.6) +
        theme_minimal() +
        scale_x_continuous("", labels = NULL) +
        scale_y_continuous("", labels = NULL)+
        
        theme(panel.grid = element_blank(),
              panel.background = element_rect(fill = "black"))
      
      #Plot both results
      plot(patchwork::wrap_plots(DAPI_Image, Virtual_cells, nrow = 1))
      
    }, .progress = list(clear = F,
                        name = "Performing test image segmentation",
                        show_after = 2,
                        type = "iterator"))
    gc()
    
    #Check if user is satisfied with the result
    Segmentation_answer <- menu(choices = c("Proceed", "Abort"), title = "Check that image/s have been correctly segmented")
    if(Segmentation_answer == 2) stop("Please review segmentation parameters and retry")
    #If everything is ok print the user defined paramters
    else{
      return(list(
        Ordered_Channels = Ordered_Channels,
        Channels_to_keep = Channels_to_keep,
        Nuclear_marker = Nuclear_marker,
        Cell_body_method = Cell_body_method, 
        Min_pixel = Min_pixel, 
        Smooth_amount = Smooth_amount, 
        Normalization = Normalization, 
        Watershed_type = Watershed_type, 
        Tolerance_value = Tolerance_value, 
        Neighborhood_distance = Neighborhood_distance, 
        Disc_size = Disc_size, 
        Tissue_mask_markers = Tissue_mask_markers,
        Perform_PCA =  Perform_PCA,
        
        Perform_nuclear_channel_processing = Perform_nuclear_channel_processing,
        Black_level = Black_level,
        White_level = White_level,
        Gamma_level = Gamma_level,
        Equalize = Equalize,
        Opening_kernel_size = Opening_kernel_size,
        Closing_kernel_size = Closing_kernel_size))
    }
  }, options = list(optimize = 3))

segmentator_tester_app <- cmpfun(
  function(Directory = NULL,
           Ordered_Channels = NULL){
    #check that the directory provided contains at least one file
    if(length(dir(Directory)) <1 ) stop("No files found in the Directory provided")
    
    #Obtain image names and channel names
    Real_Images <- dir(Directory, full.names = FALSE)
    Channels_in_images <- Ordered_Channels
    
    
    #BUILD THE USER INTERFACE
    user_interface <- shiny::fluidPage(
      
      #Set the title
      shiny::titlePanel("Segmentation exploration APP"),
      
      #We want a two panel layout, one in the left containing the input parameters and the output in the right
      shiny::sidebarLayout(
        #Set the first column (which contains the user defined parameters)
        shiny::sidebarPanel(
          #ID and width
          id="sidebar",
          
          #Image to display
          shiny::fluidRow(
            #Select the real image to be displayed
            shiny::column(5, shiny::selectInput("Real_Image_name", "Image to display", sort(Real_Images), multiple = FALSE)),
            #Select the channel to be displayed
            shiny::column(4, shiny::selectInput("Channel", "Channel to display", Channels_in_images, multiple = FALSE)),
            #Select the channels to keep
            shiny::column(3, shinyWidgets::virtualSelectInput("Keep_channels", label = "Channels to keep",
                                                              choices = Channels_in_images, 
                                                              search = TRUE,
                                                              multiple = TRUE
            ))
          ),
          #Image control
          shiny::fluidRow(
            #Select the min point of the image
            shiny::column(3, shiny::sliderInput("Min_Image", "Black", value = 0, min = 0, max = 100, step = 1)),
            #Select the max point of the image
            shiny::column(3, shiny::sliderInput("Max_Image", "White", value = 100, min = 0, max = 100, step = 1)),
            #Select the Gamma
            shiny::column(3, shiny::sliderInput("Gamma", "Gamma", value = 0, min = -3, max = +3, step = 0.01)),
            #Select the equalization
            shiny::column(3, shiny::selectInput("Equalize", "Equalize", c(YES = TRUE, NO = FALSE), selected = FALSE, multiple = FALSE))
          ),
          
          #Optional nuclear-prepocessing parameters 
          shiny::fluidRow(
            #Should this be performed or not
            shiny::column(3, shinyWidgets::materialSwitch("Pre_processing", "Pre process", value = TRUE)),
            #Select the kernels
            shiny::column(3, shiny::conditionalPanel(condition = "input.Pre_processing == '1'",
                                                     shiny::numericInput("Opening", "Opening", value = 1, min = 1, max = NA, step = 1)
            )),
            shiny::column(3, shiny::conditionalPanel(condition = "input.Pre_processing == '1'",
                                                     shiny::numericInput("Closing", "Closing", value = 1, min = 1, max = NA, step = 1)
            ))
          ),
          
          #Basic segmentation parameters
          shiny::fluidRow(
            #Select the nuclear marker
            shiny::column(4, shinyWidgets::virtualSelectInput("Nuclear", "Nuclear Marker", choices = Channels_in_images, multiple = TRUE, search = TRUE)),
            #Select the cell body method
            shiny::column(4, shiny::selectInput("Cell_body", "Cell-body method", c("none", "dilate", "discModel"), selected = "discModel", multiple = FALSE)),
            #Select the watershed type
            shiny::column(4, shiny::selectInput("Watershed", "Watershed type", c("intensity", "distance", "combine"), selected = "combine", multiple = FALSE))
          ),
          #Image pre-processing parameters
          shiny::fluidRow(
            #Select the image normalization steps
            shiny::column(4, shinyWidgets::pickerInput("Normalization", label = "Normalization", 
                                                       choices = c("sqrt", "asinh", "norm99", "maxThresh", "tissueMask"), 
                                                       multiple = TRUE, width = "fit",
                                                       options = shinyWidgets::pickerOptions(
                                                         selectedTextFormat = 'count', 
                                                         showSubtext = FALSE)
            )),
            #Select the markers for tissueMask
            shiny::column(4, shinyWidgets::pickerInput("Tissue_mask", label = "Tissue-mask markers", 
                                                       choices = Channels_in_images, 
                                                       multiple = TRUE, width = "fit",
                                                       options = shinyWidgets::pickerOptions(
                                                         selectedTextFormat = 'count', 
                                                         showSubtext = FALSE)
            )),
            #Select the smooth amount
            shiny::column(4, shiny::numericInput("Gaussian", "Smoothening", value = 1, min = 0, max = NA, step = 0.1))
          ),
          #Cell identification parameters
          shiny::fluidRow(
            #Min pixel
            shiny::column(4, shiny::numericInput("Min_pix", "Min pixel value", value = 10, min = 1, max = NA, step = 1)),
            #Neighbor distance
            shiny::column(4, shiny::numericInput("Neigh_dist", "Neighbor distance", value = 1, min = 1, max = NA, step = 1)),
            #Select the disc size
            shiny::column(4, shiny::numericInput("Disc_size", "Disc size", value = 1, min = 1, max = NA, step = 1))
          ),
          #Minor parameters
          shiny::fluidRow(
            #Tolerance
            shiny::column(4, shiny::numericInput("Tolerance", "Tolerance ", value = NULL, min = 1, max = NA, step = 1)),
            #Perform PCA
            shiny::column(4, shiny::selectInput("PCA", "Nuclear PCA", c(FALSE, TRUE), selected = FALSE, multiple = FALSE)),
            #Switch for cell overlay
            shiny::column(4, shinyWidgets::materialSwitch("Overlay", "Cell overlay", value = TRUE))
          ),
          
          shiny::fluidRow(
            #Action buttons
            shiny::column(2, shiny::actionButton("GO_button", shiny::icon("bolt-lightning"), label = "GO!")),
            shiny::column(4, shiny::actionButton("Download_Param", shiny::icon("download"), label = "Download Parameters"))
          ),
          shiny::fluidRow(
            #Parameters
            shiny::column(12, shiny::verbatimTextOutput("Parameters"))
          )
        ),
        
        #Set the outcome columns
        shiny::mainPanel(
          #First row will have the Photo and the overview of marker intensity by cell
          shiny::fluidRow(
            shiny::column(6, shiny::plotOutput("Photo",
                                               width = "auto",
                                               #Controls for zoom in
                                               dblclick = "Photo_dblclick",
                                               brush = shiny::brushOpts(id = "Photo_brush",
                                                                        resetOnNew = TRUE)
            )
            ),
            shiny::column(6, shiny::plotOutput("Cell_borders",
                                               width = "auto"))
          ),
          #Second row will contain the positive cells and the histogram
          shiny::fluidRow(
            shiny::column(6, shiny::plotOutput("Cell_surface",
                                               width = "auto")),
            shiny::column(6, shiny::plotOutput("Heatmap"))
          )
        )
      ),
      shiny::tags$head(shiny::tags$style(
        htmltools::HTML('
         #sidebar {
            background-color: #acf2ae;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')))
    )
    
    #BUILD THE SERVER
    server <- function(input, output, session){
      #All the reactives to be used
      #Generate a reactive with the real Image name and the channel number and pre-processing steps
      Photo_name <- shiny::reactive(str_c(Directory, "/", input$Real_Image_name))
      Channel_index <- shiny::reactive(which(input$Channel == Ordered_Channels))
      Photo_min <- shiny::reactive(input$Min_Image)
      Photo_max <- shiny::reactive(input$Max_Image)
      Photo_gamma <- shiny::reactive(10^input$Gamma)
      Equalize <- shiny::reactive(input$Equalize)
      Overlay <- shiny::reactive(input$Overlay)
      
      Pre_process <- shiny::reactive(input$Pre_processing)
      Opening_kernel <- shiny::reactive(input$Opening)
      Closing_kernel <- shiny::reactive(input$Closing)
      
      
      #Generate a reactivevalue that controls the image
      Segmentation_results <- shiny::reactiveValues(Image = NULL,
                                                    Mask = NULL,
                                                    Cell_data = NULL,
                                                    Segmentation_Parameters = NULL)
      
      #Generate a reactive that controls the zoom in
      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      
      #Reactive that imports the photograph
      Photo_reactive <- shiny::reactive({
        #Import the Photo
        Photo <- magick::image_read(Photo_name())[Channel_index()]
        #Perform image equalization as requested by user
        if(as.logical(Equalize())) Photo <- Photo %>% magick::image_equalize()
        #Perform image white adjustment
        Photo <- Photo %>% 
          magick::image_level(black_point = Photo_min(),
                              white_point = Photo_max(),
                              mid_point = Photo_gamma())
        if(as.logical(Pre_process())){
          Photo <- Photo %>% magick::as_EBImage() %>% 
            EBImage::opening(EBImage::makeBrush(size = Opening_kernel(), shape = "disc")) %>%
            EBImage::closing(EBImage::makeBrush(size = Closing_kernel(), shape = "disc"))
          Photo <- magick::image_read(Photo)
        }
        
        #Transform to raster and generate plot
        Photo <- Photo %>% magick::image_raster()
        #Return the result
        Photo
      })
      
      #Print the photo
      output$Photo <- shiny::renderPlot({
        #Obtain the photo
        Photo <- Photo_reactive()
        
        #If no cell data are available just plot the photo
        if(is.null(Segmentation_results$Cell_data) | !as.logical(Overlay())){
          #Generate the plot if no data are available
          Photo_plot <- ggplot() + geom_raster(aes(x = x, y = y, fill = col), data = Photo) + 
            scale_fill_identity() +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  panel.grid = element_blank())
        }
        
        else{
          #Generate the plot if data is available
          Photo_plot <- ggplot() + geom_raster(aes(x = x, y = y, fill = col), data = Photo) + 
            scale_fill_identity() +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            geom_point(aes(x = m.cx, y = m.cy), size = 0.1, color = "red", data = Segmentation_results$Cell_data) +
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  panel.grid = element_blank())
        }
        Photo_plot
      }, res = 300)
      
      #Control the zoom in of the Photo and the other plots
      shiny::observeEvent(input$Photo_dblclick, {
        brush <- input$Photo_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      
      #What to do when user hits the go button
      shiny::observeEvent(input$GO_button, {
        #First we need to import the photograph as EBI, select the required channels and transform it to a cytomapper object
        shiny::showModal(modalDialog("Importing image", footer=NULL))
        Image <- EBImage::readImage(str_c(Directory, "/", input$Real_Image_name))
        shiny::removeModal()
        
        #Mopdify nuclear channels if required by user
        if(as.logical(input$Pre_processing)){
          shiny::showModal(modalDialog("Performing Nuclear channels image Pre-processing", footer=NULL))
          Image <- magick::image_read(Image)
          Nuclear_channels_number <- match(input$Nuclear, Ordered_Channels)
          
          #Apply changes to every nuclear channel
          for(index in Nuclear_channels_number){
            Image_Modified <- Image[index]
            
            if(as.logical(input$Equalize)) Image_Modified <- Image_Modified %>% magick::image_equalize() #Equalize if necesary
            Image_Modified <- Image_Modified %>% magick::image_level(black_point = input$Min_Image,
                                                                     white_point = input$Max_Image,
                                                                     mid_point = 10^input$Gamma) #Chane withe, black and gamma
            Image_Modified <- Image_Modified %>% magick::as_EBImage() #turn to EBImage object
            Image_Modified  <- 
              Image_Modified %>% EBImage::opening(EBImage::makeBrush(size = input$Opening, shape = "disc")) %>%
              EBImage::closing(EBImage::makeBrush(size = input$Closing, shape = "disc")) #opening and closing
            
            Image_Modified <- magick::image_read(Image_Modified) #again as Magick image
            
            Image[index] <- Image_Modified
          }
          #Returna a EBImage object
          Image <- Image %>% magick::as_EBImage()
          shiny::removeModal()
        }
        
        
        Image <- cytomapper::CytoImageList(Image)#Transform it to cytoImage object
        cytomapper::channelNames(Image) <- Ordered_Channels #define channel names
        S4Vectors::mcols(Image)$img_id <- as.character(" ")#Modify name
        Image <- cytomapper::getChannels(Image, input$Keep_channels) #Keep only user defined channels
        Segmentation_results$Image <- Image
        
        #Perform cell segmentation
        shiny::showModal(modalDialog("Generating segmentation mask. This can take some time. Please wait", footer=NULL))
        Tolerance <- if(is.na(input$Tolerance)) NULL else(input$Tolerance)
        Seg_results <- simpleSeg::simpleSeg(Image,
                                            nucleus = input$Nuclear, 
                                            cellBody = input$Cell_body, 
                                            sizeSelection = input$Min_pix,
                                            smooth = input$Gaussian,
                                            transform = input$Normalization,
                                            watershed = input$Watershed,
                                            tolerance = Tolerance,
                                            ext = input$Neigh_dist,
                                            discSize = input$Disc_size,
                                            tissue = input$Tissue_mask,
                                            pca = as.logical(input$PCA),
                                            cores = 1
        )
        S4Vectors::mcols(Seg_results)$img_id <- as.character(" ")
        Segmentation_results$Mask <- Seg_results
        shiny::removeModal()
        
        #Obtain cell data
        shiny::showModal(modalDialog("Retrieving cell data", footer=NULL))
        Cells <- cytomapper::measureObjects(mask = Seg_results,
                                            image = Image,
                                            img_id = "img_id",
                                            feature_types = c("basic", "moment"),
                                            moment_feature = c("cx", "cy"),
                                            basic_feature = "mean")
        
        Cells_tibble <- as_tibble(cbind(as_tibble(SummarizedExperiment::colData(Cells)), 
                                        as_tibble(t(SummarizedExperiment::assays(Cells)[[1]])))) %>% dplyr::select(-objectNum)
        Segmentation_results$Cell_data <- Cells_tibble
        shiny::removeModal()
        
        #Generate a list with the final parameters
        Parameter_list <- list(
          Ordered_Channels = Channels_in_images,
          Channels_to_keep = input$Keep_channels,
          Nuclear_marker = input$Nuclear,
          Cell_body_method = input$Cell_body,
          Watershed_type = input$Watershed,
          Normalization = input$Normalization,
          Tissue_mask_markers = input$Tissue_mask,
          Smooth_amount = input$Gaussian,
          Min_pixel = input$Min_pix,
          Neighborhood_distance = input$Neigh_dist,
          Disc_size = input$Disc_size,
          Tolerance_value = Tolerance,
          Perform_PCA = as.logical(input$PCA),
          
          Perform_nuclear_channel_processing = as.logical(input$Pre_processing),
          Black_level = input$Min_Image,
          White_level = input$Max_Image,
          Gamma_level = input$Gamma,
          Equalize = as.logical(input$Equalize),
          Opening_kernel_size = input$Opening,
          Closing_kernel_size = input$Closing
        )
        Segmentation_results$Segmentation_Parameters <- Parameter_list
      })
      
      #First output cell borders
      output$Cell_borders <- shiny::renderPlot({
        if(is.null(Segmentation_results[["Image"]])) ggplot()
        else{
          #If no zoom in required
          if(is.null(ranges$x) | is.null(ranges$y)){
            Image <- EBImage::flip(Segmentation_results$Image[[1]])
            Image <- cytomapper::CytoImageList(Image)
            Image <- cytomapper::getChannels(Image, input$Nuclear[1])
            S4Vectors::mcols(Image)$img_id <- as.character(" ")
            cytomapper::channelNames(Image) <- input$Nuclear[1]
            
            Seg_results <- EBImage::flip(Segmentation_results$Mask[[1]])
            Seg_results <- cytomapper::CytoImageList(Seg_results)
            S4Vectors::mcols(Seg_results)$img_id <- as.character(" ")
          }
          #If user zooms in
          else{
            Image <- Segmentation_results$Image[[1]][ranges$x[1]:ranges$x[2], ranges$y[1]:ranges$y[2], 1]
            Image <- EBImage::flip(Image)
            Image <- cytomapper::CytoImageList(Image)
            S4Vectors::mcols(Image)$img_id <- as.character(" ")
            cytomapper::channelNames(Image) <- input$Nuclear[1]
            
            Seg_results <- Segmentation_results$Mask[[1]][ranges$x[1]:ranges$x[2], ranges$y[1]:ranges$y[2]]
            Seg_results <- EBImage::flip(Seg_results)
            Seg_results <- cytomapper::CytoImageList(Seg_results)
            S4Vectors::mcols(Seg_results)$img_id <- as.character(" ")
          }
          
          
          
          #Plot the actual plot
          color_list <- list(A = c(scales::alpha("black", 0), "#0549fc"))
          names(color_list) <- "DAPI"
          cytomapper::plotPixels(image = Image, 
                                 mask = Seg_results, 
                                 img_id = "img_id",
                                 colour_by = input$Nuclear[1], 
                                 colour = color_list,
                                 display = "single",
                                 legend = NULL) 
        }
      })
      #Second the cell surface
      output$Cell_surface <- shiny::renderPlot({
        if(is.null(Segmentation_results[["Image"]])) ggplot()
        else{
          #If no zoom in required
          if(is.null(ranges$x) | is.null(ranges$y)){
            Seg_results <- Segmentation_results$Mask[[1]]
          }
          #If user zooms in
          else{
            Seg_results <- Segmentation_results$Mask[[1]][ranges$x[1]:ranges$x[2], ranges$y[1]:ranges$y[2]]
          }
          EBImage::image(EBImage::colorLabels(Seg_results))
        }
      })
      #Finally plot the parameters
      output$Parameters <- shiny::renderPrint({
        if(is.null(Segmentation_results$Image)) "Awaiting initial test"
        else{Segmentation_results$Segmentation_Parameters}
      })
      
      #Download segmentation parameters to R session if required
      observeEvent(input$Download_Param, { 
        if(is.null(Segmentation_results$Segmentation_Parameters)){
          showModal(modalDialog(
            "Segmentation Parameters not found. Please run a test before re-trying",
            easyClose = TRUE,
            footer = NULL
          )
          )
        }
        else{
          Segmentation_Parameters <<- Segmentation_results$Segmentation_Parameters
          showModal(modalDialog(
            "An object called 'Segmentation_Parameters' has been created in the Global environment.",
            easyClose = TRUE,
            footer = NULL
          )
          )
        }
        
      })
      
      #Lets plot the Heatmap of selected cells
      output$Heatmap <- shiny::renderPlot({
        if(is.null(Segmentation_results[["Cell_data"]])) tibble(a = NA, b = NA)
        else{
          Data <- Segmentation_results$Cell_data[1:ncol(Segmentation_results$Cell_data)]
          
          #Scale our data and bind it to the original data
          Data_scaled <- Data %>% select(-c(1:5)) %>% scale()
          Data <- bind_cols(Data["m.cx"], Data["m.cy"], Data_scaled)
          
          #Prepare the min and max for our heatmap
          HEATMAP_MATRIX <- Data %>% select(-c(1:2)) %>% unlist()
          Min_HEATMAP <- quantile(HEATMAP_MATRIX, 0.05)
          Max_HEATMAP <- quantile(HEATMAP_MATRIX, 0.95)
          
          #Now we will select cells contained within area
          Min_x <- if(is.null(ranges$x)) min(Data$m.cx) else(ranges$x[1])
          Max_x <- if(is.null(ranges$x)) max(Data$m.cx) else(ranges$x[2])
          Min_y <- if(is.null(ranges$y)) min(Data$m.cy) else(ranges$y[1])
          Max_y <- if(is.null(ranges$y)) max(Data$m.cy) else(ranges$y[2])
          
          HEATMAP_MATRIX <- 
            Data %>% 
            dplyr::filter(m.cx >= Min_x,
                          m.cx <= Max_x,
                          m.cy >= Min_y,
                          m.cy <= Max_y) %>% 
            select(-c(1:2)) %>% as.matrix()
          
          #Prepare color function
          col_fun <- circlize::colorRamp2(c(Min_HEATMAP, 0, Max_HEATMAP), c("#0000ff", "white", "#ff0000"))
          
          #Plot our heatmap
          ComplexHeatmap::Heatmap(HEATMAP_MATRIX, 
                                  col = col_fun,
                                  cluster_rows = TRUE,
                                  cluster_columns = FALSE,
                                  show_row_names = FALSE,
                                  show_heatmap_legend = FALSE,
                                  show_row_dend = FALSE,
                                  show_column_dend = FALSE,
                                  column_names_side = "top",
                                  column_names_rot = 0,
                                  column_names_centered = TRUE,
                                  column_names_gp = grid::gpar(fontsize = 10),
                                  border = TRUE
          )
        }
      })
      
      #If browser is closed end the app
      session$onSessionEnded(function() { shiny::stopApp() })
    }
    
    #Run the server
    message("Always stop current R execution if you want to continue with your R session")
    shiny::shinyApp(user_interface, server)
  },
  options = list(optimize = 3))

Cell_segmentator_quantificator <- cmpfun(
  function(Directory = NULL,
           Parameter_list = NULL,
           Ordered_Channels = NULL,
           Channels_to_keep = NULL,
           N_cores = NULL,
           
           quantiles_to_calculate = NULL, # quantiles to be calculated for each marker
           
           
           Nuclear_marker = NULL, #marker or list of markers corresponding to the nuclei
           Cell_body_method = NULL, #Method of cytoplasm identification. Can be 'none', dilate', 'discModel' or the name of a dedicated cytoplasm marker
           Min_pixel = NULL, # Minimum pixels for an object to be recognized as a cell and not noise
           Smooth_amount = NULL, #The amount of Gaussian smoothing to be applied to the image
           Normalization = NULL, #Single value or list specifying the transformations from "sqrt", "asinh", "norm99", "maxThresh" and "tissueMask
           Watershed_type = NULL, #Method used to perform watersheding. Accepted values: "intensity", "distance" or "combine"
           Tolerance_value = NULL, #minimum height of the object in the units of image intensity between its highest point (seed) and the point where it contacts another object (MAY BE NULL)
           Neighborhood_distance = NULL, #Radius of the neighborhood in pixels for the detection of neighboring objects. Higher value smooths out small objects.
           Disc_size = NULL, #The size of dilation around nuclei to create cell disc or capture cytoplasm
           Tissue_mask_markers = NULL, #A vector specifying the channels to be used to create the tissue mask if specified in transforms
           Perform_PCA = FALSE, #Whether to run PCA on aggregated nucleus markers in order to detect the cellular nucclei
           
           Perform_nuclear_channel_processing = NULL,
           Black_level = NULL,
           White_level = NULL,
           Gamma_level = NULL,
           Equalize = NULL,
           Opening_kernel_size = NULL,
           Closing_kernel_size = NULL
  ){
    #Specify what to do on exit
    on.exit(gc())
    #If parameter list is provided obtain the parameters
    if(!is.null(Parameter_list)){
      Ordered_Channels <- Parameter_list[["Ordered_Channels"]]
      Channels_to_keep <- Parameter_list[["Channels_to_keep"]]
      Nuclear_marker <- Parameter_list[["Nuclear_marker"]]
      Cell_body_method <- Parameter_list[["Cell_body_method"]]
      Min_pixel <- Parameter_list[["Min_pixel"]]
      Smooth_amount <- Parameter_list[["Smooth_amount"]]
      Normalization <- Parameter_list[["Normalization"]]
      Watershed_type <- Parameter_list[["Watershed_type"]]
      Tolerance_value <- Parameter_list[["Tolerance_value"]]
      Neighborhood_distance <- Parameter_list[["Neighborhood_distance"]]
      Disc_size <- Parameter_list[["Disc_size"]]
      Tissue_mask_markers <- Parameter_list[["Tissue_mask_markers"]]
      Perform_PCA <- Parameter_list[["Perform_PCA"]]
      
      Perform_nuclear_channel_processing <- Parameter_list[["Perform_nuclear_channel_processing"]]
      Black_level <- Parameter_list[["Black_level"]]
      White_level <- Parameter_list[["White_level"]]
      Gamma_level <- Parameter_list[["Gamma_level"]]
      Equalize <- Parameter_list[["Equalize"]]
      Opening_kernel_size <- Parameter_list[["Opening_kernel_size"]]
      Closing_kernel_size <- Parameter_list[["Closing_kernel_size"]]
    }
    
    #If null import from arguments
    else if(is.null(Parameter_list)){
      Ordered_Channels <- Ordered_Channels
      Channels_to_keep <- Channels_to_keep
      Nuclear_marker <- Nuclear_marker
      Cell_body_method <- Cell_body_method
      Min_pixel <- Min_pixel
      Smooth_amount <- Smooth_amount
      Normalization <- Normalization
      Watershed_type <- Watershed_type
      Tolerance_value <- Tolerance_value
      Neighborhood_distance <- Neighborhood_distance
      Disc_size <- Disc_size
      Tissue_mask_markers <- Tissue_mask_markers
      Perform_PCA <- Perform_PCA
      Perform_nuclear_channel_processing <- Perform_nuclear_channel_processing
      Black_level <- Black_level
      White_level <- White_level
      Gamma_level <- Gamma_level
      Equalize <- Equalize
      Opening_kernel_size <- Opening_kernel_size
      Closing_kernel_size <- Closing_kernel_size
    }
    
    #Check arguments by generating a argument check vector and message vector
    Argument_checker <- c(Empty_directory = length(dir(Directory)) >= 1,
                          Channels_to_keep_OK = all(Channels_to_keep %in% Ordered_Channels),
                          N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0),
                          Nuclear_OK = unlist(Nuclear_marker) %in% Channels_to_keep,
                          Cell_body_OK = Cell_body_method %in% c("none", "dilate", "discModel"),
                          Min_pixel_OK = all(is.numeric(Min_pixel), Min_pixel%%1 == 0, Min_pixel >= 0),
                          Smooth_amount_OK = all(is.numeric(Smooth_amount), Smooth_amount >= 0),
                          Normalization_OK = Normalization %in% c("sqrt", "asinh", "norm99", "maxThresh", "tissueMask"),
                          Watershed_type_OK = Watershed_type %in% c("intensity", "distance", "combine"),
                          Tolerance_value_OK = if(!is.null(Tolerance_value)) {
                            all(is.numeric(Tolerance_value), Tolerance_value >= 0)
                          } else(TRUE),
                          Neighborhood_distance_OK = all(Neighborhood_distance%%1 == 0, Neighborhood_distance >= 0),
                          Disc_size_OK = all(Disc_size%%1 == 0, Disc_size >= 0),
                          Tissue_mask_markers_OK =  if(!is.null(Tissue_mask_markers)) {
                            all(Tissue_mask_markers %in% Channels_to_keep)
                          } else(TRUE),
                          Perform_PCA_OK = is.logical(Perform_PCA),
                          Perform_nuclear_channel_processing_OK = is.logical(Perform_nuclear_channel_processing)
    )
    
    Stop_messages <- c(Empty_directory = "No files found at the directory provided. Please check out the path.",
                       Channels_to_keep_OK = str_c(
                         "The following channels are not present the channel names provided: ",
                         str_c(Channels_to_keep[!(Channels_to_keep %in% Ordered_Channels)], collapse = ", "),
                         sep = ""),
                       N_cores_OK = "N_cores must be a positive integer value",
                       Nuclear_OK = str_c("Nuclear marker specified not found in ", str_c(Channels_to_keep, collapse = ", ")),
                       Cell_body_OK = "Cell body method must be one of the following: none, dilate, discModel",
                       Min_pixel_OK = "Min pixel must be a positive integer value",
                       Smooth_amount_OK = "Smooth amount must be a positive numeric value",
                       Normalization_OK = "Normalization method must be one of the following: sqrt, asinh, norm99, maxThresh, tissueMask",
                       Watershed_type_OK = "Watershed type must be NULL or one of the following: intensity, distance, combine",
                       Tolerance_value_OK = "Tolerance value must be NULL or a positive numeric value",
                       Neighborhood_distance_OK = "Neighborhood distance must be a positive integer value",
                       Disc_size_OK = "Disc size must be a positive integer value",
                       Tissue_mask_markers_OK =  str_c("Tissue mask must be NULL or one of the following: ", str_c(Channels_to_keep, collapse = ", ")),
                       Perform_PCA_OK = "Perform PCA must be a logical value",
                       Perform_nuclear_channel_processing_OK = "Perform_nuclear_channel_processing must be a logical value")
    
    
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    
    #Check specifically nuclear processing arguments if Perform_nuclear_channel_processing is true
    if(Perform_nuclear_channel_processing){
      if(!all(is.numeric(Black_level), Black_level >= 0, Black_level <= 100, Black_level < White_level)) stop("Black_level must be a numeric value between 0 - 100 and smaller than White_level")
      if(!all(is.numeric(White_level), White_level >= 0, White_level <= 100)) stop("White_level must be a numeric value between 0 - 100")
      if(!all(is.numeric(Gamma_level), Gamma_level >= -3, Gamma_level <= 3)) stop ("Gamma_level must be a numeric value between -3 and +3")
      if(!is.logical(Equalize)) stop("Equalize must be a logical value")
      if(!all(Opening_kernel_size%%1 == 0, Opening_kernel_size >= 1)) stop("Opening_kernel_size must be a positive integer value >= 1")
      if(!all(Closing_kernel_size%%1 == 0, Closing_kernel_size >= 1)) stop("Closing_kernel_size must be a positive integer value >= 1")
    }
    
    #Obtain the channels and the names of the directory
    Channels <- Ordered_Channels
    Simple_Image_Names <- dir(Directory, full.names = FALSE)
    Complete_Image_Names <- dir(Directory, full.names = TRUE)
    
    #If quantiles are not required for calculation print a basic summary befor proceeding
    if(is.null(quantiles_to_calculate)){
      cat(paste0("The following directory has been selected: ", Directory), 
          str_c("Files found in the provided directory: ", length(Simple_Image_Names)), 
          str_c("Number of cores to be used in the computation: ", N_cores),
          paste0("The following metrics will be obtained: basic cell morphology, mean marker expression, sd of markers by cell, "),
          fill = 4)
    }
    
    #If they are actually demanded by the user print them in the summary
    else {
      Actual_quantiles <- str_remove(as.character(quantiles_to_calculate), "\\.")
      Character_quantiles <- str_c("q", Actual_quantiles, sep = "")
      Character_quantiles
      cat(paste0("The following directory has been selected: ", Directory), 
          str_c("Files found in the provided directory: ", length(Simple_Image_Names)), 
          str_c("Number of cores to be used in the computation: ", N_cores),
          str_c("The following metrics will be obtained: basic cell morphology, mean marker expression, sd of markers by cell, ",
                str_c(Character_quantiles, collapse = ", ")),
          fill = 4)
    }
    
    
    Start_answer <- menu(choices = c("Proceed", "Abort"))
    if(Start_answer == 2){
      stop("The segmentation process has been aborted")
    }
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #We make the clusters
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    SEGMENTATION_RESULTS <- 
      furrr::future_map(seq_along(1:length(Complete_Image_Names)), function(Index){
        #Pre-process nuclear channels if required
        if(Perform_nuclear_channel_processing){
          Image <- magick::image_read(Complete_Image_Names[Index]) #Import image
          
          Nuclear_channels_number <- match(Nuclear_marker, Ordered_Channels)
          
          #Apply changes to every nuclear channel
          for(index in Nuclear_channels_number){
            Image_Modified <- Image[index]
            
            if(Equalize) Image_Modified <- Image_Modified %>% magick::image_equalize() #Equalize if necesary
            Image_Modified <- Image_Modified %>% magick::image_level(black_point = Black_level,
                                                                     white_point = White_level,
                                                                     mid_point = 10^Gamma_level) #Chane withe, black and gamma
            Image_Modified <- Image_Modified %>% magick::as_EBImage() #turn to EBImage object
            Image_Modified  <- 
              Image_Modified %>% EBImage::opening(EBImage::makeBrush(size = Opening_kernel_size, shape = "disc")) %>%
              EBImage::closing(EBImage::makeBrush(size = Closing_kernel_size, shape = "disc")) #opening and closing
            
            Image_Modified <- magick::image_read(Image_Modified) #again as Magick image
            
            Image[index] <- Image_Modified
          }
          #Returna a EBImage object
          Image <- Image %>% magick::as_EBImage()
        }
        
        #Import image with the EBImage importer if no processing is required
        else{
          Image <- EBImage::readImage(Complete_Image_Names[Index])
        }
        
        #Transform it to cytoImage object
        Image <- cytomapper::CytoImageList(Image)
        cytomapper::channelNames(Image) <- Ordered_Channels #define channel names
        S4Vectors::mcols(Image)$imageID <- as.character(Simple_Image_Names[Index])#Modify name
        Image <- cytomapper::getChannels(Image, Ordered_Channels[Ordered_Channels %in% Channels_to_keep]) #Keep only user defined channels
        
        #Perform cell segmentation
        Seg_results <- simpleSeg::simpleSeg(Image,
                                            nucleus = Nuclear_marker, 
                                            cellBody = Cell_body_method, 
                                            sizeSelection = Min_pixel,
                                            smooth = Smooth_amount,
                                            transform = Normalization,
                                            watershed = Watershed_type,
                                            tolerance = Tolerance_value,
                                            ext = Neighborhood_distance,
                                            discSize = Disc_size,
                                            tissue = Tissue_mask_markers,
                                            pca = Perform_PCA,
                                            cores = 1)
        S4Vectors::mcols(Seg_results)$imageID <- as.character(Simple_Image_Names[Index])
        
        
        #Calculate basic tibble with morphology 
        Position_morphology_mean <- cytomapper::measureObjects(mask = Seg_results,
                                                               image = Image,
                                                               img_id = "imageID",
                                                               feature_types = c("basic", "shape", "moment"),
                                                               shape_feature = c("area", "perimeter", "radius.mean", "radius.sd", "radius.max", "radius.min"),
                                                               moment_feature = c("cx", "cy", "eccentricity", "majoraxis"),
                                                               basic_feature = "mean")
        
        Position_morphology_mean <- as_tibble(cbind(as_tibble(SummarizedExperiment::colData(Position_morphology_mean)), 
                                                    as_tibble(t(SummarizedExperiment::assays(Position_morphology_mean)[[1]])))) %>% dplyr::select(-objectNum)
        names(Position_morphology_mean)[-c(1:12)] <- str_c(names(Position_morphology_mean)[-c(1:12)], "_AVERAGE")
        
        #Calculate the tibble with sd
        Position_sd <- cytomapper::measureObjects(mask = Seg_results,
                                                  image = Image,
                                                  img_id = "imageID",
                                                  feature_types = "basic",
                                                  basic_feature = "sd"
        )
        
        Position_sd <- as_tibble(cbind(as_tibble(SummarizedExperiment::colData(Position_sd)), 
                                       as_tibble(t(SummarizedExperiment::assays(Position_sd)[[1]])))) %>% dplyr::select(-objectNum)
        names(Position_sd)[-c(1:2)] <- str_c(names(Position_sd)[-c(1:2)], "_SD")
        
        #Bind both tibbles
        Final_tibble <- dplyr::bind_cols(Position_morphology_mean, Position_sd[-c(1:2)])
        
        #If no quantiles required return the basic tibble info
        if(is.null(quantiles_to_calculate)){
          #Remove Image to save RAM space
          rm(Image)
          gc()
          return(Final_tibble)
        }
        
        else{
          #If provided calculate the desired quantiles for each image
          quantile_tibble <- map_dfc(seq_along(1:length(quantiles_to_calculate)), 
                                     function(quantile_index){
                                       quantile_info <- cytomapper::measureObjects(mask = Seg_results,
                                                                                   image = Image,
                                                                                   img_id = "imageID",
                                                                                   feature_types = "basic",
                                                                                   basic_feature = Character_quantiles[quantile_index],
                                                                                   basic_quantiles = quantiles_to_calculate[quantile_index]
                                       )
                                       quantile_info <- as_tibble(cbind(as_tibble(SummarizedExperiment::colData(quantile_info)), 
                                                                        as_tibble(t(SummarizedExperiment::assays(quantile_info)[[1]])))) %>% dplyr::select(-objectNum)
                                       names(quantile_info)[-c(1:2)] <- str_c(names(quantile_info)[-c(1:2)], Character_quantiles[quantile_index], sep = "_")
                                       quantile_info[-c(1:2)]
                                     })
          #Remove Image to save RAM space
          rm(Image)
          gc()
          #bind both tibbles and return the result
          return(dplyr::bind_cols(Final_tibble, quantile_tibble))
        }
      },
      .progress = TRUE)
    future::plan("future::sequential")
    gc()
    
    
    return(map_dfr(SEGMENTATION_RESULTS, bind_rows))
    
  }, options = list(optimize = 3))

Image_thresholding_app_launcher <- cmpfun(
  function(Directory = NULL,
           Ordered_Channels = NULL){
    #check that the directory provided contains at least one file
    if(length(dir(Directory)) <1 ) stop("No files found in the Directory provided")
    
    #Obtain image names and channel names
    Real_Images <- dir(Directory, full.names = FALSE)
    Complete_names <- dir(Directory, full.names = TRUE)
    Channels_in_images <- Ordered_Channels
    
    #BUILD THE USER INTERFACE
    {
      user_interface <- shiny::fluidPage(
        
        #Set the title
        shiny::titlePanel("Thresholding exploration APP"),
        
        #We want a two panel layout, one in the left containing the input parameters and the output in the right
        shiny::sidebarLayout(
          #Set the first column (which contains the user defined parameters)
          shiny::sidebarPanel(
            #ID and width
            id="sidebar",
            
            #Image to display and resolution
            shiny::h4("Image parameters"),
            shiny::fluidRow(
              #Select the real image to be displayed
              shiny::column(6, shiny::selectInput("Real_Image_name", "Image to display", sort(Real_Images), multiple = FALSE)),
              #Select the channel to be displayed
              shiny::column(4, shiny::selectInput("Channel", "Channel to display", Channels_in_images, multiple = FALSE)),
              #Select the resolution
              shiny::column(2, shiny::selectInput("Res", "Image Res", c(Low = 150, Mid = 300, high = 600, Original = 1000), selected = 150, multiple = FALSE))
            ),
            
            #Channels included in tissue threshold
            shiny::h4("Tissue mask parameters"),
            shiny::fluidRow(
              #Select the channels to include in the tissue mask generation
              shiny::column(3, shinyWidgets::virtualSelectInput("Tissue_mask_channels", label = "Channels used",
                                                                choices = Channels_in_images, 
                                                                search = TRUE,
                                                                multiple = TRUE
              )),
              #Select the type of threshold being used for tissue mask
              shiny::column(3, shiny::selectInput("Tissue_threshold_type", "Threshold type", c("Otsu", "Arbitrary", "Absolute"), multiple = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Tissue_threshold_type == 'Arbitrary'",shiny::textInput("Tissue_value", "Value", value = "0.01"))),
              #Select tissue mask blurr
              shiny::column(2, shiny::selectInput("Tissue_blurr", "Blurr", c(TRUE,FALSE), selected = FALSE, multiple = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Tissue_blurr == 'TRUE'",shiny::textInput("Tissue_sigma", "Sigma", value = "0.5")))
            ),
            
            shiny::h4("Tissue thresholding parameters"),
            shiny::fluidRow(
              #Local or global thresholding
              shiny::column(3, shiny::selectInput("Target_threshold_local", "Type", c("Global", "Local"), selected = "Local", multiple = FALSE)),
              
              #Threshold type
              shiny::column(3, shiny::selectInput("Target_threshold_type", "Type", c("Otsu", "Arbitrary", "Multilevel"), selected = "Otsu", multiple = FALSE)),
              
              #Select target mask blurr
              shiny::column(3, shiny::selectInput("Target_blurr", "Blurr", c(TRUE,FALSE), selected = FALSE, multiple = FALSE)),
              shiny::column(3, shiny::conditionalPanel(condition = "input.Target_blurr == 'TRUE'",shiny::textInput("Target_sigma", "Sigma", value = "0.5")))
            ),
            #Conditional panels for values
            shiny::fluidRow(
              shiny::column(6, shiny::conditionalPanel(condition = "input.Target_threshold_type == 'Arbitrary' || input.Target_threshold_type == 'Multilevel'",
                                                       shiny::textInput("Target_Value", "Value", value = NULL))),
              shiny::column(6, shiny::conditionalPanel(condition = "input.Target_threshold_type == 'Multilevel'",
                                                       shiny::numericInput("Target_Levels", "Levels", value = 3, min = 3, max = NA, step = 1)))
            ),
            
            shiny::fluidRow(shiny::column(2, shiny::actionButton("GO_button", shiny::icon("bolt-lightning"), label = "GO!")))
            
          ),
          
          #Set the outcome columns
          shiny::mainPanel(
            #First row will have the Photo and the overview of marker intensity by cell
            shiny::fluidRow(
              shiny::column(6, shiny::plotOutput("Photo",
                                                 width = "auto",
                                                 #Controls for zoom in
                                                 dblclick = "Photo_dblclick",
                                                 brush = shiny::brushOpts(id = "Photo_brush",
                                                                          resetOnNew = TRUE)
              )
              ),
              shiny::column(6, shiny::plotOutput("Tissue_mask",
                                                 width = "auto"))
            ),
            #Second row will contain the positive cells and the histogram
            shiny::fluidRow(
              shiny::column(6, shiny::plotOutput("Target_mask",
                                                 width = "auto")),
              shiny::column(6, shiny::verbatimTextOutput("Pixel_summary"))
            )
          )
        ),
        shiny::tags$head(shiny::tags$style(
          htmltools::HTML('
         #sidebar {
            background-color: #fc92fc;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')))
      )
    }
    
    
    #BUILD THE SERVER
    server <- function(input, output, session){
      #First generate the reactives (those that need to be continuously tested for reactivity)
      Photo_name <- shiny::reactive(str_c(Directory, "/", input$Real_Image_name))
      Channel_index <- shiny::reactive(which(input$Channel == Ordered_Channels))
      Image_resolution <- shiny::reactive(input$Res)
      #Generate a reactive that controls the zoom in
      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      #Generate the reactive that controls the results in the panel
      Image_results <- shiny::reactiveValues(Tissue_mask = NULL,
                                             Target_mask = NULL,
                                             Pixel_data = NULL)
      
      
      #Import the actual photo
      Photo_reactive <- shiny::reactive({
        #Import the Photo
        Photo <- magick::image_read(Photo_name())[Channel_index()]
        return(Photo)
      })
      
      #Print the photo
      output$Photo <- shiny::renderPlot({
        #Obtain the photo
        Photo <- Photo_reactive()
        #Change resolution if required
        if(as.numeric(Image_resolution()) < 1000){
          Image_Resolution <- str_c("X", Image_resolution())
          Photo <- magick::image_scale(Photo, geometry = Image_Resolution)
        }
        #Get image info
        Image_information <- magick::image_info(Photo)
        Width <- Image_information$width
        Height <- Image_information$height
        
        #Generate the plot as annotation_raster
        return(
          ggplot() +
            annotation_raster(Photo, xmin = 0, xmax = Width, ymin = 0, ymax = Height, interpolate = TRUE)+
            scale_x_continuous(limits = c(0, Width)) +
            scale_y_continuous(limits = c(0, Height)) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  panel.grid = element_blank())
        )
      }, res = 300)
      
      #Control the zoom in of the Photo and the other plots
      shiny::observeEvent(input$Photo_dblclick, {
        brush <- input$Photo_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      
      shiny::observeEvent(input$GO_button, {
        #First we need to import the photograph as EBI, select the required channels and transform it to a cytomapper object
        shiny::showModal(modalDialog("Importing image", footer=NULL))
        Image <- magick::image_read(Photo_name())
        Image <- Image[which(Ordered_Channels %in% input$Tissue_mask_channels)]
        Image <- magick::as_EBImage(Image)
        shiny::removeModal()
        
        #Generate tissue mask
        shiny::showModal(modalDialog("Generating tissue mask", footer=NULL))
        Tissue_mask <- Tissue_mask_generator(Image = Image,
                                             Threshold_type = input$Tissue_threshold_type,
                                             Threshold_value = as.numeric(input$Tissue_value),
                                             Blurr = as.logical(input$Tissue_blurr),
                                             Sigma = as.numeric(input$Tissue_sigma))
        Image_results$Tissue_mask <- Tissue_mask
        shiny::removeModal()
        
        #Perform tissue thresholding according to user preferences
        shiny::showModal(modalDialog("Thresholding pixels", footer=NULL))
        Image_index <- match(input$Channel, input$Tissue_mask_channels)
        Target_image <- EBImage::getFrame(y = Image, i = Image_index)
        
        
        #If Arbitrary (Global or local)
        if(input$Target_threshold_type == "Arbitrary"){
          Target_result <- Pixel_thresholder(Target = Target_image ,
                                             Tissue_mask = Tissue_mask,
                                             Threshold_type = "Arbitrary",
                                             Threshold_value = as.numeric(input$Target_Value),
                                             Blurr = as.logical(input$Target_blurr),
                                             Sigma = as.numeric(input$Target_sigma)
          )
          Image_results$Target_mask <- Target_result
        }
        
        #If local and Otsu
        if(all(input$Target_threshold_type == "Otsu", input$Target_threshold_local == "Local")){
          Target_result <- Pixel_thresholder(Target = Target_image ,
                                             Tissue_mask = Tissue_mask,
                                             Threshold_type = "Otsu",
                                             Threshold_value = NULL,
                                             Blurr = as.logical(input$Target_blurr),
                                             Sigma = as.numeric(input$Target_sigma)
          )
          Image_results$Target_mask <- Target_result
        }
        
        #If global and Otsu
        if(all(input$Target_threshold_type == "Otsu", input$Target_threshold_local == "Global")){
          shiny::showModal(modalDialog("Calculating global Otsu images taking into account all images in Directory. This can take a while", footer=NULL))
          #Calculate a Tissue mask list
          Tissue_mask_list <- map(seq_along(1:length(Complete_names)), 
                                  function(Image_index){
                                    Image <- magick::image_read(Complete_names[Image_index])[which(Ordered_Channels %in% input$Tissue_mask_channels)]
                                    Image <- magick::as_EBImage(Image)
                                    
                                    #First generate the tissue mask and then remove the original image (no longer required)
                                    Tissue_mask <- Tissue_mask_generator(Image = Image,
                                                                         Threshold_type = input$Tissue_threshold_type,
                                                                         Threshold_value = as.numeric(input$Tissue_value),
                                                                         Blurr = as.logical(input$Tissue_blurr),
                                                                         Sigma = as.numeric(input$Tissue_sigma))
                                    rm(Image)
                                    gc()
                                    return(Tissue_mask)
                                  })
          #Generate a full vector containing all image values with an applied tissue mask
          Composite_Image_list <- map2(.x = 1:length(Complete_names), .y = Tissue_mask_list, function(.x, .y){
            #Import target image
            Image <- magick::image_read(Complete_names[.x])[which(Ordered_Channels %in% input$Tissue_mask_channels)]
            Target_Image <- Image[which(input$Channel == input$Tissue_mask_channels)]
            Target_Image <- magick::as_EBImage(Target_Image)
            
            #Turn target image values outside tissue mask to 0
            Target_Image[!.y] <- 0
            
            #return as vector
            return(as.vector(Target_Image))
          })
          
          #Generare a unified vector using all images
          Common_vector <- unlist(Composite_Image_list)
          #Apply otsu algorithm
          Threshold_global_otsu <- EBImage::otsu(array(Common_vector, dim = c(1, length(Common_vector))), range = c(min(Common_vector), max(Common_vector)), levels = length(unique(Common_vector)))
          Target_result <- Pixel_thresholder(Target = Target_image ,
                                             Tissue_mask = Tissue_mask,
                                             Threshold_type = "Arbitrary",
                                             Threshold_value = Threshold_global_otsu,
                                             Blurr = as.logical(input$Target_blurr),
                                             Sigma = as.numeric(input$Target_sigma)
          )
          Image_results$Target_mask <- Target_result
        }
        
        #If Multilevel input$Target_Value != "" (Global or local)
        if(all(input$Target_threshold_type == "Multilevel", input$Target_Value != "")){
          #Generate the adequate parsing of expression contained in value
          values_for_function <- eval(parse(text = as.character(input$Target_Value)))
          
          Target_result <- Pixel_Multilevel_thresholder(Target = Target_image,
                                                        Tissue_mask = Tissue_mask,
                                                        Threshold_values = values_for_function,
                                                        Blurr = as.logical(input$Target_blurr),
                                                        Sigma = as.numeric(input$Target_sigma)
          )
          Target_result$Image <- Target_result$Image/length(values_for_function)
          Image_results$Target_mask <- Target_result
        }
        
        #If Multilevel Local and input$Target_Value == ""
        if(all(input$Target_threshold_type == "Multilevel", input$Target_Value == "", input$Target_threshold_local == "Local")){
          Threshold_levels <- imagerExtra::ThresholdML(imager::cimg(array(as.vector(Target_image), dim = c(1, length(as.vector(Target_image)), 1, 1))), 
                                                       k = (as.numeric(input$Target_Levels)-1),
                                                       returnvalue = TRUE)
          
          Target_result <- Pixel_Multilevel_thresholder(Target = Target_image,
                                                        Tissue_mask = Tissue_mask,
                                                        Threshold_values = Threshold_levels,
                                                        Blurr = as.logical(input$Target_blurr),
                                                        Sigma = as.numeric(input$Target_sigma)
          )
          Target_result$Image <- Target_result$Image/length(Threshold_levels)
          Image_results$Target_mask <- Target_result
        }
        
        #If Multilevel Global and input$Target_Value == ""
        if(all(input$Target_threshold_type == "Multilevel", input$Target_Value == "", input$Target_threshold_local == "Global")){
          shiny::showModal(modalDialog("Calculating global Multilevels images taking into account all images in Directory. This can take a while", footer=NULL))
          #Calculate a Tissue mask list
          Tissue_mask_list <- map(seq_along(1:length(Complete_names)), 
                                  function(Image_index){
                                    Image <- magick::image_read(Complete_names[Image_index])[which(Ordered_Channels %in% input$Tissue_mask_channels)]
                                    Image <- magick::as_EBImage(Image)
                                    
                                    #First generate the tissue mask and then remove the original image (no longer required)
                                    Tissue_mask <- Tissue_mask_generator(Image = Image,
                                                                         Threshold_type = input$Tissue_threshold_type,
                                                                         Threshold_value = as.numeric(input$Tissue_value),
                                                                         Blurr = as.logical(input$Tissue_blurr),
                                                                         Sigma = as.numeric(input$Tissue_sigma))
                                    rm(Image)
                                    gc()
                                    return(Tissue_mask)
                                  })
          #Generate a full vector containing all image values with an applied tissue mask
          Composite_Image_list <- map2(.x = 1:length(Complete_names), .y = Tissue_mask_list, function(.x, .y){
            #Import target image
            Image <- magick::image_read(Complete_names[.x])[which(Ordered_Channels %in% input$Tissue_mask_channels)]
            Target_Image <- Image[which(input$Channel == input$Tissue_mask_channels)]
            Target_Image <- magick::as_EBImage(Target_Image)
            
            #Turn target image values outside tissue mask to 0
            Target_Image[!.y] <- 0
            
            #return as vector
            return(as.vector(Target_Image))
          })
          
          #Generare a unified vector using all images
          Common_vector <- unlist(Composite_Image_list)
          
          Threshold_levels <- imagerExtra::ThresholdML(imager::cimg(array(as.vector(Common_vector), dim = c(1, length(as.vector(Common_vector)), 1, 1))), 
                                                       k = (as.numeric(input$Target_Levels)-1),
                                                       returnvalue = TRUE)
          Target_result <- Pixel_Multilevel_thresholder(Target = Target_image,
                                                        Tissue_mask = Tissue_mask,
                                                        Threshold_values = Threshold_levels,
                                                        Blurr = as.logical(input$Target_blurr),
                                                        Sigma = as.numeric(input$Target_sigma)
          )
          Target_result$Image <- Target_result$Image/length(Threshold_levels)
          Image_results$Target_mask <- Target_result
        }
        
        #Add final parameters
        Image_results$Target_mask$mask_channels <- str_c(input$Tissue_mask_channels, collapse = ", ")
        Image_results$Target_mask$Tissue_mask <- c("Threshold_type" = input$Tissue_threshold_type,
                                                   "Value" = as.character(input$Tissue_value),
                                                   "Blurr" = as.character(input$Tissue_blurr),
                                                   "Sigma" = as.character(input$Tissue_sigma))
        
        Image_results$Target_mask$Target <- c("Threshold_type" = input$Target_threshold_type,
                                              "Value" = as.character(input$Target_Value),
                                              "Blurr" = as.character(input$Target_blurr),
                                              "Sigma" = as.character(input$Target_sigma),
                                              "Type" = as.character(input$Target_threshold_local))
        Image_results$Target_mask$Image_analized <- input$Real_Image_name
        
        shiny::removeModal()
      })
      
      #Tissue mask output
      output$Tissue_mask <- shiny::renderPlot({
        #If no plot has been generated the print a plot
        if(is.null(Image_results[["Tissue_mask"]])) ggplot()
        else{
          Photo <- Image_results[["Tissue_mask"]] 
          Photo[Photo] <- 1
          Photo <- magick::image_read(Photo)
          #Change resolution if required
          if(as.numeric(Image_resolution()) < 1000){
            Image_Resolution <- str_c("X", Image_resolution())
            Photo <- magick::image_scale(Photo, geometry = Image_Resolution)
          }
          #Get image info
          Image_information <- magick::image_info(Photo)
          Width <- Image_information$width
          Height <- Image_information$height
          
          #Generate the plot as annotation_raster
          return(
            ggplot() +
              annotation_raster(Photo, xmin = 0, xmax = Width, ymin = 0, ymax = Height, interpolate = TRUE)+
              scale_x_continuous(limits = c(0, Width)) +
              scale_y_continuous(limits = c(0, Height)) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
              theme(axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.line = element_blank(),
                    panel.background = element_rect(fill = "black"),
                    panel.grid = element_blank())
          )
          
        }
      })
      
      #Target output
      output$Target_mask <- shiny::renderPlot({
        #If no plot has been generated the print a plot
        if(is.null(Image_results[["Target_mask"]])) ggplot()
        else{
          Photo <- Image_results[["Target_mask"]][["Image"]]
          Photo <- Photo*1
          Photo <- magick::image_read(Photo)
          #Change resolution if required
          if(as.numeric(Image_resolution()) < 1000){
            Image_Resolution <- str_c("X", Image_resolution())
            Photo <- magick::image_scale(Photo, geometry = Image_Resolution)
          }
          #Get image info
          Image_information <- magick::image_info(Photo)
          Width <- Image_information$width
          Height <- Image_information$height
          
          #Generate the plot as annotation_raster
          return(
            ggplot() +
              annotation_raster(Photo, xmin = 0, xmax = Width, ymin = 0, ymax = Height, interpolate = TRUE)+
              scale_x_continuous(limits = c(0, Width)) +
              scale_y_continuous(limits = c(0, Height)) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
              theme(axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.line = element_blank(),
                    panel.background = element_rect(fill = "black"),
                    panel.grid = element_blank())
          )
          
        }
      })
      
      #Summary of results
      output$Pixel_summary <- shiny::renderPrint({
        if(is.null(Image_results[["Target_mask"]])) cat("Awaiting initial test")
        else{
          Result_list <- Image_results[["Target_mask"]]
          Result_list <- Result_list[-1]
          
          
          Result_list
        }
        
      }
      )
      
      #If browser is closed end the app
      session$onSessionEnded(function() { shiny::stopApp() })
    }
    
    #Run the server
    message("Always stop current R execution if you want to continue with your R session")
    shiny::shinyApp(user_interface, server)
  },
  options = list(optimize = 3))

Tissue_mask_generator <- cmpfun(
  function(Image = NULL,
           Threshold_type = NULL,
           Threshold_value = NULL,
           Blurr = NULL,
           Sigma = NULL){
    
    #First if Image has more than 1 frame sum all frames into 1 and normalize result
    if(EBImage::numberOfFrames(Image) > 1) {
      Image <- purrr::reduce(EBImage::getFrames(Image),
                             function(Image1, Image2) Image1 + Image2)
      Image <- Image / max(Image)
    }
    #If not normalized to max Image value
    else{
      Image <- Image / max(Image)
    }
    
    #Perform image blurring if required
    if(Blurr){
      Image <- EBImage::gblur(Image, sigma = Sigma, boundary = "replicate")
    }
    
    
    #Perform Otsu thresholding if required
    if(Threshold_type == "Otsu"){
      Otsu_thres <- EBImage::otsu(Image)
      Image <- Image >= Otsu_thres
      return(Image)
    }
    
    #Perform thresholding according to arbitrary value
    if(Threshold_type == "Arbitrary"){
      Image <- Image >= Threshold_value
      return(Image)
    }
    
    #Perform thresholding if any pixel is above 0
    if(Threshold_type == "Absolute"){
      Image <- Image > 0
      return(Image)
    }
  },
  options = list(optimize = 3))

Pixel_thresholder <- cmpfun(
  function(Target = NULL,
           Tissue_mask = NULL,
           Threshold_type = NULL,
           Threshold_value = NULL,
           Blurr = NULL,
           Sigma = NULL){
    
    #Import Image
    Image <- Target
    
    #Apply blurring if required
    #Perform image blurring if required
    if(Blurr){
      Image <- EBImage::gblur(Image, sigma = Sigma, boundary = "replicate")
    }
    
    #Turn to 0 pixels outside tissue mask
    Target[!Tissue_mask] <- 0
    
    #Perform Otsu thresholding if required
    if(Threshold_type == "Otsu"){
      Otsu_thres <- EBImage::otsu(Image)
      Image <- Image >= Otsu_thres
      return(list(Image = Image,
                  Pixel_count = sum(Image),
                  Total_foreground_pixels = sum(Tissue_mask),
                  Threshold_value = Otsu_thres)
      )
    }
    
    #Perform thresholding according to arbitrary value
    if(Threshold_type == "Arbitrary"){
      Image <- Image >= Threshold_value
      return(list(Image = Image,
                  Pixel_count = sum(Image),
                  Total_foreground_pixels = sum(Tissue_mask),
                  Threshold_value = Threshold_value)
      )
    }
  },
  options = list(optimize = 3)
)

Pixel_Multilevel_thresholder <- cmpfun(
  function(Target = NULL,
           Tissue_mask = NULL,
           Threshold_values = NULL,
           Blurr = NULL,
           Sigma = NULL){
    
    #Import Image
    Image <- Target
    
    #Apply blurring if required
    if(Blurr){
      Image <- EBImage::gblur(Image, sigma = Sigma, boundary = "replicate")
    }
    
    #Turn to 0 pixels outside tissue mask
    Target[!Tissue_mask] <- 0
    
    #For every threshold value, calculate a binary image (logical) then sum up the results
    Image <- purrr::reduce(map(Threshold_values, function(Threshold) Image > Threshold),
                           function(Image1, Image2) Image1 + Image2
    )
    
    Pixel_counts <- map_dbl(unique(as.vector(Image)), function(Value) sum(Image == Value))
    names(Pixel_counts) <- as.character(unique(as.vector(Image)))
    
    #If pixel count number is above total pixels then remove pixels from the lowest group (the 0 one)
    if(sum(Pixel_counts) > sum(Tissue_mask)){
      Diff <- sum(Pixel_counts) - sum(Tissue_mask)
      Pixel_counts[1] <- Pixel_counts[1] - Diff
    }
    
    return(list(
      Image = Image,
      Pixel_count = Pixel_counts,
      Total_foreground_pixels = sum(Tissue_mask),
      Threshold_value = str_c(Threshold_values, collapse = "_")
    ))
  },
  options = list(optimize = 3)
)

MFI_calculator <- 
  cmpfun(
    function(Target = NULL,
             Tissue_mask = NULL){
      #Replace non tissue pixels for 0
      Target[!Tissue_mask] <- 0
      #Calculate MFI (sum of intensity divided by total foreground pixels)
      return(list(MFI = sum(Target) / sum(Tissue_mask),
                  Total_foreground_pixels = sum(Tissue_mask))
      )
    },
    options = list(optimize = 3)
  )

#This final function will generate a tissue mask that combines tissue and a specific marker
Multi_mask_generator <- cmpfun(
  function(...){
    purrr::reduce(...,
                  function(Image1, Image2) Image1 & Image2)
    
  }
)

Pixel_Threshold_calculator <- cmpfun(function(
    N_cores = NULL,  
    Directory = NULL,
    Ordered_Channels = NULL,
    Channels_to_keep = NULL,
    Target_channel = NULL,
    Save_processed_images = NULL,
    Output_Directory = NULL,
    
    Local_thresholding = NULL,
    Threshold_type = NULL,
    Threshold_value = NULL,
    Levels = NULL,
    
    Threshold_type_tissueMask = NULL,
    Threshold_value_tissueMask = NULL,
    Blurr_tissueMask = NULL,
    Sigma_tissueMask = NULL,
    
    Blurr_target = NULL,
    Sigma_target = NULL
){
  
  on.exit({
    future::plan("future::sequential")
    gc()
  })
  
  #Argument check
  Argument_checker <- c(N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0),
                        Empty_directory = length(dir(Directory)) >= 1,
                        Channels_OK = all(Channels_to_keep %in% Ordered_Channels),
                        Target_channel_OK = Target_channel %in% Channels_to_keep,
                        Save_processed_images_OK = is.logical(Save_processed_images),
                        Output_Directory_OK = if(Save_processed_images){
                          length(dir(Output_Directory)) == 0
                        }else(TRUE),
                        
                        Local_thresholding_OK = is.logical(Local_thresholding),
                        Threshold_type_OK = Threshold_type %in% c("Otsu", "Arbitrary", "Multilevel"),
                        Threshold_value_OK = if(Threshold_type == "Arbitrary"){
                          all(is.numeric(Threshold_value), Threshold_value >=0, Threshold_value <= 1)
                        } else if(Threshold_type == "Multilevel"){
                          any(is.null(Threshold_value),
                              all(length(Threshold_value) > 1, is.numeric(Threshold_value), dplyr::if_else(Threshold_value >= 0 & Threshold_value <= 1, TRUE, FALSE))
                          )
                        } else(TRUE),
                        Levels_OK = if(Threshold_type == "Multilevel"){
                          all(is.numeric(Levels), Levels%%1 == 0, Levels >= 2)
                        }else(TRUE),
                        Threshold_type_tissueMask_OK = Threshold_type_tissueMask %in% c("Otsu", "Arbitrary", "Absolute"),
                        Threshold_value_tissueMask_OK = if(Threshold_type_tissueMask == "Arbitrary"){
                          all(is.numeric(Threshold_value_tissueMask), Threshold_value_tissueMask >=0, Threshold_value_tissueMask <= 1)
                        }else(TRUE),
                        Blurr_tissueMask_OK = is.logical(Blurr_tissueMask),
                        Sigma_tissueMask_OK = if(Blurr_tissueMask){
                          all(is.numeric(Sigma_tissueMask), Sigma_tissueMask > 0)
                        }else(TRUE),
                        Blurr_target_OK = is.logical(Blurr_target),
                        Sigma_target_OK = if(Blurr_target){
                          all(is.numeric(Sigma_target), Sigma_target > 0)
                        }else(TRUE)
  )
  
  Stop_messages <- c(N_cores_OK = "N_cores must be an integer value > 0",
                     Empty_directory = "No files found at the directory provided. Please check out the path.",
                     Channels_OK = str_c(
                       "The following channels are not present the channel names provided: ",
                       str_c(Channels_to_keep[!(Channels_to_keep %in% Ordered_Channels)], collapse = ", "),
                       sep = ""),
                     Target_channel_OK = str_c(Target_channel, " not present in ", str_c(Channels_to_keep, collapse = ", "), collapse = ""),
                     Save_processed_images_OK = "Save_processed_images must be a logical value",
                     Output_Directory_OK = "Output_Directory must be an empty folder",
                     Local_thresholding_OK = "Local_thresholding must be a logical value",
                     Threshold_type_OK = "Threshold_type must be one of the following: Otsu, Arbitrary, Multilevel",
                     Threshold_value_OK = "Threshold_value must be a single numeric value between 0 and 1 if Arbitrary or multiple numeric values between 0 and 1 for Multilevel",
                     Levels_OK = "Levels must be an integer value > 1",
                     Threshold_type_tissueMask_OK = "Threshold_type_TissueMask must be one of the following: Otsu, Arbitrary, Absolute",
                     Threshold_value_tissueMask_OK = "Threshold_value_tissueMask must be a single numeric value between 0 and 1",
                     Blurr_tissueMask_OK = "Blurr_tissueMask must be a logical value",
                     Sigma_tissueMask_OK = "Sigma_tissueMask must be a positive numeric value > 0",
                     Blurr_target_OK = "Blurr_target must be a logical value",
                     Sigma_target_OK = "Sigma_target must be a positive numeric value > 0"
  )
  #Check arguments and stop if necessary
  if(!all(Argument_checker)){
    stop(cat(Stop_messages[!Argument_checker],
             fill = sum(!Argument_checker)))
  }
  
  #Get the image full directory
  Image_names <- dir(Directory, full.names = TRUE)
  Image_names_short <- dir(Directory, full.names = FALSE)
  Channels_to_keep_index <- which(Channels_to_keep %in% Ordered_Channels)
  
  #If user has decided to use Multilevel and has supplied LEVELS and Thresholds value, Threshold values will prevail
  if(all(Threshold_type == "Multilevel", !is.null(Threshold_value), !is.null(Levels))){
    message("Both Threshold_value and Levels provided. Threshold_value will be used in image thresholding")
  }
  
  #The function is branched according to Local vs Global thresholding
  #First local thresholding
  if(Local_thresholding){
    print("Performing Local thresholding")
    
    #Will iterate for every image
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <- suppressMessages(
      furrr::future_map(seq_along(1:length(Image_names)), function(Image_index){
        #Import the image and keep only required channels and the target channel
        Image <- magick::image_read(Image_names[Image_index])[Channels_to_keep_index]
        Target_Image <- Image[which(Target_channel == Channels_to_keep)]
        
        Image <- magick::as_EBImage(Image)
        Target_Image <- magick::as_EBImage(Target_Image)
        
        #First generate the tissue mask and then remove the original image (no longer required)
        Tissue_mask <- Tissue_mask_generator(Image = Image,
                                             Threshold_type = Threshold_type_tissueMask,
                                             Threshold_value = Threshold_value_tissueMask,
                                             Blurr = Blurr_tissueMask,
                                             Sigma = Sigma_tissueMask)
        rm(Image)
        gc()
        
        #Now we proceed with threshold calculation
        #First binary thresholding
        if(Threshold_type != "Multilevel"){ 
          Target_Image <- Pixel_thresholder(Target = Target_Image,
                                            Tissue_mask = Tissue_mask,
                                            Threshold_type = Threshold_type,
                                            Threshold_value = Threshold_value,
                                            Blurr = Blurr_target,
                                            Sigma = Sigma_target)
          #save images and tissue masks if required by user
          if(Save_processed_images){
            #Mask
            EBImage::writeImage(Tissue_mask, paste0(Output_Directory, "/", "Processed_", Image_names_short[Image_index], "_Tissue_mask", ".tiff"))
            #Result
            EBImage::writeImage(Target_Image$Image, paste0(Output_Directory, "/", "Processed_", 
                                                           str_replace_all(Image_names_short[Result_Image_index], pattern = "_", replacement = "."),  
                                                           "_", 
                                                           str_replace_all(Target_channel, pattern = "_", replacement = "."),  "_BinaryThresholded", ".tiff"))
          }
          #Return the actual image
          return(Target_Image)
        }
        
        #Multilevel
        if(Threshold_type == "Multilevel"){
          #If threshold values have been supplied use them
          if(!is.null(Threshold_value)){
            Threshold_levels <- base::sort(Threshold_value)
          }
          #Else compute thresholds with imagerExtra
          else{
            Target_Image[!Tissue_mask] <- 0
            Threshold_levels <- imagerExtra::ThresholdML(imager::cimg(array(as.vector(Target_Image), dim = c(1, length(as.vector(Target_Image)), 1, 1))), 
                                                         k = (Levels-1),
                                                         returnvalue = TRUE)
          }
          Target_Image <- Pixel_Multilevel_thresholder(Target = Target_Image,
                                                       Tissue_mask = Tissue_mask,
                                                       Threshold_values = Threshold_levels,
                                                       Blurr = Blurr_target,
                                                       Sigma = Sigma_target)
          #save images and tissue masks if required by user
          if(Save_processed_images){
            #Mask
            EBImage::writeImage(Tissue_mask, paste0(Output_Directory, "/","Processed_", Image_names_short[Image_index], "_Tissue_mask", ".tiff"))
            #Result (divided by the number of breaks to get a graylevel image)
            EBImage::writeImage(Target_Image$Image/length(Threshold_levels), paste0(Output_Directory, "/", "Processed_", 
                                                                                    str_replace_all(Image_names_short[Result_Image_index], pattern = "_", replacement = "."),  
                                                                                    "_", 
                                                                                    str_replace_all(Target_channel, pattern = "_", replacement = "."), 
                                                                                    "_MultiThresholded", ".tiff"))
          }
          
          return(Target_Image)
        }
      }, .progress = TRUE)
    )
    
    #Return to single core
    future::plan("future::sequential")
    gc()
  }
  
  if(!Local_thresholding){
    print("Performing Global thresholding")
    
    #We will need to calculate and store temporarily the tissue masks, these will be used to calculate thresholds
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    print("Generating tissue masks")
    
    Tissue_mask_list <- suppressMessages(
      furrr::future_map(seq_along(1:length(Image_names)), function(Image_index){
        Image <- magick::image_read(Image_names[Image_index])[Channels_to_keep_index]
        Image <- magick::as_EBImage(Image)
        
        #First generate the tissue mask and then remove the original image (no longer required)
        Tissue_mask <- Tissue_mask_generator(Image = Image,
                                             Threshold_type = Threshold_type_tissueMask,
                                             Threshold_value = Threshold_value_tissueMask,
                                             Blurr = Blurr_tissueMask,
                                             Sigma = Sigma_tissueMask)
        rm(Image)
        gc()
        return(Tissue_mask)
      }, .progress = TRUE)
    )
    gc()
    
    
    #If threshold type is otsu or arbitrary, then proceed accordingly
    if(Threshold_type != "Multilevel"){
      
      #Proceed with otsu
      if(Threshold_type == "Otsu"){
        print("Calculating global Otsu threshold")
        #Generate a list that contains the image and the vectorized version of the image where tissue masks have been applied
        Composite_Image_list <- suppressMessages(
          furrr::future_map2(.x = 1:length(Image_names), .y = Tissue_mask_list, function(.x, .y){
            #Import target image
            Image <- magick::image_read(Image_names[.x])[Channels_to_keep_index]
            Target_Image <- Image[which(Target_channel == Channels_to_keep)]
            Target_Image <- magick::as_EBImage(Target_Image)
            
            
            #Turn target image values outside tissue mask to 0
            Target_Image[!.y] <- 0
            
            #return as vector
            return(list(Image = Target_Image,
                        Vector = as.vector(Target_Image)))
          }, .progress = TRUE)
        )
        gc()
        
        #Generare a unified vector using all images
        Common_vector <- unlist(map(Composite_Image_list, function(Image) Image[["Vector"]]))
        #Apply otsu algorithm
        Threshold_global_otsu <- EBImage::otsu(array(Common_vector, dim = c(1, length(Common_vector))), range = c(min(Common_vector), max(Common_vector)), levels = length(unique(Common_vector)))
        
        #Obtain results (arbitrary with global Otsu threshold)
        print("Thresholding images")
        RESULTS <- suppressMessages(
          furrr::future_map2(.x = Composite_Image_list, .y = Tissue_mask_list, function(.x, .y){
            Pixel_thresholder(Target = .x[["Image"]],
                              Tissue_mask = .y,
                              Threshold_type = "Arbitrary",
                              Threshold_value = Threshold_global_otsu,
                              Blurr = Blurr_target,
                              Sigma = Sigma_target)
          }, .progress = TRUE)
        )
        gc()
        #If images need to be stored
        if(Save_processed_images){
          print("Writing tissue mask images")
          #Tissue mask
          suppressMessages(
            furrr::future_map(seq_along(1:length(Tissue_mask_list)), function(Tissue_mask_index){
              EBImage::writeImage(Tissue_mask_list[[Tissue_mask_index]],
                                  paste0(Output_Directory, "/", "Processed_", Image_names_short[Tissue_mask_index], "_Tissue_mask", ".tiff"))
            }, .progress = TRUE)
          )
          gc()
          #Target image
          print("Writing target images")
          suppressMessages(
            furrr::future_map(seq_along(1:length(RESULTS)), function(Result_Image_index){
              EBImage::writeImage(RESULTS[[Result_Image_index]][["Image"]], 
                                  paste0(Output_Directory, "/", "Processed_", 
                                         str_replace_all(Image_names_short[Result_Image_index], pattern = "_", replacement = "."),  
                                         "_", 
                                         str_replace_all(Target_channel, pattern = "_", replacement = "."), "_BinaryThresholded", ".tiff"))
            }, .progress = TRUE)
          )
          gc()
        }
      }
      
      #Proceed with user defined threshold
      if(Threshold_type == "Arbitrary"){
        #Get user defined threshold
        Threshold_arbitrary <- Threshold_value
        #Calculate the results by iterating along every image
        print("Thresholding images")
        RESULTS <- suppressMessages(
          furrr::future_map2(.x = seq_along(1:length(Image_names)), .y = Tissue_mask_list, function(.x, .y){
            Target_Image <- magick::image_read(Image_names[.x])[Channels_to_keep_index]
            Target_Image <- Target_Image[which(Target_channel == Channels_to_keep)]
            Target_Image <- magick::as_EBImage(Target_Image)
            Pixel_thresholder(Target = Target_Image,
                              Tissue_mask = .y,
                              Threshold_type = "Arbitrary",
                              Threshold_value = Threshold_arbitrary,
                              Blurr = Blurr_target,
                              Sigma = Sigma_target)
          }, .progress = TRUE)
        )
        gc()
        #If save images is required then write them in the output directory
        if(Save_processed_images){
          print("Writing tissue mask images")
          #Tissue mask
          suppressMessages(
            furrr::future_map(seq_along(1:length(Tissue_mask_list)), function(Tissue_mask_index){
              EBImage::writeImage(Tissue_mask_list[[Tissue_mask_index]],
                                  paste0(Output_Directory, "/", "Processed_", Image_names_short[Tissue_mask_index], "_Tissue_mask", ".tiff"))
            }, .progress = TRUE)
          )
          gc()
          print("Writing target images")
          #Target image
          suppressMessages(
            furrr::future_map(seq_along(1:length(RESULTS)), function(Result_Image_index){
              EBImage::writeImage(RESULTS[[Result_Image_index]][["Image"]], 
                                  paste0(Output_Directory, "/", "Processed_", 
                                         str_replace_all(Image_names_short[Result_Image_index], pattern = "_", replacement = "."), 
                                         "_", 
                                         str_replace_all(Target_channel, pattern = "_", replacement = "."), "_BinaryThresholded", ".tiff"))
            }, .progress = TRUE)
          )
          gc()
        }
      } 
    }
    #If multilevel is required
    if(Threshold_type == "Multilevel"){
      #Generate a list that contains the image and the vectorized version of the image where tissue masks have been applied
      Composite_Image_list <- suppressMessages(
        furrr::future_map2(.x = 1:length(Image_names), .y = Tissue_mask_list, function(.x, .y){
          #Import target image
          Image <- magick::image_read(Image_names[.x])[Channels_to_keep_index]
          Target_Image <- Image[which(Target_channel == Channels_to_keep)]
          Target_Image <- magick::as_EBImage(Target_Image)
          
          #Turn target image values outside tissue mask to 0
          Target_Image[!.y] <- 0
          
          #return as vector
          return(list(Image = Target_Image,
                      Vector = as.vector(Target_Image)))
        }, .progress = TRUE)
      )
      gc()
      #Generare a unified vector using all images
      Common_vector <- unlist(map(Composite_Image_list, function(Image) Image[["Vector"]]))
      #If user has provided thresholds use them, if not compute using Imagerextra
      if(!is.null(Threshold_value)){
        Threshold_global_multilevel <- Threshold_value
      } else{
        print("Calculating Multi level thresholds")
        Threshold_global_multilevel <- imagerExtra::ThresholdML(imager::cimg(array(Common_vector, dim = c(1, length(Common_vector), 1, 1))), 
                                                                k = (Levels-1),
                                                                returnvalue = TRUE)
      }
      #Obtain results (arbitrary with global Otsu threshold)
      print("Thresholding images")
      RESULTS <- suppressMessages(
        furrr::future_map2(.x = Composite_Image_list, .y = Tissue_mask_list, function(.x, .y){
          Pixel_Multilevel_thresholder(Target = .x[["Image"]],
                                       Tissue_mask = .y,
                                       Threshold_values = Threshold_global_multilevel,
                                       Blurr = Blurr_target,
                                       Sigma = Sigma_target)
        }, .progress = TRUE)
      )
      gc()
      #If images need to be stored
      if(Save_processed_images){
        print("Writing tissue mask images")
        #Tissue mask
        suppressMessages(
          furrr::future_map(seq_along(1:length(Tissue_mask_list)), function(Tissue_mask_index){
            EBImage::writeImage(Tissue_mask_list[[Tissue_mask_index]],
                                paste0(Output_Directory, "/", "Processed_", Image_names_short[Tissue_mask_index], "_Tissue_mask", ".tiff"))
          }, .progress = TRUE)
        )
        gc()
        print("Writing target images")
        #Target image divided by the number of breaks to get a graylevel image
        suppressMessages(
          furrr::future_map(seq_along(1:length(RESULTS)), function(Result_Image_index){
            EBImage::writeImage(RESULTS[[Result_Image_index]][["Image"]]/length(Threshold_global_multilevel), 
                                paste0(Output_Directory, "/", "Processed_", 
                                       str_replace_all(Image_names_short[Result_Image_index], pattern = "_", replacement = "."), 
                                       "_", 
                                       str_replace_all(Target_channel, pattern = "_", replacement = "."), 
                                       "_MultiThresholded", ".tiff"))
          }, .progress = TRUE)
        )
        gc()
      }
    }
    
    #Return to single core
    future::plan("future::sequential")
    gc()
  }
  
  print("Generating results summary")
  #Generate the final tibble
  Final_tibble <- tibble(Subject_Names = Image_names_short,
                         Total_foreground_pixels = map_dbl(RESULTS, ~.[["Total_foreground_pixels"]]))
  
  #Generate the the result thresholds
  if(Threshold_type == "Multilevel") Target_threshold_vector <- map_chr(RESULTS, ~.[["Threshold_value"]])
  if(Threshold_type != "Multilevel") Target_threshold_vector <- map_dbl(RESULTS, ~.[["Threshold_value"]])
  
  #Generate summary
  if(Threshold_type == "Multilevel"){
    #Get the results
    RESULTS <- map_dfr(RESULTS, function(Image) Image$Pixel_count)
    names(RESULTS) <- str_c("Value_", names(RESULTS), sep = "")
    #If na turn to 0
    RESULTS[is.na(RESULTS)] <- 0
    
    #Calculate the multiScore n_pixels*value / total foreground pixels
    Multi_score <- map2_dfc(.x = RESULTS[-1], .y = 1:ncol(RESULTS[-1]), function(.x, .y) .x*.y)
    Multi_score <- apply(Multi_score, MARGIN = 1, function(Row) sum(Row))/Final_tibble$Total_foreground_pixels
    
    #Generate the proportion
    RESULTS_PROP <- map_dfc(RESULTS, function(column) column/Final_tibble$Total_foreground_pixels)
    names(RESULTS_PROP) <- str_c("PER_", names(RESULTS_PROP), sep = "")
    RESULTS <- bind_cols(RESULTS, RESULTS_PROP)
    
    #Add the multiScore
    RESULTS$Multi_score <- Multi_score
  }
  if(Threshold_type != "Multilevel"){
    RESULTS <- tibble(Positive_pixels = map_dbl(RESULTS, ~.[["Pixel_count"]]))
    RESULTS$Prop_positive <- RESULTS$Positive_pixels/Final_tibble$Total_foreground_pixels
  }
  
  #Add the thresholds
  RESULTS$Target_threshold_value <- Target_threshold_vector
  
  #Return the final value
  return(dplyr::bind_cols(Final_tibble, RESULTS))
}, options = list(optimize = 3))

MFI_Experimet_Calculator <- cmpfun(
  function(N_cores = NULL,
           Directory = NULL,
           Ordered_Channels = NULL,
           Channels_to_keep =NULL,
           Target_channel = NULL,
           
           Target_masks = NULL, #This will be a list of lists features where each item will be a target mask 
           
           Threshold_type_tissueMask = NULL,
           Threshold_value_tissueMask = NULL,
           Blurr_tissueMask = NULL,
           Sigma_tissueMask = NULL
  ){
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Argument check general arguments
    Argument_checker <- c(N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0),
                          Empty_directory = length(dir(Directory)) >= 1,
                          Channels_OK = all(Channels_to_keep %in% Ordered_Channels),
                          Target_channel_OK = Target_channel %in% Channels_to_keep,
                          
                          Threshold_type_tissueMask_OK = Threshold_type_tissueMask %in% c("Otsu", "Arbitrary", "Absolute"),
                          Threshold_value_tissueMask_OK = if(Threshold_type_tissueMask == "Arbitrary"){
                            all(is.numeric(Threshold_value_tissueMask), Threshold_value_tissueMask >=0, Threshold_value_tissueMask <= 1)
                          } else(TRUE),
                          Blurr_tissueMask_OK = is.logical(Blurr_tissueMask),
                          Sigma_tissueMask_OK = if(Blurr_tissueMask){
                            all(is.numeric(Sigma_tissueMask), Sigma_tissueMask > 0)
                          } else(TRUE)
    )
    
    Stop_messages <- c(N_cores_OK = "N_cores must be an integer value > 0",
                       Empty_directory = "No files found at the directory provided. Please check out the path.",
                       Channels_OK = str_c(
                         "The following channels are not present the channel names provided: ",
                         str_c(Channels_to_keep[!(Channels_to_keep %in% Ordered_Channels)], collapse = ", "),
                         sep = ""),
                       Target_channel_OK = str_c(Target_channel, " not present in ", str_c(Channels_to_keep, collapse = ", "), collapse = ""),
                       
                       
                       Threshold_type_tissueMask_OK = "Threshold_type_TissueMask must be one of the following: Otsu, Arbitrary, Absolute",
                       Threshold_value_tissueMask_OK = "Threshold_value_tissueMask must be a single numeric value between 0 and 1",
                       Blurr_tissueMask_OK = "Blurr_tissueMask must be a logical value",
                       Sigma_tissueMask_OK = "Sigma_tissueMask must be a positive numeric value > 0"
    )
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    
    #Check specifically the Target masks
    #If no target mask required proceed appropriately
    if(is.null(Target_masks)) print("Calculating MFI using the tissue mask alone")
    #If complex target masks then check arguments
    else{
      #Check that target masks are a list
      if(!is.list(Target_masks)) stop("Target_masks must be a list containing mask parameters")
      
      #Check that target masks items are a list
      if(!all(
        map_lgl(Target_masks, function(Individual_mask){
          is.list(Individual_mask)
        }))) stop("Individual Items in the Target_mask must be a list")
      
      #Check names of masks (Mask_name, Threshold_type, Threshold_value, Blurr, Sigma)
      Adequate_names <- 
        map_lgl(Target_masks, function(Individual_mask){
          identical(names(Individual_mask), c("Mask_name", "Threshold_type", "Threshold_value", "Blurr", "Sigma"))
        })
      if(!all(Adequate_names)){
        stop(paste0("Names of Target_masks must be the folloing: Mask_name, Threshold_type, Threshold_value, Blurr, Sigma",
                    "The following masks have inadequate names: ",
                    str_c(which(!Adequate_names), collapse = ", ")))
      }
      #Check the actual arguments within the list of lists
      walk(Target_masks, function(Individual_mask){
        #Check mask name present in channels to keep
        if(!Individual_mask[["Mask_name"]] %in% Channels_to_keep) stop(paste0(Individual_mask[["Mask_name"]], ": Invalid mask name.",
                                                                              "It must be present in channels_to_keep: ",
                                                                              str_c(Channels_to_keep, collapse = ", ")))
        
        #Check Threshold type
        if(!Individual_mask[["Threshold_type"]] %in% c("Arbitrary", "Otsu")) stop(paste0(Individual_mask[["Mask_name"]], ": Invalid Threshold type.",
                                                                                         "It must be one of the following: Arbitrary, Otsu"))
        
        #Check Threshold value if Arbitrary
        if(Individual_mask[["Threshold_type"]] == "Arbitrary"){
          if(!all(is.numeric(Individual_mask[["Threshold_value"]]), Individual_mask[["Threshold_value"]] >=0, Individual_mask[["Threshold_value"]] <= 1)){
            stop(paste0(Individual_mask[["Mask_name"]], ": Invalid Threshold_value.",
                        "It must be a numeric value between 0 and 1"))
          }
        }
        
        #Check blur
        if(!is.logical(Individual_mask[["Blurr"]])) stop(paste0(Individual_mask[["Mask_name"]], ": Invalid Blurr argument.",
                                                                "It must be a logical value"))
        #Check Sigma value
        if(Individual_mask[["Blurr"]]){
          if(!all(is.numeric(Individual_mask[["Sigma"]]), Individual_mask[["Sigma"]] > 0)) stop(paste0(Individual_mask[["Mask_name"]], ": Invalid Sigma argument.",
                                                                                                       "Sigma must be a numeric value > 0"))
        }
      })
    }
    
    #Get the full image directory and the short image names
    Full_directory <- dir(Directory, full.names = TRUE)
    Image_names <- dir(Directory, full.names = FALSE)
    
    #Will iterate for every image to obtain tissue mask and MFI
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    RESULTS <- 
      suppressMessages(
        furrr::future_map(seq_along(1:length(dir(Directory))), function(Image){
          #Import the image and turn it into EBI image format
          Image <- magick::image_read(Full_directory[Image])
          Image <- Image[which(Channels_to_keep %in% Ordered_Channels)]
          Image <- Image %>% magick::as_EBImage()
          
          
          #First generate a tissue mask
          Tissue_mask <- Tissue_mask_generator(Image = Image,
                                               Threshold_type = Threshold_type_tissueMask,
                                               Threshold_value = Threshold_value_tissueMask,
                                               Blurr = Blurr_tissueMask,
                                               Sigma = Sigma_tissueMask)
          
          #Second generate potential target tissue masks and generate the final combined mask (Tissue + target)
          if(!is.null(Target_masks)){
            #Generate a list containing all the masks images
            Target_mask_list <- 
              map(Target_masks, function(Mask_parameters){
                return(
                  Pixel_thresholder(Target = EBImage::getFrame(Image, which(Channels_to_keep == Mask_parameters[["Mask_name"]])),
                                    Tissue_mask = Tissue_mask,
                                    Threshold_type = Mask_parameters[["Threshold_type"]],
                                    Threshold_value = Mask_parameters[["Threshold_value"]],
                                    Blurr = Mask_parameters[["Blurr"]],
                                    Sigma = Mask_parameters[["Sigma"]])[["Image"]]
                )
              })
            #Add tissue mask
            Target_mask_list$Tissue_mask <- Tissue_mask
            
            #Generate the final Tissue_mask including all masks generated
            Tissue_mask <- Multi_mask_generator(Target_mask_list)
            
            #Remove target mask list (large object) and run gc()
            rm(Target_mask_list)
            gc()
          }
          
          #Get the target channel and remove Image (large object) and run gc()
          Target_Image <- EBImage::getFrame(Image, which(Channels_to_keep == Target_channel))
          rm(Image)
          gc()
          
          #Finally calculate MFI in mask
          Result <- MFI_calculator(Target = Target_Image,
                                   Tissue_mask = Tissue_mask)
          return(Result)
        }, .progress = TRUE)
      )
    #Return to single core
    future::plan("future::sequential")
    gc()
    
    #Generate the name of the mask
    if(is.null(Target_masks)) Mask_name <- "Tissue"
    if(!is.null(Target_masks)){
      Mask_name <- c(map_chr(Target_masks, ~.[["Mask_name"]]), "Tissue")
      Mask_name <- str_c(Mask_name, collapse = "_")
    }
    
    #Generate the names of the MFI_score and mask name variables
    MFI_name <- str_c(c(Target_channel, "_MFI_in_", Mask_name), collapse = "")
    Mask_name <- str_c(Mask_name, "_Pixels", collapse = "")
    
    #Generate the final tibble
    RESULTS_tibble <- tibble(Subject_Names = Image_names)
    RESULTS_tibble$Value <- map_dbl(RESULTS, ~.[["MFI"]])
    RESULTS_tibble$Area <- map_dbl(RESULTS, ~.[["Total_foreground_pixels"]])
    #Change the names
    names(RESULTS_tibble)[c(2,3)] <- c(MFI_name, Mask_name)
    
    return(RESULTS_tibble)
    
  }, 
  options = list(optimize = 3))

Marker_segmentator <-
  cmpfun(
    function(DATA = NULL,
             DATA_variable = NULL,
             DATA_cutoff = NULL,
             New_labels = NULL,
             Merge = NULL,
             Var_to_Merge = NULL){
      
      #Check arguments
      if(!DATA_variable %in% names(DATA)) {
        stop(paste0(DATA_variable, " not present in the DATA provided."))
      }
      
      if(!is.numeric(DATA_cutoff)) stop("DATA_cutoff must be a numeric vector")
      if(!length(New_labels) == length(DATA_cutoff)-1) stop(paste0(length(DATA_cutoff)-1, " New_labels must be provided"))
      if(!is.logical(Merge)) stop("Merge must be a logical value")
      if(Merge){
        if(!Var_to_Merge %in% names(DATA)) stop(paste0(Var_to_Merge, " not present in DATA provided"))
      }
      
      #Generate the segmented version of distance variable
      Segmented_distance <- as.character(cut(DATA[[DATA_variable]], breaks = DATA_cutoff, labels = New_labels))
      
      #Add it to data or to the desired cell column
      if(!Merge) DATA[[DATA_variable]] <- Segmented_distance
      if(Merge) DATA[[Var_to_Merge]] <- str_c(DATA[[Var_to_Merge]], Segmented_distance, sep = "_")
      
      return(DATA)
    },
    options = list(optimize = 3)
  )


Data_arrange_function <- cmpfun(function(DATA, X, Y, Subject_Names, Markers_to_keep) { 
  
  #Check arguments by generating a argument check vector and message vector
  if(!all(c(X, Y, Subject_Names, Markers_to_keep) %in% names(DATA))) {
    Missing_arguments <- c(X, Y, Subject_Names, Markers_to_keep)[!c(X, Y, Subject_Names, Markers_to_keep) %in% names(DATA)]
    stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA"))
  }
  
  else{
    #Import X Y ID data
    DATA_interim <- DATA[as.character(c(X, Y, Subject_Names))] 
    names(DATA_interim) <- c("X", "Y", "Subject_Names")
    
    #Bind coordinates and ID with markers
    DATA_interim <- bind_cols(DATA_interim, DATA[unique(Markers_to_keep)])
    names(DATA_interim) <- str_replace_all(names(DATA_interim), "-", "_")
    
    #Arrange data according to subject Names (required in order to work in the cell ID labeling)
    DATA_interim <- DATA_interim %>% dplyr::arrange(Subject_Names)
    
    #Assign cell Specific ID
    DATA_interim <- DATA_interim %>% mutate(Cell_no = str_c("CELL", as.character(unlist(map(
      map_dbl(unique(DATA_interim$Subject_Names), function(x) {
        nrow(DATA_interim %>% dplyr::filter(Subject_Names == x))
      }), function(x) {
        1:x
      }, .progress = list(clear = F,
                          name = "Adding Cell ID",
                          show_after = 1,
                          type = "iterator")))), sep ="_"))
    #Include the Subject_Name in the Cell_no
    DATA_interim$Cell_no <- str_c(DATA_interim$Cell_no, DATA_interim$Subject_Names, sep = "__")
    
    #Final DATA
    DATA_interim <- DATA_interim[c(ncol(DATA_interim), 1:(ncol(DATA_interim)-1))]
    names(DATA_interim) <- str_replace_all(names(DATA_interim), "-", "_")
    names(DATA_interim) <- str_replace_all(names(DATA_interim), " ", "_")
    return(DATA_interim)
  }
}, 
options = list(optimize = 3))

Data_QC_Check_function <- cmpfun(function(DATA) {
  if( (sum(is.na(DATA)) == 0) && (sum(map_lgl(DATA[-c(1:4)], function(x) length(unique(x)) >1)) == ncol(DATA)-4) ) {
    print("No missing values in data. No constant value markers. Proceed with analysis")
  }
  else if(sum(is.na(DATA)) != 0 && (sum(map_lgl(DATA[-c(1:4)], function(x) length(unique(x)) >1)) == ncol(DATA)-4)){
    paste0("Missing values in col: ", names(DATA)[map_dbl(DATA, function(x) sum(is.na(x)))>0])
  }
  else if(sum(is.na(DATA)) == 0 && sum(map_lgl(DATA[-c(1:4)], function(x) length(unique(x)) >1)) < ncol(DATA)-4) {
    paste0("constant values in col: ", names(DATA)[-c(1:4)][map_lgl(DATA[-c(1:4)], function(x) length(unique(x)) == 1)])
  }
  else {
    c(
      paste0("Missing values in col: ", names(DATA)[map_dbl(DATA, function(x) sum(is.na(x)))>0]),
      paste0("constant values in col: ", names(DATA)[-c(1:4)][map_lgl(DATA[-c(1:4)], function(x) length(unique(x)) == 1)])
    )
  }
},
options = list(optimize = 3))

Cell_in_edge_remover <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           Hull_ratio = NULL,
           Distance_to_edge = NULL,
           Image_preview = NULL
  ) {
    #Check arguments
    #Obtain the data
    DATA <- DATA
    
    if(!identical(names(DATA)[1:4],  c("Cell_no", "X", "Y", "Subject_Names"))) { #Check if Data is correctly formatted
      stop("DATA provided should have an adecuate format")
    }
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!all(is.numeric(Hull_ratio), Hull_ratio >= 0, Hull_ratio <= 1)) stop("Hull_ratio must be a numeric value between 0 and 1")
    if(!all(is.numeric(Distance_to_edge), Distance_to_edge > 0)) stop("Distance_to_edge must be a numeric value > 0")
    if(!any(is.null(Image_preview), Image_preview %in% unique(DATA$Subject_Names))) stop(paste0(Image_preview, " not found in Subject_Names"))
    
    
    #Perform a random test to allow user to stop the computation if edge correction parameter is not desired
    if(is.null(Image_preview)){
      print("Running edge correction example on a random sample")
      Image_preview <- sample(unique(DATA$Subject_Names), size = 1)
    } 
    Sample <- DATA %>% dplyr::filter(Subject_Names == Image_preview)
    Cells_sf <- sf::st_as_sf(Sample , coords = c("X", "Y"))
    Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
    Cells_in_Border_vector <- unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = F, dist = Distance_to_edge))
    
    plot(Sample %>% 
           dplyr::mutate(Removed = Cells_in_Border_vector) %>%
           ggplot(aes(x = X, y = Y, color = Cells_in_Border_vector)) +
           geom_point() +
           scale_color_manual("", labels = c("Included", "Removed"), values = c("black", "grey")) +
           theme_minimal() + 
           scale_x_continuous("") + 
           scale_y_continuous("") +
           theme(panel.grid = element_blank(),
                 axis.text = element_blank(),
                 legend.position = "bottom",
                 legend.text = element_text(size = 12)))
    
    #Ask the user if the algorihtm should proceed
    answer <- menu(c("Proceed", "Abort"), title = "Should the analysis proceed")
    #If user decides to stop then abort function and return stop message
    if(answer == 2) stop("The function has been stopped. Please tune edge correction parameters for a better result")
    
    
    #Remove cells in border
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Now we calculate our distance matrix
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <- 
      furrr::future_map_dfr(unique(DATA$Subject_Names), function(x){
        #Prepare our data
        Image_tibble <- DATA %>% dplyr::filter(Subject_Names == x)
        Cells_sf <- sf::st_as_sf(Image_tibble , coords = c("X", "Y"))
        Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
        Cells_in_Border_vector <- unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = F, dist = Distance_to_edge))
        
        
        
        #Print message to warn COO removed in analysis
        message(paste0("Sample ", as.character(x), ": ", sum(Cells_in_Border_vector), " / ", nrow(Image_tibble), " cell/s will be removed due to edge proximity.")) 
        
        #Return the Tibble with the cells that are not in the border
        return(Image_tibble[!Cells_in_Border_vector,])
      }, .progress = TRUE)
    future::plan("future::sequential")
    gc()
    
    return(RESULTS)
    
    
  }, 
  options = list(optimize = 3))

Data_set_aside <- cmpfun(
  function(DATA = NULL,
           Markers_to_set = NULL) {
    
    if(!identical(c("Cell_no", "X", "Y", "Subject_Names"), names(DATA)[c(1:4)])) {
      stop("Your data does not contain adequate format (Cell_no, X, Y, Subject_Names). Please format using the Data_arrange_function.")
    }
    
    if(!all(Markers_to_set %in% names(DATA))) {
      Missing_arguments <- c(Markers_to_set)[!Markers_to_set %in% names(DATA)]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA"))
    }
    
    else{list(Aside = DATA %>% dplyr::select(1:4, all_of(Markers_to_set)),
              DATA = DATA %>% dplyr::select(-any_of(Markers_to_set))
    )}
  }, 
  options = list(optimize = 3))

Clinical_Data_arrange_function <- cmpfun(
  function(DATA, Subject_Names, Outcomes_to_keep) { 
    #Check arguments
    if(!all(c(Subject_Names, Outcomes_to_keep) %in% names(DATA))) {
      Missing_arguments <- c(Subject_Names, Outcomes_to_keep)[!c(Subject_Names, Outcomes_to_keep) %in% names(DATA)]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA"))
    }
    
    else{
      #Import X Y ID data
      DATA_interim <- DATA[as.character(Subject_Names)] 
      names(DATA_interim) <- "Subject_Names"
      
      #Bind coordinates and ID with markers
      DATA_interim <- bind_cols(DATA_interim, DATA[unique(Outcomes_to_keep)])
      names(DATA_interim) <- str_replace_all(names(DATA_interim), "-", "_")
      names(DATA_interim) <- str_replace_all(names(DATA_interim), " ", "_")
      return(DATA_interim)
    }
  }, 
  options = list(optimize = 3)
)

Clinical_Data_analyzer <- cmpfun(
  function(DATA = NULL,
           DATA_var = NULL,
           DATA_Clinical = NULL,
           Clinical_var = NULL,
           Perform_time_to_event = NULL,
           Time_variable = NULL,
           Event_variable = NULL) {
    
    #Check if Subject names is in both Data sources
    if(!all("Subject_Names" %in% names(DATA), "Subject_Names" %in% names(DATA_CLINICAL))) {
      stop("Subject_Names variable should be present in DATA and DATA_Clinical")
    }
    #Check if at least one Subject Name is present in both Data sources
    if(!any(DATA_Clinical$Subject_Names %in% DATA$Subject_Names)) {
      stop("No match between samples from DATA and DATA_Clinical. Check that at least a single subject is present in both data sources")
    }
    #Check DATA_variables included in analysis
    if(!all(DATA_var %in% names(DATA))) {
      Missing_arguments <- DATA_var[!DATA_var %in% names(DATA)]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA"))
    }
    if(!is.logical(Perform_time_to_event)){
      stop("Perform_time_to_event should be a logical value")
    }
    
    #Proceed with regular analysis (NO TIME TO EVENT)
    else {
      if(!Perform_time_to_event) {
        #Check Clinical Variables included in Data Clinical
        if(!all(Clinical_var %in% names(DATA_Clinical))) {
          Missing_arguments <- Clinical_var[!Clinical_var %in% names(DATA_Clinical)]
          stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA_Clinical"))
        }
        else {
          #Import Data to be correlated with clinical findings
          DATA <- DATA
          DATA <- DATA %>% dplyr::select(Subject_Names, all_of(DATA_var))
          
          #Import and arrange clinical Data
          DATA_Clinical <- DATA_Clinical
          DATA_Clinical <- DATA_Clinical %>% dplyr::select(Subject_Names, all_of(Clinical_var))
          names(DATA_Clinical)[2] <- "Clin_var"
          
          #Join the clinical and the actual data
          JOINED <- left_join(DATA_Clinical, DATA, by = "Subject_Names")
          
          #If clinical data is a character or a factor
          if(any(is.character(JOINED$Clin_var), is.factor(JOINED$Clin_var))) {
            #Rearrange Subject_Names according to clinical variable
            JOINED <- JOINED %>% arrange(Clin_var)
            JOINED <- JOINED %>% mutate(Subject_Names = factor(Subject_Names, levels = JOINED$Subject_Names))
            
            #Plot samples by clinical variable
            plot(JOINED %>% pivot_longer(-c(1:2)) %>%
                   ggplot(aes(x = Subject_Names, fill = Clin_var, y = value)) +
                   facet_wrap(~name, "free_y", ncol = 1, nrow = ncol(JOINED)-2) + geom_col(width = 0.5, color = "black") +
                   cowplot::theme_cowplot() +
                   scale_x_discrete("") +
                   scale_y_continuous("value")+
                   scale_fill_viridis_d(Clinical_var) +
                   theme(axis.text.x = element_text(angle = -90, hjust = 0.5))
            )
            
            #Plot summary
            plot(JOINED %>% pivot_longer(-c(1:2)) %>%
                   ggplot(aes(x = Clin_var, fill = Clin_var, y = value)) +
                   facet_wrap(~name, "free_y", ncol = 1, nrow = ncol(JOINED)-2) + geom_bar(width = 0.5, color = "black", stat = "summary", fun = "mean") +
                   cowplot::theme_cowplot() +
                   scale_x_discrete(Clinical_var) +
                   scale_y_continuous("Mean value") +
                   scale_fill_viridis_d(Clinical_var) +
                   ggpubr::stat_compare_means(label.x.npc = "center", label.y.npc = 'bottom')
            )
            
            #Prepare the summary for everyone of the DATA vars
            RESULTS <-  map(3:ncol(JOINED), function(Variable){
              JOINED <- bind_cols(JOINED[1:2], JOINED[Variable])
              #Calculate mean
              MEAN_tibble <- JOINED %>% group_by(Clin_var) %>% summarize_at(vars(-group_cols(), -Subject_Names), .funs = "mean", na.rm = TRUE) %>% ungroup()
              names(MEAN_tibble)[2] <- str_c("Average_", names(MEAN_tibble)[2])
              #Calculate sd
              SD_tibble <- JOINED %>% group_by(Clin_var) %>% summarize_at(vars(-group_cols(), -Subject_Names), .funs = "sd", na.rm = TRUE) %>% ungroup()
              names(SD_tibble)[2] <- str_c("SD_", names(SD_tibble)[2])
              #calculate p25
              p25_tibble <- JOINED %>% group_by(Clin_var) %>% summarize_at(vars(-group_cols(), -Subject_Names), .funs = function(x) quantile(x, 0.25, na.rm = TRUE)) %>% ungroup()
              names(p25_tibble)[2] <- str_c("p25_", names(p25_tibble)[2])
              #Calculate p50
              Median_tibble <- JOINED %>% group_by(Clin_var) %>% summarize_at(vars(-group_cols(), -Subject_Names), .funs = function(x) quantile(x, 0.5, na.rm = TRUE)) %>% ungroup()
              names(Median_tibble)[2] <- str_c("Median_", names(Median_tibble)[2])
              #Calculate p75
              p75_tibble <- JOINED %>% group_by(Clin_var) %>% summarize_at(vars(-group_cols(), -Subject_Names), .funs = function(x) quantile(x, 0.75, na.rm = TRUE)) %>% ungroup()
              names(p75_tibble)[2] <- str_c("p75_", names(p75_tibble)[2])
              #Bind the resulting tibbles
              RESULTS_tibble <- bind_cols(MEAN_tibble, SD_tibble[-1], p25_tibble[-1], Median_tibble[-1], p75_tibble[-1])
              
              #Generate the t.test or the ANOVA test according to the number of levels of the clinical variable
              if(length(unique(JOINED$Clin_var)) == 2) {
                if(berryFunctions::is.error(stats::t.test(JOINED[[3]]~JOINED$Clin_var))) {t_test_results <- NA}
                else{t_test_results <- stats::t.test(JOINED[[3]]~JOINED$Clin_var)$p.value}
                return(list(Summary = RESULTS_tibble,
                            t_test_result = t_test_results))
              }
              else {
                if(berryFunctions::is.error(stats::oneway.test(JOINED[[3]]~JOINED$Clin_var))) {ANOVA_test_results <- NA}
                else{ANOVA_test_results <- stats::oneway.test(JOINED[[3]]~JOINED$Clin_var)$p.value}
                return(list(Summary = RESULTS_tibble,
                            ANOVA_result = ANOVA_test_results))
              }
              
            })
            
            #Change the name of the RESULTS list
            names(RESULTS) <- names(JOINED)[3:ncol(JOINED)]
            return(RESULTS)
            
          }
          
          #If its numeric then execute the following code
          if(is.numeric(JOINED$Clin_var)) {
            #Generate the pearson correlation coefficient and the associated pvalue
            RESULTS <- 
              map_dfr(JOINED[-(1:2)], function(Variable) {
                Correlation_Result <- stats::cor.test(Variable, JOINED$Clin_var, method = "pearson", alternative = "two.sided")
                c(Pearson_cor = Correlation_Result$estimate,
                  Pearson_pval = Correlation_Result$p.value)
              })
            #Add the names of the pearson
            RESULTS$name <- names(JOINED)[-(1:2)]
            
            #Change the names and the order of the variables
            names(RESULTS) <- c("Pearson_cor", "Pearson_pval", "name")
            RESULTS <- RESULTS[c(3, 1, 2)]
            
            #Plot the individual variables and outcomes
            plot(
              JOINED %>% pivot_longer(-c(1:2)) %>%
                ggplot(aes(x = Clin_var, y = value)) +
                facet_wrap(~name, "free_y", ncol = 1, nrow = ncol(JOINED)-2) +
                geom_point(size = 1.2) + geom_smooth(method = "lm", color = "red", se = F) +
                cowplot::theme_cowplot() +
                scale_x_continuous(Clinical_var) +
                scale_y_continuous("Variable")
            )
            
            #Plot the summary
            plot(
              RESULTS %>% ggplot(aes(x = fct_reorder(name, Pearson_cor), y = Pearson_cor, fill = Pearson_pval)) +
                geom_col(width = 0.5, color = "black") +
                cowplot::theme_cowplot() +
                scale_x_discrete("") +
                scale_y_continuous("Pearson Correlation") +
                geom_hline(yintercept = 0, color ="black", linewidth = 0.9) +
                scale_fill_gradient2(high = "white", low = "red", mid = "white", midpoint = 0.05, limits = c(0, 1))+
                theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
            )
            
            #Return the results
            return(RESULTS)
          }
        }
      }
      
      #Proceed with time to event analysis if required
      else if(Perform_time_to_event) {
        #Check time to event and event variables
        if(!all(c(Time_variable, Event_variable) %in% names(DATA_Clinical))) {
          Missing_arguments <- c(Time_variable, Event_variable)[!c(Time_variable, Event_variable) %in% names(DATA_Clinical)]
          stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA_Clinical"))
        }
        
        #Import Data to be correlated with clinical findings
        DATA <- DATA
        DATA <- DATA %>% dplyr::select(Subject_Names, all_of(DATA_var))
        
        #Import and arrange clinical Data
        DATA_Clinical <- DATA_Clinical
        DATA_Clinical <- DATA_Clinical %>% dplyr::select(Subject_Names, all_of(c(Time_variable, Event_variable)))
        names(DATA_Clinical)[2:3] <- c("Time_variable", "Event_variable")
        
        #Join the clinical and the actual data
        JOINED <- left_join(DATA_Clinical, DATA, by = "Subject_Names")
        
        res.cut <- survminer::surv_cutpoint(JOINED,
                                            time = "Time_variable", 
                                            event = "Event_variable",
                                            variables = names(JOINED)[-c(1:3)],
                                            minprop = 0.1,
                                            progressbar=TRUE
        )
        JOINED_Cat <- as_tibble(survminer::surv_categorize(res.cut))
        
        Survival_models <- 
          map(3:ncol(JOINED_Cat), function(Var) {
            Interim <- bind_cols(JOINED_Cat[1:2], JOINED_Cat[Var])
            names(Interim)[3] <- "Target"
            Model <- survival::survfit(survival::Surv(as.numeric(Time_variable), as.numeric(Event_variable)) ~ Target, data = Interim)
            Plot <- survminer::ggsurvplot(Model, data = Interim,
                                          pval = T, risk.table = F, conf.int = FALSE, 
                                          palette = "npg",
                                          title = names(JOINED_Cat)[Var],
                                          xlab = Time_variable,
                                          ylab = Event_variable)$plot
            CoxPHModel <- survival::coxph(survival::Surv(as.numeric(Time_variable), as.numeric(Event_variable)) ~ Target, data = Interim)
            
            return(list(Model = CoxPHModel,
                        Plot = Plot))
          })
        #Plot the KM plots
        plot(cowplot::plot_grid(plotlist = map(Survival_models, ~.[["Plot"]]), ncol = ceiling( (ncol(JOINED_Cat)-3) / 2)))
        
        #Return a list with the model 
        Model_list <- map(Survival_models, ~summary(.[["Model"]]))
        names(Model_list) <- names(JOINED_Cat[3:ncol(JOINED_Cat)]) #Add names
        return(Model_list)
      }
    }
  },
  options = list(optimize = 3))

Image_size_calculator <- cmpfun(
  function(DATA = NULL,
           Strategy = NULL,
           Image_to_plot = NULL,
           
           Tile_accuracy = NULL,
           
           Hull_ratio = NULL
           
  ) {
    DATA <- DATA
    #Check arguments
    if(!all(c("Subject_Names", "X", "Y") %in% names(DATA)[c(1:4)])) {
      stop("Your Data must contain the following variables: Subject_Names, X, Y. Please format using the Data_arrange_function.")
    }
    if(!Strategy %in% c("Tiling", "Concave_hull")) stop("Strategy must be one of the following: Tiling, Concave_hull")
    if(!any(Image_to_plot %in% unique(DATA$Subject_Names), is.null(Image_to_plot))) stop("Image_to_plot is not present in DATA")
    if(Strategy == "Tiling"){
      if(!all(is.numeric(Tile_accuracy), Tile_accuracy > 0)) stop("Tile_accuracy must be a positive numeric value")
    }
    if(Strategy == "Concave_hull"){
      if(!all(is.numeric(Hull_ratio), Hull_ratio >= 0, Hull_ratio <= 1)) stop("Hull_ratio must be a numeric value between 0 and 1")
    }
    
    #Tiling strategy
    if(Strategy == "Tiling"){
      Result <- tibble(Subject_Names = unique(DATA$Subject_Names),
                       Area = map_dbl(unique(DATA$Subject_Names), function(Image) {
                         Interim <- DATA %>% dplyr::filter(Subject_Names == Image) %>%
                           ggplot(aes(x = X, y = Y)) + geom_bin2d(binwidth = Tile_accuracy)
                         N_tiles <- nrow(as_tibble(layer_data(Interim))) #obtain the layer data
                         N_tiles * Tile_accuracy * Tile_accuracy #multiply the tile number by the tile area
                       }, .progress = list(clear = F,
                                           name = "Calculating tissue area",
                                           show_after = 2,
                                           type = "iterator"))
      )
      
      #If image to plot is NULL select the smallest image
      if(is.null(Image_to_plot)){
        Image_to_plot <- (Result %>% arrange(Area))[[1,1]]
      }
      
      DENSITY_plot <- DATA %>%  dplyr::filter(Subject_Names == Image_to_plot) %>% #plot the smallest sample
        ggplot(aes(x = X, y = Y)) + geom_bin2d(binwidth = Tile_accuracy) +
        cowplot::theme_cowplot() + guides(fill = "none") 
      
      CELL_plot <- DATA %>%  dplyr::filter(Subject_Names == Image_to_plot) %>%
        ggplot(aes(x = X, y = Y)) + geom_point(size = 2) +
        cowplot::theme_cowplot()
      plot(
        patchwork::wrap_plots(CELL_plot,  DENSITY_plot, nrow = 1)
      )
      return(Result)
    }
    
    #Concave_hull
    if(Strategy == "Concave_hull"){
      Result <- tibble(Subject_Names = unique(DATA$Subject_Names),
                       Area = map_dbl(unique(DATA$Subject_Names), function(Image){
                         Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
                         Cells_sf <- sf::st_as_sf(Interim, coords = c("X", "Y"))
                         Sample_polygon <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "POLYGON")
                         Area <- sf::st_area(Sample_polygon)
                         return(Area)
                       }, .progress = list(clear = F,
                                           name = "Calculating tissue area",
                                           show_after = 2,
                                           type = "iterator")))
      
      #If image to plot is NULL select the smallest image
      if(is.null(Image_to_plot)){
        Image_to_plot <- (Result %>% arrange(Area))[[1,1]]
      }
      
      #Generate the sample to plot and the plots
      Sample_to_plot <- DATA %>%  dplyr::filter(Subject_Names == Image_to_plot)
      Sample_sf <- sf::st_as_sf(Sample_to_plot, coords = c("X", "Y"))
      Sample_polygon <- sf::st_cast((Sample_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "POLYGON")
      LAYOUT_plot <-  Sample_polygon %>% ggplot() + geom_sf(linewidth = 1.5, fill = "white", color = "black") +
        cowplot::theme_cowplot() + guides(fill = "none") +
        scale_x_continuous("") + 
        scale_y_continuous("") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank())
      CELL_plot <- DATA %>%  dplyr::filter(Subject_Names == Image_to_plot) %>%
        ggplot(aes(x = X, y = Y)) + geom_point(size = 2) +
        cowplot::theme_cowplot()
      plot(
        patchwork::wrap_plots(CELL_plot,  LAYOUT_plot, nrow = 1)
      )
      #Return the final result
      return(Result)
    }
  },
  options = list(optimize = 3))

Plot_correlation_matrix <- cmpfun(
  function(DATA = NULL,
           Variables_included = NULL,
           Correlation_method = NULL) {
    #Check Variables included in analysis
    if(!all(Variables_included %in% names(DATA))) {
      Missing_arguments <- Variables_included[!Variables_included %in% names(DATA)]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA"))
    }
    
    if(!all(c(length(Correlation_method) == 1, 
              Correlation_method %in% c("pearson", "spearman"))))
    {
      stop("Correlation_method must be one of pearson or spearman")
    }
    
    #Import data and select only desired variabels
    DATA <- DATA %>% dplyr::select(all_of(Variables_included))
    #Compute correlation matrix
    cor_DATA <- stats::cor(DATA, method = Correlation_method)
    #Compute correlation plot
    corrplot::corrplot(cor_DATA, method = "shade", type = "lower", order = "hclust", addCoef.col = "black", number.cex = 0.8, tl.cex = 0.8,
                       tl.pos = "lt", tl.col = "black")
  },
  options = list(optimize = 3))

Cell_functional_assessment <- cmpfun(
  function(DATA = NULL,
           Target_Variable = NULL,
           Targets_Included = NULL,
           DATA_Aside = NULL,
           Threshold_functional_Markers = NULL,
           Levels = NULL){
    
    #Check variables 
    if(!all(Target_Variable %in% names(DATA))) {
      Missing_arguments <- Target_Variable[!Target_Variable %in% names(DATA)]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA"))
    }
    if(!is.logical(Threshold_functional_Markers)) {
      stop("Threshold_functional_Markers must be a logical value")
    }
    
    #Import phenotype DATA and generate unique ID
    DATA <- DATA %>% mutate(Unique_ID = str_c(Cell_no, Subject_Names, sep = "_"))
    DATA <- DATA %>% dplyr::select(1:4, Unique_ID, all_of(Target_Variable))
    
    #Check that target variable contains the desired targets before proceeding
    if(!any(Targets_Included %in% DATA[[6]])) {
      stop(paste0(str_c(Targets_Included, collapse = ", "), " not found in ", Target_Variable))
    }
    
    #Filter desired cells to be further analyzed
    DATA <- DATA[DATA[[6]] %in% Targets_Included, ]
    
    #Now select the Data aside and join the results to the original data
    DATA_Aside <- DATA_Aside %>% mutate(Unique_ID = str_c(Cell_no, Subject_Names, sep = "_"))
    DATA_Aside <- DATA_Aside %>% dplyr::select(-c(1:4))
    DATA_Aside <- DATA_Aside[c(ncol(DATA_Aside), 1:(ncol(DATA_Aside)-1))]
    
    if(!Threshold_functional_Markers){
      #Join both tibbles
      DATA_Joined <- left_join(DATA, DATA_Aside, by = "Unique_ID") %>% dplyr::select(-Unique_ID)
      #Change name to allow function generalizaion
      names(DATA_Joined)[5] <- "Variable"
      
      #Plot the functional marker by each Target variable
      plot(
        DATA_Joined %>% pivot_longer(-c(1:5)) %>% 
          ggplot(aes(x = Variable, y = log10(value+0.00001), fill = Variable)) + facet_wrap(~name, "free", nrow = 1, ncol = (ncol(DATA_Joined) - 5)) +
          geom_boxplot() +
          cowplot::theme_cowplot() +
          guides(fill = "none")+
          scale_x_discrete("") +
          scale_y_continuous("Log 10 functional Marker expression across target cells")
      )
      
      #Generate a summary tibble with the functional markers across the Target variables
      By_sample_Results <- 
        DATA_Joined %>% dplyr::select(-Cell_no, -X, -Y) %>% group_by(Subject_Names, Variable) %>% summarize_at(vars(-group_cols()), list(min = ~min(.x, na.rm = T),
                                                                                                                                         p25 = ~quantile(.x, 0.25, na.rm = T),
                                                                                                                                         Average = ~mean(.x, na.rm = T), 
                                                                                                                                         p50 = ~quantile(.x, 0.5, na.rm = T),
                                                                                                                                         p75 = ~quantile(.x, 0.75, na.rm = T),
                                                                                                                                         max = ~max(.x, na.rm = T))) %>%
        ungroup() %>% pivot_longer(-c(1:2)) %>% mutate(Final_Variable = str_c(Variable, name, sep = "_")) %>%
        dplyr::select(-Variable, -name) %>%
        pivot_wider(names_from = Final_Variable, values_from = value)
      
      #Return the target Variable name to its original value
      names(DATA_Joined)[5] <- Target_Variable
      
      return(list(DATA_functional_markers = DATA_Joined,
                  By_sample_results = By_sample_Results))
    }
    
    else if(Threshold_functional_Markers){
      if(Levels %%1 != 0){
        stop("Levels must be and integer value")
      }
      print("Thresholding Markers")
      DATA_Aside <- bind_cols(DATA_Aside[1], 
                              map_df(DATA_Aside[-1],
                                     function(z){
                                       if(length(unique(z))>Levels){ #requires at least n Levels to be calculated
                                         as.double(imagerExtra::ThresholdML(imager::cimg(array(z, dim = c(1, length(z), 1, 1))), k = (Levels-1))) #K to specify the amount of cut-off points
                                       }else(NA)
                                     })
      )
      #Join both tibbles
      DATA_Joined <- left_join(DATA, DATA_Aside, by = "Unique_ID") %>% dplyr::select(-Unique_ID)
      #Change name to allow function generalizaion
      names(DATA_Joined)[5] <- "Variable"
      
      #Plot the results
      plot(
        DATA_Joined %>% pivot_longer(-c(1:5)) %>%
          ggplot(aes(x = Variable, y = 1)) + facet_wrap(~name, "free", nrow = 1, ncol = (ncol(DATA_Joined) - 5)) +
          geom_col(aes(fill = as.factor(value)), position = position_fill(reverse = T),  linewidth = 1) +
          cowplot::theme_cowplot() +
          scale_x_discrete("") +
          scale_y_continuous("%") +
          scale_fill_discrete("Level")
      )
      
      #Prepare by sample results
      
      #First generate a tibble with the Subject_Names the Target variable and the functional Markers
      For_Counts <- DATA_Joined %>% dplyr::select(-Cell_no, -X, -Y)
      
      #Prepare the overall target cell counts to calculate percentages
      Cell_count_tibble <- For_Counts %>% group_by(Subject_Names) %>% dplyr::count(Variable) %>% ungroup() %>%
        pivot_wider(names_from = Variable, values_from = n)
      Cell_count_tibble[is.na(Cell_count_tibble)] <- 0
      #Make sure cell count tibble is arranged by subject_names
      Cell_count_tibble <- Cell_count_tibble %>% arrange(Subject_Names)
      
      
      #Now count the functional marker for each subject name and target cell
      By_sample_Results <-
        #Iterate by functional marker
        map_dfc(3:ncol(For_Counts), function(var){
          #Generate a simple tibble with 3 columns, Subject_Names, Target cell and functional marker
          Interim <- bind_cols(For_Counts[1:2], For_Counts[var])
          
          #Change the name of the functional marker to make it generizable
          names(Interim)[[3]] <- "Functional_Marker"
          #Prepare the cell count data
          Interim <- Interim %>% group_by(Subject_Names, Variable) %>% dplyr::count(Functional_Marker) %>% ungroup() %>%
            mutate(Final_Variable = str_c(Variable, names(For_Counts)[var], Functional_Marker, sep = "_")) %>%
            dplyr::select(-Variable,-Functional_Marker) %>%
            pivot_wider(names_from = Final_Variable, values_from = n)
          Interim[is.na(Interim)] <- 0
          
          #Make sure both tibbles are ordered in the same way with respect to the Subject Names column
          Interim <- Interim %>% arrange(Subject_Names)
          
          #Generate the proportion tibble with the cell counts
          Prop_tibble <- map_dfc(names(Cell_count_tibble)[-1], function(Target) {
            Absolute_cell_counts <- Interim %>% dplyr::select(contains(Target))
            For_percentage <- Cell_count_tibble %>% dplyr::select(contains(Target))
            
            Prop_tibble <- as_tibble(Absolute_cell_counts/For_percentage[[1]])
            names(Prop_tibble) <- str_c("PROP_", names(Prop_tibble), sep ="")
            Prop_tibble
          })
          
          bind_cols(Interim[-1], Prop_tibble)
        })
      
      #Generate the final tibble with an adequate order
      By_sample_Results <- bind_cols(
        Cell_count_tibble[1],
        map_dfc(names(Cell_count_tibble)[-1], function(Target){
          bind_cols(Cell_count_tibble %>% dplyr::select(contains(Target)),
                    By_sample_Results %>% dplyr::select(contains(Target)))
        })
        
      )
      
      #Return the target Variable name to its original value
      names(DATA_Joined)[5] <- Target_Variable
      
      return(list(DATA_functional_markers = DATA_Joined,
                  By_sample_results = By_sample_Results))
      
    }
  },
  options = list(optimize = 3))

############STEP 1 - NORMALIZATION - REQUIRED FUNCTIONS###########
message("Importing functions: STEP 1 - NORMALIZATION")
Normalization_function <- cmpfun(
  function(DATA = NULL, 
           Strategy = NULL, 
           Parameters = NULL) {
  #Check strategy
  if(!Strategy %in% c("mxnorm", "simpleSeg")){
    stop("Strategy must be one of the following: mxnorm, simpleSeg")
  }
  
  #MXNORM CODE
  if(Strategy == "mxnorm") {
    #check that argument list is correct and contains adequate data
    if(!all(c("slide_id", "image_id", "marker_cols", "transform", "method", "method_override", "method_override_name") %in% names(Parameters))){
      Missing_arguments <- c("slide_id", "image_id", "marker_cols", "transform", "method", "method_override", "method_override_name")[
        !c("slide_id", "image_id", "marker_cols", "transform", "method", "method_override", "method_override_name") %in% names(Parameters)
      ]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not present in Paramters list"))
    }
    #check if arguments are well specified
    Argument_checker <- c(data_OK = identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names")),
                          image_id_OK = all(is.character(Parameters[["image_id"]]), Parameters[["image_id"]] %in% names(DATA)),
                          slide_id_OK = any(identical(Parameters[["slide_id"]], Parameters[["image_id"]]),
                                            all(is.vector(Parameters[["slide_id"]], mode = "character"),
                                                length(Parameters[["slide_id"]]) == nrow(DATA))
                          ),
                          marker_cols_OK = all(Parameters[["marker_cols"]] %in% names(DATA)),
                          transform_OK = Parameters[["transform"]] %in% c("None", "log10", "mean_divide","log10_mean_divide"),
                          method_OK = Parameters[["method"]] %in% c("None", "ComBat","Registration")
    )
    Stop_messages <- c(data_OK = "DATA must be adequately arrange (using Data_arrange_function from step 0)",
                       image_id_OK = str_c(Parameters[["image_id"]], " must be present in DATA"),
                       slide_id_OK = "If slide_id is not identical to image_id, it must be a vector indicating the slide origin of every cell",
                       marker_cols_OK = "All marker_cols must be present in the DATA provided",
                       transform_OK = "Transform method must be one of the following: None, log10, mean_divide, log10_mean_divide",
                       method_OK = "Method must be one of the following: None, ComBat, Registration")
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    
    #Now we generate the argument list again with the ones provided
    if(identical(Parameters[["slide_id"]], Parameters[["image_id"]])){
      Parameters <- list(
        mx_data = mxnorm::mx_dataset(data = DATA,
                                     slide_id = Parameters[["slide_id"]],
                                     image_id = Parameters[["image_id"]],
                                     marker_cols = Parameters[["marker_cols"]]
        ),
        transform = Parameters[["transform"]], 
        method = Parameters[["method"]], 
        method_override = Parameters[["method_override"]], 
        method_override_name = Parameters[["method_override_name"]] 
      )
    } 
    #If slide_id is not the same as image_id generate another parameter list
    if(!identical(Parameters[["slide_id"]], Parameters[["image_id"]])) {
      DATA <- bind_cols(DATA,
                        tibble(slide_id = Parameters[["slide_id"]]))
      Parameters <- list(
        mx_data = mxnorm::mx_dataset(data = DATA,
                                     slide_id = "slide_id",
                                     image_id = Parameters[["image_id"]],
                                     marker_cols = Parameters[["marker_cols"]]
        ),
        transform = Parameters[["transform"]], 
        method = Parameters[["method"]], 
        method_override = Parameters[["method_override"]], 
        method_override_name = Parameters[["method_override_name"]] 
      )
    } 
    
    #Execute normalization
    DATA_normalized <- base::do.call(mxnorm::mx_normalize, args = Parameters)
    DATA_normalized <- as_tibble(DATA_normalized$norm_data)[-c(1:2)]
    #Append normalization results to DATA
    return(bind_cols(DATA[1:4], DATA_normalized))
  }
  
  #simpleSeg CODE
  else if(Strategy == "simpleSeg") {
    #check that argument list is correct and contains adequate data
    if(!all(c("cells", "markers", "imageID", "transformation", "method", "cores") %in% names(Parameters))){
      Missing_arguments <- c("cells", "markers", "imageID", "transformation", "method", "cores")[
        !c("cells", "markers", "imageID", "transformation", "method", "cores") %in% names(Parameters)
      ]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not present in Paramters list"))
    }
    #check if arguments are well specified
    Argument_checker <- c(markers_OK = all(Parameters[["markers"]] %in% names(Parameters[["cells"]])),
                          imageID_OK = Parameters[["imageID"]] == "Subject_Names",
                          
                          transform_OK = if(!is.null(Parameters[["transformation"]])){
                            Parameters[["transformation"]] %in% c("asinh", "sqrt")
                          } else(TRUE),
                          method_OK = if(!is.null(Parameters[["method"]])){
                            all(Parameters[["method"]] %in% c('mean', 'minMax', 'trim99', 'PC1'))
                          } else(TRUE),
                          cores_OK = all(Parameters[["cores"]]%%1 == 0, Parameters[["cores"]] > 0)
    )
    Stop_messages <- c(markers_OK = "Marker names not present in the cell Data",
                       imageID_OK = "Image ID must be Subject_Names",
                       transform_OK = "Transformation method must be NULL or one of the following: asinh, sqrt",
                       method_OK = "Method must be NULL or one of the following: NULL, mean, minMax, trim99, PC1",
                       cores_OK = "Core number must be a positive integer value"
    )
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    
    #Execute the code
    base::do.call(simpleSeg::normalizeCells, args = Parameters)  
  }
    },
  options = list(optimize = 3))

Normalization_function_parallel <- cmpfun(
  function(DATA = NULL, 
           Strategy = NULL, 
           Parameters = NULL, 
           N_cores = NULL) {
    #Check strategy
    if(!Strategy %in% c("mxnorm", "simpleSeg")){
      stop("Strategy must be one of the following: mxnorm, simpleSeg")
    }
    if(!all(N_cores >= 1, N_cores%%1 == 0)) stop("N_cores must be a positive integer value")
    
    #First mxnorm
    if(Strategy == "mxnorm") {
      #check that argument list is correct and contains adequate data
      if(!all(c("slide_id", "image_id", "marker_cols", "transform", "method", "method_override", "method_override_name") %in% names(Parameters))){
        Missing_arguments <- c("slide_id", "image_id", "marker_cols", "transform", "method", "method_override", "method_override_name")[
          !c("slide_id", "image_id", "marker_cols", "transform", "method", "method_override", "method_override_name") %in% names(Parameters)
        ]
        stop(paste0(str_c(Missing_arguments, collapse = ", "), " not present in Paramters list"))
      }
      #check if arguments are well specified
      Argument_checker <- c(data_OK = identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names")),
                            image_id_OK = all(is.character(Parameters[["image_id"]]), Parameters[["image_id"]] %in% names(DATA)),
                            slide_id_OK = any(identical(Parameters[["slide_id"]], Parameters[["image_id"]]),
                                              all(is.vector(Parameters[["slide_id"]], mode = "character"),
                                                  length(Parameters[["slide_id"]]) == nrow(DATA))
                            ),
                            marker_cols_OK = all(Parameters[["marker_cols"]] %in% names(DATA)),
                            transform_OK = Parameters[["transform"]] %in% c("None", "log10", "mean_divide","log10_mean_divide"),
                            method_OK = Parameters[["method"]] %in% c("None", "ComBat","Registration")
      )
      Stop_messages <- c(data_OK = "DATA must be adequately arrange (using Data_arrange_function from step 0)",
                         image_id_OK = str_c(Parameters[["image_id"]], " must be present in DATA"),
                         slide_id_OK = "If slide_id is not identical to image_id, it must be a vector indicating the slide origin of every cell",
                         marker_cols_OK = "All marker_cols must be present in the DATA provided",
                         transform_OK = "Transform method must be one of the following: None, log10, mean_divide, log10_mean_divide",
                         method_OK = "Method must be one of the following: None, ComBat, Registration")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
      
      #Capture DATA and parameters
      DATA <- DATA
      Parameters <- Parameters
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      #Parallelize by marker_cols and execute
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      
      #Execute the function in a map way
      RESULTS <- furrr::future_map(Parameters[["marker_cols"]], function(Marker){
        
        #Now we generate the argument list again with the ones provided
        if(identical(Parameters[["slide_id"]], Parameters[["image_id"]])){
          Simple_data <- dplyr::bind_cols(DATA[1:4], DATA[Marker])
          Simple_Parameters <- Parameters
          Simple_Parameters[["mx_data"]] <- mxnorm::mx_dataset(data = Simple_data,
                                                               slide_id = "Subject_Names",
                                                               image_id = "Subject_Names",
                                                               marker_cols = Marker)
          DATA_normalized <- base::do.call(mxnorm::mx_normalize, args = Simple_Parameters)
          DATA_normalized <- as_tibble(DATA_normalized$norm_data)[-c(1:2)]
          return(unname(unlist(DATA_normalized)))
        }
        
        #If slide_id is not the same as image_id generate another parameter list
        if(!identical(Parameters[["slide_id"]], Parameters[["image_id"]])) {
          Simple_data <- dplyr::bind_cols(DATA[1:4], 
                                          DATA[Marker],
                                          tibble(slide_id = Parameters[["slide_id"]]))
          Simple_Parameters <- Parameters
          Simple_Parameters[["mx_data"]] <- mxnorm::mx_dataset(data = Simple_data,
                                                               slide_id = "slide_id",
                                                               image_id = "Subject_Names",
                                                               marker_cols = Marker)
          DATA_normalized <- base::do.call(mxnorm::mx_normalize, args = Simple_Parameters)
          DATA_normalized <- as_tibble(DATA_normalized$norm_data)[-c(1:2)]
          return(unname(unlist(DATA_normalized)))
          
        }
      }, .progress = TRUE)
      future::plan("future::sequential")
      gc()
      
      RESULTS <- map_dfc(RESULTS, ~bind_cols(., .name_repair = "universal"))
      names(RESULTS) <- names(DATA)[-c(1:4)]
      return(bind_cols(DATA[1:4], RESULTS))
    }
    
    #Then simpleSeg
    else if(Strategy == "simpleSeg") {
      #check that argument list is correct and contains adequate data
      if(!all(c("cells", "markers", "imageID", "transformation", "method", "cores") %in% names(Parameters))){
        Missing_arguments <- c("cells", "markers", "imageID", "transformation", "method", "cores")[
          !c("cells", "markers", "imageID", "transformation", "method", "cores") %in% names(Parameters)
        ]
        stop(paste0(str_c(Missing_arguments, collapse = ", "), " not present in Paramters list"))
      }
      
      #Force cores to be 1
      Parameters[["cores"]] <- 1
      
      #check if arguments are well specified
      Argument_checker <- c(markers_OK = all(Parameters[["markers"]] %in% names(Parameters[["cells"]])),
                            imageID_OK = Parameters[["imageID"]] == "Subject_Names",
                            
                            transform_OK = if(!is.null(Parameters[["transformation"]])){
                              Parameters[["transformation"]] %in% c("asinh", "sqrt")
                            } else(TRUE),
                            method_OK = if(!is.null(Parameters[["method"]])){
                              all(Parameters[["method"]] %in% c('mean', 'minMax', 'trim99', 'PC1'))
                            } else(TRUE),
                            cores_OK = all(Parameters[["cores"]]%%1 == 0, Parameters[["cores"]] > 0)
      )
      Stop_messages <- c(markers_OK = "Marker names not present in the cell Data",
                         imageID_OK = "Image ID must be Subject_Names",
                         transform_OK = "Transformation method must be NULL or one of the following: asinh, sqrt",
                         method_OK = "Method must be NULL or one of the following: NULL, mean, minMax, trim99, PC1",
                         cores_OK = "Core number must be a positive integer value"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
      
      DATA <- DATA
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      #Execute the function in a map way
      RESULTS <- furrr::future_map(Parameters[["markers"]], 
                                              function(Marker){
                                                Simple_data <- dplyr::bind_cols(DATA[1:4], DATA[Marker])
                                                Simple_Parameters <- Parameters
                                                Simple_Parameters[["cells"]] <- Simple_data
                                                Simple_Parameters[["markers"]] <- Marker
                                                
                                                #Execute the code and select only the final column
                                                base::do.call(simpleSeg::normalizeCells, args = Simple_Parameters)[5]
                                              }, .progress = TRUE)
      future::plan("future::sequential")
      gc()
      
      #Merge results in a tibble
      RESULTS <- map_dfc(RESULTS, bind_cols)
      #Return all the results
      bind_cols(DATA[1:4], RESULTS)
    }
  },
  options = list(optimize = 3))

Normalization_diagnostics <- cmpfun(
  function(Original_DATA = NULL,
           Normalized_DATA = NULL){
    on.exit(gc())
    
    Original_DATA <- Original_DATA
    Normalized_DATA <- Normalized_DATA
    
    #Check arguments
    if(!identical(names(Original_DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))){
      stop("Original_DATA not correctly specified, please format appropiatetly (see Step 0)")
    }
    #Check arguments
    if(!identical(names(Normalized_DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))){
      stop("Normalized_DATA not correctly specified, please format appropiatetly (see Step 0)")
    }
    #Then check that both names are the same and in the same order
    if(!identical(names(Original_DATA), names(Normalized_DATA))) stop("Names in both datasets don't match. Please review")
    #Then check that both Subject_Names columns are identical
    if(!identical(Original_DATA$Subject_Names, Normalized_DATA$Subject_Names)) stop("Subject_Names columns are not identical in provided datasets")
    
    
    #Start by calculating the otsu index for each variable for original and normalized
    Thresholds_original <- bind_cols(
      tibble(Subject_Names = unique(Original_DATA$Subject_Names), Data = "Original"),
      map_dfr(unique(Original_DATA$Subject_Names), function(Image){
        Interim <- Original_DATA %>% dplyr::filter(Subject_Names == Image)
        map_dbl(Interim[-c(1:4)], function(z){
          EBImage::otsu(array(z, dim = c(1, length(z))), range = c(min(z), max(z)), levels = length(unique(z)))
        })
      }, .progress = list(clear = F,
                          name = "Calculating Otsu thresholds by sample in original data",
                          show_after = 2,
                          type = "iterator"))
    )
    
    
    Thresholds_normalized <- bind_cols(
      tibble(Subject_Names = unique(Normalized_DATA$Subject_Names), Data = "Normalized"),
      map_dfr(unique(Normalized_DATA$Subject_Names), function(Image){
        Interim <- Normalized_DATA %>% dplyr::filter(Subject_Names == Image)
        map_dbl(Interim[-c(1:4)], function(z){
          EBImage::otsu(array(z, dim = c(1, length(z))), range = c(min(z), max(z)), levels = length(unique(z)))
        })
      }, .progress = list(clear = F,
                          name = "Calculating Otsu thresholds by sample in normalied data",
                          show_after = 2,
                          type = "iterator"))
    )
    
    #Generate the plot of the Otsu thresholds by sample
    plot(
      bind_rows(Thresholds_original, Thresholds_normalized) %>% pivot_longer(cols = -c(1:2)) %>%
        ggplot(aes(x = factor(Data, levels= c("Original", "Normalized")), y = value, color = Data, fill = Data)) + facet_wrap(~name, scales = "free_y") +
        geom_boxplot(outlier.color=NA, alpha = 0.2, coef = 0) +
        geom_point(position=position_jitter(width = 0.1), alpha = 0.5) +
        guides(color = "none", fill = "none")+
        scale_x_discrete("") +
        scale_y_continuous("Otsu threshold by sample") +
        cowplot::theme_cowplot() +
        theme(axis.text.x = element_text(size = 10, color = "black"),
              strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, color = "black", face = "bold"))
    )
    
    #Generate a tibble with the CV and plot them
    CV_tibble <- map_dfr(list(Thresholds_original,
                              Thresholds_normalized), 
                         function(Tibble){
                           bind_cols(unique(Tibble[2]),
                                     map_df(Tibble[-c(1:2)], function(variable){
                                       round((sd(variable) / mean(variable))*100, 2)
                                     }))
                         })
    
    plot(
      CV_tibble %>% 
        pivot_longer(-Data) %>%
        ggplot(aes(x = name, y = value, fill = factor(Data, levels = c("Original", "Normalized")))) + 
        geom_col(width = 0.5, color = "black", size = 1.2, position = position_dodge(width = 0.6)) +
        scale_x_discrete("") +
        scale_y_continuous("CV of Otsu thresholds") +
        scale_fill_discrete("") +
        cowplot::theme_cowplot() +
        theme(axis.text = element_text(size = 12, color = "black"),
              legend.text = element_text(size = 12, color = "black"),
              legend.position = "bottom")
    )
    
    #Generate scaled versions of Original and normalized data in order to plot the histograms
    Histo_Original <- bind_cols(tibble(Data = "Original"),
                                Original_DATA[-c(1:4)] %>% scale())
    Histo_Normalized <- bind_cols(tibble(Data = "Normalized"),
                                  Normalized_DATA[-c(1:4)] %>% scale())
    
    plot(
      ggplot() + 
        geom_density(aes(y = value, x = -after_stat(density), fill = "Original"), color = "black", size = 0.8, data = Histo_Original %>% pivot_longer(-1)) + 
        geom_density(aes(y = value, x = after_stat(density), fill = "Normalized"), color = "black", size = 0.8, data = Histo_Normalized %>% pivot_longer(-1))+
        facet_wrap(~name, scales = "free") +
        geom_vline(xintercept = 0, size = 0.8, color = "black") +
        scale_x_continuous("", labels = NULL) +
        scale_y_continuous("", 
                           limits = c(-3, 3), #Limit the Y scale to avoid over-informating outsiders 
                           labels = NULL)+
        cowplot::theme_cowplot() +
        scale_fill_discrete("", breaks = c("Original", "Normalized"))+
        theme(legend.position = "bottom",
              strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, color = "black", face = "bold"))
    )
  },
  options = list(optimize = 3))

############STEP 2 - THRESHOLDING - REQUIRED FUNCTIONS###########
message("Importing functions: STEP 2 - THRESHOLDING")
SHINY_Thresholding_summary_function <- cmpfun(
  function(DATA = NULL,
           DATA_Thresholded = NULL,
           LOCAL = NULL,
           CASE = NULL) {
    
    #First GLOBAL thresholds (will not vary from case to case)
    if(!as.logical(LOCAL)){
      
      #First GLOBAL 2 level thresholding
      if(length(unique(DATA_Thresholded$Marker)) < 3){
        
        #Find the threshold
        Thresholds <- min(DATA$Marker[DATA_Thresholded[[5]]])
        #Generate tibble
        Threshold_results <- tibble(Threshold_level = str_c("Threshold_Level_", 1:length(Thresholds)),
                                    value = Thresholds)
        
        #Draw the histogram
        Histogram_plot <- DATA %>% dplyr::filter(Subject_Names == CASE) %>%
          ggplot(aes(x = Marker)) + 
          geom_histogram(bins = 1000) +
          cowplot::theme_cowplot() +
          geom_vline(aes(xintercept = value), data = Threshold_results, color = "red", linewidth = 1.2) +
          scale_x_continuous("Marker intensity") +
          scale_y_discrete("Number of cells")
        
        #Calculate positive cells and total number of cells in the sample and place data in a tibble
        DATA_case <- DATA_Thresholded %>% dplyr::filter(Subject_Names == CASE)
        
        Sample_summary <- tibble(Cell = c("Positive Cells", "Total Cells"), number = c(sum(DATA_case[[5]]), nrow(DATA_case)))
        
        #Return the list
        return(list(Histogram = Histogram_plot,
                    Threshold_tibble = Threshold_results,
                    Summary = Sample_summary
        )
        )
      }
      
      #Then GLOBAL multi-level thresholding
      else if(length(unique(DATA_Thresholded$Marker)) >= 3){
        #Find the threshold
        Thresholds <- map_dbl(unique(DATA_Thresholded$Marker), function(Level) {
          min(DATA$Marker[DATA_Thresholded$Marker == Level])
        })
        #Generate tibble
        Threshold_results <- tibble(Threshold_level = str_c("Threshold_Level_", 1:length(Thresholds)),
                                    value = Thresholds)
        #Draw the histogram
        Histogram_plot <- DATA %>% dplyr::filter(Subject_Names == CASE) %>%
          ggplot(aes(x = Marker)) + 
          geom_histogram(bins = 1000) +
          cowplot::theme_cowplot() +
          geom_vline(aes(xintercept = value), data = Threshold_results, color = "red", linewidth = 1.2) +
          scale_x_continuous("Marker intensity") +
          scale_y_discrete("Number of cells")
        
        #Calculate positive cells and total number of cells in the sample and place data in a tibble
        DATA_case <- DATA_Thresholded %>% dplyr::filter(Subject_Names == CASE)
        
        Sample_summary <- DATA_case %>% dplyr::count(Marker)
        names(Sample_summary) <- c("Cell", "number")
        Sample_summary[[1]] <- str_c("Cell_", as.character(Sample_summary[[1]]))
        Nrow_Sample_summary <- nrow(Sample_summary)
        Sample_summary[Nrow_Sample_summary + 1, 1] <- "Total Cells"
        Sample_summary[Nrow_Sample_summary + 1, 2] <- nrow(DATA_case)
        
        #Return the list
        return(list(Histogram = Histogram_plot,
                    Threshold_tibble = Threshold_results,
                    Summary = Sample_summary)
        )
      }
    }
    
    #Then LOCAL thresholds (will vary from case to case)
    else if(as.logical(LOCAL)){
      #Filter cases by Case_id
      DATA <- DATA %>% dplyr::filter(Subject_Names == CASE)
      DATA_Thresholded <- DATA_Thresholded %>% dplyr::filter(Subject_Names == CASE)
      
      #First LOCAL 2 level thresholding
      if(length(unique(DATA_Thresholded$Marker)) < 3){
        
        #Find the threshold
        Thresholds <- min(DATA$Marker[DATA_Thresholded[[5]]])
        #Generate tibble
        Threshold_results <- tibble(Threshold_level = str_c("Threshold_Level_", 1:length(Thresholds)),
                                    value = Thresholds)
        
        #Draw the histogram
        Histogram_plot <- DATA %>% dplyr::filter(Subject_Names == CASE) %>%
          ggplot(aes(x = Marker)) + 
          geom_histogram(bins = 1000) +
          cowplot::theme_cowplot() +
          geom_vline(aes(xintercept = value), data = Threshold_results, color = "red", linewidth = 1.2) +
          scale_x_continuous("Marker intensity") +
          scale_y_discrete("Number of cells")
        
        #Calculate positive cells and total number of cells in the sample and place data in a tibble
        DATA_case <- DATA_Thresholded 
        
        Sample_summary <- tibble(Cell = c("Positive Cells", "Total Cells"), number = c(sum(DATA_case[[5]]), nrow(DATA_case)))
        
        #Return the list
        return(list(Histogram = Histogram_plot,
                    Threshold_tibble = Threshold_results,
                    Summary = Sample_summary)
        )
      }
      
      #Then LOCAL multi-level thresholding
      else if(length(unique(DATA_Thresholded$Marker)) >= 3){
        
        
        #Return the list
        return(list(Histogram = ggplot() + geom_text(aes(x = 1, y = 1, label = "LOCAL MULTI-LEVEL THRESHOLDING\nIS\nSTRONGLY DISCOURAGED"), 
                                                     color = "white",
                                                     size = 8) +
                      cowplot::theme_cowplot()+
                      theme(axis.line = element_blank(),
                            axis.ticks = element_blank(),
                            axis.text = element_blank(),
                            axis.title = element_blank(),
                            panel.background = element_rect(fill = "black")),
                    Threshold_tibble = tibble(Warning = "Local Multi-level thresholding is not a good idea"),
                    Summary = tibble(Warning = "Local Multi-level thresholding is not a good idea"))
        )
      }
    }
  }, 
  options = list(optimize = 3))

Thresholding_tester_app <- cmpfun(
  function(DATA = NULL,
           Directory = NULL,
           Ordered_Channels = NULL){
    
    DATA <- DATA
    
    #Test that DATA provided is adequate
    if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))) {
      stop("Please generate an appropiate data object using the Data_arrange_function")
    }
    
    Images_in_Data <- unique(DATA$Subject_Names)
    Channels_in_Data <- names(DATA)[-c(1:4)]
    Real_Images <- dir(Directory, full.names = FALSE)
    Channels_in_images <- Ordered_Channels
    Thresholding_methods <- c("EBI_Otsu", "Kmeans", "Kmeans_Otsu", "Autothreshold", "TriClass_Otsu", "Mean", "Quantile", "Arbitrary", "Multi_level")
    Autothreshold_methods <- c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean",
                               "MinErrorI", "Minimum", "Moments", "Otsu", "RenyiEntropy", "Shanbhag", "Triangle", "Yen")
    
    #BUILD THE USER INTERFACE
    {
      user_interface <- shiny::fluidPage(
        
        #Set the title
        shiny::titlePanel("Thresholding exploration APP"),
        
        #We want a two panel layout, one in the left containing the input parameters and the output in the right
        shiny::sidebarLayout(
          #Set the first column (which contains the user defined parameters)
          shiny::sidebarPanel(
            #ID and width
            id="sidebar",
            
            shiny::fluidRow(
              #Select Image to be analyzed
              shiny::column(7, shiny::selectInput("Data_Image_name", "Image from data", sort(Images_in_Data), multiple = FALSE)),
              #Select the marker to be analyzed
              shiny::column(5, shiny::selectInput("Data_Marker_name", "Marker from data", sort(Channels_in_Data), multiple = FALSE))
            ),
            shiny::fluidRow(
              #Select the real image to be displayed
              shiny::column(7, shiny::selectInput("Real_Image_name", "Image to display", sort(Real_Images), multiple = FALSE)),
              #Select the channel to be displayed
              shiny::column(5, shiny::selectInput("Channel", "Channel to display", Channels_in_images, multiple = FALSE))
            ),
            shiny::fluidRow(
              #Select the min point of the image
              shiny::column(4, shiny::sliderInput("Min_Image", "Absolute Black", value = 0, min = 0, max = 100, step = 1)),
              #Select the max point of the image
              shiny::column(4, shiny::sliderInput("Max_Image", "Absolute White", value = 100, min = 0, max = 100, step = 1)),
              shiny::column(2, shinyWidgets::materialSwitch("Change_coords", "Pixel/dist", value = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Change_coords == '1'",
                                                       shiny::textInput("Ratio", "pixel size", value = "1")
              ))
            ),
            
            shiny::fluidRow(
              #Select the Gamma
              shiny::column(6, shiny::sliderInput("Gamma", "Gamma", value = 0, min = -3, max = +3, step = 0.01)),
              #Select the image resolution
              shiny::column(3, shiny::selectInput("Res", "Image Res", c('Very Low' = 300, Low = 500, Mid = 750, High = 1000, Original = 1400), selected = 500, multiple = FALSE)),
              #Select the equalization
              shiny::column(3, shiny::selectInput("Equalize", "Equalize", c(YES = TRUE, NO = FALSE), selected = FALSE, multiple = FALSE))
            ),
            
            shiny::fluidRow(
              #Select the thresholding method
              shiny::column(8, shiny::selectInput("Threshold_method", "Thresholding method", Thresholding_methods, multiple = FALSE)),
              #Select the thresholding method
              shiny::column(4, shiny::selectInput("Local", "Type of threshold", c(LOCAL = TRUE, GLOBAL = FALSE), selected = FALSE, multiple = FALSE))
            ),
            #Add thresholding parameters according to the selected threshold method 
            shiny::conditionalPanel(condition = "input.Threshold_method == 'Autothreshold'",
                                    shiny::selectInput("Autothreshold_method", "Autothreshold method", Autothreshold_methods, selected = "Otsu", multiple = FALSE)
            ),
            shiny::conditionalPanel(condition = "input.Threshold_method == 'TriClass_Otsu'",
                                    shiny::numericInput("TriClass_Iters", "TriClass iterations", value = 10, min = 2, max = 30)
            ),
            shiny::conditionalPanel(condition = "input.Threshold_method == 'Quantile'",
                                    shiny::sliderInput("Percentile", "Quantile selected", value = 0.5, min = 0.01, max = 0.99, step = 0.01)
            ),
            shiny::conditionalPanel(condition = "input.Threshold_method == 'Arbitrary'",
                                    shiny::textInput("Arbitrary", "User selected threshold", placeholder = "0.9")
            ),
            shiny::conditionalPanel(condition = "input.Threshold_method == 'Multi_level'",
                                    shiny::numericInput("Levels", "Number of Levels", value = 3, min = 2, max = 10)
            ),
            #Finally add a couple of rows more with extra options and the final result
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput("X_flip", "Flip X image", c(YES = TRUE, NO = FALSE), selected = FALSE, multiple = FALSE)),
              shiny::column(4, shiny::selectInput("Y_flip", "Flip Y image", c(YES = TRUE, NO = FALSE), selected = FALSE, multiple = FALSE)),
              shiny::column(4, shiny::actionButton("reset", shiny::icon("redo"), label = "Reset selection"))
            ),
            #The UI will be completed with summary tables of the sample
            shiny::fluidRow(
              shiny::column(6, htmltools::p("Final threshold/s is: ", shiny::tableOutput("Final_threshold"))),
              shiny::column(6, htmltools::p("Sample Summary: ", shiny::tableOutput("Summary")))
            ),
            #Also it will include a summary of selected cells
            shiny::fluidRow(
              shiny::column(12, htmltools::p("Selected cells: ", shiny::tableOutput("Cell_selection")))
            )
          ),
          
          #Set the outcome columns
          shiny::mainPanel(
            #First row will have the Photo and the overview of marker intensity by cell
            shiny::fluidRow(
              shiny::column(5, shiny::plotOutput("Photo",
                                                 #Controls for zoom in
                                                 dblclick = "Photo_dblclick",
                                                 brush = shiny::brushOpts(id = "Photo_brush",
                                                                          resetOnNew = TRUE)
              )
              ),
              shiny::column(5, ggiraph::girafeOutput("Cell_by_intensity"))
            ),
            #Second row will contain the positive cells and the histogram
            shiny::fluidRow(
              shiny::column(5, ggiraph::girafeOutput("Positive_cells")),
              shiny::column(5, shiny::plotOutput("Histogram"))
            )
          )
        ),
        shiny::tags$head(shiny::tags$style(
          htmltools::HTML('
         #sidebar {
            background-color: #d2dbfa;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')))
      )
    }
    #BUILD THE SERVER
    server <- function(input, output, session){
      #All the reactives to be used
      #Generate a reactive with the real Image name and the channel number
      Photo_name <- shiny::reactive(str_c(Directory, "/", input$Real_Image_name))
      Channel_index <- shiny::reactive(which(input$Channel == Ordered_Channels))
      Photo_min <- shiny::reactive(input$Min_Image)
      Photo_max <- shiny::reactive(input$Max_Image)
      Photo_gamma <- shiny::reactive(10^input$Gamma)
      Equalize <- shiny::reactive(input$Equalize)
      #Reactive expression to control the graphs
      Case_id <- shiny::reactive(input$Data_Image_name)
      Variable <- shiny::reactive(input$Data_Marker_name)
      Local_threshold <- shiny::reactive(input$Local)
      Threshold_method <- shiny::reactive(input$Threshold_method)
      Autothreshold_method <- shiny::reactive(input$Autothreshold_method)
      TriClass_Iters <- shiny::reactive(input$TriClass_Iters)
      Percentile <- shiny::reactive(input$Percentile)
      Arbitrary <- shiny::reactive(input$Arbitrary)
      Levels <- shiny::reactive(input$Levels)
      X_flip <- shiny::reactive(input$X_flip)
      Y_flip <- shiny::reactive(input$Y_flip)
      Pixel_dist_conversion <- shiny::reactive(input$Change_coords)
      Pixel_dist_ratio <- shiny::reactive(input$Ratio)
      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      Resolution <- shiny::reactive(input$Res)
      #Control the data source
      Source_DATA <- shiny::reactive({
        Final_DATA <- DATA %>% dplyr::select(1:4, all_of(Variable()))
        names(Final_DATA)[5] <- "Marker"
        #Modify pixel values if required
        if(as.logical(Pixel_dist_conversion())){
          Final_DATA$X <- Final_DATA$X * as.numeric(Pixel_dist_ratio())
          Final_DATA$Y <- Final_DATA$Y * as.numeric(Pixel_dist_ratio())
        }
        #Return the final data
        return(Final_DATA)
      })
      
      #Generate the thresholded DATA
      Thresholded_DATA <- shiny::reactive({
        DATA_thresholded <- Thresholding_function(
          DATA = Source_DATA(), 
          Strategy = as.character(Threshold_method()), 
          Local_thresholding = as.logical(Local_threshold()), 
          Method_autothreshold = as.character(Autothreshold_method()), 
          number_iterations_TriClass = as.numeric(TriClass_Iters()),
          Percentile = as.numeric(Percentile()),
          Defined_threshold = as.numeric(Arbitrary()),
          Levels = as.numeric(Levels())
        )
        DATA_thresholded
      })
      #Obtain a list with the histogram and the final thresholds using the dedicated function
      Histo_list <- shiny::reactive(SHINY_Thresholding_summary_function(DATA = Source_DATA(),
                                                                        DATA_Thresholded = Thresholded_DATA(),
                                                                        LOCAL = Local_threshold(),
                                                                        CASE = Case_id())
      )
      
      #Reactive that imports the photograph (will only be updated when data or photo parameters are updated) 
      Photo_reactive <- shiny::reactive({
        #Import the Photo
        Photo <- magick::image_read(Photo_name())[Channel_index()]
        #Perform flip and flop if required
        if(as.logical(X_flip())) Photo <- Photo %>% magick::image_flop()
        if(as.logical(Y_flip())) Photo <- Photo %>% magick::image_flip()
        #Perform image equalization as requested by user
        if(as.logical(Equalize())) Photo <- Photo %>% magick::image_equalize()
        #Perform image white adjustment
        Photo <- Photo %>% 
          magick::image_level(black_point = Photo_min(),
                              white_point = Photo_max(),
                              mid_point = Photo_gamma())
        
        
        #Obtain dimensions (these will set the axis limits) BEFORE the resolution is modified
        Photo_Dim <- magick::image_info(Photo)
        #Change resolution if appropiate
        if(as.numeric(Resolution() != 1400)){
          Image_Resolution <- str_c("X", Resolution())
          Photo <- magick::image_scale(Photo, Image_Resolution)
        }
        return(list(Photo = Photo,
                    Dims = Photo_Dim))
      })
      
      #Generate the photo plot reactive 
      Photo_plot_reactive <- shiny::reactive({
        Photo <- Photo_reactive()[["Photo"]]
        Photo_Dim <- Photo_reactive()[["Dims"]]
        #Return the result as a scaffold ggplot_object
        Photo_plot <- ggplot() + 
          annotation_raster(Photo, xmin = 0, xmax = Photo_Dim$width, ymin = 0, ymax = Photo_Dim$height, interpolate = TRUE)+
          scale_x_continuous(limits = c(0, Photo_Dim$width)) +
          scale_y_continuous(limits = c(0, Photo_Dim$height)) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
          theme(axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.background = element_rect(fill = "black"),
                panel.grid = element_blank())
        return(Photo_plot)
      }) 
      
      #Print the photo
      output$Photo <- shiny::renderPlot({
        #Try to Plot the result
        try(Photo <- Photo_plot_reactive())
        
        #If the photo returns an error return the point image
        if(berryFunctions::is.error(Photo)){
          DATA_plot <- Source_DATA() %>% dplyr::filter(Subject_Names == Case_id())
          #Modify pixel values if required
          if(as.logical(Pixel_dist_conversion())){
            DATA_plot$X <- DATA_plot$X * as.numeric(Pixel_dist_ratio())
            DATA_plot$Y <- DATA_plot$Y * as.numeric(Pixel_dist_ratio())
          }
          
          return(
            DATA_plot %>% ggplot() +
              geom_point(aes(x = X, y = Y),
                         color = "white",
                         size = 2.5) +
              scale_x_continuous(limits = c(min(DATA_plot$X), max(DATA_plot$X))) +
              scale_y_continuous(limits = c(min(DATA_plot$Y), max(DATA_plot$Y))) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
              theme(axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.line = element_blank(),
                    panel.background = element_rect(fill = "black"),
                    panel.grid = element_blank()) +
              annotate("text", x = quantile(DATA_plot$X, 0.5), y = quantile(DATA_plot$Y, 0.5), 
                       color = "red", size = 2, hjust = 0.5,
                       label = "UNABLE TO RENDER PHOTO\nUSE ME TO ZOOM IN")
          )
          
        }
        #else Generate the plot with the photo
        else{
          return(Photo)
        }
      }, res = 300)
      
      #Control the zoom in of the Photo
      shiny::observeEvent(input$Photo_dblclick, {
        brush <- input$Photo_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      
      #All cells marker expression
      #Create a reactive that will generate the very basic PLOT
      Cell_intensity_plot <- 
        shiny::reactive({
          #Get data and change the axis to coordinate all three plots displayed
          DATA_plot <- Source_DATA() %>% dplyr::filter(Subject_Names == Case_id())
          
          #Try to import the photo plot
          try(Photo <- Photo_plot_reactive())
          
          #Generate the color
          color_fun <- 
            circlize::colorRamp2(breaks = c(min(DATA_plot$Marker), quantile(DATA_plot$Marker, 0.99)),
                                 colors = c(alpha("black", 0),
                                            alpha("red", 1))
            )
          DATA_plot$Color <- color_fun(DATA_plot$Marker)
          
          #Generate the final plot
          #If the Photo returns an error do not plot it
          if(berryFunctions::is.error(Photo)){
            return(
              DATA_plot %>% ggplot() +
                ggiraph::geom_point_interactive(aes(x = X, y = Y, color = Color,
                                                    data_id = Cell_no, 
                                                    tooltip = str_c(as.character(Cell_no), " ", "Value = ", as.character(round(Marker, 4)))),
                                                hover_nearest = FALSE,
                                                size = 2.5) +
                scale_color_identity() +
                scale_x_continuous(limits = c(min(DATA_plot$X), max(DATA_plot$X))) +
                scale_y_continuous(limits = c(min(DATA_plot$Y), max(DATA_plot$Y))) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
                cowplot::theme_cowplot()+
                guides(colour = "none")+
                theme(axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_blank(),
                      axis.title = element_blank(),
                      panel.background = element_rect(fill = "black"),
                      legend.text = element_text(size = 10))
            )
          }
          #Else plot as usual
          else{
            Final_plot <-
              Photo +
              ggiraph::geom_point_interactive(aes(x = X, y = Y, color = Color,
                                                  data_id = Cell_no, 
                                                  tooltip = str_c(as.character(Cell_no), " ", "Value = ", as.character(round(Marker, 4)))),
                                              hover_nearest = FALSE,
                                              size = 2.5,
                                              data = DATA_plot) +
              scale_color_identity() +
              guides(colour = "none")
            return(Final_plot)
          }
          
        })
      #Send the plot to the UI
      output$Cell_by_intensity <- ggiraph::renderGirafe({
        plot <- ggiraph::girafe(code = print(Cell_intensity_plot()),
                                options = list(
                                  ggiraph::opts_hover(css = "stroke:black;cursor:pointer;", reactive = TRUE),
                                  ggiraph::opts_selection(type = "multiple", css = "fill:#FF3333;stroke:black;")
                                )
        )
        return(plot)
      })
      
      #Positive cells
      #Create a reactive that will generate the very basic PLOT
      Positive_cells_plot <- shiny::reactive({
        DATA_threshold <- Thresholded_DATA() %>% dplyr::filter(Subject_Names == Case_id())
        return(DATA_threshold)
      })
      #Send the plot to the UI according to the thresholding method
      output$Positive_cells <- ggiraph::renderGirafe({
        #Try to Import the photo
        try(Photo <- Photo_plot_reactive())
        
        #If there is an error with the Photo still execute the graph
        if(berryFunctions::is.error(Photo)){
          #Define behavior for 2 levels
          if(Threshold_method() != "Multi_level"){
            #Add specific code for non Multi_level
            Plot_code <- Positive_cells_plot() %>% 
              ggplot() +
              cowplot::theme_cowplot()+
              theme(axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.background = element_rect(fill = "black"),
                    legend.text = element_text(size = 10)) +
              scale_colour_manual("", values = c(alpha("black", 0), "red")) +
              guides(color = "none") +
              scale_x_continuous(limits = c(min(Positive_cells_plot()$X), max(Positive_cells_plot()$X))) +
              scale_y_continuous(limits = c(min(Positive_cells_plot()$Y), max(Positive_cells_plot()$Y))) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
              scale_fill_identity() +
              ggiraph::geom_point_interactive(aes(x = X, y = Y, color = Marker,
                                                  data_id = Cell_no, 
                                                  tooltip = as.character(Cell_no)),
                                              size = 2.5)
            #Send it to plot
            plot <- ggiraph::girafe(code = print(Plot_code),
                                    options = list(
                                      ggiraph::opts_hover(css = "stroke:black;cursor:pointer;", reactive = TRUE),
                                      ggiraph::opts_selection(type = "multiple", css = "fill:#FF3333;stroke:black;")
                                    ))
            return(plot)
          }
          #Define behavior for Multi-level
          else{
            #Add specific code for multi_level
            Plot_code <- Positive_cells_plot() %>%
              ggplot() +
              cowplot::theme_cowplot()+
              theme(axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.background = element_rect(fill = "black"),
                    legend.text = element_text(size = 10)) + 
              scale_colour_manual("", values = c(alpha("black", 0), RColorBrewer::brewer.pal(n = Levels()-1, "Reds")))+
              scale_x_continuous(limits = c(min(Positive_cells_plot()$X), max(Positive_cells_plot()$X))) +
              scale_y_continuous(limits = c(min(Positive_cells_plot()$Y), max(Positive_cells_plot()$Y))) +
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
              scale_fill_identity() +
              ggiraph::geom_point_interactive(aes(x = X, y = Y, color = as.factor(Marker),
                                                  data_id = Cell_no, 
                                                  tooltip = str_c("Value = ", as.character(round(Marker, 0)))),
                                              size = 2.5)
            #Send it to plot
            plot <- ggiraph::girafe(code = print(Plot_code),
                                    options = list(
                                      ggiraph::opts_hover(css = "stroke:black;cursor:pointer;", reactive = TRUE),
                                      ggiraph::opts_selection(type = "multiple", css = "fill:#FF3333;stroke:black;")
                                    ))
            return(plot)
          }
        }
        #If no error then continue with normal execution
        else{
          #Define behavior for 2 levels
          if(Threshold_method() != "Multi_level"){
            #Add specific code for non Multi_level
            Plot_code <- Photo +
              scale_colour_manual("", values = c(alpha("black", 0), "red")) +
              guides(color = "none") +
              ggiraph::geom_point_interactive(aes(x = X, y = Y, color = Marker,
                                                  data_id = Cell_no, 
                                                  tooltip = as.character(Cell_no)),
                                              size = 2.5,
                                              data = Positive_cells_plot())
            #Send it to plot
            plot <- ggiraph::girafe(code = print(Plot_code),
                                    options = list(
                                      ggiraph::opts_hover(css = "stroke:black;cursor:pointer;", reactive = TRUE),
                                      ggiraph::opts_selection(type = "multiple", css = "fill:#FF3333;stroke:black;")
                                    ))
            return(plot)
          }
          #Define behavior for Multi-level
          else{
            #Add specific code for multi_level
            Plot_code <- Photo +
              theme(legend.text = element_text(size = 10)) + 
              scale_colour_manual("", values = c(alpha("black", 0), RColorBrewer::brewer.pal(n = Levels()-1, "Reds")))+
              ggiraph::geom_point_interactive(aes(x = X, y = Y, color = as.factor(Marker),
                                                  data_id = Cell_no, 
                                                  tooltip = str_c("Value = ", as.character(round(Marker, 0)))),
                                              size = 2.5,
                                              data = Positive_cells_plot())
            #Send it to plot
            plot <- ggiraph::girafe(code = print(Plot_code),
                                    options = list(
                                      ggiraph::opts_hover(css = "stroke:black;cursor:pointer;", reactive = TRUE),
                                      ggiraph::opts_selection(type = "multiple", css = "fill:#FF3333;stroke:black;")
                                    ))
            return(plot)
          }
        }
      })
      
      #Histogram
      output$Histogram <- shiny::renderPlot(
        plot(Histo_list()[[1]])
      )
      #Threshold text
      output$Final_threshold <- shiny::renderTable(Histo_list()[[2]])
      
      #Threshold sample summary
      output$Summary <- shiny::renderTable(Histo_list()[[3]])
      
      #Selected cells and reset button
      selected_cells_intensity <- shiny::reactive(input$Cell_by_intensity_selected)
      selected_cells_positive <- shiny::reactive(input$Positive_cells_selected)
      #What to do in case the user hits the reset button
      shiny::observeEvent(input$reset, {
        session$sendCustomMessage(type = 'Cell_by_intensity_set', message = character(0))
        session$sendCustomMessage(type = 'Positive_cells_set', message = character(0))
      })
      #Generate the output tibble
      output$Cell_selection <- shiny::renderTable({
        #Get cell_no in both plots and remove duplicates
        Cells <- unique(c(selected_cells_intensity(), selected_cells_positive()))
        #Get Marker data and threshold data an start processing
        Marker_data <- Source_DATA() %>% dplyr::filter(Subject_Names == Case_id(), Cell_no %in% Cells) %>% dplyr::select(-Subject_Names)
        Threshold_data <- Thresholded_DATA() %>% dplyr::filter(Subject_Names == Case_id(), Cell_no %in% Cells) %>% dplyr::select(-Subject_Names, -X, -Y)
        Final <- Threshold_data %>% dplyr::count(Marker)
        names(Final) <- c("Cell", "number")
        Final
      })
      
      #If browser is closed end the app
      session$onSessionEnded(function() { shiny::stopApp() })
    }
    
    #Run the server
    message("Always stop current R execution if you want to continue with your R session")
    shiny::shinyApp(user_interface, server)
  },
  options = list(optimize = 3))

Thresholding_function <- 
  cmpfun(
    function(DATA = NULL, 
             Strategy = NULL, 
             Local_thresholding = F, 
             Method_autothreshold = "Otsu", 
             number_iterations_TriClass = 20,
             Percentile = 0.5,
             Defined_threshold = 0.1,
             Levels = 3) {
      if(!(Strategy %in% c("EBI_Otsu", "Kmeans", "Kmeans_Otsu", "Autothreshold", "TriClass_Otsu", "Mean", "Quantile", "Arbitrary", "Multi_level"))) {
        stop("Strategy not correctly specified, please choose between EBI_Otsu, Kmeans, Kmeans_Otsu, Autothreshold, TriClass_Otsu, Mean, Quantile, Arbitrary, Multi_level")
      }
      
      else if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))) {
        stop("DATA not correctly specified, please format appropiatetly (see Step 0)")
      }
      if(!is.logical(Local_thresholding)) stop("Local_thresholding must be a logical value")
      
      else if(Local_thresholding == T) {
        warning("Local thresholding should be avoided. Take into account that global thresholding is the preferred method")
        
        if(Strategy == "EBI_Otsu"){
          bind_cols(DATA[(1:4)], 
                    map_dfr(map(unique(DATA$Subject_Names), function(x) {
                      Tibble <- DATA %>% dplyr::filter(Subject_Names == x)
                      map2_df(.x = Tibble[-c(1:4)],
                              .y = map_dbl(Tibble[-c(1:4)], function(z){
                                EBImage::otsu(array(z, dim = c(1, length(z))), range = c(min(z), max(z)), levels = length(unique(z)))
                              }),
                              function(.x, .y) .x >= .y)
                    }), bind_rows, .progress = list(clear = F,
                                                    name = "Calculating thresholds",
                                                    show_after = 2,
                                                    type = "iterator"))
          )
        }
        
        else if(Strategy == "Kmeans"){
          bind_cols(DATA[(1:4)], 
                    map_dfr(unique(DATA$Subject_Names), function(x){
                      Tibble <- DATA %>% dplyr::filter(Subject_Names == x)
                      map_dfc(Tibble[-c(1:4)], function(z){
                        as.logical(mmand::threshold(z, method = "kmeans", binarise = T))
                      })
                    }, .progress = list(clear = F,
                                        name = "Calculating thresholds",
                                        show_after = 2,
                                        type = "iterator"))
          )
        }
        
        else if(Strategy == "Kmeans_Otsu"){
          Interim_list <- map(unique(DATA$Subject_Names), function(Individual) DATA %>% dplyr::filter(Subject_Names == Individual) %>% dplyr::select(-c(1:4)))
          bind_cols(DATA[(1:4)],
                    map_dfr(Interim_list,
                            function(Individual){
                              map_dfc(Individual, function(feature){
                                if(length(unique(feature))>1){ #Check if vector has more than one unique value (if not no threshold can be calculated)
                                  if(!berryFunctions::is.error(as.vector(imager::threshold(imager::as.cimg(feature, dim = c(x = 1, y = length(feature), z = 1, cc = 1)),thr = "auto")))){
                                    #Check if thresholding by otsu K means yields an error anyway (due to histogram shape)
                                    as.vector(imager::threshold(
                                      imager::as.cimg(feature, dim = c(x = 1, y = length(feature), z = 1, cc = 1)),
                                      thr = "auto", approx = F))
                                  } 
                                  else(NA)
                                } 
                                else(NA)
                              })
                            }, .progress = list(clear = F,
                                                name = "Calculating thresholds",
                                                show_after = 2,
                                                type = "iterator")))
          
        }
        
        else if(Strategy == "Autothreshold"){
          #Check autothreshold argument
          if(!Method_autothreshold %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean",
                                          "MinErrorI", "Minimum", "Moments", "Otsu", "RenyiEntropy", "Shanbhag", "Triangle", "Yen")) {
            stop("Method_autothreshold must be one of the following: IJDefault, Huang, Huang2, Intermodes, IsoData, Li, MaxEntropy, Mean,
                                         MinErrorI, Minimum, Moments, Otsu, RenyiEntropy, Shanbhag, Triangle or Yen")
          }
          
          bind_cols(DATA[(1:4)], 
                    
                    map_dfr(unique(DATA$Subject_Names), function(x) {
                      
                      Tibble <- DATA %>% dplyr::filter(Subject_Names == x)
                      
                      map_dfc(
                        Tibble[-c(1:4)], function(z) {
                          if(length(unique(z))>1){
                            as.logical(autothresholdr::auto_thresh_mask(round((z/max(z))*1000, digits = 0),#Requires integer, just in case multiply by large number and round
                                                                        method = Method_autothreshold, #Specify the method
                                                                        ignore_black = F,
                                                                        ignore_na = F))
                          } else(NA)
                        })
                    }, .progress = list(clear = F,
                                        name = "Calculating thresholds",
                                        show_after = 2,
                                        type = "iterator"))
          )
          
        }
        
        else if(Strategy == "TriClass_Otsu"){
          #Check iteration argument
          if(number_iterations_TriClass%%1 != 0) stop("number_iterations_TriClass must be an integer value")
          
          bind_cols(DATA[(1:4)], 
                    map_dfr(map(unique(DATA$Subject_Names), function(x) {
                      Tibble <- DATA %>% dplyr::filter(Subject_Names == x)
                      map_df(Tibble[-c(1:4)],
                             function(z){
                               if(length(unique(z))>1){ #requires at least 2 levels
                                 as.logical(as.double(imagerExtra::ThresholdTriclass(imager::cimg(array(z, dim = c(1, length(z), 1, 1))),
                                                                                     repeatnum = number_iterations_TriClass)))#How many times Otsu method should be refined
                               }else(NA)
                             })
                    }), bind_rows, .progress = list(clear = F,
                                                    name = "Calculating thresholds",
                                                    show_after = 2,
                                                    type = "iterator"))
          )
          
        }
        
        else if(Strategy == "Mean"){
          Interim_list <- map(unique(DATA$Subject_Names), function(Individual) DATA %>% dplyr::filter(Subject_Names == Individual) %>% dplyr::select(-c(1:4)))
          bind_cols(DATA[(1:4)], 
                    map_dfr(Interim_list,
                            function(Individual){
                              map_dfc(Individual, function(z){
                                Threshold <- mean(z)
                                z >= Threshold
                              })
                            }, .progress = list(clear = F,
                                                name = "Calculating thresholds",
                                                show_after = 2,
                                                type = "iterator"))
          )
        }
        
        else if(Strategy == "Quantile"){
          #Check percentile argument
          if(Percentile < 0.01 | Percentile > 0.99) stop("Percentile must be between 0.01 and 0.99")
          Interim_list <- map(unique(DATA$Subject_Names), function(Individual) DATA %>% dplyr::filter(Subject_Names == Individual) %>% dplyr::select(-c(1:4)))
          bind_cols(DATA[(1:4)], 
                    map_dfr(Interim_list,
                            function(Individual){
                              map_dfc(Individual, function(z){
                                Threshold <- stats::quantile(z, Percentile)
                                z >= Threshold
                              })
                            }, .progress = list(clear = F,
                                                name = "Calculating thresholds",
                                                show_after = 2,
                                                type = "iterator"))
          )
        }
        
        else if(Strategy == "Arbitrary"){
          #Check argument
          if(!is.numeric(Defined_threshold)) stop("Defined_threshold must be a numeric value")
          
          bind_cols(DATA[(1:4)], 
                    map_dfr(unique(DATA$Subject_Names), function(x){
                      Tibble <- DATA %>% dplyr::filter(Subject_Names == x)
                      map_dfc(Tibble[-c(1:4)], function(z){
                        as.logical(mmand::threshold(z, level = Defined_threshold, method = "literal", binarise = T))
                      })
                    }, .progress = list(clear = F,
                                        name = "Calculating thresholds",
                                        show_after = 2,
                                        type = "iterator"))
          )
        }
        
        else if(Strategy == "Multi_level"){
          #Check level argument
          if(Levels%%1 != 0) stop("Levels must be an integer value")
          
          bind_cols(DATA[(1:4)], 
                    map_dfr(map(unique(DATA$Subject_Names), function(x) {
                      Tibble <- DATA %>% dplyr::filter(Subject_Names == x)
                      map_df(Tibble[-c(1:4)],
                             function(z){
                               if(length(unique(z))>Levels){ #requires at least n levels levels
                                 as.double(imagerExtra::ThresholdML(imager::cimg(array(z, dim = c(1, length(z), 1, 1))), k = (Levels-1))) #K to specify the amount of cut-off points
                               }else(NA)
                             })
                    }), bind_rows, .progress = list(clear = F,
                                                    name = "Calculating thresholds",
                                                    show_after = 2,
                                                    type = "iterator"))
          )
        }
        
      }
      
      else{
        if(Strategy == "EBI_Otsu"){
          bind_cols(DATA[(1:4)], 
                    map2_df(.x = DATA[-c(1:4)],
                            .y = map_dbl(DATA[-c(1:4)], function(z){
                              EBImage::otsu(array(z, dim = c(1, length(z))), range = c(min(z), max(z)), levels = length(unique(z)))
                            }),
                            function(.x, .y) .x >= .y, 
                            .progress = list(clear = F,
                                             name = "Calculating thresholds",
                                             show_after = 2,
                                             type = "iterator")))
        }
        
        else if(Strategy == "Kmeans"){
          bind_cols(DATA[(1:4)], 
                    map_dfc(DATA[-c(1:4)],
                            function(z){
                              as.logical(mmand::threshold(z, method = "kmeans", binarise = T))
                            }, 
                            .progress = list(clear = F,
                                             name = "Calculating thresholds",
                                             show_after = 2,
                                             type = "iterator"))
          )
        }
        
        else if(Strategy == "Kmeans_Otsu"){
          bind_cols(DATA[(1:4)],
                    map_df(DATA[-c(1:4)],
                           function(feature){
                             if(length(unique(feature))>1){ #Check if vector has more than one unique value (if not no threshold can be calculated)
                               if(!berryFunctions::is.error(as.vector(imager::threshold(imager::as.cimg(feature, dim = c(x = 1, y = length(feature), z = 1, cc = 1)),thr = "auto")))){
                                 #Check if thresholding by otsu K means yields an error anyway (due to histogram shape)
                                 as.vector(imager::threshold(
                                   imager::as.cimg(feature, dim = c(x = 1, y = length(feature), z = 1, cc = 1)),
                                   thr = "auto", approx = F))
                               } 
                               else(NA)
                             } 
                             else(NA)
                           }, 
                           .progress = list(clear = F,
                                            name = "Calculating thresholds",
                                            show_after = 2,
                                            type = "iterator")))
          
        }
        
        else if(Strategy == "Autothreshold"){
          #Check autothreshold argument
          if(!Method_autothreshold %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean",
                                          "MinErrorI", "Minimum", "Moments", "Otsu", "RenyiEntropy", "Shanbhag", "Triangle", "Yen")) {
            stop("Method_autothreshold must be one of the following: IJDefault, Huang, Huang2, Intermodes, IsoData, Li, MaxEntropy, Mean,
                                         MinErrorI, Minimum, Moments, Otsu, RenyiEntropy, Shanbhag, Triangle or Yen")
          }
          
          bind_cols(DATA[(1:4)], 
                    map_df(map_df(DATA[-c(1:4)], function(x) {
                      round((x/max(x))*1000, digits = 0)
                    }),
                    function(z){
                      if(length(unique(z))>1){
                        as.logical(autothresholdr::auto_thresh_mask(z,#Requires integer, just in case multiply by large number and round
                                                                    method = Method_autothreshold, #Specify the method
                                                                    ignore_black = F,
                                                                    ignore_na = F))
                      } else(NA)
                    }, 
                    .progress = list(clear = F,
                                     name = "Calculating thresholds",
                                     show_after = 2,
                                     type = "iterator"))
          )
          
        }
        
        else if(Strategy == "TriClass_Otsu"){
          #Check iteration argument
          if(number_iterations_TriClass%%1 != 0) stop("number_iterations_TriClass must be an integer value")
          
          bind_cols(DATA[(1:4)], 
                    map_df(DATA[-c(1:4)],
                           function(z){
                             if(length(unique(z))>1){ #requires at least 3 levels
                               as.logical(as.double(imagerExtra::ThresholdTriclass(imager::cimg(array(z, dim = c(1, length(z), 1, 1))),
                                                                                   repeatnum = number_iterations_TriClass)))#How many times Otsu method should be refined
                             }else(NA)
                           }, 
                           .progress = list(clear = F,
                                            name = "Calculating thresholds",
                                            show_after = 2,
                                            type = "iterator"))
          )
          
        }
        
        else if(Strategy == "Mean"){
          bind_cols(DATA[(1:4)], 
                    map_df(DATA[-c(1:4)],
                           function(z){
                             Threshold <- mean(z)
                             z >= Threshold
                           }, 
                           .progress = list(clear = F,
                                            name = "Calculating thresholds",
                                            show_after = 2,
                                            type = "iterator"))
          )
        }
        
        else if(Strategy == "Quantile"){
          #Check percentile argument
          if(Percentile < 0.01 | Percentile > 0.99) stop("Percentile must be between 0.01 and 0.99")
          
          bind_cols(DATA[(1:4)], 
                    map_df(DATA[-c(1:4)],
                           function(z){
                             Threshold <- quantile(z, Percentile)
                             z >= Threshold
                           }, 
                           .progress = list(clear = F,
                                            name = "Calculating thresholds",
                                            show_after = 2,
                                            type = "iterator"))
          )
        }
        
        else if(Strategy == "Arbitrary"){
          #Check argument
          if(!is.numeric(Defined_threshold)) stop("Defined_threshold must be a numeric value")
          
          bind_cols(DATA[(1:4)], 
                    map_dfc(DATA[-c(1:4)],
                            function(z){
                              as.logical(mmand::threshold(z, level = Defined_threshold, method = "literal", binarise = T))
                            }, 
                            .progress = list(clear = F,
                                             name = "Calculating thresholds",
                                             show_after = 2,
                                             type = "iterator"))
          )
        }
        
        else if(Strategy == "Multi_level"){
          #Check level argument
          if(Levels%%1 != 0) stop("Levels must be an integer value")
          
          bind_cols(DATA[(1:4)], 
                    map_df(DATA[-c(1:4)],
                           function(z){
                             if(length(unique(z))>Levels){ #requires at least n Levels to be calculated
                               as.double(imagerExtra::ThresholdML(imager::cimg(array(z, dim = c(1, length(z), 1, 1))), k = (Levels-1))) #K to specify the amount of cut-off points
                             }else(NA)
                           }, 
                           .progress = list(clear = F,
                                            name = "Calculating thresholds",
                                            show_after = 2,
                                            type = "iterator"))
          )
        }
      }
      
      
    },
    options = list(optimize = 3))

Thresholding_function_tailored <- cmpfun(
  function(DATA = NULL,
           Variables_tibble = NULL) {
    if(!identical(c("variable", "Strategy", "Local_thresholding", "Method_autothreshold", 
                    "number_iterations_TriClass", "Percentile", "Defined_threshold", "Levels"),
                  names(Variables_tibble))) {
      stop("Names and order of the Variables_tibble must be variable, Strategy, Local_thresholding, Method_autothreshold, number_iterations_TriClass, Percentile, Defined_threshold, Levels")
    }
    else if(!all(unique(Variables_tibble$variable) %in% names(DATA))) {
      stop(paste0("Names provided by the Variable column in Variables_tibble should be one of: ", str_c(names(DATA), collapse = ", ")))
    }
    
    else{
      DATA <- DATA
      
      bind_cols(DATA[1:4],
                pmap_dfc(Variables_tibble, 
                         function(variable, Strategy, Local_thresholding, Method_autothreshold, 
                                  number_iterations_TriClass, Percentile, Defined_threshold, Levels) {
                           DATA <- DATA %>% dplyr::select(1:4, all_of(variable))
                           
                           Thresholding_function(DATA = DATA,
                                                 Strategy = Strategy,
                                                 Local_thresholding = Local_thresholding,
                                                 Method_autothreshold = Method_autothreshold,
                                                 number_iterations_TriClass = number_iterations_TriClass,
                                                 Percentile = Percentile,
                                                 Defined_threshold = Defined_threshold,
                                                 Levels = Levels)[5]
                         }, 
                         .progress = list(clear = F,
                                          name = "Calculating thresholds",
                                          show_after = 2,
                                          type = "iterator"))
      )
    }
  },
  options = list(optimize = 3))

Thresholding_exploration_function <- 
  cmpfun(
    function(DATA = NULL,
             Calculate_Density = NULL,
             DATA_Area = NULL) {
      
      #Check arguments
      if(!is.logical(Calculate_Density)){
        stop("Calculate_Density must be a logical value")
      }
      
      #Import the data
      DATA <- DATA
      
      #Perform the quantification of variables across cases but taking into account if data is dichotomic or polychotomic
      Results <- 
        map_dfr(unique(DATA$Subject_Names), function(Image){
          #Select variables
          DATA_vars <- DATA[-c(1:4)]
          
          #Select dichotomic variables
          DATA_Dichom <- bind_cols(DATA[c(1:4)], DATA_vars[map_lgl(DATA_vars, function(Var) length(unique(Var)) <= 2)])
          #Select polychotomic variables
          DATA_Poly <- bind_cols(DATA[c(1:4)], DATA_vars[map_lgl(DATA_vars, function(Var) length(unique(Var)) > 2)])
          
          #Start with dichotomic variables if these are present in the data
          if(length(names(DATA_Dichom)) > 4){
            #Start with dichomotic variables
            Results_Dichom <- DATA_Dichom %>% dplyr::filter(Subject_Names == Image) %>% pivot_longer(-c(1:4)) %>%
              group_by(name) %>% dplyr::count(value) %>% ungroup() %>% dplyr::filter(value) %>%
              pivot_wider(names_from = name, values_from = n) %>% dplyr::select(-value)
            
            #If no cells are positive prime the tibble with the first column
            if(nrow(Results_Dichom) == 0){
              Results_Dichom<- tibble(value = 0)
              names(Results_Dichom) <- names(DATA_Dichom)[5]
            }
          }
          
          #Continue with polychotomic variables if these are present
          if(length(names(DATA_Poly)) > 4){
            Results_Poly <- DATA_Poly %>% dplyr::filter(Subject_Names == Image) %>% pivot_longer(-c(1:4)) %>%
              group_by(name) %>% dplyr::count(value) %>% ungroup() %>% dplyr::filter(value != 0) %>%
              mutate(Variable = str_c(name, "Level", value, sep = "_")) %>% dplyr::select(-name,-value) %>%
              pivot_wider(names_from = Variable, values_from = n)
            #If no cells are positive prime the tibble with the first column
            if (nrow(Results_Poly) == 0) {
              Results_Poly <- tibble(value = 0)
              names(Results_Poly) <- str_c(names(DATA_Poly)[5], "Level", "1", sep = "_")
            }
          }
          
          #If both polychotomic and dichotomic variables are present in th data return both results
          if(all(exists("Results_Dichom"), exists("Results_Poly"))){
            return(bind_cols(Results_Dichom, Results_Poly))
          }
          #If just dichotomic variables then return Dichom
          else if(exists("Results_Dichom")) {
            return(Results_Dichom)
          }
          #If just polychotomic variables then return Poly
          else if(exists("Results_Poly")){
            return(Results_Poly)
          }
        }, 
        .progress = list(clear = F,
                         name = "Counting positive cells by marker",
                         show_after = 2,
                         type = "iterator"))
      
      #Substitute NA values for 0
      Results[is.na(Results)] <- 0
      #Arrange columns in alphabetic order
      Results <- Results[sort(names(Results))]
      
      #Bind the subject names with the results
      Results <- bind_cols(unique(DATA$Subject_Names), Results)
      names(Results)[1] <- "Subject_Names"
      
      #Calculate the total number of cells
      N_cells <- map_dbl(unique(DATA$Subject_Names), function(Image) {
        nrow(DATA %>% dplyr::filter(Subject_Names == Image))
      })
      
      #Calculate the proportion of cells that are positive
      print("Calculating cell percentages")
      Prop_tibble <- as_tibble(Results[-1] / N_cells)
      names(Prop_tibble) <- str_c("PROP_", names(Prop_tibble))
      
      Results$N_cells <- N_cells
      
      #If required calculate the density and return the results
      if(Calculate_Density){
        #Check arguments
        if(names(DATA_Area)[ncol(DATA_Area)] != "Area"){
          stop("The last column of the DATA_Area must be named Area")
        }
        
        print("Calculating cell densities")
        #Select cell counts
        For_density <- Results %>% dplyr::select(-N_cells, -contains("PROP_"))
        
        #Join the cell counts with the area tibbles
        For_density <- left_join(For_density, DATA_Area, by = "Subject_Names")
        
        #Calculate densities
        Density_results <- as_tibble(For_density[c(2:(ncol(For_density)-1))] / For_density[[ncol(For_density)]])
        names(Density_results) <- str_c("Density_", names(Density_results))
        
        #Bind the results and return the tibble 
        return(bind_cols(Results, Prop_tibble, Density_results, For_density[ncol(For_density)]))
      }
      
      #If not require return only the proportions
      else{
        #Bind the results and return the tibble 
        return(bind_cols(Results, Prop_tibble))
      }
    }, 
    options = list(optimize = 3))

Thresholding_summary_function <- cmpfun(
  function(DATA = NULL, DATA_thresholded = NULL){
    
    #Generate the two basic information sources for this analysis
    Thresholded <- DATA_thresholded[-c(1:4)]
    Markers <- DATA[-c(1:4)]
    
    #Generate two tibbles for dichotomic thresholds
    Thresholded_Dichom <- Thresholded[map_lgl(Thresholded, function(Var) length(unique(Var)) <= 2)]
    Markers_Dichom <- Markers[names(Thresholded_Dichom)]
    #If dichotomic variables are present calculate the thresholds
    if(ncol(Thresholded_Dichom) >= 1){
      #Find thresholds for dichotomic thresholds
      Thresholds_Dichom <- map2_dbl(.x = Markers_Dichom, .y = Thresholded_Dichom, function(.x, .y){
        min(.x[.y])
      }, 
      .progress = list(clear = F,
                       name = "Retrieving Dichotomic marker thresholds",
                       show_after = 2,
                       type = "iterator"))
    }
    
    #Generate two tibbles for Polychotomic thresholds
    Thresholded_Poly <- Thresholded[map_lgl(Thresholded, function(Var) length(unique(Var)) > 2)]
    Markers_Poly <- Markers[names(Thresholded_Poly)]
    #If polychotomic variables are present, calculate the thresholds
    if(ncol(Thresholded_Poly) >= 1){
      #Find thresholds for Polychotomic thresholds an turn it into a tibble 
      Thresholds_Poly <- map2(.x = Markers_Poly, .y = Thresholded_Poly, function(.x, .y){
        map_dbl(unique(.y), function(Level) {
          min(.x[.y == Level])
        })[-1]
      }, 
      .progress = list(clear = F,
                       name = "Retrieving Polychotomic marker thresholds",
                       show_after = 2,
                       type = "iterator"))
      
      #The calculate the maximum amount of levels in the sample
      Max_level_size <- max(map_dbl(Thresholds_Poly, function(Individual) length(Individual)))
      #Generate a tibble with the levels
      Thresholds_Poly <- map_dfc(Thresholds_Poly, function(Individual) {
        c(Individual, rep(NA, times = Max_level_size - length(Individual)))
      })
    }
    
    #Generate the final tibble if only dichotomic variables are present
    if(ncol(Thresholded_Poly) < 1){
      #Generate the result tibble
      Thresholds_results <- matrix(Thresholds_Dichom, byrow = T, ncol = length(Thresholds_Dichom))
      colnames(Thresholds_results) <- names(Thresholds_Dichom)
      Thresholds_results <- as_tibble(Thresholds_results)
      Thresholds_results$Threshold_level <- str_c("Threshold_Level_", 1:nrow(Thresholds_results))
      Thresholds_results <- Thresholds_results[c(ncol(Thresholds_results), 1:(ncol(Thresholds_results)-1))]
      For_histogram <- Thresholds_results %>% pivot_longer(-1)
    }
    #Generate the final tibble if only polychotomic variables are present
    else if (ncol(Thresholded_Dichom) < 1){
      #Generate the result tibble
      Thresholds_results <- Thresholds_Poly
      Thresholds_results$Threshold_level <- str_c("Threshold_Level_", 1:nrow(Thresholds_results))
      Thresholds_results <- Thresholds_results[c(ncol(Thresholds_results), 1:(ncol(Thresholds_results)-1))]
      For_histogram <- Thresholds_results %>% pivot_longer(-1)
    }
    #Generate the final tibble if both type of variables are present
    else if (ncol(Thresholded_Poly) >= 1 & ncol(Thresholded_Dichom) >= 1){
      Thresholds_results <- matrix(Thresholds_Dichom, byrow = T, ncol = length(Thresholds_Dichom))
      colnames(Thresholds_results) <- names(Thresholds_Dichom)
      Thresholds_results <- as_tibble(Thresholds_results)
      Thresholds_results[2:Max_level_size, ] <- NA
      Thresholds_results <- bind_cols(Thresholds_results, Thresholds_Poly)
      Thresholds_results$Threshold_level <- str_c("Threshold_Level_", 1:nrow(Thresholds_results))
      Thresholds_results <- Thresholds_results[c(ncol(Thresholds_results), 1:(ncol(Thresholds_results)-1))]
      For_histogram <- Thresholds_results %>% pivot_longer(-1)
    }
    
    #If not many markers present in the analysis, then print it in the plot window
    if(ncol(Markers) <= 10){
      plot(Markers %>% pivot_longer(1:ncol(Markers)) %>%
             ggplot(aes(x = value)) + facet_wrap(~name, "free", ncol = 1, nrow = ncol(Markers)) +
             geom_histogram(bins = 1000) +
             cowplot::theme_cowplot() +
             geom_vline(aes(xintercept = value), data = For_histogram, color = "red", linewidth = 1.2) +
             scale_x_continuous("Marker intensity") +
             scale_y_discrete("Number of cells")
      )
    }
    #Else directly generate a graph in the wd 
    if(ncol(Markers) > 10) {
      warning(paste0("If the amount of Markers included is more than 10, the resulting graph will be exported to ", getwd()))
      png("Thresholds_for_Markers.png", width = 5000, height = 10000)
      plot(Markers %>% pivot_longer(1:ncol(Markers)) %>%
             ggplot(aes(x = value)) + facet_wrap(~name, "free", ncol = 1, nrow = ncol(Markers)) +
             geom_histogram(bins = 1000) +
             cowplot::theme_cowplot() +
             geom_vline(aes(xintercept = value), data = For_histogram, color = "red", linewidth = 1.2) +
             scale_x_continuous("Marker intensity") +
             scale_y_discrete("Number of cells"))
      dev.off()
    }
    
    return(Thresholds_results)
    
  },
  options = list(optimize = 3))

Thresholding_graphicator_function <- 
  cmpfun(
    function(DATA = NULL,
             DATA_thresholded = NULL,
             Marker_names = NULL,
             Image_name = NULL) {
      #Test the supplied function arguments
      if(!all(c(Image_name %in% DATA$Subject_Names, Image_name %in% DATA_thresholded$Subject_Names))){
        stop("Image_name provided not present in DATA")
      }
      
      if(!all(c(Marker_names %in% names(DATA), Marker_names %in% names(DATA_thresholded)))){
        stop("Marker_names not present in DATA")
      }
      
      #First we need to select our image and our markers
      DATA <- DATA %>% dplyr::filter(Subject_Names == Image_name) %>% dplyr::select(1:4, all_of(Marker_names))
      DATA_thresholded <- DATA_thresholded %>% dplyr::filter(Subject_Names == Image_name) %>% dplyr::select(1:4, all_of(Marker_names))
      
      #Generate the plot of the values by marker
      print("Generating plots")
      PLOT_1 <- 
        DATA %>% pivot_longer(-c(1:4)) %>%
        ggplot(aes(x = X, y = Y, color = value)) +
        scale_colour_gradient("", low = alpha("white", 0), high = alpha("black", 0.95))+
        facet_wrap(~name, ncol = 1, nrow = ncol(DATA)-4, "free") +
        geom_point(size = 1.5) +
        cowplot::theme_cowplot()+
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              legend.text = element_text(size = 10))
      
      #Generate the plot of positive cells
      PLOT_2 <- 
        DATA_thresholded %>% pivot_longer(-c(1:4)) %>% 
        ggplot(aes(x = X, y = Y, color = as.character(value))) +
        scale_colour_manual(values = c(alpha("white", 0), alpha("black", 0.95)))+
        facet_wrap(~name, ncol = 1, nrow = ncol(DATA)-4, "free") +
        geom_point(size = 1.5) +
        cowplot::theme_cowplot()+
        guides(color = "none")+
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
      
      #Combine both plots with patchwork
      patchwork::wrap_plots(PLOT_1, PLOT_2, ncol = 2)
    },
    options = list(optimize = 3))

############STEP 3 - PHENOTYPING - REQUIRED FUNCTIONS###########
message("Importing functions: STEP 3 - PHENOTYPING")
Marker_combinator_generator <- cmpfun(
  function(DATA = NULL, Markers = NULL) {
    
    #Check arguments
    if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))){
      stop("DATA not correctly specified, please format appropiatetly (see Step 0)")
    }
    if(!all(Markers %in% names(DATA))){
      Missing_value <- Markers[!Markers %in% names(DATA)]
      stop(paste0("The following marker is not present in the DATA: ", str_c(Missing_value, collapse = ", ")))
    }
    if(!all(map_lgl(DATA %>% dplyr::select(all_of(Markers)), function(Var) {
      is.logical(Var) | length(unique(Var)) <= 10 
    }))) stop("Markers supplied should be logical (Generated after thresholding) or Multi-level with less than 10 Levels")
    
    print("Generating marker combination possibilities")
    Pheno_patterns <- DATA %>% dplyr::select(all_of(Markers)) %>% distinct()
    Pheno_patterns <-  Pheno_patterns %>% dplyr::mutate(Pattern = apply(Pheno_patterns, MARGIN = 1, function(x) str_c(x, collapse = "_")))
    
    print("Counting the different possibilities")
    Pattern_count <- DATA %>% mutate(Pattern = apply((DATA %>% dplyr::select(all_of(Markers))), MARGIN = 1, function(x) str_c(x, collapse = "_"))) %>% 
      group_by(Pattern) %>% dplyr::count()
    
    left_join(Pheno_patterns, Pattern_count, by = "Pattern") %>% dplyr::arrange(desc(n)) %>% dplyr::rename(Number_of_cells = n) %>% 
      mutate(Phenotype_no = str_c("Cell_type_", as.character(1:nrow(Pheno_patterns))))
  }, 
  options = list(optmize = 3))

Phenotype_assigner_function <-
  cmpfun(function(DATA = NULL, Phenotype_possibilities = NULL) {
    if(sum(names(Phenotype_possibilities)[1:(ncol(Phenotype_possibilities)-4)] %in% names(DATA)) != length(names(Phenotype_possibilities)[1:(ncol(Phenotype_possibilities)-4)])) {
      stop("Phenotype markers not matched in thresholded data")
    }
    
    else if(!identical(names(Phenotype_possibilities)[(ncol(Phenotype_possibilities)-3):ncol(Phenotype_possibilities)],
                       c("Pattern",  "Number_of_cells", "Phenotype_no", "Phenotype"))) {
      stop("Phenotype_possibilities not correctly specified. Please use marker_combinator_generator and specify phenotypes in a column named: 'Phenotype'")
    }
    
    else {
      print("Performing phenotype assignment")
      Interim <- DATA %>% mutate(Pattern = apply((DATA %>% dplyr::select(all_of(names(Phenotype_possibilities)[1:(ncol(Phenotype_possibilities)-4)]))), 
                                                 MARGIN = 1, 
                                                 function(x) str_c(x, collapse = "_")))
      Phenotypes <- Phenotype_possibilities %>% dplyr::select(Pattern, Phenotype)
      left_join(Interim, Phenotypes, by = "Pattern") %>% dplyr::select(-Pattern)
    }
  },
  options = list(optimze = 3))

Phenotype_quantifier <- cmpfun(
  function(DATA = NULL,
           Calculate_Density = NULL,
           DATA_Area = NULL) {
    
    #Check arguments
    if(!is.logical(Calculate_Density)){
      stop("Calculate_Density must be a logical value")
    }
    
    #Obtain the number of cells by image according to the phenotypes
    Results <- DATA %>% group_by(Subject_Names, Phenotype) %>% dplyr::count() %>% ungroup() %>% pivot_wider(names_from = Phenotype, values_from = n)
    Results[is.na(Results)] <- 0
    
    #Calculate the number of total cells
    N_cells <- apply(Results[-1], MARGIN = 1, sum)
    
    #Calculate proportions
    Prop_tibble <- as_tibble(Results[-1] / N_cells)
    names(Prop_tibble) <- str_c("PROP_", names(Prop_tibble))
    Results$N_cells <- N_cells
    
    if(Calculate_Density){
      #Check arguments
      if(names(DATA_Area)[ncol(DATA_Area)] != "Area"){
        stop("The last column of the DATA_Area must be named Area")
      }
      
      #Select cell counts
      For_density <- Results %>% dplyr::select(-N_cells, -contains("PROP_"))
      
      #Join the cell counts with the area tibbles
      For_density <- left_join(For_density, DATA_Area, by = "Subject_Names")
      
      #Calculate densities
      Density_results <- as_tibble(For_density[c(2:(ncol(For_density)-1))] / For_density[[ncol(For_density)]])
      names(Density_results) <- str_c("Density_", names(Density_results))
      
      #Bind the results and return the tibble 
      return(bind_cols(Results, Prop_tibble, Density_results, For_density[ncol(For_density)]))
    }
    
    else{
      #Return the bind results
      return(bind_cols(Results, Prop_tibble))
    }
  },
  options = list(optimize = 3))

Barplot_generator <- 
  cmpfun(function(DATA = NULL, Phenotypes_included = NULL, Ordering_phenotype = NULL) {
    if(!all(Phenotypes_included %in% names(DATA))){
      stop(paste0("Phenotypes not correctly stated. Choose from ", str_c(names(DATA)[-1], collapse = ", ")))
    }
    
    else if(!all(Ordering_phenotype %in% Phenotypes_included)) {
      stop(paste0("Ordering phenotypes not correctly stated. Choose from ", str_c(names(DATA)[-1], collapse = ", ")))
    }
    
    else {
      
      DATA <- DATA  %>% dplyr::select(Subject_Names, all_of(Phenotypes_included))
      DATA <- bind_cols(DATA[1],
                        (DATA[-1]/rowSums(DATA[-1]))*100)
      DATA <- DATA[order(DATA[[which(names(DATA) == Ordering_phenotype)]],decreasing=TRUE),]
      DATA <- DATA %>% mutate(Subject_Names = factor(Subject_Names, levels = DATA$Subject_Names))
      
      DATA %>% pivot_longer(-1) %>%
        ggplot(aes(x = Subject_Names, y = value, fill = name)) + geom_col(position = "fill", color = "black", width = 0.5) +
        cowplot::theme_cowplot() +
        scale_x_discrete("Image") +
        scale_y_continuous("%")+
        scale_fill_manual("Cell Type", values = unname(pals::polychrome(ncol(DATA)-1)))+
        theme(axis.text.x = element_text(angle = -90, vjust = 0.5, size = 8, color = "black"),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 20))
    }
    
  },
  options = list(optimize = 3))

Image_plotter <- cmpfun(function(DATA = NULL, Image_name = NULL, Phenotypes_included = NULL) {
  #Check arguments
  if(!Image_name %in% DATA$Subject_Names){
    stop("Image_name not present in Subject_Names")
  }
  if(!all(Phenotypes_included %in% unique(DATA$Phenotype))){
    Missing_phenotypes <- Phenotypes_included[!Phenotypes_included %in% unique(DATA$Phenotype)]
    stop(paste0(str_c(Missing_phenotypes, collapse = ", "), " not present in the phenotypes of DATA"))
  }
  
  DATA %>% dplyr::select(Subject_Names, X, Y, Phenotype) %>% dplyr::filter(Subject_Names == Image_name, Phenotype %in% Phenotypes_included) %>%
    ggplot(aes(x = X, y = Y, color = Phenotype)) +
    geom_point(size = 2, alpha = 0.95)+
    cowplot::theme_cowplot()+
    scale_x_continuous("", labels = NULL) +
    scale_y_continuous("", labels = NULL) +
    scale_color_manual("Cell Type", values = unname(pals::polychrome(length(Phenotypes_included))))+
    
    guides(color = guide_legend(override.aes = list(size = 12)))+
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20))
},
options = list(optimize = 3))

Clustering_Phenotyper <- cmpfun(
  function(DATA = NULL,
           Strategy = NULL,
           
           #Denoising parameters
           Apply_Denoise = NULL, #Specify if a denoising filtering is required before clustering
           Denoising = NULL, #Select denoising strategy from: Quantile, Standard_Deviation, Threshold, Otsu or DimRed_DBscan
           Percentile = NULL, #Select the adequate percentile for quantile threshold
           N_Standard_Deviations = NULL, #Select the number of standard deviations from mean for Standard_Deviation method
           Selected_threshold = NULL, #Select the absolute threshold for the Threshold method
           Min_cell_no = NULL, #Parameter for DBscan
           Distance_radius = NULL, #Parameter for DBscan
           
           #Dimension reduction
           Perform_Dimension_reduction = NULL,
           Dimension_reduction = NULL,
           Dimension_reduction_prop = NULL,
           Cluster_on_Reduced = NULL,
           
           #Parameters for Consensus Clustering
           Max_N_phenotypes = NULL, #Number of maximum neighborhods that you desire to find
           Consensus_reps = NULL, #Number of iterations of the algorithm to try to converge
           Consensus_p_Items = NULL, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
           Consensus_Cluster_Alg = NULL, #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
           Consensus_Distance = NULL, #Distance metric to be used (pearson(1 - Pearson correlation), spearman(1 - Spearman correlation), euclidean, binary, maximum, canberra, minkowski
           Consensus_Name = NULL, #Name of the folder that is going to be created in order to place the resulting graphs
           
           #Parameters for Self-Organizing Maps
           Max_SOM_phenotypes = NULL, #Maximum number of clusters (phenotypes) to try in the algorithm
           
           #Parameters for Graph-Based approaches
           Nearest_neighbors_for_graph = NULL, #Specify the number of closest neighbors to calculate the graph
           Graph_Method = NULL, #Specify the clustering method
           Graph_Resolution = NULL, #Specify the graph resolution
           N_steps = NULL, #Number of steps given in the WalkTrap algorithm
           
           #Parameters for K means Meta Clustering
           N_K_centroids = NULL, #Number of centroids to perform K means
           Max_N_phenotypes_Meta = NULL, #Number of maximum clusters (phenotypes) that you desire to find
           Consensus_reps_Meta = NULL, #Number of iterations of the algorithm to try to converge
           Consensus_p_Items_Meta = NULL, #Percentage of cells that you desire to sample in each iteration
           Consensus_Name_Meta = NULL, #Name of the folder that is going to be created in order to place the resulting graphs
           
           #Parameters for Batched K means
           Batch_size = NULL, #The number of cells to be included in each random batch
           Max_N_phenotypes_Batch = NULL, #Number of maximum clusters (phenotypes) that you desire to find
           N_initiations = NULL, #Number of times the algorithm is going to be tried to find the best clustering result
           Max_iterations = NULL, #Max number of iterations in each try
           
           #Parameters for Gaussian Mixture Model
           Quality_metric = NULL, #The quality measure used to test the number of clusters ("AIC" or "BIC")
           Max_N_phenotypes_GMM = NULL, #Number of maximum clusters (phenotypes) that you desire to find
           Max_iterations_km = NULL, #Number of max iterations in the K means clustering performed
           Max_iterations_em = NULL, #Number of max iterations in the Expectation Maximization algorithm
           GMM_Distance = NULL, #Distance metric to use in the model ("eucl_dist" or "maha_dist")
           
           #Parameters for CLARA clustering
           Samples_CLARA = NULL, #Number of samples the CLARA algorithm is going to use to be calculated
           Sample_per_CLARA = NULL, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
           Max_N_phenotypes_CLARA = NULL, #Number of maximum clusters (phenotypes) that you desire to find
           Distance_CLARA = NULL, #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
           #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
           N_cores = NULL #Number of cores to parallelize your computation
  ) {
    #Check arguments
    if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))) {
      stop("Please generate an appropiate data object using the Data_arrange_function")
    }
    if(!Strategy %in% c("Consensus_Clustering", "SOM", "Graph_Based", "K_Means_Meta_clustering", "Batch_K_means", "GMM", "CLARA_clustering")){
      stop("Strategy must be one of the following: Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering")
    }
    if(!is.logical(Apply_Denoise)) stop("Apply_Denoise must be a logical value")
    if(!is.logical(Perform_Dimension_reduction)) stop("Perform_Dimension_reduction must be a logical value")
    if(Perform_Dimension_reduction){
      if(!Dimension_reduction %in% c("UMAP", "TSNE", "PCA")) stop("Dimension_reduction must be one of the following: UMAP, TSNE, PCA")
      if(!all(is.numeric(Dimension_reduction_prop), Dimension_reduction_prop > 0, Dimension_reduction_prop <= 1)) stop("Dimension_reduction_prop must be a numeric value between 0 and 1")
    }
    if(!is.logical(Cluster_on_Reduced)) stop("Cluster_on_Reduced must be a logical value")
    if(Cluster_on_Reduced){
      if(!Perform_Dimension_reduction) stop("If Clustering needst o be performed on Dimension reduced data please set Perform_Dimension_reduction to TRUE")
    }
    if(Strategy == "Consensus_Clustering"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Max_N_phenotypes_OK = (Max_N_phenotypes >= 2 & Max_N_phenotypes%%1 == 0),
                            Consensus_reps_OK = (Consensus_reps >= 1 & Consensus_reps%%1 == 0),
                            Consensus_p_Items_OK = (Consensus_p_Items > 0 & Consensus_p_Items <= 1),
                            Consensus_Cluster_Alg_OK = Consensus_Cluster_Alg %in% c("hc", "pam", "km"),
                            Consensus_Distance_OK = Consensus_Distance %in% c("pearson", "spearman", "euclidean", "binary", "maximum", "canberra", "minkowski"),
                            Consensus_Name_OK = is.character(as.character(Consensus_Name))
      )
      Stop_messages <- c(Max_N_phenotypes_OK = "Max_N_Phenotypes must be an integer value > 1",
                         Consensus_reps_OK = "Consensus_reps_OK must be an integer value > 0",
                         Consensus_p_Items_OK = "Consensus_p_Items must be a numeric value > 0 and lower than 1",
                         Consensus_Cluster_Alg_OK = "Consensus_Cluster_Alg must be one of the following: hc, pam, km",
                         Consensus_Distance_OK = "Consensus_Distance must be one the following: pearson, spearman, euclidean, binary, maximum, canberra, minkowski",
                         Consensus_Name_OK = "Consensus_Name must ve a character value")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "SOM"){
      #Check arguments
      if(!(Max_SOM_phenotypes > 1 & Max_SOM_phenotypes%%1 == 0)) stop("Max_SOM_phenotypes must be an integer value > 1")
    }
    if(Strategy == "Graph_Based"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Nearest_neighbors_for_graph_OK = (Nearest_neighbors_for_graph >= 1 & Nearest_neighbors_for_graph%%1 == 0),
                            Graph_Method_OK = Graph_Method %in% c("Louvain", "Leiden", "Greedy", "WalkTrap", "Spinglass", "Leading_Eigen", "Edge_Betweenness"),
                            Graph_Resolution_OK = all(is.numeric(Graph_Resolution), Graph_Resolution > 0),
                            N_steps_OK = is.null(N_steps) || (N_steps >=1 & N_steps%%1 == 0)
                            
      )
      Stop_messages <- c(Nearest_neighbors_for_graph = "Nearest_neighbors_for_graph must be an integer value > 0",
                         Graph_Method = "Graph_Method must be one of the following: Louvain, Leiden, Greedy, WalkTrap, Spinglass, Leading_Eigen, Edge_Betweenness",
                         Graph_Resolution = "Graph_Resolution must be a numeric value > 0",
                         N_steps = "N_steps must be a integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "K_Means_Meta_clustering"){
      #Check arguments
      Argument_checker <- c(N_K_centroids_OK = all(nrow(MARKERS) > N_K_centroids, N_K_centroids%%1 == 0, N_K_centroids > 0),
                            Max_N_phenotypes_Meta_OK = (Max_N_phenotypes_Meta >= 2 & Max_N_phenotypes_Meta%%1 == 0),
                            Consensus_reps_Meta_OK = (Consensus_reps_Meta >= 1 & Consensus_reps_Meta%%1 == 0),
                            Consensus_p_Items_Meta_OK = (Consensus_p_Items_Meta > 0 & Consensus_p_Items_Meta <= 1),
                            Consensus_Name_Meta_OK = is.character(as.character(Consensus_Name_Meta))
      )
      Stop_messages <- c(N_K_centroids_OK = "N_K_centroids must be smaller than the number of cells in DATA and a integer value > 0",
                         Max_N_phenotypes_Meta_OK = "Max_N_phenotypes_Meta must be an integer value > 1",
                         Consensus_reps_Meta_OK = "Consensus_reps_Meta must be an integer value > 0",
                         Consensus_p_Items_Meta_OK = "Consensus_p_Items_Meta must be a numeric value > 0 and lower than 1",
                         Consensus_Name_Meta_OK = "Consensus_Name_Meta must ve a character value"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "Batch_K_means"){
      #Check arguments
      Argument_checker <- c(Max_N_phenotypes_Batch_OK = (Max_N_phenotypes_Batch >= 2 & Max_N_phenotypes_Batch%%1 == 0),
                            N_initiations_OK = (N_initiations >= 1 & N_initiations%%1 == 0),
                            Max_iterations_OK = (Max_iterations%%1 == 0)
      )
      Stop_messages <- c(Max_N_phenotypes_Batch_OK = "Max_N_phenotypes_Batch must be an integer value > 1",
                         N_initiations_OK = "N_initiations must be an integer value > 0",
                         Max_iterations_OK = "Max_iterations must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "GMM"){
      #Check arguments
      Argument_checker <- c(Quality_metric_OK = Quality_metric %in% c("AIC", "BIC"),
                            Max_N_phenotypes_GMM_OK = (Max_N_phenotypes_GMM >= 2 & Max_N_phenotypes_GMM%%1 == 0),
                            Max_iterations_km_OK = (Max_iterations_km >= 1 & Max_iterations_km%%1 == 0),
                            Max_iterations_em_OK = (Max_iterations_em >= 1 & Max_iterations_em%%1 == 0),
                            GMM_Distance_OK = GMM_Distance %in% c("eucl_dist", "maha_dist")
      )
      Stop_messages <- c(Quality_metric_OK = "Quality_metric must be one of the following: AIC, BIC",
                         Max_N_phenotypes_GMM_OK = "Max_N_phenotypes must be an integer value > 1",
                         Max_iterations_km_OK = "Max_iterations_km must be an integer value > 1",
                         Max_iterations_em_OK = "Max_iterations_em must be an integer value > 1",
                         GMM_Distance_OK = "GMM_Distance must be one of the following: eucl_dist, maha_dist"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "CLARA_clustering"){
      #Check arguments
      Argument_checker <- c(Samples_CLARA_OK = (Samples_CLARA >= 1 & Samples_CLARA%%1 == 0),
                            Sample_per_CLARA_OK = (Sample_per_CLARA > 0 & Sample_per_CLARA <= 1),
                            Max_N_phenotypes_CLARA_OK = (Max_N_phenotypes_CLARA >= 2 & Max_N_phenotypes_CLARA%%1 == 0),
                            Distance_CLARA_OK = Distance_CLARA %in% c("euclidean", "manhattan", "chebyshev", "canberra", "braycurtis", 
                                                                      "pearson_correlation", "simple_matching_coefficient", "minkowski", 
                                                                      "hamming", "jaccard_coefficient", "Rao_coefficient", "mahalanobis", "cosine"),
                            N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0)
      )
      Stop_messages <- c(Samples_CLARA_OK = "Samples_CLARA must be an integer value > 0",
                         Sample_per_CLARA_OK = "Sample_per_CLARA must be a numeric value between 0 and 1",
                         Max_N_phenotypes_CLARA_OK = "Max_N_phenotypes_CLARA must be an integer value > 1",
                         Distance_CLARA_OK = "Distance_CLARA must be one of the following: euclidean, manhattan, chebyshev, canberra, braycurtis, 
                                                                     pearson_correlation, simple_matching_coefficient, minkowski, 
                                                                     hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine",
                         N_cores_OK = "N_cores must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    
    #Perform dimension reduction if required
    if(Perform_Dimension_reduction){
      #First PCA 
      if(Dimension_reduction == "PCA"){
        if(Dimension_reduction_prop != 1) stop("PCA must be performed using Dimension_reduction_prop = 1")
        print("Generating PCA projections")
        #Scale and turn into matrix
        DATA_matrix <- DATA %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
        Result_PCA <- svd::propack.svd(DATA_matrix, neig = 2)$u
        DATA_Reduction <- tibble(Cell_no = DATA$Cell_no, DIMENSION_1 = unlist(Result_PCA[,1]), DIMENSION_2 = unlist(Result_PCA[,2]))
      }
      
      #Second TSNE
      if(Dimension_reduction == "TSNE"){
        if(Dimension_reduction_prop == 1) {
          print("Generating TSNE projections")
          if(nrow(DATA) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          DATA_matrix <- DATA %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = TRUE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Result_TSNE[,1]), DIMENSION_2 = unlist(Result_TSNE[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating TSNE projections")
          DATA_matrix <- DATA %>% dplyr::group_by(Subject_Names) %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% dplyr::ungroup() %>%
            dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = FALSE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          Coords <- snifter::project(Result_TSNE, 
                                     new = DATA  %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix(), 
                                     old = DATA_matrix)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
      
      #Third UMAP
      if(Dimension_reduction == "UMAP"){
        if(Dimension_reduction_prop == 1) {
          print("Generating UMAP projections")
          if(nrow(DATA) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take some time")
          #scale and turn into matrix
          DATA_matrix <- DATA %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Result_UMAP[,1]), DIMENSION_2 = unlist(Result_UMAP[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating UMAP projections")
          DATA_matrix <- DATA %>% dplyr::group_by(Subject_Names) %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% dplyr::ungroup() %>%
            dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take aome time")
          #scale and turn into matrix
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L, ret_model = TRUE)
          Coords <- uwot::umap_transform(X = DATA  %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix(), 
                                         model = Result_UMAP)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
    }
    
    #If denoising is required apply required function
    if(Apply_Denoise){
      #check denoising argument is correctly stated
      if(!Denoising %in% c("Quantile", "Standard_Deviation", "Threshold", "Otsu", "DimRed_DBscan")) {
        stop("Denoising should be one of Quantile, Standard_Deviation, Threshold, Otsu, DimRed_DBscan")
      }
      
      print("Filtering out noisy cells")
      #Identify each cell in the experiment with a unique ID
      DATA <- DATA %>% mutate(Unique_ID = 1:nrow(DATA))
      DATA <- DATA[c(ncol(DATA), 1:(ncol(DATA)-1))]
      
      #Apply desired filters
      if(Denoising == "Quantile") {
        #Check arguments
        if(Percentile < 0.01 | Percentile > 0.99) stop("Percentile must be between 0.01 and 0.99")
        
        FILTER <- map_dfc(DATA[-(1:5)], function(x){
          x <= quantile(x, Percentile)
        })
      }
      
      else if(Denoising == "Standard_Deviation"){
        #Check arguments
        if(!is.numeric(N_Standard_Deviations)) stop("N_Standard_Deviations must be a numeric value")
        
        FILTER <- map_dfc(DATA[-(1:5)], function(x){
          x <= (mean(x) - (N_Standard_Deviations*sd(x)))
        })
      }
      
      else if(Denoising == "Threshold"){
        #Check arguments
        if(!is.numeric(Selected_threshold)) stop("Selected_threshold must be a numeric value")
        
        FILTER <- map_dfc(DATA[-(1:5)], function(x){
          x <= Selected_threshold
        })
      }
      
      else if(Denoising == "Otsu"){
        FILTER <- map2_df(.x = DATA[-c(1:5)],
                          .y = map_dbl(DATA[-c(1:5)], function(z){
                            EBImage::otsu(array(z, dim = c(1, length(z))), range = c(min(z), max(z)), levels = length(unique(z)))
                          }),
                          function(.x, .y) .x <= .y)
      }
      
      else if(Denoising == "DimRed_DBscan"){
        #Requires previous dimension reduction
        if(!Perform_Dimension_reduction) stop("DBscan clustering requires Dimension reduction. Please set Perform_Dimension_reduction to TRUE")
        #Check other arguments
        if(!all(is.numeric(Min_cell_no), Min_cell_no%%1 == 0, Min_cell_no > 0)) stop("Min_cell_no must be an integer value > 0")
        if(!all(is.numeric(Distance_radius), Distance_radius > 0)) stop("Distance_radius must be a numeric value > 0")
        
        #Proceed with algorithm 
        DB_results <- dbscan::dbscan(DATA_Reduction[c("DIMENSION_1", "DIMENSION_2")], eps = Distance_radius, minPts = Min_cell_no, borderPoints = FALSE)
        
        #whole plot for small samples
        if(length(DB_results$cluster) <= 100000){
          plot(
            tibble(Dim_1 = DATA_Reduction[["DIMENSION_1"]], Dim_2 = DATA_Reduction[["DIMENSION_2"]], Cluster = DB_results$cluster) %>%
              mutate(Cluster = case_when(Cluster == 0 ~ "Noise",
                                         TRUE ~ "Approved")) %>%
              ggplot(aes(x = Dim_1, y = Dim_2, color = Cluster)) + geom_point(size = 0.8) +
              scale_color_manual("", values = c("black", "grey"))+
              cowplot::theme_cowplot()+
              theme(panel.grid = element_blank())
          )
        }
        
        #Subsample plot for large dataset
        if(length(DB_results$cluster) > 100000){
          message(">100K observations to generate plots. A random subset containing 10% of the dataset will be selected for Dimension reduction plots")
          plot(
            tibble(Dim_1 = DATA_Reduction[["DIMENSION_1"]], Dim_2 = DATA_Reduction[["DIMENSION_2"]], Cluster = DB_results$cluster) %>%
              mutate(Cluster = case_when(Cluster == 0 ~ "Noise",
                                         TRUE ~ "Approved")) %>%  
              dplyr::sample_n(size = 100000) %>%
              ggplot(aes(x = Dim_1, y = Dim_2, color = Cluster)) + geom_point(size = 1.5) +
              scale_color_manual("", values = c("black", "grey"))+
              cowplot::theme_cowplot()+
              theme(panel.grid = element_blank())
          )
        }
        
        DB_OK <- menu(choices = c("Proceed", "Abort"), title = "Are the results of the filtering OK?")
        if(DB_OK == 2) stop("Please refine Distance_radius and Min_cell_no parameters and retry")
        
        #Generate the NOISE column with a logical vector
        DATA <- DATA %>% mutate(NOISE = DB_results$cluster) %>% mutate(NOISE = case_when(NOISE == 0 ~ TRUE,
                                                                                         NOISE != 0 ~ FALSE))
      }
      
      #For no DBscan methods apply a row wise method to determine which cells are noise (a logical vector)
      if(Denoising != "DimRed_DBscan"){
        #Generate the variable to specify if the cell is noise or not
        DATA <- DATA %>% mutate(NOISE = unlist(apply(FILTER, MARGIN = 1, function(x) sum(x) == ncol(FILTER))))
      }
      
      #Generate two tibbles, one with noisy cells and other (DATA) with the actual cells
      NOISE_VECTOR <- DATA[["NOISE"]] #We generate a NOISE_VECTOR if we require to filter noise cells for Dimension reduction methods
      DATA_NOISE <- DATA %>% dplyr::filter(NOISE) %>% mutate(Phenotype = 1)
      DATA <- DATA %>% dplyr::filter(!NOISE)
      MARKERS <- DATA %>% dplyr::select(-Unique_ID, -Cell_no, -X, -Y, -Subject_Names, -NOISE)
    }
    
    #If no denoising required obtain MARKERS directly from DATA
    else{
      DATA <- DATA
      MARKERS <- DATA %>% dplyr::select(-Cell_no, -X, -Y, -Subject_Names)
    }
    
    #Generate a specific version of Markers with dimension reduction data for Dimension_SNN
    if(Cluster_on_Reduced){
      #Depending on Denoising Obtain directly from DATA_Reduction or filter first
      if(!Apply_Denoise) MARKERS <- DATA_Reduction[-"Cell_no"]
      if(Apply_Denoise) MARKERS <- DATA_Reduction[!NOISE_VECTOR, ] %>% dplyr::select(-Cell_no)
    }
    
    #Check that MARKERS is a numeric matrix
    if(!is.numeric(as.matrix(MARKERS))) stop("DATA provided must contain marker intensity values")
    
    #Continue with clustering strategies
    if(Strategy == "Consensus_Clustering"){
      #Perform consensus clustering
      Phenotype_result <- try(ConsensusClusterPlus::ConsensusClusterPlus(t(as.matrix((MARKERS %>% scale()))), 
                                                                         maxK = Max_N_phenotypes, 
                                                                         reps = Consensus_reps,
                                                                         pItem = Consensus_p_Items,
                                                                         pFeature = 1,
                                                                         title = Consensus_Name,
                                                                         clusterAlg = Consensus_Cluster_Alg,
                                                                         distance = Consensus_Distance,
                                                                         plot = "png",
                                                                         verbose = T)
      )
      #Test if consensus clustering returned an error
      if(berryFunctions::is.error(Phenotype_result)) {
        stop("Data is too large for Consensus Clustering. Please try another strategy")
      }
      else {
        #Make the user decide the number of neighborhoods according to results
        N_Phenotypes<- menu(choices = as.character(1:Max_N_phenotypes), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Phenotypes"))
        DATA_Phenotypes <- DATA %>% mutate(Phenotype = Phenotype_result[[as.double(N_Phenotypes)]][["consensusClass"]])
      }
    }
    
    else if(Strategy == "SOM"){
      print("Executing Self Organizing Map algorithm")
      #Transform data into a scaled matrix and perform Self Organizing Map
      SOM_results <- try(FlowSOM::FlowSOM(MARKERS %>% scale() %>% as.matrix(),
                                          scale = F,
                                          colsToUse = c(1:ncol(MARKERS)),
                                          maxMeta = Max_SOM_phenotypes,#To find optimal meta clusters
                                          silent = F,
                                          seed = 21)
      )
      #Test if SOM returned an error
      if(berryFunctions::is.error(SOM_results)) {
        stop("Data is too large for Self-Organizing Maps. Please try another strategy")
      }
      else{
        #Assign phenotypes to each cell
        DATA_Phenotypes <- DATA %>% mutate(Phenotype = FlowSOM::GetMetaclusters(SOM_results))
      }
      
    }
    
    else if(Strategy == "Graph_Based"){
      print("Start Graph building process")
      
      #Transform data into a nearest neighbor graph 
      SNN_graph <- try(bluster::makeSNNGraph(MARKERS %>% scale() %>% as.matrix(), 
                                             k = Nearest_neighbors_for_graph)
      )
      
      #Test if Graph construction process returned an error
      if(berryFunctions::is.error(SNN_graph)) {
        stop("Data is too large to build a graph. Please try another strategy")
      }
      else{
        print("Performing Graph-based clustering")
        #Go for graph clustering
        #Cluster the graph with louvain or leiden clustering
        if(Graph_Method == "Louvain") {
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_louvain(SNN_graph,
                                                                                 weights = NULL,
                                                                                 resolution = Graph_Resolution)$membership)
          
        }
        
        else if(Graph_Method == "Leiden") {
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_leiden(SNN_graph,
                                                                                objective_function = "modularity",
                                                                                weights = NULL,
                                                                                resolution = Graph_Resolution,
                                                                                beta = 0.01,
                                                                                initial_membership = NULL,
                                                                                n_iterations = 100,
                                                                                vertex_weights = NULL)$membership)
          
        }
        
        else if(Graph_Method == "Greedy"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_fast_greedy(SNN_graph)$membership)
        }
        
        else if(Graph_Method == "WalkTrap"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_walktrap(SNN_graph,
                                                                                  steps = N_steps,
                                                                                  membership = T)$membership)
        }
        
        else if (Graph_Method == "Spinglass") {
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_spinglass(SNN_graph,
                                                                                   weights = NULL,
                                                                                   vertex = NULL,
                                                                                   spins = 25,
                                                                                   parupdate = FALSE,
                                                                                   start.temp = 1,
                                                                                   stop.temp = 0.01,
                                                                                   cool.fact = 0.99,
                                                                                   update.rule = c("config", "random", "simple"),
                                                                                   gamma = 1,
                                                                                   implementation = c("orig", "neg"),
                                                                                   gamma.minus = 1)$membership)
          
        }
        
        else if(Graph_Method == "Leading_Eigen"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_leading_eigen(SNN_graph,
                                                                                       membership = T)$membership)
        }
        
        else if(Graph_Method == "Edge_Betweenness"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_edge_betweenness(SNN_graph,
                                                                                          weights = NULL,
                                                                                          directed = FALSE,
                                                                                          edge.betweenness = FALSE,
                                                                                          merges = FALSE,
                                                                                          bridges = FALSE,
                                                                                          modularity = FALSE,
                                                                                          membership = TRUE)$membership)
          
        }
      }
      
    }
    
    else if(Strategy == "K_Means_Meta_clustering"){
      #First we need to perform K means Clustering 
      print("Performing initial K-means algorithm")
      cl <- try(stats::kmeans(MARKERS %>% scale() %>% as.matrix(), #Scale it and turn it into a matrix
                              centers = N_K_centroids, #Number of centroids to be calculated
                              iter.max = 50, 
                              nstart = 10))
      
      #Stop function if K means returned an error
      if(berryFunctions::is.error(cl)) {
        stop("Data is too large for K means clustering. Please try another strategy")
      }
      #Proceed if no error was returned
      else{
        #Assign this K means cluster to each observation
        DATA_filter_Markers <- DATA %>% mutate(K_means_Cl = cl$cluster)
        
        #Prepare data for Meta-Clustering
        #Create a tibble with the K means centroids and the format it for Consensus clustering
        K_medoids <- as_tibble(cl$centers) %>% mutate(K_means_Cl = 1:nrow(as_tibble(cl$centers)))
        tK_medoids <- K_medoids %>% dplyr::select(-K_means_Cl) %>% as.matrix() %>% t()
        
        #Perform Consensus clustering with hierarchical clustering
        print("Perorming Consensus Clustering")
        HC <- try(ConsensusClusterPlus::ConsensusClusterPlus(tK_medoids,
                                                             maxK = Max_N_phenotypes_Meta,   
                                                             reps = Consensus_reps_Meta, 
                                                             pItem = Consensus_p_Items_Meta,
                                                             pFeature = 1,
                                                             title = Consensus_Name_Meta,
                                                             distance = "euclidean",
                                                             clusterAlg = "pam",
                                                             plot = "png",
                                                             verbose = T)
        )
        
        #Test if consensus clustering returned an error
        if(berryFunctions::is.error(HC)) {
          stop("Data is too large for Meta Clustering. Please try another strategy or select a smaller N_K_centroids value")
        }
        else {
          #Make the user decide the number of neighborhoods according to results
          N_Phenotypes<- menu(choices = as.character(1:Max_N_phenotypes_Meta), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Phenotypes"))
          
          #Bind the final Phenotype to the K medoids tibble
          K_medoids <- K_medoids %>% mutate(Phenotype = HC[[as.double(N_Phenotypes)]][["consensusClass"]])
          K_medoids_for_join <- K_medoids %>% dplyr::select(K_means_Cl, Phenotype)
          
          #Bind The DATA and the K_meoids to obtain the final matrix
          DATA_Phenotypes <- left_join(DATA_filter_Markers, K_medoids_for_join, by = "K_means_Cl") %>% dplyr::select(-K_means_Cl)
        }
      } 
      
    }
    
    else if(Strategy == "Batch_K_means"){
      #First we calculate a metric to decide the number of total phenotypes
      #Specify the params
      if(Batch_size > nrow(MARKERS)) stop(paste0("Batch_size cannot be larger than ", nrow(MARKERS)))
      
      params_mbkm <- list(batch_size = Batch_size, 
                          init_fraction = 1, 
                          early_stop_iter = 10)
      #Run the specified test using each of the number of clusters
      print("Starting Cluster number stimation process")
      Optimal <- try(ClusterR::Optimal_Clusters_KMeans(MARKERS %>% scale(),
                                                       max_clusters = Max_N_phenotypes_Batch, 
                                                       num_init = N_initiations, 
                                                       max_iters = Max_iterations, 
                                                       initializer = "kmeans++",
                                                       criterion = "Adjusted_Rsquared",
                                                       plot_clusters = T, 
                                                       mini_batch_params = params_mbkm,
                                                       verbose = T)
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_phenotypes_Batch), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Phenotypes"))
        
        #Calculate the desired number of clusters with batch k menas
        print("Performing Batched K means algorithm")
        Batch_k_means <- ClusterR::MiniBatchKmeans(MARKERS %>% scale(),
                                                   clusters = as.double(N_Phenotypes), 
                                                   batch_size = Batch_size,
                                                   num_init = N_initiations,
                                                   max_iters = Max_iterations,
                                                   init_fraction = 1,
                                                   initializer = "kmeans++",
                                                   early_stop_iter = 10,
                                                   verbose = T,
                                                   tol = 1e-07, #The required improvement rate to continue with the iterations (the lower the more iterations will be required)
                                                   CENTROIDS = NULL,
                                                   seed = 21)
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = Batch_k_means, fuzzy = F, newdata = MARKERS %>% scale()) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Phenotype"
        
        #Generate the data phenotypes tibble
        DATA_Phenotypes <- bind_cols(DATA, pr_mb)
      }
      
      
    }
    
    else if(Strategy == "GMM"){
      #First we calculate a metric to decide the number of total phenotypes
      #Run the specified test using each of the number of clusters
      print("Starting Cluster number stimation process")
      Optimal <- try(ClusterR::Optimal_Clusters_GMM(MARKERS %>% scale(),
                                                    criterion = Quality_metric,
                                                    max_clusters = Max_N_phenotypes_GMM, 
                                                    dist_mode = GMM_Distance,
                                                    seed_mode = "random_subset",
                                                    km_iter = Max_iterations_km,
                                                    em_iter = Max_iterations_em,
                                                    verbose = TRUE,
                                                    var_floor = 1e-10,
                                                    plot_data = TRUE)
                     
                     
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_phenotypes_GMM), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Phenotypes"))
        
        #Calculate the desired number of clusters with batch k menas
        print("Calculating Gaussian Mixed Model")
        GMM_model <- ClusterR::GMM(MARKERS %>% scale(),
                                   gaussian_comps = as.double(N_Phenotypes), 
                                   dist_mode = GMM_Distance,
                                   seed_mode = "random_subset",
                                   km_iter = Max_iterations_km,
                                   em_iter = Max_iterations_em,
                                   verbose = TRUE,
                                   var_floor = 1e-10,
                                   full_covariance_matrices = FALSE
        )
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = GMM_model, fuzzy = F, newdata = MARKERS %>% scale()) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Phenotype"
        
        #Generate the data phenotypes tibble
        DATA_Phenotypes <- bind_cols(DATA, pr_mb)
      }
    }
    
    else if(Strategy == "CLARA_clustering"){
      #First we calculate a metric to decide the number of total phenotypes
      print("Starting Cluster number stimation process")
      Optimal <-  try(ClusterR::Optimal_Clusters_Medoids(MARKERS %>% scale(),
                                                         max_clusters = Max_N_phenotypes_CLARA,
                                                         distance_metric = Distance_CLARA,
                                                         criterion = "silhouette" ,
                                                         clara_samples = Samples_CLARA,
                                                         clara_sample_size = Sample_per_CLARA,
                                                         swap_phase = F,
                                                         threads = N_cores,
                                                         verbose = T,
                                                         plot_clusters = T)
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Continue if everything OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_phenotypes_CLARA), 
                            title = paste0("Based on the plots generated and you previous choice, decide the appropiate number of final Phenotypes"))
        print("Performing CLARA (Clustering Large Applications)")
        CLARA_Clustering <- ClusterR::Clara_Medoids(MARKERS %>% scale(),
                                                    clusters = as.double(N_Phenotypes), 
                                                    samples = Samples_CLARA,
                                                    sample_size = Sample_per_CLARA,
                                                    distance_metric = Distance_CLARA,
                                                    threads = N_cores,
                                                    swap_phase = F,
                                                    fuzzy = FALSE,
                                                    verbose = T,
                                                    seed = 21)
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = CLARA_Clustering, fuzzy = F, newdata = MARKERS %>% scale()) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Phenotype"
        
        #Generate the data phenotypes tibble
        DATA_Phenotypes <- bind_cols(DATA, pr_mb)
      }
    }
    
    #If there are noisy and real cells bind both tibbles
    if(Apply_Denoise){
      DATA_Phenotypes <- DATA_Phenotypes %>% mutate(Phenotype = as.numeric(as.numeric(Phenotype) + 1))
      DATA_Phenotypes <- bind_rows(DATA_NOISE, DATA_Phenotypes) %>% arrange(Unique_ID) %>% dplyr::select(-Unique_ID, -NOISE)
      warning("If denoising is applied, Cluster nº1 contains the noisy cells")
    }
    
    #Turn Phenotype into a factor
    DATA_Phenotypes <- DATA_Phenotypes %>% mutate(Phenotype = factor(Phenotype))
    
    #plot dimension reduction according to the number of cells if required
    if(Perform_Dimension_reduction){
      if(nrow(DATA_Phenotypes) <= 100000){
        try(plot(
          left_join(DATA_Phenotypes, DATA_Reduction, by = "Cell_no") %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = Phenotype)) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Phenotype", values = unname(pals::polychrome(length(unique(DATA_Phenotypes$Phenotype)))))
        )
        )
      }
      if(nrow(DATA_Phenotypes) > 100000){
        message(">100K observations to generate plots. A random subset containing 10% of the dataset will be selected for Dimension reduction plots")
        try(plot(
          left_join(DATA_Phenotypes, DATA_Reduction, by = "Cell_no") %>% sample_n(size = 100000) %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = Phenotype)) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Phenotype", values = unname(pals::polychrome(length(unique(DATA_Phenotypes$Phenotype)))))
        )
        )
      }
    }
    
    
    #Skip plot rendering if too many cell features
    if(ncol(DATA_Phenotypes %>% dplyr::select(-c(1:4))) > 100){
      Plot_required <- menu(choices = c("Proceed", "Abort"), title = "More than 100 features. Proceed with plot rendering?")
      if(Plot_required == 2){
        #Print a summary with the results
        print(DATA_Phenotypes %>% dplyr::count(Neighborhood_assignment))
        
        if(Perform_Dimension_reduction) return(list(DATA = DATA_Phenotypes,
                                                    Dimension_reduction = DATA_Reduction)
        )
        else return(DATA_Phenotypes)
      }
    }
    
    #Visualize the neighbor composition data for each neighborhood
    plot(DATA_Phenotypes %>% dplyr::select(-c(1:4)) %>% pivot_longer(cols = -Phenotype) %>%
           ggplot(aes(x = as.factor(Phenotype), y = value)) +
           geom_violin(aes(color = name, fill = name), alpha=0.3, position=position_dodge(width=0.5)) +
           stat_summary(aes(color = name), 
                        fun = median, geom = "crossbar", width = 0.4, linetype = 1, linewidth = 0.5,
                        position = position_dodge(width = 0.5)) +
           cowplot::theme_cowplot()+
           scale_x_discrete("Phenotype")+
           scale_y_continuous("Marker intensity"))
    
    #Visualize the heatmap of mean by neighborhood
    Mean_tibble <- DATA_Phenotypes %>% dplyr::select(-c(1:4)) %>% group_by(Phenotype) %>% summarize_all(mean) %>% ungroup() #Obtain mean tibble
    Mean_matrix <- as.matrix(Mean_tibble[-1] %>% scale()) #Scale it and transform it into a  mtrix
    row.names(Mean_matrix) <- Mean_tibble[[1]] 
    
    plot(ComplexHeatmap::Heatmap(Mean_matrix,
                                 name = "Scaled")
    )
    
    #Print the summary
    print(DATA_Phenotypes %>% dplyr::count(Phenotype))
    #Return data and Dimension reduction if generated
    if(Perform_Dimension_reduction) return(list(DATA = DATA_Phenotypes,
                                                Dimension_reduction = DATA_Reduction)
    )
    else return(DATA_Phenotypes)
    
  },
  options = list(optimize = 3))

DATA_Phenotype_renamer <- cmpfun(
  function(DATA = NULL,
           New_names = NULL) {
    
    #Check if provided names are equal to number of hoods
    if(length(New_names) != length(unique(DATA$Phenotype))) {
      stop(paste0("Provided New_names should match the number of Phenotypes in the analysis. Number of Phenotypes: ", length(unique(DATA$Phenotype)),
                  ". Names provided: ", length(New_names)))
    }
    
    #Proceed with renaming
    else{
      DATA_Phenotypes <- DATA
      names_tibble <- tibble(Phenotype = factor(1:length(unique(DATA_Phenotypes$Phenotype))),
                             New_names = New_names)
      Result <- left_join(DATA_Phenotypes, names_tibble, by = "Phenotype")
      Result %>% mutate(Phenotype = New_names) %>% dplyr::select(-New_names)
    }
  },
  options = list(optimize = 3))

CELESTA_template_generator <- cmpfun(
  function(DATA = NULL,
           Markers_to_keep = NULL,
           Template_name = NULL){
    DATA <- DATA
    #Check that data and Markers_to_keep have been correctly specified
    if(!all(c("X", "Y", "Subject_Names", Markers_to_keep) %in% names(DATA))) {
      Missing_arguments <- c("X", "Y", "Subject_Names", Markers_to_keep)[!c("X", "Y", "Subject_Names", Markers_to_keep) %in% names(DATA)]
      stop(paste0(str_c(Missing_arguments, collapse = ", "), " not found in DATA"))
    }
    if(!is.character(as.character(Template_name))) stop("Template_name must be a character value")
    
    #Generate the basic tibble
    Basic <- tibble(Phenotype = NA, Phenotype_Number = NA, Assignment_round = NA, Phenotype_dependency = NA, 
                    high_expression_threshold_anchor = NA, high_expression_threshold_index = NA,
                    low_expression_threshold_anchor = 1, low_expression_threshold_index = 1)
    
    #Generate the marker tibble
    Markers <- as_tibble(matrix(NA, nrow = 1, ncol = length(Markers_to_keep)), .name_repair = "minimal")
    Markers <- setNames(Markers, Markers_to_keep)
    
    Final_tibble <- dplyr::bind_cols(Basic, Markers)
    Final_tibble
    
    print(paste0("The CELESTA prior information template will be saved at: ", getwd()))
    
    readr::write_excel_csv(Final_tibble, paste0(Template_name, ".csv"), na = "NA")
    
    print("For more information on how to fill in the template please visit: https://github.com/plevritis-lab/CELESTA")
    print("Complete the template and save it as a csv file")
  },
  options = list(optimize = 3))

CELESTA_phenotyper <- cmpfun(function(DATA = NULL,
                                      Template_path = NULL,
                                      Alternative_CSV_locale = NULL,
                                      N_cores = NULL,
                                      
                                      Apply_filters = NULL,
                                      high_marker_threshold = NULL,
                                      low_marker_threshold = NULL,
                                      
                                      max_iteration = 10,
                                      cell_change_threshold = 0.01
){
  DATA <- DATA
  #Check arguments
  if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))) {
    stop("DATA not correctly specified, please format appropiatetly (see Step 0)")
  }
  
  if(!is.logical(Alternative_CSV_locale)) stop("Alternative_CSV_locale must be a logical value")
  if(!is.logical(Apply_filters)) stop("Apply_filters must be a logical value")
  if(!all(N_cores >= 1, N_cores%%1 == 0)) stop("N_cores must be a positive integer value")
  if(Apply_filters){
    if(!all(is.numeric(high_marker_threshold), high_marker_threshold >= 0, high_marker_threshold <= 1)) stop("high_marker_threshold should be a numeric value between 0 and 1")
    if(!all(is.numeric(low_marker_threshold), low_marker_threshold >= 0, low_marker_threshold <= 1)) stop("low_marker_threshold should be a numeric value between 0 and 1")
  }
  if(!all(is.numeric(max_iteration), max_iteration > 0, max_iteration%%1 == 0)) stop("max_iteration should be a integer value > 0")
  if(!all(is.numeric(cell_change_threshold), cell_change_threshold >= 0, cell_change_threshold <= 1)) stop("cell_change_threshold should be a numeric value between 0 and 1")
  
  #Import Prior_marker_info
  if(!Alternative_CSV_locale) Prior_info <- readr::read_csv(Template_path, na = c("NA", ""))
  if(Alternative_CSV_locale) Prior_info <- readr::read_csv2(Template_path, na = c("NA", ""))
  
  #Check the imported Prior_info
  if(!identical(c("Phenotype",	
                  "Phenotype_Number",	
                  "Assignment_round",	
                  "Phenotype_dependency",	
                  "high_expression_threshold_anchor",	
                  "high_expression_threshold_index",
                  "low_expression_threshold_anchor",	
                  "low_expression_threshold_index"), names(Prior_info)[1:8])) stop("Template not well specified. Please review the file, the pathway provided and the CSV locale")
  
  #Check that there are no bugs in the Prior_info
  if(sum(is.na(Prior_info$Phenotype)) >= 1){
    stop("The following rows contain unintended NA values present in Template, please review the file provided: ",
         str_c(which(is.na(Prior_info$Phenotype)), collapse = ", "))
  }
  #Check if any variable has all NA in round 1 phenotypes
  if(any(map_lgl(Prior_info %>% dplyr::filter(Assignment_round == 1), function(var){
    sum(is.na(var)) == nrow(Prior_info %>% dplyr::filter(Assignment_round == 1))
  }))){
    Conflictive_vars <- names(Prior_info)[map_lgl(Prior_info %>% dplyr::filter(Assignment_round == 1), function(var){
      sum(is.na(var)) == nrow(Prior_info %>% dplyr::filter(Assignment_round == 1))
    })]
    stop(paste0("The following variables have NA values for all phenotypes identified during round 1: ",
                str_c(Conflictive_vars, collapse = ", "),
                ". This can cause errors during CELESTA execution. Please review the file provided."))
  }
  
  
  #Build the final Prior_info dataframe
  #Collapse the lineage level, bind it to marker info and add row names
  Lineage_level <- stringr::str_c(Prior_info$Assignment_round, Prior_info$Phenotype_dependency, Prior_info$Phenotype_Number, sep = "_")
  Final_template <- cbind(Prior_info[["Phenotype"]], Lineage_level, Prior_info[-c(1:8)])
  names(Final_template)[1] <- ""
  
  print(head(Final_template))
  print(paste0("The following markers will be used in the cell phenotyping process", str_c(names(Final_template)[-1], collapse = ", ")))
  print(paste0(nrow(DATA), " cells will be phenotyped in the process"))
  print(paste0(nrow(Final_template), " phenotypes will be identified"))
  
  Proceed_CELESTA <- menu(choices = c("Proceed", "Abort"), title = "Should the CELESTA algorithm be executed?")
  if(Proceed_CELESTA == 2) stop("Phenotyping has been aborted")
  
  #save exit function if parallelization fails
  on.exit({
    future::plan("future::sequential")
    gc()
  })
  #We make the clusters and load required packages
  future::plan("future::multisession", workers = N_cores) 
  options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
  furrr::furrr_options(scheduling = Inf)
  
  Phenotyping_result <- suppressMessages({
    furrr::future_map(unique(DATA$Subject_Names), function(Index){
      library(zeallot)
      library(Rmixmod)
      library(spdep)
      
      #Select individual images
      DATA <- DATA %>% dplyr::filter(Subject_Names == Index)
      
      
      Celesta_object <- CELESTA::CreateCelestaObject(project_title = "Celesta_object", 
                                                     prior_marker_info = Final_template,
                                                     imaging_data_file = DATA)
      if(Apply_filters){
        Celesta_object <- CELESTA::FilterCells(Celesta_object, 
                                               high_marker_threshold = high_marker_threshold, 
                                               low_marker_threshold = low_marker_threshold)
      }
      Celesta_object <- CELESTA::AssignCells(Celesta_object,
                                             max_iteration = max_iteration,
                                             cell_change_threshold = cell_change_threshold,
                                             high_expression_threshold_anchor = Prior_info[["high_expression_threshold_anchor"]],
                                             low_expression_threshold_anchor = Prior_info[["low_expression_threshold_anchor"]],
                                             high_expression_threshold_index = Prior_info[["high_expression_threshold_index"]],
                                             low_expression_threshold_index = Prior_info[["low_expression_threshold_index"]],
                                             save_result = FALSE)
      
      #Obtain final result, link it to original data and exit
      Phenotype_tibble <- as_tibble(Celesta_object@final_cell_type_assignment)$'Final cell type'
      Phenotype_tibble <- tibble(Phenotype = Phenotype_tibble)
      FINAL <- bind_cols(DATA, Phenotype_tibble)
      return(FINAL) 
    }, .progress = TRUE)
  })
    
  future::plan("future::sequential")
  gc()
  
  #return the complete dataset
  return(map_dfr(Phenotyping_result, bind_rows))
}, 
options = list(optimize = 3))

Phenotyping_evaluator_shiny_app_launcher <- cmpfun(
  function(DATA = NULL,
           Directory = NULL,
           Ordered_Channels = NULL){
    
    DATA <- DATA
    
    #Test that DATA provided is adequate
    if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))) {
      stop("Please generate an appropiate data object using the Data_arrange_function")
    }
    if(!"Phenotype" %in% names(DATA)) stop("DATA must contain a column specifying cell phenotypes")
    
    #Generate vectors that will guide the sliders and selectors of UI
    Images_in_Data <- unique(DATA$Subject_Names)
    Phenotypes <- unique(DATA$Phenotype)
    Real_Images <- dir(Directory, full.names = FALSE)
    Channels_in_images <- Ordered_Channels
    N_colors <- length(unique(DATA$Phenotype))
    
    #Set the colors for the plots
    if(N_colors > 22) warning("Currently the color only supports displaying 22 phenotype simulataneously. The same color will be assigned to several phenotypes")
    Color_tibble <- tibble(Phenotype = unique(DATA$Phenotype), Color_code = unname(pals::trubetskoy(n = length(unique(DATA$Phenotype))))) %>% 
      mutate(Color_code = case_when(is.na(Color_code) ~ "white",
                                    TRUE ~ Color_code))
    DATA <- left_join(DATA, Color_tibble, by = "Phenotype")
    
    #Generate a scaled version of the data if it comes from numeric values
    Variables <- names(DATA)[which(!names(DATA) %in% c("Cell_no", "X", "Y", "Subject_Names", "Phenotype", "Color_code"))]
    if(is.logical(unlist(DATA[Variables]))){
      print("DATA has been thresholded. Heatmap may yield misleading results")
      DATA[Variables] <- DATA[Variables] %>% scale()
    }
    else{DATA[Variables] <- DATA[Variables] %>% scale()}
    
    #Define quantiles 95 and 5 values of all the dataset for the heatmap
    Min_HEATMAP <- quantile(unlist(DATA[Variables]), 0.025)
    Max_HEATMAP <- quantile(unlist(DATA[Variables]), 0.975)
    
    #BUILD THE USER INTERFACE
    {
      user_interface <- shiny::fluidPage(
        
        #Set the title
        shiny::titlePanel("Phenotyping exploration APP"),
        
        #We want a two panel layout, one in the left containing the input parameters and the output in the right
        shiny::sidebarLayout(
          #Set the first column (which contains the user defined parameters)
          shiny::sidebarPanel(
            #ID and width
            id="sidebar",
            
            shiny::fluidRow(
              #Select Image to be analyzed
              shiny::column(7, shiny::selectInput("Data_Image_name", "Image from data", sort(Images_in_Data), multiple = FALSE)),
              shiny::column(2, shinyWidgets::materialSwitch("Change_coords", "Pixel/dist", value = FALSE)),
              shiny::column(2, shiny::conditionalPanel(condition = "input.Change_coords == '1'",
                                                       shiny::textInput("Ratio", "pixel size", value = "1")
              ))),
            #Real image to be displayed
            shiny::fluidRow(
              #Select the real image to be displayed
              shiny::column(7, shiny::selectInput("Real_Image_name", "Image to display", sort(Real_Images), multiple = FALSE)),
              #Select the channel to be displayed
              shiny::column(5, shiny::selectInput("Channel", "Channel to display", Channels_in_images, multiple = FALSE))
            ),
            
            #Add the check box
            shiny::fluidRow(
              shinyWidgets::virtualSelectInput("Checkbox", label = "Phenotypes to display", 
                                               choices = sort(Phenotypes), 
                                               selected = sort(Phenotypes),  
                                               search = TRUE,
                                               multiple = TRUE)
            ),
            
            #Image parameters
            shiny::fluidRow(
              #Select the min point of the image
              shiny::column(6, shiny::sliderInput("Min_Image", "Absolute Black", value = 0, min = 0, max = 100, step = 1)),
              #Select the max point of the image
              shiny::column(6, shiny::sliderInput("Max_Image", "Absolute White", value = 100, min = 0, max = 100, step = 1))
              
            ),
            shiny::fluidRow(
              #Select the Gamma
              shiny::column(6, shiny::sliderInput("Gamma", "Gamma", value = 0, min = -3, max = +3, step = 0.01)),
              #Select the image resolution
              shiny::column(3, shiny::selectInput("Res", "Image Res", c('Very Low' = 300, Low = 500, Mid = 750, High = 1000, Original = 1400), selected = 500, multiple = FALSE)),
              #Select the equalization
              shiny::column(3, shiny::selectInput("Equalize", "Equalize", c(YES = TRUE, NO = FALSE), selected = FALSE, multiple = FALSE))
            ),
            
            #Finally add a couple of rows more with extra options and the final result
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput("X_flip", "Flip X image", c(YES = TRUE, NO = FALSE), selected = FALSE, multiple = FALSE)),
              shiny::column(4, shiny::selectInput("Y_flip", "Flip Y image", c(YES = TRUE, NO = FALSE), selected = FALSE, multiple = FALSE)),
              shiny::column(4, shiny::actionButton("reset", shiny::icon("redo"), label = "Reset selection"))
            ),
            #The UI will be completed with summary tables of the sample
            shiny::fluidRow(
              shiny::column(12, htmltools::p("Sample Summary: ", shiny::tableOutput("Summary")))
            )
          ),
          
          #Set the outcome columns
          shiny::mainPanel(
            #First row will have the Photo and the overview of marker intensity by cell
            shiny::fluidRow(
              shiny::column(5, shiny::plotOutput("Photo",
                                                 #Controls for zoom in
                                                 dblclick = "Photo_dblclick",
                                                 brush = shiny::brushOpts(id = "Photo_brush",
                                                                          resetOnNew = TRUE)
              )
              ),
              shiny::column(5, ggiraph::girafeOutput("All_phenotypes"))
            ),
            #Second row will contain the positive cells table and the heatmap
            shiny::fluidRow(
              shiny::column(4, htmltools::p("Selected cells: ", shiny::tableOutput("Cell_selection"))),
              shiny::column(5, shiny::plotOutput("Heatmap"))
            )
          )
        ),
        shiny::tags$head(shiny::tags$style(
          htmltools::HTML('
         #sidebar {
            background-color: #fad2d2;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')))
      )
    }
    
    
    
    #BUILD THE SERVER
    server <- function(input, output, session){
      #All the reactives to be used
      #Generate a reactive with the real Image name and the channel number
      Photo_name <- shiny::reactive(str_c(Directory, "/", input$Real_Image_name))
      Channel_index <- shiny::reactive(which(input$Channel == Ordered_Channels))
      Photo_min <- shiny::reactive(input$Min_Image)
      Photo_max <- shiny::reactive(input$Max_Image)
      Photo_gamma <- shiny::reactive(10^input$Gamma)
      Equalize <- shiny::reactive(input$Equalize)
      #Reactive expression to control the graphs
      Case_id <- shiny::reactive(input$Data_Image_name)
      
      X_flip <- shiny::reactive(input$X_flip)
      Y_flip <- shiny::reactive(input$Y_flip)
      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      Pixel_dist_conversion <- shiny::reactive(input$Change_coords)
      Pixel_dist_ratio <- shiny::reactive(input$Ratio)
      Resolution <- shiny::reactive(input$Res)
      
      #Generate the subject specific DATA that will be used in all plots
      #Control the data source
      Source_DATA <- shiny::reactive({
        Final_DATA <- DATA %>% dplyr::filter(Subject_Names == Case_id())
        #Modify pixel values if required
        if(as.logical(Pixel_dist_conversion())){
          Final_DATA$X <- Final_DATA$X * as.numeric(Pixel_dist_ratio())
          Final_DATA$Y <- Final_DATA$Y * as.numeric(Pixel_dist_ratio())
        }
        #Return the final data
        return(Final_DATA)
      })
      
      #Control the checkbox output
      Checkbox_output <- shiny::reactive(input$Checkbox)
      
      #Reactive that imports the photograph
      Photo_reactive <- shiny::reactive({
        #Import the Photo
        Photo <- magick::image_read(Photo_name())[Channel_index()]
        #Perform flip and flop if required
        if(as.logical(X_flip())) Photo <- Photo %>% magick::image_flop()
        if(as.logical(Y_flip())) Photo <- Photo %>% magick::image_flip()
        #Perform image equalization as requested by user
        if(as.logical(Equalize())) Photo <- Photo %>% magick::image_equalize()
        #Perform image white adjustment
        Photo <- Photo %>% 
          magick::image_level(black_point = Photo_min(),
                              white_point = Photo_max(),
                              mid_point = Photo_gamma())
        
        
        ##Obtain dimensions (these will set the axis limits) BEFORE the resolution is modified
        Photo_Dim <- magick::image_info(Photo)
        #Change resolution if appropiate
        if(as.numeric(Resolution() != 1400)){
          Image_Resolution <- str_c("X", Resolution())
          Photo <- magick::image_scale(Photo, Image_Resolution)
        }
        return(list(Photo = Photo,
                    Dims = Photo_Dim))
      })
      
      Photo_plot_reactive <- shiny::reactive({
        Photo <- Photo_reactive()[["Photo"]]
        Photo_Dim <- Photo_reactive()[["Dims"]]
        #Return the result as a scaffold ggplot_object
        Photo_plot <- ggplot() + 
          annotation_raster(Photo, xmin = 0, xmax = Photo_Dim$width, ymin = 0, ymax = Photo_Dim$height, interpolate = TRUE)+
          scale_x_continuous(limits = c(0, Photo_Dim$width)) +
          scale_y_continuous(limits = c(0, Photo_Dim$height)) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
          theme(axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.background = element_rect(fill = "black"),
                panel.grid = element_blank())
        return(Photo_plot)
      }) 
      
      #Print the photo
      output$Photo <- shiny::renderPlot({
        #Plot the result
        try(Photo <- Photo_plot_reactive())
        
        #If the photo returns an error return the point image
        if(berryFunctions::is.error(Photo)){
          Final_DATA <- Source_DATA()
          
          Photo_plot <-
            Final_DATA %>% 
            ggplot() +
            geom_point(aes(x = X, y = Y),
                       color = "white",
                       size = 2.5) +
            scale_x_continuous(limits = c(min(Final_DATA$X), max(Final_DATA$X))) +
            scale_y_continuous(limits = c(min(Final_DATA$Y), max(Final_DATA$Y))) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  panel.grid = element_blank()) +
            annotate("text", x = quantile(Final_DATA$X, 0.5), y = quantile(Final_DATA$Y, 0.5), 
                     color = "red", size = 2, hjust = 0.5,
                     label = "UNABLE TO RENDER PHOTO\nUSE ME TO ZOOM IN")
          return(Photo_plot)
        }
        
        else{
          #Generate the plot
          return(Photo)
        }
        
      }, res = 300)
      
      #Control the zoom of the Photo and the rest of the graphs
      shiny::observeEvent(input$Photo_dblclick, {
        brush <- input$Photo_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      
      #All cells phenotype
      Cell_Phenotype_plot <- 
        shiny::reactive({
          #Get sample data and change the axis to coordinate all three plots displayed
          Final_DATA <- Source_DATA()
          #Get the min and max of the photo before selecting the required phenotypes
          plot_x_min <- min(Final_DATA$X)
          plot_x_max <- max(Final_DATA$X)
          plot_y_min <- min(Final_DATA$Y)
          plot_y_max <- max(Final_DATA$Y)
          
          #Filter phenotypes in checkbox
          Final_DATA <- Final_DATA %>% dplyr::filter(Phenotype %in% Checkbox_output()) 
          
          #Import the photo
          try(Photo <- Photo_plot_reactive())
          
          #If image not available plot without image
          if(berryFunctions::is.error(Photo)){
            return(
              #Generate the final plot
              Final_DATA %>% 
                ggplot() +
                scale_color_identity()+
                ggiraph::geom_point_interactive(aes(x = X, y = Y, group = Phenotype, color = Color_code,
                                                    data_id = Cell_no, 
                                                    tooltip = str_c(as.character(Cell_no)," Type = ", as.character(Phenotype)),
                                                    hover_nearest = FALSE),
                                                size = 2) +
                cowplot::theme_cowplot()+
                guides(color = "none") +
                theme(axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_blank(),
                      axis.title = element_blank(),
                      panel.background = element_rect(fill = "black"),
                      legend.position = "bottom",
                      legend.text = element_text(size = 10)) +
                scale_x_continuous(limits = c(plot_x_min, plot_x_max)) +
                scale_y_continuous(limits = c(plot_y_min,  plot_y_max)) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
            )
          }
          #If not proceed as usual
          else{
            return(
              #Generate the final plot
              Photo +
                scale_color_identity()+
                ggiraph::geom_point_interactive(aes(x = X, y = Y, group = Phenotype, color = Color_code,
                                                    data_id = Cell_no, 
                                                    tooltip = str_c(as.character(Cell_no)," Type = ", as.character(Phenotype)),
                                                    hover_nearest = FALSE),
                                                size = 2,
                                                data = Final_DATA) +
                cowplot::theme_cowplot()+
                guides(color = "none") +
                theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.line = element_blank(),
                      panel.background = element_rect(fill = "black"),
                      panel.grid = element_blank())
            )
          }
        })
      #Send plot to UI
      output$All_phenotypes <- ggiraph::renderGirafe({
        plot <- ggiraph::girafe(code = print(Cell_Phenotype_plot()),
                                options = list(
                                  ggiraph::opts_hover(css = "stroke:black;cursor:pointer;", reactive = TRUE),
                                  ggiraph::opts_selection(type = "multiple", css = "fill:#FF3333;stroke:black;")
                                )
        )
        plot
      })
      
      #The sample summary (phenotype count table)
      output$Summary <- gt::render_gt({
        Sample_DATA <- Source_DATA()
        Sample_table <- Sample_DATA %>% dplyr::count(Phenotype) 
        
        Sample_table <- Sample_table %>% mutate('%' = round(n/nrow(Source_DATA())*100, 2))
        Sample_table <- left_join(Sample_table, Color_tibble, by = "Phenotype")
        Sample_table <- Sample_table %>% dplyr::arrange(desc(n))
        
        Sample_table %>% gt::gt() %>%
          gt::opt_table_outline(style = "solid", color = "black") %>%
          gt::tab_style(
            style = list(
              gt::cell_text(color = "black", size = gt::px(20), font = "Calibri", align = "center"),
              gt::cell_fill(color = gt::from_column(column = "Color_code"))
            ),
            locations = gt::cells_body(columns = c(1:4))
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_text(color = "black", size = gt::px(22), weight = "bold", font = "Calibri", align = "center")
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::cols_width(
            1 ~ gt::pct(50),
            2	~ gt::pct(30),
            3	~ gt::pct(20),
            4 ~ gt::pct(0)
          )
      })
      
      #Selected cells and reset button
      selected_cells_phenotype <- shiny::reactive(input$All_phenotypes_selected)
      #What to do in case the user hits the reset button
      shiny::observeEvent(input$reset, {
        session$sendCustomMessage(type = 'All_phenotypes_set', message = character(0))
      })
      #Generate the output tibble for the selected cells
      output$Cell_selection <- shiny::renderTable({
        #Get cell_no in both plots and remove duplicates
        Cells <- unique(selected_cells_phenotype())
        #Get Marker data and threshold data an start processing
        Selected_Cells <- Source_DATA() %>% dplyr::filter(Cell_no %in% Cells) %>% dplyr::select(Cell_no, X, Y, Phenotype)
        Final_cells <- Selected_Cells %>% dplyr::count(Phenotype)
        dplyr::bind_rows(Final_cells, tibble(Phenotype = "TOTAL", n = sum(Final_cells$n)))
      })
      
      #Generate the heatmap
      output$Heatmap <- shiny::renderPlot({
        #Get cell_no in both plots and remove duplicates
        Cells <- unique(selected_cells_phenotype())
        
        #Get Marker data and threshold data an start processing
        Selected_Cells <- Source_DATA() %>% dplyr::filter(Cell_no %in% Cells) 
        
        #If user has not selected cells then print a fixed value matrix
        if(nrow(Selected_Cells) == 0){
          Matrix <- matrix(0, 1, 1)
          ComplexHeatmap::Heatmap(Matrix)
        }
        #If cells have been selected print the actual matrix
        else{
          #Prepare the matrix
          HEATMAP_MATRIX <- Selected_Cells %>% dplyr::select(-c(1:4), -Phenotype, -Color_code)
          HEATMAP_MATRIX <- as.matrix(HEATMAP_MATRIX)
          row.names(HEATMAP_MATRIX) <- Selected_Cells[["Cell_no"]]
          
          #Prepare the color function for the matrix itself and the rowside annotation
          col_fun <- circlize::colorRamp2(c(Min_HEATMAP, 0, Max_HEATMAP), c("#0000ff", "white", "#ff0000"))
          row_color_code <- Color_tibble[[2]]
          names(row_color_code) <- Color_tibble[[1]]
          
          
          #Prepare the side annotation
          Side_annotation <- ComplexHeatmap::rowAnnotation(
            Phenotype = Selected_Cells[["Phenotype"]],
            col = list(Phenotype = row_color_code),
            show_annotation_name = F,
            show_legend = F)
          
          ComplexHeatmap::Heatmap(HEATMAP_MATRIX, 
                                  col = col_fun,
                                  cluster_rows = T,
                                  show_row_names = FALSE,
                                  show_heatmap_legend = F,
                                  left_annotation = Side_annotation,
                                  show_row_dend = F,
                                  show_column_dend = F,
                                  column_names_side = "top",
                                  column_names_rot = 0,
                                  column_names_centered = T,
                                  column_names_gp = grid::gpar(fontsize = 10),
                                  border = T
          )
        }
      })
      
      #If browser is closed end the app
      session$onSessionEnded(function() { shiny::stopApp() })
    }
    
    #Run the server
    message("Always stop current R execution if you want to continue with your R session")
    shiny::shinyApp(user_interface, server)
  },
  options = list(optimize = 3))

expand.grid.unique <- cmpfun(
  function(x, y, include.equals=FALSE)
  {
    x <- unique(x)
    y <- unique(y)
    g <- function(i)
    {
      z <- setdiff(y, x[seq_len(i-include.equals)])
      
      if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    do.call(rbind, lapply(seq_along(x), g))
  }, 
  options = list(optimize = 3))

Concordance_calculator <- cmpfun(function(..., 
                                          Variable = NULL,
                                          Strategy = NULL){
  on.exit(gc())
  Tibble_list <- list(...)
  
  print("Argument check...")
  #Check the variable argument
  if(!is.character(Variable)) stop("Variable must be a character value")
  if(!Strategy %in% c("Rand", "FM")) stop("Strategy must be one of the following: Rand, FM")
  
  #Check the user has provided names to the DATA provided
  if(any(names(Tibble_list) == "")) stop("User must provide a name for each DATA provided")
  
  #Check that names not are repeated
  if(length(unique(names(Tibble_list))) != length(Tibble_list)) stop("DATA provided must have each a different name")
  
  #Check that names do not contain the  '_' character. If they do modify with '.'
  names(Tibble_list) <- str_replace_all(names(Tibble_list), pattern = "_", replacement = ".")
  
  #Check the length of the list
  if(length(Tibble_list) < 2) stop("At least 2 DATA sources must be provided to calculate concordance")
  
  #Check that the first four columns in the data are the sampe
  if(!all(map_lgl(Tibble_list, function(tibble){
    identical(names(tibble)[c(1:4)], c("Cell_no", "X", "Y", "Subject_Names"))
  }))
  ) stop("DATA provided must have been formatted appropiately. Names of DATA provided do not match required standards")
  
  #Check that data have the same number of rows
  if(length(unique(map_dbl(Tibble_list, function(tibble) nrow(tibble)))) != 1) stop("All DATA provided must have the same number of rows")
  
  #Check that Variable is present in all data
  Variable_in_tibbles <- map_lgl(Tibble_list, function(tibble) as.character(Variable) %in% names(tibble))
  if(!all(Variable_in_tibbles)){
    stop(paste0("The following DATA provided does not contain the ", Variable, " column: ",  
                str_c(names(Tibble_list)[!Variable_in_tibbles], collapse = ", ")))
  }
  
  #check the presence of NA values in the Variable column
  Tibbles_with_NA <- map_lgl(Tibble_list, function(tibble) sum(is.na(tibble[[Variable]])) >= 1)
  if(any(Tibbles_with_NA)){
    stop(paste0(str_c(names(Tibble_list)[Tibbles_with_NA], collapse = ", "), " contains NA in ", Variable, " column. Please remove NA values"))
  } 
  
  #Check that the same subject names are the same in all the datasets
  Subject_names_tibble <- map_df(Tibble_list, function(tibble){
    tibble[["Subject_Names"]]
  })
  Subject_names_tibble <- Subject_names_tibble %>% distinct
  if(
    !all(map_lgl(seq_along(1:nrow(Subject_names_tibble)), function(Row){
      Row <- unlist(Subject_names_tibble [Row,])
      length(unique(Row)) == 1
    }))
  ) stop("Subject_Names must be the same in all DATA provided")
  
  #Check that all Cells_ID are matched
  Cell_no_tibble <- map_df(Tibble_list, function(tibble){
    tibble[["Cell_no"]]
  })
  Cell_no_tibble <- Cell_no_tibble %>% distinct
  if(
    !all(map_lgl(seq_along(1:nrow(Cell_no_tibble)), function(Row){
      Row <- unlist(Cell_no_tibble[Row,])
      length(unique(Row)) == 1
    }))
  ) stop("Rows in each DATA provided must have the information of the same cell")
  #Remove the cell_no_tibble
  rm(Cell_no_tibble)
  
  #select the variable and turn it into a numeric value
  Tibble_list <- map(Tibble_list, function(tibble){
    Interim_tibble <- tibble %>% dplyr::select(1:4, all_of(Variable))
    Interim_tibble[5] <- as.numeric(as.factor(Interim_tibble[[5]])) 
    Interim_tibble
  })
  
  #Start performing the computation
  By_subject_results <- map(unique(Tibble_list[[1]][["Subject_Names"]]), 
                            function(Subject){
                              #Generate a tibble that has a column for every method
                              map_dfc(seq_along(1:length(Tibble_list)), function(Method){
                                Interim <- Tibble_list[[Method]] %>% dplyr::filter(Subject_Names == Subject) %>% dplyr::select(all_of(Variable))
                                names(Interim) <- names(Tibble_list)[Method]
                                return(Interim)
                              })
                            }, .progress = list(clear = F,
                                                name = paste0("Obtaining ", Variable, " information for each Subject Name"),
                                                show_after = 1,
                                                type = "iterator"))
  
  names(By_subject_results) <- unique(Tibble_list[[1]][["Subject_Names"]])
  By_subject_results
  
  #Calculate the rand index for every 2 by 2 method
  Concordance_index_result <- map(By_subject_results, function(Subject){
    #generate a comparison plan (it will indicate the map function what 2 by 2 comparisons need to be performed)
    Comparison_plan <- expand.grid.unique(names(Subject), names(Subject))
    Comparison_plan <- tibble(Method_A = Comparison_plan[,1], Method_B = Comparison_plan[,2])
    
    #Execute the comparison plan according to the strategy selected
    if(Strategy == "Rand"){
      Result <- Comparison_plan %>% mutate(Concordance_result = pmap_dbl(Comparison_plan, function(Method_A, Method_B){
        catsim::rand_index(Subject[[Method_A]], Subject[[Method_B]])
      }))
    }
    if(Strategy == "FM"){
      Result <- Comparison_plan %>% mutate(Concordance_result = pmap_dbl(Comparison_plan, function(Method_A, Method_B){
        dendextend::FM_index(Subject[[Method_A]], Subject[[Method_B]])[[1]]
      }))
    }
    return(Result)
  }, .progress = list(clear = F,
                      name = paste0("Calculating ", as.character(Strategy), " index for each Subject"),
                      show_after = 1,
                      type = "iterator"))
  
  #Generate a tibble with the results by patient
  By_Patient_results <- map_dfr(seq_along(1:length(Concordance_index_result)), 
                                function(Index){
                                  Interim_tibble <- Concordance_index_result[[Index]] %>% mutate(Comparison = str_c(Method_A, Method_B, sep = "_")) %>% 
                                    dplyr::select(Concordance_result, Comparison) %>% 
                                    pivot_wider(names_from = Comparison, values_from = Concordance_result) %>% 
                                    mutate(Subject_Names = names(Concordance_index_result)[Index])
                                  Interim_tibble[c(ncol(Interim_tibble), 1:(ncol(Interim_tibble)-1))]
                                })
  #Calculate the median and p25 and q75 for each comparsion and arrange it all in a single list
  q50_rand <- map_dbl(By_Patient_results[-1], function(Comparison) quantile(Comparison, 0.50))
  q25_rand <- map_dbl(By_Patient_results[-1], function(Comparison) quantile(Comparison, 0.25)) 
  q75_rand <- map_dbl(By_Patient_results[-1], function(Comparison) quantile(Comparison, 0.75))
  Result_list <- list(Quantile_25 = q25_rand,
                      Quantile_50 = q50_rand,
                      Quantile_75 = q75_rand)
  
  #Use this list to generate heatmaps of comparsion results
  Heatmap_plots <-   map(seq_along(1:length(Result_list)), function(Index){
    #Get the result list and change the names of the tibble, then add a column specifying the comparison
    Interim <- as_tibble(Result_list[[Index]])
    names(Interim) <- "Value"
    Interim$Comparison <- names(By_Patient_results)[-1]
    
    #Split the comparison back into the two methods being compared and bind it to the interim tibble
    Comparisons <- str_split(Interim$Comparison, pattern = "_", simplify = TRUE)
    colnames(Comparisons) <- c("Method_A", "Method_B")
    Interim <- bind_cols(Interim, as_tibble(Comparisons))
    
    #Start generating the tibble for the final ggplot (it will require to columns with all possible comparisons, including duplicated ones)
    All_comparison_tibble <- as_tibble(expand.grid(unique(c(Interim$Method_A, Interim$Method_B)), unique(c(Interim$Method_A, Interim$Method_B))))
    names(All_comparison_tibble) <- c("Method_A", "Method_B")
    All_comparison_tibble$Comparison <- str_c(All_comparison_tibble$Method_A, All_comparison_tibble$Method_B, sep = "_")
    All_comparison_tibble <- All_comparison_tibble 
    
    #Generate another tibble with the same information but reverse labels
    Interim2 <- tibble(Value = Interim$Value, 
                       Comparison = str_c(Interim$Method_B, Interim$Method_A, sep = "_"),
                       Method_A = Interim$Method_A,
                       Method_B = Interim$Method_B)
    
    
    Interim <- bind_rows(Interim, Interim2) %>% dplyr::select(-Method_A, -Method_B)
    Tibble_plot <- left_join(All_comparison_tibble, Interim, by = "Comparison")
    
    Tibble_plot %>% 
      ggplot(aes(x = Method_A, y = Method_B, fill = Value)) +
      geom_tile() +
      scale_x_discrete("") +
      scale_y_discrete("", limits = rev) +
      scale_fill_gradient2(limits = c(0,1), low = "#f23a3a", mid = "white", high = "#3af256", midpoint = 0.5) +
      cowplot::theme_cowplot() +
      geom_text(aes(label = as.character(round(Value, 2))), size = 7) +
      guides(fill = "none") +
      ggtitle(names(Result_list)[Index]) +
      theme(plot.title = element_text(size = 12, hjust = 0.5),
            axis.text.x = element_text(size = 9, face = "bold", color = "black"),
            axis.text.y = element_text(size = 9, face = "bold", color = "black"))
  })
  #Print the plots
  suppressWarnings(plot(cowplot::plot_grid(plotlist = Heatmap_plots, nrow = 1, ncol = 3)))
  
  #Generate a heatmap with the results by patient
  plot(By_Patient_results %>% pivot_longer(-1) %>% 
         ggplot(aes(x = Subject_Names, y = name, fill = value)) + 
         geom_tile() +
         scale_x_discrete("") +
         scale_y_discrete("", limits = rev) +
         scale_fill_gradient2(as.character(Strategy), limits = c(0,1), low = "#f23a3a", mid = "white", high = "#3af256", midpoint = 0.5) +
         cowplot::theme_cowplot() +
         theme(axis.text.x = element_text(size = 8, color = "black", angle = -85, hjust = 0, vjust = 0.5),
               axis.text.y = element_text(size = 10, face = "bold", color = "black")))
  
  #Return the Rand by patient
  return(By_Patient_results)
},
options = list(optimize = 3))

Cell_image_plot_generator <- 
  cmpfun(function(Image_directory = NULL,
                  Channel_to_display = NULL,
                  Image_rotate = NULL,
                  Image_x_flip = FALSE,
                  Image_y_flip = FALSE,
                  Gamma_level = NULL,
                  Equalize = FALSE,
                  Black_level = NULL,
                  White_level = NULL,
                  
                  DATA = NULL,
                  Image_name = NULL,
                  Color_by = NULL,
                  Point_size = 1,
                  Pixel_distance_ratio = NULL
  ){
    on.exit(gc())
    
    DATA <- DATA
    #Check arguments
    if(!is.null(Image_rotate)) {
      if(!all(is.numeric(Image_rotate), Image_rotate >= 0, Image_rotate <= 360, Image_rotate%%1 == 0)) stop("Image_rotate must be NULL or a numeric value between 0 and 360")
    }
    if(!is.logical(Image_x_flip)) stop("Image_x_flip must be a logical value")
    if(!is.logical(Image_y_flip)) stop("Image_y_flip must be a logical value")
    if(!all(is.numeric(Gamma_level), Gamma_level >= -3, Gamma_level <= 3)) stop ("Gamma_level must be a numeric value between -3 and +3")
    if(!is.logical(Equalize)) stop("Equalize must be a logical value")
    if(!all(is.numeric(Black_level), Black_level >= 0, Black_level <= 100, Black_level < White_level)) stop("Black_level must be a numeric value between 0 - 100 and smaller than White_level")
    if(!all(is.numeric(White_level), White_level >= 0, White_level <= 100)) stop("White_level must be a numeric value between 0 - 100")
    
    if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))) stop("DATA must be adequately formatted")
    if(!Image_name %in% DATA[["Subject_Names"]]) stop(paste0(Image_name, " not found in DATA Subject_Names"))
    if(!any(is.null(Color_by), Color_by %in% names(DATA))) stop(paste0(Color_by, " not found in DATA"))
    if(!all(is.numeric(Point_size), Point_size > 0)) stop("Point_size must be a numeric value > 0")
    if(!is.null(Pixel_distance_ratio)) {
      if(!all(is.numeric(Pixel_distance_ratio), Pixel_distance_ratio > 0)) stop("Pixel_distance_ratio must be NULL or a numeric value > 0")
    }
    
    #Lets get the data and select only the cells and the selected column specifying the color
    DATA <- DATA %>% dplyr::filter(Subject_Names == Image_name)
    
    #Select the desired variables
    if(!is.null(Color_by)) DATA <- DATA %>% dplyr::select(1:4, all_of(Color_by)) else DATA <- DATA %>% dplyr::select(1:4)
    if(!is.null(Color_by)) names(DATA)[5] <- "Variable"
    
    #Adjust to pixel ratio if required
    if(!is.null(Pixel_distance_ratio)){
      DATA$X <- DATA$X * Pixel_distance_ratio
      DATA$Y <- DATA$Y * Pixel_distance_ratio
    }
    
    #Work on the image
    Image <- magick::image_read(Image_directory)[Channel_to_display] #Get original Image
    if(Image_x_flip) Image <- Image %>% magick::image_flop()
    if(Image_y_flip) Image <- Image %>% magick::image_flip()
    if(!is.null(Image_rotate)) Image <- Image %>% magick::image_rotate(degrees = Image_rotate)
    #Get image info
    Image_data <- magick::image_info(Image)
    Image_width <- Image_data$width
    Image_height <- Image_data$height
    
    #Perform image modifications
    if(Equalize) Image <- Image %>% magick::image_equalize() #Equalize if necesary
    Image <-  Image %>% magick::image_level(black_point = Black_level,
                                            white_point = White_level,
                                            mid_point = 10^Gamma_level) #Chane withe, black and gamma
    
    #If color needs to be applied
    if(!is.null(Color_by)){
      #If its numeric
      if(is.numeric(DATA[["Variable"]])){
        ggplot() +
          annotation_raster(Image, xmin = 0, xmax = Image_width, ymin = 0, ymax = Image_height, interpolate = TRUE) +
          geom_point(aes(x = X, y = Y, color = Variable), size = Point_size, data = DATA) +
          scale_color_viridis_c() +
          guides(color = guide_legend(title = Color_by)) +
          theme(axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.grid = element_blank())
      }
      
      #If is character
      else{
        ggplot() +
          annotation_raster(Image, xmin = 0, xmax = Image_width, ymin = 0, ymax = Image_height, interpolate = TRUE) +
          geom_point(aes(x = X, y = Y, color = Variable), size = Point_size, data = DATA) +
          scale_color_viridis_d() +
          guides(color = guide_legend(title = Color_by)) +
          theme(axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.grid = element_blank())
      }
    }
    
    #If color is not required
    if(is.null(Color_by)){
      ggplot() +
        annotation_raster(Image, xmin = 0, xmax = Image_width, ymin = 0, ymax = Image_height, interpolate = TRUE) +
        geom_point(aes(x = X, y = Y), color = "grey", size = Point_size, data = DATA) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              panel.grid = element_blank())
    }
    
  }, 
  options = list(optimize = 3))

############STEP 4 - HETEROGENEITY ANALYSIS - REQUIRED FUNCTIONS###########
message("Importing functions: STEP 4 - HETEROGENEITY ANALYSIS")
Global_heterogeneity_calculator <- cmpfun(
  function(DATA = NULL, Phenotypes_included = NULL){
    #Check if the phenotypes included are present in DATA
    if(!all(Phenotypes_included %in% unique(DATA$Phenotype))) {
      stop(paste0("Phenotypes included must be any of: ", str_c(unique(DATA$Phenotype), collapse = ", ")))
    }
    #If everything is OK perform the analysis
    else{
      
      #Generate the cell count data
      Interim <- DATA %>% dplyr::filter(Phenotype %in% Phenotypes_included) %>% 
        group_by(Subject_Names, Phenotype) %>% dplyr::count() %>% ungroup() %>%
        pivot_wider(id_cols = Subject_Names, names_from = Phenotype, values_from = n)
      #Substitute NA for 0
      Interim[is.na(Interim)] <- 0
      
      #Arrange the Interim matrix according to column name
      Interim <- bind_cols(Interim[1], Interim[sort(names(Interim[-1]))])
      
      #Calculate experiment wise cell proportion distribution (for KL and JS)
      Global_counts <- DATA %>% dplyr::filter(Phenotype %in% Phenotypes_included) %>% 
        dplyr::count(Phenotype) %>% mutate(n = n/sum(n)) %>% pivot_wider(names_from = Phenotype, values_from = n)
      #Sort Global_counts according to column name
      Global_counts <- Global_counts[sort(names(Global_counts))]
      Global_counts <- unlist(Global_counts)
      
      #Calculate the different metrics
      Results <- 
        bind_cols(Interim[1], 
                  tibble(Shannon = apply(Interim[-1], MARGIN = 1, function(row) vegan::diversity(row, index = "shannon")),
                         Simpson = apply(Interim[-1], MARGIN = 1, function(row) vegan::diversity(row, index = "simpson")),
                         Inverse_simpson = apply(Interim[-1], MARGIN = 1, function(row) vegan::diversity(row, index = "invsimpson"))
                  ),
                  tibble(renyi = as.double(vegan::renyi(Interim[-1], scales = Inf))),
                  tibble(Rao_Dkk = picante::raoD(Interim[-1])$Dkk),
                  tibble(Gini = unlist(apply(Interim[-1], MARGIN = 1, function(Sample) DescTools::Gini(Sample, conf.level = NA)))),
                  tibble(Kullback_Leibler_Sample = unlist(apply(Interim[-1], MARGIN = 1, function(Row){
                    Observed <- Row/sum(Row)
                    Expected <- rep(1/length(Row), times = length(Row))
                    suppressMessages(philentropy::KL(rbind(Observed, Expected), unit = "log2"))
                  }))),
                  tibble(Jensen_Shannon_Sample = unlist(apply(Interim[-1], MARGIN = 1, function(Row){
                    Observed <- Row/sum(Row)
                    Expected <- rep(1/length(Row), times = length(Row))
                    suppressMessages(philentropy::JSD(rbind(Observed, Expected),  test.na = FALSE, unit = "log2"))
                  }))),
                  tibble(Kullback_Leibler_Experiment = unlist(apply(Interim[-1], MARGIN = 1, function(Row){
                    Observed <- Row/sum(Row)
                    Expected <- Global_counts
                    suppressMessages(philentropy::KL(rbind(Observed, Expected), unit = "log2"))
                  }))),
                  tibble(Jensen_Shannon_Experiment = unlist(apply(Interim[-1], MARGIN = 1, function(Row){
                    Observed <- Row/sum(Row)
                    Expected <- Global_counts
                    suppressMessages(philentropy::JSD(rbind(Observed, Expected),  test.na = FALSE, unit = "log2"))
                  })))
        )
      names(Results)[-1] <- c("Shannon", "Simpson", "Inverse_simpson", "Renyi_Scale_Inf", "Rao_Dkk", "Gini", "KL_Sample", "JS_Sample", "KL_Experiment", "JS_Experiment")
      #Return the result
      return(Results)
    }
  },
  options = list(optimize = 3))

Barplot_Heterogeneity_generator <- 
  cmpfun(function(DATA = NULL, Metric = NULL) {
    if(!(Metric %in% names(DATA))){
      stop(paste0("Metrics not correctly stated. Choose from: ", str_c(names(DATA)[-1], collapse = ", ")))
    }
    
    else {
      
      DATA <- DATA  %>% dplyr::select(Subject_Names, all_of(Metric))
      names(DATA)[-1] <- "metric"
      
      DATA %>% 
        ggplot(aes(x = fct_reorder(Subject_Names, metric), y = metric, fill = metric)) + geom_col(color = "black", width = 0.5) +
        cowplot::theme_cowplot() +
        scale_x_discrete("Image") +
        scale_y_continuous("Global heterogeneity")+
        scale_fill_viridis_c(as.character(Metric)) +
        theme(axis.text.x = element_text(angle = -90, vjust = 0.5, size = 8, color = "black"),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 20))
    }
    
  },
  options = list(optimize = 3))

Image_length_calculator <- cmpfun(function(DATA = NULL) {
  DF <- bind_cols(unique(DATA$Subject_Names),
                  map_dfr(unique(DATA$Subject_Names), function(Image) {
                    Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
                    c(Width = max(Interim$X)- min(Interim$X),
                      Height = max(Interim$Y) - min(Interim$Y),
                      Surface = (max(Interim$X)- min(Interim$X))*(max(Interim$Y) - min(Interim$Y))
                    )
                  })
  )
  plot(DF %>% pivot_longer(-1) %>% ggplot(aes(x = value)) + facet_wrap(~name, "free", ncol = 1, nrow = 3) + geom_histogram(bins = 50) +
         cowplot::theme_cowplot()+
         scale_x_continuous("Size"))
  names(DF)[1] <- "Subject_Names"
  DF
},
options = list(optimize = 3))

Tile_generator_function <- cmpfun(
  function(Image_name, DATA = NULL, Tile_width = NULL, Tile_height = NULL, Variables_to_keep = NULL) {
    #Check arguments
    Interim <- DATA %>% dplyr::filter(Subject_Names == Image_name)#Select cells from image
    
    #Calculate X and Y centroids
    X_centroids <- seq(from = (min(Interim$X) + (Tile_width/2)), to = (max(Interim$X) + (Tile_width/2)), by =  Tile_width)
    Y_centroids <- seq(from = (min(Interim$Y) + (Tile_height/2)), to = (max(Interim$Y) + (Tile_height/2)), by =  Tile_height)
    
    #Find all possible combinations of X with Y to find all tile centroids
    centroids_tibble <- as_tibble(expand.grid(X_centroids, Y_centroids))
    names(centroids_tibble) <- c("tile_X_centroid", "tile_Y_centroid")
    #Describe centroid tiles and assign a tile ID
    Tile_info <- centroids_tibble %>% 
      mutate(tile_id = str_c("tile_", 1:nrow(centroids_tibble))) %>% 
      mutate(tile_xmin = floor(tile_X_centroid - (Tile_width/2)), tile_xmax = ceiling(tile_X_centroid + (Tile_width/2)),
             tile_ymin = floor(tile_Y_centroid - (Tile_height/2)), tile_ymax = ceiling(tile_Y_centroid + (Tile_height/2)))
    
    #Now assign the cells of the data a unique tile
    Interim_cells <- Interim
    Interim_tiles <- Tile_info
    Interim_cells <- Interim_cells %>% mutate(tile_id = 
                                                map2_chr(.x = Interim_cells$X, .y = Interim_cells$Y, function(.x, .y) {
                                                  x_position <- (.x >= Interim_tiles$tile_xmin) & (.x < Interim_tiles$tile_xmax)
                                                  y_position <- (.y >= Interim_tiles$tile_ymin) & (.y < Interim_tiles$tile_ymax)
                                                  Final_pos <- x_position == T & y_position == T
                                                  Tile <- Interim_tiles[Final_pos, 3]
                                                  Tile <- Tile[[1]]
                                                  Tile[[1]]
                                                })
    )
    Final_tibble <- left_join(Interim_cells, Interim_tiles, by = "tile_id")
    Final_tibble <- Final_tibble %>% dplyr::select(1:4, dplyr::all_of(Variables_to_keep), dplyr::contains("tile"))
    Final_tibble <- Final_tibble %>% 
      mutate(Final_status = (Final_tibble$X >= Final_tibble$tile_xmin) & (Final_tibble$X < Final_tibble$tile_xmax) &
               (Final_tibble$Y >= Final_tibble$tile_ymin) & (Final_tibble$Y < Final_tibble$tile_ymax))
    list(Tile_info,
         Final_tibble)
  },
  options = list(optimize = 3))

Suggested_Tile_Size_Calculator <- cmpfun(
  function(DATA = NULL, 
           N_rows = NULL,
           N_cols = NULL, 
           Based_on_smaller = NULL, 
           Draw_preview = NULL){
    #Check arguments
    if(!all(c("Subject_Names", "X", "Y") %in% names(DATA))) {
      stop("Data must contain columns named Subject_Names, X and Y")
    }
    
    else if(!all(c(N_rows%%1 == 0, N_cols%%1 == 0))) {
      stop("N_rows and N_cols arguments must be integer values")
    } 
    
    else if(!is.logical(Based_on_smaller)) {
      stop("Based_on_smaller should be a logical argument, either TRUE or FALSE")
    }
    
    else if(!is.logical(Draw_preview)) {
      stop("Draw_preview should be a logical argument, either TRUE or FALSE")
    }
    
    #Define what to do if smaller image is to be used
    if(Based_on_smaller) {
      Tile_width <- round(
        min(
          map_dbl(unique(DATA$Subject_Names), function(Image){
            Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
            max(Interim$X)-min(Interim$X)
          })
        )/N_cols,
        digits = 0
      )
      
      Tile_height <-round(
        min(
          map_dbl(unique(DATA$Subject_Names), function(Image){
            Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
            max(Interim$Y)-min(Interim$Y)
          })
        )/N_rows,
        digits = 0
      )
      
      Suggestion_tibble <- tibble('Suggested Tile dimension' = c("Width", "Height", "Squared_tiles"),
                                  value = c(Tile_width, Tile_height, mean(c(Tile_width, Tile_height))))
      
      
      #Draw a preview if required
      if(Draw_preview) {
        #Calculate the area of all images
        Area_tibble <- map_dfr(unique(DATA$Subject_Names), function(Image){
          Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
          Area <- (max(Interim$X) - min(Interim$X)) * (max(Interim$Y) - min(Interim$Y))
          tibble(Subject_Names = Image,
                 Area = Area)
        })
        Area_tibble <- Area_tibble %>% dplyr::arrange(Area)
        
        Interim <- DATA %>% dplyr::filter(Subject_Names == Area_tibble[[1,1]])
        Tile_width <- Suggestion_tibble[[3,2]]
        
        Tiled_plot <- Interim %>% ggplot(aes(x = X, y = Y)) + geom_bin2d(fill = "white", color = "black", linewidth = 1.1, binwidth = Tile_width) +
          geom_point(size = 2, alpha = 0.5) + cowplot::theme_cowplot()
        
        Summary_smaller <- dplyr::as_tibble(ggplot2::layer_data(Tiled_plot)) %>% summarize(N_tiles = length(count),
                                                                                           Min_cells = min(count),
                                                                                           p25_cells = quantile(count, 0.25),
                                                                                           Average_cells = mean(count),
                                                                                           p50_cells = quantile(count, 0.5),
                                                                                           p75_cells = quantile(count, 0.75),
                                                                                           Max_cells = max(count))
        #Plot the tiled version of our data
        plot(
          Tiled_plot
        )
        #Generate the final tibble
        list_final <- list(Tile_dimension = Suggestion_tibble,
                           Image = Summary_smaller)
        names(list_final)[2] <- Area_tibble[[1,1]] 
        
        #Return the final tibble
        return(list_final)
      }
      
      #Return the final results
      return(Suggestion_tibble)
    }
    
    if(!Based_on_smaller) {
      Tile_width <- round(
        max(
          map_dbl(unique(DATA$Subject_Names), function(Image){
            Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
            max(Interim$X)-min(Interim$X)
          })
        )/N_cols,
        digits = 0
      )
      
      Tile_height <-round(
        max(
          map_dbl(unique(DATA$Subject_Names), function(Image){
            Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
            max(Interim$Y)-min(Interim$Y)
          })
        )/N_rows,
        digits = 0
      )
      
      Suggestion_tibble <- tibble('Suggested Tile dimension' = c("Width", "Height", "Squared_tiles"),
                                  value = c(Tile_width, Tile_height, mean(c(Tile_width, Tile_height))))
      
      #Draw a preview if required
      if(Draw_preview) {
        Area_tibble <- map_dfr(unique(DATA$Subject_Names), function(Image){
          Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
          Area <- (max(Interim$X) - min(Interim$X)) * (max(Interim$Y) - min(Interim$Y))
          tibble(Subject_Names = Image,
                 Area = Area)
        })
        Area_tibble <- Area_tibble %>% dplyr::arrange(dplyr::desc(Area))
        
        #Get the interim
        Interim <- DATA %>% dplyr::filter(Subject_Names == Area_tibble[[1,1]])
        Tile_width <- Suggestion_tibble[[3,2]]
        
        Tiled_plot <- Interim %>% ggplot(aes(x = X, y = Y)) + geom_bin2d(fill = "white", color = "black", linewidth = 1.1, binwidth = Tile_width) +
          geom_point(size = 2, alpha = 0.5) + cowplot::theme_cowplot()
        Summary_smaller <- dplyr::as_tibble(ggplot2::layer_data(Tiled_plot)) %>% summarize(N_tiles = length(count),
                                                                                           Min_cells = min(count),
                                                                                           p25_cells = quantile(count, 0.25),
                                                                                           Average_cells = mean(count),
                                                                                           p50_cells = quantile(count, 0.5),
                                                                                           p75_cells = quantile(count, 0.75),
                                                                                           Max_cells = max(count))
        #Plot the tiled version of our data
        plot(
          Tiled_plot
        )
        #Generate the final tibble
        list_final <- list(Tile_dimension = Suggestion_tibble,
                           Image = Summary_smaller)
        names(list_final)[2] <- Area_tibble[[1,1]] 
        
        #Return the final tibble
        return(list_final)
      }
      
      #Return the final results
      return(Suggestion_tibble)
    }
  },
  options = list(optimize = 3))

Image_tiling_processing_function <- cmpfun(function(N_cores = NULL,
                                                    DATA = NULL,
                                                    Tile_width = NULL,
                                                    Tile_height = NULL,
                                                    Variables_to_keep = NULL) {
  #Check arguments
  if(!exists(DATA, envir = .GlobalEnv)){
    stop(paste0("Check name supplied to the DATA argument", DATA, " does not exist in the Global Environment"))
  }
  if(!exists('Tile_generator_function', envir = .GlobalEnv)) {
    stop("Please execute the required dependencies Tile_generator_function")
  }
  #Load Data phenotypes and tile generator function to the environment created in the function
  DATA <- get(DATA, envir = .GlobalEnv)
  Tile_generator_function <- get('Tile_generator_function', envir = .GlobalEnv)
  if(!(is.numeric(Tile_width) & is.numeric(Tile_height))) stop("Both Tile_width and Tile_height must be numeric values")
  if(!(Tile_width%%1 == 0 & Tile_height%%1 ==0)) message("It is highly recommended that Tile_width and Tile_height are integer values")
  if(!(Tile_width > 0 & Tile_height > 0)) stop("Tile_width and Tile_height must be > 0")
  if(N_cores%%1 != 0) stop("N_cores must be an integer value")
  if(!all(Variables_to_keep %in% names(DATA))) stop(paste0(str_c(Variables_to_keep[!Variables_to_keep %in% names(DATA)], collapse = ", "),
                                                           " not found in DATA variables"))
  
  else {
    if (!all(c("X", "Y", "Subject_Names", Variables_to_keep) %in% names(DATA))) {
      stop("DATA supplied must be follow the structure specified in step 0 and contain Variables_to_keep")
    } else {
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      Tiled_Images <-
        furrr::future_map(unique(DATA$Subject_Names), function(x)
          Tile_generator_function(
            Image_name = x,
            DATA = DATA,
            Tile_width = Tile_width,
            Tile_height = Tile_height,
            Variables_to_keep = Variables_to_keep
          ),
          .progress = TRUE)
      
      future::plan("future::sequential")
      gc()
      
      names(Tiled_Images) <- unique(DATA$Subject_Names)
      return(Tiled_Images)
    }
  }
},
options = list(optimize = 3))

Tiled_image_heterogeneity_calculator <- 
  cmpfun(
    function(Tiled_images = NULL,
             Minimum_cell_no_per_tile = NULL,
             Phenotypes_included = NULL) {
      #Check arguments
      #Check that the phenotypes included are present in the data
      if(!all(Phenotypes_included %in% unique(unlist(map(Tiled_images, function(df) df[[2]]$Phenotype))))) {
        stop(paste0("Phenotypes included must be any of: ", str_c(unique(unlist(map(Tiled_images, function(df) df[[2]]$Phenotype))), collapse = ", ")))
      }
      #Check that Min cells are integer values
      if(!all(Minimum_cell_no_per_tile%%1 == 0, Minimum_cell_no_per_tile > 0)) stop("Minimum_cell_no_per_tile must be an integer value > 0")
      
      #Else proceed with analysis
      Results <- 
        map(1:length(Tiled_Images), function(x) {
          
          Image <- Tiled_Images[[x]]
          Interim <- Image[[2]] %>% dplyr::filter(Phenotype %in% Phenotypes_included)
          
          #Generate the global cell count expected proportions (required for KL and JSD)
          Cell_counts_by_image <- Interim %>% count(Phenotype)
          Expected_proportions <- Cell_counts_by_image[["n"]] / sum(Cell_counts_by_image[["n"]])
          names(Expected_proportions) <- str_c("PER_", Cell_counts_by_image[["Phenotype"]], sep = "")
          Expected_proportions <- Expected_proportions[sort(names(Expected_proportions))]
          
          #Filter out tiles with less than minimum cells per tile
          Filtered_tiles <-
            Interim  %>%
            group_by(tile_id) %>% dplyr::count() %>% ungroup() %>% dplyr::filter(n >= Minimum_cell_no_per_tile) 
          
          #If not enough tiles are present in the image print a warning message
          if(nrow(Filtered_tiles) == 0) {
            warning(paste0("No valid tiles present in: ", names(Tiled_Images)[x], ", hence it will be removed from analysis. Please consider lowering Minimum_cell_no_per_tile threshold or increasing the cell types included in analysis"))
            return(NULL)
          }
          
          else{
            
            #Build cell count by tile matrix
            Interim2 <-
              Interim %>% dplyr::filter(tile_id %in% Filtered_tiles[[1]]) %>% group_by(tile_id, Phenotype) %>% dplyr::count() %>%
              pivot_wider(id_cols = tile_id,
                          names_from = Phenotype,
                          values_from = n)
            Interim2[is.na(Interim2)] <- 0
            
            #Calculate the percentage of the total cells in the tile that belong to each phenotype and build a tibble with the info
            Interim_per <- map_dfc(Interim2[-1], function(Column) Column / rowSums(Interim2[-1]))
            names(Interim_per) <- str_c("PER_", names(Interim2)[-1], sep = "")
            Interim_per <- Interim_per[sort(names(Interim_per))]
            
            #Add missing columns if any cell type has gone missing after tile removal process (is required for KL and JSD analyses that are based on proportion)
            if(!all(names(Expected_proportions) %in% names(Interim_per))){
              Interim_missing <- suppressWarnings(as_tibble(matrix(0, nrow = nrow(Interim_per), ncol = sum(!names(Expected_proportions) %in% names(Interim_per)))))
              names(Interim_missing) <- names(Expected_proportions)[!names(Expected_proportions) %in% names(Interim_per)]
              Interim_per <- bind_cols(Interim_per, Interim_missing)
              Interim_per <- Interim_per[sort(names(Interim_per))]
            }
            
            #Calculate the total cells per tile
            Total_cells_tibble <- tibble(n_cells = rowSums(Interim2[-1]))
            
            #Calculate the metrics and generate the result tibble
            Results <- bind_cols(
              Interim2[1],
              tibble(Shannon = apply(Interim2[-1], MARGIN = 1, function(row) vegan::diversity(row, index = "shannon")),
                     Simpson = apply(Interim2[-1], MARGIN = 1, function(row) vegan::diversity(row, index = "simpson")),
                     Inverse_simpson = apply(Interim2[-1], MARGIN = 1, function(row) vegan::diversity(row, index = "invsimpson"))
              ),
              tibble(renyi = as.double(vegan::renyi(Interim2[-1], scales = Inf))),
              tibble(rao_Dkk = picante::raoD(Interim2[-1])$Dkk),
              tibble(Gini = unlist(apply(Interim2[-1], MARGIN = 1, function(Sample) DescTools::Gini(Sample, conf.level = NA)))),
              tibble(Kullback_Leibler = unlist(apply(Interim_per, MARGIN = 1, function(Row){
                Observed <- Row
                Expected <- Expected_proportions
                suppressMessages(philentropy::KL(rbind(Observed, Expected), unit = "log2"))
              }))),
              tibble(Jensen_Shannon = unlist(apply(Interim_per, MARGIN = 1, function(Row){
                Observed <- Row
                Expected <- Expected_proportions
                suppressMessages(philentropy::JSD(rbind(Observed, Expected),  test.na = FALSE, unit = "log2"))
              }))),
              Interim2[-1],
              Total_cells_tibble,
              Interim_per
            )
            
            #Change names
            names(Results)[2:9] <-
              c("Shannon",
                "Simpson",
                "Inverse_simpson",
                "Renyi_Scale_Inf",
                "Rao_Dkk",
                "Gini",
                "Kullback_Leibler", 
                "Jensen_Shannon")
            
            #Bind results to the tile info matrix and eliminate rows with NA
            Image_results <- left_join(Image[[1]], Results, by = "tile_id")
            #Safely remove images with less than one evaluable metric
            Image_results <- Image_results[apply(Image_results, MARGIN = 1, function(row) sum(is.na(row)) < 9),]
            #reorder the output and return
            Image_results <- Image_results %>% dplyr::select(1:7, 
                                                             all_of(c("Shannon",
                                                                      "Simpson",
                                                                      "Inverse_simpson",
                                                                      "Renyi_Scale_Inf",
                                                                      "Rao_Dkk",
                                                                      "Gini",
                                                                      "Kullback_Leibler", 
                                                                      "Jensen_Shannon")),
                                                             n_cells,
                                                             names(Image_results)[names(Image_results) %in% Phenotypes_included],
                                                             contains("PER_"))
            return(Image_results)
            
          }
        }, 
        .progress = list(clear = F,
                         name = "Calculating heterogeneity by tile",
                         show_after = 2,
                         type = "iterator"))
      
      names(Results) <- names(Tiled_Images)
      
      #Return results except for NULL values
      return(Results[!map_lgl(Results, is.null)])
    }, 
    options = list(optimize = 3))

Tiled_image_heterogeneity_graph_maker <- cmpfun(
  function(DATA = NULL,
           Tiled_images = NULL,
           Image_name = NULL,
           Metric = NULL){
    #Check arguments
    if(!all(Image_name %in% DATA$Subject_Names, Image_name %in% names(Tiled_images))) stop("Image_name not found in DATA or Tiled_images")
    if(!Metric %in% names(Tiled_images[[1]])) stop("Metric not found in DATA")
    
    Cells <- DATA %>% dplyr::filter(Subject_Names == Image_name) %>% dplyr::select(1:4, Phenotype)
    Tiles <- Tiled_images[[Image_name]]
    Cells <- Cells %>% dplyr::filter(Phenotype %in% names(Tiles))
    
    Tiles <- cbind(Tiles[1:7], Tiles[Metric])
    names(Tiles)[8] <- "value"
    
    Cells  %>%
      ggplot() + 
      geom_rect(aes(group = tile_id, xmin = tile_xmin, ymin = tile_ymin, xmax = tile_xmax, ymax = tile_ymax, fill = value), color = "black", 
                alpha = 0.5, data = Tiles) + 
      geom_point(aes(x = X, y = Y, color = Phenotype), size = 2, alpha = 0.8)+
      cowplot::theme_cowplot()+
      scale_x_continuous("", labels = NULL) +
      scale_y_continuous("", labels = NULL) +
      scale_color_manual("Cell Type", values = unname(pals::polychrome(length(unique(Cells$Phenotype)))))+
      guides(color = guide_legend(override.aes = list(size = 12)))+
      theme(panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20)) +
      scale_fill_gradient2(Metric, 
                           limits = c(min(Tiles$value), max(Tiles$value)), 
                           low = "blue", high = "red", mid = "white", 
                           midpoint = quantile(Tiles$value, 0.5))
    
  },
  options = list(optimize = 3))

Tiled_image_heterogeneity_analyzer <- 
  cmpfun(
    function(Tiled_heterogeneity_DATA = NULL,
             Strategy = NULL,
             Metric = NULL,
             Threshold = NULL) {
      
      #Check arguments
      if (!(Strategy %in% c("Quantify_by_Threshold", "Overall_Summary"))) {
        stop("Strategy should either Quantify_by_Threshold or Overall_Summary")
      }
      else if(!all(map_lgl(seq_along(1:length(Tiled_heterogeneity_DATA)), function(Image) Metric %in% names(Tiled_heterogeneity_DATA[[Image]])))) {
        stop("Metric should be contained in the Tiled_heterogeneity_DATA object")
      }
      
      
      else if(Strategy == "Quantify_by_Threshold") {
        if(!is.numeric(Threshold)) stop("Threshold must be a numeric value")
        
        map_dfr(seq_along(1:length(Tiled_heterogeneity_DATA)), function(Image) {
          
          #Import tiled image data and select requested heterogeneity metric
          Image_filtered <- Tiled_heterogeneity_DATA[[Image]] %>% dplyr::select(1:7, all_of(Metric))
          names(Image_filtered)[8] <- "value"
          
          #Prepare data to perform Morans I
          For_Morans <- Image_filtered %>% mutate(value = case_when(value >= Threshold ~ 1,
                                                                    TRUE ~ 0))
          if(length(unique(For_Morans$value)) > 1) {
            
            #Prepare data for Morans I calculation
            DIST_Morans <- as.matrix(dist(cbind(For_Morans$tile_X_centroid, For_Morans$tile_Y_centroid), method = "euclidean"))#We calculate distance matrix
            INVERSE_DIST <- 1/DIST_Morans #We calculate inverse of distance
            diag(INVERSE_DIST) <- 0
            results <- ape::Moran.I(For_Morans$value, INVERSE_DIST, na.rm = T)#Calculate MORAN's I
            
            #Generate result tibble
            Results_tibble <- tibble(Subject_Names = names(Tiled_heterogeneity_DATA)[Image],
                                     Tiles_above_Threshold = sum(Image_filtered$value >= Threshold),
                                     Total_tiles = nrow(Image_filtered),
                                     Percentage_above_Threshold = Tiles_above_Threshold / Total_tiles,
                                     MoransI_obseved = results$observed, 
                                     MoransI_expected = results$expected, 
                                     MoransI_sd = results$sd,
                                     MoransI_pval = results$p.value,
                                     Threshold = Threshold,
                                     Metric = Metric)
          }
          
          else if(length(unique(For_Morans$value)) == 1){
            
            #No need to calculate Morans I 
            Results_tibble <- tibble(Subject_Names = names(Tiled_heterogeneity_DATA)[Image],
                                     Tiles_above_Threshold = sum(Image_filtered$value >= Threshold),
                                     Total_tiles = nrow(Image_filtered),
                                     Percentage_above_Threshold = Tiles_above_Threshold / Total_tiles,
                                     MoransI_obseved = NA, 
                                     MoransI_expected = NA, 
                                     MoransI_sd = NA,
                                     MoransI_pval = NA,
                                     Threshold = Threshold,
                                     Metric = Metric)
          }
          
        }, 
        .progress = list(clear = F,
                         name = "Calculating heterogeneity summary acording to threshold",
                         show_after = 2,
                         type = "iterator"))
        
      }
      
      else if(Strategy == "Overall_Summary") {
        map_dfr(seq_along(1:length(Tiled_heterogeneity_DATA)), function(Image) {
          Image_filtered <- Tiled_heterogeneity_DATA[[Image]] %>% dplyr::select(1:7, all_of(Metric))
          
          names(Image_filtered)[8] <- "value"
          
          if(length(unique(Image_filtered$value)) > 1) {
            
            #Prepare data for Morans I calculation
            DIST_Morans <- as.matrix(dist(cbind(Image_filtered$tile_X_centroid, Image_filtered$tile_Y_centroid), method = "euclidean"))#We calculate distance matrix
            INVERSE_DIST <- 1/DIST_Morans #We calculate inverse of distance
            diag(INVERSE_DIST) <- 0
            results <- ape::Moran.I(Image_filtered$value, INVERSE_DIST, na.rm = T)#Calculate MORAN's I
            
            Results_tibble <- tibble(Subject_Names = names(Tiled_heterogeneity_DATA)[Image],
                                     Min_value = min(Image_filtered$value, na.rm = TRUE),
                                     p25 = quantile(Image_filtered$value, 0.25, na.rm = TRUE),
                                     Average = mean(Image_filtered$value, na.rm = TRUE),
                                     p50 = quantile(Image_filtered$value, 0.50, na.rm = TRUE),
                                     p75 = quantile(Image_filtered$value, 0.75, na.rm = TRUE),
                                     Max_value = max(Image_filtered$value, na.rm = TRUE),
                                     sd_value = sd(Image_filtered$value, na.rm = TRUE),
                                     Total_tiles = nrow(Image_filtered),
                                     Evaluable_tiles = nrow(Image_filtered) - sum(is.na(Image_filtered$value)),
                                     MoransI_obseved = results$observed, 
                                     MoransI_expected = results$expected, 
                                     MoransI_sd = results$sd,
                                     MoransI_pval = results$p.value,
                                     Metric = Metric
            )
          }
          
          else if(length(unique(Image_filtered$value)) == 1) {
            
            #no need to calculate Morans I
            Results_tibble <- tibble(Subject_Names = names(Tiled_heterogeneity_DATA)[Image],
                                     Min_value = min(Image_filtered$value, na.rm = TRUE),
                                     p25 = quantile(Image_filtered$value, 0.25, na.rm = TRUE),
                                     Average = mean(Image_filtered$value, na.rm = TRUE),
                                     p50 = quantile(Image_filtered$value, 0.50, na.rm = TRUE),
                                     p75 = quantile(Image_filtered$value, 0.75, na.rm = TRUE),
                                     Max_value = max(Image_filtered$value, na.rm = TRUE),
                                     sd_value = sd(Image_filtered$value, na.rm = TRUE),
                                     Total_tiles = nrow(Image_filtered),
                                     Evaluable_tiles = nrow(Image_filtered) - sum(is.na(Image_filtered$value)),
                                     MoransI_obseved = NA, 
                                     MoransI_expected = NA, 
                                     MoransI_sd = NA,
                                     MoransI_pval = NA,
                                     Metric = Metric
            )
          }
          
        }, 
        .progress = list(clear = F,
                         name = "Calculating heterogeneity overall summary",
                         show_after = 2,
                         type = "iterator"))
      }
    },
    options = list(optimize = 3))

Texture_features_calculator <- cmpfun(
  function(Tiled_images = NULL,
           Phenotype_included = NULL) {
    #Check that the phenotypes included are present in the data
    if(!all(Phenotype_included %in% unique(unlist(map(Tiled_images, function(df) df[[2]]$Phenotype))))) {
      stop(paste0("Phenotypes included must be any of: ", str_c(unique(unlist(map(Tiled_images, function(df) df[[2]]$Phenotype))), collapse = ", ")))
    }
    
    #First import our data
    Tiled_images <- Tiled_images
    #Now we need to calculate the number of cells of interest by tile and transform the tile pattern into a pseudo image, then we calculate the glcm and the texture metrics
    Texture_metric_results <- 
      map_dfr(1:length(Tiled_images), function(Image) {
        #Calculate the cell count by tile
        Cell_count_by_tile <- Tiled_images[[Image]][[2]] %>% dplyr::filter(Phenotype == Phenotype_included) %>%
          group_by(tile_id) %>% dplyr::count(Phenotype)
        
        #Bind the tile matrix with each cell count
        Result <- dplyr::left_join(Tiled_images[[Image]][[1]], Cell_count_by_tile, by = "tile_id") %>% dplyr::select(-Phenotype)
        Result[is.na(Result)] <- 0
        
        #Start generating the pseudo image
        x_length <- length(unique(Result$tile_X_centroid))#Calculte the X pixel length
        y_length <- length(unique(Result$tile_Y_centroid))#Calculate the Y pixel length
        Interim <- Result %>% arrange(tile_X_centroid, desc(tile_Y_centroid))#arrange the tibble in an adequate format
        
        #Build the pseudo image according to the cell number by the cell count
        Pseudo_image <- matrix(Interim[[8]], nrow = y_length, ncol = x_length)#Calculate the matrix 
        colnames(Pseudo_image) <- unique(Interim$tile_X_centroid)
        rownames(Pseudo_image) <- unique(Interim$tile_Y_centroid)
        
        #If our matrix contain diverse pixel types then execute the functions
        if(length(unique(as.vector(Pseudo_image))) > 1){
          #Now transform our matrix into a raster object and calculate the texture features based on the gray-level-coocurrence-matrix
          Raster_values <- glcm::glcm(raster::raster(Pseudo_image), n_grey = length(unique(as.vector(Pseudo_image))), window = c(3, 3), shift = c(1, 1), statistics =
                                        c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy",
                                          "second_moment", "correlation"), na_opt = "center", na_val = NA, scale_factor = 1, asinteger = FALSE)
          
          #We average the results of the different texture metrics
          Averaged_results <- map_dbl(names(Raster_values), function(characteristics) {
            Interim <- tabularaster::as_tibble(raster::subset(Raster_values, characteristics, drop = F), cell = F)
            mean(Interim[[1]], na.rm = T)#Calculate mean values for all texture metrics
          })
          names(Averaged_results) <- str_c("Mean_", names(Raster_values))
        }
        
        #If only a unique value present in the sample print message and return a NA tibble
        else{
          message(paste0(names(Tiled_images)[Image], ": the GLCM based texture features could not be calculated for the selected cell type"))
          Averaged_results <- tibble(Mean_glcm_mean = NA,
                                     Mean_glcm_variance = NA,
                                     Mean_glcm_homogeneity = NA,
                                     Mean_glcm_contrast = NA,
                                     Mean_glcm_dissimilarity = NA,
                                     Mean_glcm_entropy = NA, 
                                     Mean_glcm_second_moment = NA, 
                                     Mean_glcm_correlation = NA)
        }
        
        return(Averaged_results)
      },
      .progress = list(clear = F,
                       name = "Calculating texture features",
                       show_after = 2,
                       type = "iterator")
      )
    
    
    #Bind the results to the Image name and return the final result
    return(bind_cols(tibble(Subject_names = names(Tiled_images)),
                     Texture_metric_results))
    
  }, options = list(optimize = 3))

Tiled_images_graphicator <- cmpfun(
  function(Tiled_images = NULL,
           Image_name = NULL,
           Phenotypes_included = NULL) {
    if(!Image_name %in% names(Tiled_images)) stop("Image_name not found in Tiled_images")
    
    #Import the image data
    Tiled_Images <- Tiled_images[[Image_name]]
    
    #Check that the phenotypes included are present in the data
    if(!all(Phenotypes_included %in% unique(Tiled_Images[[2]]$Phenotype))) {
      stop(paste0("Phenotypes included must be any of: ", str_c(unique(Tiled_Images[[2]]$Phenotype), collapse = ", ")))
    }
    
    else{
      #Generate a cell count by tile info (we need to change the counting system to account for tiles were there are no cells and dissapear from count)
      Interim <- Tiled_Images[[2]] %>% dplyr::select(tile_id, Phenotype) %>% dplyr::filter(Phenotype %in% Phenotypes_included) %>%
        group_by(tile_id) %>%
        dplyr::count(Phenotype) %>% pivot_wider(names_from = Phenotype, values_from = n)
      
      #Generate a tibble and account for cero values
      Interim <- dplyr::left_join(tibble(tile_id = unique(Tiled_Images[[2]]$tile_id)),
                                  Interim,
                                  by = "tile_id")
      Interim[is.na(Interim)] <- 0
      
      #Join the cell count tibble with the grid information
      For_graph <- dplyr::left_join(Tiled_Images[[1]], Interim, by = "tile_id")
      
      
      #Generate a plot for each of the phenotypes
      Plot_list <- map(8:ncol(For_graph), function(Variable){
        #Generate a tibble containing tile info and only a single marker
        Graph_tibble <- bind_cols(For_graph[1:7], For_graph[Variable])
        names(Graph_tibble)[8] <- "Variable"
        
        #Generate the individual cell tible
        Individual_cells <- Tiled_Images[[2]] %>% dplyr::filter(Phenotype == names(For_graph[Variable]))
        
        Plot <- Graph_tibble %>% ggplot() +
          geom_rect(aes(group = tile_id, xmin = tile_xmin, ymin = tile_ymin, xmax = tile_xmax, ymax = tile_ymax, fill = Variable), color = "black", 
                    alpha = 0.5) +
          geom_point(aes(x = X, y = Y), alpha = 0.4, size = 1.5, data = Individual_cells) +
          cowplot::theme_cowplot()+
          scale_x_continuous("", labels = NULL) +
          scale_y_continuous("", labels = NULL) +
          scale_fill_viridis_c(names(For_graph)[Variable], na.value = "white") +
          ggtitle(names(For_graph)[Variable])+
          theme(panel.grid = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 20),
                plot.title = element_text(size = 25, hjust = 0.5, vjust = -3)) 
        Plot
      })
      cowplot::plot_grid(plotlist = Plot_list)
    }
  },
  options = list(optimize = 3))

SPIAT_object_generator <- cmpfun(
  function(DATA_Intensities = NULL,
           DATA_Phenotypes = NULL) {
    #Check arguments
    if(!identical(names(DATA_Intensities)[c(1:4)], c("Cell_no", "X", "Y", "Subject_Names"))) {
      stop("DATA_Intensities should be adequately formated using Data_arrange_function from Step 0")
    }
    if(!identical(names(DATA_Phenotypes)[c(1:4)], c("Cell_no", "X", "Y", "Subject_Names"))) {
      stop("Column Names in Data_Phenotypes are not correctly stated (should contain Cell_no, X, Y and Subject_Names")
    }
    if(!"Phenotype" %in% names(DATA_Phenotypes)) stop("DATA_Phenotypes must contain a Phenotype column")
    
    
    #Select data
    DATA_Phenotypes <- DATA_Phenotypes
    DATA_Intensities <- DATA_Intensities[names(DATA_Phenotypes)[-ncol(DATA_Phenotypes)]]
    
    Formatted_image <- 
      map(unique(DATA_Phenotypes$Subject_Names), function(Image) {
        
        #Select individual images
        RESULTS_INTERIM <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
        Results_matrix <- DATA_Intensities %>% dplyr::filter(Subject_Names == Image)
        
        #Generate the intensity matrix
        Results_matrix <- t(as.matrix(Results_matrix[-c(1:4)]))
        colnames(Results_matrix) <- RESULTS_INTERIM[[1]]
        
        #Generate the SPIAT IMAGE
        Formatted_image <- SPIAT::format_image_to_spe(intensity_matrix = Results_matrix, 
                                                      phenotypes = RESULTS_INTERIM[["Phenotype"]], 
                                                      coord_x = RESULTS_INTERIM[[2]], 
                                                      coord_y = RESULTS_INTERIM[[3]])
        Formatted_image
        
      }, .progress = list(clear = F,
                          name = "Generating SPIAT type objects",
                          show_after = 1,
                          type = "iterator"))
    
    names(Formatted_image) <- unique(DATA_Phenotypes$Subject_Names)
    return(Formatted_image)
    
  },
  options = list(optimize = 3))

Own_grid_metrics <- 
  function(spe_object, FUN, n_split, ...) {
    split <- SPIAT::image_splitter(spe_object, n_split)
    list.metric <- list()
    for (i in seq_len(length(split))) {
      spe <- split[[i]]
      if (is.na(spe)) {
        spe <- NULL
      }
      if (methods::is(spe, "SpatialExperiment")) {
        metric <- quiet_basic(FUN(spe, ...))
        if (length(metric) == 0) {
          metric <- 0
        }
        list.metric[[i]] <- metric
      }
      else {
        list.metric[[i]] <- 0
      }
    }
    x <- raster::raster(ncol = n_split, nrow = n_split, xmn = 0, 
                        ymn = 0, xmx = max(SpatialExperiment::spatialCoords(spe_object)[, 
                                                                                        "Cell.X.Position"]), ymx = max(SpatialExperiment::spatialCoords(spe_object)[, 
                                                                                                                                                                    "Cell.X.Position"]))
    raster::values(x) <- unlist(list.metric)
    y <- raster::flip(x, direction = "y")
    return(y)
  }

quiet_basic <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

SPIAT_Heterogeneity_Analyzer <- cmpfun(
  function(N_cores = NULL,
           DATA_SPIAT = NULL,
           DATA_Phenotypes = NULL,
           Tile_size = NULL,
           Phenotypes_included = NULL,
           Autocorrelation_metric = NULL,
           Entropy_threshold = NULL) {
    #Check arguments by generating a argument check vector and message vector
    Argument_checker <- c(N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0),
                          DATA_SPIAT_OK = all(map_lgl(DATA_SPIAT, function(Image) class(Image) == "SpatialExperiment")),
                          DATA_Phenotypes_OK = "Phenotype" %in% names(DATA_Phenotypes),
                          Tile_size_OK = all(is.numeric(Tile_size), Tile_size > 0),
                          Phenotypes_included_OK = all(Phenotypes_included %in% DATA_Phenotypes$Phenotype),
                          Autocorrelation_metric_OK = Autocorrelation_metric %in% c("globalmoran", "GearyC"),
                          Entropy_threshold_OK = is.numeric(Entropy_threshold)
    )
    
    Stop_messages <- c(N_cores_OK = "N_cores must be an integer value > 0",
                       DATA_SPIAT_OK = "DATA_SPIAT must be generated with the SPIAT_object_generator function",
                       DATA_Phenotypes_OK = "DATA_Phenotypes must contain a Phenotype object",
                       Tile_size_OK = "Tile_size must be a numeric value > 0",
                       Phenotypes_included_OK = str_c("Phenotypes must be any of the following: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")),
                       Autocorrelation_metric_OK = "Autocorrelation_metric must be one of the following: globalmoran, GearyC",
                       Entropy_threshold_OK = "Entropy_threshold must be a numeric value")
    
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    
    #Import list containing SPIAT OBJECTS
    DATA_SPIAT <- DATA_SPIAT
    
    #Generate list containing Data_phenotypes objects
    DATA_Phenotypes <- map(names(DATA_SPIAT), function(Image) DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image))
    
    #Import my own version of grid_metrics
    Own_grid_metrics <- get("Own_grid_metrics")
    quiet_basic <- get("quiet_basic")
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #prepare the cluster
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <-
      furrr::future_map(seq_along(1:length(DATA_SPIAT)), function(Index){
        library(SPIAT)
        #Define functions inside the cores
        Own_grid_metrics <- 
          function(spe_object, FUN, n_split, ...) {
            split <- SPIAT::image_splitter(spe_object, n_split)
            list.metric <- list()
            for (i in seq_len(length(split))) {
              spe <- split[[i]]
              if (is.na(spe)) {
                spe <- NULL
              }
              if (methods::is(spe, "SpatialExperiment")) {
                metric <- quiet_basic(FUN(spe, ...))
                if (length(metric) == 0) {
                  metric <- 0
                }
                list.metric[[i]] <- metric
              }
              else {
                list.metric[[i]] <- 0
              }
            }
            x <- raster::raster(ncol = n_split, nrow = n_split, xmn = 0, 
                                ymn = 0, xmx = max(SpatialExperiment::spatialCoords(spe_object)[, 
                                                                                                "Cell.X.Position"]), ymx = max(SpatialExperiment::spatialCoords(spe_object)[, 
                                                                                                                                                                            "Cell.X.Position"]))
            raster::values(x) <- unlist(list.metric)
            y <- raster::flip(x, direction = "y")
            return(y)
          }
        quiet_basic <- function(x) { 
          sink(tempfile()) 
          on.exit(sink()) 
          invisible(force(x)) 
        } 
        
        #obtain individual SPIAT object
        SPIAT_Object <- DATA_SPIAT[[Index]]
        
        #Obtain Individual Phenotype object
        Phenotype_Object <- DATA_Phenotypes[[Index]]
        
        #Calculate Image area (approximation)
        Width <- max(Phenotype_Object$X) - min(Phenotype_Object$X)
        Height <- max(Phenotype_Object$Y) - min(Phenotype_Object$Y)
        Area <- Width * Height
        
        #Calculate the number of constant size tiles needed to cover the total area
        N_tiles <- ceiling(Area / (Tile_size^2))
        
        #Perform grid metrics
        grid_object <- Own_grid_metrics(SPIAT_Object, FUN = calculate_entropy, n_split = N_tiles,
                                        cell_types_of_interest = Phenotypes_included,
                                        feature_colname = "Phenotype")
        
        #Calculate percentage of tiles above a defined entropy threshold
        try(Percent <- SPIAT::calculate_percentage_of_grids(grid_object, threshold = Entropy_threshold, above = TRUE))
        try(Spatial_autocorrelation <- SPIAT::calculate_spatial_autocorrelation(grid_object, metric = Autocorrelation_metric))
        
        c(Subject_Names = names(DATA_SPIAT)[Index], 
          PER_above_threshold = Percent,
          N_tiles = N_tiles*N_tiles,
          Spatial_autocorrelation = Spatial_autocorrelation)
      },
      .progress = TRUE)
    future::plan("future::sequential")
    gc()
    
    return(map_df(RESULTS, bind_rows))
    
  },
  options = list(optimize = 3))

############STEP 5 - SIMPLE CELL TO CELL ANALYSIS - REQUIRED FUNCTIONS########### 
message("Importing functions: STEP 5 - SIMPLE CELL TO CELL ANALYSIS")

Advanced_Distance_function_A_B <- cmpfun(
  function(DATA, cell_A, cell_B){
    Tibble_A <- DATA %>% dplyr::filter(Phenotype == cell_A)
    Tibble_B <- DATA %>% dplyr::filter(Phenotype == cell_B)
    Tibble_interim <- as_tibble(expand.grid(Tibble_A$Cell_no, Tibble_B$Cell_no))
    names(Tibble_interim) <- c("Cell_no", "NOTHING")
    Tibble_interim <- left_join(Tibble_interim, Tibble_A, by = "Cell_no") %>% dplyr::select(1:4)
    names(Tibble_interim) <- c("Cell_A", "Cell_no", "X_Cell_A", "Y_Cell_A")
    Tibble_interim <- left_join(Tibble_interim, Tibble_B, by = "Cell_no") %>% dplyr::select(1:6)
    names(Tibble_interim) <- c("Cell_A_no", "Cell_B_no", "X_Cell_A", "Y_Cell_A", "X_Cell_B", "Y_Cell_B")
    Tibble_interim %>% mutate(DIST = sqrt((X_Cell_A - X_Cell_B)^2 + (Y_Cell_A - Y_Cell_B)^2)) %>%
      dplyr::select(Cell_A_no, Cell_B_no, DIST) %>% pivot_wider(id_cols = Cell_A_no, names_from = Cell_B_no, values_from = DIST) %>%
      rename(Cell_Of_Origin_no = Cell_A_no)
  }, options = list(optimize = 3))

Distance_matrix_generator <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           Cell_Of_Origin = NULL,
           Target_Cell = NULL,
           Allow_Cero_Distance = NULL,
           Perform_edge_correction = NULL,
           Hull_ratio = NULL,
           Distance_to_edge = NULL
  ) {
    #Check arguments
    if(!exists(DATA, envir = .GlobalEnv)){
      stop("A DATA_Phenotypes object must be created before running the analysis. Check name supplied to the DATA argument")
    }
    #Obtain the data
    DATA_Phenotypes <- get(DATA, envir = .GlobalEnv)
    Advanced_Distance_function_A_B <- get("Advanced_Distance_function_A_B", envir = .GlobalEnv)
    
    if(!identical(names(DATA_Phenotypes)[1:4],  c("Cell_no", "X", "Y", "Subject_Names"))) { #Check if Data is correctly formatted
      stop("DATA provided should have an adecuate format")
    }
    if(!("Phenotype" %in% names(DATA_Phenotypes))) {
      stop("DATA should contain a column named Phenotype specifying the cell types")
    }
    if(!all(c(Cell_Of_Origin %in% unique(DATA_Phenotypes$Phenotype), 
              Target_Cell %in% unique(DATA_Phenotypes$Phenotype)
    )
    )) { #Check if provided cell types are in the phenotype variable
      stop(paste0("Cell of origin provided and Target cells should be one of: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
    }
    if(!is.logical(Allow_Cero_Distance)) stop("Allow_Cero_Distance must be a logical value")
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!is.logical(Perform_edge_correction)) stop("Perform_edge_correction must be a logical value")
    if(Perform_edge_correction){
      if(!all(is.numeric(Hull_ratio), Hull_ratio >= 0, Hull_ratio <= 1)) stop("Hull_ratio must be a numeric value between 0 and 1")
      if(!all(is.numeric(Distance_to_edge), Distance_to_edge > 0)) stop("Distance_to_edge must be a numeric value > 0")
    }
    
    #Perform a random test to allow user to stop the computation if edge correction parameter is not desired
    if(Perform_edge_correction){
      print("Running edge correction example on a random sample")
      Sample <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == sample(unique(DATA_Phenotypes$Subject_Names), size = 1))
      Cells_sf <- sf::st_as_sf(Sample , coords = c("X", "Y"))
      Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
      Cells_in_Border_vector <- unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = F, dist = Distance_to_edge))
      
      plot(Sample %>% 
             dplyr::mutate(Removed = Cells_in_Border_vector) %>%
             ggplot(aes(x = X, y = Y, color = Cells_in_Border_vector)) +
             geom_point() +
             scale_color_manual("", labels = c("Included", "Removed"), values = c("black", "grey")) +
             theme_minimal() + 
             scale_x_continuous("") + 
             scale_y_continuous("") +
             theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   legend.position = "bottom",
                   legend.text = element_text(size = 12)))
      
      #Ask the user if the algorihtm should proceed
      answer <- menu(c("Proceed", "Abort"), title = "Should the analysis proceed")
      #If user decides to stop then abort function and return stop message
      if(answer == 2) stop("The function has been stopped. Please tune edge correction parameters for a better result")
      
      #Remove cells
      Keep_vector_list <- map(unique(DATA_Phenotypes$Subject_Names), function(x){
        #Prepare our data
        Image_tibble <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == x)
        Cells_sf <- sf::st_as_sf(Image_tibble , coords = c("X", "Y"))
        Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
        Cells_in_Border_vector <- unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = F, dist = Distance_to_edge))
        #Calculate cells in border
        COO_in_Border_vector <- Image_tibble$Phenotype == Cell_Of_Origin  & Cells_in_Border_vector
        
        #Print message to warn COO removed in analysis
        message(paste0("Sample ", as.character(x), ": ", sum(COO_in_Border_vector), " / ", sum(Image_tibble$Phenotype == Cell_Of_Origin), " ", Cell_Of_Origin, " cell/s will be removed due to edge proximity.")) 
        
        #Return a vector with the cells to keep (either no COO or COO not in edge)
        return(!COO_in_Border_vector)
      })
      names(Keep_vector_list) <- unique(DATA_Phenotypes$Subject_Names)
    }
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Now we calculate our distance matrix
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <-
      furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(x) {
        #Prepare our data
        Image_tibble <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == x)
        #First we will filter cells of origin in the border as desired by the user
        if(Perform_edge_correction){
          Cells_to_keep <- Keep_vector_list[[x]]
          Image_tibble <- Image_tibble[Cells_to_keep,]
        }
        
        Pre_Cell_counts <- Image_tibble %>% dplyr::count(Phenotype) %>% dplyr::filter(Phenotype %in% c(Cell_Of_Origin, Target_Cell))#Select cells of interest
        #Build a DF where the first row is the COO cell count and the second row is the Target cell count
        Cell_counts <- bind_rows(Pre_Cell_counts %>% dplyr::filter(Phenotype == Cell_Of_Origin),
                                 Pre_Cell_counts %>% dplyr::filter(Phenotype == Target_Cell))
        
        #Acount for samples without any COO or Target Cell
        if(nrow(Cell_counts) < 2) {
          return(
            list(Cell_counts = Cell_counts,
                 Distance_matrix = tibble(Cell_Of_Origin_no = NA,
                                          Target_cell_1 = NA))
          )
        }
        
        #Acount for samples with a unique COO that is also the target cell (infrequent but may occur)
        else if((Cell_counts[[1,1]] == Cell_counts[[2,1]]) & Cell_counts[[1,2]] == 1){
          return(
            list(Cell_counts = Cell_counts,
                 Distance_matrix = tibble(Cell_Of_Origin_no = NA,
                                          Target_cell_1 = NA))
          )
        }
        
        #If not proceed
        else {
          Distance_matrix <- Advanced_Distance_function_A_B(DATA = Image_tibble, cell_A = Cell_Of_Origin, cell_B = Target_Cell)#Define COO and target cells
          
          if(!Allow_Cero_Distance){
            #If distance is 0, substitute it for NA
            Distance_matrix[Distance_matrix == 0] <- NA
          }
          return(
            list(Cell_counts = Cell_counts,
                 Distance_matrix = Distance_matrix)
          )
          
        }
        
      },
      .progress = TRUE)
    future::plan("future::sequential")
    gc()
    
    names(RESULTS) <- unique(DATA_Phenotypes$Subject_Names)
    
    #Calculate which samples have NA values and should be removed
    Samples_to_remove <- map_lgl(RESULTS, function(x) is.na(x[[2]][[1,1]]))
    
    #If present print a warning and remove the samples
    if(sum(Samples_to_remove) > 0) {
      warning(paste0("Samples without COO or target cells will be removed from the analysis. ",
                     "The following samples will be removed: ", str_c(names(RESULTS)[Samples_to_remove], collapse = ", ")
      )
      )
      return(RESULTS[!Samples_to_remove])
    }
    
    #Else return the resulsts straight forward
    else{
      return(RESULTS)
    }
    
  }, 
  options = list(optimize = 3))

Cumulative_Interaction_generator <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           Start_from = NULL,
           Stop_at = NULL,
           Sampling_frequency = NULL) {
    #Check arguments
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!all(is.character(DATA), exists(DATA, envir = .GlobalEnv))) stop("DATA argument must be the name of an existing object")
    if(!all(is.numeric(Start_from), is.numeric(Stop_at), is.numeric(Sampling_frequency), Start_from < Stop_at, Sampling_frequency < (Stop_at - Start_from))) {
      stop("Start_from, Stop_at and Sampling_frequency must be numeric values. Start_from must be smaller than Stop_at. Sampling_frequency must be smaller than the range Start_from - Stop_at")
    }
    if(!all(Start_from >= 0, Stop_at > 0, Sampling_frequency > 0)) stop("Start_from, Stop_at and Sampling_frequency must have positive values")
    
    
    #Get our data from the global environment
    DATA <- get(DATA, envir = .GlobalEnv)
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Prepare our clustering
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <- suppressMessages(
      furrr::future_map(DATA, function(x) {
        #Get our tibble and format it adequately and remove NA values if necessary
        Longer_distance_matrix <-  x[[2]] %>% pivot_longer(2:ncol(x[[2]])) %>% na.omit()
        
        #Calculate the cumulative distance tibble with the desired sampling strategy
        Cumulative_distance_tibble <-
          map_dfc(seq(from = Start_from, to = Stop_at, by = Sampling_frequency), function(filter) {
            Interim <- Longer_distance_matrix %>% mutate(counts = value <= filter) %>% group_by(Cell_Of_Origin_no, counts) %>% dplyr::count() %>% dplyr::filter(counts == T) %>%
              ungroup() %>% dplyr::select(-counts)
            Final <- left_join(tibble(Cell_Of_Origin_no = unique(Longer_distance_matrix$Cell_Of_Origin_no)), Interim, by = "Cell_Of_Origin_no") %>% 
              mutate(number = case_when(is.na(n) ~ 0,
                                        TRUE ~ n)) %>% dplyr::select(-n)
            names(Final) <- c("Cell_Of_Origin_no", as.character(filter))
            Final
          }) %>% dplyr::select(-contains("Cell")) %>% mutate(Cell_Of_Origin_no = unique(Longer_distance_matrix$Cell_Of_Origin_no))
        Cumulative_distance_tibble <- Cumulative_distance_tibble[c(ncol(Cumulative_distance_tibble), 1:(ncol(Cumulative_distance_tibble)-1))]
        
        #Arrange the results in a list and return it
        list(Cell_counts = x[[1]],
             Cumulative_distance = Cumulative_distance_tibble)
      },
      .progress = TRUE)
    )
    
    future::plan("future::sequential")
    gc()
    
    names(RESULTS) <- names(DATA)
    
    return(RESULTS)
    
  },
  options = list(optimize = 3))

Advanced_Distance_function_A_B_single <- cmpfun(
  function(DATA, cell_A, cell_B){
    Tibble_A <- DATA %>% dplyr::filter(Phenotype == cell_A) %>% sample_n(size = 1)
    Tibble_B <- DATA %>% dplyr::filter(Phenotype == cell_B)
    Tibble_interim <- as_tibble(expand.grid(Tibble_A$Cell_no, Tibble_B$Cell_no))
    names(Tibble_interim) <- c("Cell_no", "NOTHING")
    Tibble_interim <- left_join(Tibble_interim, Tibble_A, by = "Cell_no") %>% dplyr::select(1:4)
    names(Tibble_interim) <- c("Cell_A", "Cell_no", "X_Cell_A", "Y_Cell_A")
    Tibble_interim <- left_join(Tibble_interim, Tibble_B, by = "Cell_no") %>% dplyr::select(1:6)
    names(Tibble_interim) <- c("Cell_A_no", "Cell_B_no", "X_Cell_A", "Y_Cell_A", "X_Cell_B", "Y_Cell_B")
    Tibble_interim %>% mutate(DIST = sqrt((X_Cell_A - X_Cell_B)^2 + (Y_Cell_A - Y_Cell_B)^2)) %>%
      dplyr::select(Cell_A_no, Cell_B_no, DIST) %>% pivot_wider(id_cols = Cell_A_no, names_from = Cell_B_no, values_from = DIST)
  }, 
  options = list(optimize = 3))

Random_Distance_matrix_generator <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           Cell_Of_Origin = NULL,
           Target_Cell = NULL,
           Random_cells_per_sample = NULL,
           Allow_Cero_Distance = NULL,
           Perform_edge_correction = NULL,
           Hull_ratio = NULL,
           Distance_to_edge = NULL
  ) {
    #Check arguments
    if(!exists("Advanced_Distance_function_A_B_single", envir = .GlobalEnv)) stop("Please execute STEP 5 required functions")
    Advanced_Distance_function_A_B_single <- get("Advanced_Distance_function_A_B_single",  envir = .GlobalEnv)
    if(!exists(DATA, envir = .GlobalEnv)){
      stop("A DATA_Distance object must be created before running the tiling analysis. Check name supplied to the DATA argument")
    }
    DATA_Phenotypes <- get(DATA, envir = .GlobalEnv)
    if(!identical(names(DATA_Phenotypes)[1:4],  c("Cell_no", "X", "Y", "Subject_Names"))) { #Check if Data is correctly formatted
      stop("DATA provided should have an adecuate format")
    }
    if(!("Phenotype" %in% names(DATA_Phenotypes))) {
      stop("DATA should contain a column named Phenotype specifying the cell types")
    }
    #Check if provided cell types are in the phenotype variable
    if(!all(c(Cell_Of_Origin %in% unique(DATA_Phenotypes$Phenotype), 
              Target_Cell %in% unique(DATA_Phenotypes$Phenotype))
    )) { 
      stop(paste0("Cell of origin provided and Target cells should be one of: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
    }
    if(!all(Random_cells_per_sample%%1 == 0, Random_cells_per_sample > 0)) stop("Random_cells_per_sample must be an integer value > 0")
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!is.logical(Allow_Cero_Distance)) stop("Allow_Cero_Distance should be a logical value")
    if(!is.logical(Perform_edge_correction)) stop("Perform_edge_correction must be a logical value")
    if(Perform_edge_correction){
      if(!all(is.numeric(Hull_ratio), Hull_ratio >= 0, Hull_ratio <= 1)) stop("Hull_ratio must be a numeric value between 0 and 1")
      if(!all(is.numeric(Distance_to_edge), Distance_to_edge > 0)) stop("Distance_to_edge must be a numeric value > 0")
    }
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Perform Randomization of labels and calculate distances
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <-
      furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(Images){
        #Prepare our data
        Image_tibble <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Images)
        Pre_Cell_counts <- Image_tibble %>% dplyr::count(Phenotype) %>% dplyr::filter(Phenotype %in% c(Cell_Of_Origin, Target_Cell))#Select cells of interest
        #Build a DF where the first row is the COO cell count and the second row is the Target cell count
        Cell_counts <- bind_rows(Pre_Cell_counts %>% dplyr::filter(Phenotype == Cell_Of_Origin),
                                 Pre_Cell_counts %>% dplyr::filter(Phenotype == Target_Cell))
        
        #Acount for samples without any COO or Target Cell
        if(nrow(Cell_counts) < 2) {
          return(
            list(Cell_counts = Cell_counts,
                 Distance_matrix = tibble(Cell_Of_Origin_no = NA,
                                          Target_cell_1 = NA))
          )
        }
        
        #Acount for samples with a unique COO that is also the target cell (infrequent but may occur)
        if((Cell_counts[[1,1]] == Cell_counts[[2,1]]) & Cell_counts[[1,2]] == 1){
          return(
            list(Cell_counts = Cell_counts,
                 Distance_matrix = tibble(Cell_Of_Origin_no = NA,
                                          Target_cell_1 = NA))
          )
        }
        
        #If border cell correction required generate the border polygon
        if(Perform_edge_correction){
          Cells_sf <- sf::st_as_sf(Image_tibble , coords = c("X", "Y"))
          Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
          Cells_in_Border_vector <- unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = F, dist = Distance_to_edge))
        }
        
        #If all ok proceed with analysis
        #Number of random cell generated by each sample
        Distance_matrix <- map_dfr(seq_along(1:Random_cells_per_sample), function(y){ 
          I_dont_care <- y
          #First generate new random cell type labels
          new_Phenotype <- sample(Image_tibble$Phenotype, size = nrow(Image_tibble), replace = F)
          Random_tibble <- Image_tibble %>% mutate(Phenotype = new_Phenotype)
          
          #If needed remove COO that are close to border
          if(Perform_edge_correction){
            Random_tibble <- Random_tibble[!(Random_tibble$Phenotype == Cell_Of_Origin & Cells_in_Border_vector), ]
            
            #If no random cells are remaining then resample the labels once again
            while(nrow(Random_tibble %>% dplyr::filter(Phenotype == Cell_Of_Origin)) == 0){
              new_Phenotype <- sample(Image_tibble$Phenotype, size = nrow(Image_tibble), replace = F)
              Random_tibble <- Image_tibble %>% mutate(Phenotype = new_Phenotype)
              Random_tibble <- Random_tibble[!(Random_tibble$Phenotype == Cell_Of_Origin & Cells_in_Border_vector), ]
            }
          }
          
          #Then apply distance function
          Distance_matrix <- Advanced_Distance_function_A_B_single(DATA = Random_tibble, cell_A = Cell_Of_Origin, cell_B = Target_Cell)#Define COO and target cell
          names(Distance_matrix)[-1] <- str_c("Random_Target_cell_", as.character(1:(ncol(Distance_matrix)-1)))
          Distance_matrix
        })
        Distance_matrix <- Distance_matrix %>% mutate(Cell_A_no = str_c("Random_COO_cell_", as.character(1:nrow(Distance_matrix))))
        names(Distance_matrix)[1] <- "Cell_Of_Origin_no"
        
        if(!Allow_Cero_Distance){
          #If distance is 0, substitute it for NA
          Distance_matrix[Distance_matrix == 0] <- NA
        }
        #Return the final list
        return(
          list(Cell_counts = Cell_counts,
               Distance_matrix = Distance_matrix)
        )
      },
      .progress = TRUE)
    future::plan("future::sequential")
    gc()
    
    names(RESULTS) <- unique(DATA_Phenotypes$Subject_Names)
    
    Samples_to_remove <- map_lgl(RESULTS, function(x) is.na(x[[2]][[1,1]]))
    
    #If samples to remove are present print a warning and proceed to remove required samples
    if(sum(Samples_to_remove) > 0) {
      warning(paste0("Samples without COO or target cells will be removed from the analysis. ",
                     "The following samples will be removed: ", str_c(names(RESULTS)[Samples_to_remove], collapse = ", ")
      )
      )
      return(RESULTS[!Samples_to_remove])
    }
    
    else{
      return(RESULTS)
    }
  },
  options = list(optimize = 3))

Distance_analyzer <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           DATA_RANDOM = NULL,
           Metric = NULL,
           Include_Random = NULL,
           By_Sample_Random = NULL) {
    #Check arguments
    if(!is.logical(Include_Random)) stop("Include_Random should be a logical value")
    if(!Metric %in% c("Average_Distance", "Min_Distance", "Max_Distance")) stop("Metric should be one of the following: Average_Distance, Min_Distance, Max_Distance")
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    #Specify if random samples should be include in the computation
    if(Include_Random) {
      #Check arguments
      if(!is.logical(By_Sample_Random)) stop("By_Sample_Random should be a logical value")
      if(!length(intersect(names(DATA), names(DATA_RANDOM))) == length(unique(c(names(DATA), names(DATA_RANDOM))))){
        outersect <- function(x, y) {
          sort(c(x[!x%in%y],
                 y[!y%in%x]))
        }
        Removed_cases <- outersect(names(DATA), names(DATA_RANDOM))
        
        message(paste0("Only samples present in DATA and DATA_RANDOM will be used. The following samples will be removed: ",
                       str_c(Removed_cases, collapse = ", ")))
        DATA <- DATA[intersect(names(DATA), names(DATA_RANDOM))]
        DATA_RANDOM <- DATA_RANDOM[intersect(names(DATA), names(DATA_RANDOM))]
      }
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      #Generate the future
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      
      if(Metric == "Average_Distance") {
        print("Calculating Result")
        #Generate results
        RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          mean(as.matrix(Interim[-1]), na.rm = T)
        })
        
        RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          sd(as.matrix(Interim[-1]), na.rm = T) / sqrt(sum(!is.na(Interim[-1])))
        })
        
        print("Calculating Random Background")
        if(By_Sample_Random){
          #Generate Random results
          RESULTS_RANDOM <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
            Interim <- DATA[[2]]
            mean(as.matrix(Interim[-1]), na.rm = T)
          })
          
          #Calculate Standard Error of Random Mean
          RESULTS_RANDOM_SE <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
            Interim <- DATA[[2]]
            sd(as.matrix(Interim[-1]), na.rm = T) / sqrt(sum(!is.na(Interim[-1])))
          })
        }
        
        if(!By_Sample_Random){
          #Generate Random results for all Random samples at the same time
          RESULTS_RANDOM <- mean(unlist(
            map(DATA_RANDOM, function(DATA){
              DATA[[2]][,-1]
            })),
            na.rm = T)
          #Calculate Standard Error of Random Mean for all Random samples at the same time
          RESULTS_RANDOM_SE <- sd(unlist(
            map(DATA_RANDOM, function(DATA){
              DATA[[2]][,-1]
            })),
            na.rm = T) / sqrt(sum(!is.na(unlist(
              map(DATA_RANDOM, function(DATA){
                DATA[[2]][,-1]
              })))))
        }
        
        
        #Generate results tibble
        RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
        names(RESULTS_tibble)[2] <- Metric
        
        RESULTS_tibble$SE <- RESULTS_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Average_Distance - 1.96*SE,
                                                    Result_95CI = Average_Distance + 1.96*SE)
        
        RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
        RESULTS_tibble$N_Target <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
        
        RESULTS_tibble$Random <- RESULTS_RANDOM
        RESULTS_tibble$Random_SE <- RESULTS_RANDOM_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Random_05CI = Random - 1.96*Random_SE,
                                                    Random_95CI = Random + 1.96*Random_SE) %>%
          mutate(Outside_CI = case_when(Average_Distance > Random_95CI | Average_Distance < Random_05CI ~ "Significant",
                                        TRUE ~ "Not Significant"),
                 Sign = case_when(Average_Distance > Random ~ "Above expected",
                                  TRUE ~ "Below expected"))
        RESULTS_tibble$N_Random_COO <- map_dbl(DATA_RANDOM, function(DATA_RANDOM) nrow(DATA_RANDOM[[2]]))
        
        #Establish a lower limit of 0 for CI
        RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
        RESULTS_tibble$Random_05CI[RESULTS_tibble$Random_05CI < 0] <- 0
        
        print("Generating plot")
        #Plot the results
        plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Average_Distance))) +
               geom_col(aes(y = Average_Distance), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
               geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
               geom_errorbar(aes(ymin = Random_05CI, ymax = Random_95CI), color = "red", linewidth = 0.9, width = 0.3)+
               cowplot::theme_cowplot() +
               scale_x_discrete("") +
               scale_y_continuous("Average Distance") +
               theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
        )
        
        future::plan("future::sequential")
        gc()
        #Return the final results
        return(RESULTS_tibble)
      }
      
      else if(Metric == "Min_Distance") {
        print("Calculating Result")
        #Generate results
        RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          mean(apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T)))
        })
        
        RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          sd(unlist(apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T)))) / sqrt(nrow(Interim))
        })
        
        print("Calculating Random Background")
        if(By_Sample_Random){
          #Generate Random Results
          RESULTS_RANDOM <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
            Interim <- DATA[[2]]
            mean(apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T)))
          })
          
          RESULTS_RANDOM_SE <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
            Interim <- DATA[[2]]
            sd(unlist(apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T)))) / sqrt(nrow(Interim))
          })
        }
        
        if(!By_Sample_Random){
          #Generate the Random Results overall
          RESULTS_RANDOM <- mean(unlist(
            furrr::future_map(DATA_RANDOM, function(DATA){
              Interim <- DATA[[2]]
              apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T))
            })
          ))
          
          #Generate the Random standard deviation overall
          RESULTS_RANDOM_SE <- sd(unlist(
            furrr::future_map(DATA_RANDOM, function(DATA){
              Interim <- DATA[[2]]
              apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T))
            })
          )) / sqrt(sum(!is.na(unlist(
            furrr::future_map(DATA_RANDOM, function(DATA){
              Interim <- DATA[[2]]
              apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T))
            })
          ))))
        }
        
        #Generate results tibble
        RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
        names(RESULTS_tibble)[2] <- Metric
        
        RESULTS_tibble$SE <- RESULTS_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Min_Distance - 1.96*SE,
                                                    Result_95CI = Min_Distance + 1.96*SE)
        
        RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
        RESULTS_tibble$N_Target <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
        
        RESULTS_tibble$Random <- RESULTS_RANDOM
        RESULTS_tibble$Random_SE <- RESULTS_RANDOM_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Random_05CI = Random - 1.96*Random_SE,
                                                    Random_95CI = Random + 1.96*Random_SE) %>%
          mutate(Outside_CI = case_when(Min_Distance > Random_95CI | Min_Distance < Random_05CI ~ "Significant",
                                        TRUE ~ "Not Significant"),
                 Sign = case_when(Min_Distance > Random ~ "Above expected",
                                  TRUE ~ "Below expected"))
        RESULTS_tibble$N_Random_COO <- map_dbl(DATA_RANDOM, function(DATA_RANDOM) nrow(DATA_RANDOM[[2]]))
        
        #Establish a lower limit of 0 for CI
        RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
        RESULTS_tibble$Random_05CI[RESULTS_tibble$Random_05CI < 0] <- 0
        
        print("Generating plot")
        #Plot the results
        plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Min_Distance))) +
               geom_col(aes(y = Min_Distance), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
               geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
               geom_errorbar(aes(ymin = Random_05CI, ymax = Random_95CI), color = "red", linewidth = 0.9, width = 0.3)+
               cowplot::theme_cowplot() +
               scale_x_discrete("") +
               scale_y_continuous("Average Min Distance") +
               theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
        )
        
        future::plan("future::sequential")
        gc()
        return(RESULTS_tibble)
      }
      
      else if(Metric == "Max_Distance") {
        print("Calculating Result")
        #Generate results
        RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          mean(apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T)))
        })
        
        RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          sd(unlist(apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T)))) / sqrt(nrow(Interim))
        })
        
        print("Calculating Random Background")
        if(By_Sample_Random){
          #Generate Random Results
          RESULTS_RANDOM <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
            Interim <- DATA[[2]]
            mean(apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T)))
          })
          
          RESULTS_RANDOM_SE <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
            Interim <- DATA[[2]]
            sd(unlist(apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T)))) / sqrt(nrow(Interim))
          })
        }
        
        if(!By_Sample_Random){
          #Generate the Random Results overall
          RESULTS_RANDOM <- mean(unlist(
            map(DATA_RANDOM, function(DATA){
              Interim <- DATA[[2]]
              apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T))
            })
          ))
          
          #Generate the Random standard deviation overall
          RESULTS_RANDOM_SE <- sd(unlist(
            map(DATA_RANDOM, function(DATA){
              Interim <- DATA[[2]]
              apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T))
            })
          )) / sqrt(sum(!is.na(unlist(
            map(DATA_RANDOM, function(DATA){
              Interim <- DATA[[2]]
              apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T))
            })
          ))))
        }
        
        
        #Generate results tibble
        RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
        names(RESULTS_tibble)[2] <- Metric
        
        RESULTS_tibble$SE <- RESULTS_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Max_Distance - 1.96*SE,
                                                    Result_95CI = Max_Distance + 1.96*SE)
        
        RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
        RESULTS_tibble$N_Target <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
        
        RESULTS_tibble$Random <- RESULTS_RANDOM
        RESULTS_tibble$Random_SE <- RESULTS_RANDOM_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Random_05CI = Random - 1.96*Random_SE,
                                                    Random_95CI = Random + 1.96*Random_SE) %>%
          mutate(Outside_CI = case_when(Max_Distance > Random_95CI | Max_Distance < Random_05CI ~ "Significant",
                                        TRUE ~ "Not Significant"),
                 Sign = case_when(Max_Distance > Random ~ "Above expected",
                                  TRUE ~ "Below expected"))
        RESULTS_tibble$N_Random_COO <- map_dbl(DATA_RANDOM, function(DATA_RANDOM) nrow(DATA_RANDOM[[2]]))
        
        #Establish a lower limit of 0 for CI
        RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
        RESULTS_tibble$Random_05CI[RESULTS_tibble$Random_05CI < 0] <- 0
        
        print("Generating plot")
        #Plot the results
        plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Max_Distance))) +
               geom_col(aes(y = Max_Distance), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
               geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
               geom_errorbar(aes(ymin = Random_05CI, ymax = Random_95CI), color = "red", linewidth = 0.9, width = 0.3)+
               cowplot::theme_cowplot() +
               scale_x_discrete("") +
               scale_y_continuous("Average Max Distance") +
               theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
        )
        
        future::plan("future::sequential")
        gc()
        return(RESULTS_tibble)
      }
    }
    
    #Specify what to do if random samples are not included in the analysis
    else if(!Include_Random){
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      #Generate future cluster
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      
      if(Metric == "Average_Distance") {
        print("Calculating Result")
        
        #Generate results
        RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          mean(as.matrix(Interim[-1]), na.rm = T)
        })
        
        RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          sd(as.matrix(Interim[-1]), na.rm = T) / sqrt(sum(!is.na(Interim[-1])))
        })
        
        #Generate results tibble
        RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
        names(RESULTS_tibble)[2] <- Metric
        
        RESULTS_tibble$SE <- RESULTS_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Average_Distance - 1.96*SE,
                                                    Result_95CI = Average_Distance + 1.96*SE)
        
        RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
        RESULTS_tibble$N_Target <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
        
        #Establish a lower limit of 0 for CI
        RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
        
        #plot the results
        plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Average_Distance))) +
               geom_col(aes(y = Average_Distance), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
               geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
               cowplot::theme_cowplot() +
               scale_x_discrete("") +
               scale_y_continuous("Average Distance") +
               theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
        )
        
        future::plan("future::sequential")
        gc()
        return(RESULTS_tibble)
      }
      
      else if(Metric == "Min_Distance") {
        print("Calculating Result")
        #Generate results
        RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          mean(apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T)))
        })
        
        RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          sd(unlist(apply(Interim[-1], MARGIN = 1, function(x) min(x, na.rm = T)))) / sqrt(nrow(Interim))
        })
        
        #Generate results tibble
        RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
        names(RESULTS_tibble)[2] <- Metric
        
        RESULTS_tibble$SE <- RESULTS_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Min_Distance - 1.96*SE,
                                                    Result_95CI = Min_Distance + 1.96*SE)
        
        RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
        RESULTS_tibble$N_Target <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
        
        #Establish a lower limit of 0 for CI
        RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
        
        #Plot the results
        plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Min_Distance))) +
               geom_col(aes(y = Min_Distance), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
               geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
               cowplot::theme_cowplot() +
               scale_x_discrete("") +
               scale_y_continuous("Average Min Distance") +
               theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
        )
        
        future::plan("future::sequential")
        gc()
        return(RESULTS_tibble)
      }
      
      else if(Metric == "Max_Distance") {
        print("Calculating Result")
        #Generate results
        RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          mean(apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T)))
        })
        
        RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
          Interim <- DATA[[2]]
          sd(unlist(apply(Interim[-1], MARGIN = 1, function(x) max(x, na.rm = T)))) / sqrt(nrow(Interim))
        })
        
        #Generate results tibble
        RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
        names(RESULTS_tibble)[2] <- Metric
        
        RESULTS_tibble$SE <- RESULTS_SE
        RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Max_Distance - 1.96*SE,
                                                    Result_95CI = Max_Distance + 1.96*SE)
        
        RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
        RESULTS_tibble$N_Target <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
        
        #Establish a lower limit of 0 for CI
        RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
        
        #plot the results
        plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Max_Distance))) +
               geom_col(aes(y = Max_Distance), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
               geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
               cowplot::theme_cowplot() +
               scale_x_discrete("") +
               scale_y_continuous("Average Max Distance") +
               theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
        )
        
        future::plan("future::sequential")
        gc()
        return(RESULTS_tibble)
      }
    }
    
  },
  options = list(optimize = 3))

Cells_in_Radius_analyzer <- cmpfun(
  function(DATA = NULL,
           DATA_RANDOM = NULL,
           Radius = NULL,
           Include_Random = NULL,
           By_Sample_Random = NULL) {
    #Check arguments
    if(!is.logical(Include_Random)) stop("Include_Random should be a logical value")
    if(!(as.character(Radius) %in% names(DATA[[1]][[2]]))){
      stop(paste0("Radius should be one of: ", str_c(names(DATA[[1]][[2]])[-1], collapse = ", ")))
    }
    
    #If everything is OK proceed with analysis
    if(Include_Random) {
      if(!is.logical(By_Sample_Random)) stop("By_Sample_Random should be a logical value")
      if(!length(intersect(names(DATA), names(DATA_RANDOM))) == length(unique(c(names(DATA), names(DATA_RANDOM))))){
        outersect <- function(x, y) {
          sort(c(x[!x%in%y],
                 y[!y%in%x]))
        }
        Removed_cases <- outersect(names(DATA), names(DATA_RANDOM))
        
        message(paste0("Only samples present in DATA and DATA_RANDOM will be used. The following samples will be removed: ",
                       str_c(Removed_cases, collapse = ", ")))
        DATA <- DATA[intersect(names(DATA), names(DATA_RANDOM))]
        DATA_RANDOM <- DATA_RANDOM[intersect(names(DATA), names(DATA_RANDOM))]
      }
      
      print("Calulating Results")
      #Generate Results
      RESULTS <- map_dbl(DATA, function(x) {
        Interim <- x[[2]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        mean(Interim[[1]])
      })
      
      RESULTS_SE <- map_dbl(DATA, function(x) {
        Interim <- x[[2]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        sd(Interim[[1]]) / sqrt(length(Interim[[1]]))
      })
      
      #Calculate sample-wise random distribution
      print("Calculating Random Background")
      if(By_Sample_Random){
        #Generate Random results
        RESULTS_RANDOM <- map_dbl(DATA_RANDOM, function(x) {
          Interim <- x[[2]]
          Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
          mean(Interim[[1]])
        }) 
        
        RESULTS_RANDOM_SE <- map_dbl(DATA_RANDOM, function(x) {
          Interim <- x[[2]]
          Interim <- Interim %>% dplyr::select(as.character(Radius))
          sd(Interim[[1]]) / sqrt(length(Interim[[1]]))
        })
      }
      
      #Calculate experiment-wise random distribution
      if(!By_Sample_Random){
        RESULTS_RANDOM <- mean(unlist(
          map(DATA_RANDOM, function(x){
            Interim <- x[[2]]
            Interim <- Interim %>% dplyr::select(as.character(Radius))
          })
        ))
        
        RESULTS_RANDOM_SE <- sd(unlist(
          map(DATA_RANDOM, function(x){
            Interim <- x[[2]]
            Interim <- Interim %>% dplyr::select(as.character(Radius))
          })
        )) / sqrt(length(unlist(
          map(DATA_RANDOM, function(x){
            Interim <- x[[2]]
            Interim <- Interim %>% dplyr::select(as.character(Radius))
          })
        )))
      }
      
      #Generate the number of cells
      Cell_A <- map_dbl(DATA, function(x) x[[1]][[1,2]])
      Cell_B <- map_dbl(DATA, function(x) x[[1]][[2,2]])
      Name_A <- unique(map_chr(DATA, function(x) x[[1]][[1,1]]))
      Name_B <- unique(map_chr(DATA, function(x) x[[1]][[2,1]]))
      
      #Generate results tibble
      RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), Average_cells = RESULTS)
      
      RESULTS_tibble$SE <- RESULTS_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Average_cells - 1.96*SE,
                                                  Result_95CI = Average_cells + 1.96*SE)
      RESULTS_tibble$Radius <- Radius
      
      RESULTS_tibble$Cell_A <- Cell_A
      names(RESULTS_tibble)[[7]] <- Name_A
      RESULTS_tibble$Cell_B <- Cell_B
      names(RESULTS_tibble)[[8]] <- Name_B
      
      
      RESULTS_tibble$Random <- RESULTS_RANDOM
      RESULTS_tibble$Random_SE <- RESULTS_RANDOM_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Random_05CI = Random - 1.96*Random_SE,
                                                  Random_95CI = Random + 1.96*Random_SE) %>%
        mutate(Outside_CI = case_when(Average_cells > Random_95CI | Average_cells < Random_05CI ~ "Significant",
                                      TRUE ~ "Not Significant"),
               Sign = case_when(Average_cells > Random ~ "Above expected",
                                TRUE ~ "Below expected"))
      RESULTS_tibble$N_Random_COO <- map_dbl(DATA_RANDOM, function(DATA_RANDOM) nrow(DATA_RANDOM[[2]]))
      #Establish a lower limit of 0 for CI
      RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
      RESULTS_tibble$Random_05CI[RESULTS_tibble$Random_05CI < 0] <- 0
      
      print("Generating plot")
      plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Average_cells))) +
             geom_col(aes(y = Average_cells), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
             geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
             geom_errorbar(aes(ymin = Random_05CI, ymax = Random_95CI), color = "red", linewidth = 0.9, width = 0.3)+
             cowplot::theme_cowplot() +
             scale_x_discrete("") +
             scale_y_continuous(str_c("Average cells within a radius of ", as.character(Radius))) +
             theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
      )
      
      return(RESULTS_tibble)
    }
    
    else if(!Include_Random){
      print("Calulating Results")
      #Generate Results
      RESULTS <- map_dbl(DATA, function(x) {
        Interim <- x[[2]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        mean(Interim[[1]])
      })
      
      RESULTS_SE <- map_dbl(DATA, function(x) {
        Interim <- x[[2]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        sd(Interim[[1]]) / sqrt(length(Interim[[1]]))
      })
      
      #Generate the number of cells
      Cell_A <- map_dbl(DATA, function(x) x[[1]][[1,2]])
      Cell_B <- map_dbl(DATA, function(x) x[[1]][[2,2]])
      Name_A <- unique(map_chr(DATA, function(x) x[[1]][[1,1]]))
      Name_B <- unique(map_chr(DATA, function(x) x[[1]][[2,1]]))
      
      #Generate results tibble
      RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), Average_cells = RESULTS)
      
      RESULTS_tibble$SE <- RESULTS_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Average_cells - 1.96*SE,
                                                  Result_95CI = Average_cells + 1.96*SE)
      RESULTS_tibble$Radius <- Radius
      
      RESULTS_tibble$Cell_A <- Cell_A
      names(RESULTS_tibble)[[7]] <- Name_A
      RESULTS_tibble$Cell_B <- Cell_B
      names(RESULTS_tibble)[[8]] <- Name_B
      #Establish a lower limit of 0 for CI
      RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
      
      print("Generating plot")
      plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Average_cells))) +
             geom_col(aes(y = Average_cells), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
             geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
             cowplot::theme_cowplot() +
             scale_x_discrete("") +
             scale_y_continuous(str_c("Average cells within a radius of ", as.character(Radius))) +
             theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
      )
      
      return(RESULTS_tibble)
    }
  },
  options = list(optimize = 3))

Cell_to_Cell_graph_maker <- cmpfun(
  function(Image_name = NULL,
           DATA_Phenotypes = NULL,
           Strategy = NULL,
           DATA_Distances = NULL,
           DATA_Cumulative = NULL,
           Radius = NULL){
    #Argument checker by Chat GPT
    # Check that Image_name is provided and is character
    if (is.null(Image_name) || !is.character(Image_name)) {
      stop("Image_name must be provided and must be of type character.")
    }
    # Check that Strategy is provided and valid
    valid_strategies <- c("Min_Distance", "Average_Distance", "Max_Distance", "Cells_in_Radius")
    if (is.null(Strategy) || !Strategy %in% valid_strategies) {
      stop(paste0("Strategy must be one of the following: ", paste(valid_strategies, collapse = ", ")))
    }
    # Check that DATA_Phenotypes is provided and is a data frame
    if (is.null(DATA_Phenotypes) || !is.data.frame(DATA_Phenotypes)) {
      stop("DATA_Phenotypes must be provided and must be a data frame.")
    }
    # Check that DATA_Distances is provided and is a list (for non-"Cells_in_Radius" strategies)
    if (Strategy != "Cells_in_Radius" && (is.null(DATA_Distances) || !is.list(DATA_Distances))) {
      stop("DATA_Distances must be provided and must be a list when using Min_Distance, Average_Distance, or Max_Distance.")
    }
    # Check that DATA_Cumulative is provided and is a list (only for "Cells_in_Radius")
    if (Strategy == "Cells_in_Radius" && (is.null(DATA_Cumulative) || !is.list(DATA_Cumulative))) {
      stop("DATA_Cumulative must be provided and must be a list when using Cells_in_Radius strategy.")
    }
    # Check that Radius is provided and is numeric (only for "Cells_in_Radius")
    if (Strategy == "Cells_in_Radius" && (is.null(Radius) || !is.numeric(Radius))) {
      stop("Radius must be provided and must be numeric when using Cells_in_Radius strategy.")
    }
    # Ensure that Image_name exists in the DATA_Phenotypes and DATA_Distances/Cumulative
    if (Strategy %in% c("Min_Distance", "Average_Distance", "Max_Distance")) {
      if (!all(Image_name %in% unique(DATA_Phenotypes$Subject_Names), Image_name %in% names(DATA_Distances))) {
        stop(paste0("Image_name not present in data. It should be one of: ", str_c(names(DATA_Distances), collapse = ", ")))
      }
    } else if (Strategy == "Cells_in_Radius") {
      if (!all(Image_name %in% unique(DATA_Phenotypes$Subject_Names), Image_name %in% names(DATA_Cumulative))) {
        stop(paste0("Image_name not present in data. It should be one of: ", str_c(names(DATA_Cumulative), collapse = ", ")))
      }
    }
    
    if(Strategy == "Min_Distance") {
      #Import general data frames
      DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_name)
      DATA_Distances <- DATA_Distances[[Image_name]]
      
      #Define the COO and Target_Cell
      COO <- DATA_Distances[[1]][[1,1]]
      Target_Cell <- DATA_Distances[[1]][[2,1]]
      
      #First prepare 3 tibbles, a tibble with COO cells, a tibble with target cells and a tibble with other cells
      Other_tibble <- DATA_Phenotypes %>% dplyr::filter(!Phenotype %in% c(COO, Target_Cell))
      Target_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == Target_Cell)
      COO_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == COO)
      
      #Calculate the min distance
      For_Join <- bind_cols(DATA_Distances[[2]][1],
                            map_dbl(1:nrow(DATA_Distances[[2]]), function(Row){
                              min(DATA_Distances[[2]][Row,-1])
                            })
      )
      names(For_Join) <- c("Cell_no", "Min_Distance")
      
      #Join the min distance with the COO tibble
      COO_tibble <- left_join(COO_tibble, For_Join, by = "Cell_no") %>% dplyr::filter(!is.na(Min_Distance))
      
      #Build the plot
      plot(
        ggplot() +
          geom_point(aes(x = X, y = Y), color = "lightgrey", data = Other_tibble) +
          ggforce::geom_circle(aes(x0 = X, y0 = Y, r = Min_Distance, fill = Min_Distance), alpha = 0.3, color = NA, data = COO_tibble)+
          geom_point(aes(x = X, y = Y), size = 2, color = "red", data = Target_tibble) +
          geom_point(aes(x = X, y = Y), size = 2, color = "blue", data = COO_tibble) +
          cowplot::theme_cowplot() +
          scale_x_continuous("", labels = NULL)+
          scale_y_continuous("", labels = NULL)+
          scale_fill_viridis_c("Min dist to target")+
          ggtitle(Image_name)+
          theme(panel.grid = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 20),
                legend.position = "bottom",
                plot.title = element_text(size = 25, hjust = 0.5, vjust = -3)) 
      )
    }
    
    else if(Strategy == "Average_Distance") {
      #Import general data frames
      DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_name)
      DATA_Distances <- DATA_Distances[[Image_name]]
      
      #Define the COO and Target_Cell
      COO <- DATA_Distances[[1]][[1,1]]
      Target_Cell <- DATA_Distances[[1]][[2,1]]
      
      #First prepare 3 tibbles, a tibble with COO cells, a tibble with target cells and a tibble with other cells
      Other_tibble <- DATA_Phenotypes %>% dplyr::filter(!Phenotype %in% c(COO, Target_Cell))
      Target_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == Target_Cell)
      COO_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == COO)
      
      #Calculate the average distance
      For_Join <- bind_cols(DATA_Distances[[2]][1],
                            map_dbl(1:nrow(DATA_Distances[[2]]), function(Row){
                              mean(unlist(DATA_Distances[[2]][Row,-1]))
                            })
      )
      names(For_Join) <- c("Cell_no", "Average_Distance")
      
      #Join the average distance with the COO tibble
      COO_tibble <- left_join(COO_tibble, For_Join, by = "Cell_no") %>% dplyr::filter(!is.na(Average_Distance))
      
      #Build the plot
      plot(
        ggplot() +
          geom_point(aes(x = X, y = Y), color = "lightgrey", data = Other_tibble) +
          ggforce::geom_circle(aes(x0 = X, y0 = Y, r = Average_Distance, fill = Average_Distance), alpha = 0.3, color = NA, data = COO_tibble)+
          geom_point(aes(x = X, y = Y), size = 2, color = "red", data = Target_tibble) +
          geom_point(aes(x = X, y = Y), size = 2, color = "blue", data = COO_tibble) +
          cowplot::theme_cowplot() +
          scale_x_continuous("", labels = NULL)+
          scale_y_continuous("", labels = NULL)+
          scale_fill_viridis_c("Average dist to target")+
          ggtitle(Image_name)+
          theme(panel.grid = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 20),
                legend.position = "bottom",
                plot.title = element_text(size = 25, hjust = 0.5, vjust = -3)) 
      )
    }
    
    else if(Strategy == "Max_Distance") {
      #Import general data frames
      DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_name)
      DATA_Distances <- DATA_Distances[[Image_name]]
      
      #Define the COO and Target_Cell
      COO <- DATA_Distances[[1]][[1,1]]
      Target_Cell <- DATA_Distances[[1]][[2,1]]
      
      #First prepare 3 tibbles, a tibble with COO cells, a tibble with target cells and a tibble with other cells
      Other_tibble <- DATA_Phenotypes %>% dplyr::filter(!Phenotype %in% c(COO, Target_Cell))
      Target_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == Target_Cell)
      COO_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == COO)
      
      #Calculate the max distance
      For_Join <- bind_cols(DATA_Distances[[2]][1],
                            map_dbl(1:nrow(DATA_Distances[[2]]), function(Row){
                              max(DATA_Distances[[2]][Row,-1])
                            })
      )
      names(For_Join) <- c("Cell_no", "Max_Distance")
      
      #Join the max distance with the COO tibble
      COO_tibble <- left_join(COO_tibble, For_Join, by = "Cell_no") %>% dplyr::filter(!is.na(Max_Distance))
      
      #Build the plot
      plot(
        ggplot() +
          geom_point(aes(x = X, y = Y), color = "lightgrey", data = Other_tibble) +
          ggforce::geom_circle(aes(x0 = X, y0 = Y, r = Max_Distance, fill = Max_Distance), alpha = 0.3, color = NA, data = COO_tibble)+
          geom_point(aes(x = X, y = Y), size = 2, color = "red", data = Target_tibble) +
          geom_point(aes(x = X, y = Y), size = 2, color = "blue", data = COO_tibble) +
          cowplot::theme_cowplot() +
          scale_x_continuous("", labels = NULL)+
          scale_y_continuous("", labels = NULL)+
          scale_fill_viridis_c("Max dist to target")+
          ggtitle(Image_name)+
          theme(panel.grid = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 20),
                legend.position = "bottom",
                plot.title = element_text(size = 25, hjust = 0.5, vjust = -3)) 
      )
    }
    
    else if(Strategy == "Cells_in_Radius"){
      #Import general data frames
      DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_name)
      DATA_Cumulative_Interaction <- DATA_Cumulative_Interaction[[Image_name]]
      
      #Check that radius size is present in the data
      if(!(as.character(Radius) %in% names(DATA_Cumulative_Interaction[[2]]))){
        stop(paste0("Radius size should be one of: ", str_c(names(DATA_Cumulative_Interaction[[2]])[-1], collapse = ", ")))
      }
      
      #Define the COO and Target_Cell
      COO <- DATA_Cumulative_Interaction[[1]][[1,1]]
      Target_Cell <- DATA_Cumulative_Interaction[[1]][[2,1]]
      
      #First prepare 3 tibbles, a tibble with COO cells, a tibble with target cells and a tibble with other cells
      Other_tibble <- DATA_Phenotypes %>% dplyr::filter(!Phenotype %in% c(COO, Target_Cell))
      Target_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == Target_Cell)
      COO_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == COO)
      
      #Calculate the cells within radius
      For_Join <- DATA_Cumulative_Interaction[[2]] %>% dplyr::select(1, as.character(Radius))
      names(For_Join) <- 
        names(For_Join) <- c("Cell_no", "Cells_in_Radius")
      
      #Join the max distance with the COO tibble
      COO_tibble <- left_join(COO_tibble, For_Join, by = "Cell_no") %>% dplyr::filter(!is.na(Cells_in_Radius))
      
      #Build the plot
      plot(
        ggplot() +
          geom_point(aes(x = X, y = Y), color = "lightgrey", data = Other_tibble) +
          ggforce::geom_circle(aes(x0 = X, y0 = Y, r = Radius, fill = Cells_in_Radius), alpha = 0.3, color = NA, data = COO_tibble)+
          geom_point(aes(x = X, y = Y), size = 2, color = "red", data = Target_tibble) +
          geom_point(aes(x = X, y = Y), size = 2, color = "blue", data = COO_tibble) +
          cowplot::theme_cowplot() +
          scale_x_continuous("", labels = NULL)+
          scale_y_continuous("", labels = NULL)+
          scale_fill_viridis_c(str_c("Cells in ", as.character(Radius), " radius"))+
          ggtitle(Image_name)+
          theme(panel.grid = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 20),
                legend.position = "bottom",
                plot.title = element_text(size = 25, hjust = 0.5, vjust = -3)) 
      )
    }
  }, 
  options = list(optimize = 3))

Gcross_calculator <- cmpfun(
  function(DATA = NULL,
           Cell_Of_Origin = NULL,
           Target_Cell = NULL,
           Stop_at = NULL, #Final distance where cumulative interaction analysis will stop
           Sampling_frequency = NULL, #Sampling distance interval
           Use_Clinical = NULL,
           DATA_Clinical = NULL,
           Clinical_var = NULL
  ) {
    #Argument check by ChatGTP
    if(is.null(Cell_Of_Origin) || !is.character(Cell_Of_Origin)) {
      stop("Cell_Of_Origin must be a non-null character value.")
    }
    if(is.null(Target_Cell) || !is.character(Target_Cell)) {
      stop("Target_Cell must be a non-null character value.")
    }
    if(!all(c(Cell_Of_Origin, Target_Cell) %in% unique(DATA$Phenotype))){
      stop(paste0("Cell_Of_Origin and Target_Cell must be one of the following: ", str_c(unique(DATA$Phenotype), collapse = ", ")))
    }
    if(is.null(Stop_at) || !is.numeric(Stop_at) || Stop_at <= 0 || Stop_at < Sampling_frequency) {
      stop("Stop_at must be a positive numeric value larger than Sampling frequency.")
    }
    if(is.null(Sampling_frequency) || !is.numeric(Sampling_frequency) || Sampling_frequency <= 0) {
      stop("Sampling_frequency must be a positive numeric value.")
    }
    if(is.null(Use_Clinical) || !is.logical(Use_Clinical)) {
      stop("Use_Clinical must be a logical (TRUE/FALSE) value.")
    }
    if(Use_Clinical) {
      if(is.null(Clinical_var) || !is.character(Clinical_var)) {
        stop("Clinical_var must be a non-null character value when Use_Clinical is TRUE.")
      }
      # Check if Clinical_var exists in DATA_Clinical
      if(!all(Clinical_var %in% colnames(DATA_Clinical))) {
        stop("Clinical_var must be a valid column in DATA_Clinical.")
      }
    }
    
    #First obtain our data
    DATA_Phenotypes <- DATA
    
    #Calculate if any sample needs to be removed from the analysis
    Samples_to_remove <- map_lgl(unique(DATA_Phenotypes$Subject_Names), function(Image){
      nrow(DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image, Phenotype %in% c(Cell_Of_Origin, Target_Cell)) %>% dplyr::count(Phenotype)) < 2
    })
    
    
    #If required, remove the troublesome samples and print a warning
    if(sum(Samples_to_remove) > 0){
      DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(!(Subject_Names %in% unique(DATA_Phenotypes$Subject_Names)[Samples_to_remove]))
      warning(paste0("Samples without COO or target cells will be removed from the analysis. ",
                     "The following samples will be removed: ", str_c(unique(DATA$Subject_Names)[Samples_to_remove], collapse = ", ")
      ))
    }
    
    #If not require continue with the analysis
    DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(Phenotype %in% c(Cell_Of_Origin, Target_Cell))
    
    
    #Generate our basic result tibble containing the Km calculation for each data
    RESULTS <- map_dfr(unique(DATA_Phenotypes$Subject_Names), function(x) {
      Interim <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == x)  #select cell types
      
      #Prepare point pattern
      point_pattern <- 
        spatstat.geom::ppp(x = Interim$X, y = Interim$Y,
                           n = nrow(Interim), 
                           window = spatstat.geom::owin(xrange = c(min(Interim$X), max(Interim$X)),
                                                        yrange = c(min(Interim$Y), max(Interim$Y))),
                           marks = factor(Interim$Phenotype, levels = c(Cell_Of_Origin, Target_Cell))
        )
      
      #obtain the results
      RESULTS <- as_tibble(spatstat.explore::Gcross(point_pattern,
                                                    i = Cell_Of_Origin, #Select cell of origin
                                                    j = Target_Cell, #Select target cell
                                                    r = seq(from = 0, to = Stop_at, by = Sampling_frequency), #select radius to calculate the Gcross function
                                                    correction=c("km")
      )
      )
      
      RESULTS <- RESULTS %>% mutate(Subject_Names = x) %>% dplyr::select(Subject_Names, r, theo, km)
    }, .progress = list(clear = F,
                        name = "Calculating G-cross function",
                        show_after = 1,
                        type = "iterator"))
    
    #If no clinical information provided then proceed with exit from the function
    if(!Use_Clinical) {
      try(plot(RESULTS %>% ggplot(aes(x = r, y = km, group = Subject_Names, color = Subject_Names)) + geom_line(linewidth = 1.2)+
                 cowplot::theme_cowplot() +
                 scale_x_continuous("Radius size") +
                 scale_y_continuous("Probability of encountering a Target Cell")+
                 scale_color_manual(values = unname(pals::polychrome(length(unique(DATA_Phenotypes$Subject_Names)))))
      ))
      
      AUC_results <- map_dfr(unique(RESULTS$Subject_Names), function(Image) {
        Interim <- RESULTS %>% dplyr::filter(Subject_Names == Image)
        
        AUC <-
          integrate(
            approxfun(Interim$r, Interim$km),
            lower = 0,
            upper = max(Interim$r),
            subdivisions = 2000
          )$value
        
        AUC_tibble <- tibble(
          Subject_Names = Image,
          AUC = AUC,
          COO = Cell_Of_Origin,
          Target_Cell = Target_Cell,
          Radius = Stop_at
        )
      })
      
      return(list(Raw_DATA = RESULTS,
                  Simplified_DATA = AUC_results))
    }
    
    #If clinical information provided then we can use the clinical information to  generate a graph
    else if(Use_Clinical) {
      print("Calculating association with clinical data")
      DATA_Clinical <- DATA_Clinical %>% dplyr::select(Subject_Names, all_of(Clinical_var))
      
      RESULTS <- left_join(RESULTS, DATA_Clinical, by = "Subject_Names")
      names(RESULTS)[5] <- "Clin_Var"
      
      plot(RESULTS %>% ggplot(aes(x = r, y = km, group = Subject_Names, color = Clin_Var)) + geom_line(linewidth = 1.2)+
             cowplot::theme_cowplot() +
             scale_x_continuous("Radius size") +
             scale_y_continuous("Probability of encountering a Target Cell")+
             scale_color_viridis_d(Clinical_var))
      
      names(RESULTS)[5] <- Clinical_var
      
      
      AUC_results <- map_dfr(unique(RESULTS$Subject_Names), function(Image) {
        Interim <- RESULTS %>% dplyr::filter(Subject_Names == Image)
        
        AUC <-
          integrate(
            approxfun(Interim$r, Interim$km),
            lower = 0,
            upper = max(Interim$r),
            subdivisions = 2000
          )$value
        
        AUC_tibble <- tibble(
          Subject_Names = Image,
          AUC = AUC,
          COO = Cell_Of_Origin,
          Target_Cell = Target_Cell,
          Radius = Stop_at,
          Clinical_var = unique(Interim[Clinical_var])[[1]]
        )
      })
      names(AUC_results)[6] <- Clinical_var
      return(list(Raw_DATA = RESULTS,
                  Simplified_DATA = AUC_results))
      
    }
  },
  options = list(optimize = 3))

Ripley_function_calculator <- cmpfun(
  function(DATA = NULL,
           Cell_type = NULL,
           Max_distance = NULL,
           Strategy = NULL,
           N_simulations = NULL){
    #Argument validation (Here all the functino has been optimized by ChatpGPT)
    if(is.null(Cell_type) || !is.character(Cell_type)) {
      stop("Cell_type must be a non-null character value.")
    }
    if(!Cell_type %in% unique((DATA %>% dplyr::select(Phenotype))[[1]])) stop(paste0("Cell_type must be one of the following: ", 
                                                                                     str_c(unique((DATA %>% dplyr::select(Phenotype))[[1]]), collapse = ", ")
    )
    )
    if(is.null(Max_distance) || !is.numeric(Max_distance) || Max_distance <= 0) {
      stop("Max_distance must be a positive numeric value.")
    }
    if(is.null(Strategy) || !Strategy %in% c("Ripleys_K", "Ripleys_L")) {
      stop("Strategy must be one of 'Ripleys_K' or 'Ripleys_L'.")
    }
    if(is.null(N_simulations) || !is.numeric(N_simulations) || N_simulations <= 0) {
      stop("N_simulations must be a positive numeric value.")
    }
    
    # Filter the dataset based on cell type
    DATA_Phenotypes <- DATA %>% dplyr::filter(Phenotype == Cell_type)
    
    # Map across unique subject names
    map_dfr(unique(DATA_Phenotypes$Subject_Names), function(Image) {
      Interim <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)  # Select cell type
      
      # Prepare point pattern
      point_pattern <- spatstat.geom::ppp(
        x = Interim$X, y = Interim$Y, 
        n = nrow(Interim), 
        window = spatstat.geom::owin(xrange = c(min(Interim$X), max(Interim$X)),
                                     yrange = c(min(Interim$Y), max(Interim$Y)))
      )
      
      # Define function parameters and run envelope calculations
      fun_name <- if(Strategy == "Ripleys_K") 'Kest' else 'Lest'
      
      Simulation <- spatstat.explore::envelope(
        point_pattern, 
        fun = fun_name,
        funargs = list(rmax = Max_distance, correction = c("Ripley"), 
                       nlarge = 3000, var.approx = T, ratio = FALSE),
        nsim = N_simulations, 
        verbose = FALSE
      ) 
      
      Results <- as_tibble(Simulation)
      
      if(any(is.na(Results))) {
        warning(paste(Strategy, "function could not be calculated. Try switching Max_distance argument"))
        RESULT_tibble <- tibble(
          Subject_Names = Image,
          Observed_AUC = NA,
          CSR_AUC = NA,
          Observed_limit_AUC = NA,
          Ratio = NA,
          Significant = NA
        )
      } else {
        # Calculate AUC for observed and CSR values
        Observed_AUC <- integrate(approxfun(Results$r, Results$obs), lower = 0, upper = max(Results$r), subdivisions = 2000)$value
        CSR_AUC <- integrate(approxfun(Results$r, Results$theo), lower = 0, upper = max(Results$r), subdivisions = 2000)$value
        Ratio <- Observed_AUC / CSR_AUC
        
        # Determine significance
        if(all(Ratio > 1 & !any(is.nan(Results$lo)))) {
          Significant <- CSR_AUC < integrate(approxfun(Results$r, Results$lo), lower = 0, upper = max(Results$r), subdivisions = 2000)$value
          Observed_limit_AUC <- integrate(approxfun(Results$r, Results$lo), lower = 0, upper = max(Results$r), subdivisions = 2000)$value
        } else if(all(Ratio < 1 & !any(is.nan(Results$hi)))) {
          Significant <- Observed_AUC > integrate(approxfun(Results$r, Results$hi), lower = 0, upper = max(Results$r), subdivisions = 2000)$value
          Observed_limit_AUC <- integrate(approxfun(Results$r, Results$hi), lower = 0, upper = max(Results$r), subdivisions = 2000)$value
        } else {
          Significant <- NA
          Observed_limit_AUC <- NA
        }
        
        RESULT_tibble <- tibble(
          Subject_Names = Image,
          Observed_AUC = Observed_AUC,
          CSR_AUC = CSR_AUC,
          Observed_limit_AUC = Observed_limit_AUC,
          Ratio = Ratio,
          Significant = Significant
        )
      }
      
      return(RESULT_tibble)
      
    }, .progress = list(clear = FALSE,
                        name = "Calculating spatial aggregation function",
                        show_after = 1,
                        type = "iterator"))
  },
  options = list(optimize = 3))  

Multi_level_modelling_function <- cmpfun(
  function(DATA_cumulative = NULL,
           DATA_Clinical = NULL,
           Clinical_var = NULL,
           DATA_Densities = NULL,
           Cell_Of_Origin = NULL,
           Target_cell = NULL,
           Calculate_R2 = NULL,
           N_bootstrap = NULL) {
    if(length(Clinical_var) > 1) {
      stop("Select a single clinical variable")
    }
    if(!(Clinical_var %in% names(DATA_Clinical))) {
      stop(paste0("Clinical var should be one of: ", str_c(names(DATA_Clinical)[-1], collapse = ", ")))
    }
    if(is.double(DATA_Clinical[[Clinical_var]])) {
      stop("Clinical variable should be a character vector, not a double vector")
    }
    if(!any(str_detect(names(DATA_Densities), Target_cell))){
      stop("Target cell density should be present in DATA_Densities")
    }
    if(!is.logical(Calculate_R2)) stop("Calculate_R2 must be a logical value")
    if(Calculate_R2){
      if(!all(N_bootstrap >= 1 & N_bootstrap%%1 == 0)) stop("N_bootstrap must be an integer value > 0")
    }
    
    
    #Import the data frames to the function
    DATA_cumulative <- DATA_cumulative
    DATA_Clinical <- DATA_Clinical %>% dplyr::select(Subject_Names, all_of(Clinical_var))
    DATA_Densities <- DATA_Densities %>% dplyr::select(Subject_Names, contains("Density_")) %>% dplyr::select(Subject_Names,
                                                                                                              contains(str_c("Density_", Target_cell, sep = "")))
    
    #First we need to create a dataframe with in an adequate format
    print("Formatting data")
    Interim <- map_dfr(1:length(DATA_cumulative), function(Image) {
      DATA_cumulative[[Image]][[2]] %>% pivot_longer(-1) %>% 
        mutate(name = as.double(name),
               Cell_Of_Origin_ID = str_c(names(DATA_cumulative)[Image], Cell_Of_Origin_no, sep = "_"),
               Subject_Names = names(DATA_cumulative)[Image]) %>%
        dplyr::select(Subject_Names, Cell_Of_Origin_ID, name, value)
    })
    
    
    #Add the target density info
    Interim <- left_join(Interim, DATA_Densities, by = "Subject_Names")
    #Add the clinical information
    Interim <- left_join(Interim, DATA_Clinical, by = "Subject_Names")
    #Modify col names
    names(Interim)[-c(1:2)] <- c("DIST", "N_Target", "Density_Target", "Clin_Group")
    
    Interim <- Interim %>% mutate(DIST = as.double(scale(DIST)),
                                  Density_Target = as.double(scale(Density_Target)),
                                  N_Target = as.double(scale(N_Target)))
    
    #Fit the model
    print("Fitting the multi-level model")
    Model <- lme4::lmer(N_Target ~ Clin_Group*DIST + Density_Target + (1|Cell_Of_Origin_ID), data = Interim)
    print(summary(Model))
    
    #Make predictions
    tryCatch({
      Prediction <- expand_grid(unique(Interim$DIST),
                                unique(Interim$Clin_Group))
      names(Prediction) <- c("DIST", "Clin_Group")
      Prediction$Density_Target <- 0
      Prediction$Cell_Of_Origin_ID <- unique(Interim$Cell_Of_Origin_ID)[1]
      Prediction$Prediction <- predict(Model, newdata = Prediction)
      #plot results
      plot(Prediction %>%
             ggplot(aes(x = DIST, y  = Prediction, group = Clin_Group, color = Clin_Group)) +
             geom_line(linewidth = 1.5) +
             cowplot::theme_cowplot() +
             scale_x_continuous(str_c("Scaled Distance from ", Cell_Of_Origin, sep = "")) +
             scale_y_continuous(str_c("Scaled number of ", Target_cell, " cells", sep = "")) +
             scale_color_viridis_d(Clinical_var))
    },
    error = function(cond) print("Unable to calculate Multi-level model predictions. Consider subsetting samples or modifying cumulative interaction parameters.")
    )
    
    
    #Calculate R2 values if required by user
    if(Calculate_R2){
      print("Calculating model R2 - This can take some time...")
      try(
        Model_R2 <- 
          partR2::partR2(Model, partvars = c("DIST", "Clin_Group:DIST", "Density_Target"), R2_type = "conditional", nboot = N_bootstrap, data = Interim)
      )
      if(berryFunctions::is.error(Model_R2)) warning("Unable to calculate predictos R^2 for the current model. This may occur with very large datasets")
      else print(summary(Model_R2))
    }
    
    
  },
  options = list(optimize = 3))

Advanced_Distance_function_A_B_C <- cmpfun(
  function(DATA, cell_A, cell_B, cell_C){
    Tibble_A <- DATA %>% dplyr::filter(Phenotype == cell_A) 
    Tibble_B <- DATA %>% dplyr::filter(Phenotype == cell_B)
    Tibble_C <- DATA %>% dplyr::filter(Phenotype == cell_C)
    
    #First tibble A to B cell types
    Tibble_interim <- as_tibble(expand.grid(Tibble_A$Cell_no, Tibble_B$Cell_no))
    names(Tibble_interim) <- c("Cell_no", "NOTHING")
    Tibble_interim <- left_join(Tibble_interim, Tibble_A, by = "Cell_no") %>% dplyr::select(1:4)
    names(Tibble_interim) <- c("Cell_A", "Cell_no", "X_Cell_A", "Y_Cell_A")
    Tibble_interim <- left_join(Tibble_interim, Tibble_B, by = "Cell_no") %>% dplyr::select(1:6)
    names(Tibble_interim) <- c("Cell_A_no", "Cell_B_no", "X_Cell_A", "Y_Cell_A", "X_Cell_B", "Y_Cell_B")
    
    Final_tibble_A_B <- Tibble_interim %>% mutate(DIST = sqrt((X_Cell_A - X_Cell_B)^2 + (Y_Cell_A - Y_Cell_B)^2)) %>%
      dplyr::select(Cell_A_no, Cell_B_no, DIST) %>% pivot_wider(id_cols = Cell_A_no, names_from = Cell_B_no, values_from = DIST) %>%
      dplyr::rename("Cell_Of_Origin_no" = "Cell_A_no")
    
    #Second tibble A to C cell types
    Tibble_interim <- as_tibble(expand.grid(Tibble_A$Cell_no, Tibble_C$Cell_no))
    names(Tibble_interim) <- c("Cell_no", "NOTHING")
    Tibble_interim <- left_join(Tibble_interim, Tibble_A, by = "Cell_no") %>% dplyr::select(1:4)
    names(Tibble_interim) <- c("Cell_A", "Cell_no", "X_Cell_A", "Y_Cell_A")
    Tibble_interim <- left_join(Tibble_interim, Tibble_C, by = "Cell_no") %>% dplyr::select(1:6)
    names(Tibble_interim) <- c("Cell_A_no", "Cell_C_no", "X_Cell_A", "Y_Cell_A", "X_Cell_C", "Y_Cell_C")
    Final_tibble_A_C <- Tibble_interim %>% mutate(DIST = sqrt((X_Cell_A - X_Cell_C)^2 + (Y_Cell_A - Y_Cell_C)^2)) %>%
      dplyr::select(Cell_A_no, Cell_C_no, DIST) %>% pivot_wider(id_cols = Cell_A_no, names_from = Cell_C_no, values_from = DIST) %>%
      dplyr::rename("Cell_Of_Origin_no" = "Cell_A_no")
    
    #Final result
    list(A_to_B <- Final_tibble_A_B,
         A_to_C <- Final_tibble_A_C)
  }, 
  options = list(optimize = 3))

Trio_Distance_matrix_generator <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           Cell_Of_Origin = NULL,
           Target_Cell_1 = NULL,
           Target_Cell_2 = NULL,
           Perform_edge_correction = NULL,
           Hull_ratio = NULL,
           Distance_to_edge = NULL
  ) {
    
    #First get our required functions
    if(!exists(DATA, envir = .GlobalEnv)){
      stop("A DATA_Phenotypes object must be created before running the analysis. Check name supplied to the DATA argument")
    }
    DATA_Phenotypes <- get(DATA, envir = .GlobalEnv)
    if(!exists("Advanced_Distance_function_A_B_C", envir = .GlobalEnv)){
      stop("Please execute STEP 5 REQUIRED FUNCTIONS before proceeding with the analysis")
    }
    Advanced_Distance_function_A_B_C <- get("Advanced_Distance_function_A_B_C", envir = .GlobalEnv)
    if(!identical(names(DATA_Phenotypes)[1:4],  c("Cell_no", "X", "Y", "Subject_Names"))) { #Check if Data is correctly formatted
      stop("DATA provided should have an adecuate format")
    }
    if(!("Phenotype" %in% names(DATA_Phenotypes))) {
      stop("DATA should contain a column named Phenotype specifying the cell types")
    }
    if(!all(c(Cell_Of_Origin %in% unique(DATA_Phenotypes$Phenotype), 
              Target_Cell_1 %in% unique(DATA_Phenotypes$Phenotype),
              Target_Cell_2 %in% unique(DATA_Phenotypes$Phenotype)
    )
    )) { #Check if provided cell types are in the phenotype variable
      stop(paste0("Cell of origin provided and Target cells should be one of: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
    }
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(Perform_edge_correction){
      if(!all(is.numeric(Hull_ratio), Hull_ratio >= 0, Hull_ratio <= 1)) stop("Hull_ratio must be a numeric value between 0 and 1")
      if(!all(is.numeric(Distance_to_edge), Distance_to_edge > 0)) stop("Distance_to_edge must be a numeric value > 0")
    }
    
    #Perform a random test to allow user to stop the computation if edge correction parameter is not desired
    if(Perform_edge_correction){
      print("Running edge correction example on a random sample")
      Sample <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == sample(unique(DATA_Phenotypes$Subject_Names), size = 1))
      Cells_sf <- sf::st_as_sf(Sample , coords = c("X", "Y"))
      Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
      Cells_in_Border_vector <- unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = FALSE, dist = Distance_to_edge))
      
      plot(Sample %>% 
             dplyr::mutate(Removed = Cells_in_Border_vector) %>%
             ggplot(aes(x = X, y = Y, color = Cells_in_Border_vector)) +
             geom_point() +
             scale_color_manual("", labels = c("Included", "Removed"), values = c("black", "grey")) +
             theme_minimal() + 
             scale_x_continuous("") + 
             scale_y_continuous("") +
             theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   legend.position = "bottom",
                   legend.text = element_text(size = 12)))
      
      #Ask the user if the algorihtm should proceed
      answer <- menu(c("Proceed", "Abort"), title = "Should the analysis proceed")
      #If user decides to stop then abort function and return stop message
      if(answer == 2) stop("The function has been stopped. Please tune edge correction parameters for a better result")
      
      #Remove cells
      Keep_vector_list <- map(unique(DATA_Phenotypes$Subject_Names), function(x){
        #Prepare our data
        Image_tibble <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == x)
        Cells_sf <- sf::st_as_sf(Image_tibble , coords = c("X", "Y"))
        Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
        Cells_in_Border_vector <- unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = F, dist = Distance_to_edge))
        #Calculate cells in border
        COO_in_Border_vector <- Image_tibble$Phenotype == Cell_Of_Origin & Cells_in_Border_vector
        
        #Print message to warn COO removed in analysis
        message(paste0("Sample ", as.character(x), ": ", sum(COO_in_Border_vector), " / ", sum(Image_tibble$Phenotype == Cell_Of_Origin), " ", Cell_Of_Origin, " cell/s will be removed due to edge proximity.")) 
        
        #Return a vector with the cells to keep (either no COO or COO not in edge)
        return(!COO_in_Border_vector)
      })
      names(Keep_vector_list) <- unique(DATA_Phenotypes$Subject_Names)
    }
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Proceed with computation
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    RESULTS <- 
      furrr::future_map(unique(DATA_Phenotypes$Subject_Names),  function(x) {
        #Prepare our data
        Image_tibble <-
          DATA_Phenotypes %>% dplyr::filter(Subject_Names == x)
        #First we will filter cells of origin in the border as desired by the user
        if(Perform_edge_correction){
          Cells_to_keep <- Keep_vector_list[[x]]
          Image_tibble <- Image_tibble[Cells_to_keep,]
        }
        
        Pre_Cell_counts <-
          Image_tibble %>% dplyr::count(Phenotype) %>% dplyr::filter(Phenotype %in% c(Cell_Of_Origin, Target_Cell_1, Target_Cell_2))#Select cells of interest
        #Build a DF where the first row is the COO cell count and the second row is the Target cell count
        Cell_counts <-
          bind_rows(
            Pre_Cell_counts %>% dplyr::filter(Phenotype == Cell_Of_Origin),
            Pre_Cell_counts %>% dplyr::filter(Phenotype == Target_Cell_1),
            Pre_Cell_counts %>% dplyr::filter(Phenotype == Target_Cell_2)
          )
        
        #Acount for samples without any COO or Target Cell 1 or 2
        if (nrow(Cell_counts) < 3) {
          return(
            list(
              Cell_counts = Cell_counts,
              A_to_B = tibble(
                Cell_Of_Origin_no = NA,
                Target_cell_1 = NA
              ),
              A_to_C = tibble(
                Cell_Of_Origin_no = NA,
                Target_cell_1 = NA
              )
            )
          )
        }
        
        #If everything is OK proceed to calculate the distance matrices
        Distance_matrices <- Advanced_Distance_function_A_B_C(DATA = Image_tibble, cell_A = Cell_Of_Origin, cell_B = Target_Cell_1, cell_C = Target_Cell_2)
        return(
          list(Cell_counts = Cell_counts,
               A_to_B = Distance_matrices[[1]],
               A_to_C = Distance_matrices[[2]])
        )
      },
      .progress = TRUE) 
    future::plan("future::sequential")
    gc()
    #Add names
    names(RESULTS) <- unique(DATA_Phenotypes$Subject_Names)
    
    #Calculate which samples have NA values and should be removed
    Samples_to_remove <- map_lgl(RESULTS, function(x) is.na(x[[2]][[1,1]]))
    
    #If present print a warning and remove the samples
    if(sum(Samples_to_remove) > 0) {
      warning(paste0("Samples without COO or target cells will be removed from the analysis. ",
                     "The following samples will be removed: ", str_c(names(RESULTS)[Samples_to_remove], collapse = ", ")
      )
      )
      return(RESULTS[!Samples_to_remove])
    }
    
    #Else return the resulsts straight forward
    else{
      return(RESULTS)
    }
    
  }, 
  options = list(optimize = 3))

Advanced_Distance_function_A_B_C_single <- 
  cmpfun(
    function(DATA, 
             cell_A, 
             cell_B, 
             cell_C
             ){
  Tibble_A <- DATA %>% dplyr::filter(Phenotype == cell_A) %>% sample_n(size = 1)
  Tibble_B <- DATA %>% dplyr::filter(Phenotype == cell_B)
  Tibble_C <- DATA %>% dplyr::filter(Phenotype == cell_C)
  
  #First tibble A to B cell types
  Tibble_interim <- as_tibble(expand.grid(Tibble_A$Cell_no, Tibble_B$Cell_no))
  names(Tibble_interim) <- c("Cell_no", "NOTHING")
  Tibble_interim <- left_join(Tibble_interim, Tibble_A, by = "Cell_no") %>% dplyr::select(1:4)
  names(Tibble_interim) <- c("Cell_A", "Cell_no", "X_Cell_A", "Y_Cell_A")
  Tibble_interim <- left_join(Tibble_interim, Tibble_B, by = "Cell_no") %>% dplyr::select(1:6)
  names(Tibble_interim) <- c("Cell_A_no", "Cell_B_no", "X_Cell_A", "Y_Cell_A", "X_Cell_B", "Y_Cell_B")
  
  Final_tibble_A_B <- Tibble_interim %>% mutate(DIST = sqrt((X_Cell_A - X_Cell_B)^2 + (Y_Cell_A - Y_Cell_B)^2)) %>%
    dplyr::select(Cell_A_no, Cell_B_no, DIST) %>% pivot_wider(id_cols = Cell_A_no, names_from = Cell_B_no, values_from = DIST) %>%
    dplyr::rename("Cell_Of_Origin_no" = "Cell_A_no")
  
  #Second tibble A to C cell types
  Tibble_interim <- as_tibble(expand.grid(Tibble_A$Cell_no, Tibble_C$Cell_no))
  names(Tibble_interim) <- c("Cell_no", "NOTHING")
  Tibble_interim <- left_join(Tibble_interim, Tibble_A, by = "Cell_no") %>% dplyr::select(1:4)
  names(Tibble_interim) <- c("Cell_A", "Cell_no", "X_Cell_A", "Y_Cell_A")
  Tibble_interim <- left_join(Tibble_interim, Tibble_C, by = "Cell_no") %>% dplyr::select(1:6)
  names(Tibble_interim) <- c("Cell_A_no", "Cell_C_no", "X_Cell_A", "Y_Cell_A", "X_Cell_C", "Y_Cell_C")
  Final_tibble_A_C <- Tibble_interim %>% mutate(DIST = sqrt((X_Cell_A - X_Cell_C)^2 + (Y_Cell_A - Y_Cell_C)^2)) %>%
    dplyr::select(Cell_A_no, Cell_C_no, DIST) %>% pivot_wider(id_cols = Cell_A_no, names_from = Cell_C_no, values_from = DIST) %>%
    dplyr::rename("Cell_Of_Origin_no" = "Cell_A_no")
  
  #Final result
  list(A_to_B <- Final_tibble_A_B,
       A_to_C <- Final_tibble_A_C)
  
  
},
options = list(optimize = 3))

Trio_Random_Distance_matrix_generator <- 
  cmpfun(
    function(N_cores = NULL,
             DATA = NULL,
             Cell_Of_Origin = NULL,
             Target_Cell_1 = NULL,
             Target_Cell_2 = NULL,
             Random_cells_per_sample = NULL,
             Perform_edge_correction = NULL,
             Hull_ratio = NULL,
             Distance_to_edge = NULL
    ) {
      #Check arguments
      if(!exists(DATA, envir = .GlobalEnv)){
        stop("A DATA_Distance object must be created before running the tiling analysis. Check name supplied to the DATA argument")
      }
      DATA_Phenotypes <- get(DATA, envir = .GlobalEnv)
      if(!exists("Advanced_Distance_function_A_B_C_single",  envir = .GlobalEnv)){
        stop("Please execute required STEP 5 functions")
      }
      Advanced_Distance_function_A_B_C_single <- get("Advanced_Distance_function_A_B_C_single",  envir = .GlobalEnv)
      if(!identical(names(DATA_Phenotypes)[1:4],  c("Cell_no", "X", "Y", "Subject_Names"))) { #Check if Data is correctly formatted
        stop("DATA provided should have an adecuate format")
      }
      if(!("Phenotype" %in% names(DATA_Phenotypes))) {
        stop("DATA should contain a column named Phenotype specifying the cell types")
      }
      #Check if provided cell types are in the phenotype variable
      if(!all(c(Cell_Of_Origin %in% unique(DATA_Phenotypes$Phenotype), 
                Target_Cell_1 %in% unique(DATA_Phenotypes$Phenotype),
                Target_Cell_2 %in% unique(DATA_Phenotypes$Phenotype)))
      ) stop(paste0("Cell of origin provided and Target cells should be one of: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
      if(!all(Random_cells_per_sample%%1 == 0, Random_cells_per_sample > 0)) stop("Random_cells_per_sample must be an integer value > 0")
      if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
      if(!is.logical(Perform_edge_correction)) stop("Perform_edge_correction must be a logical value")
      if(Perform_edge_correction){
        if(!all(is.numeric(Hull_ratio), Hull_ratio >= 0, Hull_ratio <= 1)) stop("Hull_ratio must be a numeric value between 0 and 1")
        if(!all(is.numeric(Distance_to_edge), Distance_to_edge > 0)) stop("Distance_to_edge must be a numeric value > 0")
      }
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      #Perform Randomization of labels and calculate distances
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      
      RESULTS <-suppressWarnings({
        furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(Images){
          #Prepare our data
          Image_tibble <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Images)
          Pre_Cell_counts <- Image_tibble %>% dplyr::count(Phenotype) %>% dplyr::filter(Phenotype %in% c(Cell_Of_Origin, Target_Cell_1, Target_Cell_2))#Select cells of interest
          #Build a DF where the first row is the COO cell count and the second row is the Target cell count
          Cell_counts <- bind_rows(Pre_Cell_counts %>% dplyr::filter(Phenotype == Cell_Of_Origin),
                                   Pre_Cell_counts %>% dplyr::filter(Phenotype == Target_Cell_1),
                                   Pre_Cell_counts %>% dplyr::filter(Phenotype == Target_Cell_2))
          
          #Acount for samples without any COO or Target Cell 1 or 2
          if(nrow(Cell_counts) < 3) {
            return(
              list(
                Cell_counts = Cell_counts,
                A_to_B = tibble(
                  Cell_Of_Origin_no = NA,
                  Target_cell_1 = NA
                ),
                A_to_C = tibble(
                  Cell_Of_Origin_no = NA,
                  Target_cell_1 = NA
                )
              )
            )
          }
          
          #If border cell correction required generate the border polygon
          if(Perform_edge_correction){
            Cells_sf <- sf::st_as_sf(Image_tibble , coords = c("X", "Y"))
            Edge_line <- sf::st_cast((Cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
            Cells_in_Border_vector <- as.vector(unlist(sf::st_is_within_distance(Cells_sf, Edge_line, sparse = F, dist = Distance_to_edge)))
          }
          
          #If all ok proceed with analysis
          Distance_matrix <- map(seq_along(1:Random_cells_per_sample), function(y){ #Number of random cell generated by each sample
            I_dont_care <- y
            #First generate new random cell type labels
            new_Phenotype <- sample(Image_tibble$Phenotype, size = nrow(Image_tibble), replace = FALSE)
            Random_tibble <- Image_tibble %>% mutate(Phenotype = new_Phenotype)
            
            
            #If needed remove COO that are close to border
            if(Perform_edge_correction){
              Random_tibble <- Random_tibble[!(Random_tibble$Phenotype == Cell_Of_Origin & Cells_in_Border_vector), ]
              
              #If no random cells are remaining then resample the labels once again
              while(nrow(Random_tibble %>% dplyr::filter(Phenotype == Cell_Of_Origin)) == 0){
                new_Phenotype <- sample(Image_tibble$Phenotype, size = nrow(Image_tibble), replace = F)
                Random_tibble <- Image_tibble %>% mutate(Phenotype = new_Phenotype)
                Random_tibble <- Random_tibble[!(Random_tibble$Phenotype == Cell_Of_Origin & Cells_in_Border_vector), ]
              }
            }
            
            #Then apply distance function
            Distance_matrix <- Advanced_Distance_function_A_B_C_single(DATA = Random_tibble, cell_A = Cell_Of_Origin, cell_B = Target_Cell_1, cell_C = Target_Cell_2) #Select the cell of origin (cell_A) and target cells (B and C)
            return(Distance_matrix)
          })
          
          #Obtain individual distance matrices
          Cell_A_to_B <- map_dfr(Distance_matrix, function(x) {
            Interim <- x[[1]]
            names(Interim)[-1] <- str_c("Random_Target_cell_", as.character(1:(ncol(Interim)-1)))
            return(Interim)
          })
          Cell_A_to_C <- map_dfr(Distance_matrix, function(x) {
            Interim <- x[[2]]
            names(Interim)[-1] <- str_c("Random_Target_cell_", as.character(1:(ncol(Interim)-1)))
            return(Interim)
          })
          
          #Return all three elements
          return(
            list(
              Cell_counts = Cell_counts,
              A_to_B = Cell_A_to_B,
              A_to_C = Cell_A_to_C
            )
          )
          
        },
        .progress = TRUE)
      })
      future::plan("future::sequential")
      gc()
      
      #Change the names of the RESULTS list
      names(RESULTS) <- unique(DATA_Phenotypes$Subject_Names)
      
      #Calculate for which samples the distance matrix could not be calculated
      Samples_to_remove <- map_lgl(RESULTS, function(x) is.na(x[[2]][[1,1]]))
      
      #If samples to remove are present print a warning and proceed to remove required samples
      if(sum(Samples_to_remove) > 0) {
        warning(paste0("Samples without COO or target cells will be removed from the analysis. ",
                       "The following samples will be removed: ", str_c(names(RESULTS)[Samples_to_remove], collapse = ", ")
        )
        )
        return(RESULTS[!Samples_to_remove])
      }
      
      #If no samples need to be removed return the unmodified RESULTS tibble
      else{
        return(RESULTS)
      }
      
    },
    options = list(optimize = 3))

Trio_Cumulative_Interaction_generator <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           Start_from = NULL,
           Stop_at = NULL,
           Sampling_frequency = NULL
  ) {
    #Check arguments
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!all(is.character(DATA), exists(DATA, envir = .GlobalEnv))) stop("DATA argument must be the name of an existing object")
    if(!all(is.numeric(Start_from), is.numeric(Stop_at), is.numeric(Sampling_frequency), Start_from < Stop_at, Sampling_frequency < (Stop_at - Start_from))) {
      stop("Start_from, Stop_at and Sampling_frequency must be numeric values. Start_from must be smaller than Stop_at. Sampling_frequency must be smaller than the range Start_from - Stop_at")
    }
    if(!all(Start_from >= 0, Stop_at > 0, Sampling_frequency > 0)) stop("Start_from, Stop_at and Sampling_frequency must have positive values")
    
    #Get our data from the global environment
    DATA <- get(DATA, envir = .GlobalEnv)
    
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Prepare our clustering
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <-
      suppressMessages({
        furrr::future_map(DATA, function(x) {
          #Get our tibble and format it adequately and remove NA values if necessary
          Longer_distance_matrix_A_to_B <-  x$A_to_B %>% pivot_longer(2:ncol(x$A_to_B)) %>% na.omit()
          
          Longer_distance_matrix_A_to_C <-  x$A_to_C %>% pivot_longer(2:ncol(x$A_to_C)) %>% na.omit()
          
          #Calculate the cumulative distance tibble with the desired sampling strategy for both A to B
          Cumulative_distance_tibble_A_to_B <-
            map_dfc(seq(from = Start_from, to = Stop_at, by = Sampling_frequency), function(filter) {
              Interim <- Longer_distance_matrix_A_to_B %>% mutate(counts = value <= filter) %>% group_by(Cell_Of_Origin_no, counts) %>% dplyr::count() %>% dplyr::filter(counts == T) %>%
                ungroup() %>% dplyr::select(-counts)
              Final <- left_join(tibble(Cell_Of_Origin_no = unique(Longer_distance_matrix_A_to_B$Cell_Of_Origin_no)), Interim, by = "Cell_Of_Origin_no") %>% 
                mutate(number = case_when(is.na(n) ~ 0,
                                          TRUE ~ n)) %>% dplyr::select(-n)
              names(Final) <- c("Cell_Of_Origin_no", as.character(filter))
              Final
            }) %>% dplyr::select(-contains("Cell")) %>% mutate(Cell_Of_Origin_no = unique(Longer_distance_matrix_A_to_B$Cell_Of_Origin_no))
          Cumulative_distance_tibble_A_to_B <- Cumulative_distance_tibble_A_to_B[c(ncol(Cumulative_distance_tibble_A_to_B), 1:(ncol(Cumulative_distance_tibble_A_to_B)-1))]
          
          #Calculate the cumulative distance tibble with the desired sampling strategy for for both A to C
          Cumulative_distance_tibble_A_to_C <-
            map_dfc(seq(from = Start_from, to = Stop_at, by = Sampling_frequency), function(filter) {
              Interim <- Longer_distance_matrix_A_to_C %>% mutate(counts = value <= filter) %>% group_by(Cell_Of_Origin_no, counts) %>% dplyr::count() %>% dplyr::filter(counts == T) %>%
                ungroup() %>% dplyr::select(-counts)
              Final <- left_join(tibble(Cell_Of_Origin_no = unique(Longer_distance_matrix_A_to_C$Cell_Of_Origin_no)), Interim, by = "Cell_Of_Origin_no") %>% 
                mutate(number = case_when(is.na(n) ~ 0,
                                          TRUE ~ n)) %>% dplyr::select(-n)
              names(Final) <- c("Cell_Of_Origin_no", as.character(filter))
              Final
            }) %>% dplyr::select(-contains("Cell")) %>% mutate(Cell_Of_Origin_no = unique(Longer_distance_matrix_A_to_C$Cell_Of_Origin_no))
          Cumulative_distance_tibble_A_to_C <- Cumulative_distance_tibble_A_to_C[c(ncol(Cumulative_distance_tibble_A_to_C), 1:(ncol(Cumulative_distance_tibble_A_to_C)-1))]
          
          #Now we calculate the TRIO Socore (square root of Cumulative interaction AB * Cumulative interaction AC) combining the cumulative interaction matrices
          Trio_Score <- bind_cols(Cumulative_distance_tibble_A_to_C[1], as_tibble(sqrt(Cumulative_distance_tibble_A_to_B[-1] * Cumulative_distance_tibble_A_to_C[-1])))
          
          #Return the final result
          list(Cell_counts = x[[1]],
               Cum_dist_A_to_B = Cumulative_distance_tibble_A_to_B,
               Cum_dist_A_to_C = Cumulative_distance_tibble_A_to_C,
               Trio_Score = Trio_Score
          )
        },
        .progress = TRUE)
      })
    future::plan("future::sequential")
    gc()
    
    names(RESULTS) <- names(DATA)
    #Return the results
    return(RESULTS)
  },
  options = list(optimize = 3))

Trio_Min_Distance_analyzer <- cmpfun(
  function(N_cores = NULL,
           DATA = NULL,
           DATA_RANDOM = NULL,
           Include_Random = NULL,
           By_Sample_Random = NULL
  ) {
    #Check arguments
    if(!is.logical(Include_Random)) stop("Include_Random should be a logical value")    
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    #Specify if random samples should be include in the computation
    if(Include_Random) {
      #Check arguments
      if(!is.logical(By_Sample_Random)) stop("By_Sample_Random should be a logical value") 
      if(!length(intersect(names(DATA), names(DATA_RANDOM))) == length(unique(c(names(DATA), names(DATA_RANDOM))))){
        outersect <- function(x, y) {
          sort(c(x[!x%in%y],
                 y[!y%in%x]))
        }
        Removed_cases <- outersect(names(DATA), names(DATA_RANDOM))
        
        message(paste0("Only samples present in DATA and DATA_RANDOM will be used. The following samples will be removed: ",
                       str_c(Removed_cases, collapse = ", ")))
        DATA <- DATA[intersect(names(DATA), names(DATA_RANDOM))]
        DATA_RANDOM <- DATA_RANDOM[intersect(names(DATA), names(DATA_RANDOM))]
      }
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      #Generate results (average min distance to TRIO)
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
        #Obtain the 2 individual distance matrices
        Interim_A_to_B <- DATA[[2]]
        Interim_A_to_C <- DATA[[3]]
        #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
        Max_min_dist <- 
          map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
            Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
            Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
            max(c(Min_A_to_B, Min_A_to_C))
          })
        #Calculate the average min distance to TRIO
        mean(Max_min_dist)
      }, 
      .progress = TRUE)
      
      #Generate the SE of the mean
      RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
        Interim_A_to_B <- DATA[[2]]
        Interim_A_to_C <- DATA[[3]]
        #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
        Max_min_dist <- 
          map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
            Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
            Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
            max(c(Min_A_to_B, Min_A_to_C))
          })
        #Calculate the SE
        sd(Max_min_dist) / sqrt(length(Max_min_dist))
      }, 
      .progress = TRUE)
      
      #Calculate Sample-wise random distribution
      if(By_Sample_Random){
        #Generate Random Results (average min distance to TRIO)
        RESULTS_RANDOM <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
          Interim_A_to_B <- DATA[[2]]
          Interim_A_to_C <- DATA[[3]]
          #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
          Max_min_dist <- 
            map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
              Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
              Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
              max(c(Min_A_to_B, Min_A_to_C))
            })
          #Calculate the average min distance to TRIO
          mean(Max_min_dist)
        }, .progress = TRUE)
        
        #Generate the SE of the RANDOM mean
        RESULTS_RANDOM_SE <- furrr::future_map_dbl(DATA_RANDOM, function(DATA) {
          Interim_A_to_B <- DATA[[2]]
          Interim_A_to_C <- DATA[[3]]
          #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
          Max_min_dist <- 
            map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
              Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
              Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
              max(c(Min_A_to_B, Min_A_to_C))
            })
          #Calculate the SE
          sd(Max_min_dist) / sqrt(length(Max_min_dist))
        }, .progress = TRUE)
      }
      
      #Calculate Experiment-wise random distribution
      if(!By_Sample_Random){
        #Generate Random Results (average min distance to TRIO)
        RESULTS_RANDOM <- mean(unlist(
          furrr::future_map(DATA_RANDOM, function(DATA) {
            Interim_A_to_B <- DATA[[2]]
            Interim_A_to_C <- DATA[[3]]
            #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
            Max_min_dist <- 
              map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
                Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
                Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
                max(c(Min_A_to_B, Min_A_to_C))
              })
            Max_min_dist
          }, .progress = TRUE)
        ))
        
        #Generate the SE of the RANDOM mean
        RESULTS_RANDOM_SE <- 
          sd(unlist(
            map(DATA_RANDOM, function(DATA) {
              Interim_A_to_B <- DATA[[2]]
              Interim_A_to_C <- DATA[[3]]
              #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
              Max_min_dist <- 
                map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
                  Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
                  Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
                  max(c(Min_A_to_B, Min_A_to_C))
                })
              Max_min_dist
            }, .progress = list(clear = F,
                                name = "Calculate Random CI - Stage 1",
                                show_after = 1,
                                type = "iterator"))
          )) / sqrt(length(unlist(
            map(DATA_RANDOM, function(DATA) {
              Interim_A_to_B <- DATA[[2]]
              Interim_A_to_C <- DATA[[3]]
              #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
              Max_min_dist <- 
                map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
                  Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
                  Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
                  max(c(Min_A_to_B, Min_A_to_C))
                })
              Max_min_dist
            }, .progress = list(clear = F,
                                name = "Calculate Random CI - Stage 2",
                                show_after = 1,
                                type = "iterator"))
          )))
      }
      
      #Generate results tibble
      RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
      names(RESULTS_tibble)[2] <- "Min_dist_to_TRIO"
      
      RESULTS_tibble$SE <- RESULTS_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Min_dist_to_TRIO - 1.96*SE,
                                                  Result_95CI = Min_dist_to_TRIO + 1.96*SE)
      
      RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
      RESULTS_tibble$N_Target_1 <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
      RESULTS_tibble$N_Target_2 <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[3]][1, -1])))
      
      RESULTS_tibble$Random <- RESULTS_RANDOM
      RESULTS_tibble$Random_SE <- RESULTS_RANDOM_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Random_05CI = Random - 1.96*Random_SE,
                                                  Random_95CI = Random + 1.96*Random_SE) %>%
        mutate(Outside_CI = case_when(Min_dist_to_TRIO > Random_95CI | Min_dist_to_TRIO < Random_05CI ~ "Significant",
                                      TRUE ~ "Not Significant"),
               Sign = case_when(Min_dist_to_TRIO > Random ~ "Above expected",
                                TRUE ~ "Below expected"))
      RESULTS_tibble$N_Random_COO <- map_dbl(DATA_RANDOM, function(DATA_RANDOM) nrow(DATA_RANDOM[[2]]))
      
      #Establish a lower limit of 0 for CI
      RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
      RESULTS_tibble$Random_05CI[RESULTS_tibble$Random_05CI < 0] <- 0
      
      #Plot the results
      plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Min_dist_to_TRIO))) +
             geom_col(aes(y = Min_dist_to_TRIO), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
             geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
             geom_errorbar(aes(ymin = Random_05CI, ymax = Random_95CI), color = "red", linewidth = 0.9, width = 0.3)+
             cowplot::theme_cowplot() +
             scale_x_discrete("") +
             scale_y_continuous("Min Distance to TRIO") +
             theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
      )
      
      future::plan("future::sequential")
      gc()
      #Return the final tibble
      return(RESULTS_tibble)
    }
    
    else if(!Include_Random){
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      
      #Generate results (average min distance to TRIO)
      RESULTS <- furrr::future_map_dbl(DATA, function(DATA) {
        #Obtain the 2 individual distance matrices
        Interim_A_to_B <- DATA[[2]]
        Interim_A_to_C <- DATA[[3]]
        #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
        Max_min_dist <- 
          map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
            Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
            Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
            max(c(Min_A_to_B, Min_A_to_C))
          })
        #Calculate the average min distance to TRIO
        mean(Max_min_dist)
      }, .progress = TRUE)
      
      #Generate the SE of the mean
      RESULTS_SE <- furrr::future_map_dbl(DATA, function(DATA) {
        Interim_A_to_B <- DATA[[2]]
        Interim_A_to_C <- DATA[[3]]
        #Calculate the min distance to TRIO by calculating first the min distance from COO to each Target and then calculating the max of both distances
        Max_min_dist <- 
          map_dbl(1:nrow(Interim_A_to_B), function(Cell_A){
            Min_A_to_B <- min(Interim_A_to_B[Cell_A, 2:ncol(Interim_A_to_B)])
            Min_A_to_C <- min(Interim_A_to_C[Cell_A, 2:ncol(Interim_A_to_C)])
            max(c(Min_A_to_B, Min_A_to_C))
          })
        #Calculate the SE
        sd(Max_min_dist) / sqrt(length(Max_min_dist))
      }, .progress = TRUE)
      
      #Generate results tibble
      RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), value = RESULTS)
      names(RESULTS_tibble)[2] <- "Min_dist_to_TRIO"
      
      RESULTS_tibble$SE <- RESULTS_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Min_dist_to_TRIO - 1.96*SE,
                                                  Result_95CI = Min_dist_to_TRIO + 1.96*SE)
      
      RESULTS_tibble$N_COO <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]]$Cell_Of_Origin_no)))
      RESULTS_tibble$N_Target_1 <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[2]][1, -1])))
      RESULTS_tibble$N_Target_2 <- map_dbl(DATA, function(DATA) sum(!is.na(DATA[[3]][1, -1])))
      
      #Establish a lower limit of 0 for CI
      RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
      
      #Plot the results
      plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Min_dist_to_TRIO))) +
             geom_col(aes(y = Min_dist_to_TRIO), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
             geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
             cowplot::theme_cowplot() +
             scale_x_discrete("") +
             scale_y_continuous("Average Min Distance to TRIO") +
             theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
      )
      
      future::plan("future::sequential")
      gc()
      #Return the final tibble
      return(RESULTS_tibble)
    }
  },
  options = list(optimize = 3))

Trio_Cells_in_Radius_analyzer <- cmpfun(
  function(DATA = NULL,
           DATA_RANDOM = NULL,
           Radius = NULL,
           Include_Random = NULL,
           By_Sample_Random = NULL
           ) {
    
    #Check arguments
    if(!(as.character(Radius) %in% names(DATA[[1]][[4]]))){
      stop(paste0("Radius should be one of: ", str_c(names(DATA[[1]][[4]])[-1], collapse = ", ")))
    }
    if(!is.logical(Include_Random)) stop("Include_Random should be a logical value")
    
    print("Proceeding with analysis")
    #If everything is OK proceed with analysis
    if(Include_Random) {
      #Check arguments
      if(!is.logical(By_Sample_Random)) stop("By_Sample_Random should be a logical value")
      if(!length(intersect(names(DATA), names(DATA_RANDOM))) == length(unique(c(names(DATA), names(DATA_RANDOM))))){
        outersect <- function(x, y) {
          sort(c(x[!x%in%y],
                 y[!y%in%x]))
        }
        Removed_cases <- outersect(names(DATA), names(DATA_RANDOM))
        
        message(paste0("Only samples present in DATA and DATA_RANDOM will be used. The following samples will be removed: ",
                       str_c(Removed_cases, collapse = ", ")))
        DATA <- DATA[intersect(names(DATA), names(DATA_RANDOM))]
        DATA_RANDOM <- DATA_RANDOM[intersect(names(DATA), names(DATA_RANDOM))]
      }
      
      print("Proceeding with analysis")
      #Generate Results
      RESULTS <- map_dbl(DATA, function(x) {
        Interim <- x[[4]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        mean(Interim[[1]])
      })
      
      RESULTS_SE <- map_dbl(DATA, function(x) {
        Interim <- x[[4]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        sd(Interim[[1]]) / sqrt(length(Interim[[1]]))
      })
      
      #Generate Sample-wise random distributions
      if(By_Sample_Random){
        #Generate Random results (Average TRIO score)
        RESULTS_RANDOM <- map_dbl(DATA_RANDOM, function(x) {
          Interim <- x[[4]]
          Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
          mean(Interim[[1]])
        }) 
        #Generate the standard error
        RESULTS_RANDOM_SE <- map_dbl(DATA_RANDOM, function(x) {
          Interim <- x[[4]]
          Interim <- Interim %>% dplyr::select(as.character(Radius))
          sd(Interim[[1]]) / sqrt(length(Interim[[1]]))
        })
      }
      
      #Generate Experiment-wise random distribution
      if(!By_Sample_Random){
        RESULTS_RANDOM <-  mean(unlist(
          map(DATA_RANDOM, function(x) {
            Interim <- x[[4]]
            Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
            Interim
          }) 
        ))
        RESULTS_RANDOM_SE <- sd(unlist(
          map(DATA_RANDOM, function(x) {
            Interim <- x[[4]]
            Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
            Interim
          })
        )) / sqrt(length(unlist(
          map(DATA_RANDOM, function(x) {
            Interim <- x[[4]]
            Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
            Interim
          })
        )))
      }
      
      #Generate the number of cells
      Cell_A <- map_dbl(DATA, function(x) x[[1]][[1,2]])
      Cell_B <- map_dbl(DATA, function(x) x[[1]][[2,2]])
      Cell_C <- map_dbl(DATA, function(x) x[[1]][[3,2]])
      Name_A <- unique(map_chr(DATA, function(x) x[[1]][[1,1]]))
      Name_B <- unique(map_chr(DATA, function(x) x[[1]][[2,1]]))
      Name_C <- unique(map_chr(DATA, function(x) x[[1]][[3,1]]))
      
      print("Generating output")
      #Generate results tibble
      RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), Average_TRIO_score = RESULTS)
      RESULTS_tibble$SE <- RESULTS_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Average_TRIO_score - 1.96*SE,
                                                  Result_95CI = Average_TRIO_score + 1.96*SE)
      RESULTS_tibble$Radius <- Radius
      RESULTS_tibble$Cell_A <- Cell_A
      names(RESULTS_tibble)[[7]] <- Name_A
      RESULTS_tibble$Cell_B <- Cell_B
      names(RESULTS_tibble)[[8]] <- Name_B
      RESULTS_tibble$Cell_C <- Cell_C
      names(RESULTS_tibble)[[9]] <- Name_C
      
      RESULTS_tibble$Random <- RESULTS_RANDOM
      RESULTS_tibble$Random_SE <- RESULTS_RANDOM_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Random_05CI = Random - 1.96*Random_SE,
                                                  Random_95CI = Random + 1.96*Random_SE) %>%
        mutate(Outside_CI = case_when(Average_TRIO_score > Random_95CI | Average_TRIO_score < Random_05CI ~ "Significant",
                                      TRUE ~ "Not Significant"),
               Sign = case_when(Average_TRIO_score > Random ~ "Above expected",
                                TRUE ~ "Below expected"))
      RESULTS_tibble$N_Random_COO <- map_dbl(DATA_RANDOM, function(DATA_RANDOM) nrow(DATA_RANDOM[[2]]))
      #Establish a lower limit of 0 for CI
      RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
      RESULTS_tibble$Random_05CI[RESULTS_tibble$Random_05CI < 0] <- 0
      
      print("Generating plot")
      #Plot the results
      plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Average_TRIO_score))) +
             geom_col(aes(y = Average_TRIO_score), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
             geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
             geom_errorbar(aes(ymin = Random_05CI, ymax = Random_95CI), color = "red", linewidth = 0.9, width = 0.3)+
             cowplot::theme_cowplot() +
             scale_x_discrete("") +
             scale_y_continuous(str_c("Average TRIO score within a radius of ", as.character(Radius))) +
             theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
      )
      #Return the results
      return(RESULTS_tibble)
    }
    
    else if(!Include_Random){
      print("Proceeding with analysis")
      #Generate Results
      RESULTS <- map_dbl(DATA, function(x) {
        Interim <- x[[4]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        mean(Interim[[1]])
      })
      
      RESULTS_SE <- map_dbl(DATA, function(x) {
        Interim <- x[[4]]
        Interim <- Interim %>% dplyr::select(as.character(Radius))  #select the radius length (required to pick up choice from length list)
        sd(Interim[[1]]) / sqrt(length(Interim[[1]]))
      })
      
      #Generate the number of cells
      Cell_A <- map_dbl(DATA, function(x) x[[1]][[1,2]])
      Cell_B <- map_dbl(DATA, function(x) x[[1]][[2,2]])
      Cell_C <- map_dbl(DATA, function(x) x[[1]][[3,2]])
      Name_A <- unique(map_chr(DATA, function(x) x[[1]][[1,1]]))
      Name_B <- unique(map_chr(DATA, function(x) x[[1]][[2,1]]))
      Name_C <- unique(map_chr(DATA, function(x) x[[1]][[3,1]]))
      
      print("Generating output")
      #Generate results tibble
      RESULTS_tibble <- tibble(Subject_Names = names(RESULTS), Average_TRIO_score = RESULTS)
      RESULTS_tibble$SE <- RESULTS_SE
      RESULTS_tibble <- RESULTS_tibble %>% mutate(Result_05CI = Average_TRIO_score - 1.96*SE,
                                                  Result_95CI = Average_TRIO_score + 1.96*SE)
      RESULTS_tibble$Radius <- Radius
      RESULTS_tibble$Cell_A <- Cell_A
      names(RESULTS_tibble)[[7]] <- Name_A
      RESULTS_tibble$Cell_B <- Cell_B
      names(RESULTS_tibble)[[8]] <- Name_B
      RESULTS_tibble$Cell_C <- Cell_C
      names(RESULTS_tibble)[[9]] <- Name_C
      
      #Establish a lower limit of 0 for CI
      RESULTS_tibble$Result_05CI[RESULTS_tibble$Result_05CI < 0] <- 0
      
      print("Generating plot")
      #Plot the results
      plot(RESULTS_tibble %>% ggplot(aes(x = fct_reorder(Subject_Names, Average_TRIO_score))) +
             geom_col(aes(y = Average_TRIO_score), width = 0.5, color = "black", fill = "white", linewidth = 0.7)+
             geom_errorbar(aes(ymin = Result_05CI, ymax = Result_95CI), color = "black", linewidth = 0.9, width = 0.3)+
             cowplot::theme_cowplot() +
             scale_x_discrete("") +
             scale_y_continuous(str_c("Average TRIO score within a radius of ", as.character(Radius))) +
             theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
      )
      #Return the results
      return(RESULTS_tibble)
    }
  },
  options = list(optimize = 3))

Trio_graph_maker <- cmpfun(
  function(Image_name = NULL,
           DATA_Phenotypes = NULL,
           Strategy = NULL,
           TRIO_Distances = NULL,
           TRIO_Cumulative = NULL,
           Radius = NULL
  ){
    #Check arguments (Generated with ChatGPT)
    if(is.null(Image_name) || !is.character(Image_name)) {
      stop("Image_name must be specified as a non-null character string.")
    }
    # Check if Strategy is provided and matches one of the expected values
    if(is.null(Strategy) || !Strategy %in% c("Min_Distance", "TRIO_in_Radius")) {
      stop("Strategy must be either 'Min_Distance' or 'TRIO_in_Radius'.")
    }
    # Check if TRIO_Distances is provided when Strategy is 'Min_Distance'
    if(Strategy == "Min_Distance" && is.null(TRIO_Distances)) {
      stop("TRIO_Distances must be provided when Strategy is 'Min_Distance'.")
    }
    # Check if TRIO_Cumulative is provided when Strategy is 'TRIO_in_Radius'
    if(Strategy == "TRIO_in_Radius" && is.null(TRIO_Cumulative)) {
      stop("TRIO_Cumulative must be provided when Strategy is 'TRIO_in_Radius'.")
    }
    # Check if Radius is provided when Strategy is 'TRIO_in_Radius'
    if(Strategy == "TRIO_in_Radius" && is.null(Radius)) {
      stop("Radius must be provided as a non-null value when Strategy is 'TRIO_in_Radius'.")
    }
    
    #First Min Distance analysis
    if(Strategy == "Min_Distance") {
      #Check that Image_name is in the data
      if(!all(Image_name %in% unique(DATA_Phenotypes$Subject_Names),
              Image_name %in% names(TRIO_Distances))){
        stop(paste0("Image_name not present in data. It should be one of: ", str_c(names(TRIO_Distances), collapse = ", ")))
      }
      #Import general data frames
      DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_name)
      DATA_Distances <- TRIO_Distances[[Image_name]]
      
      #Define the COO and Target_Cell
      COO <- DATA_Distances[[1]][[1,1]]
      Target_Cell_1 <- DATA_Distances[[1]][[2,1]]
      Target_Cell_2 <- DATA_Distances[[1]][[3,1]]
      
      #First prepare 4 tibbles, a tibble with COO cells, two tibbles with one target cell each, and a tibble with other cells
      Other_tibble <- DATA_Phenotypes %>% dplyr::filter(!Phenotype %in% c(COO, Target_Cell_1, Target_Cell_2))
      Target_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype %in% c(Target_Cell_1, Target_Cell_2))
      COO_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == COO)
      
      #Calculate the min distance to TRIO
      For_Join <- bind_cols(DATA_Distances[[2]][1],
                            map_dbl(1:nrow(DATA_Distances[[2]]), function(Row){
                              #calculate the min to the first target cell and second target cell
                              Minimum_Target_Cell_1 <- min(DATA_Distances[[2]][Row,-1])
                              Minimum_Target_Cell_2 <- min(DATA_Distances[[3]][Row,-1])
                              
                              #Calculate the maximum of both
                              max(Minimum_Target_Cell_1, Minimum_Target_Cell_2)
                              
                            }))
      names(For_Join) <- c("Cell_no", "Min_Distance_to_Trio")
      
      #Join the min distance with the COO tibble
      COO_tibble <- left_join(COO_tibble, For_Join, by = "Cell_no") %>% dplyr::filter(!is.na(Min_Distance_to_Trio))
      
      #Build the plot
      plot(
        ggplot() +
          geom_point(aes(x = X, y = Y), color = "lightgrey", data = Other_tibble) +
          ggforce::geom_circle(aes(x0 = X, y0 = Y, r = Min_Distance_to_Trio, fill = Min_Distance_to_Trio), alpha = 0.3, color = NA, data = COO_tibble)+
          geom_point(aes(x = X, y = Y, color = Phenotype), size = 2.5, data = Target_tibble) +
          geom_point(aes(x = X, y = Y), size = 2.5, color = "black", data = COO_tibble) +
          cowplot::theme_cowplot() +
          scale_x_continuous("", labels = NULL)+
          scale_y_continuous("", labels = NULL)+
          scale_fill_viridis_c("Min dist to TRIO")+
          scale_color_discrete("")+
          guides(color = guide_legend(override.aes = list(size = 8)),
                 fill = guide_colorbar(theme = theme(legend.key.width = unit(10, "cm"))))+
          ggtitle(Image_name)+
          theme(panel.grid = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 20),
                legend.position = "bottom",
                plot.title = element_text(size = 25, hjust = 0.5, vjust = -3)) 
      )
    }
    
    #Then TRIO in radius analysis
    else if(Strategy == "TRIO_in_Radius"){
      #Check that Image_name is in the data
      if(!all(Image_name %in% unique(DATA_Phenotypes$Subject_Names),
              Image_name %in% names(TRIO_Cumulative))){
        stop(paste0("Image_name not present in data. It should be one of: ", str_c(names(TRIO_Cumulative), collapse = ", ")))
      }
      #Import general data frames
      DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_name)
      DATA_Cumulative_Interaction <- TRIO_Cumulative[[Image_name]]
      
      #Check that radius size is present in the data
      if(!(as.character(Radius) %in% names(DATA_Cumulative_Interaction[[2]]))){
        stop(paste0("Radius size should be one of: ", str_c(names(DATA_Cumulative_Interaction[[2]])[-1], collapse = ", ")))
      }
      
      #Define the COO and Target_Cell
      COO <- DATA_Cumulative_Interaction[[1]][[1,1]]
      Target_Cell_1 <- DATA_Cumulative_Interaction[[1]][[2,1]]
      Target_Cell_2 <- DATA_Cumulative_Interaction[[1]][[3,1]]
      
      #First prepare 3 tibbles, a tibble with COO cells, a tibble with target cells and a tibble with other cells
      Other_tibble <- DATA_Phenotypes %>% dplyr::filter(!Phenotype %in% c(COO, Target_Cell_1, Target_Cell_2))
      Target_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype %in% c(Target_Cell_1, Target_Cell_2))
      COO_tibble <- DATA_Phenotypes %>% dplyr::filter(Phenotype == COO)
      
      #Calculate the cells within radius
      For_Join <- DATA_Cumulative_Interaction[["Trio_Score"]] %>% dplyr::select(1, as.character(Radius))
      names(For_Join) <- c("Cell_no", "Trio_Score_in_Radius")
      
      
      #Join the max distance with the COO tibble
      COO_tibble <- left_join(COO_tibble, For_Join, by = "Cell_no") %>% dplyr::filter(!is.na(Trio_Score_in_Radius))
      
      #Build the plot
      plot(
        ggplot() +
          geom_point(aes(x = X, y = Y), color = "lightgrey", data = Other_tibble) +
          ggforce::geom_circle(aes(x0 = X, y0 = Y, r = Radius, fill = Trio_Score_in_Radius), alpha = 0.3, color = NA, data = COO_tibble)+
          geom_point(aes(x = X, y = Y, color = Phenotype), size = 2.5, data = Target_tibble) +
          geom_point(aes(x = X, y = Y), size = 2, color = "black", data = COO_tibble) +
          cowplot::theme_cowplot() +
          scale_x_continuous("", labels = NULL)+
          scale_y_continuous("", labels = NULL)+
          scale_color_discrete("")+
          scale_fill_viridis_c(str_c("TRIO score ", as.character(Radius), " radius"))+
          guides(color = guide_legend(override.aes = list(size = 8)),
                 fill = guide_colorbar(theme = theme(legend.key.width = unit(10, "cm"))))+
          ggtitle(Image_name)+
          theme(panel.grid = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 20),
                legend.position = "bottom",
                plot.title = element_text(size = 25, hjust = 0.5, vjust = -3)) 
      )
    }
  }, 
  options = list(optimize = 3))

SPIAT_entropy_gradient_generator <- cmpfun(
  function(DATA_SPIAT = NULL, #Provide the list of SPIAT objects
           Gradient_start = NULL, #Provide the approximate size of the square tiles to be used
           Gradient_stop = NULL, #Specify the cell phenotypes to be included in the analysis
           Gradient_sampling = NULL, #Specify the heterogeneity threshold to use in the analysis
           Phenotypes_included = NULL
           ) {
    
    #Check arguments by generating a argument check vector and message vector
    Argument_checker <- c(DATA_SPIAT_OK = all(map_lgl(DATA_SPIAT, function(Image) class(Image) == "SpatialExperiment")),
                          Gradient_start_OK = all(is.numeric(Gradient_start), Gradient_start >= 0, Gradient_start < Gradient_stop),
                          Gradient_stop = all(is.numeric(Gradient_stop), Gradient_stop >= 0),
                          Gradient_sampling = all(is.numeric(Gradient_sampling), (Gradient_stop - Gradient_start) >= 0) ,
                          Phenotypes_included_OK = all(Phenotypes_included %in% unique(unlist(map(DATA_SPIAT, function(x) x@colData@listData$Phenotype))))
    )
    Stop_messages <- c(DATA_SPIAT_OK = "DATA_SPIAT must be generated with the SPIAT_object_generator function",
                       Gradient_start_OK = "Gradient_start must be a positive value smaller than Gradient_stop",
                       Gradient_stop = "Gradient_stop must be a positive value greater than Gradient_start",
                       Gradient_sampling = "Gradient sampling must be a positive value smaller than the differentce between Gradient_stop and Gradient_start",
                       Phenotypes_included_OK = str_c("Phenotypes_included must be any of the following: ", 
                                                      str_c(unique(unlist(map(DATA_SPIAT, function(x) x@colData@listData$Phenotype))), collapse = ", "),
                                                      collapse = "")
    )
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    
    #Get SPIAT DATA
    DATA_SPIAT <- DATA_SPIAT
    
    #Select the gradients for entropy computation
    gradient_pos <- seq(from = Gradient_start, to = Gradient_stop, by = Gradient_sampling)
    names(gradient_pos) <- str_c("Radius_", gradient_pos, sep = "")
    #Calculate the results
    RESULTS <- 
      map_dfc(gradient_pos, function(pos) {
        map_dbl(DATA_SPIAT, function(Image){
          Result <- try(mean(SPIAT::calculate_entropy(Image, cell_types_of_interest = Phenotypes_included,
                                                      radius = pos)[[13]]),#Select entropy cells of origin (first) and target (second)
                        silent = T)
          if(berryFunctions::is.error(Result)){
            return(NA)
          }
          else{
            return(Result)
          }
        })
      }, .progress = list(clear = F,
                          name = "Calculating entropy gradient",
                          show_after = 1,
                          type = "iterator"))
    
    RESULTS$Subject_Names <- names(DATA_SPIAT)
    RESULTS <- RESULTS[c(ncol(RESULTS), 1:(ncol(RESULTS)-1))]
    
    plot(RESULTS %>% pivot_longer(-1) %>%
           mutate(name = factor(name, levels = str_c("Radius_", gradient_pos))) %>%
           ggplot(aes(x = name, y = value, color = Subject_Names, group = Subject_Names)) + geom_line() +
           cowplot::theme_cowplot() +
           scale_x_discrete("Radius", labels = as.character(gradient_pos)) +
           scale_y_continuous("Entropy")
    )
    return(RESULTS)
  },
  options = list(optimize = 3))

Cell_to_pixel_distance_calculator <- 
  cmpfun(
    function(N_cores = NULL,
             Directory = NULL,
             Image_rotate = NULL,
             Image_x_flip = NULL,
             Image_y_flip = NULL,
             DATA = NULL,
             Phenotypes_included = NULL,
             Pixel_distance_ratio = NULL){
      
      #Specify that on exit rerturn to single core and run gc
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      #Argument check general arguments
      Argument_checker <- c(N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0),
                            Empty_directory_OK = length(dir(Directory)) >= 1,
                            Image_rotate_OK = if(!is.null(Image_rotate)) {
                              all(is.numeric(Image_rotate), Image_rotate >= 0, Image_rotate <= 360, Image_rotate%%1 == 0)
                            } else(TRUE),
                            Image_x_flip_OK = is.logical(Image_x_flip),
                            Image_y_flip_OK = is.logical(Image_y_flip),
                            DATA_OK = all(identical(names(DATA)[c(1:4)], c("Cell_no", "X", "Y", "Subject_Names")), "Phenotype" %in% names(DATA)),
                            Phenotypes_included_OK = all(Phenotypes_included %in% DATA[["Phenotype"]]),
                            Pixel_distance_ratio_OK = if(!is.null(Pixel_distance_ratio)) {
                              all(is.numeric(Pixel_distance_ratio), Pixel_distance_ratio > 0)
                            } else(TRUE)
      )
      
      Stop_messages <- c(N_cores_OK = "N_cores must be an integer value > 0",
                         Empty_directory_OK = "No files found at the directory provided. Please check out the path.",
                         Image_rotate_OK = "Image_rotate must be either NULL or a integer value between 0 and 360",
                         Image_x_flip_OK = "Image_x_flip must be a logical value",
                         Image_y_flip_OK = "Image_y_flip must be a logical value",
                         DATA_OK = "DATA must be adequately formatted and must contain a column containing Phenotype information",
                         Phenotypes_included_OK = paste0("Phenotypes_included must be any of the following: ", str_c(DATA[["Phenotype"]], collapse = ", ")),
                         Pixel_distance_ratio_OK = "Pixel_distance_ratio must be either NULL or a numeric value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
      
      #Check specifically that directory contains adequate files, and that these are present in Subject_Names of data
      Full_image_names <- dir(Directory, full.names = TRUE)
      Image_names <- dir(Directory, full.names = FALSE)
      
      #Select only the thresholded and not the tissue masks
      Full_image_names_selected <- Full_image_names[str_detect(Image_names, "Thresholded")]
      Image_names_selected <- Image_names[str_detect(Image_names, "Thresholded")]
      Image_names_selected_list <- str_split(Image_names_selected, "_")
      
      
      #Check that all have been processed using the Pixel_Threshold_calculator
      if(!all(map_lgl(Image_names_selected_list, ~.[[1]] == "Processed"))){
        Problematic_images <- Image_names_selected[!map_lgl(Image_names_selected_list, ~.[[1]] == "Processed")]
        stop(paste0("According to image names, the following images have not been processed using the Pixel_Threshold_calculator: ", str_c(Problematic_images, collapse = ", ")))
      }
      if(!all(map_lgl(Image_names_selected_list, ~length(.) == 4))){
        Problematic_images <- Image_names_selected[!map_lgl(Image_names_selected_list, ~length(.) == 4)]
        stop(paste0("According to image names, the following images have not been processed using the Pixel_Threshold_calculator: ", 
                    str_c(Problematic_images, collapse = ", ")))
      }
      #Check that all images in the directory are unique
      if(length(unique(map_chr(Image_names_selected_list, ~.[[2]]))) != length(Image_names_selected_list)){
        Problematic_images <- map_chr(Image_names_selected_list, ~.[[2]])[duplicated(map_chr(Image_names_selected_list, ~.[[2]]))]
        stop(paste0("The following image appear to be duplicated in the directory: ", 
                    str_c(Problematic_images, collapse = ", "),
                    ". Images must be unique."))
      }
      #Check that all images in the directory are from the same target
      if(length(unique(map_chr(Image_names_selected_list, ~.[[3]]))) != 1){
        Number_markers <- sort(table(map_chr(Image_names_selected_list, ~.[[3]])), decreasing = TRUE)
        stop(paste0("Besides ", names(Number_markers)[1], " the directory contains images using the following markers: ", 
                    str_c(names(Number_markers)[-1], collapse = ", "),
                    ". A single marker type must be included in the directory"))
      }
      #Check that all images in the directory have been thresholded homogenously
      if(length(unique(map_chr(Image_names_selected_list, ~.[[4]]))) != 1){
        Number_threshold_strategies <- sort(table(map_chr(Image_names_selected_list, ~.[[4]])), decreasing = TRUE)
        stop(paste0("Besides ", names(Number_threshold_strategies)[1], " the directory contains images thresholded using the following strategy: ", 
                    str_c(names(Number_markers)[-1], collapse = ", "),
                    ". A single threshold strategy must be selected."))
      }
      #Check that image names are present in DATA Subject_Names calculating the closest
      Subject_names_in_images <- map_chr(Image_names_selected_list, ~.[[2]])
      Subject_names_in_data <- unique(DATA$Subject_Names)
      
      #Generate a look up tibble with the image name, the closest name in data$Subject_Names and the image path
      #Evaluate which subject name in data is the closest one to the subject name in images
      Closest_name_vector <- map_dbl(Subject_names_in_images, function(Image_name){
        Distance_vector <- adist(Image_name, Subject_names_in_data, fixed = TRUE, ignore.case = TRUE)
        which.min(Distance_vector)
      })
      Names_tibble <- tibble(Images_names = Subject_names_in_images,
                             Subject_names_in_data = Subject_names_in_data[Closest_name_vector])
      Names_tibble$Identical <- apply(Names_tibble, MARGIN = 1, function(Row) identical(Row[[1]], Row[[2]]))
      
      #If Subject_Names in data are duplicated then stop the computing
      if(sum(duplicated(Names_tibble$Subject_names_in_data)) > 0){
        Duplicated_names_list <- 
          map(Names_tibble$Subject_names_in_data[duplicated(Names_tibble$Subject_names_in_data)],
              function(Duplicated_names) unname(unlist(Names_tibble %>% dplyr::filter(Subject_names_in_data == Duplicated_names) %>% dplyr::select(Images_names))))
        names(Duplicated_names_list) <- str_c(Names_tibble$Subject_names_in_data[duplicated(Names_tibble$Subject_names_in_data)], " in DATA_matched by: ")
        
        print(Duplicated_names_list)
        stop("Multiple image names are matched with same Subject_Names in data. Please check image names in directory.")
      }
      
      #If match is not exact print a message
      if(sum(!Names_tibble$Identical) > 0){
        message("Subject names in the following images do not exactly. Approximate match will be used")
        print(Names_tibble %>% dplyr::filter(!Identical))
      }
      
      #Remove Subject_Names in data not present in Names_tibble
      if(sum(!unique(DATA$Subject_Names) %in% Names_tibble$Subject_names_in_data) > 0){
        Absent_Subject_Names <- unique(DATA$Subject_Names)[!unique(DATA$Subject_Names) %in% Names_tibble$Subject_names_in_data]
        message("The following Subject_Names in DATA are not present in image names: ", 
                str_c(Absent_Subject_Names, collapse = ", "),
                ". They will be removed before analysis")
        DATA <- DATA %>% dplyr::filter(Subject_Names %in% Names_tibble$Subject_names_in_data)
      }
      
      #Generate the final Names_tibble including image path
      Names_tibble$Image_URL <- Full_image_names_selected
      
      #Also note the target being measured (used to name distance column in final result)
      Target_bein_measured <- Image_names_selected_list[[1]][[3]]
      
      #Run a random test with the image with the lowest cell counts
      Smallest_sample <- (DATA %>% dplyr::count(Subject_Names) %>% arrange(n))[[1,1]] 
      DATA_smallest <- DATA %>% dplyr::filter(Subject_Names == Smallest_sample)
      Image_path <- Names_tibble[[which(Names_tibble$Subject_names_in_data == Smallest_sample), 4]]
      Image <- magick::image_read(as.character(Image_path))
      if(!is.null(Image_rotate)) Image <- Image %>% magick::image_rotate(degrees = Image_rotate)
      Image <- Image %>% magick::as_EBImage()
      Test_image_tibble <- as_tibble(expand.grid(1:dim(Image)[[1]], 1:dim(Image)[[2]]))
      names(Test_image_tibble) <- c("Y", "X")
      Test_image_tibble <- Test_image_tibble[c("X", "Y")]
      Test_image_tibble$Value <- as.vector(Image)
      if(Image_x_flip) Test_image_tibble$X <- rev(Test_image_tibble$X)
      if(Image_y_flip) Test_image_tibble$Y <- rev(Test_image_tibble$Y)
      rm(Image)
      gc()
      Test_image_tibble <- Test_image_tibble %>% dplyr::filter(Value != 0)#Remove zero-values
      if(!is.null(Pixel_distance_ratio)) Test_image_tibble <- Test_image_tibble %>% mutate(X = X*Pixel_distance_ratio, Y = Y*Pixel_distance_ratio)#Apply pixel distance ratio if required
      
      #plot both results
      print(paste0("Generating a sample overlay image using ", Smallest_sample))
      plot(
        ggplot() +
          geom_tile(aes(x = X, y = Y, color = Value), data = Test_image_tibble)+
          geom_point(aes(x = X, y = Y), color = "red", data = DATA_smallest) +
          theme_minimal() +
          guides(color = "none") +
          scale_x_continuous("", labels = NULL) +
          scale_y_continuous("", labels = NULL) +
          theme(panel.grid = element_blank(),
                panel.background = element_rect(fill = "black"))
      )
      
      #Generate a menu to proceed with compuation
      answer <- menu(c("Proceed", "Abort"), title = "Check parameters provided and sample image generated. Should the analysis proceed?")
      #If user decides to stop then abort function and return stop message
      if(answer == 2) stop("The function has been stopped. Please tune parameters and try again")
      
      #If OK then run the final analysis
      print("Running distance to positive pixel computation")
      
      #Will iterate for every image in the names tibble
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      
      RESULTS <- suppressMessages(
        furrr::future_map_dfr(seq_along(1:nrow(Names_tibble)), function(Index_image){
          #Import DATA
          DATA_image <- DATA %>% dplyr::filter(Subject_Names == Names_tibble[["Subject_names_in_data"]][[Index_image]])
          
          #Remove Phenotypes not included
          DATA_image <- DATA_image %>% dplyr::filter(Phenotype %in% Phenotypes_included)
          
          #If no phenotypes present in image remove it
          if(nrow(DATA_image) == 0){
            message(paste0(Names_tibble$Subject_names_in_data[[Index_image]], " does not contain cells in Phenotypes_included. It will be removed."))
            return(DATA_image)
          } 
          
          #Import Image
          Image <- magick::image_read(Names_tibble$Image_URL[[Index_image]])
          #Apply the image transformations as required by user
          if(!is.null(Image_rotate)) Image <- Image %>% magick::image_rotate(degrees = Image_rotate)
          Image <- Image %>% magick::as_EBImage()
          Image_tibble <- as_tibble(expand.grid(1:dim(Image)[[1]], 1:dim(Image)[[2]]))
          names(Image_tibble) <- c("Y", "X")
          Image_tibble <- Image_tibble[c("X", "Y")]
          Image_tibble$Value <- as.vector(Image)
          if(Image_x_flip) Image_tibble$X <- rev(Image_tibble$X)
          if(Image_y_flip) Image_tibble$Y <- rev(Image_tibble$Y)
          Image_tibble <- Image_tibble %>% dplyr::filter(Value != 0)
          if(!is.null(Pixel_distance_ratio)) Image_mage_tibble <- Image_mage_tibble %>% mutate(X = X*Pixel_distance_ratio, Y = Y*Pixel_distance_ratio)
          
          #Remove image
          rm(Image)
          gc()
          
          #Cells will be origin, pixels will be targets. 
          COO_info <- cbind(DATA_image[["X"]], DATA_image[["Y"]])
          
          #First calculate min distance to closest neighbors for binary thresholded images
          if(length(unique(Image_tibble$Value)) == 1){
            #Calculate the targets
            Targets <- rtree::RTree(cbind(Image_tibble[["X"]], Image_tibble[["Y"]]))
            
            #Calculate the closest pixel
            Index <- rtree::knn(Targets, COO_info, 1L)
            Closest_pixel_tibble <- Image_tibble[unlist(Index), c(1,2)]
            names(Closest_pixel_tibble) <- c("Target_X", "Target_Y")
            
            Closest_pixel_tibble <- suppressMessages(bind_cols(COO_info, Closest_pixel_tibble))
            names(Closest_pixel_tibble)[c(1:2)] <- c("X", "Y")
            Closest_pixel_tibble <- Closest_pixel_tibble %>% dplyr::mutate(Dist_X = (X - Target_X)^2,
                                                                           Dist_Y = (Y - Target_Y)^2,
                                                                           DIST = sqrt(Dist_X + Dist_Y))
            DATA_image$DIST <- Closest_pixel_tibble$DIST
            names(DATA_image)[ncol(DATA_image)] <- str_c(Target_bein_measured, "_DIST", collapse = "_")
            return(DATA_image)
          }
          
          #Then calculate min distance for every value of multithresholded images
          if(length(unique(Image_tibble$Value)) > 1){
            #Obtain the unique values
            Unique_values <- unique(Image_tibble$Value)
            
            #Calculate a tibble containing distances to every unique value
            Closest_pixel_tibble <- 
              map_dfc(seq_along(1:length(Unique_values)), function(Indivudal_target){
                Image_tibble <- Image_tibble %>% dplyr::filter(Value == Unique_values[Indivudal_target])
                
                Targets <- rtree::RTree(cbind(Image_tibble[["X"]], Image_tibble[["Y"]]))
                
                #Calculate the closest pixel
                Index <- rtree::knn(Targets, COO_info, 1L)
                Closest_pixel_tibble <- Image_tibble[unlist(Index), c(1,2)]
                names(Closest_pixel_tibble) <- c("Target_X", "Target_Y")
                
                Closest_pixel_tibble <- suppressMessages(bind_cols(COO_info, Closest_pixel_tibble))
                names(Closest_pixel_tibble)[c(1:2)] <- c("X", "Y")
                Closest_pixel_tibble <- Closest_pixel_tibble %>% dplyr::mutate(Dist_X = (X - Target_X)^2,
                                                                               Dist_Y = (Y - Target_Y)^2,
                                                                               DIST = sqrt(Dist_X + Dist_Y))
                Closest_pixel_tibble <- Closest_pixel_tibble %>% dplyr::select(DIST)
                names(Closest_pixel_tibble) <- str_c(Target_bein_measured, "_DIST_", round(Unique_values[Indivudal_target], digits = 3), collapse = "")
                return(Closest_pixel_tibble)
              })
            DATA_image <- bind_cols(DATA_image, Closest_pixel_tibble)
            return(DATA_image)
          }
        }, .progress = TRUE)
      )
      
      #Return to single core
      future::plan("future::sequential")
      gc()
      
      return(RESULTS)
      
    },
    options = list(optimize = 3)
  )

############STEP 6 - NEIGHBORHOOD ANALYSIS - REQUIRED FUNCTIONS###########
message("Importing functions: STEP 6 - NEIGHBORHOOD ANALYSIS")

Tailored_Closest_neighbor_calculator <- 
  cmpfun(function(N_cores = NULL, #Number of cores to parallelize your computation
                  DATA = NULL,
                  Strategy = NULL,
                  N_neighbors = NULL,
                  Include_COO_in_neighborhood = NULL,
                  Max_dist_allowed = NULL,
                  Cell_Of_Origin = NULL,
                  Target_Cell = NULL
                  ){
    DATA <- DATA
    #Check arguments
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!identical(names(DATA)[c(1:4)], c("Cell_no", "X", "Y", "Subject_Names"))) stop("DATA must be formatted adequately")
    if(!"Phenotype" %in% names(DATA)) stop("DATA must contain a column named 'Phenotype")
    if(!all(c(Cell_Of_Origin, Target_Cell) %in% unique(DATA$Phenotype))) stop(
      paste0("Cell_Of_Origin and Target cell must be one of the following: ", str_c(unique(DATA$Phenotype), collapse = ", "))
    )
    if(!Strategy %in% c("Number", "Distance", "Both")) stop("Strategy must be any of the following: Number, Distance, Both")
    if(Strategy == "Number" || Strategy == "Both"){
      if(!all(N_neighbors%%1 == 0, N_neighbors >= 2)) stop("N_neighbors must be an integer value > 1")
    }
    if(Strategy == "Distance" || Strategy == "Both"){
      if(!all(is.numeric(Max_dist_allowed), Max_dist_allowed > 0)) stop("Max_dist_allowed must be a numeric value > 0")
    }
    if(!is.logical(Include_COO_in_neighborhood)) stop("Include_COO_in_neighborhood must be a logical value")
    
    #Import data to the function
    Function_DATA_Phenotypes <- DATA %>% dplyr::select(1:4, Phenotype)
    
    #Filter samples that dont have any cell of origio or any target cell
    Samples_to_keep <- map_lgl(unique(Function_DATA_Phenotypes$Subject_Names), function(Image){
      Interim <- Function_DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
      
      COO_present <- sum(Interim$Phenotype %in% Cell_Of_Origin) >= 1
      Target_Cell_present <- sum(Interim$Phenotype %in% Target_Cell) >= 1
      
      COO_present & Target_Cell_present
    })
    
    #If any invalid sample is present remove it from analysis and print results
    if(sum(!Samples_to_keep) > 0){
      warning(paste0("Samples without COO or without Target cells will be excluded. The following samples will be eliminated: ",
                     str_c(unique(Function_DATA_Phenotypes$Subject_Names)[!Samples_to_keep], collapse = ", ")))
      Function_DATA_Phenotypes <- Function_DATA_Phenotypes %>% dplyr::filter(Subject_Names %in% unique(Function_DATA_Phenotypes$Subject_Names)[Samples_to_keep])
    }
    
    #Compute the neighbors for each cell in each image
    RESULTS <- 
      map_dfr(unique(Function_DATA_Phenotypes$Subject_Names), function(Image) {
        #Select data from each unique image
        Interim <- Function_DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
        
        #Generate specific tibbles for cells of origin and Target cells
        Tibble_COO <- Interim %>% dplyr::filter(Phenotype %in% Cell_Of_Origin)
        Tibble_Targets <- Interim %>% dplyr::filter(Phenotype %in% Target_Cell)
        
        #Modify these tibbles according to the requirements of the function
        COO <- cbind(Tibble_COO[[2]], Tibble_COO[[3]])
        Targets <- rtree::RTree(cbind(Tibble_Targets[[2]], Tibble_Targets[[3]]))
        
        #Calculate the index of the closest neighbors in the Targets according to strategy
        if(Strategy == "Number" | Strategy == "Both"){
          if(Include_COO_in_neighborhood) Index <- rtree::knn(Targets, COO, as.integer(N_neighbors))
          if(!Include_COO_in_neighborhood) Index <- rtree::knn(Targets, COO, as.integer(N_neighbors+1))
        }
        if(Strategy == "Distance"){
          Index <- rtree::withinDistance(Targets, COO, Max_dist_allowed)
        }
        
        #save exit function if parallelization fails
        on.exit({
          future::plan("future::sequential")
          gc()
        })
        
        #Make the cluster
        future::plan("future::multisession", workers = N_cores) 
        options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
        furrr::furrr_options(scheduling = Inf)
        #Generate a DF that includes for each cell the N closest neighbors
        closest_neighbors_by_cell <-
          furrr::future_map_dfr(seq_along(1:length(Index)), function(row){
            cell_X <- Tibble_COO[[row,2]]
            cell_Y <- Tibble_COO[[row,3]]
            Cell_no_ID <- Tibble_COO[[row,1]]
            
            RESULT <- Tibble_Targets[Index[[row]],]
            #If COO is excluded, eliminate accordingly
            if(!Include_COO_in_neighborhood) RESULT <- RESULT %>% dplyr::filter(!Cell_no %in% Cell_no_ID)
            RESULT <- RESULT %>% mutate(DIST = sqrt((X - cell_X)^2 + (Y - cell_Y)^2)) %>% arrange(DIST)
            
            #If strategy includes distance and number filter out the neighbors that are not in a allowed distance range
            if(Strategy == "Both"){
              RESULT <- RESULT %>% dplyr::filter(DIST <= Max_dist_allowed)
            }
            #Count the neighbors (relative to all neighbors) and distance metrics
            Neighbors_in_window_result <- RESULT %>% dplyr::count(Phenotype) %>% pivot_wider(names_from = Phenotype, values_from = n)
            FINAL <- bind_cols(Tibble_COO[row,], Neighbors_in_window_result/nrow(RESULT))
            FINAL <- FINAL %>% mutate(N_neighbors = nrow(RESULT),
                                      min_DIST = min(RESULT$DIST), 
                                      max_DIST = max(RESULT$DIST), 
                                      avg_DIST = mean(RESULT$DIST),
                                      median_DIST = quantile(RESULT$DIST, 0.5))
            return(FINAL)
          },
          .progress = TRUE) 
        future::plan("future::sequential")
        gc()
        #Replace NA values by 0
        closest_neighbors_by_cell[is.na(closest_neighbors_by_cell)] <- 0
        
        #If any element of the Tibble target is missing in the neighbors then add 0 manually
        if(!all(unique(Tibble_Targets$Phenotype) %in% names(closest_neighbors_by_cell))){
          #Find missing targets
          names_var <- unique(Tibble_Targets$Phenotype)[!unique(Tibble_Targets$Phenotype) %in% names(closest_neighbors_by_cell)]
          #Generate a 0 tibble and change names
          Absent_tibble <- as_tibble(matrix(0, nrow = nrow(closest_neighbors_by_cell), ncol = length(names_var)))
          names(Absent_tibble) <- names_var
          
          closest_neighbors_by_cell <- bind_cols(closest_neighbors_by_cell, Absent_tibble)
        }
        return(closest_neighbors_by_cell)
      }, 
      .progress = list(clear = FALSE,
                       name = "Calculating closest neighbors",
                       show_after = 1,
                       type = "iterator"))
    
    #Change NA values to 0
    RESULTS[is.na(RESULTS)] <- 0
    
    #Modify column order
    RESULTS <- RESULTS[c("Cell_no","X","Y","Subject_Names","Phenotype",
                         names(RESULTS)[-which(names(RESULTS) %in% 
                                                 c("Cell_no","X","Y","Subject_Names","Phenotype",
                                                   "N_neighbors", "min_DIST","max_DIST","avg_DIST","median_DIST"))],
                         "N_neighbors", "min_DIST","max_DIST","avg_DIST","median_DIST")]
    
    #Plot correlation matrix and distances
    For_correlation <- RESULTS[names(RESULTS)[-which(names(RESULTS) %in% 
                                                       c("Cell_no","X","Y","Subject_Names","Phenotype",
                                                         "N_neighbors", "min_DIST","max_DIST","avg_DIST","median_DIST"))]]
    #We need to remove columns without variability
    Drop_vars <- map_lgl(For_correlation, ~length(unique(.)) == 1)
    if(sum(Drop_vars >= 1)) message(paste0("The following phenotypes appear at a constant rate in neighbors and will be dropped from the correlation analysis: ",
                                           str_c(names(Drop_vars)[Drop_vars], collapse = ", ")))
    #Select only variables that are keen for correlation
    For_correlation <- For_correlation[!Drop_vars]
    cor_DATA <- cor(For_correlation, method = "pearson")
    corrplot::corrplot(cor_DATA, method = "shade", type = "lower", order = "hclust", addCoef.col = "black", number.cex = 0.8, tl.cex = 0.8,
                       tl.pos = "lt", tl.col = "black")
    
    plot(RESULTS %>% dplyr::select(max_DIST, avg_DIST, median_DIST) %>% pivot_longer(1:3) %>%
           ggplot(aes(x = value)) + facet_wrap(~name, ncol = 1, nrow = 3, "free") + geom_histogram(binwidth = 2)+
           cowplot::theme_cowplot()+
           scale_x_continuous("Distance"))
    
    RESULTS_Proportion <- RESULTS
    RESULTS_Absolute <- RESULTS
    
    RESULTS_Absolute[c(names(RESULTS)[-which(names(RESULTS) %in% 
                                               c("Cell_no","X","Y","Subject_Names","Phenotype",
                                                 "N_neighbors", "min_DIST","max_DIST","avg_DIST","median_DIST"))])] <- 
      round(RESULTS_Absolute[c(names(RESULTS)[-which(names(RESULTS) %in% 
                                                       c("Cell_no","X","Y","Subject_Names","Phenotype",
                                                         "N_neighbors", "min_DIST","max_DIST","avg_DIST","median_DIST"))])]*RESULTS$N_neighbors,
            digits = 0)
    
    
    
    
    #Return the final data
    return(list(
      Percentage = RESULTS_Proportion,
      Absolute_count = RESULTS_Absolute
    ))
    
  },
  options = list(optimize = 3))        

Neighborhood_discovery_function <- cmpfun(
  function(DATA = NULL,
           
           Allowed_max_Dist = NULL,
           Allowed_avg_Dist = NULL,
           Allowed_median_Dist = NULL,
           
           #Dimension reduction
           Perform_Dimension_reduction = NULL,
           Dimension_reduction = NULL,
           Dimension_reduction_prop = NULL,
           Cluster_on_Reduced = NULL,
           
           #Strategy for clustering
           Strategy = NULL,
           
           #Parameters for Consensus Clustering
           Max_N_neighborhoods = NULL,
           Consensus_reps = NULL,
           Consensus_p_Items = NULL,
           Consensus_Cluster_Alg = NULL,
           Consensus_Distance = NULL,
           Consensus_Name = NULL,
           
           #Parameters for Self-Organizing Maps
           Max_SOM_neighborhoods = NULL, #Maximum number of clusters (neighborhoods) to try in the algorithm
           
           #Parameters for Graph methods
           Graph_type = NULL,
           Graph_Method = NULL,
           Nearest_neighbors_for_graph = NULL,
           Graph_Resolution = NULL,
           Graph_Distance_method = NULL,
           N_steps = NULL,
           
           #Parameters for K means Meta Clustering
           N_K_centroids = NULL, #Number of centroids to perform K means
           Max_N_neighborhoods_Meta = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Consensus_reps_Meta = NULL, #Number of iterations of the algorithm to try to converge
           Consensus_p_Items_Meta = NULL, #Percentage of cells that you desire to sample in each iteration
           Consensus_Name_Meta = NULL, #Name of the folder that is going to be created in order to place the resulting graphs
           
           #Parameters for Batched K means
           Batch_size = NULL, #The number of cells to be included in each random batch
           Max_N_neighborhoods_Batch = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Percentage_centroid_initiation = NULL,
           N_initiations = NULL, #Number of times the algorithm is going to be tried to find the best clustering result
           Max_iterations = NULL, #Max number of iterations in each try
           
           #Parameters for Gaussian Mixture Model
           Quality_metric = NULL, #The quality measure used to test the number of clusters ("AIC" or "BIC")
           Max_N_neighborhoods_GMM = NULL, #Number of maximum clusters (phenotypes) that you desire to find
           Max_iterations_km = NULL, #Number of max iterations in the K means clustering performed
           Max_iterations_em = NULL, #Number of max iterations in the Expectation Maximization algorithm
           GMM_Distance = NULL, #Distance metric to use in the model ("eucl_dist" or "maha_dist")
           
           #Parameters for CLARA clustering
           Samples_CLARA = NULL, #Number of samples the CLARA algorithm is going to use to be calculated
           Sample_per_CLARA = NULL, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
           Max_N_neighborhoods_CLARA = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Distance_CLARA = NULL, #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
           #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
           N_cores = NULL #Number of cores to parallelize your computation
  ) {
    #Check arguments by generating a argument check vector and message vector
    Argument_checker <- c(DATA_OK = all(c("min_DIST", "max_DIST", "avg_DIST", "median_DIST") %in% names(DATA)),
                          Distance_OK = all(c(is.numeric(c(Allowed_max_Dist, Allowed_avg_Dist, Allowed_median_Dist)), 
                                              c(Allowed_max_Dist, Allowed_avg_Dist, Allowed_median_Dist) > 0)),
                          Strategy_OK = Strategy %in% c("Consensus_Clustering", "SOM", "Graph_Based", "K_Means_Meta_clustering", "Batch_K_means", "GMM", "CLARA_clustering")
    )
    Stop_messages <- c(DATA_OK = "DATA must be obtained from Closest_neighbor_calculator or Tailored_Closest_neighbor_calculator functions",
                       Distance_OK = "Allowed distances must be numeric and > 0",
                       Strategy_OK = "Strategy must be one of the following Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering")
    #Check arguments and stop if necessary
    if(!all(Argument_checker)){
      stop(cat(Stop_messages[!Argument_checker],
               fill = sum(!Argument_checker)))
    }
    if(!is.logical(Perform_Dimension_reduction)) stop("Perform_Dimension_reduction must be a logical value")
    if(Perform_Dimension_reduction){
      if(!Dimension_reduction %in% c("UMAP", "TSNE", "PCA")) stop("Dimension_reduction must be one of the following: UMAP, TSNE, PCA")
      if(!all(is.numeric(Dimension_reduction_prop), Dimension_reduction_prop > 0, Dimension_reduction_prop <= 1)) stop("Dimension_reduction_prop must be a numeric value between 0 and 1")
    }
    if(!is.logical(Cluster_on_Reduced)) stop("Cluster_on_Reduced must be a logical value")
    if(Cluster_on_Reduced){
      if(!Perform_Dimension_reduction) stop("If Clustering needst o be performed on Dimension reduced data please set Perform_Dimension_reduction to TRUE")
    }
    #Check arguments for Consensus Clustering
    if(Strategy == "Consensus_Clustering"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Max_N_neighborhoods_OK = (Max_N_neighborhoods >= 2 & Max_N_neighborhoods%%1 == 0),
                            Consensus_reps_OK = (Consensus_reps >= 1 & Consensus_reps%%1 == 0),
                            Consensus_p_Items_OK = (Consensus_p_Items > 0 & Consensus_p_Items <= 1),
                            Consensus_Cluster_Alg_OK = Consensus_Cluster_Alg %in% c("hc", "pam", "km"),
                            Consensus_Distance_OK = Consensus_Distance %in% c("pearson", "spearman", "euclidean", "binary", "maximum", "canberra", "minkowski"),
                            Consensus_Name_OK = is.character(as.character(Consensus_Name))
      )
      Stop_messages <- c(Max_N_neighborhoods_OK = "Max_N_neighborhoods must be an integer value > 1",
                         Consensus_reps_OK = "Consensus_reps_OK must be an integer value > 0",
                         Consensus_p_Items_OK = "Consensus_p_Items must be a numeric value > 0 and lower than 1",
                         Consensus_Cluster_Alg_OK = "Consensus_Cluster_Alg must be one of the following: hc, pam, km",
                         Consensus_Distance_OK = "Consensus_Distance must be one the following: pearson, spearman, euclidean, binary, maximum, canberra, minkowski",
                         Consensus_Name_OK = "Consensus_Name must ve a character value")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for Self Organizing maps
    if(Strategy == "SOM"){
      #Check arguments
      if(!(Max_SOM_neighborhoods > 1 & Max_SOM_neighborhoods%%1 == 0)) stop("Max_SOM_neighborhoods must be an integer value > 1")
    }
    #Check arguments for Graph-Based clustering
    if(Strategy == "Graph_Based"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Graph_type_OK = Graph_type %in% c("Complete", "SNN"),
                            Graph_Distance_method_OK = (Graph_Distance_method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                            Graph_Method_OK = Graph_Method %in% c("Louvain", "Leiden", "Optimal", "Greedy", "WalkTrap", "Spinglass", "Leading_Eigen", "Edge_Betweenness"),
                            Graph_Resolution_OK = all(is.numeric(Graph_Resolution), Graph_Resolution > 0),
                            N_steps_OK = any(is.null(N_steps), (N_steps >=1 & N_steps%%1 == 0))
      )
      Stop_messages <- c(Graph_type_OK = "Graph_type should be one of the following: Complete, SNN",
                         Nearest_neighbors_for_graph = "Nearest_neighbors_for_graph must be an integer value > 0",
                         Graph_Method = "Graph_Method must be one of the following: Louvain, Leiden, Greedy, WalkTrap, Spinglass, Leading_Eigen, Edge_Betweenness",
                         Graph_Resolution = "Graph_Resolution must be a numeric value > 0",
                         N_steps = "N_steps must be a integer value > 0")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
      #Check specific argument of SNN graphs
      if(Graph_type == "SNN"){
        if(!all(Nearest_neighbors_for_graph%%1 == 0, Nearest_neighbors_for_graph > 0)) stop("Nearest_neighbors_for_graph should be a integer value > 0")
      }
    }
    #Check arguments for K means meta clustering
    if(Strategy == "K_Means_Meta_clustering"){
      #Check arguments
      Argument_checker <- c(Max_N_neighborhoods_Meta_OK = (Max_N_neighborhoods_Meta >= 2 & Max_N_neighborhoods_Meta%%1 == 0),
                            Consensus_reps_Meta_OK = (Consensus_reps_Meta >= 1 & Consensus_reps_Meta%%1 == 0),
                            Consensus_p_Items_Meta_OK = (Consensus_p_Items_Meta > 0 & Consensus_p_Items_Meta <= 1),
                            Consensus_Name_Meta_OK = is.character(as.character(Consensus_Name_Meta))
      )
      Stop_messages <- c(Max_N_neighborhoods_Meta_OK = "Max_N_neighborhoods_Meta must be an integer value > 1",
                         Consensus_reps_Meta_OK = "Consensus_reps_Meta must be an integer value > 0",
                         Consensus_p_Items_Meta_OK = "Consensus_p_Items_Meta must be a numeric value > 0 and lower than 1",
                         Consensus_Name_Meta_OK = "Consensus_Name_Meta must ve a character value"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for Batched K means
    if(Strategy == "Batch_K_means"){
      #Check arguments
      Argument_checker <- c(Max_N_neighborhoods_Batch_OK = (Max_N_neighborhoods_Batch >= 2 & Max_N_neighborhoods_Batch%%1 == 0),
                            N_initiations_OK = (N_initiations >= 1 & N_initiations%%1 == 0),
                            Max_iterations_OK = (Max_iterations%%1 == 0)
      )
      Stop_messages <- c(Max_N_phenotypes_Batch_OK = "Max_N_neighborhoods_Batch must be an integer value > 1",
                         N_initiations_OK = "N_initiations must be an integer value > 0",
                         Max_iterations_OK = "Max_iterations must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for Gaussian mixture models
    if(Strategy == "GMM"){
      #Check arguments
      Argument_checker <- c(Quality_metric_OK = Quality_metric %in% c("AIC", "BIC"),
                            Max_N_neighborhoods_GMM_OK = (Max_N_neighborhoods_GMM >= 2 & Max_N_neighborhoods_GMM%%1 == 0),
                            Max_iterations_km_OK = (Max_iterations_km >= 1 & Max_iterations_km%%1 == 0),
                            Max_iterations_em_OK = (Max_iterations_em >= 1 & Max_iterations_em%%1 == 0),
                            GMM_Distance_OK = GMM_Distance %in% c("eucl_dist", "maha_dist")
      )
      Stop_messages <- c(Quality_metric_OK = "Quality_metric must be one of the following: AIC, BIC",
                         Max_N_phenotypes_GMM_OK = "Max_N_phenotypes must be an integer value > 1",
                         Max_iterations_km_OK = "Max_iterations_km must be an integer value > 1",
                         Max_iterations_em_OK = "Max_iterations_em must be an integer value > 1",
                         GMM_Distance_OK = "GMM_Distance must be one of the following: eucl_dist, maha_dist"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for CLARA clustering
    if(Strategy == "CLARA_clustering"){
      #Check arguments
      Argument_checker <- c(Samples_CLARA_OK = (Samples_CLARA >= 1 & Samples_CLARA%%1 == 0),
                            Sample_per_CLARA_OK = (Sample_per_CLARA > 0 & Sample_per_CLARA <= 1),
                            Max_N_neighborhoods_CLARA_OK = (Max_N_neighborhoods_CLARA >= 2 & Max_N_neighborhoods_CLARA%%1 == 0),
                            Distance_CLARA_OK = Distance_CLARA %in% c("euclidean", "manhattan", "chebyshev", "canberra", "braycurtis", 
                                                                      "pearson_correlation", "simple_matching_coefficient", "minkowski", 
                                                                      "hamming", "jaccard_coefficient", "Rao_coefficient", "mahalanobis", "cosine"),
                            N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0)
      )
      Stop_messages <- c(Samples_CLARA_OK = "Samples_CLARA must be an integer value > 0",
                         Sample_per_CLARA_OK = "Sample_per_CLARA must be a numeric value between 0 and 1",
                         Max_N_neighborhoods_CLARA_OK = "Max_N_neighborhoods_CLARA must be an integer value > 1",
                         Distance_CLARA_OK = "Distance_CLARA must be one of the following: euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine",
                         N_cores_OK = "N_cores must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    
    #Start executing the code
    print("Preparing data for analysis")
    #Import closest neighbor data
    DATA_Neighbors <- DATA
    
    #Filter out cells not whitin a neighborhood
    DATA_Neighbors <- DATA_Neighbors %>% dplyr::filter(max_DIST <= Allowed_max_Dist, avg_DIST <= Allowed_avg_Dist, median_DIST <= Allowed_median_Dist)
    
    #We select only neighbor columns
    Neighbor_patterns <- DATA_Neighbors %>% dplyr::select(-c(1:5), -contains("N_neighbors"), -c((ncol(DATA_Neighbors)-4):(ncol(DATA_Neighbors))))
    Neighbor_patterns_scaled <- Neighbor_patterns %>% scale()
    
    #Perform dimension reduction if required
    if(Perform_Dimension_reduction){
      #First PCA 
      if(Dimension_reduction == "PCA"){
        if(Dimension_reduction_prop != 1) stop("PCA must be performed using Dimension_reduction_prop = 1")
        print("Generating PCA projections")
        #Scale and turn into matrix
        DATA_matrix <- Neighbor_patterns_scaled %>% as.matrix()
        Result_PCA <- svd::propack.svd(DATA_matrix, neig = 2)$u
        DATA_Reduction <- tibble(DIMENSION_1 = unlist(Result_PCA[,1]), DIMENSION_2 = unlist(Result_PCA[,2]))
      }
      
      #Second TSNE
      if(Dimension_reduction == "TSNE"){
        if(Dimension_reduction_prop == 1) {
          print("Generating TSNE projections")
          if(nrow(DATA) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          DATA_matrix <- Neighbor_patterns_scaled %>% as.matrix()
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = TRUE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Result_TSNE[,1]), DIMENSION_2 = unlist(Result_TSNE[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating TSNE projections")
          DATA_matrix <- Neighbor_patterns_scaled %>% as_tibble %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = FALSE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          Coords <- snifter::project(Result_TSNE, 
                                     new = Neighbor_patterns_scaled %>% as.matrix(), 
                                     old = DATA_matrix)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
      
      #Third UMAP
      if(Dimension_reduction == "UMAP"){
        if(Dimension_reduction_prop == 1) {
          print("Generating UMAP projections")
          if(nrow(DATA) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take some time")
          #scale and turn into matrix
          DATA_matrix <- Neighbor_patterns_scaled %>% as.matrix()
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Result_UMAP[,1]), DIMENSION_2 = unlist(Result_UMAP[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating UMAP projections")
          DATA_matrix <- Neighbor_patterns_scaled %>% as_tibble() %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take aome time")
          #scale and turn into matrix
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L, ret_model = TRUE)
          Coords <- uwot::umap_transform(X = Neighbor_patterns_scaled %>% as.matrix(), 
                                         model = Result_UMAP)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
    }
    
    #Generate a specific version of Markers with dimension reduction data for clustering
    if(Cluster_on_Reduced)  Neighbor_patterns_scaled <- DATA_Reduction
    
    
    #Define what to do if consensus clustering is required
    if(Strategy == "Consensus_Clustering"){
      #Perform consensus clustering
      Neighborhood_result <- try(ConsensusClusterPlus::ConsensusClusterPlus(t(as.matrix((Neighbor_patterns_scaled))), 
                                                                            maxK = Max_N_neighborhoods, 
                                                                            reps = Consensus_reps,
                                                                            pItem = Consensus_p_Items,
                                                                            pFeature = 1,
                                                                            title = Consensus_Name,
                                                                            clusterAlg = Consensus_Cluster_Alg,
                                                                            distance = Consensus_Distance,
                                                                            plot = "png",
                                                                            verbose = T)
      )
      
      if(berryFunctions::is.error(Neighborhood_result)) stop("Consensus Clustering Failed. Your data is probably too large for this method. Please try another strategy")
      
      #Make the user decide the number of neighborhoods according to results
      N_Neighbors <- menu(choices = as.character(1:Max_N_neighborhoods), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Neighborhoods"))
      Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = Neighborhood_result[[as.double(N_Neighbors)]][["consensusClass"]])
    }
    #Define what to do if SOM is required
    if(Strategy == "SOM"){
      print("Executing Self Organizing Map algorithm")
      #Transform data into a scaled matrix and perform Self Organizing Map
      SOM_results <- try(FlowSOM::FlowSOM(as.matrix(Neighbor_patterns_scaled),
                                          scale = F,
                                          colsToUse = 1:ncol(Neighbor_patterns_scaled),
                                          maxMeta = Max_SOM_neighborhoods, #To find optimal meta clusters
                                          silent = F,
                                          seed = 21)
      )
      #Test if SOM returned an error
      if(berryFunctions::is.error(SOM_results)) {
        stop("Data is too large for Self-Organizing Maps. Please try another strategy")
      }
      else{
        #Assign phenotypes to each cell
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = FlowSOM::GetMetaclusters(SOM_results))
      }
    }
    #Then define what to do if graph based clustering is required
    if(Strategy == "Graph_Based"){
      #Generate graphs according to user defined preferences
      if(Graph_type == "Complete"){
        print("Generating the complete graph")
        #Calculate distance matrix and then calculate the graph
        #We define the number and ID of edges of the graph
        Graph_tibble <- as_tibble(expand.grid.unique(1:nrow(Neighbor_patterns_scaled), 1:nrow(Neighbor_patterns_scaled)))
        names(Graph_tibble) <- c("from", "to")
        Graph_tibble <- Graph_tibble %>% mutate(ID = str_c(from, to, sep = "_"))
        
        #We determine the distance between nodes that will be the features of the edges
        DISTANCE_MATRIX <- as_tibble(as.matrix(dist(Neighbor_patterns_scaled, method = Graph_Distance_method)))
        DISTANCE_MATRIX <- DISTANCE_MATRIX %>% mutate(from = as.character(1:nrow(DISTANCE_MATRIX)))
        DISTANCE_MATRIX <- DISTANCE_MATRIX[c(ncol(DISTANCE_MATRIX), 2:(ncol(DISTANCE_MATRIX)-1))] %>% pivot_longer(-1, names_to = "to", values_to = "weight") %>%
          mutate(ID = str_c(from, to, sep = "_")) %>% dplyr::select(-from, -to)
        
        #We bind the edges to their features (distance) and we build the graph
        GRAPH_DF <- left_join(Graph_tibble, DISTANCE_MATRIX, by = "ID") %>% dplyr::select(-ID)
        GRAPH_DF <- GRAPH_DF %>% mutate(weight = 1/weight)
        Neighborhood_ID <- tibble(Name = as.character(1:nrow(Neighbor_patterns_scaled)))
        Neighborhood_pattern_graph <- igraph::graph_from_data_frame(GRAPH_DF, directed = F, vertices = Neighborhood_ID)
      }
      if(Graph_type == "SNN"){
        print("Generating the SNN graph")
        #Transform data into a nearest neighbor graph 
        Neighborhood_pattern_graph <- try(bluster::makeSNNGraph(as.matrix(Neighbor_patterns_scaled), 
                                                                k = Nearest_neighbors_for_graph)
        )
        
        #Test if Graph construction process returned an error
        if(berryFunctions::is.error(Neighborhood_pattern_graph)) {
          stop("Data is too large to build a graph. Please try another strategy")
        }
      }
      
      print("Performing graph-based clustering")
      #Cluster the graph with louvain or leiden clustering
      if(Graph_Method == "Louvain") {
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_louvain(Neighborhood_pattern_graph,
                                                                                                            weights = NULL,
                                                                                                            resolution = Graph_Resolution)$membership)
      }
      
      else if(Graph_Method == "Leiden") {
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_leiden(Neighborhood_pattern_graph,
                                                                                                           objective_function = "modularity",
                                                                                                           weights = NULL,
                                                                                                           resolution = Graph_Resolution,
                                                                                                           beta = 0.01,
                                                                                                           initial_membership = NULL,
                                                                                                           n_iterations = 100,
                                                                                                           vertex_weights = NULL)$membership)
      }
      
      else if(Graph_Method == "Optimal"){
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_optimal(Neighborhood_pattern_graph)$membership)
      }
      
      else if(Graph_Method == "Greedy"){
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_fast_greedy(Neighborhood_pattern_graph)$membership)
      }
      
      else if(Graph_Method == "WalkTrap"){
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_walktrap(Neighborhood_pattern_graph,
                                                                                                             steps = N_steps,
                                                                                                             membership = T)$membership)
      }
      
      else if (Graph_Method == "Spinglass") {
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_spinglass(Neighborhood_pattern_graph,
                                                                                                              weights = NULL,
                                                                                                              vertex = NULL,
                                                                                                              spins = 25,
                                                                                                              parupdate = FALSE,
                                                                                                              start.temp = 1,
                                                                                                              stop.temp = 0.01,
                                                                                                              cool.fact = 0.99,
                                                                                                              update.rule = c("config", "random", "simple"),
                                                                                                              gamma = 1,
                                                                                                              implementation = c("orig", "neg"),
                                                                                                              gamma.minus = 1)$membership)
      }
      
      else if(Graph_Method == "Leading_Eigen"){
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_leading_eigen(Neighborhood_pattern_graph)$membership)
      }
      
      else if(Graph_Method == "Edge_Betweenness"){
        Neighbor_patterns <- Neighbor_patterns %>% mutate(Neighborhood_assignment = igraph::cluster_edge_betweenness(Neighborhood_pattern_graph,
                                                                                                                     weights = NULL,
                                                                                                                     directed = FALSE,
                                                                                                                     edge.betweenness = FALSE,
                                                                                                                     merges = FALSE,
                                                                                                                     bridges = FALSE,
                                                                                                                     modularity = FALSE,
                                                                                                                     membership = TRUE)$membership)
      }
    }
    #Define what to do if K means meta clustering is required
    if(Strategy == "K_Means_Meta_clustering"){
      print("Performing initial K-means algorithm")
      if(N_K_centroids >= nrow(Neighbor_patterns_scaled)) stop(paste0("N_K_centroids should be smaller than: ", nrow(Neighbor_patterns_scaled)))
      
      #First we need to perform K means Clustering 
      cl <- try(kmeans(as.matrix(Neighbor_patterns_scaled), #Scale it and turn it into a matrix
                       centers = N_K_centroids, #Number of centroids to be calculated
                       iter.max = 50, 
                       nstart = 10))
      
      #Stop function if K means returned an error
      if(berryFunctions::is.error(cl)) {
        stop("Data is too large for K means clustering. Please try another strategy")
      }
      #Proceed if no error was returned
      else{
        #Assign this K means cluster to each observation
        DATA_filter_Markers <- Neighbor_patterns_scaled %>% mutate(K_means_Cl = cl$cluster)
        
        #Prepare data for Meta-Clustering
        #Create a tibble with the K means centroids and the format it for Consensus clustering
        K_medoids <- as_tibble(cl$centers) %>% mutate(K_means_Cl = 1:nrow(as_tibble(cl$centers)))
        tK_medoids <- K_medoids %>% dplyr::select(-K_means_Cl) %>% as.matrix %>% t
        
        print("Perorming Consensus Clustering")
        #Perform Consensus clustering with hierarchical clustering
        HC <- try(ConsensusClusterPlus::ConsensusClusterPlus(tK_medoids, 
                                                             maxK = Max_N_neighborhoods_Meta,   
                                                             reps = Consensus_reps_Meta, 
                                                             pItem = Consensus_p_Items_Meta,
                                                             pFeature = 1,
                                                             title = Consensus_Name_Meta,
                                                             distance = "euclidean",
                                                             clusterAlg = "pam",
                                                             plot = "png",
                                                             verbose = T))
        #Test if consensus clustering returned an error
        if(berryFunctions::is.error(HC)) {
          stop("Data is too large for Meta Clustering. Please try another strategy or select a smaller N_K_centroids value")
        }
        else {
          #Make the user decide the number of neighborhoods according to results
          N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_Meta), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Neighborhoods"))
          
          #Bind the final Neighborhood_assignment to the K medoids tibble
          K_medoids <- K_medoids %>% mutate(Neighborhood_assignment = HC[[as.double(N_Phenotypes)]][["consensusClass"]])
          K_medoids_for_join <- K_medoids %>% dplyr::select(K_means_Cl, Neighborhood_assignment)
          
          #Bind The DATA and the K_meoids to obtain the final matrix
          Neighbor_patterns <- left_join(DATA_filter_Markers, K_medoids_for_join, by = "K_means_Cl") %>% dplyr::select(-K_means_Cl)
          #Reorder the columns
          Neighbor_patterns <- Neighbor_patterns[c(1:(ncol(Neighbor_patterns)-1), ncol(Neighbor_patterns))]
        }
      } 
    }
    #Define what to do if Batch K means is required
    if(Strategy == "Batch_K_means"){
      #First we calculate a metric to decide the number of total phenotypes
      #Specify the params
      params_mbkm <- list(batch_size = Batch_size, 
                          init_fraction = 1, 
                          early_stop_iter = 10)
      print("Starting Cluster number stimation process")
      
      if(Batch_size >= nrow(Neighbor_patterns_scaled)) stop(paste0("Batch_size should be smaller than: ", nrow(Neighbor_patterns_scaled)))
      #Run the specified test using each of the number of clusters
      Optimal <- try(ClusterR::Optimal_Clusters_KMeans(Neighbor_patterns_scaled, 
                                                       max_clusters = Max_N_neighborhoods_Batch, 
                                                       num_init = N_initiations, 
                                                       max_iters = Max_iterations, 
                                                       initializer = "kmeans++",
                                                       criterion = "Adjusted_Rsquared",
                                                       plot_clusters = T, 
                                                       mini_batch_params = params_mbkm,
                                                       verbose = T)
      )
      
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_Batch), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Neighborhoods"))
        
        print("Performing Batched K means algorithm")
        #Calculate the desired number of clusters with batch k menas
        Batch_k_means <- ClusterR::MiniBatchKmeans(Neighbor_patterns_scaled,
                                                   clusters = as.double(N_Phenotypes), 
                                                   batch_size = Batch_size,
                                                   num_init = N_initiations,
                                                   max_iters = Max_iterations,
                                                   init_fraction = 1,
                                                   initializer = "kmeans++",
                                                   early_stop_iter = 10,
                                                   verbose = T,
                                                   tol = 1e-07, #The required improvement rate to continue with the iterations (the lower the more iterations will be required)
                                                   CENTROIDS = NULL,
                                                   seed = 21)
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = Batch_k_means, fuzzy = F, newdata = Neighbor_patterns_scaled) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Neighborhood_assignment"
        
        #Generate the data phenotypes tibble
        Neighbor_patterns <- bind_cols(Neighbor_patterns, pr_mb)
      }
    }
    #Define what to do if GMM is required
    if(Strategy == "GMM"){
      print("Starting Cluster number stimation process")
      #First we calculate a metric to decide the number of total phenotypes
      #Run the specified test using each of the number of clusters
      Optimal <- try(ClusterR::Optimal_Clusters_GMM(Neighbor_patterns_scaled, 
                                                    criterion = Quality_metric,
                                                    max_clusters = Max_N_neighborhoods_GMM, 
                                                    dist_mode = GMM_Distance,
                                                    seed_mode = "random_subset",
                                                    km_iter = Max_iterations_km,
                                                    em_iter = Max_iterations_em,
                                                    verbose = TRUE,
                                                    var_floor = 1e-10,
                                                    plot_data = TRUE)
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_GMM), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Phenotypes"))
        
        print("Calculating Gaussian Mixed Model")
        #Calculate the desired number of clusters with batch k menas
        GMM_model <- ClusterR::GMM(Neighbor_patterns_scaled,
                                   gaussian_comps = as.double(N_Phenotypes), 
                                   dist_mode = GMM_Distance,
                                   seed_mode = "random_subset",
                                   km_iter = Max_iterations_km,
                                   em_iter = Max_iterations_em,
                                   verbose = TRUE,
                                   var_floor = 1e-10,
                                   full_covariance_matrices = FALSE
        )
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = GMM_model, fuzzy = F, newdata = Neighbor_patterns_scaled) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Neighborhood_assignment"
        
        #Generate the data phenotypes tibble
        Neighbor_patterns <- bind_cols(Neighbor_patterns, pr_mb)
      }
    }
    #Define what to do if CLARA clustering is required
    if(Strategy == "CLARA_clustering"){
      print("Starting Cluster number stimation process")
      #First we calculate a metric to decide the number of total phenotypes
      Optimal <-  try(ClusterR::Optimal_Clusters_Medoids(Neighbor_patterns_scaled,
                                                         max_clusters = Max_N_neighborhoods_CLARA,
                                                         distance_metric = Distance_CLARA,
                                                         criterion = "silhouette" ,
                                                         clara_samples = Samples_CLARA,
                                                         clara_sample_size = Sample_per_CLARA,
                                                         swap_phase = F,
                                                         threads = N_cores,
                                                         verbose = T,
                                                         plot_clusters = T
      )
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Continue if everything OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_CLARA), 
                            title = paste0("Based on the plots generated and you previous choice, decide the appropiate number of final Neighborhoods"))
        
        print("Performing CLARA (Clustering Large Applications)")
        CLARA_Clustering <- ClusterR::Clara_Medoids(Neighbor_patterns_scaled,
                                                    clusters = as.double(N_Phenotypes), 
                                                    samples = Samples_CLARA,
                                                    sample_size = Sample_per_CLARA,
                                                    distance_metric = Distance_CLARA,
                                                    threads = N_cores,
                                                    swap_phase = F,
                                                    fuzzy = FALSE,
                                                    verbose = T,
                                                    seed = 21
        )
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = CLARA_Clustering, fuzzy = F, newdata = Neighbor_patterns_scaled) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Neighborhood_assignment"
        
        #Generate the data phenotypes tibble
        Neighbor_patterns <- bind_cols(Neighbor_patterns, pr_mb)
      }
    }
    
    print("Generating plots")
    
    
    if(Perform_Dimension_reduction){
      #plot dimension reduction according to the number of cells
      if(nrow(DATA_Reduction) <= 100000){
        try(plot(
          DATA_Reduction %>% mutate(Neighborhood_assignment = Neighbor_patterns[["Neighborhood_assignment"]]) %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = as.factor(Neighborhood_assignment))) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Neighborhood_assignment", values = unname(pals::polychrome(length(unique(Neighbor_patterns$Neighborhood_assignment)))))
        )
        )
      }
      if(nrow(DATA_Reduction) > 100000){
        message(">100K observations to generate plots. A random subset containing 10% of the dataset will be selected for Dimension reduction plots")
        try(plot(
          DATA_Reduction %>% mutate(Neighborhood_assignment = Neighbor_patterns[["Neighborhood_assignment"]]) %>% 
            sample_n(size = 100000) %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = as.factor(Neighborhood_assignment))) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Neighborhood_assignment", values = unname(pals::polychrome(length(unique(Neighbor_patterns$Neighborhood_assignment)))))
        )
        )
      }
    }
    
    #Visualize the neighbor composition data for each neighborhood
    plot(Neighbor_patterns %>% pivot_longer(cols = -Neighborhood_assignment) %>%
           ggplot(aes(x = as.factor(Neighborhood_assignment), y = value)) + 
           geom_violin(aes(color = name, fill = name), alpha=0.3, position=position_dodge(width=0.5)) +
           stat_summary(aes(color = name), 
                        fun = median, geom = "crossbar", width = 0.4, linetype = 1, linewidth = 0.5,
                        position = position_dodge(width = 0.5)) +
           cowplot::theme_cowplot()+
           scale_x_discrete("Neighborhood")+
           scale_y_continuous("Neighbors in tha hood")+
           scale_color_discrete("") +
           scale_fill_discrete(""))
    
    #Visualize the heatmap of mean by neighborhood
    Mean_tibble <- Neighbor_patterns %>% group_by(Neighborhood_assignment) %>% dplyr::summarize_all(mean) %>% ungroup() #Obtain mean tibble
    Mean_matrix <- as.matrix(Mean_tibble[-1] %>% scale()) #Scale it and transform it into a  mtrix
    row.names(Mean_matrix) <- Mean_tibble[[1]] 
    
    plot(ComplexHeatmap::Heatmap(Mean_matrix,
                                 name = "Scaled")
    )
    
    #Generate the final results
    Final_result <- bind_cols(DATA_Neighbors, Neighbor_patterns["Neighborhood_assignment"])
    
    #Print the number of observations by neighborhoods
    print(Final_result %>% dplyr::count(Neighborhood_assignment) %>% arrange(desc(n)))
    #Turn into a factor
    Final_result$Neighborhood_assignment <- as.factor(Final_result$Neighborhood_assignment)
    
    if(Perform_Dimension_reduction) return(list(DATA = Final_result,
                                                Dimension_reduction = DATA_Reduction)
    )
    else return(Final_result)
    
  },
  options = list(optimize = 3))  

UTAG_message_passing <- cmpfun(
  function(DATA = NULL,
           COO_to_visit = NULL,
           
           Neighbor_strategy = NULL,
           Message_strategy = NULL,
           
           
           N_neighbors = NULL,
           Max_dist_allowed = NULL,
           
           Weighting_Strategy = NULL,
           COO_weight = NULL,
           
           N_cores = NULL) {
    #What to do on exit
    on.exit(future::plan("sequential"))
    
    #Check arguments
    if(!exists(DATA, envir = .GlobalEnv)) stop("DATA must be the name of an existing object")
    #Import data
    DATA <- get(DATA, envir = .GlobalEnv)
    #Check data structure
    if(!identical(names(DATA)[c(1:4)], c("Cell_no", "X", "Y", "Subject_Names"))) stop("DATA must be formatted adequately")
    #Check other arguments
    if(!any(is.null(COO_to_visit), all(is.logical(COO_to_visit), length(COO_to_visit) == nrow(DATA)))) stop(str_c("COO_to_visit must be NULL or a logical vector of length ", nrow(DATA), collapse = ""))
    if(!Neighbor_strategy %in% c("Number", "Distance", "Both")) stop("Neighbor_strategy must be one of the following: Number, Distance, Both")
    if(!Message_strategy %in% c("Averaging", "Sum")) stop("Message_strategy must be one of the following: Averaging, Sum")
    if(Neighbor_strategy %in% c("Number", "Both")){
      if(!all(is.numeric(N_neighbors), N_neighbors%%1 == 0, N_neighbors > 0)) stop("N_neighbors must be a positive integer value > 0")
    }
    if(Neighbor_strategy %in% c("Distance", "Both")){
      if(!all(is.numeric(Max_dist_allowed), Max_dist_allowed > 0)) stop("Max_dist_allowed must be a numeric value > 0")
    }
    if(Message_strategy == "Averaging"){
      if(!(Weighting_Strategy %in% c("None", "Proximity", "Disregarded_minority", "Both"))) {
        stop("Weighting_Strategy must be one of None, Proximity, Disregarded_minority or Both")
      }
      if(!any(is.null(COO_weight), all(is.numeric(COO_weight), COO_weight > 0, COO_weight < 1))) stop("COO_weight must be NULL or a numeric value between 0 and 1")
    }
    if(Message_strategy == "Sum"){
      if(!(Weighting_Strategy %in% c("None", "Proximity"))) {
        stop("Weighting_Strategy must be one of None or Proximity for Sum strategy")
      }
      if(!is.null(COO_weight)) message("COO_weight is not supported for Sum strategy. Argument will be ignored")
    }
    if(!all(!is.null(N_cores), N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    
    
    #Generate a list of COO and Targets cells to be included in the analysis
    if(!is.null(COO_to_visit)){
      COO_info <- tibble(Subject_Names = DATA$Subject_Names,
                         COO_Included = COO_to_visit)
      COO_info <- map(unique(DATA$Subject_Names), function(Image){
        Vector <- COO_info %>% dplyr::filter(Subject_Names == Image) %>% select(COO_Included)
        Vector[[1]]
      })
      names(COO_info) <- unique(DATA$Subject_Names)
    }
    
    #Add 1 to N_neighbors
    N_neighbors <- N_neighbors + 1
    
    #Perform message passing
    RESULTS <- 
      map_dfr(unique(DATA$Subject_Names), function(Image){
        #First select cells from the image to be analyzed
        Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
        
        #Calculate some initial weights if Disregarded_minority needs to be used
        if(Weighting_Strategy %in% c("Disregarded_minority", "Both")){
          Image_mean_marker_intensity <- map_dbl(Interim[-c(1:4)], function(Marker) -log(mean(Marker), base = 10))
          Image_mean_marker_intensity_weight <- Image_mean_marker_intensity / min(Image_mean_marker_intensity)
        }
        
        #Calculate the targets
        Tibble_Targets <- cbind(Interim[[2]],Interim[[3]])
        Tibble_Targets <- rtree::RTree(as.matrix(Tibble_Targets))
        #Calculate the COO
        Tibble_COO <- cbind(Interim[[2]], Interim[[3]])
        #If user has selected COO_to visit, filter the Tibble_COO object
        if(!is.null(COO_to_visit)){
          COO_to_visit <- COO_info[[Image]]
          Tibble_COO <- Tibble_COO[COO_to_visit, ]
        }
        Tibble_COO <- as.matrix(Tibble_COO)
        
        #Calculate the closest neighbors according to the instructions given
        if(Neighbor_strategy == "Number" | Neighbor_strategy == "Both"){
          Index <- rtree::knn(Tibble_Targets, Tibble_COO, as.integer(N_neighbors))
        }
        if(Neighbor_strategy == "Distance"){
          Index <- rtree::withinDistance(Tibble_Targets, Tibble_COO, Max_dist_allowed)
        }
        
        #save exit function if parallelization fails
        on.exit({
          future::plan("future::sequential")
          gc()
        })
        #Make the cluster
        future::plan("future::multisession", workers = N_cores) 
        options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
        furrr::furrr_options(scheduling = Inf)
        #Calculate message passed cells
        Message_passed_cells <- 
          furrr::future_map_dfr(1:length(Index), function(row) { 
            #If no cells have been removed from the message passing then proceed as usual
            if(is.null(COO_to_visit)){
              #Extract Target cells coordinates
              cell_X <- Interim[[row,2]]
              cell_Y <- Interim[[row,3]]
            }
            
            #If cells have been removed from the COO ist then proceed in a special manner
            if(!is.null(COO_to_visit)){
              Interim_COO <- Interim[COO_to_visit, ]
              cell_X <- Interim_COO[[row,2]]
              cell_Y <- Interim_COO[[row,3]]
            }
            
            #Obtain all neighbors (including COO)
            Neighbors_in_window <- Interim[Index[[row]],]
            
            #Calculate distances
            Neighbors_in_window <- Neighbors_in_window %>% mutate(DIST = sqrt((X - cell_X)^2 + (Y - cell_Y)^2)) %>% arrange(DIST)
            
            #Remove neighbors accounting for distance if required
            if(Neighbor_strategy == "Both"){
              Neighbors_in_window <- Neighbors_in_window %>% dplyr::filter(DIST <= Max_dist_allowed)
            }
            
            #Split by Cell Of Origin and neighbors
            COO <- Neighbors_in_window[1,]
            Neighbors <- Neighbors_in_window[-1,]
            
            #If no neighbors are identified return the individual cell
            if(nrow(Neighbors) == 0){
              Final_cell <- COO %>% dplyr::select(-DIST) %>% mutate(mean_DIST = NA, max_DIST = NA, N_neighbors = 0) #Cells without neighbors are not influenced
              return(Final_cell)
            }
            
            #If neighbors are present then proceed with message passing with the user selected weighting strategy
            #AVERAGING
            if(Message_strategy == "Averaging"){
              if(Weighting_Strategy == "None"){
                #Assign a FIXED weight to all neighbors (INDEPENDENT OF DISTANCE)
                Neighbors <- Neighbors %>% mutate(Neighboring_weight = 1)
                Neighbors$Neighboring_weight[is.infinite(Neighbors$Neighboring_weight)] <- max(Neighbors$Neighboring_weight[!is.infinite(Neighbors$Neighboring_weight)])
                
                #Calculate weighted mean for each marker according to cell proximity
                Averaged_markers <- map_dbl(Neighbors[-c(1:4, (ncol(Neighbors)-1), ncol(Neighbors))], function(x) stats::weighted.mean(x, w = Neighbors$Neighboring_weight))
                
                #Perform message passing
                #If COO_weight is null then perform the classic algorithm (all cells are weighted equally)
                if(is.null(COO_weight)){
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(1, nrow(Neighbors))))
                }
                #If not perform the alternative with user supplied COO_weight
                else{
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(COO_weight, 1-COO_weight)))
                }
                Final_cell <- COO %>% dplyr::select(-DIST) %>% mutate(mean_DIST = mean(Neighbors$DIST), max_DIST = max(Neighbors$DIST), N_neighbors = nrow(Neighbors))
                return(Final_cell)
              }
              else if(Weighting_Strategy == "Proximity"){
                #Assign a weight by neighbor proximity
                Neighbors <- Neighbors %>% mutate(Neighboring_weight =  1/ (DIST/sum(Neighbors$DIST)))
                Neighbors$Neighboring_weight[is.infinite(Neighbors$Neighboring_weight)] <- max(Neighbors$Neighboring_weight[!is.infinite(Neighbors$Neighboring_weight)])
                
                #Calculate weighted mean for each marker according to cell proximity
                Averaged_markers <- map_dbl(Neighbors[-c(1:4, (ncol(Neighbors)-1), ncol(Neighbors))], function(x) stats::weighted.mean(x, w = Neighbors$Neighboring_weight))
                
                #Perform message passing
                #If COO_weight is null then perform the classic algorithm (all cells are weighted equally)
                if(is.null(COO_weight)){
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(1, nrow(Neighbors))))
                }
                #If not perform the alternative with user supplied COO_weight
                else{
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(COO_weight, 1-COO_weight)))
                }
                Final_cell <- COO %>% dplyr::select(-DIST) %>% mutate(mean_DIST = mean(Neighbors$DIST), max_DIST = max(Neighbors$DIST), N_neighbors = nrow(Neighbors))
                return(Final_cell)
              }
              else if(Weighting_Strategy == "Disregarded_minority"){
                #Assign a FIXED weight to all neighbors (INDEPENDENT OF DISTANCE)
                Neighbors <- Neighbors %>% mutate(Neighboring_weight = 1)
                Neighbors$Neighboring_weight[is.infinite(Neighbors$Neighboring_weight)] <- max(Neighbors$Neighboring_weight[!is.infinite(Neighbors$Neighboring_weight)])
                
                #Calculate weighted mean for each marker according to cell proximity
                Averaged_markers <- map_dbl(Neighbors[-c(1:4, (ncol(Neighbors)-1), ncol(Neighbors))], function(x) stats::weighted.mean(x, w = Neighbors$Neighboring_weight))
                
                #Weight results according to Image marker abundance
                Averaged_markers_weighted <- Averaged_markers * Image_mean_marker_intensity_weight
                
                #Perform message passing
                #If COO_weight is null then perform the classic algorithm (all cells are weighted equally)
                if(is.null(COO_weight)){
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(1, nrow(Neighbors))))
                }
                #If not perform the alternative with user supplied COO_weight
                else{
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(COO_weight, 1-COO_weight)))
                }
                Final_cell <- COO %>% dplyr::select(-DIST) %>% mutate(mean_DIST = mean(Neighbors$DIST), max_DIST = max(Neighbors$DIST), N_neighbors = nrow(Neighbors))
                return(Final_cell)
              }
              else if(Weighting_Strategy == "Both"){
                #Assign a weight by neighbor proximity
                Neighbors <- Neighbors %>% mutate(Neighboring_weight =  1/ (DIST/sum(Neighbors$DIST)))
                Neighbors$Neighboring_weight[is.infinite(Neighbors$Neighboring_weight)] <- max(Neighbors$Neighboring_weight[!is.infinite(Neighbors$Neighboring_weight)])
                
                #Calculate weighted mean for each marker according to cell proximity
                Averaged_markers <- map_dbl(Neighbors[-c(1:4, (ncol(Neighbors)-1), ncol(Neighbors))], function(x) stats::weighted.mean(x, w = Neighbors$Neighboring_weight))
                
                #Weight results according to Image marker abundance
                Averaged_markers_weighted <- Averaged_markers * Image_mean_marker_intensity_weight
                
                #Perform message passing adjusted by weight
                #If COO_weight is null then perform the classic algorithm (all cells are weighted equally)
                if(is.null(COO_weight)){
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(1, nrow(Neighbors))))
                }
                #If not perform the alternative with user supplied COO_weight
                else{
                  COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Averaged_markers, function(.x, .y) stats::weighted.mean(c(.x, .y), w = c(COO_weight, 1-COO_weight)))
                }
                Final_cell <- COO %>% dplyr::select(-DIST) %>% mutate(mean_DIST = mean(Neighbors$DIST), max_DIST = max(Neighbors$DIST), N_neighbors = nrow(Neighbors))
                return(Final_cell)
              }
            }
            #SUM
            if(Message_strategy == "Sum"){
              #If no correction then perform bare sum
              if(Weighting_Strategy == "None"){
                #Add neighbor markers
                Added_markers <- map_dbl(Neighbors[-c(1:4, ncol(Neighbors))], sum)
                #Add neighbor markers to COO
                COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Added_markers, sum)
                #Generate the final cell
                Final_cell <- COO %>% dplyr::select(-DIST) %>% mutate(mean_DIST = mean(Neighbors$DIST), max_DIST = max(Neighbors$DIST), N_neighbors = nrow(Neighbors))
                return(Final_cell)
              }
              
              if(Weighting_Strategy == "Proximity"){
                #First get our neighbors distance and calculate weights 
                Distance_weight <- Neighbors$DIST / sum(Neighbors$DIST)
                Distance_weight <- (1/Distance_weight) / sum(1/Distance_weight)
                
                #Then we sum all the values in our matrix to obtain the total credit to be delivered
                Expression_matrix <- Neighbors %>% dplyr::select(-c(1:4), -DIST) %>% Matrix::as.matrix()
                Total_credit <- sum(Expression_matrix)
                Credit_by_Neighbor <- Total_credit*Distance_weight
                
                #Now we calculate the relative credit each cell will assign to each marker
                Relative_value <- sweep(Expression_matrix, MARGIN = 1, STATS = Matrix::rowSums(Expression_matrix), FUN = "/", check.margin = FALSE) 
                Final_expression <- sweep(Relative_value, MARGIN = 1, STATS = Credit_by_Neighbor, FUN = "*", check.margin = FALSE) 
                
                #Calculate final neighbors
                Neighbors_Weighted <- bind_cols(Neighbors[c(1:4)], Final_expression, Neighbors["DIST"])
                #Add neighbor markers
                Added_markers <- map_dbl(Neighbors_Weighted[-c(1:4, ncol(Neighbors_Weighted))], sum)
                #Add neighbor markers to COO
                COO[-c(1:4, ncol(COO))] <- map2_dfc(.x = unlist(COO[-c(1:4, ncol(COO))]), .y = Added_markers, sum)
                #Generate the final cell
                Final_cell <- COO %>% dplyr::select(-DIST) %>% mutate(mean_DIST = mean(Neighbors$DIST), max_DIST = max(Neighbors$DIST), N_neighbors = nrow(Neighbors))
                return(Final_cell)
              }
            }
          },
          .progress = TRUE)
        future::plan("future::sequential")
        gc()
        return(Message_passed_cells)
      }, .progress = list(clear = F,
                          name = "Performing Message Passing",
                          show_after = 1,
                          type = "iterator")
      )
    
    #Turn results into a single tibble and return
    return(RESULTS)
  }, 
  options = list(optimize = 3))     

UTAG_Neighborhood_identifier <- cmpfun(
  function(DATA = NULL,
           Strategy = NULL,
           Min_Neighbors = NULL,
           
           #Denoising parameters
           Apply_Denoise = NULL, #Specify if a denoising filtering is required before clustering
           Denoising = NULL, #Select denoising strategy from: Quantile, Standard_Deviation, Threshold, Otsu or DimRed_DBscan
           Percentile = NULL, #Select the adequate percentile for quantile threshold
           N_Standard_Deviations = NULL, #Select the number of standard deviations from mean for Standard_Deviation method
           Selected_threshold = NULL, #Select the absolute threshold for the Threshold method
           Min_cell_no = NULL, #Parameter for DBscan
           Distance_radius = NULL, #Parameter for DBscan
           
           #Dimension reduction
           Perform_Dimension_reduction = NULL,
           Dimension_reduction = NULL,
           Dimension_reduction_prop = NULL,
           Cluster_on_Reduced = NULL,
           
           #Parameters for Consensus Clustering
           Max_N_neighborhoods = NULL, #Number of maximum neighborhods that you desire to find
           Consensus_reps = NULL, #Number of iterations of the algorithm to try to converge
           Consensus_p_Items = NULL, #Percentage of the closest neighbor patterns that you desire to sample in each iteration
           Consensus_Cluster_Alg = NULL, #Clustering algorithm to be used (’hc’ hierarchical (hclust), ’pam’ for paritioning around medoids, ’km’ for k-means )
           Consensus_Distance = NULL, #Distance metric to be used (pearson(1 - Pearson correlation), spearman(1 - Spearman correlation), euclidean, binary, maximum, canberra, minkowski
           Consensus_Name = NULL, #Name of the folder that is going to be created in order to place the resulting graphs
           
           #Parameters for Self-Organizing Maps
           Max_SOM_neighborhoods = NULL, #Maximum number of clusters (neighborhoods) to try in the algorithm
           
           #Parameters for Graph-Based approaches
           Nearest_neighbors_for_graph = NULL, #Specify the number of closest neighbors to calculate the graph
           Graph_Method = NULL, #Specify the clustering method
           Graph_Resolution = NULL, #Specify the graph resolution
           N_steps = NULL, #Number of steps given in the WalkTrap algorithm
           
           #Parameters for K means Meta Clustering
           N_K_centroids = NULL, #Number of centroids to perform K means
           Max_N_neighborhoods_Meta = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Consensus_reps_Meta = NULL, #Number of iterations of the algorithm to try to converge
           Consensus_p_Items_Meta = NULL, #Percentage of cells that you desire to sample in each iteration
           Consensus_Name_Meta = NULL, #Name of the folder that is going to be created in order to place the resulting graphs
           
           #Parameters for Batched K means
           Batch_size = NULL, #The number of cells to be included in each random batch
           Max_N_neighborhoods_Batch = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Percentage_centroid_initiation = NULL,
           N_initiations = NULL, #Number of times the algorithm is going to be tried to find the best clustering result
           Max_iterations = NULL, #Max number of iterations in each try
           
           #Parameters for Gaussian Mixture Model
           Quality_metric = NULL, #The quality measure used to test the number of clusters ("AIC" or "BIC")
           Max_N_neighborhoods_GMM = NULL, #Number of maximum clusters (phenotypes) that you desire to find
           Max_iterations_km = NULL, #Number of max iterations in the K means clustering performed
           Max_iterations_em = NULL, #Number of max iterations in the Expectation Maximization algorithm
           GMM_Distance = NULL, #Distance metric to use in the model ("eucl_dist" or "maha_dist")
           
           #Parameters for CLARA clustering
           Samples_CLARA = NULL, #Number of samples the CLARA algorithm is going to use to be calculated
           Sample_per_CLARA = NULL, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
           Max_N_neighborhoods_CLARA = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Distance_CLARA = NULL, #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
           #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
           N_cores = NULL #Number of cores to parallelize your computation
  ){
    DATA <- DATA
    #Check argument
    if(!identical(names(DATA)[1:4], c("Cell_no", "X", "Y", "Subject_Names"))) {
      stop("Please generate an appropiate data object using the UTAG_message_passing")
    }
    if(!all(c("N_neighbors", "mean_DIST", "max_DIST") %in% names(DATA))){
      stop("Please generate an appropiate data object using the UTAG_message_passing")
    }
    if(!all(Min_Neighbors%%1 == 0, Min_Neighbors >= 0)) stop("Min_Neighbors must be a positive integer value")
    if(!Strategy %in% c("Consensus_Clustering", "SOM", "Graph_Based", 
                        "K_Means_Meta_clustering", "Batch_K_means", "GMM", 
                        "CLARA_clustering")) stop("Strategy must be one of the following: Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering")
    if(!is.logical(Apply_Denoise)) stop("Apply_Denoise must be a logical value")
    if(!is.logical(Perform_Dimension_reduction)) stop("Perform_Dimension_reduction must be a logical value")
    if(Perform_Dimension_reduction){
      if(!Dimension_reduction %in% c("UMAP", "TSNE", "PCA")) stop("Dimension_reduction must be one of the following: UMAP, TSNE, PCA")
      if(!all(is.numeric(Dimension_reduction_prop), Dimension_reduction_prop > 0, Dimension_reduction_prop <= 1)) stop("Dimension_reduction_prop must be a numeric value between 0 and 1")
    }
    if(!is.logical(Cluster_on_Reduced)) stop("Cluster_on_Reduced must be a logical value")
    if(Cluster_on_Reduced){
      if(!Perform_Dimension_reduction) stop("If Clustering needst o be performed on Dimension reduced data please set Perform_Dimension_reduction to TRUE")
    }
    if(Strategy == "Consensus_Clustering"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Max_N_neighborhoods_OK = (Max_N_neighborhoods >= 2 & Max_N_neighborhoods%%1 == 0),
                            Consensus_reps_OK = (Consensus_reps >= 1 & Consensus_reps%%1 == 0),
                            Consensus_p_Items_OK = (Consensus_p_Items > 0 & Consensus_p_Items <= 1),
                            Consensus_Cluster_Alg_OK = Consensus_Cluster_Alg %in% c("hc", "pam", "km"),
                            Consensus_Distance_OK = Consensus_Distance %in% c("pearson", "spearman", "euclidean", "binary", "maximum", "canberra", "minkowski"),
                            Consensus_Name_OK = is.character(as.character(Consensus_Name))
      )
      Stop_messages <- c(Max_N_neighborhoods_OK = "Max_N_neighborhoods must be an integer value > 1",
                         Consensus_reps_OK = "Consensus_reps_OK must be an integer value > 0",
                         Consensus_p_Items_OK = "Consensus_p_Items must be a numeric value > 0 and lower than 1",
                         Consensus_Cluster_Alg_OK = "Consensus_Cluster_Alg must be one of the following: hc, pam, km",
                         Consensus_Distance_OK = "Consensus_Distance must be one the following: pearson, spearman, euclidean, binary, maximum, canberra, minkowski",
                         Consensus_Name_OK = "Consensus_Name must ve a character value")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "SOM"){
      #Check arguments
      if(!(Max_SOM_neighborhoods > 1 & Max_SOM_neighborhoods%%1 == 0)) stop("Max_SOM_neighborhoods must be an integer value > 1")
    }
    if(Strategy == "Graph_Based"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Nearest_neighbors_for_graph_OK = (Nearest_neighbors_for_graph >= 1 & Nearest_neighbors_for_graph%%1 == 0),
                            Graph_Method_OK = Graph_Method %in% c("Louvain", "Leiden", "Greedy", "WalkTrap", "Spinglass", "Leading_Eigen", "Edge_Betweenness"),
                            Graph_Resolution_OK = all(is.numeric(Graph_Resolution), Graph_Resolution > 0),
                            N_steps_OK = is.null(N_steps) || (N_steps >=1 & N_steps%%1 == 0)
      )
      Stop_messages <- c(Nearest_neighbors_for_graph = "Nearest_neighbors_for_graph must be an integer value > 0",
                         Graph_Method = "Graph_Method must be one of the following: Louvain, Leiden, Greedy, WalkTrap, Spinglass, Leading_Eigen, Edge_Betweenness",
                         Graph_Resolution = "Graph_Resolution must be a numeric value > 0",
                         N_steps = "N_steps must be a integer value > 0")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "K_Means_Meta_clustering"){
      #Check arguments
      Argument_checker <- c(N_K_centroids_OK = all(nrow(DATA) > N_K_centroids, N_K_centroids%%1 == 0, N_K_centroids > 0),
                            Max_N_neighborhoods_Meta_OK = (Max_N_neighborhoods_Meta >= 2 & Max_N_neighborhoods_Meta%%1 == 0),
                            Consensus_reps_Meta_OK = (Consensus_reps_Meta >= 1 & Consensus_reps_Meta%%1 == 0),
                            Consensus_p_Items_Meta_OK = (Consensus_p_Items_Meta > 0 & Consensus_p_Items_Meta <= 1),
                            Consensus_Name_Meta_OK = is.character(as.character(Consensus_Name_Meta))
      )
      Stop_messages <- c(N_K_centroids_OK = "N_K_centroids must be smaller than the number of cells in DATA and an integer value > 0",
                         Max_N_neighborhoods_Meta_OK = "Max_N_neighborhoods_Meta must be an integer value > 1",
                         Consensus_reps_Meta_OK = "Consensus_reps_Meta must be an integer value > 0",
                         Consensus_p_Items_Meta_OK = "Consensus_p_Items_Meta must be a numeric value > 0 and lower than 1",
                         Consensus_Name_Meta_OK = "Consensus_Name_Meta must ve a character value"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "Batch_K_means"){
      #Check arguments
      Argument_checker <- c(Batch_size_OK = (Batch_size <= nrow(DATA) & Batch_size%%1 == 0 & Batch_size > 0),
                            Max_N_neighborhoods_Batch_OK = (Max_N_neighborhoods_Batch >= 2 & Max_N_neighborhoods_Batch%%1 == 0),
                            N_initiations_OK = (N_initiations >= 1 & N_initiations%%1 == 0),
                            Max_iterations_OK = (Max_iterations%%1 == 0)
      )
      Stop_messages <- c(Batch_size_OK = str_c("Batch size should be an integer value < ", nrow(DATA), " and > 0", collapse = ""),
                         Max_N_phenotypes_Batch_OK = "Max_N_neighborhoods_Batch must be an integer value > 1",
                         N_initiations_OK = "N_initiations must be an integer value > 0",
                         Max_iterations_OK = "Max_iterations must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "GMM"){
      #Check arguments
      Argument_checker <- c(Quality_metric_OK = Quality_metric %in% c("AIC", "BIC"),
                            Max_N_neighborhoods_GMM_OK = (Max_N_neighborhoods_GMM >= 2 & Max_N_neighborhoods_GMM%%1 == 0),
                            Max_iterations_km_OK = (Max_iterations_km >= 1 & Max_iterations_km%%1 == 0),
                            Max_iterations_em_OK = (Max_iterations_em >= 1 & Max_iterations_em%%1 == 0),
                            GMM_Distance_OK = GMM_Distance %in% c("eucl_dist", "maha_dist")
      )
      Stop_messages <- c(Quality_metric_OK = "Quality_metric must be one of the following: AIC, BIC",
                         Max_N_phenotypes_GMM_OK = "Max_N_phenotypes must be an integer value > 1",
                         Max_iterations_km_OK = "Max_iterations_km must be an integer value > 1",
                         Max_iterations_em_OK = "Max_iterations_em must be an integer value > 1",
                         GMM_Distance_OK = "GMM_Distance must be one of the following: eucl_dist, maha_dist"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    if(Strategy == "CLARA_clustering"){
      #Check arguments
      Argument_checker <- c(Samples_CLARA_OK = (Samples_CLARA >= 1 & Samples_CLARA%%1 == 0),
                            Sample_per_CLARA_OK = (Sample_per_CLARA > 0 & Sample_per_CLARA <= 1),
                            Max_N_neighborhoods_CLARA_OK = (Max_N_neighborhoods_CLARA >= 2 & Max_N_neighborhoods_CLARA%%1 == 0),
                            Distance_CLARA_OK = Distance_CLARA %in% c("euclidean", "manhattan", "chebyshev", "canberra", "braycurtis", 
                                                                      "pearson_correlation", "simple_matching_coefficient", "minkowski", 
                                                                      "hamming", "jaccard_coefficient", "Rao_coefficient", "mahalanobis", "cosine"),
                            N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0)
      )
      Stop_messages <- c(Samples_CLARA_OK = "Samples_CLARA must be an integer value > 0",
                         Sample_per_CLARA_OK = "Sample_per_CLARA must be a numeric value between 0 and 1",
                         Max_N_neighborhoods_CLARA_OK = "Max_N_neighborhoods_CLARA must be an integer value > 1",
                         Distance_CLARA_OK = "Distance_CLARA must be one of the following: euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine",
                         N_cores_OK = "N_cores must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    
    
    #Import data and filter by the n of neighbors
    DATA <- DATA %>% dplyr::filter(N_neighbors >= Min_Neighbors)
    DATA <- DATA %>% dplyr::select(-mean_DIST, -max_DIST, -N_neighbors)
    
    #Perform dimension reduction if required
    if(Perform_Dimension_reduction){
      #First PCA 
      if(Dimension_reduction == "PCA"){
        if(Dimension_reduction_prop != 1) stop("PCA must be performed using Dimension_reduction_prop = 1")
        print("Generating PCA projections")
        #Scale and turn into matrix
        DATA_matrix <- DATA %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
        Result_PCA <- svd::propack.svd(DATA_matrix, neig = 2)$u
        DATA_Reduction <- tibble(Cell_no = DATA$Cell_no, DIMENSION_1 = unlist(Result_PCA[,1]), DIMENSION_2 = unlist(Result_PCA[,2]))
      }
      
      #Second TSNE
      if(Dimension_reduction == "TSNE"){
        if(Dimension_reduction_prop == 1) {
          print("Generating TSNE projections")
          if(nrow(DATA) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          DATA_matrix <- DATA %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = TRUE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Result_TSNE[,1]), DIMENSION_2 = unlist(Result_TSNE[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating TSNE projections")
          DATA_matrix <- DATA %>% dplyr::group_by(Subject_Names) %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% dplyr::ungroup() %>%
            dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = FALSE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          Coords <- snifter::project(Result_TSNE, 
                                     new = DATA  %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix(), 
                                     old = DATA_matrix)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
      
      #Third UMAP
      if(Dimension_reduction == "UMAP"){
        if(Dimension_reduction_prop == 1) {
          print("Generating UMAP projections")
          if(nrow(DATA) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take some time")
          #scale and turn into matrix
          DATA_matrix <- DATA %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Result_UMAP[,1]), DIMENSION_2 = unlist(Result_UMAP[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating UMAP projections")
          DATA_matrix <- DATA %>% dplyr::group_by(Subject_Names) %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% dplyr::ungroup() %>%
            dplyr::select(-c(1:4)) %>% scale() %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take aome time")
          #scale and turn into matrix
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L, ret_model = TRUE)
          Coords <- uwot::umap_transform(X = DATA  %>% dplyr::select(-c(1:4)) %>% scale() %>% as.matrix(), 
                                         model = Result_UMAP)
          DATA_Reduction <- bind_cols(DATA["Cell_no"], DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
    }
    
    #If denoising is required apply required function
    if(Apply_Denoise){
      #check denoising argument is correctly stated
      if(!Denoising %in% c("Quantile", "Standard_Deviation", "Threshold", "Otsu", "DimRed_DBscan")) {
        stop("Denoising should be one of Quantile, Standard_Deviation, Threshold, Otsu, DimRed_DBscan")
      }
      
      print("Filtering out noisy cells")
      #Identify each cell in the experiment with a unique ID
      DATA <- DATA %>% mutate(Unique_ID = 1:nrow(DATA))
      DATA <- DATA[c(ncol(DATA), 1:(ncol(DATA)-1))]
      
      #Apply desired filters
      if(Denoising == "Quantile") {
        #Check arguments
        if(Percentile < 0.01 | Percentile > 0.99) stop("Percentile must be between 0.01 and 0.99")
        
        FILTER <- map_dfc(DATA[-(1:5)], function(x){
          x <= quantile(x, Percentile)
        })
      }
      
      else if(Denoising == "Standard_Deviation"){
        #Check arguments
        if(!is.numeric(N_Standard_Deviations)) stop("N_Standard_Deviations must be a numeric value")
        
        FILTER <- map_dfc(DATA[-(1:5)], function(x){
          x <= (mean(x) - (N_Standard_Deviations*sd(x)))
        })
      }
      
      else if(Denoising == "Threshold"){
        #Check arguments
        if(!is.numeric(Selected_threshold)) stop("Selected_threshold must be a numeric value")
        
        FILTER <- map_dfc(DATA[-(1:5)], function(x){
          x <= Selected_threshold
        })
      }
      
      else if(Denoising == "Otsu"){
        FILTER <- map2_df(.x = DATA[-c(1:5)],
                          .y = map_dbl(DATA[-c(1:5)], function(z){
                            EBImage::otsu(array(z, dim = c(1, length(z))), range = c(min(z), max(z)), levels = length(unique(z)))
                          }),
                          function(.x, .y) .x <= .y)
      }
      
      else if(Denoising == "DimRed_DBscan"){
        #Requires previous dimension reduction
        if(!Perform_Dimension_reduction) stop("DBscan clustering requires Dimension reduction. Please set Perform_Dimension_reduction to TRUE")
        #Check other arguments
        if(!all(is.numeric(Min_cell_no), Min_cell_no%%1 == 0, Min_cell_no > 0)) stop("Min_cell_no must be an integer value > 0")
        if(!all(is.numeric(Distance_radius), Distance_radius > 0)) stop("Distance_radius must be a numeric value > 0")
        
        #Proceed with algorithm 
        DB_results <- dbscan::dbscan(DATA_Reduction[c("DIMENSION_1", "DIMENSION_2")], eps = Distance_radius, minPts = Min_cell_no, borderPoints = FALSE)
        
        #whole plot for small samples
        if(length(DB_results$cluster) <= 100000){
          plot(
            tibble(Dim_1 = DATA_Reduction[["DIMENSION_1"]], Dim_2 = DATA_Reduction[["DIMENSION_2"]], Cluster = DB_results$cluster) %>%
              mutate(Cluster = case_when(Cluster == 0 ~ "Noise",
                                         TRUE ~ "Approved")) %>%
              ggplot(aes(x = Dim_1, y = Dim_2, color = Cluster)) + geom_point(size = 0.8) +
              scale_color_manual("", values = c("black", "grey"))+
              cowplot::theme_cowplot()+
              theme(panel.grid = element_blank())
          )
        }
        
        #Subsample plot for large dataset
        if(length(DB_results$cluster) > 100000){
          message(">100K observations to generate plots. A random subset containing 10% of the dataset will be selected for Dimension reduction plots")
          plot(
            tibble(Dim_1 = DATA_Reduction[["DIMENSION_1"]], Dim_2 = DATA_Reduction[["DIMENSION_2"]], Cluster = DB_results$cluster) %>%
              mutate(Cluster = case_when(Cluster == 0 ~ "Noise",
                                         TRUE ~ "Approved")) %>%  
              sample_n(size = 100000) %>%
              ggplot(aes(x = Dim_1, y = Dim_2, color = Cluster)) + geom_point(size = 1.5) +
              scale_color_manual("", values = c("black", "grey"))+
              cowplot::theme_cowplot()+
              theme(panel.grid = element_blank())
          )
        }
        
        DB_OK <- menu(choices = c("Proceed", "Abort"), title = "Are the results of the filtering OK?")
        if(DB_OK == 2) stop("Please refine Distance_radius and Min_cell_no parameters and retry")
        
        #Generate the NOISE column with a logical vector
        DATA <- DATA %>% mutate(NOISE = DB_results$cluster) %>% mutate(NOISE = case_when(NOISE == 0 ~ TRUE,
                                                                                         NOISE != 0 ~ FALSE))
      }
      
      #For no DBscan methods apply a row wise method to determine which cells are noise (a logical vector)
      if(Denoising != "DimRed_DBscan"){
        #Generate the variable to specify if the cell is noise or not
        DATA <- DATA %>% mutate(NOISE = unlist(apply(FILTER, MARGIN = 1, function(x) sum(x) == ncol(FILTER))))
      }
      
      #Generate two tibbles, one with noisy cells and other (DATA) with the actual cells
      NOISE_VECTOR <- DATA[["NOISE"]] #We generate a NOISE_VECTOR if we require to filter noise cells for Dimension reduction methods
      DATA_NOISE <- DATA %>% dplyr::filter(NOISE) %>% mutate(Phenotype = 1)
      DATA <- DATA %>% dplyr::filter(!NOISE)
      MARKERS <- DATA %>% dplyr::select(-Unique_ID, -Cell_no, -X, -Y, -Subject_Names, -NOISE)
    }
    
    #If denoising is not required obtain MARKERS directly from DATA
    else{
      DATA <- DATA
      MARKERS <- DATA %>% dplyr::select(-Cell_no, -X, -Y, -Subject_Names)
    }
    
    #Generate a specific version of Markers with dimension reduction data for Dimension_SNN
    if(Cluster_on_Reduced){
      #Depending on Denoising Obtain directly from DATA_Reduction or filter first
      if(!Apply_Denoise) MARKERS <- DATA_Reduction %>% dplyr::select(-Cell_no)
      if(Apply_Denoise) MARKERS <- DATA_Reduction[!NOISE_VECTOR, ] %>% dplyr::select(-Cell_no)
    }
    
    #Continue with clustering strategies
    if(Strategy == "Consensus_Clustering"){
      #Perform consensus clustering
      Phenotype_result <- try(ConsensusClusterPlus::ConsensusClusterPlus(t(as.matrix((MARKERS %>% scale()))), 
                                                                         maxK = Max_N_neighborhoods, 
                                                                         reps = Consensus_reps,
                                                                         pItem = Consensus_p_Items,
                                                                         pFeature = 1,
                                                                         title = Consensus_Name,
                                                                         clusterAlg = Consensus_Cluster_Alg,
                                                                         distance = Consensus_Distance,
                                                                         plot = "png",
                                                                         verbose = T)
      )
      #Test if consensus clustering returned an error
      if(berryFunctions::is.error(Phenotype_result)) {
        stop("Data is too large for Consensus Clustering. Please try another strategy")
      }
      else {
        #Make the user decide the number of neighborhoods according to results
        N_Phenotypes <- menu(choices = as.character(1:Max_N_neighborhoods), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Neighborhoods"))
        DATA_Phenotypes <- DATA %>% mutate(Phenotype = Phenotype_result[[as.double(N_Phenotypes)]][["consensusClass"]])
      }
    }
    
    else if(Strategy == "SOM"){
      print("Executing Self Organizing Map algorithm")
      #Transform data into a scaled matrix and perform Self Organizing Map
      SOM_results <- try(FlowSOM::FlowSOM(MARKERS %>% scale() %>% as.matrix(),
                                          scale = F,
                                          colsToUse = c(1:ncol(MARKERS)),
                                          maxMeta = Max_SOM_neighborhoods, #To find optimal meta clusters
                                          silent = F,
                                          seed = 21)
      )
      #Test if SOM returned an error
      if(berryFunctions::is.error(SOM_results)) {
        stop("Data is too large for Self-Organizing Maps. Please try another strategy")
      }
      else{
        #Assign phenotypes to each cell
        DATA_Phenotypes <- DATA %>% mutate(Phenotype = FlowSOM::GetMetaclusters(SOM_results))
      }
      
    }
    
    else if(Strategy == "Graph_Based"){
      print("Start Graph building process")
      #Transform data into a nearest neighbor graph 
      SNN_graph <- try(bluster::makeSNNGraph(MARKERS %>% scale() %>% as.matrix(), 
                                             k = Nearest_neighbors_for_graph)
      )
      
      #Test if Graph construction process returned an error
      if(berryFunctions::is.error(SNN_graph)) {
        stop("Data is too large to build a graph. Please try another strategy")
      }
      else{
        print("Performing Graph-based clustering")
        #Go for graph clustering
        #Cluster the graph with louvain or leiden clustering
        if(Graph_Method == "Louvain") {
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_louvain(SNN_graph,
                                                                                 weights = NULL,
                                                                                 resolution = Graph_Resolution)$membership)
          
        }
        
        else if(Graph_Method == "Leiden") {
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_leiden(SNN_graph,
                                                                                objective_function = "modularity",
                                                                                weights = NULL,
                                                                                resolution = Graph_Resolution,
                                                                                beta = 0.01,
                                                                                initial_membership = NULL,
                                                                                n_iterations = 100,
                                                                                vertex_weights = NULL)$membership)
          
        }
        
        else if(Graph_Method == "Greedy"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_fast_greedy(SNN_graph)$membership)
        }
        
        else if(Graph_Method == "WalkTrap"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_walktrap(SNN_graph,
                                                                                  steps = N_steps,
                                                                                  membership = T)$membership)
        }
        
        else if (Graph_Method == "Spinglass") {
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_spinglass(SNN_graph,
                                                                                   weights = NULL,
                                                                                   vertex = NULL,
                                                                                   spins = 25,
                                                                                   parupdate = FALSE,
                                                                                   start.temp = 1,
                                                                                   stop.temp = 0.01,
                                                                                   cool.fact = 0.99,
                                                                                   update.rule = c("config", "random", "simple"),
                                                                                   gamma = 1,
                                                                                   implementation = c("orig", "neg"),
                                                                                   gamma.minus = 1)$membership)
          
        }
        
        else if(Graph_Method == "Leading_Eigen"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_leading_eigen(SNN_graph,
                                                                                       membership = T)$membership)
        }
        
        else if(Graph_Method == "Edge_Betweenness"){
          DATA_Phenotypes <- DATA %>% mutate(Phenotype = igraph::cluster_edge_betweenness(SNN_graph,
                                                                                          weights = NULL,
                                                                                          directed = FALSE,
                                                                                          edge.betweenness = FALSE,
                                                                                          merges = FALSE,
                                                                                          bridges = FALSE,
                                                                                          modularity = FALSE,
                                                                                          membership = TRUE)$membership)
          
        }
      }
      
    }
    
    else if(Strategy == "K_Means_Meta_clustering"){
      print("Performing initial K-means algorithm")
      #First we need to perform K means Clustering 
      cl <- try(kmeans(MARKERS %>% scale %>% as.matrix, #Scale it and turn it into a matrix
                       centers = N_K_centroids, #Number of centroids to be calculated
                       iter.max = 50, 
                       nstart = 10))
      
      #Stop function if K means returned an error
      if(berryFunctions::is.error(cl)) {
        stop("Data is too large for K means clustering. Please try another strategy")
      }
      #Proceed if no error was returned
      else{
        #Assign this K means cluster to each observation
        DATA_filter_Markers <- DATA %>% mutate(K_means_Cl = cl$cluster)
        
        #Prepare data for Meta-Clustering
        #Create a tibble with the K means centroids and the format it for Consensus clustering
        K_medoids <- as_tibble(cl$centers) %>% mutate(K_means_Cl = 1:nrow(as_tibble(cl$centers)))
        tK_medoids <- K_medoids %>% dplyr::select(-K_means_Cl) %>% as.matrix %>% t
        
        print("Perorming Consensus Clustering")
        #Perform Consensus clustering with hierarchical clustering
        HC <- try(ConsensusClusterPlus::ConsensusClusterPlus(tK_medoids, 
                                                             maxK = Max_N_neighborhoods_Meta,   
                                                             reps = Consensus_reps_Meta, 
                                                             pItem = Consensus_p_Items_Meta,
                                                             pFeature = 1,
                                                             title = Consensus_Name_Meta,
                                                             distance = "euclidean",
                                                             clusterAlg = "pam",
                                                             plot = "png",
                                                             verbose = T))
        #Test if consensus clustering returned an error
        if(berryFunctions::is.error(HC)) {
          stop("Data is too large for Meta Clustering. Please try another strategy or select a smaller N_K_centroids value")
        }
        else {
          #Make the user decide the number of neighborhoods according to results
          N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_Meta), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Neighborhoods"))
          
          #Bind the final Phenotype to the K medoids tibble
          K_medoids <- K_medoids %>% mutate(Phenotype = HC[[as.double(N_Phenotypes)]][["consensusClass"]])
          K_medoids_for_join <- K_medoids %>% dplyr::select(K_means_Cl, Phenotype)
          
          #Bind The DATA and the K_meoids to obtain the final matrix
          DATA_Phenotypes <- left_join(DATA_filter_Markers, K_medoids_for_join, by = "K_means_Cl") %>% dplyr::select(-K_means_Cl)
        }
      } 
    }
    
    else if(Strategy == "Batch_K_means"){
      #First we calculate a metric to decide the number of total phenotypes
      #Specify the params
      params_mbkm <- list(batch_size = Batch_size, 
                          init_fraction = 1, 
                          early_stop_iter = 10)
      print("Starting Cluster number stimation process")
      #Run the specified test using each of the number of clusters
      Optimal <- try(ClusterR::Optimal_Clusters_KMeans(MARKERS %>% scale(), 
                                                       max_clusters = Max_N_neighborhoods_Batch, 
                                                       num_init = N_initiations, 
                                                       max_iters = Max_iterations, 
                                                       initializer = "kmeans++",
                                                       criterion = "Adjusted_Rsquared",
                                                       plot_clusters = T, 
                                                       mini_batch_params = params_mbkm,
                                                       verbose = T)
      )
      
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_Batch), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Neighborhoods"))
        
        print("Performing Batched K means algorithm")
        #Calculate the desired number of clusters with batch k menas
        Batch_k_means <- ClusterR::MiniBatchKmeans(MARKERS %>% scale(),
                                                   clusters = as.double(N_Phenotypes), 
                                                   batch_size = Batch_size,
                                                   num_init = N_initiations,
                                                   max_iters = Max_iterations,
                                                   init_fraction = 1,
                                                   initializer = "kmeans++",
                                                   early_stop_iter = 10,
                                                   verbose = T,
                                                   tol = 1e-07, #The required improvement rate to continue with the iterations (the lower the more iterations will be required)
                                                   CENTROIDS = NULL,
                                                   seed = 21)
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = Batch_k_means, fuzzy = F, newdata = MARKERS %>% scale()) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Phenotype"
        
        #Generate the data phenotypes tibble
        DATA_Phenotypes <- bind_cols(DATA, pr_mb)
      }
      
      
    }
    
    else if(Strategy == "GMM"){
      print("Starting Cluster number stimation process")
      #First we calculate a metric to decide the number of total phenotypes
      #Run the specified test using each of the number of clusters
      Optimal <- try(ClusterR::Optimal_Clusters_GMM(MARKERS %>% scale(), 
                                                    criterion = Quality_metric,
                                                    max_clusters = Max_N_neighborhoods_GMM, 
                                                    dist_mode = GMM_Distance,
                                                    seed_mode = "random_subset",
                                                    km_iter = Max_iterations_km,
                                                    em_iter = Max_iterations_em,
                                                    verbose = TRUE,
                                                    var_floor = 1e-10,
                                                    plot_data = TRUE)
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_GMM), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Phenotypes"))
        
        print("Calculating Gaussian Mixed Model")
        #Calculate the desired number of clusters with batch k menas
        GMM_model <- ClusterR::GMM(MARKERS %>% scale(),
                                   gaussian_comps = as.double(N_Phenotypes), 
                                   dist_mode = GMM_Distance,
                                   seed_mode = "random_subset",
                                   km_iter = Max_iterations_km,
                                   em_iter = Max_iterations_em,
                                   verbose = TRUE,
                                   var_floor = 1e-10,
                                   full_covariance_matrices = FALSE
        )
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = GMM_model, fuzzy = F, newdata = MARKERS %>% scale()) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Phenotype"
        
        #Generate the data phenotypes tibble
        DATA_Phenotypes <- bind_cols(DATA, pr_mb)
      }
    }
    
    else if(Strategy == "CLARA_clustering"){
      print("Starting Cluster number stimation process")
      #First we calculate a metric to decide the number of total phenotypes
      Optimal <-  try(ClusterR::Optimal_Clusters_Medoids(MARKERS %>% scale(),
                                                         max_clusters = Max_N_neighborhoods_CLARA,
                                                         distance_metric = Distance_CLARA,
                                                         criterion = "silhouette" ,
                                                         clara_samples = Samples_CLARA,
                                                         clara_sample_size = Sample_per_CLARA,
                                                         swap_phase = F,
                                                         threads = N_cores,
                                                         verbose = T,
                                                         plot_clusters = T
      )
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Continue if everything OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_neighborhoods_CLARA), 
                            title = paste0("Based on the plots generated and you previous choice, decide the appropiate number of final Neighborhoods"))
        
        print("Performing CLARA (Clustering Large Applications)")
        CLARA_Clustering <- ClusterR::Clara_Medoids(MARKERS %>% scale(),
                                                    clusters = as.double(N_Phenotypes), 
                                                    samples = Samples_CLARA,
                                                    sample_size = Sample_per_CLARA,
                                                    distance_metric = Distance_CLARA,
                                                    threads = N_cores,
                                                    swap_phase = F,
                                                    fuzzy = FALSE,
                                                    verbose = T,
                                                    seed = 21
        )
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = CLARA_Clustering, fuzzy = F, newdata = MARKERS %>% scale()) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Phenotype"
        
        #Generate the data phenotypes tibble
        DATA_Phenotypes <- bind_cols(DATA, pr_mb)
      }
    }
    
    #If there are noisy and real cells bind both tibbles
    if(Apply_Denoise){
      DATA_Phenotypes <- DATA_Phenotypes %>% mutate(Phenotype = as.numeric(as.numeric(Phenotype) + 1))
      DATA_Phenotypes <- bind_rows(DATA_NOISE, DATA_Phenotypes) %>% arrange(Unique_ID) %>% dplyr::select(-Unique_ID, -NOISE)
      message("If denoising is applied, Cluster nº1 contains the noisy cells")
    }
    #Turn it into a factor
    DATA_Phenotypes <- DATA_Phenotypes %>% mutate(Phenotype = factor(Phenotype))
    
    #Since we are recycling the function from STEP 3 we need to change the col name to Neighborhood
    names(DATA_Phenotypes)[ncol(DATA_Phenotypes)] <- "Neighborhood_assignment"
    
    #plot dimension reduction according to the number of cells
    if(Perform_Dimension_reduction){
      if(nrow(DATA_Phenotypes) <= 100000){
        try(plot(
          left_join(DATA_Phenotypes, DATA_Reduction, by = "Cell_no") %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = Neighborhood_assignment)) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Neighborhood_assignment", values = unname(pals::polychrome(length(unique(DATA_Phenotypes$Neighborhood_assignment)))))
        )
        )
      }
      if(nrow(DATA_Phenotypes) > 100000){
        message(">100K observations to generate plots. A random subset containing 10% of the dataset will be selected for Dimension reduction plots")
        try(plot(
          left_join(DATA_Phenotypes, DATA_Reduction, by = "Cell_no") %>% sample_n(size = 100000) %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = Neighborhood_assignment)) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Neighborhood_assignment", values = unname(pals::polychrome(length(unique(DATA_Phenotypes$Neighborhood_assignment)))))
        )
        )
      }
    }
    
    #Skip plot rendering if too many cell features
    if(ncol(DATA_Phenotypes %>% dplyr::select(-c(1:4))) > 100){
      Plot_required <- menu(choices = c("Proceed", "Abort"), title = "More than 100 features. Proceed with plot rendering?")
      if(Plot_required == 2){
        #Print a summary with the results
        print(DATA_Phenotypes %>% dplyr::count(Neighborhood_assignment))
        #Return data and Dimension reduction if generated
        if(Perform_Dimension_reduction) return(list(DATA = DATA_Phenotypes,
                                                    Dimension_reduction = DATA_Reduction)
        )
        else return(DATA_Phenotypes)
      }
    }
    
    #Visualize the neighbor composition data for each neighborhood
    plot(DATA_Phenotypes %>% dplyr::select(-c(1:4)) %>% pivot_longer(cols = -Neighborhood_assignment) %>%
           ggplot(aes(x = as.factor(Neighborhood_assignment), y = value)) +
           geom_violin(aes(color = name, fill = name), alpha=0.3, position=position_dodge(width=0.5)) +
           stat_summary(aes(color = name), 
                        fun = median, geom = "crossbar", width = 0.4, linetype = 1, linewidth = 0.5,
                        position = position_dodge(width = 0.5)) +
           cowplot::theme_cowplot()+
           scale_x_discrete("Neighborhood")+
           scale_y_continuous("Marker intensity"))
    
    #Visualize the heatmap of mean by neighborhood
    Mean_tibble <- DATA_Phenotypes %>% dplyr::select(-c(1:4)) %>% group_by(Neighborhood_assignment) %>% summarize_all(mean) %>% ungroup() #Obtain mean tibble
    Mean_matrix <- as.matrix(Mean_tibble[-1] %>% scale()) #Scale it and transform it into a  matrix
    row.names(Mean_matrix) <- Mean_tibble[[1]] 
    
    plot(ComplexHeatmap::Heatmap(Mean_matrix,
                                 name = "Scaled")
    )
    
    #Print a summary with the results
    print(DATA_Phenotypes %>% dplyr::count(Neighborhood_assignment))
    
    #Return data and Dimension reduction if generated
    if(Perform_Dimension_reduction) return(list(DATA = DATA_Phenotypes,
                                                Dimension_reduction = DATA_Reduction)
    )
    else return(DATA_Phenotypes)
    
  }, options = list(optimize = 3))

DATA_neighborhoods_renamer <- cmpfun(
  function(DATA = NULL,
           New_names = NULL) {
    #Check that DATA has a neighborhood_assignment variable
    if(!"Neighborhood_assignment" %in% names(DATA)) stop("DATA must be generated using Neighborhood_discovery_function or UTAG_Neighborhood_identifier")
    
    #Check if provided names are equal to number of hoods
    if(length(New_names) != length(unique(DATA$Neighborhood_assignment))) {
      stop(paste0("Provided New_names should match the number of Neighborhoods in the analysis. Number of neighborhoods: ", length(unique(DATA$Neighborhood_assignment)),
                  ". Names provided: ", length(New_names)))
    }
    
    #Proceed with renaming
    else{
      DATA_neighborhoods <- DATA
      names_tibble <- tibble(Neighborhood_assignment = factor(1:length(unique(DATA_neighborhoods$Neighborhood_assignment))),
                             New_names = New_names)
      Result <- left_join(DATA_neighborhoods, names_tibble, by = "Neighborhood_assignment")
      Result %>% mutate(Neighborhood_assignment = New_names) %>% dplyr::select(-New_names)
    }
  },
  options = list(optimize = 3))    

Neighborhood_Quantifier <- cmpfun(
  function(DATA = NULL,
           Calculate_Density = NULL,
           DATA_Area = NULL
  ) {
    
    #Check arguments
    if(!is.logical(Calculate_Density)){
      stop("Calculate_Density must be a logical value")
    }
    #Check that DATA has a neighborhood_assignment variable
    if(!"Neighborhood_assignment" %in% names(DATA)) stop("DATA must be generated using Neighborhood_discovery_function or UTAG_Neighborhood_identifier")
    
    #Import Neighborhood data
    DATA_neighborhoods <- DATA
    
    #Generate a tibble where the number of neighborhoods by sample are specified
    RESULTS_TIBBLE <- map_dfr(unique(DATA_neighborhoods$Subject_Names), function(Image) {
      #Select individual images
      Interim <- DATA_neighborhoods %>% dplyr::filter(Subject_Names == Image)
      
      #Count the number of neighborhoods
      Results <- Interim %>% dplyr::count(Neighborhood_assignment) %>% pivot_wider(names_from = Neighborhood_assignment, values_from = n)
      #Quantify the proportion
      Results_prop <- Results/nrow(Interim)
      names(Results_prop) <- str_c("PROP_", names(Results))
      Results$N_cells <- nrow(Interim)
      Results$Subject_Names <- Image
      
      #arrange the tibble and exit loop
      Results <- Results[c(ncol(Results), 1:(ncol(Results)-1))]
      bind_cols(Results, Results_prop)
      
    }, .progress = list(clear = F,
                        name = "Calculating neighborhood counts",
                        show_after = 1,
                        type = "iterator"))
    
    #Substitute NA by 0
    RESULTS_TIBBLE[is.na(RESULTS_TIBBLE)] <- 0
    
    #Graph without clinical data
    plot(RESULTS_TIBBLE %>% dplyr::select(Subject_Names, contains("PROP_")) %>% pivot_longer(-Subject_Names) %>%
           ggplot(aes(x = Subject_Names, y = value)) + facet_wrap(~name, "free_y", ncol = 1, nrow = length(unique(DATA_neighborhoods$Neighborhood_assignment))) +
           geom_col(width = 0.5, color = "black") +
           cowplot::theme_cowplot() +
           scale_y_continuous("% of neighborhood in sample")+
           scale_x_discrete("")+
           theme(axis.text.x = element_text(angle = -90,
                                            vjust = 0.5))
    )
    
    #If density computing is required, execute the following code
    if(Calculate_Density){
      #Check arguments
      if(names(DATA_Area)[ncol(DATA_Area)] != "Area"){
        stop("The last column of the DATA_Area must be named Area")
      }
      
      print("Calculating densities")
      #Select cell counts
      For_density <- RESULTS_TIBBLE %>% dplyr::select(-N_cells, -contains("PROP_"))
      
      #Join the cell counts with the area tibbles
      For_density <- left_join(For_density, DATA_Area, by = "Subject_Names")
      
      #Calculate densities
      Density_results <- as_tibble(For_density[c(2:(ncol(For_density)-1))] / For_density[[ncol(For_density)]])
      names(Density_results) <- str_c("Density_", names(Density_results))
      
      
      #Graph without clinical data
      plot(bind_cols(For_density[1], Density_results) %>% dplyr::select(Subject_Names, contains("Density_")) %>% pivot_longer(-Subject_Names) %>%
             ggplot(aes(x = Subject_Names, y = value)) + facet_wrap(~name, "free_y", ncol = 1, nrow = length(unique(DATA_neighborhoods$Neighborhood_assignment))) +
             geom_col(width = 0.5, color = "black") +
             cowplot::theme_cowplot() +
             scale_y_continuous("Density of neighborhood in sample")+
             scale_x_discrete("")+
             theme(axis.text.x = element_text(angle = -90,
                                              vjust = 0.5))
      )
      #Bind the results and return the tibble 
      return(bind_cols(RESULTS_TIBBLE, Density_results, For_density[ncol(For_density)]))
    }
    
    #If not required exit the function returning the common results
    else{
      #Exit the function with the final results
      return(RESULTS_TIBBLE)
    }
  },
  options = list(optimize = 3))

Neighborhood_voting_function <- cmpfun(
  function(N_cores = NULL,
           Tiled_Images = NULL,
           Minimum_cell_no_per_tile = NULL,
           Neighborhoods_included = NULL) {
    #Import data from a Tiled_Images list
    Tiled_Images <- Tiled_Images
    
    #Check arguments
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!all(Minimum_cell_no_per_tile >= 1 & Minimum_cell_no_per_tile%%1 == 0)) stop("Minimum_cell_no_per_tile must be an integer value > 0")
    #Check that the Neighborhoods_included are present in the data
    if(!all(Neighborhoods_included %in% unique(unlist(map(Tiled_Images, function(df) df[[2]]$Neighborhood_assignment))))) {
      stop(paste0("Neighborhoods_included included must be any of: ", str_c(unique(unlist(map(Tiled_Images, function(df) df[[2]]$Neighborhood_assignment))), collapse = ", ")))
    }
    
    
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    
    #Generate the tile info with the votes for each tile
    RESULTS <-
      furrr::future_map(1:length(Tiled_Images), function(Image) {
        #Generate the count of votes within each tile
        Interim <- Tiled_Images[[Image]][[2]]
        Interim <- Interim %>% dplyr::filter(Neighborhood_assignment %in% Neighborhoods_included) #Select only desired neighborhoods
        Tiles <- Interim %>% dplyr::count(tile_id) %>% dplyr::filter(n >= Minimum_cell_no_per_tile) %>% dplyr::select(tile_id) #Filter tiles without enough voters
        Votes <- Interim %>% dplyr::filter(tile_id %in% Tiles[[1]]) %>% group_by(tile_id, Neighborhood_assignment) %>% dplyr::count() %>%
          pivot_wider(names_from = Neighborhood_assignment, values_from = n) #Count the votes per tile
        Votes[is.na(Votes)] <- 0
        Votes_prop <- Votes[-1] / apply(Votes[-1], MARGIN = 1, function(x) sum(x)) #Calculate the proportion of votes supporting each neighborhood
        names(Votes_prop) <- str_c("PROP_", names(Votes_prop)) 
        Votes$N_votes <- apply(Votes[-1], MARGIN = 1, function(x) sum(x)) #Count the total voter per tile
        
        Votes <- bind_cols(Votes, Votes_prop) #Bind absolute votes information with proportion votes
        
        Tiles_with_votes <- left_join(Tiled_Images[[Image]][[1]], Votes, by = "tile_id") %>% na.omit() #Bind tile info with election results
        
        #Count vote percentages and find the winner by tile
        Vote_count <- Tiles_with_votes %>% dplyr::select(contains("PROP"))
        
        #If valid vote count (Vote counts is at least one row in length) proceed with calculating the winner
        if(nrow(Vote_count) > 0) {
          #Generate a function that calculates the winner, 2nd and third candidates in each tile
          Election_results <- map_dfr(seq_along(1:nrow(Vote_count)), function(Tile_no) {
            Election_results <- Vote_count[Tile_no, ] %>% pivot_longer(1:ncol(Vote_count)) %>% arrange(desc(value)) %>% 
              mutate(name = substr(name, start = 6, stop = nchar(name))) %>%
              mutate(value = case_when(value == 0 ~ NA,
                                       TRUE ~ value),
                     name = case_when(is.na(value) ~ NA,
                                      TRUE ~ name))
            #Specify the results if absolute winner (only one result returned), super winner or simple winner (more than one result returned)
            if(nrow(Election_results) == 1) {
              Election_results[2,] <- NA
              Election_results[3,] <- NA
            }
            else if(nrow(Election_results == 2)) {
              Election_results[3,] <- NA
            }
            Election_results_tibble <- tibble(Winner = Election_results[[1,1]],
                                              Second = Election_results[[2,1]],
                                              Third = Election_results[[3,1]]) %>%
              mutate(Majority_type = case_when(Election_results[[1,2]] == 1 ~ "ABSOLUTE",
                                               Election_results[[1,2]] >= 0.5 & Election_results[[1,2]] <1 ~ "SUPER",
                                               Election_results[[1,2]] < 0.5 ~ "SIMPLE"))
          })
        }
        
        #if invalid then the result is NA
        else{
          Election_results <- tibble(Winner = NA,
                                     Second = NA,
                                     Third = NA,
                                     Majority_type = NA)
        }
        
        #Combine the results
        return(bind_cols(Tiles_with_votes, Election_results))
      },
      .progress = TRUE)
    future::plan("future::sequential")
    gc()
    
    names(RESULTS) <- names(Tiled_Images)
    
    Summary_tibble <- map_dfr(seq_along(1:length(RESULTS)), function(Image) {
      Subject_Names <- names(RESULTS)[Image]
      N_tiles <- nrow(RESULTS[[Image]])
      Counts <- RESULTS[[Image]] %>% dplyr::count(Winner) %>% pivot_wider(names_from = Winner, values_from = n)
      Prop_counts <- Counts / N_tiles
      names(Prop_counts) <- str_c("PROP_", names(Prop_counts))
      Final_tibble <- tibble(Subject_Names = Subject_Names)
      Final_tibble2 <- tibble(N_tiles = N_tiles)
      
      bind_cols(Final_tibble, Counts, Final_tibble2, Prop_counts)
      
    })
    
    Summary_tibble[is.na(Summary_tibble)] <- 0
    
    PROPORTIONS <- Summary_tibble %>% dplyr::select(dplyr::contains("PROP_"))
    
    OTHER <- Summary_tibble %>% dplyr::select(-dplyr::contains("PROP_"), -Subject_Names, -N_tiles)
    
    Winner_summary <- bind_cols(Summary_tibble %>% dplyr::select(Subject_Names, N_tiles), OTHER, PROPORTIONS)
    
    return(list(Images = RESULTS,
                Winner_summary = Winner_summary
    )
    )
    
    future::plan("future::sequential")
    gc()
  },
  options = list(optimize = 3))

Tiled_neighborhoods_graphicator <- cmpfun(
  function(DATA_elections = NULL,
           Image_name = NULL,
           Graph_only_winner_neighborhood = NULL) {
    #Check arguments
    if(!is.logical(Graph_only_winner_neighborhood)) stop("Graph_only_winner_neighborhood must be a logical value")
    if(!Image_name %in% names(DATA_elections$Images)) stop("Image_name not present in DATA_elections")
    
    #Select individual image to graph
    Image_to_graph <- DATA_elections$Images[[Image_name]]
    
    #Graph all available neighborhoods
    if(!Graph_only_winner_neighborhood) {
      
      Image_to_graph %>% dplyr::select(dplyr::contains("tile_"), dplyr::contains("PROP")) %>%
        pivot_longer(dplyr::contains("PROP")) %>%
        ggplot() + geom_rect(aes(group = tile_id, xmin = tile_xmin, ymin = tile_ymin, xmax = tile_xmax, ymax = tile_ymax, fill = value), color = "black") +
        facet_wrap(~name) + theme_minimal() + theme(panel.grid = element_blank()) +
        scale_fill_gradient(low = "grey", high = "red") 
    }
    
    else if(Graph_only_winner_neighborhood) {
      Image_to_graph %>%
        ggplot() + 
        geom_rect(aes(group = tile_id, xmin = tile_xmin, ymin = tile_ymin, xmax = tile_xmax, ymax = tile_ymax, fill = Winner), color = NA) + 
        theme_minimal() +
        scale_fill_manual("Winner neighborhood", values = unname(pals::polychrome(length(unique(Image_to_graph$Winner))))) +
        theme(panel.grid = element_blank()) 
    }
    
  },
  options = list(optimize = 3))

Tiled_Image_Clustering_function <- cmpfun(
  function(Tiled_images = NULL, 
           Minimum_cell_no_per_tile = NULL, 
           Minimum_valid_tiles_per_image = NULL,
           Phenotypes_included = NULL,
           
           #Type of data to cluster
           Cluster_Data = NULL,
           
           #Dimension reduction
           Perform_Dimension_reduction = NULL,
           Dimension_reduction = NULL,
           Dimension_reduction_prop = NULL,
           Cluster_on_Reduced = NULL,
           
           #Clustering strategy
           Strategy = NULL, 
           
           #Parameters for Consensus Clustering
           Max_N_Clusters = NULL, 
           Consensus_reps = NULL, 
           Consensus_p_Items = NULL, 
           Consensus_Cluster_Alg = NULL, 
           Consensus_Distance = NULL, 
           Consensus_Name = NULL,
           
           #Parameters for Self-Organizing Maps
           Max_SOM_Clusters = NULL, #Maximum number of clusters (neighborhoods) to try in the algorithm
           
           #Parameters for Graph methods
           Graph_type = NULL,
           Graph_Method = NULL,
           Nearest_neighbors_for_graph = NULL,
           Graph_Resolution = NULL,
           Graph_Distance_method = NULL,
           N_steps = NULL,
           
           #Parameters for K means Meta Clustering
           N_K_centroids = NULL, #Number of centroids to perform K means
           Max_N_Clusters_Meta = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Consensus_reps_Meta = NULL, #Number of iterations of the algorithm to try to converge
           Consensus_p_Items_Meta = NULL, #Percentage of cells that you desire to sample in each iteration
           Consensus_Name_Meta = NULL, #Name of the folder that is going to be created in order to place the resulting graphs
           
           #Parameters for Batched K means
           Batch_size = NULL, #The number of cells to be included in each random batch
           Max_N_Clusters_Batch = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Percentage_centroid_initiation = NULL,
           N_initiations = NULL, #Number of times the algorithm is going to be tried to find the best clustering result
           Max_iterations = NULL, #Max number of iterations in each try
           
           #Parameters for Gaussian Mixture Model
           Quality_metric = NULL, #The quality measure used to test the number of clusters ("AIC" or "BIC")
           Max_N_Clusters_GMM = NULL, #Number of maximum clusters (phenotypes) that you desire to find
           Max_iterations_km = NULL, #Number of max iterations in the K means clustering performed
           Max_iterations_em = NULL, #Number of max iterations in the Expectation Maximization algorithm
           GMM_Distance = NULL, #Distance metric to use in the model ("eucl_dist" or "maha_dist")
           
           #Parameters for CLARA clustering
           Samples_CLARA = NULL, #Number of samples the CLARA algorithm is going to use to be calculated
           Sample_per_CLARA = NULL, #Percentage (from 0 to 1) of the total cells that are going to be allocated to each sample
           Max_N_Clusters_CLARA = NULL, #Number of maximum clusters (neighborhoods) that you desire to find
           Distance_CLARA = NULL, #euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, 
           #simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine
           N_cores = NULL #Number of cores to parallelize your computation
  ) {
    #Check arguments
    if(!all(Phenotypes_included %in% unique(unlist(map(Tiled_images, function(df) df[[2]]$Phenotype))))) {
      stop(paste0("Phenotypes included must be any of: ", str_c(unique(unlist(map(Tiled_images, function(df) df[[2]]$Phenotype))), collapse = ", ")))
    }
    if(!all(Minimum_cell_no_per_tile >= 1 & Minimum_cell_no_per_tile%%1 == 0)) stop("Minimum_cell_no_per_tile must be a integer value > 0")
    if(!all(Minimum_valid_tiles_per_image >= 1 & Minimum_valid_tiles_per_image%%1 == 0)) stop("Minimum_valid_tiles_per_image must be a integer value > 0")
    if(!Cluster_Data %in% c("Cell_Density", "Cell_Percentage")) stop("Cluster_Data must be any of the following: Cell_Density, Cell_Percentage")
    if(!Strategy %in% c("Consensus_Clustering", "SOM", "Graph_Based", "K_Means_Meta_clustering", "Batch_K_means", "GMM", "CLARA_clustering")){
      stop("Strategy must be any of the following: Consensus_Clustering, SOM, Graph_Based, K_Means_Meta_clustering, Batch_K_means, GMM, CLARA_clustering")
    } 
    if(!is.logical(Perform_Dimension_reduction)) stop("Perform_Dimension_reduction must be a logical value")
    if(Perform_Dimension_reduction){
      if(!Dimension_reduction %in% c("UMAP", "TSNE", "PCA")) stop("Dimension_reduction must be one of the following: UMAP, TSNE, PCA")
      if(!all(is.numeric(Dimension_reduction_prop), Dimension_reduction_prop > 0, Dimension_reduction_prop <= 1)) stop("Dimension_reduction_prop must be a numeric value between 0 and 1")
    }
    if(!is.logical(Cluster_on_Reduced)) stop("Cluster_on_Reduced must be a logical value")
    if(Cluster_on_Reduced){
      if(!Perform_Dimension_reduction) stop("If Clustering needst o be performed on Dimension reduced data please set Perform_Dimension_reduction to TRUE")
    }
    #Ceck arguments for Consensus Clustering
    if(Strategy == "Consensus_Clustering"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Max_N_Clusters_OK = (Max_N_Clusters >= 2 & Max_N_Clusters%%1 == 0),
                            Consensus_reps_OK = (Consensus_reps >= 1 & Consensus_reps%%1 == 0),
                            Consensus_p_Items_OK = (Consensus_p_Items > 0 & Consensus_p_Items <= 1),
                            Consensus_Cluster_Alg_OK = Consensus_Cluster_Alg %in% c("hc", "pam", "km"),
                            Consensus_Distance_OK = Consensus_Distance %in% c("pearson", "spearman", "euclidean", "binary", "maximum", "canberra", "minkowski"),
                            Consensus_Name_OK = is.character(as.character(Consensus_Name))
      )
      Stop_messages <- c(Max_N_Clusters_OK = "Max_N_Clusters must be an integer value > 1",
                         Consensus_reps_OK = "Consensus_reps_OK must be an integer value > 0",
                         Consensus_p_Items_OK = "Consensus_p_Items must be a numeric value > 0 and lower than 1",
                         Consensus_Cluster_Alg_OK = "Consensus_Cluster_Alg must be one of the following: hc, pam, km",
                         Consensus_Distance_OK = "Consensus_Distance must be one the following: pearson, spearman, euclidean, binary, maximum, canberra, minkowski",
                         Consensus_Name_OK = "Consensus_Name must ve a character value")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for Self Organizing maps
    if(Strategy == "SOM"){
      #Check arguments
      if(!(Max_SOM_Clusters > 1 & Max_SOM_Clusters%%1 == 0)) stop("Max_SOM_Clusters must be an integer value > 1")
    }
    #Check arguments for Graph-Based clustering
    if(Strategy == "Graph_Based"){
      #Check arguments by generating a argument check vector and message vector
      Argument_checker <- c(Graph_type_OK = Graph_type %in% c("Complete", "SNN"),
                            Graph_Distance_method_OK = (Graph_Distance_method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                            Graph_Method_OK = Graph_Method %in% c("Louvain", "Leiden", "Greedy", "WalkTrap", "Spinglass", "Leading_Eigen", "Edge_Betweenness"),
                            Graph_Resolution_OK = all(is.numeric(Graph_Resolution), Graph_Resolution > 0),
                            N_steps_OK = any(is.null(N_steps), (N_steps >=1 & N_steps%%1 == 0))
      )
      Stop_messages <- c(Graph_type_OK = "Graph_type should be one of the following: Complete, SNN",
                         Nearest_neighbors_for_graph = "Nearest_neighbors_for_graph must be an integer value > 0",
                         Graph_Method = "Graph_Method must be one of the following: Louvain, Leiden, Greedy, WalkTrap, Spinglass, Leading_Eigen, Edge_Betweenness",
                         Graph_Resolution = "Graph_Resolution must be a numeric value > 0",
                         N_steps = "N_steps must be a integer value > 0")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
      #Check specific argument of SNN graphs
      if(Graph_type != "Complete"){
        if(!all(Nearest_neighbors_for_graph%%1 == 0, Nearest_neighbors_for_graph > 0)) stop("Nearest_neighbors_for_graph should be a integer value > 0")
      }
    }
    #Check arguments for K means meta clustering
    if(Strategy == "K_Means_Meta_clustering"){
      #Check arguments
      Argument_checker <- c(Max_N_Clusters_Meta_OK = (Max_N_Clusters_Meta >= 2 & Max_N_Clusters_Meta%%1 == 0),
                            Consensus_reps_Meta_OK = (Consensus_reps_Meta >= 1 & Consensus_reps_Meta%%1 == 0),
                            Consensus_p_Items_Meta_OK = (Consensus_p_Items_Meta > 0 & Consensus_p_Items_Meta <= 1),
                            Consensus_Name_Meta_OK = is.character(as.character(Consensus_Name_Meta))
      )
      Stop_messages <- c(Max_N_Clusters_Meta_OK = "Max_N_Clusters_Meta must be an integer value > 1",
                         Consensus_reps_Meta_OK = "Consensus_reps_Meta must be an integer value > 0",
                         Consensus_p_Items_Meta_OK = "Consensus_p_Items_Meta must be a numeric value > 0 and lower than 1",
                         Consensus_Name_Meta_OK = "Consensus_Name_Meta must ve a character value"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for Batched K means
    if(Strategy == "Batch_K_means"){
      #Check arguments
      Argument_checker <- c(Max_N_Clusters_Batch_OK = (Max_N_Clusters_Batch >= 2 & Max_N_Clusters_Batch%%1 == 0),
                            N_initiations_OK = (N_initiations >= 1 & N_initiations%%1 == 0),
                            Max_iterations_OK = (Max_iterations%%1 == 0)
      )
      Stop_messages <- c(Max_N_phenotypes_Batch_OK = "Max_N_Clusters_Batch must be an integer value > 1",
                         N_initiations_OK = "N_initiations must be an integer value > 0",
                         Max_iterations_OK = "Max_iterations must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for Gaussian mixture models
    if(Strategy == "GMM"){
      #Check arguments
      Argument_checker <- c(Quality_metric_OK = Quality_metric %in% c("AIC", "BIC"),
                            Max_N_Clusters_GMM_OK = (Max_N_Clusters_GMM >= 2 & Max_N_Clusters_GMM%%1 == 0),
                            Max_iterations_km_OK = (Max_iterations_km >= 1 & Max_iterations_km%%1 == 0),
                            Max_iterations_em_OK = (Max_iterations_em >= 1 & Max_iterations_em%%1 == 0),
                            GMM_Distance_OK = GMM_Distance %in% c("eucl_dist", "maha_dist")
      )
      Stop_messages <- c(Quality_metric_OK = "Quality_metric must be one of the following: AIC, BIC",
                         Max_N_phenotypes_GMM_OK = "Max_N_phenotypes must be an integer value > 1",
                         Max_iterations_km_OK = "Max_iterations_km must be an integer value > 1",
                         Max_iterations_em_OK = "Max_iterations_em must be an integer value > 1",
                         GMM_Distance_OK = "GMM_Distance must be one of the following: eucl_dist, maha_dist"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    #Check arguments for CLARA clustering
    if(Strategy == "CLARA_clustering"){
      #Check arguments
      Argument_checker <- c(Samples_CLARA_OK = (Samples_CLARA >= 1 & Samples_CLARA%%1 == 0),
                            Sample_per_CLARA_OK = (Sample_per_CLARA > 0 & Sample_per_CLARA <= 1),
                            Max_N_Clusters_CLARA_OK = (Max_N_Clusters_CLARA >= 2 & Max_N_Clusters_CLARA%%1 == 0),
                            Distance_CLARA_OK = Distance_CLARA %in% c("euclidean", "manhattan", "chebyshev", "canberra", "braycurtis", 
                                                                      "pearson_correlation", "simple_matching_coefficient", "minkowski", 
                                                                      "hamming", "jaccard_coefficient", "Rao_coefficient", "mahalanobis", "cosine"),
                            N_cores_OK = (N_cores >= 1 & N_cores%%1 == 0)
      )
      Stop_messages <- c(Samples_CLARA_OK = "Samples_CLARA must be an integer value > 0",
                         Sample_per_CLARA_OK = "Sample_per_CLARA must be a numeric value between 0 and 1",
                         Max_N_Clusters_CLARA_OK = "Max_N_Clusters_CLARA must be an integer value > 1",
                         Distance_CLARA_OK = "Distance_CLARA must be one of the following: euclidean, manhattan, chebyshev, canberra, braycurtis, pearson_correlation, simple_matching_coefficient, minkowski, hamming, jaccard_coefficient, Rao_coefficient, mahalanobis, cosine",
                         N_cores_OK = "N_cores must be an integer value > 0"
      )
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    
    #Else proceed with analysis. First calculate the total cells and the percentage by tile
    Cell_counts_by_tile <- 
      map(Tiled_Images, function(x) {
        Interim <- x[[2]] %>% dplyr::filter(Phenotype %in% Phenotypes_included)
        
        Filtered_tiles <-
          Interim  %>%
          group_by(tile_id) %>% dplyr::count() %>% ungroup() %>% dplyr::filter(n >= Minimum_cell_no_per_tile) #Filter out tiles with less than minimum cells per tile
        
        
        #Build cell count by tile matrix
        Interim2 <-
          Interim %>% dplyr::filter(tile_id %in% Filtered_tiles[[1]]) %>% group_by(tile_id, Phenotype) %>% dplyr::count() %>%
          pivot_wider(id_cols = tile_id,
                      names_from = Phenotype,
                      values_from = n)
        Interim2[is.na(Interim2)] <- 0
        
        #Calculate the percentage of the total cells in the tile that belong to each phenotype and build a tibble with the info
        Interim_per <- map_dfc(Interim2[-1], function(Column) Column / rowSums(Interim2[-1]))
        names(Interim_per) <- str_c("PER_", names(Interim2)[-1], sep = "")
        
        #Calculate the total cells per tile
        Total_cells_tibble <- tibble(n_cells = rowSums(Interim2[-1]))
        
        #Obtain the results tibble
        Results <- bind_cols(
          Interim2[1],
          Interim2[-1],
          Total_cells_tibble,
          Interim_per
        )
        
        #Bind results to the tile info matrix and eliminate rows with NA
        return(left_join(x[[1]], Results, by = "tile_id") %>% na.omit())
        
      }, .progress = list(clear = F,
                          name = "Counting cells in each tile",
                          show_after = 1,
                          type = "iterator"))
    
    #Check how many images in the experiment have passed the filtering steps before proceeding
    #First stop the function if no images have survived the thresholds
    if(all(map_dbl(Cell_counts_by_tile, nrow) < Minimum_valid_tiles_per_image)){
      stop("No images with adequate number of evaluable tiles. Please refine tiling strategy, or the following parameters: Minimum_cell_no_per_tile or Minimum_valid_tiles_per_image")
    }
    
    #If at least some images have survived the threshold then ask the user if they want to proceed 
    if(any(map_dbl(Cell_counts_by_tile, nrow) < Minimum_valid_tiles_per_image)){
      User_choice <- menu(choices = c("Proceed", "Stop"), title = paste0(as.character(sum(map_dbl(Cell_counts_by_tile, nrow) < Minimum_valid_tiles_per_image)),
                                                                         " out of ",
                                                                         as.character(length(Cell_counts_by_tile)),
                                                                         " images do not have enough evaluable tiles. Do you want to proceed with the analysis"
      )
      )
      if(User_choice == 2) {stop("Please refine tiling strategy, or the following parameters: Minimum_cell_no_per_tile or Minimum_valid_tiles_per_image")}
    }
    
    #If the user decides to proceed and there are images that have to be removed from the analysis print a warning message accordingly and remove the invalid images
    if(any(map_dbl(Cell_counts_by_tile, nrow) < Minimum_valid_tiles_per_image)) {
      warning(paste0("The following images will be removed from the analysis: ", 
                     str_c(names(Cell_counts_by_tile[map_dbl(Cell_counts_by_tile, nrow) < Minimum_valid_tiles_per_image]), collapse = ", ")
      )
      )
      Cell_counts_by_tile <- Cell_counts_by_tile[map_dbl(Cell_counts_by_tile, nrow) >= Minimum_valid_tiles_per_image]
    }
    
    #Now we prepare the matrix we are going to submit to clustering
    Aggregated_tile_tibble <- map_dfr(1:length(Cell_counts_by_tile), function(Image){
      #First filter the desired columns of the data
      if(Cluster_Data == "Cell_Density") {
        Results <- bind_cols(Cell_counts_by_tile[[Image]][1:7], 
                             Cell_counts_by_tile[[Image]] %>% dplyr::select(-c(1:7), -n_cells) %>% dplyr::select(-contains("PER_"))
        )
      }
      else if(Cluster_Data == "Cell_Percentage") {
        Results <- bind_cols(Cell_counts_by_tile[[Image]][1:7], 
                             Cell_counts_by_tile[[Image]] %>% dplyr::select(-c(1:7), -n_cells) %>% dplyr::select(contains("PER_"))
        )
        Results[-c(1:7)] <- round(Results[-c(1:7)], digits = 3)
      }
      
      #Now add Subject_Names, reorder columns and return
      Results$Subject_Names <- names(Cell_counts_by_tile)[Image]
      Results <- Results[c(ncol(Results), 1:7, 8:(ncol(Results)-1))]
      return(Results)
    }, .progress = list(clear = F,
                        name = "Preparing matrix for clustering",
                        show_after = 1,
                        type = "iterator"))
    
    #Replace NA values for 0
    Aggregated_tile_tibble[is.na(Aggregated_tile_tibble)] <- 0
    #Generate final data matrix and it's scaled variant
    Tile_patterns <- Aggregated_tile_tibble %>% dplyr::select(-c(1:8)) 
    Tile_patterns_scaled <- Tile_patterns %>% scale()
    
    #Perform dimension reduction if required
    if(Perform_Dimension_reduction){
      #First PCA 
      if(Dimension_reduction == "PCA"){
        if(Dimension_reduction_prop != 1) stop("PCA must be performed using Dimension_reduction_prop = 1")
        print("Generating PCA projections")
        #Scale and turn into matrix
        DATA_matrix <- Tile_patterns_scaled %>% as.matrix()
        Result_PCA <- svd::propack.svd(DATA_matrix, neig = 2)$u
        DATA_Reduction <- tibble(DIMENSION_1 = unlist(Result_PCA[,1]), DIMENSION_2 = unlist(Result_PCA[,2]))
      }
      
      #Second TSNE
      if(Dimension_reduction == "TSNE"){
        if(Dimension_reduction_prop == 1) {
          print("Generating TSNE projections")
          if(nrow(Tile_patterns_scaled) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          DATA_matrix <- Tile_patterns_scaled %>% as.matrix()
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = TRUE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Result_TSNE[,1]), DIMENSION_2 = unlist(Result_TSNE[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating TSNE projections")
          DATA_matrix <- Tile_patterns_scaled %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. tSNE embedding can take a long time")
          #scale and turn into matrix
          Result_TSNE <- snifter::fitsne(DATA_matrix,
                                         simplified = FALSE,
                                         n_components = 2L,
                                         n_jobs = 1L,
                                         perplexity = 30,
                                         n_iter = 500L,
                                         initialization = "pca",
                                         pca = FALSE,
                                         neighbors = "auto",
                                         negative_gradient_method = "fft",
                                         learning_rate = "auto",
                                         early_exaggeration = 12,
                                         early_exaggeration_iter = 250L,
                                         exaggeration = NULL,
                                         dof = 1,
                                         theta = 0.5,
                                         n_interpolation_points = 3L,
                                         min_num_intervals = 50L,
                                         ints_in_interval = 1,
                                         metric = "euclidean",
                                         metric_params = NULL,
                                         initial_momentum = 0.5,
                                         final_momentum = 0.8,
                                         max_grad_norm = NULL,
                                         random_state = NULL,
                                         verbose = FALSE)
          Coords <- snifter::project(Result_TSNE, 
                                     new = Tile_patterns_scaled %>% as.matrix(), 
                                     old = DATA_matrix)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
      
      #Third UMAP
      if(Dimension_reduction == "UMAP"){
        if(Dimension_reduction_prop == 1) {
          print("Generating UMAP projections")
          if(nrow(Tile_patterns_scaled) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take some time")
          #scale and turn into matrix
          DATA_matrix <- Tile_patterns_scaled %>% as.matrix()
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Result_UMAP[,1]), DIMENSION_2 = unlist(Result_UMAP[,2]))
        }
        
        if(Dimension_reduction_prop != 1) {
          print("Generating UMAP projections")
          DATA_matrix <- Tile_patterns_scaled %>% dplyr::sample_frac(size = Dimension_reduction_prop) %>% as.matrix()
          if(nrow(DATA_matrix) > 50000) print("Warning! Data set contains more than 50K observations. UMAP embedding can take aome time")
          #scale and turn into matrix
          Result_UMAP <- uwot::tumap(DATA_matrix, n_components = 2L, ret_model = TRUE)
          Coords <- uwot::umap_transform(X = Tile_patterns_scaled %>% as.matrix(), 
                                         model = Result_UMAP)
          DATA_Reduction <- bind_cols(DIMENSION_1 = unlist(Coords[,1]), DIMENSION_2 = unlist(Coords[,2]))
        }
      }
    }
    
    #If clustering on dimension reduction is required
    if(Cluster_on_Reduced){
      #Depending on Denoising Obtain directly from DATA_Reduction or filter first
      Tile_patterns_scaled <- DATA_Reduction
    }
    
    #Perform the actual clustering
    #First define what to do if consensus clustering is required
    if(Strategy == "Consensus_Clustering"){
      #Perform consensus clustering
      Clustering_result <- try(ConsensusClusterPlus::ConsensusClusterPlus(t(as.matrix(Tile_patterns_scaled)), 
                                                                          maxK = Max_N_Clusters, 
                                                                          reps = Consensus_reps,
                                                                          pItem = Consensus_p_Items,
                                                                          pFeature = 1,
                                                                          title = Consensus_Name,
                                                                          clusterAlg = Consensus_Cluster_Alg,
                                                                          distance = Consensus_Distance,
                                                                          plot = "png",
                                                                          verbose = T))
      if(berryFunctions::is.error(Clustering_result)) stop("Consensus Clustering Failed. Your data is probably too large for this method. Please try another strategy")
      
      
      #Make the user decide the number of neighborhoods according to results
      N_Clusters <- menu(choices = as.character(1:Max_N_Clusters), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Clusters"))
      Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = Clustering_result[[as.double(N_Clusters)]][["consensusClass"]])
    }
    #Define what to do if SOM is required
    if(Strategy == "SOM"){
      print("Executing Self Organizing Map algorithm")
      #Transform data into a scaled matrix and perform Self Organizing Map
      SOM_results <- try(FlowSOM::FlowSOM(as.matrix(Tile_patterns_scaled),
                                          scale = F,
                                          colsToUse = 1:ncol(Tile_patterns_scaled),
                                          maxMeta = Max_SOM_Clusters, #To find optimal meta clusters
                                          silent = F,
                                          seed = 21)
      )
      #Test if SOM returned an error
      if(berryFunctions::is.error(SOM_results)) {
        stop("Data is too large for Self-Organizing Maps. Please try another strategy")
      }
      else{
        #Assign phenotypes to each cell
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = FlowSOM::GetMetaclusters(SOM_results))
      }
    }
    #Then define what to do if graph based clustering is required
    if(Strategy == "Graph_Based"){
      #Generate graphs according to user defined preferences
      if(Graph_type == "Complete"){
        print("Generating the complete graph")
        #Calculate distance matrix and then calculate the graph
        #We define the number and ID of edges of the graph
        Graph_tibble <- as_tibble(expand.grid.unique(1:nrow(Tile_patterns_scaled), 1:nrow(Tile_patterns_scaled)))
        names(Graph_tibble) <- c("from", "to")
        Graph_tibble <- Graph_tibble %>% mutate(ID = str_c(from, to, sep = "_"))
        
        #We determine the distance between nodes that will be the features of the edges
        DISTANCE_MATRIX <- as_tibble(as.matrix(dist(Tile_patterns_scaled, method = Graph_Distance_method)))
        DISTANCE_MATRIX <- DISTANCE_MATRIX %>% mutate(from = as.character(1:nrow(DISTANCE_MATRIX)))
        DISTANCE_MATRIX <- DISTANCE_MATRIX[c(ncol(DISTANCE_MATRIX), 2:(ncol(DISTANCE_MATRIX)-1))] %>% pivot_longer(-1, names_to = "to", values_to = "weight") %>%
          mutate(ID = str_c(from, to, sep = "_")) %>% dplyr::select(-from, -to)
        
        #We bind the edges to their features (distance) and we build the graph
        GRAPH_DF <- left_join(Graph_tibble, DISTANCE_MATRIX, by = "ID") %>% dplyr::select(-ID)
        GRAPH_DF <- GRAPH_DF %>% mutate(weight = 1/weight)
        Neighborhood_ID <- tibble(Name = as.character(1:nrow(Tile_patterns_scaled)))
        Neighborhood_pattern_graph <- igraph::graph_from_data_frame(GRAPH_DF, directed = F, vertices = Neighborhood_ID)
      }
      if(Graph_type == "SNN"){
        print("Generating the SNN graph")
        #Transform data into a nearest neighbor graph 
        Neighborhood_pattern_graph <- try(bluster::makeSNNGraph(as.matrix(Tile_patterns_scaled), 
                                                                k = Nearest_neighbors_for_graph)
        )
        
        #Test if Graph construction process returned an error
        if(berryFunctions::is.error(Neighborhood_pattern_graph)) {
          stop("Data is too large to build a graph. Please try another strategy")
        }
      }
      
      print("Performing graph-based clustering")
      #Cluster the graph with louvain or leiden clustering
      if(Graph_Method == "Louvain") {
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_louvain(Neighborhood_pattern_graph,
                                                                                               weights = NULL,
                                                                                               resolution = Graph_Resolution)$membership)
      }
      
      else if(Graph_Method == "Leiden") {
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_leiden(Neighborhood_pattern_graph,
                                                                                              objective_function = "modularity",
                                                                                              weights = NULL,
                                                                                              resolution = Graph_Resolution,
                                                                                              beta = 0.01,
                                                                                              initial_membership = NULL,
                                                                                              n_iterations = 100,
                                                                                              vertex_weights = NULL)$membership)
      }
      
      else if(Graph_Method == "Optimal"){
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_optimal(Neighborhood_pattern_graph)$membership)
      }
      
      else if(Graph_Method == "Greedy"){
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_fast_greedy(Neighborhood_pattern_graph)$membership)
      }
      
      else if(Graph_Method == "WalkTrap"){
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_walktrap(Neighborhood_pattern_graph,
                                                                                                steps = N_steps,
                                                                                                membership = T)$membership)
      }
      
      else if (Graph_Method == "Spinglass") {
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_spinglass(Neighborhood_pattern_graph,
                                                                                                 weights = NULL,
                                                                                                 vertex = NULL,
                                                                                                 spins = 25,
                                                                                                 parupdate = FALSE,
                                                                                                 start.temp = 1,
                                                                                                 stop.temp = 0.01,
                                                                                                 cool.fact = 0.99,
                                                                                                 update.rule = c("config", "random", "simple"),
                                                                                                 gamma = 1,
                                                                                                 implementation = c("orig", "neg"),
                                                                                                 gamma.minus = 1)$membership)
      }
      
      else if(Graph_Method == "Leading_Eigen"){
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_leading_eigen(Neighborhood_pattern_graph,
                                                                                                     membership = T)$membership)
      }
      
      else if(Graph_Method == "Edge_Betweenness"){
        Tile_patterns <- Tile_patterns %>% mutate(Cluster_assignment = igraph::cluster_edge_betweenness(Neighborhood_pattern_graph,
                                                                                                        weights = NULL,
                                                                                                        directed = FALSE,
                                                                                                        edge.betweenness = FALSE,
                                                                                                        merges = FALSE,
                                                                                                        bridges = FALSE,
                                                                                                        modularity = FALSE,
                                                                                                        membership = TRUE)$membership)
      }
    }
    #Define what to do if K means meta clustering is required
    if(Strategy == "K_Means_Meta_clustering"){
      print("Performing initial K-means algorithm")
      if(N_K_centroids >= nrow(Tile_patterns_scaled)) stop(paste0("N_K_centroids should be smaller than: ", nrow(Tile_patterns_scaled)))
      
      #First we need to perform K means Clustering 
      cl <- try(kmeans(as.matrix(Tile_patterns_scaled), #Scale it and turn it into a matrix
                       centers = N_K_centroids, #Number of centroids to be calculated
                       iter.max = 50, 
                       nstart = 10))
      
      #Stop function if K means returned an error
      if(berryFunctions::is.error(cl)) stop("Data is too large for K means clustering. Please try another strategy")
      
      #Proceed if no error was returned
      else{
        #Assign this K means cluster to each observation
        DATA_filter_Markers <- Tile_patterns_scaled %>% mutate(K_means_Cl = cl$cluster)
        
        #Prepare data for Meta-Clustering
        #Create a tibble with the K means centroids and the format it for Consensus clustering
        K_medoids <- as_tibble(cl$centers) %>% mutate(K_means_Cl = 1:nrow(as_tibble(cl$centers)))
        tK_medoids <- K_medoids %>% dplyr::select(-K_means_Cl) %>% as.matrix %>% t
        
        print("Perorming Consensus Clustering")
        #Perform Consensus clustering with hierarchical clustering
        HC <- try(ConsensusClusterPlus::ConsensusClusterPlus(tK_medoids, 
                                                             maxK = Max_N_Clusters_Meta,   
                                                             reps = Consensus_reps_Meta, 
                                                             pItem = Consensus_p_Items_Meta,
                                                             pFeature = 1,
                                                             title = Consensus_Name_Meta,
                                                             distance = "euclidean",
                                                             clusterAlg = "pam",
                                                             plot = "png",
                                                             verbose = T))
        #Test if consensus clustering returned an error
        if(berryFunctions::is.error(HC)) {
          stop("Data is too large for Meta Clustering. Please try another strategy or select a smaller N_K_centroids value")
        }
        else {
          #Make the user decide the number of neighborhoods according to results
          N_Phenotypes<- menu(choices = as.character(1:Max_N_Clusters_Meta), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Clusters"))
          
          #Bind the final Cluster_assignment to the K medoids tibble
          K_medoids <- K_medoids %>% mutate(Cluster_assignment = HC[[as.double(N_Phenotypes)]][["consensusClass"]])
          K_medoids_for_join <- K_medoids %>% dplyr::select(K_means_Cl, Cluster_assignment)
          
          #Bind The DATA and the K_meoids to obtain the final matrix
          Tile_patterns <- left_join(DATA_filter_Markers, K_medoids_for_join, by = "K_means_Cl") %>% dplyr::select(-K_means_Cl)
        }
      } 
    }
    #Define what to do if Batch K means is required
    if(Strategy == "Batch_K_means"){
      #First we calculate a metric to decide the number of total phenotypes
      #Specify the params
      params_mbkm <- list(batch_size = Batch_size, 
                          init_fraction = 1, 
                          early_stop_iter = 10)
      print("Starting Cluster number stimation process")
      
      if(Batch_size >= nrow(Tile_patterns)) stop(paste0("Batch_size should be smaller than: ", nrow(Tile_patterns)))
      #Run the specified test using each of the number of clusters
      Optimal <- try(ClusterR::Optimal_Clusters_KMeans(Tile_patterns_scaled, 
                                                       max_clusters = Max_N_Clusters_Batch, 
                                                       num_init = N_initiations, 
                                                       max_iters = Max_iterations, 
                                                       initializer = "kmeans++",
                                                       criterion = "Adjusted_Rsquared",
                                                       plot_clusters = T, 
                                                       mini_batch_params = params_mbkm,
                                                       verbose = T)
      )
      
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_Clusters_Batch), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Clusters"))
        
        print("Performing Batched K means algorithm")
        #Calculate the desired number of clusters with batch k menas
        Batch_k_means <- ClusterR::MiniBatchKmeans(Tile_patterns_scaled,
                                                   clusters = as.double(N_Phenotypes), 
                                                   batch_size = Batch_size,
                                                   num_init = N_initiations,
                                                   max_iters = Max_iterations,
                                                   init_fraction = 1,
                                                   initializer = "kmeans++",
                                                   early_stop_iter = 10,
                                                   verbose = T,
                                                   tol = 1e-07, #The required improvement rate to continue with the iterations (the lower the more iterations will be required)
                                                   CENTROIDS = NULL,
                                                   seed = 21)
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = Batch_k_means, fuzzy = F, newdata = Tile_patterns_scaled) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Cluster_assignment"
        
        #Generate the data phenotypes tibble
        Tile_patterns <- bind_cols(Tile_patterns, pr_mb)
      }
    }
    #Define what to do if GMM is required
    if(Strategy == "GMM"){
      print("Starting Cluster number stimation process")
      #First we calculate a metric to decide the number of total phenotypes
      #Run the specified test using each of the number of clusters
      Optimal <- try(ClusterR::Optimal_Clusters_GMM(Tile_patterns_scaled, 
                                                    criterion = Quality_metric,
                                                    max_clusters = Max_N_Clusters_GMM, 
                                                    dist_mode = GMM_Distance,
                                                    seed_mode = "random_subset",
                                                    km_iter = Max_iterations_km,
                                                    em_iter = Max_iterations_em,
                                                    verbose = TRUE,
                                                    var_floor = 1e-10,
                                                    plot_data = TRUE)
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Proceed if all OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_Clusters_GMM), 
                            title = paste0("Look at the plot generated, Then decide the appropiate number of Phenotypes"))
        
        print("Calculating Gaussian Mixed Model")
        #Calculate the desired number of clusters with batch k menas
        GMM_model <- ClusterR::GMM(Tile_patterns_scaled,
                                   gaussian_comps = as.double(N_Phenotypes), 
                                   dist_mode = GMM_Distance,
                                   seed_mode = "random_subset",
                                   km_iter = Max_iterations_km,
                                   em_iter = Max_iterations_em,
                                   verbose = TRUE,
                                   var_floor = 1e-10,
                                   full_covariance_matrices = FALSE
        )
        
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = GMM_model, fuzzy = F, newdata = Tile_patterns_scaled) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Cluster_assignment"
        
        #Generate the data phenotypes tibble
        Tile_patterns <- bind_cols(Tile_patterns, pr_mb)
      }
    }
    #Define what to do if CLARA clustering is required
    if(Strategy == "CLARA_clustering"){
      print("Starting Cluster number stimation process")
      #First we calculate a metric to decide the number of total phenotypes
      Optimal <-  try(ClusterR::Optimal_Clusters_Medoids(Tile_patterns_scaled,
                                                         max_clusters = Max_N_Clusters_CLARA,
                                                         distance_metric = Distance_CLARA,
                                                         criterion = "silhouette" ,
                                                         clara_samples = Samples_CLARA,
                                                         clara_sample_size = Sample_per_CLARA,
                                                         swap_phase = F,
                                                         threads = N_cores,
                                                         verbose = T,
                                                         plot_clusters = T
      )
      )
      #Test if optimal number of clusters returned an error
      if(berryFunctions::is.error(Optimal)) {
        stop("Could not calculate best cluster number for the data provided. Please try another strategy")
      }
      #Continue if everything OK
      else{
        #Make the user decide the total number of clusters to be used in the final analysis
        N_Phenotypes<- menu(choices = as.character(1:Max_N_Clusters_CLARA), 
                            title = paste0("Based on the plots generated and you previous choice, decide the appropiate number of final Clusters"))
        
        print("Performing CLARA (Clustering Large Applications)")
        CLARA_Clustering <- ClusterR::Clara_Medoids(Tile_patterns_scaled,
                                                    clusters = as.double(N_Phenotypes), 
                                                    samples = Samples_CLARA,
                                                    sample_size = Sample_per_CLARA,
                                                    distance_metric = Distance_CLARA,
                                                    threads = N_cores,
                                                    swap_phase = F,
                                                    fuzzy = FALSE,
                                                    verbose = T,
                                                    seed = 21
        )
        #Assign the cluster to each observation of MARKER
        pr_mb <- predict(object = CLARA_Clustering, fuzzy = F, newdata = Tile_patterns_scaled) 
        pr_mb <- as_tibble(pr_mb)
        names(pr_mb) <- "Cluster_assignment"
        
        #Generate the data phenotypes tibble
        Tile_patterns <- bind_cols(Tile_patterns, pr_mb)
      }
    }
    
    print("Generating Plots")
    
    #Plot dimension reduction scatter point
    if(Perform_Dimension_reduction){
      #plot dimension reduction according to the number of cells
      if(nrow(DATA_Reduction) <= 100000){
        try(plot(
          DATA_Reduction %>% mutate(Cluster_assignment = Tile_patterns[["Cluster_assignment"]]) %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = as.factor(Cluster_assignment))) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Cluster_assignment", values = unname(pals::polychrome(length(unique(Tile_patterns$Cluster_assignment)))))
        )
        )
      }
      if(nrow(DATA_Reduction) > 100000){
        message(">100K observations to generate plots. A random subset containing 10% of the dataset will be selected for Dimension reduction plots")
        try(plot(
          DATA_Reduction %>% mutate(Cluster_assignment = Tile_patterns[["Cluster_assignment"]]) %>% 
            sample_n(size = 100000) %>%
            ggplot(aes(x = DIMENSION_1, y = DIMENSION_2, color = as.factor(Cluster_assignment))) +
            geom_point(size = 2, alpha = 0.95) +
            cowplot::theme_cowplot() +
            scale_color_manual("Cluster_assignment", values = unname(pals::polychrome(length(unique(Tile_patterns$Cluster_assignment)))))
        )
        )
      }
    }
    
    #Visualize the cluster composition data for each neighborhood
    plot(Tile_patterns %>% pivot_longer(cols = -Cluster_assignment) %>%
           ggplot(aes(x = as.factor(Cluster_assignment), y = value)) + 
           geom_violin(aes(color = name, fill = name), alpha=0.3, position=position_dodge(width=0.5)) +
           stat_summary(aes(color = name), 
                        fun = median, geom = "crossbar", width = 0.4, linetype = 1, linewidth = 0.5,
                        position = position_dodge(width = 0.5)) +
           cowplot::theme_cowplot()+
           scale_x_discrete("Cluster")+
           scale_y_continuous("Cells in Cluster"))
    
    #Visualize the heatmap of mean by cluster
    Mean_tibble <- Tile_patterns %>% group_by(Cluster_assignment) %>% dplyr::summarize_all(mean) %>% ungroup() #Obtain mean tibble
    Mean_matrix <- as.matrix(Mean_tibble[-1] %>% scale()) #Scale it and transform it into a  mtrix
    row.names(Mean_matrix) <- Mean_tibble[[1]] 
    
    plot(ComplexHeatmap::Heatmap(Mean_matrix,
                                 name = "Scaled")
    )
    
    #Generate the final tibble
    Final_result <- bind_cols(Aggregated_tile_tibble, Tile_patterns["Cluster_assignment"])
    
    #Print the Neighborhood quantification
    print(Final_result %>% dplyr::count(Cluster_assignment) %>% arrange(desc(n)))
    Final_result$Cluster_assignment <- as.factor(Final_result$Cluster_assignment)
    
    #Return a list with each of the tiled images
    Final_result_list <- map(unique(Final_result$Subject_Names), function(Image){
      Final_result %>% dplyr::filter(Subject_Names == Image)
    })
    names(Final_result_list) <- unique(Final_result$Subject_Names)
    return(Final_result_list)
  },
  options = list(optimize = 3))

Clustered_Tiled_Images_renamer <- cmpfun(
  function(Tiled_images = NULL,
           New_names = NULL) {
    
    #Check if provided names are equal to number of hoods
    if(length(New_names) != length(unique(unlist(map(Tiled_images, function(Image) Image$Cluster_assignment))))) {
      stop(paste0("Provided New_names should match the number of Neighborhoods in the analysis. Number of neighborhoods: ", 
                  length(unique(unlist(map(Tiled_images, function(Image) Image$Cluster_assignment)))),
                  ". Names provided: ", length(New_names)))
    }
    
    else{
      #Create a names tibble
      names_tibble <- tibble(Cluster_assignment = factor(1:length(unique(unlist(map(Tiled_images, function(Image) Image$Cluster_assignment))))),
                             New_names = New_names)
      
      map(Tiled_images, function(Image){
        left_join(Image, names_tibble, by = "Cluster_assignment") %>% mutate(Cluster_assignment = New_names) %>% dplyr::select(-New_names)
      })
    }
    
    
  },
  options = list(optimize = 3))

Clustered_Tiled_Images_analyzer <- 
  cmpfun(function(Tiled_images = NULL,
                  Perform_heterogeneity_analysis = NULL,
                  Graph_Modularity_resolution = NULL) {
    #Check arguments
    if(!all(map_lgl(Tiled_images, function(x) "Cluster_assignment" %in% names(x)))) stop("Tiled_images must be created using Tiled_Image_Clustering_function")
    if(!is.logical(Perform_heterogeneity_analysis)) stop("Perform_heterogeneity_analysis must be a logical value")
    if(Perform_heterogeneity_analysis){
      if(!all(is.numeric(Graph_Modularity_resolution), Graph_Modularity_resolution > 0)) stop("Graph_Modularity_resolution must be a numeric value > 0")
    }
    
    #Generate a tibble with the counts 
    RESULTS <- map_dfr(Tiled_images, function(Image){
      Count_DF <- Image %>% dplyr::count(Cluster_assignment) %>% pivot_wider(names_from = Cluster_assignment, values_from = n)
      Count_DF$n_tiles <- nrow(Image)
      return(Count_DF)
    }, .progress = list(clear = F,
                        name = "Counting cells in each tile",
                        show_after = 1,
                        type = "iterator"))
    
    #Substitute na for 0
    RESULTS[is.na(RESULTS)] <- 0
    
    #Reorder the tibble columns
    RESULTS <- RESULTS[c(which(names(RESULTS) == "n_tiles"), which(names(RESULTS) != "n_tiles"))]
    
    #Add subject names and reorder the tibble
    RESULTS$Subject_Names <- names(Tiled_images)
    RESULTS <- RESULTS[c(ncol(RESULTS), 1:(ncol(RESULTS)-1))]
    
    #Calculate the percentage of tiles belonging to each cluster
    Per_DF <- map_dfc(RESULTS[-c(1:2)], function(Column) Column/RESULTS$n_tiles)
    names(Per_DF) <- str_c("PER_", names(Per_DF))
    
    #If modularity is not calculated return the result
    if(!Perform_heterogeneity_analysis){
      return(bind_cols(RESULTS, Per_DF))
    }
    
    #If modularity needs to be calculated generate graph and calculate the modularity
    else if(Perform_heterogeneity_analysis){
      #We will calculate a graph for each image
      
      Modularity_scores <- 
        map_dbl(Tiled_images, function(Image){
          
          #We define the number and ID of edges of the graph
          Graph_tibble <- as_tibble(expand.grid.unique(Image$tile_id, Image$tile_id))
          names(Graph_tibble) <- c("from", "to")
          Graph_tibble <- Graph_tibble %>% mutate(ID = str_c(from, to, sep = "_"))
          
          #We determine the distance between nodes that will be the features of the edges
          DISTANCE_MATRIX <- as_tibble(as.matrix(dist(Image %>% dplyr::select(tile_X_centroid, tile_Y_centroid), method = "euclidean")))
          names(DISTANCE_MATRIX) <- Image$tile_id
          DISTANCE_MATRIX <- DISTANCE_MATRIX %>% mutate(from = Image$tile_id)
          DISTANCE_MATRIX <- DISTANCE_MATRIX[c(ncol(DISTANCE_MATRIX), 1:(ncol(DISTANCE_MATRIX)-1))]
          DISTANCE_MATRIX <- DISTANCE_MATRIX %>% pivot_longer(-1, names_to = "to", values_to = "weight") %>%
            mutate(ID = str_c(from, to, sep = "_")) %>% dplyr::select(-from, -to)
          
          #We join both tibbles to generate the final graph
          GRAPH_DF <- left_join(Graph_tibble, DISTANCE_MATRIX, by = "ID") %>% dplyr::select(-ID)
          GRAPH_DF <- GRAPH_DF %>% mutate(weight = 1/weight)
          
          #We add the tile clustering information
          NODE_ID <- Image %>% dplyr::select(tile_id, Cluster_assignment)
          
          #We generate the graph
          Tile_pattern_graph <- igraph::graph_from_data_frame(GRAPH_DF, directed = F, vertices = NODE_ID)
          
          #We calculate the graph modularity
          igraph::modularity(Tile_pattern_graph, membership = as.factor(NODE_ID$Cluster_assignment), directed = F, resolution = Graph_Modularity_resolution)
          
        }, .progress = list(clear = F,
                            name = "Calculating graph modularity",
                            show_after = 1,
                            type = "iterator"))
      #Generate a modularity tibble
      Modularity_tibble <- tibble(Subject_Names = names(Tiled_images), Graph_Modularity = Modularity_scores)
      
      
      #Generate an interim tibble to calculate other heterogeneity metrics
      Interim <- RESULTS %>% dplyr::select(-n_tiles)
      #Calculate heterogeneity metrics
      Heterogeneity_results <- 
        bind_cols(Interim[1], 
                  map_dfc(c("shannon" = "shannon", "simpson" = "simpson", "invsimpson" = "invsimpson"), 
                          function(Metric) vegan::diversity(Interim[-1], index = Metric)),
                  tibble(renyi = as.double(vegan::renyi(Interim[-1], scales = Inf))),
                  tibble(Rao_Dkk = picante::raoD(Interim[-1])$Dkk),
                  tibble(Gini = unlist(apply(Interim[-1], MARGIN = 1, function(Sample) DescTools::Gini(Sample, conf.level = NA))))
        )
      #change names
      names(Heterogeneity_results)[-1] <- c("Shannon", "Simpson", "Inverse_simpson", "Renyi_Scale_Inf", "Rao_Dkk", "Gini")
      
      #Bind both tibbles
      Heterogeneity_results <- left_join(Modularity_tibble, Heterogeneity_results, by = "Subject_Names")
      
      return(
        list(Tile_count_tibble = bind_cols(RESULTS, Per_DF),
             Heterogeneity_analysis = Heterogeneity_results)
      )
    }
  }, options = list(optimize = 3))

Clustered_Tiled_Images_graphicator <- 
  cmpfun(function(Tiled_images = NULL,
                  Image_name = NULL){
    #Check arguments
    if(!Image_name %in% names(Tiled_images)) stop(paste0(Image_name, " not found in Tiled_images"))
    
    Image_to_graph <- Tiled_images[[Image_name]]
    plot(Image_to_graph %>%
           ggplot() + 
           geom_rect(aes(group = tile_id, xmin = tile_xmin, ymin = tile_ymin, xmax = tile_xmax, ymax = tile_ymax, fill = Cluster_assignment), color = NA) + 
           theme_minimal() +
           scale_fill_manual("Cluster", values = unname(pals::polychrome(length(unique(Image_to_graph$Cluster_assignment))))) +
           theme(panel.grid = element_blank())
    ) 
  }, options = list(optimize = 3))

Interaction_counter <- cmpfun(
  function(DATA = NULL,
           Phenotypes_included = NULL,
           N_cores = NULL,
           
           Graph_type = NULL,
           K_number = NULL,
           Dist_threshold = NULL,
           
           Method = NULL,
           patch_size = NULL,
           
           Perform_significance_testing = NULL,
           N_iterations = NULL,
           p_threshold = NULL){
    #Check arguments
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    if(!all(is.character(DATA), exists(DATA, envir = .GlobalEnv))) stop("DATA must be the name of an existing object")
    #Import data
    DATA <- get(DATA, envir = .GlobalEnv)
    
    if(!all(Phenotypes_included %in% unique(DATA$Phenotype))){
      stop(paste0("Phenotypes provided must be one of: ", str_c(unique(DATA$Phenotype), collapse = ", ")))
    }
    if(!is.logical(Perform_significance_testing)) stop("Perform_significance_testing must be a logical value")
    if(!Graph_type %in% c("expansion", "knn", "delaunay")) stop("Graph_type must be one of the following: expansion, knn, delaunay")
    if(!all(is.numeric(K_number), K_number%%1 == 0, K_number >= 1)) stop("K_numer must be an integer value > 0")
    if(Graph_type == "expansion"){
      if(!all(is.numeric(Dist_threshold), Dist_threshold > 0)) stop("Dist_threshold must be a numeric value > 0")
    }
    if(!Method %in% c("classic", "histocat", "patch")) stop("Method must be one of the following: classic, histocat, patch")
    if(!all(is.numeric(patch_size), patch_size > 0)) stop("patch_size must be a numeric value > 0")
    if(Perform_significance_testing){
      if(!all(is.numeric(N_iterations), N_iterations%%1 == 0, N_iterations > 0)) stop("N_iterations must be a integer value > 0")
      if(!all(is.numeric(p_threshold), p_threshold <= 1, p_threshold > 0)) stop("p_threshold must be a numeric value between 0 and 1")
    }
    
    DATA <- DATA %>% dplyr::filter(Phenotype %in% Phenotypes_included)
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    #Generate the cluster
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    Final_list <-
      suppressMessages(
        furrr::future_map(unique(DATA$Subject_Names), function(Image){
        #Get the Image data
        Interim <- DATA %>% dplyr::filter(Subject_Names == Image)
        
        #First we create a SpatialExperiment object before we can proceed
        Spatial_object <- SpatialExperiment::SpatialExperiment(sample_id = Image,
                                                               colData = Interim,
                                                               spatialCoordsNames = names(Interim)[2:3]
        )
        
        #Now we build the graph to the calculate interactions
        Graph <- imcRtools::buildSpatialGraph(Spatial_object,
                                   img_id = "Subject_Names",
                                   type = Graph_type,
                                   directed = TRUE,
                                   threshold = Dist_threshold, 
                                   name = "spatialcontext_graph",
                                   k = K_number,
                                   coords = names(Interim)[2:3]
        )
        
        #Now we calculate the output
        
        if(!Perform_significance_testing){
          out <- imcRtools::countInteractions(
            Graph,
            group_by = "Subject_Names",
            label = "Phenotype",
            colPairName = "spatialcontext_graph",
            method =  Method,
            patch_size = patch_size
          )
          out <- as_tibble(out)
          return(out)
        }
        
        if(Perform_significance_testing){
          out <- 
            imcRtools::testInteractions(Graph,
                             group_by = "Subject_Names",
                             label = "Phenotype",
                             method = Method,
                             colPairName = "spatialcontext_graph",
                             BPPARAM = BiocParallel::SerialParam(RNGseed = 123),
                             iter = N_iterations,
                             p_threshold = p_threshold)
          
          out <- as_tibble(out)
          return(out)
        }
        
      },
      .progress = TRUE)
      )
      
    future::plan("future::sequential")
    gc()
    
    names(Final_list) <- unique(DATA$Subject_Names)
    return(Final_list)
  }, 
  options = list(optimize = 3))

Interaction_analyzer <- cmpfun(
  function(DATA = NULL,
           Exclude_non_significant = NULL,
           
           Cluster = NULL,
           Max_N_Clusters = NULL,
           Consensus_reps = NULL,
           Consensus_p_Items = NULL,
           Consensus_Cluster_Alg = NULL,
           Consensus_Distance = NULL,
           Consensus_Name = NULL){
    #Check arguments
    if(!all(map_lgl(DATA, function(x) all(c("from_label", "to_label") %in% names(x))))) stop("DATA must be created using the function Interaction_counter")
    if(!is.logical(Exclude_non_significant)) stop("Exclude_non_significant must be a logical value")
    if(!is.logical(Cluster)) stop("Cluster must be a logical value")
    if(Cluster){
      #Check arguments of consensus clustering
      Argument_checker <- c(Max_N_Clusters_OK = (Max_N_Clusters >= 2 & Max_N_Clusters%%1 == 0),
                            Consensus_reps_OK = (Consensus_reps >= 1 & Consensus_reps%%1 == 0),
                            Consensus_p_Items_OK = (Consensus_p_Items > 0 & Consensus_p_Items <= 1),
                            Consensus_Cluster_Alg_OK = Consensus_Cluster_Alg %in% c("hc", "pam", "km"),
                            Consensus_Distance_OK = Consensus_Distance %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                            Consensus_Name_OK = is.character(as.character(Consensus_Name))
      )
      Stop_messages <- c(Max_N_Clusters_OK = "Max_N_Clusters must be an integer value > 1",
                         Consensus_reps_OK = "Consensus_reps_OK must be an integer value > 0",
                         Consensus_p_Items_OK = "Consensus_p_Items must be a numeric value > 0 and lower than 1",
                         Consensus_Cluster_Alg_OK = "Consensus_Cluster_Alg must be one of the following: hc, pam, km",
                         Consensus_Distance_OK = "Consensus_Distance must be one the following: euclidean, maximum, manhattan, canberra, binary, minkowski",
                         Consensus_Name_OK = "Consensus_Name must ve a character value")
      #Check arguments and stop if necessary
      if(!all(Argument_checker)){
        stop(cat(Stop_messages[!Argument_checker],
                 fill = sum(!Argument_checker)))
      }
    }
    
    
    
    #If significance is not an issue proceed with analysis
    if(!Exclude_non_significant){
      
      print("Obtaining data")
      Interaction_DF <- map_dfr(DATA, function(Image){
        Image %>% dplyr::mutate(col_id = str_c(from_label, to_label, sep = "_")) %>% pivot_wider(id_cols = group_by, names_from = col_id, values_from = ct) %>%
          dplyr::rename("Subject_Names" = "group_by")
      })
      
      #If no clustering is required return the actual data
      if(!Cluster){
        return(Interaction_DF)
      }
      
      #If Clustering is required then proceed with clustering
      if(Cluster){
        print("Performing clustering")
        #We will remove the non-clusteable samples using a while loop
        Na_count <- sum(is.na(dist(Interaction_DF[-1], Consensus_Distance))) #Generate the entering condition
        while(Na_count >= 1){
          Na_tibble <- as_tibble(as.matrix(dist(Interaction_DF[-1], "euclidean", diag = T, upper = T)))
          warning(paste0("Some samples had insufficient data. The following samples will be removedd: ", 
                         str_c(Interaction_DF[[1]][
                           which(map_dbl(Na_tibble, function(Column) sum(is.na(Column))) == max(map_dbl(Na_tibble, function(Column) sum(is.na(Column)))))], 
                           collapse = ", "))) #Print a warning message everytime samples are removed
          Interaction_DF <- Interaction_DF[
            -which(map_dbl(Na_tibble, function(Column) sum(is.na(Column))) == max(map_dbl(Na_tibble, function(Column) sum(is.na(Column))))),] #Generate the output tibble
          Na_count <- sum(is.na(dist(Interaction_DF[-1], Consensus_Distance)))#Update the NA counter
        }
        
        #We need to calculate our own distance matrix to account for potential NA values
        for_dist <- Interaction_DF %>% dplyr::select(-Subject_Names) %>% scale()
        Distance_matrix <- dist(for_dist, Consensus_Distance)
        #Perform consensus clustering
        Clustering_result <- ConsensusClusterPlus::ConsensusClusterPlus(Distance_matrix, 
                                                                        maxK = Max_N_Clusters, 
                                                                        reps = Consensus_reps,
                                                                        pItem = Consensus_p_Items,
                                                                        pFeature = 1, #For a distance matrix we need to always use 100% of features
                                                                        title = Consensus_Name,
                                                                        clusterAlg = Consensus_Cluster_Alg,
                                                                        plot = "png",
                                                                        verbose = T)
        
        #Make the user decide the number of neighborhoods according to results
        N_Clusters <- menu(choices = as.character(1:Max_N_Clusters), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Clusters"))
        Interaction_DF <- Interaction_DF %>% mutate(Cluster_assignment = as.character(Clustering_result[[as.double(N_Clusters)]][["consensusClass"]]))
        
        #Visualize the heatmap of mean by Cluster
        print("Generating Heatmap")
        Mean_tibble <- Interaction_DF %>% dplyr::select(-Subject_Names) %>% group_by(Cluster_assignment) %>% dplyr::summarize_all(.funs = function(x) mean(x, na.rm = T)) %>%
          ungroup()  #Obtain mean tibble
        Mean_matrix <- as.matrix(Mean_tibble[-1] %>% scale()) #Scale it and transform it into a  mtrix
        row.names(Mean_matrix) <- Mean_tibble[[1]] 
        
        plot(ComplexHeatmap::Heatmap(Mean_matrix,
                                     name = "Scaled")
        )
        #Print the cluster count by sample
        print(Interaction_DF %>% dplyr::count(Cluster_assignment))
        
        #Return the final data
        return(Interaction_DF)
      }
    }
    
    #If non significant metrics are to be excluded execute the following
    if(Exclude_non_significant){
      print("Obtaining data")
      Interaction_DF <- map_dfr(DATA, function(Image){
        Image %>% dplyr::mutate(col_id = str_c(from_label, to_label, sep = "_")) %>%
          dplyr::filter(sig) %>%
          pivot_wider(id_cols = group_by, names_from = col_id, values_from = ct) %>%
          dplyr::rename("Subject_Names" = "group_by")
      })
      #If no clustering is required return the actual data
      if(!Cluster){
        return(Interaction_DF)
      }
      
      #If Clustering is required then proceed with clustering
      if(Cluster){
        print("Performing clustering")
        #We will remove the non-clusteable samples using a while loop
        Na_count <- sum(is.na(dist(Interaction_DF[-1], Consensus_Distance))) #Generate the entering condition
        while(Na_count >= 1){
          Na_tibble <- as_tibble(as.matrix(dist(Interaction_DF[-1], Consensus_Distance, diag = T, upper = T)))
          warning(paste0("Some samples had insufficient data. The following samples will be removedd: ", 
                         str_c(Interaction_DF[[1]][
                           which(map_dbl(Na_tibble, function(Column) sum(is.na(Column))) == max(map_dbl(Na_tibble, function(Column) sum(is.na(Column)))))], 
                           collapse = ", "))) #Print a warning message everytime samples are removed
          Interaction_DF <- Interaction_DF[
            -which(map_dbl(Na_tibble, function(Column) sum(is.na(Column))) == max(map_dbl(Na_tibble, function(Column) sum(is.na(Column))))),] #Generate the output tibble
          Na_count <- sum(is.na(dist(Interaction_DF[-1], Consensus_Distance)))#Update the NA counter
        }
        
        #We need to calculate our own distance matrix to account for potential NA values
        for_dist <- Interaction_DF %>% dplyr::select(-Subject_Names) %>% scale()
        Distance_matrix <- dist(for_dist, Consensus_Distance)
        
        #Perform consensus clustering
        Clustering_result <- ConsensusClusterPlus::ConsensusClusterPlus(Distance_matrix, 
                                                                        maxK = Max_N_Clusters, 
                                                                        reps = Consensus_reps,
                                                                        pItem = Consensus_p_Items,
                                                                        pFeature = 1, #For a distance matrix we need to always use 100% of features
                                                                        title = Consensus_Name,
                                                                        clusterAlg = Consensus_Cluster_Alg,
                                                                        plot = "png",
                                                                        verbose = T)
        
        #Make the user decide the number of neighborhoods according to results
        N_Clusters <- menu(choices = as.character(1:Max_N_Clusters), title = paste0("Check the results at: ", getwd(), ". Then decide the appropiate number of Clusters"))
        Interaction_DF <- Interaction_DF %>% mutate(Cluster_assignment = as.character(Clustering_result[[as.double(N_Clusters)]][["consensusClass"]]))
        
        #Visualize the heatmap of mean by Cluster
        Mean_tibble <- Interaction_DF %>% dplyr::select(-Subject_Names) %>% group_by(Cluster_assignment) %>% dplyr::summarize_all(.funs = function(x) mean(x, na.rm = T)) %>%
          ungroup() #Obtain mean tibble
        Mean_matrix <- as.matrix(Mean_tibble[-1] %>% scale()) #Scale it and transform it into a  mtrix
        row.names(Mean_matrix) <- Mean_tibble[[1]] 
        
        plot(ComplexHeatmap::Heatmap(Mean_matrix,
                                     name = "Scaled")
        )
        #Print the cluster count by sample
        print(Interaction_DF %>% dplyr::count(Cluster_assignment))
        
        #Return the final data
        return(Interaction_DF)
      }
      
    }
  }, options = list(optimize = 3))

Quick_Tumor_Stroma_identifier <- 
  cmpfun(function(DATA_Phenotypes = NULL,
                  Index_phenotype = NULL,
                  Accuracy = NULL,
                  Min_cell_no = NULL,
                  Image_preview = NULL,
                  N_cores = NULL){
    #Check arguments
    if(!all(is.character(DATA_Phenotypes), exists(DATA_Phenotypes, envir = .GlobalEnv))) stop("DATA_Phenotypes must be the name of an existing object")
    #Import all required Data from the environment
    DATA_Phenotypes <- get(DATA_Phenotypes, envir = globalenv())
    
    #Check more arguments
    if(!Index_phenotype %in% unique(DATA_Phenotypes$Phenotype)) stop(paste0("Index phenotype must be one of the following: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
    if(!all(is.numeric(Accuracy), Accuracy > 0)) stop("Accuracy must be a numeric value > 0")
    if(!all(is.numeric(Min_cell_no), Min_cell_no%%1 == 0, Min_cell_no > 0)) stop("Min_cell_no must be an integer value > 0")
    if(!Image_preview %in% unique(DATA_Phenotypes$Subject_Names)) stop(paste0(Image_preview, " not found in Subject_Names"))
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    
    
    
    print("Performing initial test")
    For_plot <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_preview)
    
    #Generate tiled image
    Test_plot_source <- For_plot %>% dplyr::filter(Phenotype == Index_phenotype) %>%
      ggplot(aes(x = X, y = Y)) + geom_bin2d(binwidth = Accuracy)
    
    plot(
      as_tibble(layer_data(Test_plot_source)) %>% dplyr::filter(value >= Min_cell_no) %>%
        ggplot() +
        geom_rect(aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), fill = "grey", color = "black") + 
        theme_minimal() + 
        geom_point(aes(x = X, y = Y), color = "red", alpha = 0.2, data = For_plot %>% dplyr::filter(Phenotype == Index_phenotype)) +
        theme(panel.grid = element_blank()) 
    )
    
    #Ask the user if the algorihtm should proceed
    answer <- menu(c("Proceed", "Abort"), title = "The plot depics the suggested area occupied by tumor cells. Should the algorithm proceed with the analysis?")
    #If user decides to stop then abort function and return stop message
    if(answer == 2) {
      stop("The function has been stopped. Please tune the parameters for a better result")
    }
    
    #If not proceed with computation
    else{
      
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      Final_list <-
        furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(Image){
          #Select individual images
          Base <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
          
          #Generate tiled image
          Plot_source <- Base %>% dplyr::filter(Phenotype == Index_phenotype) %>%
            ggplot(aes(x = X, y = Y)) + geom_bin2d(binwidth = Accuracy)
          
          #Filter layer information according to pre-specified requirements
          Interim <- as_tibble(layer_data(Plot_source)) %>% dplyr::filter(value >= Min_cell_no) %>% dplyr::select(xbin, ybin, x, y, xmin, xmax, ymin, ymax)
          
          #Assign cells to a specific compartment
          Final <- Base %>% mutate(Compartment = map2_lgl(.x = Base$X, .y = Base$Y, function(.x, .y) {
            x_position <- (.x >= Interim$xmin) & (.x < Interim$xmax)
            y_position <- (.y >= Interim$ymin) & (.y < Interim$ymax)
            Final_pos <- any(x_position & y_position)
          })) %>% mutate(Compartment = case_when(Compartment ~ "Tumor",
                                                 !Compartment ~ "Stroma"))
          return(Final)
        },
        .progress = TRUE)
      future::plan("future::sequential")
      gc()
      
      return(
        map_dfr(Final_list, bind_rows)
      )
    }
  }, options = list(optimize = 3))

DBSCAN__Tumor_Stroma_identifier <- cmpfun(
  function(DATA_Phenotypes = NULL,
           Index_phenotype = NULL,
           Image_preview = NULL,
           Min_cells = NULL,
           Distance_radius = NULL,
           N_cores = NULL
  ){
    #Check arguments
    #Check arguments
    if(!all(is.character(DATA_Phenotypes), exists(DATA_Phenotypes, envir = .GlobalEnv))) stop("DATA_Phenotypes must be the name of an existing object")
    #Import all required Data from the environment
    DATA_Phenotypes <- get(DATA_Phenotypes, envir = globalenv())
    
    #Check more arguments
    if(!Index_phenotype %in% unique(DATA_Phenotypes$Phenotype)) stop(paste0("Index phenotype must be one of the following: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
    if(!all(is.numeric(Min_cells), Min_cells%%1 == 0, Min_cells > 0)) stop("Min_cells must be an integer value > 0")
    if(!Image_preview %in% unique(DATA_Phenotypes$Subject_Names)) stop(paste0(Image_preview, " not found in Subject_Names"))
    if(!all(is.numeric(Distance_radius), Distance_radius > 0)) stop("Distance_radius must be an integer value > 0")
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    
    
    
    print("Performing initial test")
    #First generate the DBSCAN model for the Image preview
    #Select Image to be previewed
    For_test <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_preview)
    #Obtain individual data sets containing Index cell and other cells
    For_test_Index <- For_test %>% dplyr::filter(Phenotype == Index_phenotype)
    For_test_no_Index <- For_test %>% dplyr::filter(Phenotype != Index_phenotype)
    
    #Calculate the DBSCAN model with the required parameters
    DB_results <- dbscan::dbscan(For_test_Index[c("X", "Y")], eps = Distance_radius, minPts = Min_cells, borderPoints = FALSE)
    
    #Obtain position in the tumor and the stroma for both index cells and other cells
    For_test_Index <- For_test_Index %>% mutate(Cluster = as.character(DB_results$cluster)) %>%
      mutate(Cluster = case_when(Cluster == "0" ~ "Stroma",
                                 T ~ "Tumor"))
    For_test_no_Index <- For_test_no_Index %>% 
      mutate(Cluster = predict(DB_results, newdata = For_test_no_Index[c("X", "Y")], data = For_test_Index[c("X", "Y")])) %>% 
      mutate(Cluster = case_when(Cluster == "0" ~ "Stroma",
                                 T ~ "Tumor"))
    
    #Obtain final result
    Test_final <- bind_rows(For_test_Index, For_test_no_Index) %>% mutate(Cell_arrange = as.numeric(substr(Cell_no, start = 6, stop = nchar(Cell_no)))) %>%
      arrange(Cell_arrange) %>% dplyr::select(-Cell_arrange)
    
    #Obtain three plots
    PLOT1 <- For_test %>% mutate(Phenotype = case_when(Phenotype == Index_phenotype ~ "Tumor Cell",
                                                       TRUE ~ "OTHER")) %>%
      ggplot(aes(x = X, y = Y, color = Phenotype)) + 
      geom_point(size = 1) +
      theme_minimal() +
      scale_color_manual("", values = c(alpha("grey", 0.1), alpha("black", 0.5))) +
      ggtitle("Cells in analysis") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5))
    
    PLOT2 <- For_test_Index %>% dplyr::filter(Cluster != "Stroma") %>% 
      ggplot(aes(x = X, y = Y)) + 
      geom_point() +
      theme_minimal() +
      ggtitle("Tumor cells that are clustered") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5))
    
    PLOT3 <- Test_final %>% 
      ggplot(aes(x = X, y = Y, color = Cluster)) + 
      geom_point(size = 1) +
      theme_minimal() +
      scale_color_manual("", values = c(alpha("grey", 0.1), alpha("black", 0.5))) +
      ggtitle("Final Result") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5))
    
    #plot final result
    plot(patchwork::wrap_plots(PLOT1, PLOT2, nrow = 1))
    
    #Ask user if they want to proceed
    User_answer <- menu(choices = c("Proceed", "Abort"), title = "Evaluate images and decide if analysis should proceed")
    if(User_answer == 2){
      stop("Algorithm has been aborted. Refine Min_cells and Distance radius analysis")
    }
    #If they want to continue
    else{
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      Final_list <-
        furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(Image){
          #Select Image to be previewed
          For_test <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
          #Obtain individual data sets containing Index cell and other cells
          For_test_Index <- For_test %>% dplyr::filter(Phenotype == Index_phenotype)
          For_test_no_Index <- For_test %>% dplyr::filter(Phenotype != Index_phenotype)
          
          #Calculate the DBSCAN model with the required parameters
          DB_results <- dbscan::dbscan(For_test_Index[c("X", "Y")], eps = Distance_radius, minPts = Min_cells, borderPoints = FALSE)
          
          #Obtain position in the tumor and the stroma for both index cells and other cells
          For_test_Index <- For_test_Index %>% mutate(Compartment = as.character(DB_results$cluster)) %>%
            mutate(Compartment = case_when(Compartment == "0" ~ "Stroma",
                                           T ~ "Tumor"))
          For_test_no_Index <- For_test_no_Index %>% 
            mutate(Compartment = predict(DB_results, newdata = For_test_no_Index[c("X", "Y")], data = For_test_Index[c("X", "Y")])) %>% 
            mutate(Compartment = case_when(Compartment == "0" ~ "Stroma",
                                           T ~ "Tumor"))
          
          #Obtain final result
          Test_final <- bind_rows(For_test_Index, For_test_no_Index) %>% mutate(Cell_arrange = as.numeric(substr(Cell_no, start = 6, stop = nchar(Cell_no)))) %>%
            arrange(Cell_arrange) %>% dplyr::select(-Cell_arrange)
        },
        .progress = TRUE)
      future::plan("future::sequential")
      gc()
      
      return(
        map_dfr(Final_list, bind_rows)
      )
    }
  },
  options = list(optimize = 3))

LISA__Tumor_Stroma_identifier <- 
  cmpfun(function(DATA_Phenotypes = NULL,
                  Index_phenotype = NULL,
                  Image_preview = NULL,
                  Association_Dist_min = NULL,
                  Association_Dist_max = NULL,
                  Type_of_LISA_function = NULL,
                  Window_type = NULL,
                  N_cores = NULL){
    
    #Check arguments
    if(!all(is.character(DATA_Phenotypes), exists(DATA_Phenotypes, envir = .GlobalEnv))) stop("DATA_Phenotypes must be the name of an existing object")
    
    
    #Import all required Data from the environment
    DATA_Phenotypes <- get(DATA_Phenotypes, envir = .GlobalEnv)
    
    #Check more arguments
    if(!Index_phenotype %in% unique(DATA_Phenotypes$Phenotype)) stop(paste0("Index phenotype must be one of the following: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
    if(!Image_preview %in% unique(DATA_Phenotypes$Subject_Names)) stop(paste0(Image_preview, " not found in Subject_Names"))
    
    if(!all(is.numeric(Association_Dist_min), Association_Dist_min > 0)) stop("Association_Dist_min must be a numeric value > 0")
    if(!all(is.numeric(Association_Dist_max), Association_Dist_max > 0)) stop("Association_Dist_max must be a numeric value > 0")
    if(!Association_Dist_max > Association_Dist_min) stop("Association_Dist_max must be greater than Association_Dist_min")
    if(!Type_of_LISA_function %in% c("L", "K")) stop("Type_of_LISA_function must be one of the following: L, K")
    if(!Window_type %in% c("square", "convex", "concave")) stop("Window_type must be one of the following: square, convex, concave")
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    
    DATA_Phenotypes <- DATA_Phenotypes %>% dplyr::rename("x" = "X", "y" = "Y")
    
    #Select Image to be previewed
    For_test <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_preview)
    
    #Change all non-tumor cells to OTHER phenotype
    For_test <- For_test %>% mutate(Cell_ref = case_when(Phenotype == Index_phenotype ~ "TUMOR",
                                                         TRUE ~ "OTHER"))

    
    #Turn our data into a SpatialExperiment object
    Spatial_object_Test <- SpatialExperiment::SpatialExperiment(sample_id = Image_preview,
                                                                colData = For_test,
                                                                spatialCoordsNames = c("x", "y")
    )
    
    #Perform LISA clustering
    Lisa_test <- lisaClust::lisaClust(Spatial_object_Test,
                                      k = 3,
                                      Rs = seq(from = Association_Dist_min, to = Association_Dist_max, by = floor((Association_Dist_max - Association_Dist_min)/5)),
                                      imageID = "Subject_Names",
                                      cellType = "Cell_ref",
                                      spatialCoords = c("x", "y"),
                                      window = Window_type,
                                      lisaFunc = Type_of_LISA_function)
    
    #Add results to original data
    For_test <- For_test %>% mutate(Region = SummarizedExperiment::colData(Lisa_test)$region)
    
    rm(Lisa_test)
    gc()
    
    #Generate the tumor proportion results and add labels
    Proportion_results <- as_tibble(prop.table(table(For_test$Region, For_test$Cell_ref), margin = 1), .name_repair = "universal")
    names(Proportion_results) <- c("Region", "Cell", "Proportion")
    Proportion_results <- Proportion_results %>% pivot_wider(names_from = Cell, values_from = Proportion)
    
    #Generate name results
    Tumor_region <- Proportion_results[[which(Proportion_results[["TUMOR"]] == max(Proportion_results[["TUMOR"]])), 1]]
    Stroma_region <- Proportion_results[[which(Proportion_results[["TUMOR"]] == min(Proportion_results[["TUMOR"]])), 1]]
    Interface_region <- Proportion_results[["Region"]][which((Proportion_results[["Region"]] != Tumor_region & Proportion_results[["Region"]] != Stroma_region))]
    
    For_test <- For_test %>% mutate(Region = case_when(Region == Tumor_region ~ "Tumor",
                                                       Region == Stroma_region ~ "Stroma",
                                                       Region == Interface_region ~ "Border"))
    
    PLOT1 <- For_test %>% ggplot(aes(x = x, y = y, color = Cell_ref )) +
      geom_point(size = 1.5) +
      theme_minimal() +
      scale_color_manual(values = c(alpha("grey", 0.05), alpha("black", 0.8)))+
      ggtitle("Tumor / Non tumor cells") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5))
    
    PLOT2 <-  For_test %>% ggplot(aes(x = x, y = y, color = Region)) + 
      geom_point(size = 1.5) +
      theme_minimal() +
      scale_color_manual(values = c(alpha("grey", 0.05), alpha("blue", 0.5), alpha("red", 0.5)))+
      ggtitle("Tissue segmented cells") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5))
    plot(patchwork::wrap_plots(PLOT1, PLOT2, nrow = 1))
    
    #Ask user if they want to proceed
    User_answer <- menu(choices = c("Proceed", "Abort"), title = "Evaluate images and decide if analysis should proceed")
    if(User_answer == 2){
      stop("Algorithm has been aborted. Refine parameters to improve performance (Association_Dist, LISA_function, Window_type)")
    }
    #If they want to continue
    else{
      #save exit function if parallelization fails
      on.exit({
        future::plan("future::sequential")
        gc()
      })
      
      future::plan("future::multisession", workers = N_cores) 
      options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
      furrr::furrr_options(scheduling = Inf)
      
      Final_list <-
        suppressMessages(furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(Image){
          #Select Image to be previewed
          For_test <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
          
          #Change all non-tumor cells to OTHER phenotype
          For_test <- For_test %>% mutate(Cell_ref = case_when(Phenotype == Index_phenotype ~ "TUMOR",
                                                               TRUE ~ "OTHER"))
          #Turn our data into a SpatialExperiment object
          Spatial_object_Test <- SpatialExperiment::SpatialExperiment(sample_id = Image,
                                                                      colData = For_test,
                                                                      spatialCoordsNames = names(For_test)[2:3]
          )
          
          #Perform LISA clustering
          Lisa_test <- lisaClust::lisaClust(Spatial_object_Test,
                                            k = 3,
                                            Rs = seq(from = Association_Dist_min, to = Association_Dist_max, by = floor((Association_Dist_max - Association_Dist_min)/5)),
                                            imageID = "Subject_Names",
                                            cellType = "Cell_ref",
                                            spatialCoords = c("x", "y"),
                                            window = Window_type,
                                            lisaFunc = Type_of_LISA_function)
          
          #Add results to original data
          For_test <- For_test %>% mutate(Compartment = SummarizedExperiment::colData(Lisa_test)$region)
          
          rm(Lisa_test)
          gc()
          
          #Generate the tumor proportion results and add labels
          Proportion_results <- as_tibble(prop.table(table(For_test$Compartment, For_test$Cell_ref), margin = 1), .name_repair = "universal")
          names(Proportion_results) <- c("Region", "Cell", "Proportion")
          Proportion_results <- Proportion_results %>% pivot_wider(names_from = Cell, values_from = Proportion)
          
          #Generate name results
          Tumor_region <- Proportion_results[[which(Proportion_results[["TUMOR"]] == max(Proportion_results[["TUMOR"]])), 1]]
          Stroma_region <- Proportion_results[[which(Proportion_results[["TUMOR"]] == min(Proportion_results[["TUMOR"]])), 1]]
          Interface_region <- Proportion_results[["Region"]][which((Proportion_results[["Region"]] != Tumor_region & Proportion_results[["Region"]] != Stroma_region))]
          
          For_test <- For_test %>% mutate(Compartment = case_when(Compartment == Tumor_region ~ "Tumor",
                                                                  Compartment == Stroma_region ~ "Stroma",
                                                                  Compartment == Interface_region ~ "Border"))
          For_test <- For_test %>% dplyr::select(-Cell_ref)
          return(For_test)
        },
        .progress = TRUE))
        
      
      future::plan("future::sequential")
      gc()
      
      FINAL_result <- map_dfr(Final_list, bind_rows)
      return(FINAL_result %>% dplyr::rename("X" = "x", "Y" = "y"))
    }
  }, options = list(optimize = 3))

Advanced_Tumor_Stroma_identifier <- 
  cmpfun(function(DATA_Phenotypes = NULL,
                  Index_phenotype = NULL,
                  Filtering_Method = NULL,
                  
                  Accuracy = NULL,
                  Min_cell_no = NULL,
                  Distance_radius = NULL,
                  
                  
                  
                  Hull_ratio = NULL,
                  Calculate_border = NULL,
                  Dist_to_border = NULL,
                  
                  Image_preview = NULL,
                  N_cores = NULL){
    #Check arguments
    if(!all(is.character(DATA_Phenotypes), exists(DATA_Phenotypes, envir = .GlobalEnv))) stop("DATA_Phenotypes must be the name of an existing object")
    #Import all required Data from the environment
    DATA_Phenotypes <- get(DATA_Phenotypes, envir = globalenv())
    
    #Check more arguments
    if(!Index_phenotype %in% unique(DATA_Phenotypes$Phenotype)) stop(paste0("Index phenotype must be one of the following: ", str_c(unique(DATA_Phenotypes$Phenotype), collapse = ", ")))
    if(!Filtering_Method %in% c("Tiling", "DBSCAN")) stop("Filtering_Method must be one of the following: Tiling, DBSCAN")
    if(Filtering_Method == "Tiling"){
      if(!all(is.numeric(Accuracy), Accuracy > 0)) stop("Accuracy must be a numeric value > 0")
      if(!all(is.numeric(Min_cell_no), Min_cell_no%%1 == 0, Min_cell_no > 0)) stop("Min_cell_no must be an integer value > 0")
    }
    if(Filtering_Method == "DBSCAN"){
      if(!all(is.numeric(Min_cell_no), Min_cell_no%%1 == 0, Min_cell_no > 0)) stop("Min_cell_no must be an integer value > 0")
      if(!all(is.numeric(Distance_radius), Distance_radius > 0)) stop("Distance_radius must be a numeric value > 0")
    }
    
    if(!all(is.numeric(Hull_ratio), Hull_ratio >= 0, Hull_ratio <= 1)) stop("Hull_ratio must be a numeric value between 0 and 1")
    if(!is.logical(Calculate_border)) stop("Calculate_border must be a logical value")
    if(Calculate_border) if(!all(is.numeric(Dist_to_border), Dist_to_border > 0)) stop("Dist_to_border must be a numeric value > 0")
    if(!Image_preview %in% unique(DATA_Phenotypes$Subject_Names)) stop(paste0(Image_preview, " not found in Subject_Names"))
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    
    
    print("Performing initial test")
    #First work on the tiling filtering method
    if(Filtering_Method == "Tiling"){
      #Select the image to be previewer
      For_plot <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_preview)
      
      #Generate tiled image
      Test_plot_source <- For_plot %>% dplyr::filter(Phenotype == Index_phenotype) %>%
        ggplot(aes(x = X, y = Y)) + geom_bin2d(binwidth = Accuracy)
      
      #Plot adequate tiles and overlay the tumor cells
      Tile_plot <- as_tibble(layer_data(Test_plot_source)) %>% dplyr::filter(value >= Min_cell_no) %>%
        ggplot() +
        geom_rect(aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), fill = "grey", color = "black") +
        theme_minimal() + 
        scale_x_continuous("") + 
        scale_y_continuous("") +
        geom_point(aes(x = X, y = Y), color = "red", alpha = 0.2, data = For_plot %>% dplyr::filter(Phenotype == Index_phenotype)) +
        theme(panel.grid = element_blank(),
              axis.text = element_blank())
      
      #Filter layer information according to pre-specified requirements
      Interim <- as_tibble(layer_data(Test_plot_source)) %>% dplyr::filter(value >= Min_cell_no) %>% dplyr::select(xbin, ybin, x, y, xmin, xmax, ymin, ymax)
      
      #Select the tumor cells
      Base <- For_plot %>% dplyr::filter(Phenotype == Index_phenotype)
      
      #Filter tumor cells according to their belonging or not to and adequate tile
      Final_tumor_cells <- Base %>% mutate(Compartment = map2_lgl(.x = Base$X, .y = Base$Y, function(.x, .y) {
        x_position <- (.x >= Interim$xmin) & (.x < Interim$xmax)
        y_position <- (.y >= Interim$ymin) & (.y < Interim$ymax)
        Final_pos <- any(x_position & y_position)
      })) %>% dplyr::filter(Compartment)
      
      #Now we are going to build a polygon with these points
      #Prepare the require objects (sf object with tumor cells, sf object with all cells, polygon object and line object)
      Tumor_cells_sf <- sf::st_as_sf(Final_tumor_cells, coords = c("X", "Y"))
      All_cells_sf <- sf::st_as_sf(For_plot, coords = c("X", "Y"))
      Final_tumor_cells_polygon <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "POLYGON")
      Final_tumor_cells_line <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
      
      #Define compartments
      For_plot$Compartment <- "Stroma"
      For_plot[For_plot$Cell_no %in% as_tibble(sf::st_filter(All_cells_sf, Final_tumor_cells_polygon))[[1]],
               "Compartment"] <- "Tumor"
      #If border compartment needs to be calculated perform appropriate changes in the data
      if(Calculate_border){
        For_plot[unlist(sf::st_is_within_distance(All_cells_sf, Final_tumor_cells_line, sparse = F, dist = Dist_to_border)),
                 "Compartment"] <- "Border"
      }
      
      #Generate the final plot of the cell assignment
      Final_plot <- For_plot %>% ggplot(aes(x = X, y = Y, color = Compartment)) + geom_point() +
        scale_color_manual(values = c("red", "blue", "green")) +
        theme_minimal() + 
        scale_x_continuous("") + 
        scale_y_continuous("") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              legend.position = "bottom")
      
      #Generate the polygon layout plot
      Layout_plot <- Final_tumor_cells_polygon %>% ggplot() + geom_sf(linewidth = 1.5, fill = "white", color = "black") +
        theme_minimal() + 
        scale_x_continuous("") + 
        scale_y_continuous("") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank())
      
      #Plot all the results and then ask the user
      plot(patchwork::wrap_plots(Tile_plot, Final_plot, Layout_plot, nrow = 2))
      
      
      #Ask the user if the algorihtm should proceed
      answer <- menu(c("Proceed", "Abort"), title = "The plot depics the suggested area occupied by tumor cells. Should the algorithm proceed with the analysis?")
      #If user decides to stop then abort function and return stop message
      if(answer == 2) {
        stop("The function has been stopped. Please tune the parameters for a better result")
      }
      
      #If everything is OK proceed with computation
      else{
        #save exit function if parallelization fails
        on.exit({
          future::plan("future::sequential")
          gc()
        })
        
        future::plan("future::multisession", workers = N_cores) 
        options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
        furrr::furrr_options(scheduling = Inf)
        
        Final_list <-
          furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(Image){
            #Select the image to be previewer
            For_plot <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
            
            #Generate tiled image
            Test_plot_source <- For_plot %>% dplyr::filter(Phenotype == Index_phenotype) %>%
              ggplot(aes(x = X, y = Y)) + geom_bin2d(binwidth = Accuracy)
            
            #Filter layer information according to pre-specified requirements
            Interim <- as_tibble(layer_data(Test_plot_source)) %>% dplyr::filter(value >= Min_cell_no) %>% dplyr::select(xbin, ybin, x, y, xmin, xmax, ymin, ymax)
            
            #Select the tumor cells
            Base <- For_plot %>% dplyr::filter(Phenotype == Index_phenotype)
            
            #Filter tumor cells according to their belonging or not to and adequate tile
            Final_tumor_cells <- Base %>% mutate(Compartment = map2_lgl(.x = Base$X, .y = Base$Y, function(.x, .y) {
              x_position <- (.x >= Interim$xmin) & (.x < Interim$xmax)
              y_position <- (.y >= Interim$ymin) & (.y < Interim$ymax)
              Final_pos <- any(x_position & y_position)
            })) %>% dplyr::filter(Compartment)
            
            #Now we are going to build a polygon with these points
            #Prepare the require objects (sf object with tumor cells, sf object with all cells, polygon object and line object)
            Tumor_cells_sf <- sf::st_as_sf(Final_tumor_cells, coords = c("X", "Y"))
            All_cells_sf <- sf::st_as_sf(For_plot, coords = c("X", "Y"))
            Final_tumor_cells_polygon <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "POLYGON")
            Final_tumor_cells_line <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
            
            #Define compartments
            For_plot$Compartment <- "Stroma"
            
            For_plot[For_plot$Cell_no %in% as_tibble(sf::st_filter(All_cells_sf, Final_tumor_cells_polygon))[[1]],
                     "Compartment"] <- "Tumor"
            
            #If border compartment needs to be calculated perform appropriate changes in the data
            if(Calculate_border){
              For_plot[unlist(sf::st_is_within_distance(All_cells_sf, Final_tumor_cells_line, sparse = F, dist = Dist_to_border)),
                       "Compartment"] <- "Border"
            }
            
            return(list(DATA = For_plot,
                        Area = sf::st_area(Final_tumor_cells_polygon)))
            
            
          },
          .progress = TRUE)
        future::plan("future::sequential")
        gc()
        
        #Name the list accorrdingly
        names(Final_list) <- unique(DATA_Phenotypes$Subject_Names)
        #Collapse data into a single DF
        Final_DATA <- map_dfr(Final_list, function(Image) Image[[1]])
        #Collapse areas into a single DF
        Final_Areas <- tibble(Subject_Names = unique(DATA_Phenotypes$Subject_Names),
                              Area = map_dbl(Final_list, function(Image) Image[[2]]))
        
        #Return all the results
        return(list(DATA_Phenotypes = Final_DATA,
                    DATA_Compartment_Area = Final_Areas))
      }
    }
    
    #If DBSCAN is the filtering method of choice, proceed accordingly
    if(Filtering_Method == "DBSCAN"){
      #Select Image to be previewed
      For_test <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image_preview)
      #Obtain individual data sets containing Index cell and other cells
      For_test_Index <- For_test %>% dplyr::filter(Phenotype == Index_phenotype)
      For_test_no_Index <- For_test %>% dplyr::filter(Phenotype != Index_phenotype)
      
      #Calculate the DBSCAN model with the required parameters
      DB_results <- dbscan::dbscan(For_test_Index[c("X", "Y")], eps = Distance_radius, minPts = Min_cell_no, borderPoints = FALSE)
      
      #Obtain position in the tumor and the stroma for both index cells and other cells
      For_test_Index <- For_test_Index %>% mutate(Compartment = as.character(DB_results$cluster)) %>%
        mutate(Compartment = case_when(Compartment == "0" ~ "Stroma",
                                       T ~ "Tumor"))
      #Obtain the final tumor cells
      Final_tumor_cells <- For_test_Index %>% dplyr::filter(Compartment == "Tumor")
      
      #Now we are going to build a polygon with these points
      #Prepare the require objects (sf object with tumor cells, sf object with all cells, polygon object and line object)
      Tumor_cells_sf <- sf::st_as_sf(Final_tumor_cells, coords = c("X", "Y"))
      All_cells_sf <- sf::st_as_sf(For_test, coords = c("X", "Y"))
      Final_tumor_cells_polygon <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "POLYGON")
      Final_tumor_cells_line <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
      
      #Define compartments
      For_test$Compartment <- "Stroma"
      For_test[For_test$Cell_no %in% as_tibble(sf::st_filter(All_cells_sf, Final_tumor_cells_polygon))[[1]],
               "Compartment"] <- "Tumor"
      #If border compartment needs to be calculated perform appropriate changes in the data
      if(Calculate_border){
        For_test[unlist(sf::st_is_within_distance(All_cells_sf, Final_tumor_cells_line, sparse = F, dist = Dist_to_border)),
                 "Compartment"] <- "Border"
      }
      
      PLOT1 <- For_test %>% mutate(Phenotype = case_when(Phenotype == Index_phenotype ~ "Tumor Cell",
                                                         TRUE ~ "OTHER")) %>%
        ggplot(aes(x = X, y = Y, color = Phenotype)) + 
        geom_point(size = 1) +
        theme_minimal() +
        scale_color_manual("", values = c(alpha("grey", 0.1), alpha("black", 0.5))) +
        ggtitle("Cells in analysis") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5))
      
      PLOT2 <- For_test_Index %>% dplyr::filter(Compartment != "Stroma") %>% 
        ggplot(aes(x = X, y = Y)) + 
        geom_point() +
        theme_minimal() +
        ggtitle("Tumor cells that are clustered") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5))
      
      #Generate the final plot of the cell assignment
      Final_plot <- For_test %>% ggplot(aes(x = X, y = Y, color = Compartment)) + geom_point() +
        scale_color_manual(values = c("red", "blue", "green")) +
        theme_minimal() + 
        scale_x_continuous("") + 
        scale_y_continuous("") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              legend.position = "bottom")
      
      #Generate the polygon layout plot
      Layout_plot <- Final_tumor_cells_polygon %>% ggplot() + geom_sf(linewidth = 1.5, fill = "white", color = "black") +
        theme_minimal() + 
        scale_x_continuous("") + 
        scale_y_continuous("") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank())
      
      plot(patchwork::wrap_plots(PLOT1, PLOT2, Final_plot, Layout_plot, ncol = 2, nrow = 2, widths = 1))
      
      #Ask user if they want to proceed
      User_answer <- menu(choices = c("Proceed", "Abort"), title = "Evaluate images and decide if analysis should proceed")
      if(User_answer == 2){
        stop("Algorithm has been aborted. Refine Min_cells and Distance radius analysis")
      }
      #If they want to continue
      else{
        #save exit function if parallelization fails
        on.exit({
          future::plan("future::sequential")
          gc()
        })
        
        future::plan("future::multisession", workers = N_cores) 
        options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
        furrr::furrr_options(scheduling = Inf)
        
        Final_list <-
          furrr::future_map(unique(DATA_Phenotypes$Subject_Names), function(Image){
            #Select Image to be previewed
            For_test <- DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image)
            #Obtain individual data sets containing Index cell and other cells
            For_test_Index <- For_test %>% dplyr::filter(Phenotype == Index_phenotype)
            
            #Calculate the DBSCAN model with the required parameters
            DB_results <- dbscan::dbscan(For_test_Index[c("X", "Y")], eps = Distance_radius, minPts = Min_cell_no, borderPoints = FALSE)
            
            #Obtain position in the tumor and the stroma for both index cells and other cells
            For_test_Index <- For_test_Index %>% mutate(Compartment = as.character(DB_results$cluster)) %>%
              mutate(Compartment = case_when(Compartment == "0" ~ "Stroma",
                                             T ~ "Tumor"))
            #Obtain the final tumor cells
            Final_tumor_cells <- For_test_Index %>% dplyr::filter(Compartment == "Tumor")
            
            #Now we are going to build a polygon with these points
            #Prepare the require objects (sf object with tumor cells, sf object with all cells, polygon object and line object)
            Tumor_cells_sf <- sf::st_as_sf(Final_tumor_cells, coords = c("X", "Y"))
            All_cells_sf <- sf::st_as_sf(For_test, coords = c("X", "Y"))
            Final_tumor_cells_polygon <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "POLYGON")
            Final_tumor_cells_line <- sf::st_cast((Tumor_cells_sf %>% summarise() %>% sf::st_concave_hull(ratio = Hull_ratio) %>% summarise), "LINESTRING")
            
            #Define compartments
            For_test$Compartment <- "Stroma"
            For_test[For_test$Cell_no %in% as_tibble(sf::st_filter(All_cells_sf, Final_tumor_cells_polygon))[[1]],
                     "Compartment"] <- "Tumor"
            #If border compartment needs to be calculated perform appropriate changes in the data
            if(Calculate_border){
              For_test[unlist(sf::st_is_within_distance(All_cells_sf, Final_tumor_cells_line, sparse = F, dist = Dist_to_border)),
                       "Compartment"] <- "Border"
            }
            
            #Return the final results
            return(list(DATA = For_test,
                        Area = sf::st_area(Final_tumor_cells_polygon)))
          },
          .progress = TRUE)
        future::plan("future::sequential")
        gc()
        
        #Name accordingly
        names(Final_list) <- unique(DATA_Phenotypes$Subject_Names)
        
        #Collapse data into a single DF
        Final_DATA <- map_dfr(Final_list, function(Image) Image[[1]])
        
        #Collaps areas into a single DF
        Final_Areas <- tibble(Subject_Names = unique(DATA_Phenotypes$Subject_Names),
                              Area = map_dbl(Final_list, function(Image) Image[[2]]))
        
        #Return the final results
        return(list(DATA_Phenotypes = Final_DATA,
                    DATA_Compartment_Area = Final_Areas))
        
      }
    }
  }, 
  options = list(optimize = 3))

Compartment_Phenotype_quantifier <- 
  cmpfun(function(DATA = NULL,
                  Calculate_Density = NULL,
                  DATA_Area = NULL){
    DATA <- DATA
    #Check that DATA has been created with the advanced tumor stroma identifier
    if(!identical(names(DATA), c("DATA_Phenotypes", "DATA_Compartment_Area"))){
      stop("DATA must have been created with the Advanced_Tumor_Stroma_identifier")
    }
    
    if(!is.logical(Calculate_Density)){
      stop("Calculate_Density must be a logical value")
    }
    if(names(DATA_Area)[ncol(DATA_Area)] != "Area"){
      stop("The last column of the DATA_Area must be named Area")
    }
    
    #Import our DATA
    DATA_Phenotypes <- DATA[[1]]
    DATA_Tumor_Area <- DATA[[2]]
    names(DATA_Tumor_Area)[2] <- "Tumor_Area"
    
    
    #Generate a list of the cell types divided by compartment
    Cells_by_compartment <- map(unique(DATA_Phenotypes$Compartment), function(Unique_Compartment){
      DATA_Phenotypes %>% dplyr::filter(Compartment == Unique_Compartment)
    })
    names(Cells_by_compartment) <- unique(DATA_Phenotypes$Compartment)
    
    #Calculate the cell counts and percentages by compartment
    Cell_prop_by_compartment <- map(Cells_by_compartment, function(Unique_Compartment){
      #Obtain the number of cells by image according to the phenotypes
      Results <- Unique_Compartment %>% group_by(Subject_Names, Phenotype) %>% dplyr::count() %>% ungroup() %>% 
        pivot_wider(names_from = Phenotype, values_from = n)
      Results[is.na(Results)] <- 0
      
      #Calculate the number of total cells
      N_cells <- apply(Results[-1], MARGIN = 1, sum)
      
      #Calculate proportions
      Prop_tibble <- as_tibble(Results[-1] / N_cells)
      names(Prop_tibble) <- str_c("PROP_", names(Prop_tibble))
      Results$N_cells <- N_cells
      
      #Arrange columns by alphabetic order
      Results <- Results[c("Subject_Names", sort(names(Results)[-c(1, ncol(Results))]), "N_cells")]
      Prop_tibble <- Prop_tibble[sort(names(Prop_tibble))]
      
      return(bind_cols(Results, Prop_tibble))
    })
    
    #If densities need to be calculated execute the following code
    if(Calculate_Density){
      #Check arguments
      if(names(DATA_Area)[ncol(DATA_Area)] != "Area"){
        stop("The last column of the DATA_Area must be named Area")
      }
      
      #Obtain the total area and tumor area to calculate Tumor / Stroma areas
      DATA_Overall_Area <- DATA_AREA
      DATA_Areas <- left_join(DATA_Overall_Area, DATA_Tumor_Area, by = "Subject_Names") %>% mutate(Stroma_Area = Area - Tumor_Area)
      
      
      #Start with tumor densities
      TUMOR <- left_join(Cell_prop_by_compartment[["Tumor"]], DATA_Areas[c("Subject_Names", "Tumor_Area")], by = "Subject_Names")
      
      #Select cell counts
      For_density_TUMOR <- TUMOR %>% dplyr::select(-N_cells, -contains("PROP_"))
      
      #Calculate tumor densities
      Density_results_TUMOR <- as_tibble(For_density_TUMOR[c(2:(ncol(For_density_TUMOR)-1))] / For_density_TUMOR[[ncol(For_density_TUMOR)]])
      names(Density_results_TUMOR) <- str_c("Density_", names(Density_results_TUMOR))
      TUMOR <- bind_cols(TUMOR, Density_results_TUMOR)
      
      
      #Continue with stromal densities
      STROMA <- left_join(Cell_prop_by_compartment[["Stroma"]], DATA_Areas[c("Subject_Names", "Stroma_Area")], by = "Subject_Names")
      
      #Select cell counts
      For_density_STROMA <- STROMA %>% dplyr::select(-N_cells, -contains("PROP_"))
      
      #Calculate stromal densities
      Density_results_STROMA <- as_tibble(For_density_STROMA[c(2:(ncol(For_density_STROMA)-1))] / For_density_STROMA[[ncol(For_density_STROMA)]])
      names(Density_results_STROMA) <- str_c("Density_", names(Density_results_STROMA))
      STROMA <- bind_cols(STROMA, Density_results_STROMA)
      
      Cell_prop_by_compartment[["Tumor"]] <- TUMOR
      Cell_prop_by_compartment[["Stroma"]] <- STROMA
      
      #If the input data has stromal compartment, then print a warning
      if(length(Cell_prop_by_compartment) > 2) warning("Border densities are not stimated in the current CSM version")
    }
    
    #Return the final result
    return(Cell_prop_by_compartment)
  }, options = list(optimize = 3))

SPIAT_Tissue_structuring_function <- cmpfun(
  function(N_cores = NULL,
           DATA_SPIAT = NULL, 
           DATA_Phenotypes = NULL, 
           Cell_type_to_define_cluster = NULL,
           Minimum_number_cells_cluster = NULL,
           Cell_types_of_interest = NULL,
           Layers_margin = NULL,
           Simplify_result = NULL
  ){
    #Check arguments
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    
    if(!exists(DATA_SPIAT, envir = .GlobalEnv)) stop("DATA_SPIAT must be an existing object")
    DATA_SPIAT <- get(DATA_SPIAT, envir = .GlobalEnv)
    if(!all(map_lgl(DATA_SPIAT, function(Image) class(Image) == "SpatialExperiment"))) stop("DATA_SPIAT must be created using the SPIAT_object_generator function")
    
    if(!exists(DATA_Phenotypes, envir = .GlobalEnv)) stop("DATA_Phenotypes must be an existing object")
    DATA_Phenotypes <- get(DATA_Phenotypes, envir = .GlobalEnv)
    if(!identical(names(DATA_Phenotypes)[c(1:4)], c("Cell_no", "X", "Y", "Subject_Names"))) stop("DATA_Phenotypes must have an adequate format")
    if(!"Phenotype" %in% names(DATA_Phenotypes)) stop("DATA_Phenotypes must have an adequate format")
    
    if(!all(Cell_type_to_define_cluster %in% unique(unlist(map(DATA_SPIAT, function(x) x@colData@listData$Phenotype))))){
      stop(paste0(Cell_type_to_define_cluster, " not found in DATA_SPIAT"))
    }
    if(!all(Cell_types_of_interest %in% unique(unlist(map(DATA_SPIAT, function(x) x@colData@listData$Phenotype))))) {
      Absent_cell_types <- Cell_types_of_interest[!Cell_types_of_interest %in% unique(unlist(map(DATA_SPIAT, function(x) x@colData@listData$Phenotype)))]
      stop(paste0(str_c(Absent_cell_types, collapse = ", "), " not found in DATA_SPIAT"))
    }
    if(!all(is.numeric(Minimum_number_cells_cluster), Minimum_number_cells_cluster > 0, Minimum_number_cells_cluster%%1 == 0)){
      stop("Minimum_number_cell_cluster must be a integer value > 0")
    }
    if(!all(is.numeric(Layers_margin), Layers_margin%%1 == 0)) stop("Layers_margin must be a integer value")
    if(!is.logical(Simplify_result)) stop("Simplify_result must be a logical value")
    
    #Proceed with execution
    Phenotypes_list <- map(names(DATA_SPIAT), function(Image) DATA_Phenotypes %>% dplyr::filter(Subject_Names == Image))
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    RESULTS <-
      suppressMessages(
        furrr::future_map(seq_along(1:length(DATA_SPIAT)), function(Image) {
        library(SPIAT)
        Formatted_image <- DATA_SPIAT[[Image]] #Get formatted image
        
        #Calculate the ratio of target cells that are in a border
        Border_Ratio <- R_BC(Formatted_image, 
                             cell_type_of_interest = Cell_type_to_define_cluster, #Cell type that defines the tumor
                             feature_colname = "Phenotype")
        
        #Calculate the clusters
        formatted_border <- identify_bordering_cells(Formatted_image,
                                                     reference_cell = Cell_type_to_define_cluster,
                                                     feature_colname = "Phenotype", 
                                                     ahull_alpha = NULL, #Controls the size of cell clusters
                                                     n_to_exclude = Minimum_number_cells_cluster, #Controls the minimum amount of cells to be a cluster
                                                     plot_final_border = FALSE)
        
        #Calculate the number of cluster islands found in the image
        N_tumor_clusters <- attr(formatted_border, "n_of_clusters")
        
        #Now we calculate distances to this borders defined previously
        formatted_distance <- calculate_distance_to_margin(formatted_border)
        
        #Assign a structure to the tissue
        formatted_structure <- define_structure(formatted_distance, 
                                                cell_types_of_interest = Cell_types_of_interest, #Select our cell types of interest 
                                                feature_colname = "Phenotype", 
                                                n_margin_layers = Layers_margin #Layers of cells that define the internal and external border of the tumor margin
        )
        categories <- unique(formatted_structure$Structure)
        plot_cell_categories(formatted_structure, feature_colname = "Structure")
        
        #Calculate proportions of cells in each structure
        immune_proportions <- calculate_proportions_of_cells_in_structure(
          spe_object = formatted_structure, 
          cell_types_of_interest = Cell_types_of_interest,
          feature_colname = "Phenotype")
        
        #Calculate distances
        immune_distances <- calculate_summary_distances_of_cells_to_borders(
          spe_object = formatted_structure, 
          cell_types_of_interest = Cell_types_of_interest,
          feature_colname = "Phenotype")
        
        #Assign location to cell types
        list(Cells = Phenotypes_list[[Image]] %>% mutate(Location = formatted_structure$Structure),
             Border_Ratio = Border_Ratio,
             N_tumor_clusters = N_tumor_clusters,
             Plot = plot_cell_categories(formatted_structure, feature_colname = "Structure"),
             Immune_proportions = immune_proportions,
             Immune_distances = immune_distances)
      },
      .progress = TRUE)
      )
    future::plan("future::sequential")
    gc()
    
    #Change names of the results
    names(RESULTS) <- names(DATA_SPIAT)
    
    #Simplify the results if desired
    if(Simplify_result) {
      return(map_dfr(RESULTS, function(x) x[[1]]) %>% dplyr::select(1:4, Phenotype, Location) %>%
               mutate(Location = case_when(Location == "Border" ~ "Border",
                                           Location == "Infiltrated.CoI" ~ "Core",
                                           Location == "Inside" ~ "Core",
                                           Location == "Stromal.CoI" ~ "Stroma",
                                           Location == "Outside" ~ "Stroma",
                                           Location == "Internal.margin.CoI" ~ "Internal_Border",
                                           Location == "Internal.margin" ~ "Internal_Border",
                                           Location == "External.margin.CoI" ~ "External_Border",
                                           Location == "External.margin" ~ "External_Border")))
    }
    
    #If not return the complete results
    else {return(RESULTS)}
    
  },
  options = list(optimize = 3))

SPIAT_neighborhood_identifier <- cmpfun(
  function(N_cores = NULL,
           DATA_SPIAT = NULL,
           Strategy = NULL,
           Cell_types_of_interest = NULL,
           Radius = NULL,
           Min_neighborhood_size = NULL,
           K_phenograph = NULL, #Still under development
           No_Phenotype_name = NULL) {
    #Check arguments
    if(!all(N_cores >= 1 & N_cores%%1 == 0)) stop("N_cores must be an integer value > 0")
    
    if(!exists(DATA_SPIAT, envir = .GlobalEnv)) stop("DATA_SPIAT must be an existing object")
    DATA_SPIAT <- get(DATA_SPIAT, envir = .GlobalEnv)
    if(!all(map_lgl(DATA_SPIAT, function(Image) class(Image) == "SpatialExperiment"))) stop("DATA_SPIAT must be created using the SPIAT_object_generator function")
    
    if(!Strategy %in% c("hierarchical", "dbscan")) stop("Strategy must be one of the following: hierarchical, dbscan")
    if(!all(Cell_types_of_interest %in% unique(unlist(map(DATA_SPIAT, function(x) x@colData@listData$Phenotype))))) {
      Absent_cell_types <- Cell_types_of_interest[!Cell_types_of_interest %in% unique(unlist(map(DATA_SPIAT, function(x) x@colData@listData$Phenotype)))]
      stop(paste0(str_c(Absent_cell_types, collapse = ", "), " not found in DATA_SPIAT"))
    }
    if(!all(is.numeric(Radius), Radius > 0)){
      stop("Radius must be a numeric value > 0")
    }
    if(!all(is.numeric(Min_neighborhood_size), Min_neighborhood_size%%1 == 0, Min_neighborhood_size > 0)) stop("Min_neighborhood_size must be a integer value > 0")
    if(!any(is.null(No_Phenotype_name), is.character(No_Phenotype_name))) stop("No_Phenotype_name must be Null or a character value")
    
    
    #save exit function if parallelization fails
    on.exit({
      future::plan("future::sequential")
      gc()
    })
    #Proceed with analysis
    future::plan("future::multisession", workers = N_cores) 
    options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")
    furrr::furrr_options(scheduling = Inf)
    
    RESULTS <-
      suppressMessages(
        furrr::future_map(DATA_SPIAT, function(Image){
        library(SPIAT)
        #First calculate the neighborhood
        SPIAT_neighborhoods <-  identify_neighborhoods(
          Image,
          method = Strategy, #Method to be implemented
          cell_types_of_interest = Cell_types_of_interest, #Cell types of interest to find the neighbors
          radius = Radius, #Distance threshold to consider two cells interacting (required for hierarchical and DBSCAN)
          min_neighborhood_size = Min_neighborhood_size, #Minimum size of neighborhoods (required for hierarchical and DBSCAN)
          k = K_phenograph, #A parameter required by phenographr
          feature_colname = "Phenotype",
          no_pheno = No_Phenotype_name #Name for the cells without a phenotype
        )
        
        #We analyze neighborhood composition
        SPIAT_neighborhoods_Composition <- composition_of_neighborhoods(SPIAT_neighborhoods, 
                                                                        feature_colname = "Phenotype")
        
        #We plot neighborhood composition
        plot_composition_heatmap(SPIAT_neighborhoods_Composition, 
                                 feature_colname = "Phenotype",
                                 log_values = T)
        
        #We calculate the Average_Nearest_Neighbour_Index ANN_index by each cluster
        Cluster_Interaction_analysis <- map_dfr(str_subset(unique(SPIAT_neighborhoods$Neighborhood), "Cluster"), function(Cluster) {
          Results <- average_nearest_neighbor_index(SPIAT_neighborhoods, 
                                                    reference_celltypes = Cluster, 
                                                    feature_colname = "Neighborhood", 
                                                    p_val = 0.05 #select threshold p value
          )
          return(c(Cluster = Cluster,
                   ANN_index = Results$ANN_index,
                   Pattern = Results$pattern,
                   p_val = Results$`p-value`))
        })
        
        #We list our results
        return(list(Neighborhoods = SPIAT_neighborhoods,
                    Neighborhoods_Composition = SPIAT_neighborhoods_Composition,
                    Neighborhoods_Composition_plot = plot_composition_heatmap(SPIAT_neighborhoods_Composition, 
                                                                              feature_colname = "Phenotype",
                                                                              log_values = T),
                    Average_Nearest_Neighbour_Index <- Cluster_Interaction_analysis
        ))
      },
      .progress = TRUE)
      )
    future::plan("future::sequential")
    gc()
    
    return(RESULTS)
  },
  options = list(optimize = 3))
###########################END##############################
message("CSM has been successfully loaded")

#################################################################################################################
#################################################################################################################
################################################UPDATE LOG################################################

#VERSION 1.0.1
##Data_set_aside function from STEP 0 has been updated to change the way it works.
##Describe_my_computing_unit function from STEP 0 is now updated to consider other OS environments
##All required functions are bite compiled to marginally improve speed

#VERSION 1.0.2
##Bug in Thresholding_exploration_function from STEP 2 has been solved (if no cells in an passed any variable threshold they were dropped from the tibble)
##Area analyzer has been added
###Suggested_Tile_Size_Calculator STEP 0 has been optimized to graph the output with geom_bin2d in stead of actually tiling the source data

#VERSION 1.0.3
##Bugs in distance matrix calculator, cumulative interaction calculator, Gcross calculator and SPIAT_entropy_gradient_generator (all from STEP 5) that ocurred 
##if no COO or target cells are present in the samples (an error was returned) have been solved
##Thresholding_exploration_function (STEP 2) now computes the cell density if sample area information is provided
##Phenotype_quantifyier (STEP 3) now computes the cell density if sample area information is provided
##Clinical_Data_analyzer (STEP 0) has been modified so that the label of of the statistic test does no longer influence the y axis
##Plot_correlation_matrix (STEP 0, 2 and 3) has now been added to allow the testing of multiple correlations in your data

#VERSION 1.0.4
##Clinical_data_analyzer (STEP 0) has been improved to deal with NA values
##Gcross_calculator warning message has been corrected to include samples that are removed from the analysis
##Neighborhood_Quantifier (STEP 6) now calculates the neighborhood density by sample
##Suggested_Tile_Size_Calculator (STEP 4 and 6) has been improved (size of line substituted by linewidth) to avoid annoying warning message
##Closest_neighbor_calculator and Neighborhood_voting_function (STEP 6) have been debugged to account for samples without COO or target cells



#VERSION 1.1.0
##Thresholding exploration function (STEP 2) has been improved to work with multi-level thresholded variables
##Thresholding_summary_function (STEP 2) has been improved to work with multi-level thresholded variables
##Minor bugs corrected in clustering phenotyper (STEP 3) in k_means_meta_clustering
##Compatibility of Marker_combinator_generator (STEP 3) function with multi-level thresholded variables has been checked
##New functions to perform heterogeneity analysis based on image texture have been implemented (STEP 4)
##New heterogeneity metric has been added (Gini index) has been added to all heterogeneity analytical tools (STEP 4)
##New functions to plot an image that has been previously tiled (STEP 4)
##New function to fit multi-level models to predict cell to cell interactions according to clinical variables (STEP 5)
##Distance_matrix_generator function (STEP 5) has been modified to allow same phenotype cell-cell interaction
##Distance analyzer function (STEP 5) now allows interrogating data with NA values
##Distance analyzer function (STEP 5) now produces plots with 0 bound CI. CI are now expressed as errorbars and not like simple geom_segments 
##New functions to compute UTAG-based neighborhoods (STEP 6)
##Cell_functional_assessment function has been created (many STEPs) to correlate features with functional markers

#VERSION 1.1.1
##TRIO analysis (STEP 5) has been implemented
##Cells_in_Radius_analyzer (STEP 5) has been improved. 05CI is now 0 bound and the plot now uses geom_errorbar in stead of geom_segment
##Distance_analysis and Cells_in_Radius (STEP 5) now have a graph maker available
##TRIO Min_Distance_analysis analysis and TRIO_score_in_Radius (STEP 5) now have a graph maker available

#VERSION 1.1.2
##conflicts with select between dplyr and raster packages have been solved (multiple STEPs)
##Distance_analyzer function (STEP 5) has been improve to allow calculating experiment-wise random distributions
##Cells in radius analyzer function (STEP 5) has been improve to implement experiment-wise random distributions
##Trio_Cells_in_Radius_analyzer and Trio_Min_Distance_analyzer (STEP 5) have been improve to implement experiment-wise random distributions

#VERSION 1.1.3
##Minor bug in Cells_in_Radius_analyzer (STEP 5) caused by defective call of RANDOM_cumulative_distance object has been solved

#VERSION 1.1.4
##Clinical_Data_analyzer (Many STEPs) has been improved. Several different tibbles are build for each DATA variable in stead of a single tibble. This improves visualization
##Thresholding_summary_function (STEP 2) has been modified to acount for experiments analyzing multiple markers. If more than 10 markers are present a graph is exported to the wd



#VERSION 1.2
##Thresholding_graphicator_function (STEP 2) has been implemented. It allows to graphicate individual marker intensity data and cells considered to be positive
##Closest_neighbor_calculator (STEP 6) has been updated. The function now calculates correlation between  cells in every neighborhood. This can be interpreted as an atraction/rejection pattern
##Histocat related neighborhood finder (STEP 6) has been implemented, along with a dedicated graphicator
##Tailored_Closest_neighbor_calculator (STEP 6) has been implemented. It allows to use a user defined definition of neighbor
##Interaction_counter and interaction_analyzer (STEP 6) have been implemented. It allows the user to find recurrent patterns of multiple cell-cell interactions in tissue
##Quick_Tumor_Stroma_identifier (STEP 6) has been implemented
##DBSCAN_Tumor_Stroma_identifier (STEP 6) has been implemented
##clusterApplyLB is now the method of choice to paralellize computations (multiple STEPs). It allows workload balancing to speed up calculations

#VERSION 1.2.1
##Data_arrange_function (STEP 0) has been improved to account for a potential bug when cells were not ordered by image in the original dataset
##Kmeans and user pre-defined trehold have been implemented in Thresholding_function and Thresholding_function_tailored (STEP 2)
##Thresholding_summary_function (STEP 2) has been modified to avoid a bug in plot paneling
##Gaussian mixture models have been implemented in Clustering_Phenotyper (STEP 3)
##Cumulative_Interaction_generator (STEP 5) has been modified to be compatible with ClusterLB parallelization
##Trio_Cumulative_Interaction_generator (STEP 5) has been modified to be compatible with ClusterLB parallelization
##UTAG_message_passing (STEP 6) has been updated to allow the user to decide the weighting of the final mixing between COO and neighbor cells
##UTAG_Neighborhood_identifier (STEP 6) has been updated to support Gaussian Mixture models (GMM)
##LISA__Tumor_Stroma_identifier (STEP 6) has been implemented. It allows tissue segmentation into three compartments (tissue, stroma and border)
##Advanced_Tumor_Stroma_identifier (STEP 6) has been implemented. It allows tissue segmentation into three compartments (tissue, stroma and border)
##Compartment_Phenotype_quantifier (STEP 6) has been implemented. It allows phenotype by sample and compartment quantification



#VERSION 1.3
##Segmentation_tester and Cell_segmentator_quantificator (STEP 0) functions have been implemented to allow image segmentation from MULTI-TIFF immunofluorescence images
##shiny_app_launcher function (STEP 2) has been implemented to allow the exploration of different thresholding methodologies, comparing Images and outcomes

#VERSION 1.3.1
##Bug solved in Distance_matrix_generator (STEP 5) that avoided choosing the DATA source to compute the distance matrix
##Bug solved in Image_tiling_processing_function (multiple STEPs) that caused an error when assigning names to the list
##Attachment of packages to the session has been reduced significantly (multiple STEPs). This will reduce the RAM memory demands, as far as Rstudio settings are adequate
##Minor bug corrected in Texture_features_calculator (STEP 4)
##Feedback messages have been implemented for most of the functions. This will allow the user to know how is the computation doing (multiple STEPs)

#Version 1.3.2
##All functions now include argument checkers to improve feedback and apply early function stops (multiple STEPs)

#Version 1.3.3
##Minor bugs corrected in SHINY_Thresholding_summary_function and shiny_app_launcher (STEP 2)
##In addition, shiny_app_launcher now includes interactive graphs and a summary of the total number of cells positive for each marker (STEP2)

#Version 1.3.4
##Normalization_function (STEP 1) now supports parallel computing it can speed up heavy computation and will only marginally slow down low demanding tasks
##Suggested_Tile_Size_Calculator (STEP 4 and 6) now returns a summary of the sample if data preview is required
##DATA_neighborhoods (STEP 6) now can calculate neighborhoods using new different clustering strategies
##Marker_combinator_generator (STEP 3) now has a new argument checker that accounts for non-logical inputs
##Tiled_Image_Clustering_function (STEP 6) now supports clustering with more strategies and the possibility of performing clustering using all observations
##Closest_neighbor_calculator  (STEP 6)now supports the option to include or not the COO in the neighbors counts
##Tailored_Closest_neighbor_calculator (STEP 6) now supports the option to include or not the COO in the neighbors counts. In addition, it returns both neighbor percentage and absolute number
##CELESTA phenotyping algorithm has been implemented through theCELESTA_template_generator and CELESTA_phenotyper functions (STEP 3)
##Phenotyping_evaluator_shiny_app_launcher (STEP 3) has been implemented to explore phenotyping results
##Minor bug in Clustering_Phenotyper (STEP 3) solved (bugged argument chekcer for batched K means)
##shiny_app_launcher (STEP 2) layout has been improved removing legend from positive cell plot
##classic Normalization_function (STEP 1) has been reintroduced (it's faster in most scenarios). Parallelized normalization can now be performed using Normalization_function_parallel
##A minor bug in Normalization_function_parallel (STEP 1) (related to resulting data structure) has been solved
##A bug in Clustering_Phenotyper (STEP 3) related to CLARA clustering has been solved
##Concordance_calculator (STEP 3) has been implemented. It calculates rand index between clustering techniques to check concordance
##Normalization_diagnostics (STEP 1) has been implemented. It compares Normalized data with non-normalized data



#VERSION 1.4
##Code is now sourced to speed up the import of different functions
##Normalization_function and Normalization_function_parallel now support mxnorm normalization with 2 sources of variability

#VERSION 1.4.1
##The UPDATE LOG is now placed in the SOURCE file
##The SOURCE file has been cleaned. No duplicated functions exist
##Bug in Concordance_calculator (STEP 3) that occurred with list names containing '_' has been solved
##shiny_app_launcher (STEP 2) has now been improved (areas can be zoomed in based on image)
##Phenotyping_evaluator_shiny_app_launcher (STEP 3) has been improved (areas can be zoomed in based on image)

#VERSION 1.4.2
##shiny_app_launcher (STEP 2) has been improved (more precise image / cell alignment and improved color layout)
##Phenotyping_evaluator_shiny_app_launcher (STEP 3) has been improved (more precise image / cell alignment and improved color layout)

#VERSION 1.4.3
##shiny_app_launcher (STEP 2) has been improved (overlay between positive cells and image and also between cell intensities and image)
##Phenotyping_evaluator_shiny_app_launcher (STEP 3) has been improved (overlay between selected phenotypes and image and improved gt::table)
##segmentator_tester_app (STEP 0) is now available. It allows the user to interactively explore the parameters that govern cell segmentation
##Neighborhood_discovery_function (STEP 6) now allows the user to decide if dimension reduction must be performed or not and the scaling step has been modified to be more accurate when removing duplicated values
##Neighborhood_discovery_function (STEP 6) now allows the user to use graph clustering on dimension reduction data
##Tiled_Image_Clustering_function (STEP 6) has been updated. The scaling process now ocurs before clustering to improve results when removing duplicates
##Tiled_Image_Clustering_function (STEP 6) now allows the user to use graph clustering on dimension reduction data 
##Improved visualization of functions that require clustering (many STEPs)

#VERSION 1.4.4
##Phenotyping_evaluator_shiny_app_launcher(STEP 3) has changed the layout and now all the phenotypes are overalayed on the image
##Barplot_generator (STEP 3) now allows to graph up to 36 colors
##Tiled_image_heterogeneity_calculator (STEP 4) is now safer, removing images without evaluable tiles and computes differently NA values to allow NaN values that are sometimes computed in Gini
##Heterogeneity_by_tile_Analysis (STEP 4) is now safer when it comes to NA value handling
##Tiled_image_heterogeneity_analyzer, Tiled_image_heterogeneity_calculator, Global_heterogeneity_calculator (STEP 4) now calculate heterogeneity metrics in a safer way (to avoid single column misscalculations)
##RcolorBrewer 'pastel' palette is no longer used. It has been replaced by pals::polychrome, that allows up to 36 colors
##Tiled_image_heterogeneity_graph_maker (STEP 4) now graphs the tiles in the background for improved visualization
##Bug in Distance_matrix_generator (STEP 5) has been solved. It did not import correctly desired data to the execution environment
##Bug in Interaction_analyzer (STEP 6) has been solved regarding distance argument check

#VERSION 1.4.5
##DATA_arrange_function (STEP 0) now generates a unique cell_no combining numbering and Subject_Names
##shiny_app_launcher (SETP 2) has been renamed to Thresholding_tester_app
##Clustering_Phenotyper (STEP 3) has been improved to allow dimension reduction and performing clustering on dimension reduced data
##Bug in Image_plotter (STEP 3) related to point color has been solved
##Global_heterogeneity_calculator (STEP 4) now includes KL and JSD divergence metrics (they calculate divergence from absolute homogeneity and from experiment wise cell distribution)
##Tiled_image_heterogeneity_calculator (STEP 4) now includes KL and JSD divergence metrics (they calculate tile divergence from global cell type distribution)
##Distance_matrix_generator (STEP 5) now allows the user to remove COO from border
##Random_Distance_matrix_generator (STEP 5) now allows the user to remove random COO from border
##Distance_analyzer (STEP 5) and Cells_in_Radius_analyzer (STEP 5) now check if observed and random matrices have the same intersecting samples
##Trio analysis (STEP 5) including the following functions Trio_Distance_matrix_generator Trio_Random_Distance_matrix_generator Trio_Min_Distance_analyzer Trio_Cells_in_Radius_analyzer is now adapted to perform edge correction
##Multi_level_modeling_function (STEP 5) has been updated to avoid error return when calculating R^2
##UTAG_message_passing (STEP 6) has now been updated to include SUM-based message passing and to account for different neighboring definitions and weighting strategies. In addition the parallelization strategies have been modified
##Data_closes_neighbors (STEP 6) has been removed. Only Tailored_Closest_neighbor_calculator remains.
##Tailored_Closest_neighbor_calculator (STEP 6) has been updated with a new parallelization strategy. Now threads work on observations in stead of samples
##Neighborhood_discovery_function (STEP 6) has been updated to include the new dimension reduction standards and the remove_duplicated option has been removed
##Tiled_Image_Clustering_function (STEP 6) has been updated to include the new dimension reduction standards and the remove_duplicated option has been removed

#VERSION 1.4.6
##Bug in Neighborhood_discovery_function (STEP 6) related to UMAP and tSNE calculation has been removed
##Thresholding_tester_app (STEP 2) has been improved to allow the user to use the app without available images and to allow pixel/distance conversion
##Phenotyping_evaluator_shiny_app_launcher (STEP 3) has been improved to allow the user to use the app without available images and to allow pixel/distance conversion
##Bug in Trio_graph_maker(STEP 5) related to trio score in radius graphication has been resolved. Bug occurred when graphicating data with edge correction.
##Bug in Cell_to_Cell_graph_maker (STEP 5) related to ocurred to graphication after edge correction.
##Bug in Clinical_Data_analyzer (Many STPEPs) related to correlation calculation has been corrected

#VERSION 1.4.7
##Future and purrr are the standard for paralellization (Many STEPs)
##Clinical_Data_analyzer (Many STEPs) now returns summary of CoxPH models in addition to KM plots
##EBImage_color_thresholder, Channel_deconvolution_function, Color_deconvolution_App_launcher and Image_deconvolution_function (STEP 0) are now available. They allow the user to work with RGB images obtained from IHC or histochemical sources
##Cell_in_edge_remover(STEP 0) is now available. It allows users to remove cells from border before performing analysis
##Segmentation_tester (STEP 0), segmentator_tester_app(STEP 0) and Cell_segmentator_quantificator(STEP 0) now allow the user to pre-process nuclear channels to improve cell segmentation results
##Bug in segmentator_tester_app (STEP 0) regarding cell border ploting has been solved
##Thresholding_tester_app (STEP 2) has been optimized to process images at higher speed
##Bug in Thresholding_function (STEP 2) regarding LOCAL multilevel thresholding has been solved
##Phenotyping_evaluator_shiny_app_launcher (STEP 3) has been optimize to process images at higher speed
##Multi_level_modelling_function (STEP 5) now allows the user to decide whether to perform pseudoR2 calculation or not (may be computationally intensive) and behaves differently if predictions cannot be calculated
##Graphication of >100000 cells (Many STEPS) has been limited to an absolute number of 100000
##Distance_analyzer (STEP 5) now allows parallel computation based on future
##Trio_Min_Distance_analyzer (STEP 5) now allows parallel computation based on future
##Bug in Gcross_calculator (STEP 5) related to argument check has been solved
##Bug in SPIAT_entropy_gradient_generator (STEP 5) related to argument check has been solved
##Bug in Neighborhood_voting_function (STEP 6) has been solved (typo in argument specification)
##Neighborhood_discovery_function (STEP 6) now also returns dimension reduction
##New module allows the analysis of in a pixel wise manner. This module includes the following functions:
##Tissue_mask_generator, Pixel_thresholder, Pixel_Multilevel_thresholder, MFI_calculator, Multi_mask_generator, Pixel_Threshold_calculator, 
##MFI_Experimet_Calculator, Cell_to_pixel_distance_calculator, Marker_segmentator, Cell_image_plot_generator, Image_thresholding_app_launcher

############################################################################