
library(neurobase)
library(shiny)
library(shinyFiles)
library(tidyverse)
library(stringi)
library(rintrojs)

regr_variables_ = c()
regr_variables_demo_ = c()
select_var = c()

img_betas = list()
slices_to_show = c()
img_cols = c()
plane = c()

paths_to_nii_ = c()
paths_to_nii_demo_ = c()

paths_temp = c()

ui = fluidPage(
  fluidRow(headerPanel(
    h3("Image on Scalar Regression Program", style = "color: #02169B; font-weight: normal; border: 1px")
  ),
  div(style = "height:55px; background-color: #F1F1F1; border-bottom: groove; border-color: #02169B; border-width: 3px;")),
  tags$style(HTML(".tabbable > .nav > li > a                  {background-color: #F1F1F1;  color:black}
                  .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}")),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(5%);
             left: calc(40%);
             }
             "
      )
    )
  ),
  introjsUI(),
  tabsetPanel(
    type = 'tabs',
    tabPanel('Home', 
             br(),
             br(),
             br(),
             sidebarPanel(wellPanel(
               h4(tags$b("Instructions")),
               hr(),
               h5(
                 'If you are new to the app, visit the `Demo` tab for a complete walk-through 
                 with examples. Otherwise, set mask image, outcome and clinical variables, and the brain
                 image directory. Then search/select a subject to explore and fit regression models.'
               )
             ), 
             wellPanel(
               h4(tags$b("Load data")),
               h5(
                 'Load a mask image data set, outcome and clinical variables data,
                  and select a brain image directory.'
               ),
               fileInput(
                 "mask_image",
                 "Load mask image",
                 multiple = TRUE,
                 accept = c(".nii")
               ),
               fileInput(
                 "clinical_vars",
                 "Load outcome variables and clinical variables",
                 multiple = TRUE,
                 accept = c(".csv")
               ),
               fileInput('dir2',
                         "Choose directory",
                         multiple = TRUE,
                         accept = c()
               )
             ),
             br(),
             wellPanel(
               h4(tags$b("Subject summary")),
               selectInput(
                 'select_patient_to_explore',
                 label = 'Search and select a subject to explore',
                 choices = c()
               ),
               actionButton('run', 'Run')
             )
             ,
             hr(),
             wellPanel(
               h4(tags$b("Image on Scalar Regression Analysis")),
               hr(),
               h5(
                 'Select predictors to use in the regression analysis by choosing them from the dropdown menu,
                  and clicking `Add variable to model` for each selection. Then click `fit model` to run the
                  regression analysis. The analyis may take minutes to run. Then, use `Select variable to view` to
                  change the parameter displayed.'
               ),
               selectInput('select_regr_vars', label = 'Select predictor variables for regression model:',
                           choices = c()),
               actionButton('add_regr_variable', 'Add variable to model'),
               textOutput('print_regr_variables'),
               hr(),
               actionButton('run_regression', 'Fit model'),
               selectInput('select_param_view', label = 'Select variable on which to view brain images:',
                           choices = c())
             )),
             mainPanel(
               wellPanel(style = 'background: #fcfcfc',
                 h4(tags$b('Data summary')),
                 hr(),
                 verbatimTextOutput('results_summary'),
                 plotOutput('results_image'),
                 hr()
               ),
               wellPanel(style = 'background: #fcfcfc',
                 h4(tags$b('Image on Scalar Regression results')),
                 hr(),
                 plotOutput('model_results_image'),
                 hr()
               )
             )),
    tabPanel('Demo',
             br(),
             br(),
             fluidRow(
               column(width = 4,
                      wellPanel(
                        h5('Welcome to the interactive demo! To get started, click the `Start Demo` button.'),
                        actionButton('start_interactive_demo', 'Start Demo')
                      )
               ),
               column(width = 5,
                      introBox(
                        wellPanel(
                          h5('Data sets for the demo. Click `Start Demo` for further instructions.
                             The mask image and brain scans directory are zipped, so they must be unzipped before
                             being uploaded.'),
                          introBox(
                            downloadButton('download_mask', 'Download mask image'),
                            data.step = 4, data.intro = 'Click download. Find the download in your computer.
                            Right click the file and extract the file contents. Then, select the file with extension
                            .nii and upload it.'
                          ),
                          introBox(
                            downloadButton('download_covars', 'Download covariates'),
                            data.step = 6, data.intro = 'Download the covariates and clinical variables
                            data set. The file you download has the extension .csv and does not require any other 
                            processing to be uploaded.'
                          ),
                          introBox(
                            downloadButton('download_brain_scans', 'Download brain scans directory'),
                            data.step = 8, data.intro = 'Download the directory with each brain scan file 
                            for the individuals of interest. This file is zipped. Extract it like you did the 
                            first file. The file should contain 10 .nii.gz files. Select all of them by selecting all
                            the files, and ckick OK.'
                          ),
                        ),
                        data.step = 3, data.intro = 'Here you will download and prepare the data you will need
                        to run this app. Your data will have to follow the formats we use in order to work properly.'
                      )
               )
             ),
             br(),
             introBox(
               sidebarPanel(wellPanel(
                 h4(tags$b("Instructions")),
                 hr(),
                 h5(
                   'If you are new to the app, visit the `Demo` tab for a complete walk-through 
                    with examples. Otherwise, set mask image, outcome and clinical variables, and the brain
                   image directory. Then search/select a subject to explore and fit regression models.'
                 )
               ), 
               introBox(
                 wellPanel(
                   h4(tags$b("Load data")),
                   h5(
                     'Load a mask image data set, outcome and clinical variables data,
                      and select a brain image directory.'
                   ),
                   introBox(
                     fileInput(
                       "mask_image_demo",
                       "Load mask image",
                       multiple = TRUE,
                       accept = c(".nii")
                     ), data.step = 5, data.intro = 'Upload the data set you just downloaded by
                     navigating to it in the file explorer that appears when you click the button. When
                     the file is successfuly uploaded, the blue bar will fill up. Remember to extract the
                     .zip file you downloaded before attempting to upload the file.'
                   ),
                   introBox(
                     fileInput(
                       "clinical_vars_demo",
                       "Load outcome variables and clinical variables",
                       multiple = TRUE,
                       accept = c(".csv")
                     ), data.step = 7, data.intro = 'Just like in the last data upload, click the 
                     button to navigate to the file on your computer to upload it to the program. The
                     file should have the extension .csv.'
                   ),
                   introBox(
                     fileInput('dir2_demo',
                               "Choose directory",
                               multiple = TRUE,
                               accept = c()
                     ), data.step = 9, data.intro = 'This time you have to upload the entire directory
                     you downloaded, so navigate to the location, but select all the files you just downloaded
                     and click ok. You should have selected 10 files with the extension .nii.gz. Remember to extract
                     the .zip archive and navigate into the directory before selecting files to upload.'
                   )
                 ), data.step = 2, data.intro = 'In this section, you will load the mask image data,
                 the outcome variables a clinical variables data, and choose all individuals\' brain scan files from a directory. 
                 We will walk you through each step with example data.'
               )
               ,
               br(),
               introBox(
                 wellPanel(
                   h4(tags$b("Subject summary")),
                   selectInput(
                     'select_patient_to_explore_demo',
                     label = 'Search and select a subject to explore',
                     choices = c()
                   ),
                   actionButton('run_demo', 'Run')
                 ), data.step = 10, data.intro = 'Select a subject from the dropdown on which
                 to explore summary data. Then, the summary will appear in the `Data summary` window.'
               ),
               hr(),
               introBox(
                 wellPanel(
                   h4(tags$b("Image on Scalar Regression Analysis")),
                   hr(),
                   h5(
                     'Select predictors to use in the regression analysis by choosing them from the dropdown menu,
                      and clicking `Add variable to model` for each selection. Then click `fit model` to run the
                      regression analysis. The analyis may take minutes to run. Then, use `Select variable to view` to
                      change the parameter displayed.'
                   ),
                   introBox(
                     selectInput('select_regr_vars_demo', label = 'Select predictor variables for regression model:',
                                 choices = c()),
                     actionButton('add_regr_variable_demo', 'Add variable to model'),
                     textOutput('print_regr_variables_demo'),
                     data.step = 13, data.intro = 'In this section, you can choose the independent variables to 
                     include in the regression analysis. Select one at a time from the dropdown menu and click 
                     `Add variable to model` to update the model with your new selection. Each indenpendent variable
                     will show in a comma separated list below the `Add variable` button.'
                   ),
                   hr(),
                   introBox(
                     actionButton('run_regression_demo', 'Fit model'),
                     data.step = 14, data.intro = 'Click this button to fit the model. These models require wait time
                     to run, so expect to wait a few minutes or more.'
                   ),
                   introBox(
                     selectInput('select_param_view_demo', label = 'Select variable on which to view brain images:',
                                 choices = c()),
                     data.step = 16, data.intro = 'Use the dropdown to select parameters one at a time to view
                     their effects on brain images. Once you have made a selection, click'
                   )
                 ), data.step = 12, data.intro = 'In this section, you can choose the independent variables to 
                    include in the regression analysis, run the analysis and select ways of viewing the results.'
               )
               ),
               data.step = 1, data.intro = 'This is the sidebar. It contains all tools and options
               used to run the app'
             ),
             mainPanel(
               introBox(
                 wellPanel(style = 'background: #fcfcfc',
                           h4(tags$b('Data summary')),
                           hr(),
                           verbatimTextOutput('results_summary_demo'),
                           plotOutput('results_image_demo'),
                           hr()
                 ), data.step = 11, data.intro = 'This panel displays the summary information 
                 for the selected subject. Once the regression model is fitted, additional summary statistics
                 will show here as well.'
               ),
               introBox(
                 wellPanel(style = 'background: #fcfcfc',
                           h4(tags$b('Image on Scalar Regression results')),
                           hr(),
                           introBox(
                             plotOutput('model_results_image_demo'),
                             data.step = 17, data.intro = 'The update image based on the variable you select
                             will display here. Try a few selections to see how the image changes.'
                           ),
                           hr()
                 ),
                 data.step = 15, data.intro = 'When the model is fitted, results will display in this window.'
               )
             )
          ),
    tabPanel('About',
             HTML('This program was developed at the University of Michigan Department of Biostatistics.'),
             HTML('<br> References and further reading: publication in development')),
    tabPanel('Download',
             'Download for offline use coming soon!')
  ),
  
)

server = function(input, output, session) {
  
  showNotification('Click the `Demo` tab for a full demonstration of the app!', 
                   type = 'message', duration = 7)
  
  observeEvent(input$start_interactive_demo, {
    introjs(session, options = list('nextLabel' = 'Next', 'prevLabel' = 'Previous', 'skipLabel' = 'Skip'),
            events = list('oncomplete' = I('alert("Congratulations, you completed this demonstration!")')))
  })
  
  AAL_mask_download = readnii('data_demo/AAL_90_3mm.nii')
  output$download_mask <- downloadHandler(
    filename = function() {
      paste('mask-', Sys.Date(), '.zip', sep='')
    },
    content = function(con) {
      fl = paste('AAL_90_3mm-', Sys.Date(), '.nii', sep='')
      writenii(AAL_mask_download, filename = fl, gzipped = FALSE)
      zip(zipfile = con, files = fl)
    },
    contentType = 'application/zip'
  )
  
  BioStats_phenos = read.csv('data_demo/BioStats_phenos_demo.csv')
  output$download_covars <- downloadHandler(
    filename = function() {
      paste('BioStats_phenos-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(BioStats_phenos, con)
    }
  )
  
  output$download_brain_scans <- downloadHandler(
    filename = function() {
      paste('imaging_data-', Sys.Date(), '.zip', sep='')
    },
    content = function(con) {
      example_imaging_names = c('sub-N1_2bk-baseline_con_3mm.nii.gz',
                                'sub-N2_2bk-baseline_con_3mm.nii.gz',
                                'sub-N3_2bk-baseline_con_3mm.nii.gz',
                                'sub-N4_2bk-baseline_con_3mm.nii.gz',
                                'sub-N5_2bk-baseline_con_3mm.nii.gz',
                                'sub-N6_2bk-baseline_con_3mm.nii.gz',
                                'sub-N7_2bk-baseline_con_3mm.nii.gz',
                                'sub-N8_2bk-baseline_con_3mm.nii.gz',
                                'sub-N9_2bk-baseline_con_3mm.nii.gz',
                                'sub-N10_2bk-baseline_con_3mm.nii.gz')
      for (name in example_imaging_names) {
        temp_image = readnii(paste('data_demo', name, sep = '/'))
        writenii(temp_image, filename = name)
      }
      zip(zipfile = con, files = example_imaging_names)
    },
    contentType = 'application/zip'
  )
  
  observeEvent(input$dir2, {
    dir.create(paste(getwd(), 'temp_fls', sep = '/'))
    paths_to_nii_ <<- paste(paste(getwd(), 'temp_fls', sep = '/'), input$dir2$name, sep = '/')
    file.copy(input$dir2$datapath, paths_to_nii_)
    patient_list_update()
  })
  
  observeEvent(input$dir2_demo, {
    dir.create(paste(getwd(), 'temp_fls_demo', sep = '/'))
    paths_to_nii_demo_ <<- paste(paste(getwd(), 'temp_fls_demo', sep = '/'), input$dir2_demo$name, sep = '/')
    file.copy(input$dir2_demo$datapath, paths_to_nii_demo_)
    patient_list_update_demo()
  })
  
  observeEvent(input$mask_image, {
    AAL_mask <<- readnii(input$mask_image$datapath)
  })
  
  observeEvent(input$mask_image_demo, {
    AAL_mask_demo <<- readnii(input$mask_image_demo$datapath)
  })
  
  observeEvent(input$clinical_vars, {
    phenos_dat <<- read.csv(input$clinical_vars$datapath)
    updateSelectInput(session,
                      'select_regr_vars',
                      label = 'Select predictor variables for regression model:',
                      choices = colnames(phenos_dat))
  })
  
  observeEvent(input$clinical_vars_demo, {
    phenos_dat_demo <<- read.csv(input$clinical_vars_demo$datapath)
    updateSelectInput(session,
                      'select_regr_vars_demo',
                      label = 'Select predictor variables for regression model:',
                      choices = colnames(phenos_dat_demo))
  })
  
  regr_selections = reactive({
    req(input$add_regr_variable)
    input$add_regr_variable
    isolate(regr_variables_ <<-
              c(regr_variables_, input$select_regr_vars))
    regr_variables_
  })
  
  regr_selections_demo = reactive({
    req(input$add_regr_variable_demo)
    input$add_regr_variable_demo
    isolate(regr_variables_demo_ <<-
              c(regr_variables_demo_, input$select_regr_vars_demo))
    regr_variables_demo_
  })
  
  output$print_regr_variables = renderText({
    toString(regr_selections())
  })
  
  output$print_regr_variables_demo = renderText({
    toString(regr_selections_demo())
  })
  
  model_fit_results = reactive({
    req(input$run_regression)
    input$run_regression
    suffix = "_2bk-baseline_con_3mm.nii.gz"
    mask = ifelse(AAL_mask > 0, 1, 0)
    voxels = which(mask == 1, arr.ind = TRUE)
    colnames(voxels) <- c("x", "y", "z")
    z_range = unique(voxels[, 3])
    z_show_slices = z_range[seq(10, 40, by = 2)]
    imgs = readnii(paste(imagepath, paste0(isolate(input$select_patient_to_explore), suffix), sep = '/'))
    imgsfiles = dir(imagepath)
    subjID = unlist(strsplit(imgsfiles, suffix))
    X_names = regr_variables_
    X = phenos_dat[match(subjID, as.character(phenos_dat[['Subject']])), X_names, drop = FALSE]
    
    matX = NULL
    nm_X = names(X)
    nms = NULL
    for (i in 1:ncol(X)) {
      if (is.factor(X[[i]])) {
        levels_X = levels(X[[i]])
        if (length(levels_X) > 1) {
          for (j in 2:length(levels_X)) {
            matX = cbind(matX, ifelse(as.character(X[[i]]) == levels_X[j], 1, 0))
            nms = c(nms, paste(nm_X[i], levels_X[j], sep = "_"))
          }
        }
      } else{
        matX = cbind(matX, X[[i]])
        nms = c(nms, nm_X[i])
      }
    }
    
    imgdat = matrix(NA, nrow = length(imgsfiles), ncol = sum(mask == 1))
    for (i in 1:length(imgsfiles)) {
      img = readnii(file.path(imagepath, imgsfiles[i]))
      img[] = ifelse(is.nan(img[]), 0, img[])
      img[] = ifelse(mask[] == 1, img[], NaN)
      imgdat[i, ] = img[mask[] == 1]
    }
    
    sigma2_beta = 0.01
    XtX = crossprod(matX)
    XtY = crossprod(matX, imgdat)
    beta0 = solve(XtX + sigma2_beta * diag(nrow(XtX)), XtY)
    beta = beta0
    for (j in 1:ncol(beta)) {
      beta[, j] = ifelse(beta0[, j] > quantile(abs(beta0[, j]), prob = 0.95), beta0[, j], 0)
    }
    
    img_temp = readnii(file.path(imagepath, imgsfiles[1]))
    for (j in 1:nrow(beta)) {
      img_betas[[j]] <<- img_temp
      img_betas[[j]][which(mask[] == 1)] <<- beta[j, ]
      img_betas[[j]][which(mask[] != 1)] <<- NaN
    }
    names(img_betas) <<- nms
    
    img_cols <<- colorRampPalette(c("blue", "yellow", "red"))(256)
    #show results
    select_var <<- nms[1]
    slices_to_show <<- c(15, 25, 35, 45)
    plane <<- "coronal" #"coronal", "sagittal"
    
    updateSelectInput(session = session,
                      'select_param_view',
                      choices = nms)
    
    output$results_summary = renderPrint({
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      subjID = unlist(strsplit(imgsfiles, suffix))
      X_names = regr_variables_
      X = phenos_dat[match(subjID, as.character(phenos_dat[['Subject']])), X_names, drop = FALSE]
      print(summary(X))
    })
  })
  
  model_fit_results_demo = reactive({
    req(input$run_regression_demo)
    input$run_regression_demo
    suffix = "_2bk-baseline_con_3mm.nii.gz"
    mask = ifelse(AAL_mask_demo > 0, 1, 0)
    voxels = which(mask == 1, arr.ind = TRUE)
    colnames(voxels) <- c("x", "y", "z")
    z_range = unique(voxels[, 3])
    z_show_slices = z_range[seq(10, 40, by = 2)]
    imgs = readnii(paste(imagepath_demo, paste0(isolate(input$select_patient_to_explore_demo), suffix), sep = '/'))
    imgsfiles = dir(imagepath_demo)
    subjID = unlist(strsplit(imgsfiles, suffix))
    X_names = regr_variables_demo_
    X = phenos_dat_demo[match(subjID, as.character(phenos_dat_demo[['Subject']])), X_names, drop = FALSE]
    
    matX = NULL
    nm_X = names(X)
    nms = NULL
    for (i in 1:ncol(X)) {
      if (is.factor(X[[i]])) {
        levels_X = levels(X[[i]])
        if (length(levels_X) > 1) {
          for (j in 2:length(levels_X)) {
            matX = cbind(matX, ifelse(as.character(X[[i]]) == levels_X[j], 1, 0))
            nms = c(nms, paste(nm_X[i], levels_X[j], sep = "_"))
          }
        }
      } else{
        matX = cbind(matX, X[[i]])
        nms = c(nms, nm_X[i])
      }
    }
    
    imgdat = matrix(NA, nrow = length(imgsfiles), ncol = sum(mask == 1))
    for (i in 1:length(imgsfiles)) {
      img = readnii(file.path(imagepath_demo, imgsfiles[i]))
      img[] = ifelse(is.nan(img[]), 0, img[])
      img[] = ifelse(mask[] == 1, img[], NaN)
      imgdat[i, ] = img[mask[] == 1]
    }
    
    sigma2_beta = 0.01
    XtX = crossprod(matX)
    XtY = crossprod(matX, imgdat)
    beta0 = solve(XtX + sigma2_beta * diag(nrow(XtX)), XtY)
    beta = beta0
    for (j in 1:ncol(beta)) {
      beta[, j] = ifelse(beta0[, j] > quantile(abs(beta0[, j]), prob = 0.95), beta0[, j], 0)
    }
    
    img_temp = readnii(file.path(imagepath_demo, imgsfiles[1]))
    for (j in 1:nrow(beta)) {
      img_betas[[j]] <<- img_temp
      img_betas[[j]][which(mask[] == 1)] <<- beta[j, ]
      img_betas[[j]][which(mask[] != 1)] <<- NaN
    }
    names(img_betas) <<- nms
    
    img_cols <<- colorRampPalette(c("blue", "yellow", "red"))(256)
    #show results
    select_var <<- nms[1]
    slices_to_show <<- c(15, 25, 35, 45)
    plane <<- "coronal" #"coronal", "sagittal"
    
    updateSelectInput(session = session,
                      'select_param_view_demo',
                      choices = nms)
    
    output$results_summary_demo = renderPrint({
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      subjID = unlist(strsplit(imgsfiles, suffix))
      X_names = regr_variables_demo_
      X = phenos_dat_demo[match(subjID, as.character(phenos_dat_demo[['Subject']])), X_names, drop = FALSE]
      print(summary(X))
    })
    
  })
  
  output$model_results_image = renderPlot({
    model_fit_results()
    new_view_var_ = new_view_variable()
    if (input$select_param_view %>% str_length == 0) {
      return(
        image(
          img_betas[[select_var]],
          z = slices_to_show,
          plot.type = "single",
          col = img_cols,
          plane = plane
        )
      )
    } else {
      return(
        image(
          img_betas[[new_view_variable()]],
          z = slices_to_show,
          plot.type = "single",
          col = img_cols,
          plane = plane
        )
      )
    }
  }, bg = 'black')
  
  output$model_results_image_demo = renderPlot({
    model_fit_results_demo()
    new_view_var_demo_ = new_view_variable_demo()
    if (input$select_param_view_demo %>% str_length == 0) {
      return(
        image(
          img_betas[[select_var]],
          z = slices_to_show,
          plot.type = "single",
          col = img_cols,
          plane = plane
        )
      )
    } else {
      return(
        image(
          img_betas[[new_view_variable_demo()]],
          z = slices_to_show,
          plot.type = "single",
          col = img_cols,
          plane = plane
        )
      )
    }
  }, bg = 'black')
  
  new_view_variable = reactive({
    req(input$select_param_view %>% str_length > 0)
    if (is.null(input$select_param_view) &
        !(is_empty(select_var))) {
      return(select_var)
    } else if (!is.null(input$select_param_view)) {
      return(input$select_param_view)
    }
  })
  
  new_view_variable_demo = reactive({
    req(input$select_param_view_demo %>% str_length > 0)
    if (is.null(input$select_param_view_demo) &
        !(is_empty(select_var))) {
      return(select_var)
    } else if (!is.null(input$select_param_view_demo)) {
      return(input$select_param_view_demo)
    }
  })
  
  patient_list_update = reactive({
    req(length(paths_to_nii_) > 0)
    input$dir2$datapath
    input$mask_image$datapath
    paths_temp = unlist(str_split(paths_to_nii_[1], pattern = '/'))
    paths_temp = paths_temp[-length(paths_temp)]
    paths_temp <<- stri_paste(paths_temp, collapse = '/')
    imagepath <<- paths_temp
    if (length(imagepath) > 0) {
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      imgsfiles = dir(stri_paste(imagepath, collapse = '/'))
      subjID = unlist(strsplit(imgsfiles, suffix))
      updateSelectInput(inputId = 'select_patient_to_explore',
                        choices = subjID,
                        session = session)
    }
    return()
  })
  
  patient_list_update_demo = reactive({
    req(length(paths_to_nii_demo_) > 0)
    input$dir2_demo$datapath
    input$mask_image_demo$datapath
    paths_temp = unlist(str_split(paths_to_nii_demo_[1], pattern = '/'))
    paths_temp = paths_temp[-length(paths_temp)]
    paths_temp <<- stri_paste(paths_temp, collapse = '/')
    imagepath_demo <<- paths_temp
    if (length(imagepath_demo) > 0) {
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      imgsfiles = dir(stri_paste(imagepath_demo, collapse = '/'))
      subjID = unlist(strsplit(imgsfiles, suffix))
      updateSelectInput(inputId = 'select_patient_to_explore_demo',
                        choices = subjID,
                        session = session)
    }
    return()
  })
  
  output$results_image = renderPlot({
    req(input$run)
    suffix = "_2bk-baseline_con_3mm.nii.gz"
    mask = ifelse(AAL_mask > 0, 1, 0)
    voxels = which(mask == 1, arr.ind = TRUE)
    colnames(voxels) <- c("x", "y", "z")
    z_range = unique(voxels[, 3])
    z_show_slices = z_range[seq(10, 40, by = 2)]
    imagepath <<- stri_paste(imagepath, collapse = '/')
    imgs = readnii(paste(imagepath, paste0(input$select_patient_to_explore, suffix), sep = '/'))
    image(imgs, z = z_show_slices, plot.type = "single")
  }, bg = 'black')
  
  output$results_image_demo = renderPlot({
    req(input$run_demo)
    suffix = "_2bk-baseline_con_3mm.nii.gz"
    mask = ifelse(AAL_mask_demo > 0, 1, 0)
    voxels = which(mask == 1, arr.ind = TRUE)
    colnames(voxels) <- c("x", "y", "z")
    z_range = unique(voxels[, 3])
    z_show_slices = z_range[seq(10, 40, by = 2)]
    imagepath_demo <<- stri_paste(imagepath_demo, collapse = '/')
    imgs = readnii(paste(imagepath_demo, paste0(input$select_patient_to_explore_demo, suffix), sep = '/'))
    image(imgs, z = z_show_slices, plot.type = "single")
  }, bg = 'black')
}

shinyApp(ui = ui, server = server)
