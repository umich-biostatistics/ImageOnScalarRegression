
# pkgs = as.character(installed.packages()[, 'Package'])
# req.pkgs = c('neurobase', 'shiny', 'shinyFiles', 'tidyverse')
# sapply(req.pkgs, function(x) {
#   if (!(x %in% pkgs))
#     install.packages(x)
# })

library(neurobase)
library(shiny)
library(shinyFiles)
library(tidyverse)
library(stringi)

regr_variables_ = c()
select_var = c()

img_betas = list()
slices_to_show = c()
img_cols = c()
plane = c()

paths_to_nii_ = c()


paths_temp = c() # path up to the nii files

ui = fluidPage(
  fluidRow(headerPanel(
    h3("Image on Scalar Regression", style = "color: #02169B; font-weight: bold;")
  ),
  div(style = "height:72px; background-color: #F1F1F1;")),
  br(),
  br(),
  br(),
  sidebarPanel(wellPanel(
    h4(tags$b("Instructions")),
    hr(),
    h4(
      'Set mask image, outcome and clinical variables, and the brain
              image directory. Then, search and select a subject to explore.'
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
  )
  
  ),
  mainPanel(
    h4('Data summary:'),
    hr(),
    verbatimTextOutput('results_summary'),
    plotOutput('results_image'),
    h4('Image on Scalar Regression results:'),
    hr(),
    plotOutput('model_results_image')
  )
)

server = function(input, output, session) {
  # volumes <- getVolumes()
  # shinyDirChoose(input,
  #                'dir',
  #                roots = volumes,
  #                session = session)
  # 
  # mdat_path <- reactive({
  #   return(parseDirPath(volumes, input$dir))
  # })
# 
#   output$mdat_path_display = renderText({
#     req(input$dir)
#     dir_ = unlist(input$dir)
#     dir2_ = dir_[-length(dir_)]
#     dir_start_ = dir_[length(dir_)]
#     dir_new_ = c(dir_start_, dir2_)
#     return(paste0(dir_new_, sep = '/'))
#   })
  
  observeEvent(input$dir2, {
    #print(paste(getwd(), input$dir2$name, sep = '/'))
    dir.create(paste(getwd(), 'temp_fls', sep = '/'))
    paths_to_nii_ <<- paste(paste(getwd(), 'temp_fls', sep = '/'), input$dir2$name, sep = '/')
    #print("ABCEDF:")
    #print(paths_to_nii_)
    file.copy(input$dir2$datapath, paths_to_nii_)
    patient_list_update()
    #f = readnii(input$dir2$datapath[1])
  })
  
  observeEvent(input$mask_image, {
    AAL_mask <<- readnii(input$mask_image$datapath)
  })
  
  observeEvent(input$clinical_vars, {
    phenos_dat <<- read.csv(input$clinical_vars$datapath)
    #print(input$clinical_vars$datapath)
    updateSelectInput(session,
                      'select_regr_vars',
                      label = 'Select predictor variables for regression model:',
                      choices = colnames(phenos_dat))
  })
  
  regr_selections = reactive({
    req(input$add_regr_variable)
    input$add_regr_variable
    isolate(regr_variables_ <<-
              c(regr_variables_, input$select_regr_vars))
    regr_variables_
  })
  
  output$print_regr_variables = renderText({
    toString(regr_selections())
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
    #print("imagepath-----------------------------------------------------------------------")
    #print(imagepath)
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
      #print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA:::::")
      #print(imagepath)
      #print(imgsfiles[i])
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
      #req(input$run)
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      subjID = unlist(strsplit(imgsfiles, suffix))
      X_names = regr_variables_
      X = phenos_dat[match(subjID, as.character(phenos_dat[['Subject']])), X_names, drop = FALSE]
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
          # main = list(select_var, col = "white"),
          # mar = rep(1, 4)
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
          # main = list(select_var, col = "white"),
          # mar = rep(1, 4)
        )
      )
    }
    
    
  }, bg = 'black') #, main = list(select_var, col = "white"), mar = rep(1, 4))
  
  new_view_variable = reactive({
    req(input$select_param_view %>% str_length > 0)
    if (is.null(input$select_param_view) &
        !(is_empty(select_var))) {
      return(select_var)
    } else if (!is.null(input$select_param_view)) {
      return(input$select_param_view)
    }
  })
  
  patient_list_update = reactive({
    #req(input$mask_image$datapath > 0)
    #print('test this::::')
    #print(length(paths_to_nii_))
    req(length(paths_to_nii_) > 0)
    input$dir2$datapath
    input$mask_image$datapath
    #imagepath <<- mdat_path()
    paths_temp = unlist(str_split(paths_to_nii_[1], pattern = '/'))
    #print('zzzzzzzzssssssss:')
    #print(paths_temp)
    paths_temp = paths_temp[-length(paths_temp)]
    paths_temp <<- stri_paste(paths_temp, collapse = '/')
    imagepath <<- paths_temp
    #print('imagepath ------------------------------------------------------------------------------------')
    #print(stri_paste(imagepath, collapse = '/'))
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
  
  output$results_image = renderPlot({
    req(input$run)
    suffix = "_2bk-baseline_con_3mm.nii.gz"
    mask = ifelse(AAL_mask > 0, 1, 0)
    voxels = which(mask == 1, arr.ind = TRUE)
    colnames(voxels) <- c("x", "y", "z")
    z_range = unique(voxels[, 3])
    z_show_slices = z_range[seq(10, 40, by = 2)]
    #print('BIG debug')
    #print("IMAGE PATH:")
    #print(imagepath)
    imagepath <<- stri_paste(imagepath, collapse = '/')
    #print("NEXT")
    #print(paste(imagepath, paste0(input$select_patient_to_explore, suffix), sep = '/'))
    imgs = readnii(paste(imagepath, paste0(input$select_patient_to_explore, suffix), sep = '/'))
    image(imgs, z = z_show_slices, plot.type = "single")
  }, bg = 'black')
}

shinyApp(ui = ui, server = server)
