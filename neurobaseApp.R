
pkgs = as.character(installed.packages()[,'Package'])
req.pkgs = c('neurobase', 'shiny', 'shinyFiles', 'tidyverse')
sapply(req.pkgs, function(x) { if(!(x %in% pkgs)) install.packages(x) })

library(neurobase)
library(shiny)
library(shinyFiles)
library(tidyverse)

regr_variables_ = c()
select_var = c()

img_betas = list()
slices_to_show = c()
img_cols = c()
plane = c()


ui = fluidPage(
  fluidRow(
    headerPanel(
      h3("Neurobase Viewer", style = "color: #02169B; font-weight: bold;")
    ),
    div(style = "height:72px; background-color: #F1F1F1;")
  ),
  br(),
  br(),
  br(),
  sidebarPanel(
    wellPanel(
      h4(tags$b("Instructions")),
      hr(),
      h4(
        'Set mask image, outcome and clinical variables, and the brain
              image directory. Then, search and select a patient to explore.'
      )
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
    tags$b('Select the brain images directory'),
    br(),
    shinyDirButton('dir', label = 'Select directory', title = 'Select directory brain images'),
    wellPanel(textOutput("mdat_path_display")),
    br(),
    br(),
    selectInput(
      'select_patient_to_explore',
      label = 'Search and select a patient to explore',
      choices = c()
    ),
    actionButton('run', 'Run'),
    hr(),
    wellPanel(
      h4(tags$b("Regression Analysis")),
      hr(),
      h4(
        'Select predictors to use in the regression analysis with ______, fit the
        regression model and preview results by variable name using _______.'
      )
    ),
    selectInput('select_regr_vars', label = 'Select predictor variables for regression model:',
                       choices = c()),
    actionButton('add_regr_variable', 'Add variable to model'),
    textOutput('print_regr_variables'),
    actionButton('run_regression', 'Fit model'),
    selectInput('select_param_view', label = 'Select variable on which to view brain images:',
                choices = c())
  ),
  mainPanel(
    h3('Summary of results for the selected patient:'),
    hr(),
    verbatimTextOutput('results_summary'),
    plotOutput('results_image'),
    h3('Regression results by variable of interest:'),
    hr(),
    plotOutput('model_results_image')
  )
)


server = function(input, output, session) {
  volumes <- getVolumes()
  shinyDirChoose(
    input,
    'dir',
    roots = volumes,
    session = session#,
    #filetypes = c('.nii.gz', '.nii')
  )
  
  mdat_path <- reactive({
    return(parseDirPath(volumes, input$dir))
  })
  
  output$mdat_path_display = renderText({
    req(input$dir)
    dir_ = unlist(input$dir)
    dir2_ = dir_[-length(dir_)]
    dir_start_ = dir_[length(dir_)]
    dir_new_ = c(dir_start_, dir2_)
    return(paste0(dir_new_, sep = '/'))
  })
  
  observeEvent(input$mask_image, {
    AAL_mask <<- readnii(input$mask_image$datapath)
  })
  
  observeEvent(input$clinical_vars, {
    phenos_dat <<- read.csv(input$clinical_vars$datapath)
    updateSelectInput(
      session,
      'select_regr_vars',
      label = 'Select predictor variables for regression model:',
      choices = colnames(phenos_dat)
    )
  })
  
  regr_selections = reactive({
    req(input$add_regr_variable)
    input$add_regr_variable
    isolate(regr_variables_ <<- c(regr_variables_, input$select_regr_vars))
    regr_variables_
  })
  
  output$print_regr_variables = renderText({
    toString(regr_selections())
  })
  
  model_fit_results = reactive({
    req(input$run_regression)
    input$run_regression
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      mask = ifelse(AAL_mask>0,1,0)
      voxels = which(mask==1,arr.ind = TRUE)
      colnames(voxels) <- c("x","y","z")
      z_range = unique(voxels[,3])
      z_show_slices = z_range[seq(10,40,by=2)]
      imgs = readnii(paste(imagepath, paste0(isolate(input$select_patient_to_explore), suffix), sep = '/'))
      imgsfiles = dir(imagepath)
      subjID = unlist(strsplit(imgsfiles,suffix))
      X_names = regr_variables_
      X = phenos_dat[match(subjID, as.character(phenos_dat[['Subject']])), X_names]
      
      matX = NULL
      nm_X = names(X)
      nms = NULL
      for(i in 1:ncol(X)){
        if(is.factor(X[[i]])){
          levels_X = levels(X[[i]])
          if(length(levels_X)>1){
            for(j in 2:length(levels_X)){
              matX = cbind(matX,ifelse(as.character(X[[i]])==levels_X[j],1,0))
              nms = c(nms,paste(nm_X[i],levels_X[j],sep="_"))
            }
          }
        } else{
          matX = cbind(matX,X[[i]])
          nms = c(nms,nm_X[i])
        }
      }
      
      imgdat = matrix(NA, nrow=length(imgsfiles),ncol=sum(mask==1))
      for(i in 1:length(imgsfiles)){
        img = readnii(file.path(imagepath,imgsfiles[i]))
        img[] = ifelse(is.nan(img[]),0,img[])
        img[] = ifelse(mask[]==1,img[],NaN)
        imgdat[i,] = img[mask[]==1]
      }
      
      sigma2_beta = 0.01
      XtX = crossprod(matX)
      XtY = crossprod(matX,imgdat)
      beta0 = solve(XtX+sigma2_beta*diag(nrow(XtX)),XtY)
      beta = beta0
      for(j in 1:ncol(beta)){
        beta[,j] = ifelse(beta0[,j]>quantile(abs(beta0[,j]),prob=0.95),beta0[,j],0)
      }
      
      img_temp = readnii(file.path(imagepath,imgsfiles[1]))
      #img_betas = list()
      for(j in 1:nrow(beta)){
        img_betas[[j]] <<- img_temp
        img_betas[[j]][which(mask[]==1)] <<- beta[j,]
        img_betas[[j]][which(mask[]!=1)] <<- NaN
      }
      names(img_betas) <<- nms
      
      img_cols <<- colorRampPalette(c("blue","yellow","red"))(256)
      #show results
      select_var <<- nms[1]
      slices_to_show <<- c(15,25,35,45)
      plane <<- "coronal" #"coronal", "sagittal"
      
      updateSelectInput(session = session, 'select_param_view', choices = nms)
      
      # return(
      # image(img_betas[[select_var]],z = slices_to_show,plot.type="single",
      #       col=img_cols, plane = plane,
      #       main=list(select_var,col="white"),mar = rep(1, 4)) )
    
  })
  
  output$model_results_image = renderPlot({
    model_fit_results()
    new_view_var_ = new_view_variable()
    if(input$select_param_view %>% str_length == 0) {
      return(
        image(img_betas[[select_var]], z = slices_to_show, plot.type="single",
              col=img_cols, plane = plane,
              main=list(select_var,col="white"), mar = rep(1, 4))
      )
    } else {
      return(
      image(img_betas[[new_view_variable()]], z = slices_to_show, plot.type="single",
          col=img_cols, plane = plane,
          main=list(select_var,col="white"), mar = rep(1, 4))
    )
    }
    
    
  }, bg = 'black', main=list(select_var, col="white"), mar = rep(1, 4))
  
  new_view_variable = reactive({
    req(input$select_param_view %>% str_length > 0)
    if(is.null(input$select_param_view) & !(is_empty(select_var))) {
      return(select_var)
    } else if (!is.null(input$select_param_view)) {
      return(input$select_param_view)
    } 
    #req(select_param_view$input)
    #return(select_param_view$input)
  })
  
  observe({
    req(input$mask_image$datapath > 0)
    imagepath <<- mdat_path()
    if(length(imagepath) > 0) {
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      imgsfiles = dir(imagepath)
      subjID = unlist(strsplit(imgsfiles,suffix))
      updateSelectInput(inputId = 'select_patient_to_explore',
                        choices = subjID, session = session)
    }
    return()
  })
  
  output$results_summary = renderPrint({
    req(input$run)
    suffix = "_2bk-baseline_con_3mm.nii.gz"
    subjID = unlist(strsplit(input$select_patient_to_explore,suffix))
    X_names = c("Female","p","g")
    X = phenos_dat[match(subjID, as.character(phenos_dat[['Subject']])), X_names]
    print(summary(X))
  })
  
  output$results_image = renderPlot({
    req(input$run)
    suffix = "_2bk-baseline_con_3mm.nii.gz"
    mask = ifelse(AAL_mask>0,1,0)
    voxels = which(mask==1,arr.ind = TRUE)
    colnames(voxels) <- c("x","y","z")
    z_range = unique(voxels[,3])
    z_show_slices = z_range[seq(10,40,by=2)]
    imgs = readnii(paste(imagepath, paste0(input$select_patient_to_explore, suffix), sep = '/'))
    image(imgs, z = z_show_slices, plot.type = "single")
  }, bg = 'black')
}

shinyApp(ui = ui, server = server)
