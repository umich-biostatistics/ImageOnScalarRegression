
library(neurobase)
library(shiny)
library(shinyFiles)


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
    
    
    actionButton('run', 'Run')
    
  ),
  
  mainPanel(
    verbatimTextOutput('results_summary'),
    plotOutput('results_image')
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
  })
  
  observe({
    req(input$mask_image$datapath > 0)
    print(input$mask_image$datapath)
    imagepath <<- mdat_path()
    if(length(imagepath) > 0) {
      print("Image path:")
      print(imagepath )
      suffix = "_2bk-baseline_con_3mm.nii.gz"
      imgsfiles = dir(imagepath)
      subjID = unlist(strsplit(imgsfiles,suffix))
      print(imgsfiles)
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
    z_show_slices = z_range[seq(10,40,by=2)]
    print('input dir:')
    print(paste(imagepath, input$select_patient_to_explore, sep = '/'))
    imgs = readnii(paste(imagepath, paste0(input$select_patient_to_explore, suffix), sep = '/'))
    image(imgs, z = z_show_slices, plot.type = "single")
  }, bg = 'black')
  
}

shinyApp(ui = ui, server = server)







