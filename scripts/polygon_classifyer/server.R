library(rprojroot)

setwd(find_rstudio_root_file())

library(shiny)
library(sqldf)
library(shiny)

source("./classificador_vol_america/scripts/create_classification_leaflet.R")
source("./classificador_vol_america/scripts/read_data.R")
source("./classificador_vol_america/scripts/save_data.R")

shinyServer(function(input, output, session) {

  done <- reactiveVal(c())
  
  files_in_progress <- reactiveVal(if(length(get_vectors_in_progress_classification_file_names())>0){
    T
  }else{
    F
  })
  
  files <- if(isolate(files_in_progress())==T){
    get_vectors_in_progress_classification_file_names()
  }else{
    get_vectors_clumped_file_names_not_classified()
  }
  #activefile <- reactiveValues(done=c())
  
  rndm <- reactiveVal(sample(length(files),1))
  activePolygon <- reactiveValues(count=1, vectors=NULL)
  
  
  activefile <- reactive({
    print("renderActiveFile")
    n <- rndm()
    name <- files[n]
    print(substring(files[n],1,7))
    dir <- if(files_in_progress()==F){
      vect <-  readOGR(paste("./classificador_vol_america/vect/", files[n],"_clmp", ".shp", sep=""))
      vect$cover <- NA  
    } else{
      vect <-  readOGR(paste("./classificador_vol_america/vect/classified/inprogress/", files[n], ".shp", sep=""))
    }
    plot_info <- read_plot(substring(files[n],1,7))
    activePolygon$vectors <- vect
    return(list(n=n,name=name,vect=vect,plot_info=plot_info))
  })

  #activefile <-  reactiveValues(rndm=sample(1:length(files),1), vect=readOGR(paste("./classificador_vol_america/vect/", files[rndm()],".shp", sep="")), plot_info=read_plot(substring(files[rndm()],1,7)))

  

  # tableData <- reactiveVal()
  

  
  output$map <- renderLeaflet({
    create_classification_leaflet(activefile()$vect[activePolygon$count,])
  })
  
  # observeEvent(input$tabs, {
  #   if(input$tabs == "data"){
  #     print("data tab selected")
  #     tableData(readIFN_VA_Class())
  #   }
  # })
  
  # output$dataTable = renderTable({
  #   tableData()[!is.na(tableData()$sys_dt_done),]
  # })
  
  output$title <- renderText({
    print("renderTitle")
    #print(activePolygon$vectors[activePolygon$count,"cover"])
    if(!is.na(activePolygon$vectors[activePolygon$count,]$cover)){
      activePolygon$count <- activePolygon$count + 1
    }
    return(paste(activefile()$plot_info$plot_id, 
                  " - ",
                  paste(activefile()$plot_info$admin_municipality,
                       activefile()$plot_info$admin_region,
                       activefile()$plot_info$admin_province,
                       activefile()$plot_info$admin_aut_community, sep=", "),  
                 
                  paste(" (",round(activefile()$plot_info$topo_altitude_asl, digits=0), " m)", sep=""), 
                  " ",
                  activePolygon$count,
                  "/",
                  nrow(activePolygon$vectors),
                  sep=""))
  })
  
  
  observeEvent(input$nextButton, {
    #print(input$plotCover)
    activePolygon$vectors[activePolygon$count,"cover"] <- input$plotCover
    save_in_progress_classification_vector(activefile()$name, activePolygon$vectors)
    #print(2)
    if(activePolygon$count>=nrow(activePolygon$vectors)){
    #if(activePolygon$count>=3){
      save_classified_vector(activefile()$name, activePolygon$vectors)
      if(is.null(done)||length(done)==0){
        done <- activefile()$name
      } else{
        done <- append(done, activefile()$n)
      }
      tochoosefrom <- 1:length(files)[!(1:length(files) %in% done())]
      rdnm <- sample(tochoosefrom,1)
      #avisar d'alguna manera
      #mirar si així canvia el activefile, si no, fer el mateix que amb activePolygon
    } else{
      activePolygon$count = activePolygon$count + 1
    }
    
    updateSliderInput(session, "plotCover", value="BC")
    
    enable("previousButton")
    
  })
  
  # observeEvent(input$saveButton,{
  #   #activePolygon$vectors[activePolygon$count,"cover"] <- input$plotCover
  #   save_in_progress_classification_vector(activefile()$name, activePolygon$vectors)
  # })
  
  observeEvent(input$previousButton, {
    
    activePolygon$count = activePolygon$count - 1
    
    updateSliderInput(session, "plotCover", value=activePolygon$vectors[activePolygon$count-1,"vector"])
    
    disable("previousButton")
  })
  
})