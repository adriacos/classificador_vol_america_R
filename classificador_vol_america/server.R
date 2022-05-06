
#if (!require("shiny")){
#    install.packages("shiny")
#}
library(shiny)

#if (!require("sqldf")){
#    install.packages("sqldf")
#}
library(sqldf)

library(shiny)

source("scripts/create_map.R")
source("scripts/read_data.R")
source("scripts/save_data.R")

shinyServer(function(input, output, session) {

    
    data <- readData()
    #insertIFN_VA_Class(data)
    activeParcela <- reactiveValues(count=1, data=data)
    
    tableData <- reactiveVal()
    
    output$map <- renderLeaflet(
        create_map(activeParcela$data[activeParcela$count, "coords_latitude"],activeParcela$data[activeParcela$count, "coords_longitude"])
        )
    
    observeEvent(input$tabs, {
        if(input$tabs == "data"){
            print("data tab selected")
            tableData(readIFN_VA_Class())
        }
    })
    
    output$dataTable = renderTable({
        tableData()[!is.na(tableData()$sys_dt_done),]
    })
    
    output$title <- renderText({
        return(paste(activeParcela$data[activeParcela$count,]$plot_id, 
            " - ",
            paste(activeParcela$data[activeParcela$count,]$admin_municipality,
            activeParcela$data[activeParcela$count,]$admin_region,
            activeParcela$data[activeParcela$count,]$admin_province,
            activeParcela$data[activeParcela$count,]$admin_aut_community, sep=", "),  

            paste(" (",round(activeParcela$data[activeParcela$count,]$topo_altitude_asl, digits=0), " m)", sep=""), 
            sep=""))
        })
    
   observeEvent(input$nextButton, {
        
            activeParcela$data[activeParcela$count,]$cubiertaParcela <- input$cubiertaParcela
            
            activeParcela$data[activeParcela$count,]$sys_dt_done <- Sys.time()
            
            updateIFN_VA_Class(activeParcela$data[activeParcela$count,])
            
            if(activeParcela$count>=nrow(activeParcela$data)){
                activeParcela$data <- activeParcela$data[1,]
                data <- readData()
                activeParcela$data <- rbind(activeParcela$data, data)
                activeParcela$count = 2
            } else{
                activeParcela$count = activeParcela$count + 1
            }
           
            updateSliderInput(session, "cubiertaParcela", value="forestal")
          
            enable("previousButton")
        
    })
    observeEvent(input$previousButton, {

        activeParcela$count = activeParcela$count - 1
        
        updateSliderInput(session, "cubiertaParcela", value=activeParcela$data[activeParcela$count,]$cubiertaParcela)
       
        disable("previousButton")
    })
    
})
