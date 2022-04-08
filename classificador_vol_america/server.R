
if (!require("shiny")){
    install.packages("shiny")
}
library(shiny)

if (!require("sqldf")){
    install.packages("sqldf")
}
library(sqldf)

library(shiny)

source("scripts/create_map.R")
source("scripts/read_data.R")
source("scripts/save_data.R")


shinyServer(function(input, output, session) {

    
    data <- readData()
    #insertIFN_VA_Class(data)
    activeParcela <- reactiveValues(count=1, data=data)
    
    output$landscape_1 <- renderImage({
        list(src = "./images/landscape_1.png",
             contentType = 'image/png',
             width = 90,
             height = 60,
             alt = "This is alternate text")
    })
    output$landscape_2 <- renderImage({
        list(src = "./images/landscape_2.png",
             contentType = 'image/png',
             width = 90,
             height = 60,
             alt = "This is alternate text")
    })
    output$landscape_3 <- renderImage({
        list(src = "./images/landscape_3.png",
             contentType = 'image/png',
             width = 90,
             height = 60,
             alt = "This is alternate text")
    })
    output$landscape_4 <- renderImage({
        list(src = "./images/landscape_4.png",
             contentType = 'image/png',
             width = 90,
             height = 60,
             alt = "This is alternate text")
    })
    
    output$map <- renderLeaflet(
        create_map(data[activeParcela$count, "coords_latitude"],data[activeParcela$count, "coords_longitude"])
        )
    
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
        if(input$cubiertaParcela=="null"){
            showNotification("Debe seleccionar una opción para la cubierta de la parcela.")
        }else if((input$percForestal + input$percAgricola + input$percPrados + input$percUrbano + input$percInproductivo + input$percOtros) != 100){
            showNotification("Los porcentajes de cubierta de paisaje deben sumar 100.")
        } else{
            activeParcela$data[activeParcela$count,]$cubiertaParcela <- input$cubiertaParcela
            activeParcela$data[activeParcela$count,]$percForestal <- input$percForestal
            activeParcela$data[activeParcela$count,]$percAgricola <- input$percAgricola
            activeParcela$data[activeParcela$count,]$percPrados <- input$percPrados
            activeParcela$data[activeParcela$count,]$percUrbano <- input$percUrbano
            activeParcela$data[activeParcela$count,]$percInproductivo <- input$percInproductivo
            activeParcela$data[activeParcela$count,]$percOtros <- input$percOtros
            
            activeParcela$data[activeParcela$count,]$sys_dt_done <- Sys.time()
            
            updateIFN_VA_Class(activeParcela$data[activeParcela$count,])
            
            activeParcela$count = activeParcela$count + 1
           
            updateSliderInput(session, "cubiertaParcela", value="null")
            updateSliderInput(session, "percForestal", value=0)
            updateSliderInput(session, "percAgricola", value=0)
            updateSliderInput(session, "percPrados", value=0)
            updateSliderInput(session, "percInproductivo", value=0)
            updateSliderInput(session, "percUrbano", value=0)
            updateSliderInput(session, "percOtros", value=0)
            
            enable("previousButton")
        }
    })
    observeEvent(input$previousButton, {

        activeParcela$count = activeParcela$count - 1
        
        updateSliderInput(session, "cubiertaParcela", value=activeParcela$data[activeParcela$count,]$cubiertaParcela)
        updateSliderInput(session, "percForestal", value=activeParcela$data[activeParcela$count,]$percForestal)
        updateSliderInput(session, "percAgricola", value=activeParcela$data[activeParcela$count,]$percAgricola)
        updateSliderInput(session, "percPrados", value=activeParcela$data[activeParcela$count,]$percPrados)
        updateSliderInput(session, "percInproductivo", value=activeParcela$data[activeParcela$count,]$percInproductivo)
        updateSliderInput(session, "percUrbano", value=activeParcela$data[activeParcela$count,]$percUrbano)
        updateSliderInput(session, "percOtros", value=activeParcela$data[activeParcela$count,]$percOtros)
        
        disable("previousButton")
    })
    
})
