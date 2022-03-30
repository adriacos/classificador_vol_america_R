
if (!require("shiny")){
    install.packages("shiny")
}
library(shiny)

if (!require("sqldf")){
    install.packages("sqldf")
}
library(sqldf)


source("scripts/create_map.R")
source("scripts/read_data.R")
source("scripts/save_data.R")
count = 1

shinyServer(function(input, output) {
#    observe({
#        valAgricola <- input$percAgricola
#        updateSliderInput("percForestal", value = valAgricola)
#    })
    
    data <- readData()
    insertIFN_VA_Class(data)
    output$map <- renderLeaflet(create_map(data[count, "coords_latitude"],data[count, "coords_longitude"]))
    data[count, "percForestal"] <- input$percForestal
    data[count, "percAgricola"] <- input$percAgricola
    data[count, "percPrados"] <- input$percPrados
    data[count, "percResidencial"] <- input$percResidencial
    data[count, "percIndustrial"] <- input$percIndustrial
    data[count, "percInproductivo"] <- input$percInproductivo
    data[count, "percAgua"] <- input$percAgua
    data[count, "percOtros"] <- input$percOtros
    #input$checkForest
})
