
if (!require("shiny")){
    install.packages("shiny")
}
library(shiny)

if (!require("shinyjs")){
    install.packages("shinyjs")
}
library(shinyjs)

if (!require("leaflet")){
    install.packages("leaflet")
}
library(leaflet)

shinyUI(fluidPage(
    useShinyjs(),
#    tags$script("
#        //Shiny.addCustomMessageHandler('percAgricola', function(value) {
#        //    console.log(value);
#            Shiny.setInputValue('percPrados', 50);
#        //})
#    "),
    checkboxInput(inputId="checkForest", 
                  label = "Parcela forestal", 
                  value=TRUE),
    
    disabled(sliderInput(inputId = "percForestal", label = "Forestal", value=100, min=0, max=100,step=25)),
    
    sliderInput(inputId = "percAgricola", label = "Agricola", value=0, min=0, max=100,step=25),
    sliderInput(inputId = "percPrados", label = "Prados", value=0, min=0, max=100,step=25),
    sliderInput(inputId = "percResidencial", label = "Residencial", value=0, min=0, max=100,step=25),
    sliderInput(inputId = "percIndustrial", label = "Industrial", value=0, min=0, max=100,step=25),
    sliderInput(inputId = "percInproductivo", label = "Inproductivo", value=0, min=0, max=100,step=25),
    sliderInput(inputId = "percAgua", label = "Agua", value=0, min=0, max=100,step=25),
    sliderInput(inputId = "percOtros", label = "Otros", value=0, min=0, max=100,step=25),
 
    leafletOutput("map")
    
))





