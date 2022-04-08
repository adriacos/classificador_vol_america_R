
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
    tags$script("
        Shiny.addCustomMessageHandler('changePercForestal', function(value) {
        Shiny.setInputValue('percForestal', value);
        });
    "),
    tags$head(
        tags$style(HTML("
            .selectize-control {
                margin-bottom:4px;
                }")),
        tags$style(HTML("
            .selectize-input {
                padding: 3px 12px;
                min-height: 0px
                }")),
        tags$style(HTML("
            .irs--shiny.irs-with-grid {
                height: 48px;
                }")),
        tags$style(HTML("
            .irs--shiny .irs-bar {
                top:18px;
                height: 6px;
                }")),
        tags$style(HTML("
            .irs--shiny .irs-grid {
                height: 24px;
                }")),
        tags$style(HTML("
            .irs--shiny .irs-line {
                top: 18px;
                height: 6px;
                }")),
        tags$style(HTML("
            .irs--shiny .irs-handle {
                top:12px;
                height: 16px;
                width:16px;
                }")),
        tags$style(HTML("
            .form-group {
                margin-bottom:6px;
                }")),
        tags$style(HTML("
            label {
                margin-bottom:2px;
                }")),
        tags$style(HTML("
            #map {
                height:624px;
                }")),
        tags$style(HTML("
            .well {
                padding: 9px 19px 1px 19px;
                margin-bottom: 12px;
                }")),
        tags$style(HTML("
            #nextButton {
                float:right;
                }")),
    ),

    
    headerPanel(
        textOutput("title")
    ),
    sidebarLayout(
        
        sidebarPanel(
            wellPanel("Radio de 25 m (círculo rojo)",
                      selectInput(inputId="cubiertaParcela", 
                                   label="Cubierta de la parcela",
                                   choices=c("-"="null",
                                             "Forestal"="forestal", 
                                             "Agrícola"="agricola",
                                             "Prados"="prados",
                                             "Urbano"="urbano",
                                             "Inproductivo"="inproductivo",
                                             "Otros"="otros"),
                                   selected="null")),
            wellPanel(
                fluidRow("Radio de 1 km (círculo rojo)"),
                fluidRow(
                    sliderInput(inputId = "percForestal", label = "Forestal", value=100, min=0, max=100,step=10, width='100%'),
                    sliderInput(inputId = "percAgricola", label = "Agricola", value=0, min=0, max=100,step=10),
                    sliderInput(inputId = "percPrados", label = "Prados", value=0, min=0, max=100,step=10),
                    #sliderInput(inputId = "percResidencial", label = "Residencial", value=0, min=0, max=100,step=25),
                    #sliderInput(inputId = "percIndustrial", label = "Industrial", value=0, min=0, max=100,step=25),
                    sliderInput(inputId = "percInproductivo", label = "Inproductivo", value=0, min=0, max=100,step=10),
                    sliderInput(inputId = "percUrbano", label = "Urbano", value=0, min=0, max=100,step=10),
                    #sliderInput(inputId = "percAgua", label = "Agua", value=0, min=0, max=100,step=25),
                    sliderInput(inputId = "percOtros", label = "Otros", value=0, min=0, max=100,step=10)
                ),    
                fluidRow(
                          column(3,
                                 imageOutput("landscape_1")),
                          column(3,
                                 imageOutput("landscape_2")),
                          column(3,
                                 imageOutput("landscape_3")),
                          column(3,
                                 imageOutput("landscape_4")),
                      )),
            wellPanel(
                useShinyjs(),
                fluidRow(
                    column(6, 
                           disabled(actionButton(inputId="previousButton", label="Previous"))
                    ),
                    column(6,
                           actionButton(inputId="nextButton", label="Next")
                    )
                ))
            ),
        mainPanel(
            leafletOutput("map", height=624),
        )
        )
    )
)





