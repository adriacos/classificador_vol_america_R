
#if (!require("shiny")){
#    install.packages("shiny")
#}
library(shiny)

#if (!require("shinyjs")){
#    install.packages("shinyjs")
#}
library(shinyjs)

#if (!require("leaflet")){
#    install.packages("leaflet")
#}
library(leaflet)

reclass_df6 <- c(0,0.0001,NA,0.0001,0.16666,0.083333,0.16666,0.33333,0.25,0.33333,0.5,0.416666,0.5,0.66666,0.583333,0.66666,0.83333,0.75, 0.83333, 0.99999, 0.916666, 0.99999,1,NA)
reclass_df5 <- c(0,0.0001,NA,0.0001,0.2,0.1,0.2,0.4,0.3,0.4,0.6,0.5,0.6,0.8,0.7,0.8,0.9999,0.9,0.9999,1,NA)
reclass_df4 <- c(0,0.0001,NA,0.0001,0.25,0.12,0.25,0.5,0.37,0.5,0.75,0.62,0.75,0.9999,0.75,0.9999,1,NA)
reclass_m6 <- matrix(reclass_df6, ncol=3, byrow=TRUE)
reclass_m5 <- matrix(reclass_df5, ncol=3, byrow=TRUE)
reclass_m4 <- matrix(reclass_df4, ncol=3, byrow=TRUE)

r <- rast
r<- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 
r <- reclassify(r, reclass_m5)
r<- focal(r, w=matrix(1/25,nrow=5,ncol=5)) 
r <- reclassify(r, reclass_m5)
r<- focal(r, w=matrix(1/25,nrow=5,ncol=5)) 
r <- reclassify(r, reclass_m5)
r<- focal(r, w=matrix(1/9,nrow=3,ncol=3)) 
r <- reclassify(r, reclass_m5)

plot(r, col=grey_scale(6))


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
    
    tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("classificador", 
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
                                         sliderInput(inputId = "percForestal", label = "Forestal", value=0, min=0, max=100,step=10, width='100%'),
                                         sliderInput(inputId = "percAgricola", label = "Agricola", value=0, min=0, max=100,step=10),
                                         sliderInput(inputId = "percPrados", label = "Prados", value=0, min=0, max=100,step=10),
                                         #sliderInput(inputId = "percResidencial", label = "Residencial", value=0, min=0, max=100,step=25),
                                         #sliderInput(inputId = "percIndustrial", label = "Industrial", value=0, min=0, max=100,step=25),
                                         sliderInput(inputId = "percInproductivo", label = "Inproductivo", value=0, min=0, max=100,step=10),
                                         sliderInput(inputId = "percUrbano", label = "Urbano", value=0, min=0, max=100,step=10),
                                         #sliderInput(inputId = "percAgua", label = "Agua", value=0, min=0, max=100,step=25),
                                         sliderInput(inputId = "percOtros", label = "Otros", value=0, min=0, max=100,step=10)
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
                    ),
                tabPanel("data", 
                         tableOutput('dataTable')
                    ))
   
))





