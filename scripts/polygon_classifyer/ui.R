library(shiny)
library(shinyjs)
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
  
  tabsetPanel(type = "tabs", id = "tabs",
              tabPanel("classificador", 
                       headerPanel(
                         textOutput("title")
                       ),
                       sidebarLayout(
                         
                         sidebarPanel(
                           wellPanel("Land cover of selected polygon",
                                     radioButtons("plotCover", 
                                                  label="Cubierta mayoritaria de la parcela",
                                                  choiceNames=list("Bosc", 
                                                                   "Matollar",
                                                                   "Agrícola",
                                                                   "Pastures",
                                                                   "Urbà",
                                                                   "Roquissars i tarteres",
                                                                   "Aigues continentals",
                                                                   "Zones humides",
                                                                   "Litoral i mar",
                                                                   "Núvols",
                                                                   "Ombres",
                                                                   HTML("<p style='color:red;'>Altres/No identificable</p>")),
                                                  choiceValues = c("BC", 
                                                                   "MT",
                                                                   "AG",
                                                                   "PT",
                                                                   "UB",
                                                                   "RC",
                                                                   "Ac",
                                                                   "ZH",
                                                                   "LT",
                                                                   "NV",
                                                                   "OM",
                                                                   "SD"),
                                                  selected = "BC",
                                     )
                           ),
                           wellPanel(
                             useShinyjs(),
                             fluidRow(
                               column(6, 
                                      disabled(actionButton(inputId="previousButton", label="Previous"))
                               ),
                               column(6,
                                      actionButton(inputId="nextButton", label="Next")
                               )
                             ),
                             # fluidRow(actionButton(inputId="saveButton", label="Save"))
                             )
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