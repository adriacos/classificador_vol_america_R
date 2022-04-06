
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
    activeParcela <- reactiveValues(count=1, data=data, 
                                    initiated=list("forestal"=TRUE, "agricola"=TRUE, "prados"=TRUE, "inproductivo"=TRUE, "urbano"=TRUE, "otros"=TRUE))
    
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
    
    observeEvent(input$percForestal, {
        print(activeParcela$initiated$forestal)
        if(activeParcela$initiated$forestal==FALSE){
            activeParcela$initiated$forestal <- TRUE
            return
        }
        valAgricola = 100 - input$percForestal - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
        if(valAgricola<0){
            updateSliderInput(session, "percAgricola", value=0)
            valPrados = 100 - input$percForestal - input$percInproductivo - input$percUrbano - input$percOtros
            if(valPrados<0){
                updateSliderInput(session, "percPrados", value=0)
                valInproductivo = 100 - input$percForestal - input$percUrbano - input$percOtros
                if(valInproductivo<0){
                    updateSliderInput(session, "percInproductivo", value=0)
                    valUrbano = 100 - input$percForestal - input$percOtros
                    if (valUrbano<0){
                        updateSliderInput(session, "percUrbano", value=0)
                        valOtros = 100 - input$percForestal
                        updateSliderInput(session, "percOtros", value=valOtros)    
                    }
                    else{
                        updateSliderInput(session, "percUrbano", value=valUrbano)    
                    }
                }
                else{
                    updateSliderInput(session, "percInproductivo", value=valInproductivo)
                }
            }
            else{
                updateSliderInput(session, "percPrados", value=valPrados)
            }
            
        } else{
            updateSliderInput(session, "percAgricola", value=valAgricola)
        }
    })

    
    observeEvent(input$percAgricola, {
        print(activeParcela$initiated$agricola)
        if(activeParcela$initiated$agricola==FALSE){
            activeParcela$initiated$agricola <- TRUE
            return
        }
        valForestal = 100 - input$percAgricola - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
        if(valForestal<0){
            updateSliderInput(session, "percForestal", value=0)
                valPrados = 100 - input$percAgricola - input$percInproductivo - input$percUrbano - input$percOtros
                if(valPrados<0){
                    updateSliderInput(session, "percPrados", value=0)
                    valInproductivo = 100 - input$percAgricola - input$percUrbano - input$percOtros
                    if(valInproductivo<0){
                        updateSliderInput(session, "percInproductivo", value=0)
                        valUrbano = 100 - input$percAgricola - input$percOtros
                        if (valUrbano<0){
                            updateSliderInput(session, "percUrbano", value=0)
                            valOtros = 100 - input$percAgricola
                            updateSliderInput(session, "percOtros", value=valOtros)    
                        }
                        else{
                            updateSliderInput(session, "percUrbano", value=valUrbano)    
                        }
                    }
                    else{
                        updateSliderInput(session, "percInproductivo", value=valInproductivo)
                    }
                }
                else{
                    updateSliderInput(session, "percPrados", value=valPrados)
                }
           
        } else{
            updateSliderInput(session, "percForestal", value=valForestal)
        }
    })
    
    observeEvent(input$percPrados, {
        print(activeParcela$initiated$prados)
        if(activeParcela$initiated$prados==FALSE){
            activeParcela$initiated$prados <- TRUE
            return
        }
        valForestal = 100 - input$percAgricola - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
        if(valForestal<0){
            updateSliderInput(session, "percForestal", value=0)
            valAgricola = 100 - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
            if(valAgricola<0){
                updateSliderInput(session, "percAgricola", value=0)
                valInproductivo = 100 - input$percPrados - input$percUrbano - input$percOtros
                if(valInproductivo<0){
                    updateSliderInput(session, "percInproductivo", value=0)
                    valUrbano = 100 - input$percPrados - input$percOtros
                    if (valUrbano<0){
                        updateSliderInput(session, "percUrbano", value=0)
                        valOtros = 100 - input$percPrados
                        updateSliderInput(session, "percOtros", value=valOtros)    
                    }
                    else{
                        updateSliderInput(session, "percUrbano", value=valUrbano)    
                    }
                }
                else{
                    updateSliderInput(session, "percInproductivo", value=valInproductivo)
                }
            }
            else{
                updateSliderInput(session, "percAgricola", value=valAgricola)
            }
            
        } else{
            updateSliderInput(session, "percForestal", value=valForestal)
        }
    })
    observeEvent(input$percInproductivo, {
        print(activeParcela$initiated$inproductivo)
        if(activeParcela$initiated$inproductivo==FALSE){
            activeParcela$initiated$inproductivo <- TRUE
            return
        }
        valForestal = 100 - input$percAgricola - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
        if(valForestal<0){
            updateSliderInput(session, "percForestal", value=0)
            valAgricola = 100 - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
            if(valAgricola<0){
                updateSliderInput(session, "percAgricola", value=0)
                valPrados = 100 - input$percInproductivo - input$percUrbano - input$percOtros
                if(valPrados<0){
                    updateSliderInput(session, "percPrados", value=0)
                    valUrbano = 100 - input$percInproductivo - input$percOtros
                    if (valUrbano<0){
                        updateSliderInput(session, "percUrbano", value=0)
                        valOtros = 100 - input$percInproductivo
                        updateSliderInput(session, "percOtros", value=valOtros)    
                    }
                    else{
                        updateSliderInput(session, "percUrbano", value=valUrbano)    
                    }
                }
                else{
                    updateSliderInput(session, "percPrados", value=valPrados)
                }
            }
            else{
                updateSliderInput(session, "percAgricola", value=valAgricola)
            }
            
        } else{
            updateSliderInput(session, "percForestal", value=valForestal)
        }
    })

    observeEvent(input$percUrbano, {
        print(activeParcela$initiated$urbano)
        if(activeParcela$initiated$urbano ==FALSE){
            activeParcela$initiated$urbano <- TRUE
            return
        }
        valForestal = 100 - input$percAgricola - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
        if(valForestal<0){
            updateSliderInput(session, "percForestal", value=0)
            valAgricola = 100 - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
            if(valAgricola<0){
                updateSliderInput(session, "percAgricola", value=0)
                valPrados = 100 - input$percInproductivo - input$percUrbano - input$percOtros
                if(valPrados<0){
                    updateSliderInput(session, "percPrados", value=0)
                    valInproductivo = 100 - input$percUrbano - input$percOtros
                    if (valInproductivo<0){
                        updateSliderInput(session, "percInproductivo", value=0)
                        valOtros = 100 - input$percUrbano
                        updateSliderInput(session, "percOtros", value=valOtros)    
                    }
                    else{
                        updateSliderInput(session, "percInproductivo", value=valInproductivo)    
                    }
                }
                else{
                    updateSliderInput(session, "percPrados", value=valPrados)
                }
            }
            else{
                updateSliderInput(session, "percAgricola", value=valAgricola)
            }
            
        } else{
            updateSliderInput(session, "percForestal", value=valForestal)
        }
    })
    
    observeEvent(input$percOtros, {
        print(activeParcela$initiated$otros)
        if(activeParcela$initiated$otros==FALSE){
            activeParcela$initiated$otros <- TRUE
            return
        }
        valForestal = 100 - input$percAgricola - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
        if(valForestal<0){
            updateSliderInput(session, "percForestal", value=0)
            valAgricola = 100 - input$percPrados - input$percInproductivo - input$percUrbano - input$percOtros
            if(valAgricola<0){
                updateSliderInput(session, "percAgricola", value=0)
                valPrados = 100 - input$percInproductivo - input$percUrbano - input$percOtros
                if(valPrados<0){
                    updateSliderInput(session, "percPrados", value=0)
                    valInproductivo = 100 - input$percUrbano - input$percOtros
                    if (valInproductivo<0){
                        updateSliderInput(session, "percInproductivo", value=0)
                        valUrbano = 100 - input$percOtros
                        updateSliderInput(session, "percUrbano", value=valUrbano)    
                    }
                    else{
                        updateSliderInput(session, "percInproductivo", value=valInproductivo)    
                    }
                }
                else{
                    updateSliderInput(session, "percPrados", value=valPrados)
                }
            }
            else{
                updateSliderInput(session, "percAgricola", value=valAgricola)
            }
            
        } else{
            updateSliderInput(session, "percForestal", value=valForestal)
        }
    })
    
    observeEvent(input$nextButton, {
        if(input$cubiertaParcela=="null"){
            showNotification("Debe seleccionar una opción para la cubierta de la parcela.")
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
            #activeParcela$data[activeParcela$count,] = data[activeParcela$count,]
            activeParcela$initiated$forestal <- FALSE
            activeParcela$initiated$agricola <- FALSE
            activeParcela$initiated$prados <- FALSE
            activeParcela$initiated$inproductivo <- FALSE
            activeParcela$initiated$urbano <- FALSE
            activeParcela$initiated$otros <- FALSE
            
            updateSliderInput(session, "cubiertaParcela", value="null")
            updateSliderInput(session, "percForestal", value=100)
            updateSliderInput(session, "percAgricola", value=activeParcela$data[activeParcela$count,]$percAgricola)
            updateSliderInput(session, "percPrados", value=activeParcela$data[activeParcela$count,]$percPrados)
            updateSliderInput(session, "percInproductivo", value=activeParcela$data[activeParcela$count,]$percInproductivo)
            updateSliderInput(session, "percUrbano", value=activeParcela$data[activeParcela$count,]$percUrbano)
            updateSliderInput(session, "percOtros", value=activeParcela$data[activeParcela$count,]$percOtros)
            
            enable("previousButton")
        }
    })
    observeEvent(input$previousButton, {

        activeParcela$count = activeParcela$count - 1
        #activeParcela$data[activeParcela$count,] = data[activeParcela$count,]
        activeParcela$initiated$forestal <- FALSE
        activeParcela$initiated$agricola <- FALSE
        activeParcela$initiated$prados <- FALSE
        activeParcela$initiated$inproductivo <- FALSE
        activeParcela$initiated$urbano <- FALSE
        activeParcela$initiated$otros <- FALSE
        
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
