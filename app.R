library("shiny")
library("billboarder")
library(dplyr)
library(ggplot2)

# data ----
#mincatat <- min(diamonds$carat)
maxcarat <- max(diamonds$carat)

# ui ----

ui <- fluidPage(
    
    tags$h1("sliiiide"),
    br(),
    
    fluidRow(
        
        column(
            width = 3,
            
            wellPanel(
                sliderInput("yearslider", "Slide", min = 0, max = maxcarat, value = 0)
                
            )
            
        ),
        
        column(
            width = 9,
            billboarderOutput(outputId = "barres")
        )
        
    )
)


# server ----

server <- function(input, output, session) {
    
    dat <- reactive({
        req(input$yearslider)
        dat <- diamonds %>% 
            mutate(price2 = ifelse(carat>input$yearslider, price, 0) ) %>% 
            group_by(color) %>% 
            summarise(avgprice = mean(price2, na.rm=T))
    })
    
    output$barres <- renderBillboarder({
        
        billboarder() %>% 
            bb_barchart(data = isolate(dat()))
    })
    
    observeEvent(input$yearslider, {
        
        billboarderProxy(shinyId = "barres") %>% 
            bb_barchart(data = dat())
        
    }, ignoreInit = TRUE)
    
    
    shiny::onStop(shiny::stopApp)
}


# run app ----

shinyApp(ui = ui, server = server)