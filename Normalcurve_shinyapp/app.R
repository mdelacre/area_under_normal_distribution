library(devtools)
library(shiny)
library(gplots)
library(mathjaxr)

ui <- fluidPage(
  
  fluidRow( # row 1: titles for descriptive AND estimator   
    
    column(width=8,offset=4,
           h1(strong("La loi normale"), style = "font-size:30px;")),

  ), fluidRow( # row 2: titles for descriptive AND estimator   
    
    column(width=8,offset=0,
           h1(strong("Calculs à effectuer :"), style = "font-size:20px;")),

  ),fluidRow( #row 3 : 
    
    column(width=12,offset=0,

           wellPanel(

             selectInput("computation", "Que souhaitez-vous déterminer?", 
                         c("Une aire ..." = "aire",
                           "Une borne ..." = "borne1",
                           "Deux bornes..." = "borne2")
             ),conditionalPanel(
               condition = "input.computation == 'aire'",
               radioButtons("airetype", 
                            h3("", style = "font-size:1px;"),
                            choices = list("... comprise entre deux bornes" = 1, 
                                           "... située en deça d'une borne donnée" = 2,
                                           "... située au delà d'une borne donnée" = 3),
                            selected = 1)
             ),conditionalPanel(
               condition = "input.computation == 'borne1'",
               radioButtons("borne1type", 
                            h3(""),
                            choices = list("... pour laquelle on connait l'aire qui se situe en deça" = 1, 
                                           "... pour laquelle on connait l'aire qui se situe au dela" = 2),
                            selected = 1)
             ),conditionalPanel(
               condition = "input.computation == 'borne2'",
               radioButtons("borne2type", 
                            h3(""),
                            choices = list("... telles que l'aire comprise à l'intérieur de celles-ci soit connue" = 1),
                            selected = 1)
             )
             
           )
                      
    )
    

  ), fluidRow( # row 4: Informations requises   
    
    column(width=5,offset=0,
           h1(strong("Informations requises :"), style = "font-size:20px;")
    ), column(width=5,offset=2,
                 h1(strong("Résultats :"), style = "font-size:20px;")
    )
    
  ), fluidRow( # row 6: plots et résultats

    column(width=2,offset=0,
        conditionalPanel(
          condition = "input.computation == 'aire' & input.airetype==1",
          numericInput(inputId="mean", label=withMathJax("\\(\\mu :\\)"),value=0, min = -1000000, max = 1000000,step=.0001),
          numericInput(inputId="sd", label=withMathJax("\\(\\sigma :\\)"),value=1, min = 0, max = 1000000,step=.0001),
          numericInput(inputId="binf", label="Borne inférieure : ",value=-1.96, min = -1000000, max = 1000000,step=.0001),
          numericInput(inputId="bsup", label="Borne supérieure : ",value=1.96, min = -1000000, max = 1000000,step=.0001)
           ),
        conditionalPanel(
          condition = "input.computation == 'aire' & input.airetype > 1",
          numericInput(inputId="mean", label=withMathJax("\\(\\mu :\\)"),value=0, min = -1000000, max = 1000000,step=.0001),
          numericInput(inputId="sd", label=withMathJax("\\(\\sigma :\\)"),value=1, min = 0, max = 1000000,step=.0001),
          numericInput(inputId="binf", label="Borne : ",value=-1.96, min = -1000000, max = 1000000,step=.0001)
        ),
        conditionalPanel(
          condition = "input.computation == 'borne1' & input.borne1type==1",
          numericInput(inputId="mean", label=withMathJax("\\(\\mu :\\)"),value=0, min = -11000000, max = 1000000,step=.0001),
          numericInput(inputId="sd", label=withMathJax("\\(\\sigma :\\)"),value=1, min = 0, max = 1000000,step=.0001),
          numericInput(inputId="aire1", label="Aire : ",value=.5, min = 0, max = 1,step=.000001)
        ),
        conditionalPanel(
          condition = "input.computation == 'borne1' & input.borne1type==2",
          numericInput(inputId="mean", label=withMathJax("\\(\\mu :\\)"),value=0, min = -11000000, max = 1000000,step=.0001),
          numericInput(inputId="sd", label=withMathJax("\\(\\sigma :\\)"),value=1, min = 0, max = 1000000,step=.0001),
          numericInput(inputId="aire2", label="Aire : ",value=.5, min = 0, max = 1,step=.000001)
        ),
        conditionalPanel(
          condition = "input.computation == 'borne2' & input.borne2type==1",
          numericInput(inputId="mean", label=withMathJax("\\(\\mu :\\)"),value=0, min = -11000000, max = 1000000,step=.0001),
          numericInput(inputId="sd", label=withMathJax("\\(\\sigma :\\)"),value=1, min = 0, max = 1000000,step=.0001),
          numericInput(inputId="aire3", label="Aire : ",value=.5, min = 0, max = 1,step=.000001)
        )
    ),column(width = 12,offset=0,
                 mainPanel(
                   plotOutput("plot"), # Place for the graph
    )
        
    )
    
  )
  
)





# Define server logic to plot various variables against mpg ----
server = function(input, output) {
}

shinyApp(ui,server)






