library(devtools)
library(shiny)
library(gplots)

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
    
    column(width=8,offset=0,
           h1(strong("Informations requises :"), style = "font-size:20px;")),
    
  ), ###fluidRow( # row 5: Lesditesinfo   
    wellPanel(
      fluidRow("test1")
      ,fluidRow("test2")
      ,fluidRow("test3")
      
    )### --> Faire rentrer ceci dans un conditionalPanel, pour en prévoir un différent 
     ### en fonction du calcul demandé
    
  ), fluidRow( # row 6: plots et résultats
    
    
  )
  

)
    
# Define server logic to plot various variables against mpg ----
server = function(input, output) {
}

shinyApp(ui,server)






