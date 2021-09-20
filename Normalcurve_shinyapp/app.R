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
    
    column(width=3,offset=1,
           conditionalPanel(
             condition = "input.computation == 'aire' & input.airetype==1",
             numericInput(inputId="mean", label=withMathJax("\\(\\mu :\\)"),value=0, min = -1000000, max = 1000000,step=.0001),
             numericInput(inputId="sd", label=withMathJax("\\(\\sigma :\\)"),value=1, min = 0, max = 1000000,step=.0001),
             numericInput(inputId="b_inf", label="Borne inférieure : ",value=-1.96, min = -1000000, max = 1000000,step=.0001),
             numericInput(inputId="b_sup", label="Borne supérieure : ",value=1.96, min = -1000000, max = 1000000,step=.0001)
           ),
           conditionalPanel(
             condition = "input.computation == 'aire' & input.airetype > 1",
             numericInput(inputId="mean2", label=withMathJax("\\(\\mu :\\)"),value=0, min = -1000000, max = 1000000,step=.0001),
             numericInput(inputId="sd2", label=withMathJax("\\(\\sigma :\\)"),value=1, min = 0, max = 1000000,step=.0001),
             numericInput(inputId="b", label="Borne : ",value=-1.96, min = -1000000, max = 1000000,step=.0001)
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
    ),column(width = 6,offset=2,
            mainPanel(
              plotOutput("plottoprint"), # Place for the graph
            )
            
    )

  ), fluidRow( # row 5: lesdites infos requises

  )
  
)





# Define server logic to plot various variables against mpg ----
server = function(input, output) {
  
  output$plottoprint <- renderPlot(

    if(input$computation == "aire" & input$airetype == 1){
      
      par(mfrow=c(2,1),xpd=TRUE,bty="n")
      x=seq(input$mean-5*input$sd,input$mean+5*input$sd,.01)
      y = dnorm(x,input$mean,input$sd)
      plot(x,y,cex=.3,main="Distribution des scores bruts",ylab="densité",xlab="scores X",xaxt="n")
      segments(x0=input$b_inf, y0= 0, x1=input$b_inf, y1=dnorm(input$b_inf,input$mean,input$sd), lty = 1, xpd = FALSE,col="darkblue")
      segments(x0=input$b_sup, y0= 0, x1=input$b_sup, y1=dnorm(input$b_sup,input$mean,input$sd), lty = 1, xpd = FALSE,col="darkblue")
      polygon(c(input$b_inf,x[x>=input$b_inf & x <=input$b_sup],input$b_sup),c(0,y[x>=input$b_inf & x <= input$b_sup],0), col="lightblue")
      segments(x0=input$mean, y0= 0, x1=input$mean, y1=dnorm(input$mean,input$mean,input$sd), lty = 2, xpd = FALSE,col="black")
      hauteur=max(dnorm(x,input$mean,input$sd))/10
      text(input$b_inf,-hauteur,labels=as.character(round(input$b_inf,2)),col="darkblue",cex=.8)
      text(input$b_sup,-hauteur,labels=as.character(round(input$b_sup,2)),col="darkblue",cex=.8)
      text(input$mean,-hauteur,labels=as.character(input$mean),col="black",cex=.8)
      par(xpd=FALSE)
      abline(h=0)
      
      par(xpd=TRUE)
      x=seq(-5,5,.01)
      z = dnorm(x,0,1)
      plot(x,dnorm(x,0,1),cex=.3,main="Distribution des scores Z",ylab="densité",xlab="scores z",,xaxt="n")
      z_inf <- (input$b_inf - input$mean)/input$sd
      z_sup <- (input$b_sup - input$mean)/input$sd
      segments(x0=z_inf, y0=0, x1=z_inf, y1=dnorm(z_inf,0,1), lty = par("lty"), xpd = FALSE)
      segments(x0=z_sup, y0=0, x1=z_sup, y1=dnorm(z_sup,0,1), lty = par("lty"), xpd = FALSE)
      polygon(c(z_inf,x[x>=z_inf & x <=z_sup],z_sup),c(0,z[x>=z_inf & x <= z_sup],0), col="lightgreen")
      segments(x0=0, y0= (-.5), x1=0, y1=dnorm(0,0,1), lty = 2, xpd = FALSE,col="black")
      text(z_inf,-.05,labels=as.character(round(z_inf,2)),col="darkgreen",cex=.8)
      text(z_sup,-.05,labels=as.character(round(z_sup,2)),col="darkgreen",cex=.8)
      text(0,-.1,labels="0",col="black",cex=.8)
      par(xpd=FALSE)
      abline(h=0)
      
    } else if (input$computation == "aire" & input$airetype == 2){
      
      par(mfrow=c(2,1),xpd=TRUE,bty="n")
      x=seq(input$mean-5*input$sd,input$mean+5*input$sd,.01)
      y = dnorm(x,input$mean,input$sd)
      plot(x,y,cex=.3,main="Distribution des scores bruts",ylab="densité",xlab="scores X",xaxt="n")
      segments(x0=input$b, y0= 0, x1=input$b, y1=dnorm(input$b,input$mean,input$sd), lty = 1, xpd = FALSE,col="darkblue")
      polygon(c(input$b,x[x<=input$b]),c(y[x<=input$b],0), col="lightblue")
      segments(x0=input$mean, y0= 0, x1=input$mean, y1=dnorm(input$mean,input$mean,input$sd), lty = 2, xpd = FALSE,col="darkblue")
      hauteur=max(dnorm(x,input$mean,input$sd))/10
      text(input$b,-hauteur,labels=as.character(round(input$b,2)),col="darkblue",cex=.8)
      text(input$mean,-hauteur,labels=as.character(input$mean),col="black",cex=.8)
      par(xpd=FALSE)
      abline(h=0)
      
      par(xpd=TRUE)
      x=seq(-5,5,.01)
      z = dnorm(x,0,1)
      plot(x,dnorm(x,0,1),cex=.3,main="Distribution des scores Z",ylab="densité",xlab="scores z",xaxt="n")
      z_inf <- (input$b - input$mean)/input$sd
      segments(x0=z_inf, y0=0, x1=z_inf, y1=dnorm(z_inf,0,1), lty = par("lty"), xpd = FALSE)
      polygon(c(z_inf,x[x<=z_inf]),c(z[x<=z_inf],0), col="lightgreen")
      segments(x0=0, y0= (-.5), x1=0, y1=dnorm(0,0,1), lty = 2, xpd = FALSE,col="black")
      text(z_inf,-.05,labels=as.character(round(z_inf,2)),col="darkgreen",cex=.8)
      text(0,-.1,labels="0",col="black",cex=.8)
      par(xpd=FALSE)
      abline(h=0)
      
      
    }  else if (input$computation == "aire" & input$airetype == 3){
      
      par(mfrow=c(2,1),xpd=TRUE,bty="n")
      x=seq(input$mean-5*input$sd,input$mean+5*input$sd,.01)
      y = dnorm(x,input$mean,input$sd)
      plot(x,y,cex=.3,main="Distribution des scores bruts",ylab="densité",xlab="scores X",xaxt="n")
      segments(x0=input$b, y0= 0, x1=input$b, y1=dnorm(input$b,input$mean,input$sd), lty = 1, xpd = FALSE,col="darkblue")
      polygon(c(x[x>=input$b],input$b),c(y[x>=input$b],0), col="lightblue")
      segments(x0=input$mean, y0= 0, x1=input$mean, y1=dnorm(input$mean,input$mean,input$sd), lty = 2, xpd = FALSE,col="darkblue")
      hauteur=max(dnorm(x,input$mean,input$sd))/10
      text(input$b,-hauteur,labels=as.character(round(input$b,2)),col="darkblue",cex=.8)
      text(input$mean,-hauteur,labels=as.character(input$mean),col="black",cex=.8)
      par(xpd=FALSE)
      abline(h=0)

      par(xpd=TRUE)
      x=seq(-5,5,.01)
      z = dnorm(x,0,1)
      plot(x,dnorm(x,0,1),cex=.3,main="Distribution des scores Z",ylab="densité",xlab="scores z",xaxt="n")
      z_inf <- (input$b - input$mean)/input$sd
      segments(x0=z_inf, y0=0, x1=z_inf, y1=dnorm(z_inf,0,1), lty = par("lty"), xpd = FALSE)
      polygon(c(x[x>=z_inf],z_inf),c(z[x>=z_inf],0), col="lightgreen")
      segments(x0=0, y0= (-.5), x1=0, y1=dnorm(0,0,1), lty = 2, xpd = FALSE,col="black")
      text(z_inf,-.05,labels=as.character(round(z_inf,2)),col="darkgreen",cex=.8)
      text(0,-.1,labels="0",col="black",cex=.8)
      par(xpd=FALSE)
      abline(h=0)
      
      
    }  else if (input$computation == "borne1" & input$airetype == 1){
      
      plot(1,1,main="En construction",col="white",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
      
    }  else if (input$computation == "borne1" & input$airetype == 2){
      
      plot(1,1,main="En construction",col="white",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
      
    }  else if (input$computation == "borne2" & input$airetype == 1){
      
      plot(1,1,main="En construction",col="white",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")

    }  
    
  )
    
}

shinyApp(ui,server)






