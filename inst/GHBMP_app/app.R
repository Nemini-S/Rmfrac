library(shiny)
library(shinycssloaders)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Simulation of GHBMP and estimation of Hurst function"),

  fluidRow(
    column(3,
           wellPanel(
             h4("Simulation"),

             textInput("func", "Hurst function (in terms of t)", "0.2+0*t"),
             textInput("time", "Time sequence", "seq(0,1,by=(1/2)^10)"),
             numericInput("num", "J (positive integer for simulation)", value = 15, min = 1, step = 1),
             actionButton("submit", "Submit")
           ),
           wellPanel(
             h4("Estimation"),
             numericInput("N", "Number of subintervals for estimation", value = 100, min = 1, step = 1),
             numericInput("Q", "Q (integer for estimation (>=2))", value = 2, min = 2, step = 1),
             numericInput("L", "L (integer for estimation (>=2))", value = 2, min = 2, step = 1),
             checkboxGroupInput(
               "checkbox_group",
               "Select Hurst function to plot",
               choices = list("Theoretical Hurst function" = "H",
                              "Raw estimate of Hurst function" = "Raw_Est_H",
                              "Smoothed estimate of Hurst function" = "Smooth_Est_H"))
           )
    ),
    column(9,
           withSpinner((plotOutput("functionPlot")), type = 8, color = "grey")

    )
  )
)



server <- function(input, output,session) {

    PR <- eventReactive(input$submit, {

    H <- parse(text = input$func)
    T <- tryCatch(eval(eval(parse(text = input$time))), error = function(e) NULL)
    J <- input$num

    func.H <- function(t) {
      eval(parse(text = H))
    }

    process <- GHBMP(T,func.H,J)
    process <- process[order(process[,1]),]
    colnames(process) <- c("t1", "PP")
    process

  })


  output$functionPlot <- renderPlot({

    p<-ggplot(PR(), aes(x =t1, y =PP))+geom_line() +
      labs(y="X(t)",x="t")

    N <- input$N
    Q <- input$Q
    L <- input$L

    H <- parse(text = input$func)
    func.H <- function(t) {
      eval(parse(text = H))
    }

    H_est <- Hurst(PR(),N,Q,L)
    colnames(H_est) <- c("x","y")
    t1<-PR()[,1]

    if("H" %in% input$checkbox_group){

      H1<-sapply(t1,func.H)
      data1<-data.frame(t1,H1)

      if("Raw_Est_H" %in% input$checkbox_group && "Smooth_Est_H" %in% input$checkbox_group){

        p <- p +
          geom_line(data =data1, aes(x =t1, y =H1,col="Theoretical H"))+
          geom_line(data = H_est, aes(x =x, y =y,col="Raw Estimate H")) +
          geom_smooth(data = H_est, aes(x =x, y =y,col="Smoothed Estimate H")
                      ,method="loess",se=FALSE,span = 0.3) +
          scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H"),
                             values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green"))


      } else if ("Raw_Est_H" %in% input$checkbox_group && !("Smooth_Est_H" %in% input$checkbox_group)){

        p <- p +
          geom_line(data =data1, aes(x =t1, y =H1,col="Theoretical H"))+
          geom_line(data = H_est, aes(x =x, y =y,col="Raw Estimate H")) +
          scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Raw Estimate H"),
                             values=c("Theoretical H"="blue", "Raw Estimate H"="red"))


      } else if (!("Raw_Est_H" %in% input$checkbox_group) && "Smooth_Est_H" %in% input$checkbox_group) {

        p <- p +
          geom_line(data =data1, aes(x =t1, y =H1,col="Theoretical H"))+
          geom_smooth(data = H_est, aes(x =x, y =y,col="Smoothed Estimate H")
                      ,method="loess",se=FALSE,span = 0.3) +
          scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Smoothed Estimate H"),
                             values=c("Theoretical H"="blue", "Smoothed Estimate H"="green"))


      } else if (!("Raw_Est_H" %in% input$checkbox_group) && !("Smooth_Est_H" %in% input$checkbox_group)) {

        p <- p +
          geom_line(data =data1, aes(x =t1, y =H1,col="Theoretical H"))+
          scale_color_manual(name = "Hurst function", breaks=c("Theoretical H"),
                             values=c("Theoretical H"="blue"))

      }
    } else { if ("Raw_Est_H" %in% input$checkbox_group && "Smooth_Est_H" %in% input$checkbox_group) {
      p <- p +
        geom_line(data = H_est, aes(x =x, y =y,col="Raw Estimate H")) +
        geom_smooth(data = H_est, aes(x =x, y =y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(name = "Hurst function", breaks=c( "Raw Estimate H", "Smoothed Estimate H"),
                           values=c( "Raw Estimate H"="red", "Smoothed Estimate H"="green"))


    } else if(!("Raw_Est_H" %in% input$checkbox_group) && "Smooth_Est_H" %in% input$checkbox_group){
      p <- p +
        geom_smooth(data = H_est, aes(x =x, y =y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(name = "Hurst function", breaks=c( "Smoothed Estimate H"),
                           values=c(  "Smoothed Estimate H"="green"))


    } else if("Raw_Est_H" %in% input$checkbox_group && !("Smooth_Est_H" %in% input$checkbox_group)){
      p <- p +
        geom_line(data = H_est, aes(x =x, y =y,col="Raw Estimate H")) +
        scale_color_manual(name = "Hurst function", breaks=c( "Raw Estimate H"),
                           values=c( "Raw Estimate H"="red"))


    } else if(!("Raw_Est_H" %in% input$checkbox_group) && !("Smooth_Est_H" %in% input$checkbox_group)){

      p <- p
    }

    }

    print(p)

  })

}

shinyApp(ui = ui, server = server)


