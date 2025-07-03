ui <- shiny::navbarPage("Simulations",

      shiny::tabPanel("Brownian Motion",
          shiny::sidebarLayout(
          shiny::sidebarPanel(
          shiny::numericInput("x1", "Initial value", value = 0),
          shiny::numericInput("t_start", "Initial time point", value = 0,min=0),
          shiny::numericInput("t_end", "Terminal time point", value = 1,min=0),
          shiny::numericInput("N2", "Number of time steps", value = 1000,min=0,step=1),
          shiny::actionButton("submit1", "Simulate")),
          shiny::mainPanel(plotOutput("bmPlot")))),

      shiny::tabPanel("Fractional Brownian Motion",
          shiny::sidebarLayout(
          shiny::sidebarPanel(
          shiny::numericInput("H", "Hurst Parameter", value = 0.3,min=0,max=1),
          shiny::numericInput("t_end", "Terminal time point", value = 1,min=0),
          shiny::numericInput("N3", "Number of time steps", value = 1000,step=1,min=1),
          shiny::actionButton("submit2", "Simulate")),
          shiny::mainPanel(plotOutput("fbmPlot")))),

      shiny::tabPanel("GHBMP",
          shiny::fluidRow(
          shiny::column(3,
          shiny::wellPanel(
          shiny::h4("Simulation"),
          shiny::textInput("func", "Hurst function (in terms of t)", "0.2+0*t"),
          shiny::textInput("time", "Time sequence", "seq(0,1,by=(1/2)^10)"),
          shiny::numericInput("num", "J (positive integer for simulation)", value = 15, min = 1, step = 1),
          shiny::actionButton("submit3", "Simulate")
            ),
          shiny::wellPanel(
          shiny::h4("Estimation"),
          shiny::numericInput("N", "Number of sub-intervals for estimation", value = 100, min = 1, step = 1),
          shiny::numericInput("Q", "Q (integer for estimation (>=2))", value = 2, min = 2, step = 1),
          shiny::numericInput("L", "L (integer for estimation (>=2))", value = 2, min = 2, step = 1),
          shiny::checkboxGroupInput("checkbox_group","Select Hurst function to plot",
              choices = list("Theoretical Hurst function" = "H","Raw estimate of Hurst function" = "Raw_Est_H",
                             "Smoothed estimate of Hurst function" = "Smooth_Est_H")))),
          shiny::column(9,shinycssloaders::withSpinner((shiny::plotOutput("functionPlot")), type = 8, color = "grey"))
            )),

)

server <- function(input,output,session) {

  PR <- shiny::eventReactive(input$submit3, {

    H <- parse(text = input$func)
    Time <- tryCatch(eval(eval(parse(text = input$time))), error = function(e) NULL)
    J <- input$num

    func.H <- function(t) {
      eval(parse(text = H))
    }

    process <- GHBMP(Time,func.H,J)
    process <- process[order(process[,1]),]
    colnames(process) <- c("t1", "PP")
    process

  })


  output$functionPlot <- shiny::renderPlot({

    p<-ggplot2::ggplot(PR(), ggplot2::aes(x =t1, y =PP))+ggplot2::geom_line() +
      ggplot2::labs(y="X(t)",x="t")

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
          ggplot2::geom_line(data =data1, ggplot2::aes(x =t1, y =H1,col="Theoretical H"))+
          ggplot2::geom_line(data = H_est, ggplot2::aes(x =x, y =y,col="Raw Estimate H")) +
          ggplot2::geom_smooth(data = H_est, ggplot2::aes(x =x, y =y,col="Smoothed Estimate H")
                               ,method="loess",se=FALSE,span = 0.3) +
          ggplot2::scale_color_manual(name = "Hurst functions", breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H"),
                                      values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green"))


      } else if ("Raw_Est_H" %in% input$checkbox_group && !("Smooth_Est_H" %in% input$checkbox_group)){

        p <- p +
          ggplot2::geom_line(data =data1, ggplot2::aes(x =t1, y =H1,col="Theoretical H"))+
          ggplot2::geom_line(data = H_est, ggplot2::aes(x =x, y =y,col="Raw Estimate H")) +
          ggplot2::scale_color_manual(name = "Hurst functions", breaks=c("Theoretical H", "Raw Estimate H"),
                                      values=c("Theoretical H"="blue", "Raw Estimate H"="red"))


      } else if (!("Raw_Est_H" %in% input$checkbox_group) && "Smooth_Est_H" %in% input$checkbox_group) {

        p <- p +
          ggplot2::geom_line(data =data1, ggplot2::aes(x =t1, y =H1,col="Theoretical H"))+
          ggplot2::geom_smooth(data = H_est, ggplot2::aes(x =x, y =y,col="Smoothed Estimate H")
                               ,method="loess",se=FALSE,span = 0.3) +
          ggplot2::scale_color_manual(name = "Hurst functions", breaks=c("Theoretical H", "Smoothed Estimate H"),
                                      values=c("Theoretical H"="blue", "Smoothed Estimate H"="green"))


      } else if (!("Raw_Est_H" %in% input$checkbox_group) && !("Smooth_Est_H" %in% input$checkbox_group)) {

        p <- p +
          ggplot2::geom_line(data =data1, ggplot2::aes(x =t1, y =H1,col="Theoretical H"))+
          ggplot2::scale_color_manual(name = "Hurst functions", breaks=c("Theoretical H"),
                                      values=c("Theoretical H"="blue"))

      }
    } else { if ("Raw_Est_H" %in% input$checkbox_group && "Smooth_Est_H" %in% input$checkbox_group) {
      p <- p +
        ggplot2::geom_line(data = H_est, ggplot2::aes(x =x, y =y,col="Raw Estimate H")) +
        ggplot2::geom_smooth(data = H_est, ggplot2::aes(x =x, y =y,col="Smoothed Estimate H")
                             ,method="loess",se=FALSE,span = 0.3) +
        ggplot2::scale_color_manual(name = "Hurst functions", breaks=c( "Raw Estimate H", "Smoothed Estimate H"),
                                    values=c( "Raw Estimate H"="red", "Smoothed Estimate H"="green"))


    } else if(!("Raw_Est_H" %in% input$checkbox_group) && "Smooth_Est_H" %in% input$checkbox_group){
      p <- p +
        ggplot2::geom_smooth(data = H_est, ggplot2::aes(x =x, y =y,col="Smoothed Estimate H")
                             ,method="loess",se=FALSE,span = 0.3) +
        ggplot2::scale_color_manual(name = "Hurst functions", breaks=c( "Smoothed Estimate H"),
                                    values=c(  "Smoothed Estimate H"="green"))


    } else if("Raw_Est_H" %in% input$checkbox_group && !("Smooth_Est_H" %in% input$checkbox_group)){
      p <- p +
        ggplot2::geom_line(data = H_est, ggplot2::aes(x =x, y =y,col="Raw Estimate H")) +
        ggplot2::scale_color_manual(name = "Hurst functions", breaks=c( "Raw Estimate H"),
                                    values=c( "Raw Estimate H"="red"))


    } else if(!("Raw_Est_H" %in% input$checkbox_group) && !("Smooth_Est_H" %in% input$checkbox_group)){

      p <- p
    }

    }

    print(p)

  })


  simBM <- shiny::eventReactive(input$submit1, {

    x1 <- input$x1
    t_start <- input$t_start
    t_end <- input$t_end
    N2 <- input$N2
    Bm(x1=x1,t_start=t_start,t_end=t_end,N=N2)

  })

  output$bmPlot <- shiny::renderPlot({
    p1 <- ggplot2::ggplot(simBM(),ggplot2::aes(x=t,y=X)) +
      ggplot2::geom_line() +
      ggplot2::labs(y="X(t)",x="t")

    print(p1)
  })

  simFBM <- shiny::eventReactive(input$submit2, {

    H <- input$H
    t_end <- input$t_end
    N3 <- input$N3
    FBM(H=H,t_end=t_end,N=N3)

  })

  output$fbmPlot <- shiny::renderPlot({
    p2 <- ggplot2::ggplot(simFBM(),ggplot2::aes(x=t,y=X)) +
      ggplot2::geom_line() +
      ggplot2::labs(y="X(t)",x="t")

    print(p2)
  })

}

shiny::shinyApp(ui = ui, server = server)


