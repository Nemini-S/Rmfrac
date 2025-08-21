ui <- shiny::navbarPage("",
      shiny::tabPanel("Brownian Motion",
      shiny::fluidRow(
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Simulation Parameters"),
      shiny::numericInput("x_BM", "Initial value", value = 0),
      shiny::numericInput("t_start_BM", "Initial time point", value = 0, min = 0),
      shiny::numericInput("t_end_BM", "Terminal time point", value = 1, min = 0),
      shiny::numericInput("N_BM", "Number of time steps", value = 1000, min = 0, step = 1),
      shiny::actionButton("submit_BM", "Simulate")),

      shiny::wellPanel(
      shiny::h4("Excursion set and area"),
      shiny::numericInput("N_BMs", "Number of time steps", value = 10000, min = 0, step = 1),
      shiny::numericInput("A_BM", "Constant level", value = 0),
      shiny::selectInput("level_BM",label = "Compare to level",
                          choices = list("Greater" = "greater", "Lower" = "lower"), selected = "greater"),
      shiny::checkboxInput("sm_BM", "Excursion set", value = FALSE),
      shiny::checkboxInput("ea_BM", "Excursion area", value = FALSE))),

      shiny::column(8,
      shinycssloaders::withSpinner((shiny::plotOutput("bmPlot", height = "500px")), type = 8, color = "grey")),

      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Maximum and minimum"),
      shiny::checkboxInput("max_BM", "Maximum", value = FALSE),
      shiny::checkboxInput("min_BM", "Minimum", value = FALSE))),

      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Longest Streak"),
      shiny::checkboxInput("increasing_BM", "Increasing (orange)", value = FALSE),
      shiny::checkboxInput("decreasing_BM", "Decreasing (brown)", value = FALSE))))),



      shiny::tabPanel("Brownian Bridge",
      shiny::fluidRow(
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Simulation Parameters"),
      shiny::numericInput("xend_BB", "Terminating value", value = 0),
      shiny::numericInput("t_end_BB", "Terminal time point", value = 1, min = 0),
      shiny::numericInput("N_BB", "Number of time steps", value = 1000, min = 0, step = 1),
      shiny::actionButton("submit_BB", "Simulate")),

      shiny::wellPanel(
      shiny::h4("Excursion set and area"),
      shiny::numericInput("N_BBs", "Number of time steps", value = 10000, min = 0, step = 1),
      shiny::numericInput("A_BB", "Constant level", value = 0),
      shiny::selectInput("level_BB",label = "Compare to level",
                         choices = list("Greater" = "greater", "Lower" = "lower"),selected = "greater"),
      shiny::checkboxInput("sm_BB", "Excursion set", value = FALSE),
      shiny::checkboxInput("ea_BB", "Excursion area", value = FALSE))),

      shiny::column(8,
      shinycssloaders::withSpinner((shiny::plotOutput("bbPlot", height = "503px")), type = 8, color = "grey")),
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Maximum and minimum"),
      shiny::checkboxInput("max_BB", "Maximum", value = FALSE),
      shiny::checkboxInput("min_BB", "Minimum", value = FALSE))),

      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Longest Streak"),
      shiny::checkboxInput("increasing_BB", "Increasing (orange)", value = FALSE),
      shiny::checkboxInput("decreasing_BB", "Decreasing (brown)", value = FALSE))))),


      shiny::tabPanel("Fractional Brownian Motion",
      shiny::fluidRow(
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Simulation Parameters"),
      shiny::numericInput("H_FBM", "Hurst Parameter", value = 0.5, min = 0, max = 1),
      shiny::numericInput("x_FBM", "Initial value", value = 0),
      shiny::numericInput("t_start_FBM", "Initial time point", value = 0, min = 0),
      shiny::numericInput("t_end_FBM", "Terminal time point", value = 1, min = 0),
      shiny::numericInput("N_FBM", "Number of time steps", value = 1000, step = 1, min = 1),
      shiny::actionButton("submit_FBM", "Simulate")),

      shiny::wellPanel(
      shiny::h4("Excursion set and area"),
      shiny::numericInput("N_FBMs", "Number of time steps", value = 10000, min = 0, step = 1),
      shiny::numericInput("A_FBM", "Constant level", value = 0),
      shiny::selectInput("level_FBM",label = "Compare to level",
                          choices = list("Greater" = "greater", "Lower" = "lower"),selected = "greater"),
      shiny::checkboxInput("sm_FBM", "Excursion set", value = FALSE),
      shiny::checkboxInput("ea_FBM", "Excursion area", value = FALSE))),


      shiny::column(8,
      shinycssloaders::withSpinner((shiny::plotOutput("fbmPlot", height = "503px")), type = 8, color = "grey")),
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Maximum and minimum"),
      shiny::checkboxInput("max_FBM", "Maximum", value = FALSE),
      shiny::checkboxInput("min_FBM", "Minimum", value = FALSE))),
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Longest Streak"),
      shiny::checkboxInput("increasing_FBM", "Increasing (orange)", value = FALSE),
      shiny::checkboxInput("decreasing_FBM", "Decreasing (brown)", value = FALSE))))),

      shiny::tabPanel("Fractional Brownian Bridge",
      shiny::fluidRow(
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Simulation Parameters"),
      shiny::numericInput("H_FBB", "Hurst Parameter", value = 0.5, min = 0, max = 1),
      shiny::numericInput("xend_FBB", "Terminating value", value = 0),
      shiny::numericInput("t_end_FBB", "Terminal time point", value = 1, min = 0),
      shiny::numericInput("N_FBB", "Number of time steps", value = 1000, min = 0, step = 1),
      shiny::actionButton("submit_FBB", "Simulate")),

      shiny::wellPanel(
      shiny::h4("Excursion set and area"),
      shiny::numericInput("N_FBBs", "Number of time steps", value = 10000, min = 0, step = 1),
      shiny::numericInput("A_FBB", "Constant level", value = 0),
      shiny::selectInput("level_FBB",label = "Compare to level",
                         choices = list("Greater" = "greater", "Lower" = "lower"), selected = "greater"),
      shiny::checkboxInput("sm_FBB", "Excursion set", value = FALSE),
      shiny::checkboxInput("ea_FBB", "Excursion area", value = FALSE))),
      shiny::column(8,
      shinycssloaders::withSpinner((shiny::plotOutput("fbbPlot",height="576px")), type = 8, color = "grey")),
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Maximum and minimum"),
      shiny::checkboxInput("max_FBB", "Maximum", value = FALSE),
      shiny::checkboxInput("min_FBB", "Minimum", value = FALSE))),
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Longest Streak"),
      shiny::checkboxInput("increasing_FBB", "Increasing (orange)", value = FALSE),
      shiny::checkboxInput("decreasing_FBB", "Decreasing (brown)", value = FALSE))))),


      shiny::tabPanel("Fractional Gaussian Noise",
      shiny::fluidRow(
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Simulation Parameters"),
      shiny::numericInput("H_FGN", "Hurst Parameter", value = 0.5, min = 0, max = 1),
      shiny::numericInput("t_start_FGN", "Initial time point", value = 0, min = 0),
      shiny::numericInput("t_end_FGN", "Terminal time point", value = 1, min = 0),
      shiny::numericInput("N_FGN", "Number of time steps", value = 1000, min = 0, step = 1),
      shiny::actionButton("submit_FGN", "Simulate")),

      shiny::wellPanel(
      shiny::h4("Excursion set and area"),
      shiny::numericInput("N_FGNs", "Number of time steps", value = 10000, min = 0, step = 1),
      shiny::numericInput("A_FGN", "Constant level", value = 0),
      shiny::selectInput("level_FGN",label = "Compare to level",
                         choices = list("Greater" = "greater", "Lower" = "lower"), selected = "greater"),
      shiny::checkboxInput("sm_FGN", "Excursion set", value = FALSE),
      shiny::checkboxInput("ea_FGN", "Excursion area", value = FALSE))),

      shiny::column(8,
      shinycssloaders::withSpinner((shiny::plotOutput("fgnPlot", height = "500px")), type = 8, color = "grey")),
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Maximum and minimum"),
      shiny::checkboxInput("max_FGN", "Maximum", value = FALSE),
      shiny::checkboxInput("min_FGN", "Minimum", value = FALSE))),
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Longest Streak"),
      shiny::checkboxInput("increasing_FGN", "Increasing (orange)", value = FALSE),
      shiny::checkboxInput("decreasing_FGN", "Decreasing (brown)", value = FALSE))))),

      shiny::tabPanel("GHBMP",
      shiny::fluidRow(
      shiny::column(4,
      shiny::wellPanel(
      shiny::h4("Simulation"),
      shiny::textInput("func", "Hurst function (in terms of t)", "0.2 + 0*t"),
      shiny::textInput("time", "Time sequence", "seq(0, 1 , by = (1/2)^10)"),
      shiny::numericInput("num", "J (positive integer for simulation)", value = 15, min = 1, step = 1),
      shiny::actionButton("submit3", "Simulate")),
      shiny::wellPanel(
      shiny::h4("Estimation"),
      shiny::numericInput("N_int", "Number of sub-intervals for estimation", value = 100, min = 1, step = 1),
      shiny::numericInput("Q", "Q (integer for estimation (>=2))", value = 2, min = 2, step = 1),
      shiny::numericInput("L", "L (integer for estimation (>=2))", value = 2, min = 2, step = 1),
      shiny::checkboxGroupInput("checkbox_group", "Select",
                                choices = list("Theoretical Hurst function" = "H","Raw estimate of Hurst function" = "Raw_Est_H",
                                                "Smoothed estimate of Hurst function" = "Smooth_Est_H",
                                                "Raw estimate of Local Fractal Dimension" = "LFD_Est",
                                                "Smoothed estimate of Local Fractal Dimension" = "LFD_Smooth_Est")))),

     shiny::column(8,
     shinycssloaders::withSpinner((shiny::plotOutput("functionPlot", height = "500px")), type = 8, color = "grey"),
     shiny::fluidRow(
     shiny::column(6,
     shiny::wellPanel(
     shiny::h4("Excursion set and area"),
     shiny::numericInput("N_GHBMP", "Number of time steps", value = 10000, min = 0, step = 1),
     shiny::numericInput("A_GHBMP", "Constant level", value = 0),
     shiny::selectInput("level",label = "Compare to level",
                        choices = list("Greater" = "greater", "Lower" = "lower"), selected = "greater"),
     shiny::checkboxInput("sm", "Excursion set", value = FALSE),
     shiny::checkboxInput("ea", "Excursion area", value = FALSE))),

     shiny::column(5,
     shiny::wellPanel(
     shiny::h4("Maximum and minimum"),
     shiny::checkboxInput("max3", "Maximum", value = FALSE),
     shiny::checkboxInput("min3", "Minimum", value = FALSE))),
     shiny::column(5,
     shiny::wellPanel(
     shiny::h4("Longest Streak"),
     shiny::checkboxInput("increasing3", "Increasing (orange)", value = FALSE),
     shiny::checkboxInput("decreasing3", "Decreasing (brown)", value = FALSE))))))),

     shiny::tabPanel("Input Time Series",
     shiny::fluidRow(
     shiny::column(2,
     shiny::selectInput("panel_select","",choices = c("Estimations", "Excursion set and area"),
                        selected = "Estimations")), column(10)),

     shiny::fluidRow(
     shiny::column(3,
     shiny::conditionalPanel(condition = "input.panel_select == 'Estimations'",
     shiny::wellPanel(
     shiny::h4("Estimation"),
     shiny::numericInput("N_intTS", "Number of sub-intervals for estimation", value = 100, min = 1, step = 1),
     shiny::numericInput("Q_TS", "Q (integer for estimation (>=2))", value = 2, min = 2, step = 1),
     shiny::numericInput("L_TS", "L (integer for estimation (>=2))", value = 2, min = 2, step = 1),
     shiny::checkboxGroupInput("checkbox_group_TS", "Select",
                                choices = list("Raw estimate of Hurst function" = "Raw_Est_H",
                                               "Smoothed estimate of Hurst function" = "Smooth_Est_H",
                                               "Raw estimate of Local Fractal Dimension" = "LFD_Est",
                                               "Smoothed estimate of Local Fractal Dimension" = "LFD_Smooth_Est")))),

     shiny::conditionalPanel(condition = "input.panel_select=='Excursion set and area'",
     shiny::wellPanel(
     shiny::h4("Excursion set and area"),
     shiny::numericInput("N_TS", "Number of time steps", value = 10000, min = 0, step = 1),
     shiny::numericInput("A_TS", "Constant level", value = 0),
     shiny::selectInput("level_TS", label = "Compare to level",
                        choices = list("Greater" = "greater", "Lower" = "lower"), selected = "greater"),
     shiny::checkboxInput("sm_TS", "Excursion set", value = FALSE),
     shiny::checkboxInput("ea_TS", "Excursion area", value = FALSE))),


     shiny::wellPanel(
     shiny::h4("Maximum and minimum"),
     shiny::checkboxInput("max_TS", "Maximum", value = FALSE),
     shiny::checkboxInput("min_TS", "Minimum", value = FALSE)),

     shiny::wellPanel(
     shiny::h4("Longest Streak"),
     shiny::checkboxInput("increasing_TS", "Increasing (orange)", value = FALSE),
     shiny::checkboxInput("decreasing_TS", "Decreasing (brown)", value = FALSE))),

     shiny::column(9,
     shiny::wellPanel(fileInput("file", "Upload CSV File", accept = ".csv"), uiOutput("column_ui")),

     shiny::conditionalPanel(condition = "input.panel_select =='Estimations'",
     shinycssloaders::withSpinner((shiny::plotOutput("tsplot1", height = "500px")), type = 8, color = "grey")),
     shiny::conditionalPanel(condition = "input.panel_select =='Excursion set and area'",
     shinycssloaders::withSpinner((shiny::plotOutput("tsplot2", height = "500px")), type = 8, color = "grey"))

     ))),
)


server <- function(input, output, session) {

  data_input <- shiny::reactive({
    req(input$file)
    DF_Input <- read.csv(input$file$datapath, header = TRUE)
    DF_Input
  })

  output$tsplot1 <- shiny::renderPlot({

    InputTS <- data_input()
    shiny::req(InputTS)
    InputTS <- na.omit(InputTS)
    InputTS <- InputTS[order(InputTS[,1]),]

    p6 <- ggplot2::ggplot(InputTS, ggplot2::aes(x = InputTS[,1], y = InputTS[,2])) + ggplot2::geom_line() +
      ggplot2::labs(y = "Time Series", x = "Time", color = "")

    N <- input$N_intTS
    Q <- input$Q_TS
    L <- input$L_TS

    H_est <- Hurst(InputTS, N, Q, L)
    colnames(H_est) <- c("x", "y")
    IQR_H <- stats::IQR(InputTS[,2])
    range_H <- range(InputTS[,2])
    q1_H <-stats::quantile(InputTS[,2], 0.25)

    t1<-InputTS[,1]

    LFD_est <- LFD(InputTS, N, Q, L)
    colnames(LFD_est) <- c("x1", "y1")
    IQR_L <- stats::IQR(InputTS[,2])
    range_L <- range(InputTS[,2])
    q1_L <- stats::quantile(InputTS[,2], 0.25)

    if("Raw_Est_H" %in% input$checkbox_group_TS){
      p6 <- p6 + ggplot2::geom_line(data = H_est,ggplot2::aes(x = .data$x, y = ((.data$y * (IQR_H)) + q1_H), color = factor("Raw Estimate H")), linewidth = 1) +
        ggplot2::scale_y_continuous(name = "Time Series", limits = range_H,
                                    sec.axis = ggplot2::sec_axis(transform = function(x){(x - q1_H) / (IQR_H)}, name = "Estimator"))
    }

    if("Smooth_Est_H" %in% input$checkbox_group_TS){
      p6 <- p6 + ggplot2::geom_smooth(data = H_est, ggplot2::aes(x = .data$x, y = ((.data$y * (IQR_H)) + q1_H), color = factor("Smoothed Estimate H"))
                                      , method = "loess", se = FALSE, span = 0.3, linewidth = 1) +
        ggplot2::scale_y_continuous(name = "Time Series", limits = range_H,
                                    sec.axis = ggplot2::sec_axis(transform = function(x){(x - q1_H) / (IQR_H)}, name = "Estimator"))
    }

    if("LFD_Est" %in% input$checkbox_group_TS){
      p6 <- p6 + ggplot2::geom_line(data = LFD_est, ggplot2::aes(x = .data$x1, y = ((.data$y1 * (IQR_L)) + q1_L), color = factor("Raw Estimate LFD")), linewidth = 1) +
        ggplot2::scale_y_continuous(name = "Time Series", limits = range_L,
                                    sec.axis = ggplot2::sec_axis(transform = function(x){(x - q1_L) / (IQR_L)}, name = "Estimator"))

    }

    if("LFD_Smooth_Est" %in% input$checkbox_group_TS){
      p6 <- p6 + ggplot2::geom_smooth(data = LFD_est, ggplot2::aes(x = .data$x1, y = ((.data$y1 * (IQR_L)) + q1_L), color = factor("Smoothed Estimate LFD"))
                                      , method = "loess", se = FALSE, span = 0.3, linewidth = 1) +
        ggplot2::scale_y_continuous(name = "Time Series", limits = range_L,
                                    sec.axis = ggplot2::sec_axis(transform = function(x){(x - q1_L) / (IQR_L)}, name = "Estimator"))
    }



    if (!is.null(input$min_TS) && input$min_TS){

      X.minimum <- min(InputTS[,2])
      t.X.minimum <- ((InputTS[,1])[which(InputTS[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p6 <- p6 + ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5)+
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t, 2),",",round(x, 2),")")), vjust = 1.5, color = "red")
    }

    if (!is.null(input$max_TS) && input$max_TS){

      X.maximum <- max(InputTS[,2])
      t.X.maximum <- ((InputTS[,1])[which(InputTS[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p6 <- p6 + ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5)+
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max=(",round(t, 2),",",round(x, 2),")")),vjust = -0.5, color = "red")
    }


    if(!is.null(input$increasing_TS) && input$increasing_TS){
      t <- InputTS[,1]
      x <- InputTS[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p6 <- p6 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }



    }

    if (!is.null(input$decreasing_TS) && input$decreasing_TS){
      t <- InputTS[,1]
      x <- InputTS[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p6 <- p6 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }

    if (!is.null(input$sm_TS) && (input$sm_TS) && ("greater" %in% input$level_TS)){

      colnames(InputTS) <- c("x","y")
      N <- input$N_TS
      A <- input$A_TS

      ag_df <- stats::aggregate(InputTS[,2] ~ InputTS[,1], FUN = mean)
      t <- seq(ag_df[1, 1], ag_df[nrow(ag_df), 1], length.out = N + 1)
      int_X <- stats::approx(x = ag_df[,1], y = ag_df[,2], xout = t)$y
      diff <- ((ag_df[nrow(ag_df), 1] - ag_df[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p6 <- p6 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }
    }

    print(p6)

  })

  output$tsplot2 <- shiny::renderPlot({

    InputTS <- data_input()
    shiny::req(InputTS)
    InputTS <- na.omit(InputTS)
    InputTS <- InputTS[order(InputTS[,1]),]

    p7 <- ggplot2::ggplot(InputTS, ggplot2::aes(x = InputTS[,1], y = InputTS[,2])) + ggplot2::geom_line() +
      ggplot2::labs(y = "Time Series", x = "Time", color = "")


    if (!is.null(input$min_TS) && input$min_TS){

      X.minimum <- min(InputTS[,2])
      t.X.minimum <- ((InputTS[,1])[which(InputTS[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p7 <- p7 + ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5)+
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t, 2),",",round(x, 2),")")), vjust = 1.5, color = "red")
    }

    if (!is.null(input$max_TS) && input$max_TS){

      X.maximum <- max(InputTS[,2])
      t.X.maximum <- ((InputTS[,1])[which(InputTS[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p7 <- p7 + ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max = (",round(t, 2),",",round(x, 2),")")), vjust = -0.5, color = "red")
    }


    if(!is.null(input$increasing_TS) && input$increasing_TS){
      t <- InputTS[,1]
      x <- InputTS[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p7 <- p7 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }



    }

    if (!is.null(input$decreasing_TS) && input$decreasing_TS){
      t <- InputTS[,1]
      x <- InputTS[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p7 <- p7 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }

    if (!is.null(input$sm_TS) && (input$sm_TS) && ("greater" %in% input$level_TS)){

      colnames(InputTS) <- c("x", "y")
      N <- input$N_TS
      A <- input$A_TS

      ag_df <- stats::aggregate(InputTS[,2] ~ InputTS[,1], FUN = mean)
      t <- seq(ag_df[1, 1], ag_df[nrow(ag_df), 1], length.out=N + 1)
      int_X <- stats::approx(x = ag_df[,1], y = ag_df[,2], xout = t)$y
      diff <- ((ag_df[nrow(ag_df), 1] - ag_df[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p7 <- p7 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }
    }


    if (!is.null(input$sm_TS) && (input$sm_TS) && ("lower" %in% input$level_TS)){

      colnames(InputTS) <- c("x", "y")

      N <- input$N_TS
      A <- input$A_TS

      ag_df <- stats::aggregate(InputTS[,2] ~ InputTS[,1], FUN = mean)
      t <- seq(ag_df[1, 1], ag_df[nrow(ag_df), 1], length.out = N + 1)
      int_X <- stats::approx(x = ag_df[,1], y = ag_df[,2], xout = t)$y
      diff <-((ag_df[nrow(ag_df), 1] - ag_df[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 <= A) && (x2 > A)){

          S = S + (diff * (A - x1) / (x2 - x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A - x1) / (x2 - x1))

        } else if ((x1 > A) && (x2 <= A)){

          S = S + (diff * (x2 - A) / (x2 - x1))

          seg$T_start[i] <- t2 - (diff * (x2 - A) / (x2 - x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }


      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p7 <- p7 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }

    }


    if (!is.null(input$ea_TS) && (input$ea_TS) && ("greater" %in% input$level_TS)){

      colnames(InputTS) <- c("x", "y")

      N <- input$N_TS
      A <- input$A_TS

      ag_df <- stats::aggregate(InputTS[,2] ~ InputTS[,1], FUN = mean)
      t <- seq(ag_df[1, 1], ag_df[nrow(ag_df), 1], length.out = N + 1)
      int_X <- stats::approx(x = ag_df[,1], y = ag_df[,2], xout = t)$y
      diff <- ((ag_df[nrow(ag_df), 1] - ag_df[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          DF_Area <- rbind(DF_Area,data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area,data.frame(t = t2, X_t = x2))}

        } else if ((x1 >= A) && (x2 < A)){

          x_cross <- t1+(diff * (x1 - A) / (x1 - x2))
          DF_Area <- rbind(DF_Area,data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area,data.frame(t = rev(DF_Area$t), X_t = rep(A,nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 < A) && (x2 >= A)){


          x_cross <- t1 + (diff * (A - x2) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = ag_df[nrow(ag_df), 1], X_t = ag_df[nrow(ag_df), 2]))
        DF_Area <- rbind(DF_Area,data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area)> 0){

        p7 <- p7 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t ,y = X_t, group = .data$G), fill = "lightblue")
      }

    }

    if (!is.null(input$ea_TS) && (input$ea_TS) && ("lower" %in% input$level_TS)){

      colnames(InputTS) <- c("x", "y")

      N <- input$N_TS
      A <- input$A_TS

      ag_df <- stats::aggregate(InputTS[,2] ~ InputTS[,1], FUN = mean)
      t <- seq(ag_df[1, 1], ag_df[nrow(ag_df), 1], length.out = N + 1)
      int_X <- stats::approx(x = ag_df[,1], y = ag_df[,2], xout = t)$y
      diff <- ((ag_df[nrow(ag_df), 1] - ag_df[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){


          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 <= A) && (x2 > A)){

          x_cross <- t1 + (diff * (A - x1) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area,data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 > A) && (x2 <= A)){

          x_cross <- t1 + (diff * (x2 - A) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = ag_df[nrow(ag_df), 1], X_t = ag_df[nrow(ag_df), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p7 <- p7 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }
    }

    print(p7)

  })

  PR <- shiny::eventReactive(input$submit3, {

    H <- parse(text = input$func)
    Time <- tryCatch(eval(eval(parse(text = input$time))), error = function(e) NULL)
    J <- input$num

    func.H <- function(t) {
      eval(parse(text = H))
    }

    process <- GHBMP(Time, func.H, J)
    process <- process[order(process[,1]),]
    colnames(process) <- c("t1", "PP")
    process

  })


  output$functionPlot <- shiny::renderPlot({

    simPR <- PR()
    shiny::req(simPR)

    p<-ggplot2::ggplot(simPR, ggplot2::aes(x =t1, y =PP)) + ggplot2::geom_line() +
      ggplot2::labs(y = "X(t)", x = "t", color = "")

    N <- input$N_int
    Q <- input$Q
    L <- input$L

    H <- parse(text = input$func)
    func.H <- function(t) {
      eval(parse(text = H))
    }

    H_est <- Hurst(simPR, N, Q, L)
    colnames(H_est) <- c("x", "y")
    t1 <- simPR[,1]

    LFD_est <- LFD(simPR, N, Q, L)
    colnames(LFD_est) <- c("x1", "y1")

    if("H" %in% input$checkbox_group){

      H1 <- sapply(t1, func.H)
      data1 <- data.frame(t1, H1)

      p <- p + ggplot2::geom_line(data = data1, ggplot2::aes(x = .data$t1, y = .data$H1,color = factor("Theoretical H")), linewidth = 1)

    }

    if("Raw_Est_H" %in% input$checkbox_group){
      p <- p + ggplot2::geom_line(data = H_est, ggplot2::aes(x = .data$x, y = .data$y, color = factor("Raw Estimate H")), linewidth = 1)

    }

    if("Smooth_Est_H" %in% input$checkbox_group){
      p <- p + ggplot2::geom_smooth(data = H_est, ggplot2::aes(x = .data$x, y = .data$y, color = factor("Smoothed Estimate H"))
                                    , method = "loess", se = FALSE, span = 0.3, linewidth = 1)

    }

    if("LFD_Est" %in% input$checkbox_group){

      p <- p + ggplot2::geom_line(data = LFD_est, ggplot2::aes(x = .data$x1, y = .data$y1, color = factor("Raw Estimate LFD")), linewidth = 1)

    }

    if("LFD_Smooth_Est"%in% input$checkbox_group){

      p <- p + ggplot2::geom_smooth(data = LFD_est, ggplot2::aes(x = .data$x1, y = .data$y1, color = factor("Smoothed Estimate LFD"))
                                    , method = "loess", se = FALSE, span = 0.3, linewidth = 1)

    }

    if (!is.null(input$max3) && input$max3){

      X.maximum <- max(simPR[,2])
      t.X.maximum <- ((simPR[,1])[which(simPR[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p <- p+ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5)+
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max = (",round(t, 2),",",round(x, 2),")")),vjust = -0.5,color = "red")
    }

    if (!is.null(input$min3) && input$min3){

      X.minimum <- min(simPR[,2])
      t.X.minimum <- ((simPR[,1])[which(simPR[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p <- p + ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t,2),",",round(x,2),")")), vjust = 1.5, color = "red")
    }

    if(!is.null(input$increasing3) && input$increasing3){
      t <- simPR[,1]
      x <- simPR[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p <- p + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }



    }

    if (!is.null(input$decreasing3) && input$decreasing3){
      t <- simPR[,1]
      x <- simPR[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p <- p + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }


    if (!is.null(input$sm) && (input$sm) && ("greater" %in% input$level)){

      colnames(simPR) <- c("x", "y")
      N <- input$N_GHBMP
      A <- input$A_GHBMP

      t <- seq(simPR[1, 1], simPR[nrow(simPR), 1], length.out = N + 1)
      int_X <- stats::approx(simPR[,1], simPR[,2], xout = t)$y
      diff <- ((simPR[nrow(simPR), 1] - simPR[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p <- p + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0) ,color = "red")
      }
    }


    if (!is.null(input$sm) && (input$sm) && ("lower" %in% input$level)){

      colnames(simPR) <- c("x", "y")

      N <- input$N_GHBMP
      A <- input$A_GHBMP

      t <- seq(simPR[1, 1], simPR[nrow(simPR), 1], length.out = N + 1)
      int_X <- stats::approx(simPR[,1], simPR[,2], xout = t)$y
      diff<-((simPR[nrow(simPR), 1] - simPR[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 <= A) && (x2 > A)){

          S = S + (diff * (A - x1) / (x2 - x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A - x1) / (x2 - x1))

        } else if ((x1 > A) && (x2 <= A)){

          S = S + (diff * (x2 - A) / (x2 - x1))

          seg$T_start[i] <- t2 - (diff * (x2 - A) / (x2 - x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }


      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p <- p + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }

    }


    if (!is.null(input$ea) && (input$ea) && ("greater" %in% input$level)){

      colnames(simPR) <- c("x", "y")

      N <- input$N_GHBMP
      A <- input$A_GHBMP

      t <- seq(simPR[1, 1], simPR[nrow(simPR), 1], length.out = N + 1)
      int_X <- stats::approx(simPR[,1], simPR[,2], xout = t)$y
      diff <- ((simPR[nrow(simPR), 1] - simPR[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 >= A) && (x2 < A)){

          x_cross <- t1 + (diff * (x1 - A) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 < A) && (x2 >= A)){


          x_cross <- t1 + (diff * (A - x2) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simPR[nrow(simPR), 1],X_t = simPR[nrow(simPR), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p <- p + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data=DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }

    }

    if (!is.null(input$ea) && (input$ea) && ("lower" %in% input$level)){

      colnames(simPR) <- c("x", "y")

      N <- input$N_GHBMP
      A <- input$A_GHBMP

      t <- seq(simPR[1, 1], simPR[nrow(simPR), 1], length.out = N + 1)
      int_X <- stats::approx(simPR[,1], simPR[,2], xout = t)$y
      diff<-((simPR[nrow(simPR), 1] - simPR[1, 1]) / N)


      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){


          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 <= A) && (x2 > A)){

          x_cross <- t1 + (diff * (A - x1) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area,data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 > A) && (x2 <= A)){

          x_cross <- t1 + (diff * (x2 - A) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simPR[nrow(simPR), 1], X_t = simPR[nrow(simPR), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p <- p + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area,ggplot2::aes(x = t ,y = X_t, group = .data$G), fill = "lightblue")
      }
    }

    print(p)

  })


  simBM <- shiny::eventReactive(input$submit_BM, {

    x1 <- input$x_BM
    t_start <- input$t_start_BM
    t_end <- input$t_end_BM
    N <- input$N_BM
    Bm(x_start = x1, t_start = t_start, t_end = t_end, N = N)

  })

  output$bmPlot <- shiny::renderPlot({
    simBMdf <- simBM()
    shiny::req(simBMdf)
    simBMdf <- simBMdf[order(simBMdf[,1]), ]

    p1 <- ggplot2::ggplot(simBMdf, ggplot2::aes(x = t, y = X)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = "X(t)", x = "t")

    if (!is.null(input$max_BM) && input$max_BM){

      X.maximum<-max(simBMdf[,2])
      t.X.maximum<-((simBMdf[,1])[which(simBMdf[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p1 <- p1 + ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max = (",round(t, 2),",",round(x, 2),")")),vjust = -0.5, color = "red")
    }

    if (!is.null(input$min_BM) && input$min_BM){

      X.minimum <- min(simBMdf[,2])
      t.X.minimum <- ((simBMdf[,1])[which(simBMdf[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p1 <- p1+ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t, 2),",",round(x, 2),")")), vjust = 1.5, color = "red")
    }


    if(!is.null(input$increasing_BM) && input$increasing_BM){
      t <- simBMdf[,1]
      x <- simBMdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p1 <- p1 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }



    }

    if (!is.null(input$decreasing_BM) && input$decreasing_BM){
      t <- simBMdf[,1]
      x <- simBMdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p1 <- p1 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }


    if (!is.null(input$sm_BM) && (input$sm_BM) && ("greater" %in% input$level_BM)){

      colnames(simBMdf) <- c("x", "y")
      N <- input$N_BMs
      A <- input$A_BM

      t <- seq(simBMdf[1, 1], simBMdf[nrow(simBMdf), 1], length.out = N + 1)
      int_X <- stats::approx(simBMdf[,1], simBMdf[,2], xout = t)$y
      diff<-((simBMdf[nrow(simBMdf), 1] - simBMdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg)>0){

        p1 <- p1 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0) , color = "red")
      }
    }


    if (!is.null(input$sm_BM) && (input$sm_BM) && ("lower" %in% input$level_BM)){

      colnames(simBMdf) <- c("x","y")

      N <- input$N_BMs
      A <- input$A_BM

      t <- seq(simBMdf[1, 1], simBMdf[nrow(simBMdf), 1], length.out = N + 1)
      int_X <- stats::approx(simBMdf[,1], simBMdf[,2], xout = t)$y
      diff <- ((simBMdf[nrow(simBMdf), 1] - simBMdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 <= A) && (x2 > A)){

          S = S + (diff * (A - x1) / (x2 - x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A-x1)/(x2-x1))

        } else if ((x1 > A) && (x2 <= A)){

          S = S + (diff * (x2 - A) / (x2 - x1))

          seg$T_start[i] <- t2 - (diff * (x2 - A) / (x2 - x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }


      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p1 <- p1 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }

    }


    if (!is.null(input$ea_BM) && (input$ea_BM) && ("greater" %in% input$level_BM)){

      colnames(simBMdf) <- c("x", "y")

      N <- input$N_BMs
      A <- input$A_BM

      t <- seq(simBMdf[1, 1], simBMdf[nrow(simBMdf), 1], length.out=N + 1)
      int_X <- stats::approx(simBMdf[,1], simBMdf[,2], xout = t)$y
      diff<-((simBMdf[nrow(simBMdf), 1] - simBMdf[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 >= A) && (x2 < A)){

          x_cross <- t1 + (diff * (x1 - A) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = t1,X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 < A) && (x2 >= A)){


          x_cross <- t1 + (diff * (A - x2) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simBMdf[nrow(simBMdf), 1], X_t = simBMdf[nrow(simBMdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p1 <- p1 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t,y = X_t, group = .data$G), fill = "lightblue")
      }

    }

    if (!is.null(input$ea_BM) && (input$ea_BM) && ("lower" %in% input$level_BM)){

      colnames(simBMdf) <- c("x", "y")

      N <- input$N_BMs
      A <- input$A_BM

      t <- seq(simBMdf[1, 1], simBMdf[nrow(simBMdf), 1],length.out = N + 1)
      int_X <- stats::approx(simBMdf[,1],simBMdf[,2], xout = t)$y
      diff <- ((simBMdf[nrow(simBMdf), 1]-simBMdf[1, 1]) / N)


      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){


          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i==N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 <= A) && (x2 > A)){

          x_cross <- t1 + (diff * (A - x1) / (x2 - x1))
          DF_Area <- rbind(DF_Area,data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0) ,X_t = numeric(0))

        } else if ((x1 > A) && (x2 <= A)){

          x_cross <- t1 + (diff * (x2 - A) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simBMdf[nrow(simBMdf), 1], X_t = simBMdf[nrow(simBMdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p1 <- p1 + ggplot2::geom_hline(yintercept = A,color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t,y = X_t, group = .data$G), fill = "lightblue")
      }
    }





    print(p1)
  })

  #Brownian Bridge

  simBB <- shiny::eventReactive(input$submit_BB, {

    x_end <- input$xend_BB
    t_end <- input$t_end_BB
    N <- input$N_BB
    Bbridge(x_end = x_end, t_end = t_end, N = N)
  })

  output$bbPlot <- shiny::renderPlot({
    simBBdf <- simBB()
    shiny::req(simBBdf)
    simBBdf <- simBBdf[order(simBBdf[,1]), ]

    p2 <- ggplot2::ggplot(simBBdf, ggplot2::aes(x = t, y = X)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = "X(t)", x = "t")

    if (!is.null(input$max_BB) && input$max_BB){

      X.maximum <- max(simBBdf[,2])
      t.X.maximum <- ((simBBdf[,1])[which(simBBdf[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p2 <- p2+ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5)+
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max = (",round(t, 2),",",round(x, 2),")")), vjust = -0.5, color = "red")
    }

    if (!is.null(input$min_BB) && input$min_BB){

      X.minimum <- min(simBBdf[,2])
      t.X.minimum <- ((simBBdf[,1])[which(simBBdf[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p2 <- p2 + ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t, 2),",",round(x, 2),")")), vjust = 1.5, color = "red")
    }


    if(!is.null(input$increasing_BB) && input$increasing_BB){
      t <- simBBdf[,1]
      x <- simBBdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p2 <- p2 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }



    }

    if (!is.null(input$decreasing_BB) && input$decreasing_BB){
      t <- simBBdf[,1]
      x <- simBBdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p2 <- p2 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }



    if (!is.null(input$sm_BB) && (input$sm_BB) && ("greater" %in% input$level_BB)){

      colnames(simBBdf) <- c("x","y")
      N <- input$N_BBs
      A <- input$A_BB

      t <- seq(simBBdf[1, 1], simBBdf[nrow(simBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simBBdf[,1], simBBdf[,2], xout = t)$y
      diff <- ((simBBdf[nrow(simBBdf), 1] - simBBdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p2 <- p2 + ggplot2::geom_hline(yintercept = A, color="blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }
    }


    if (!is.null(input$sm_BB) && (input$sm_BB) && ("lower" %in% input$level_BB)){

      colnames(simBBdf) <- c("x", "y")

      N <- input$N_BBs
      A <- input$A_BB

      t <- seq(simBBdf[1, 1], simBBdf[nrow(simBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simBBdf[,1], simBBdf[,2], xout = t)$y
      diff <- ((simBBdf[nrow(simBBdf), 1] - simBBdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 <= A) && (x2 > A)){

          S = S + (diff * (A - x1) / (x2 - x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A - x1) / (x2 - x1))

        } else if ((x1 > A) && (x2 <= A)){

          S = S + (diff * (x2 - A) / (x2 - x1))

          seg$T_start[i] <- t2 - (diff * (x2 - A) / (x2 - x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }


      seg <- na.omit(seg)

      if (nrow(seg)>0){

        p2 <- p2 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }

    }


    if (!is.null(input$ea_BB) && (input$ea_BB) && ("greater" %in% input$level_BB)){

      colnames(simBBdf) <- c("x", "y")

      N <- input$N_BBs
      A <- input$A_BB

      t <- seq(simBBdf[1, 1], simBBdf[nrow(simBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simBBdf[,1], simBBdf[,2], xout = t)$y
      diff <- ((simBBdf[nrow(simBBdf) ,1] - simBBdf[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area,data.frame(t = t2, X_t = x2))}

        } else if ((x1 >= A) && (x2 < A)){

          x_cross <- t1 + (diff * (x1 - A) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 < A) && (x2 >= A)){


          x_cross <- t1 + (diff * (A - x2) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simBBdf[nrow(simBBdf), 1], X_t = simBBdf[nrow(simBBdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p2 <- p2 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }

    }

    if (!is.null(input$ea_BB) && (input$ea_BB) && ("lower" %in% input$level_BB)){

      colnames(simBBdf) <- c("x", "y")

      N <- input$N_BBs
      A <- input$A_BB

      t <- seq(simBBdf[1, 1], simBBdf[nrow(simBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simBBdf[,1], simBBdf[,2], xout = t)$y
      diff <- ((simBBdf[nrow(simBBdf), 1] - simBBdf[1, 1]) / N)


      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){


          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 <= A) && (x2 > A)){

          x_cross <- t1 + (diff * (A - x1) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 > A) && (x2 <= A)){

          x_cross <- t1 + (diff * (x2 - A) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simBBdf[nrow(simBBdf), 1], X_t = simBBdf[nrow(simBBdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p2 <- p2 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }
    }
    print(p2)
  })

  #Fractional Brownian Motion

  simFBM <- shiny::eventReactive(input$submit_FBM, {

    H <- input$H_FBM
    x_start <- input$x_FBM
    t_start <- input$t_start_FBM
    t_end <- input$t_end_FBM
    N <- input$N_FBM
    FBm(H = H, x_start = x_start, t_start = t_start, t_end = t_end, N = N)
  })

  output$fbmPlot <- shiny::renderPlot({

    simFBMdf <- simFBM()
    shiny::req(simFBMdf)
    simFBMdf <- simFBMdf[order(simFBMdf[,1]),]

    p3 <- ggplot2::ggplot(simFBMdf, ggplot2::aes(x = t, y = X)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = "X(t)", x = "t")

    if (!is.null(input$max_FBM) && input$max_FBM){

      X.maximum <- max(simFBMdf[,2])
      t.X.maximum <- ((simFBMdf[,1])[which(simFBMdf[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p3 <- p3+ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max = (",round(t, 2),",",round(x, 2),")")), vjust = -0.5, color = "red")
    }

    if (!is.null(input$min_FBM) && input$min_FBM){

      X.minimum <- min(simFBMdf[,2])
      t.X.minimum <- ((simFBMdf[,1])[which(simFBMdf[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p3 <- p3 + ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t, 2),",",round(x, 2),")")), vjust = 1.5, color = "red")
    }


    if(!is.null(input$increasing_FBM) && input$increasing_FBM){
      t <- simFBMdf[,1]
      x <- simFBMdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p3 <- p3 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }



    }

    if (!is.null(input$decreasing_FBM) && input$decreasing_FBM){

      t <- simFBMdf[,1]
      x <- simFBMdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p3 <- p3 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }



    if (!is.null(input$sm_FBM) && (input$sm_FBM) && ("greater" %in% input$level_FBM)){

      colnames(simFBMdf) <- c("x", "y")
      N <- input$N_FBMs
      A <- input$A_FBM

      t <- seq(simFBMdf[1, 1], simFBMdf[nrow(simFBMdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBMdf[,1], simFBMdf[,2], xout = t)$y
      diff <- ((simFBMdf[nrow(simFBMdf), 1] - simFBMdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p3 <- p3 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }
    }


    if (!is.null(input$sm_FBM) && (input$sm_FBM) && ("lower" %in% input$level_FBM)){

      colnames(simFBMdf) <- c("x", "y")

      N <- input$N_FBMs
      A <- input$A_FBM

      t <- seq(simFBMdf[1, 1], simFBMdf[nrow(simFBMdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBMdf[,1], simFBMdf[,2], xout = t)$y
      diff<-((simFBMdf[nrow(simFBMdf), 1] - simFBMdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 <= A) && (x2 > A)){

          S = S + (diff * (A - x1) / (x2 - x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A - x1) / (x2 - x1))

        } else if ((x1 > A) && (x2 <= A)){

          S = S + (diff * (x2 - A) / (x2 - x1))

          seg$T_start[i] <- t2 - (diff * (x2 - A) / (x2 - x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }


      seg <- na.omit(seg)

      if (nrow(seg)>0){

        p3 <- p3 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }

    }


    if (!is.null(input$ea_FBM) && (input$ea_FBM) && ("greater" %in% input$level_FBM)){

      colnames(simFBMdf) <- c("x", "y")

      N <- input$N_FBMs
      A <- input$A_FBM

      t <- seq(simFBMdf[1, 1], simFBMdf[nrow(simFBMdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBMdf[,1], simFBMdf[,2], xout = t)$y
      diff <- ((simFBMdf[nrow(simFBMdf), 1] - simFBMdf[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 >= A) && (x2 < A)){

          x_cross <- t1 + (diff * (x1 - A) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 < A) && (x2 >= A)){


          x_cross <- t1 + (diff * (A - x2) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simFBMdf[nrow(simFBMdf), 1], X_t = simFBMdf[nrow(simFBMdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p3 <- p3 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }

    }

    if (!is.null(input$ea_FBM) && (input$ea_FBM) && ("lower" %in% input$level_FBM)){

      colnames(simFBMdf) <- c("x", "y")

      N <- input$N_FBMs
      A <- input$A_FBM

      t <- seq(simFBMdf[1, 1], simFBMdf[nrow(simFBMdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBMdf[,1], simFBMdf[,2], xout = t)$y
      diff<-((simFBMdf[nrow(simFBMdf), 1] - simFBMdf[1, 1]) / N)


      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){


          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 <= A) && (x2 > A)){

          x_cross <- t1 + (diff * (A - x1) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 > A) && (x2 <= A)){

          x_cross <- t1 + (diff * (x2 - A) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simFBMdf[nrow(simFBMdf), 1], X_t = simFBMdf[nrow(simFBMdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p3 <- p3 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }
    }

    print(p3)
  })

  #Fractional Brownian bridge

  simFBB <- shiny::eventReactive(input$submit_FBB, {

    H <- input$H_FBB
    x_end <- input$xend_FBB
    t_end <- input$t_end_FBB
    N <- input$N_FBB
    FBbridge(H = H, x_end = x_end, t_end = t_end, N = N)
  })

  output$fbbPlot <- shiny::renderPlot({


    simFBBdf <- simFBB()
    shiny::req(simFBBdf)
    simFBBdf <- simFBBdf[order(simFBBdf[,1]), ]

    p4 <- ggplot2::ggplot(simFBBdf, ggplot2::aes(x = t, y = X)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = "X(t)", x = "t")

    if (!is.null(input$max_FBB) && input$max_FBB){

      X.maximum <- max(simFBBdf[,2])
      t.X.maximum <- ((simFBBdf[,1])[which(simFBBdf[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p4 <- p4 + ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max = (",round(t, 2),",",round(x, 2),")")), vjust = -0.5, color = "red")
    }

    if (!is.null(input$min_FBB) && input$min_FBB){

      X.minimum <- min(simFBBdf[,2])
      t.X.minimum <- ((simFBBdf[,1])[which(simFBBdf[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p4 <- p4 + ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t, 2),",",round(x, 2),")")), vjust = 1.5, color = "red")
    }


    if(!is.null(input$increasing_FBB) && input$increasing_FBB){
      t <- simFBBdf[,1]
      x <- simFBBdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p4 <- p4 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }



    }

    if (!is.null(input$decreasing_FBB) && input$decreasing_FBB){

      t <- simFBBdf[,1]
      x <- simFBBdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p4 <- p4 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }

    if (!is.null(input$sm_FBB) && (input$sm_FBB) && ("greater" %in% input$level_FBB)){

      colnames(simFBBdf) <- c("x", "y")
      N <- input$N_FBBs
      A <- input$A_FBB

      t <- seq(simFBBdf[1, 1], simFBBdf[nrow(simFBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBBdf[,1], simFBBdf[,2], xout = t)$y
      diff <- ((simFBBdf[nrow(simFBBdf),1] - simFBBdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p4 <- p4 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }
    }


    if (!is.null(input$sm_FBB) && (input$sm_FBB) && ("lower" %in% input$level_FBB)){

      colnames(simFBBdf) <- c("x", "y")

      N <- input$N_FBBs
      A <- input$A_FBB

      t <- seq(simFBBdf[1, 1], simFBBdf[nrow(simFBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBBdf[,1], simFBBdf[,2], xout = t)$y
      diff <- ((simFBBdf[nrow(simFBBdf), 1] - simFBBdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 <= A) && (x2 > A)){

          S = S + (diff * (A - x1) / (x2 - x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A - x1) / (x2 - x1))

        } else if ((x1 > A) && (x2 <= A)){

          S = S + (diff * (x2 - A) / (x2 - x1))

          seg$T_start[i] <- t2 - (diff * (x2 - A) / (x2 - x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }


      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p4 <- p4 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }

    }


    if (!is.null(input$ea_FBB) && (input$ea_FBB) && ("greater" %in% input$level_FBB)){

      colnames(simFBBdf) <- c("x","y")

      N <- input$N_FBBs
      A <- input$A_FBB

      t <- seq(simFBBdf[1, 1], simFBBdf[nrow(simFBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBBdf[,1], simFBBdf[,2], xout = t)$y
      diff<-((simFBBdf[nrow(simFBBdf), 1] - simFBBdf[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 >= A) && (x2 < A)){

          x_cross <- t1 + (diff * (x1 - A) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 < A) && (x2 >= A)){


          x_cross <- t1 + (diff * (A - x2) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simFBBdf[nrow(simFBBdf), 1], X_t = simFBBdf[nrow(simFBBdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p4 <- p4 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }

    }

    if (!is.null(input$ea_FBB) && (input$ea_FBB) && ("lower" %in% input$level_FBB)){

      colnames(simFBBdf) <- c("x", "y")

      N <- input$N_FBBs
      A <- input$A_FBB

      t <- seq(simFBBdf[1, 1], simFBBdf[nrow(simFBBdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFBBdf[,1], simFBBdf[,2], xout = t)$y
      diff <- ((simFBBdf[nrow(simFBBdf), 1] - simFBBdf[1, 1]) / N)


      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){


          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 <= A) && (x2 > A)){

          x_cross <- t1 + (diff * (A - x1) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 > A) && (x2 <= A)){

          x_cross <- t1 + (diff * (x2 - A) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simFBBdf[nrow(simFBBdf), 1], X_t = simFBBdf[nrow(simFBBdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area)>0){

        p4 <- p4 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }
    }

    print(p4)
  })


  simFGN <- shiny::eventReactive(input$submit_FGN, {

    H <- input$H_FGN
    t_start <- input$t_start_FGN
    t_end <- input$t_end_FGN
    n <- input$N_FGN
    FGn(H = H, t_start = t_start, t_end = t_end, n = n)
  })

  output$fgnPlot <- shiny::renderPlot({


    simFGNdf <- simFGN()
    shiny::req(simFGNdf)
    simFGNdf <- simFGNdf[order(simFGNdf[,1]), ]

    p5 <- ggplot2::ggplot(simFGNdf,ggplot2::aes(x = t, y = X)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = "X(t)", x = "t")

    if (!is.null(input$max_FGN) && input$max_FGN){

      X.maximum <- max(simFGNdf[,2])
      t.X.maximum <- ((simFGNdf[,1])[which(simFGNdf[,2] == X.maximum)])
      max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

      p5 <- p5 + ggplot2::geom_point(data = max_points_df, ggplot2::aes(x = t, y = x), color = "red", size = 1.5) +
        ggplot2::geom_text(data = max_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Max = (",round(t, 2),",",round(x, 2),")")), vjust = -0.5, color = "red")
    }

    if (!is.null(input$min_FGN) && input$min_FGN){

      X.minimum <- min(simFGNdf[,2])
      t.X.minimum <- ((simFGNdf[,1])[which(simFGNdf[,2] == X.minimum)])
      min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

      p5 <- p5 + ggplot2::geom_point(data = min_points_df, ggplot2::aes(x = t, y = x), color="red", size = 1.5) +
        ggplot2::geom_text(data = min_points_df,
                           ggplot2::aes(x = t, y = x, label = paste0("Min = (",round(t, 2),",",round(x, 2),")")), vjust = 1.5, color = "red")
    }


    if(!is.null(input$increasing_FGN) && input$increasing_FGN){
      t <- simFGNdf[,1]
      x <- simFGNdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p5 <- p5 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "orange", linewidth = 1)

      }

    }

    if (!is.null(input$decreasing_FGN) && input$decreasing_FGN){

      t <- simFGNdf[,1]
      x <- simFGNdf[,2]

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }


      if (length(streak) > 0) {

        time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

        max_int <- max(time_int)

        longest_streak <- streak[time_int == max_int]

        long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

        for (i in 1:length(longest_streak)) {
          id <- longest_streak[[i]][1]:longest_streak[[i]][2]
          df1 <- data.frame(t = t[id], x = x[id], group = i)
          long_streak_df <- rbind(long_streak_df, df1)
        } }

      else{
        long_streak_df <- NULL
      }


      if (!is.null(long_streak_df)) {
        p5 <- p5 + ggplot2::geom_line(data = long_streak_df, ggplot2::aes(x = t, y = x, group = group), color = "brown", linewidth = 1)

      }

    }


    if (!is.null(input$sm_FGN) && (input$sm_FGN) && ("greater" %in% input$level_FGN)){

      colnames(simFGNdf) <- c("x", "y")
      N <- input$N_FGNs
      A <- input$A_FGN

      t <- seq(simFGNdf[1, 1], simFGNdf[nrow(simFGNdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFGNdf[,1], simFGNdf[,2], xout = t)$y
      diff <- ((simFGNdf[nrow(simFGNdf), 1] - simFGNdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 >= A) && (x2 < A)){

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1 - A) / (x1 - x2))

        } else if ((x1 < A) && (x2 >= A)){

          seg$T_start[i] <- t2 - (diff * (A - x2) / (x1 - x2))
          seg$T_end[i] <- t2

        } else {

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

      seg <- na.omit(seg)

      if (nrow(seg)>0){

        p5 <- p5 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }
    }


    if (!is.null(input$sm_FGN) && (input$sm_FGN) && ("lower" %in% input$level_FGN)){

      colnames(simFGNdf) <- c("x", "y")

      N <- input$N_FGNs
      A <- input$A_FGN

      t <- seq(simFGNdf[1, 1], simFGNdf[nrow(simFGNdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFGNdf[,1], simFGNdf[,2], xout = t)$y
      diff<-((simFGNdf[nrow(simFGNdf), 1] - simFGNdf[1, 1]) / N)

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1 <= A) && (x2 > A)){

          S = S + (diff * (A - x1) / (x2 - x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A - x1) / (x2 - x1))

        } else if ((x1 > A) && (x2 <= A)){

          S = S + (diff * (x2 - A) / (x2 - x1))

          seg$T_start[i] <- t2 - (diff * (x2 - A) / (x2 - x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }


      seg <- na.omit(seg)

      if (nrow(seg) > 0){

        p5 <- p5 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_segment(data = seg, ggplot2::aes(x = T_start, xend = T_end, y = 0, yend = 0), color = "red")
      }

    }


    if (!is.null(input$ea_FGN) && (input$ea_FGN) && ("greater" %in% input$level_FGN)){

      colnames(simFGNdf) <- c("x", "y")

      N <- input$N_FGNs
      A <- input$A_FGN

      t <- seq(simFGNdf[1, 1], simFGNdf[nrow(simFGNdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFGNdf[,1], simFGNdf[,2], xout = t)$y
      diff<-((simFGNdf[nrow(simFGNdf), 1] - simFGNdf[1, 1]) / N)

      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 >= A) && (x2 >= A)){

          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 >= A) && (x2 < A)){

          x_cross <- t1 + (diff * (x1 - A) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 < A) && (x2 >= A)){


          x_cross <- t1 + (diff * (A - x2) / (x1 - x2))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simFGNdf[nrow(simFGNdf), 1], X_t = simFGNdf[nrow(simFGNdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area)>0){

        p5 <- p5 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t,y = X_t, group = .data$G), fill = "lightblue")
      }

    }

    if (!is.null(input$ea_FGN) && (input$ea_FGN) && ("lower" %in% input$level_FGN)){

      colnames(simFGNdf) <- c("x", "y")

      N <- input$N_FGNs
      A <- input$A_FGN

      t <- seq(simFGNdf[1, 1], simFGNdf[nrow(simFGNdf), 1], length.out = N + 1)
      int_X <- stats::approx(simFGNdf[,1], simFGNdf[,2], xout = t)$y
      diff <- ((simFGNdf[nrow(simFGNdf), 1] - simFGNdf[1, 1]) / N)


      Area <- 0
      DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i + 1]

        t1 <- t[i]
        t2 <- t[i + 1]

        if((x1 <= A) && (x2 <= A)){


          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1))
          if (i == N) {
            DF_Area <- rbind(DF_Area, data.frame(t = t2, X_t = x2))}

        } else if ((x1 <= A) && (x2 > A)){

          x_cross <- t1 + (diff * (A - x1) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = t1, X_t = x1),
                           data.frame(t = x_cross, X_t = A),
                           data.frame(t = x_cross, X_t = A))

          DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t = numeric(0), X_t = numeric(0))

        } else if ((x1 > A) && (x2 <= A)){

          x_cross <- t1 + (diff * (x2 - A) / (x2 - x1))
          DF_Area <- rbind(DF_Area, data.frame(t = x_cross, X_t = A))

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area, data.frame(t = simBMdf[nrow(simBMdf), 1], X_t = simBMdf[nrow(simBMdf), 2]))
        DF_Area <- rbind(DF_Area, data.frame(t = rev(DF_Area$t), X_t = rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

      DF_Area <- na.omit(DF_Area)

      if (!is.null(DF_Area) && nrow(DF_Area) > 0){

        p5 <- p5 + ggplot2::geom_hline(yintercept = A, color = "blue", linetype = "dashed") +
          ggplot2::geom_polygon(data = DF_Area, ggplot2::aes(x = t, y = X_t, group = .data$G), fill = "lightblue")
      }
    }

    print(p5)
  })


}

shiny::shinyApp(ui = ui, server = server)


