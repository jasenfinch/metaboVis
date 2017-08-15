#' LDA
#' @importFrom shiny shinyApp fluidPage sidebarPanel mainPanel
#' @importFrom shiny uiOutput tags fluidRow reactive renderUI
#' @importFrom shiny selectInput headerPanel downloadButton
#' @importFrom shinythemes shinytheme
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline geom_point
#' @importFrom ggplot2 xlab ylab theme_bw ggtitle labs scale_shape_manual
#' @importFrom ggplot2 scale_colour_manual geom_bar
#' @importFrom ggthemes scale_colour_ptol ptol_pal
#' @importFrom FIEmspro nlda

LDA <- function(){
  # options(shiny.sanitize.errors = TRUE)
  shinyApp(
    ui = (fluidPage(theme = shinytheme('flatly'),
                    headerPanel('LDA'),
                    sidebarPanel(
                      uiOutput('object'),
                      uiOutput('classes'),
                      tags$hr(),
                      uiOutput('dfs1'),
                      uiOutput('dfs2'),
                      tags$hr(),
                      downloadButton('export',label = 'Export Data'),
                      tags$hr(),
                      tags$button(
                        id = 'close',
                        type = "button",
                        class = "btn action-button",
                        onclick = "setTimeout(function(){window.close();},500);",  # close browser
                        "Exit"
                      )
                    ),
                    mainPanel(
                      fluidRow(
                        uiOutput('LDA')
                      ),
                      tags$hr(),
                      fluidRow(
                        uiOutput('Loadings')
                      ),
                      tags$hr(),
                      fluidRow(
                        uiOutput('Tw')
                      )
                    )
    )
    ),
    server = function(input, output) {
      getData <- reactive({
        if (is.null(input$Object) | input$Object == '') {

        } else {
          analysis <- get(input$Object)
          if (class(analysis) == 'Workflow') {
            analysis <- analysis@analysed
          }
          if (length(analysis@preTreated) > 0) {
            dat <- analysis@preTreated$Data
            info <- analysis@preTreated$Info
          } else {
            dat <- analysis@rawData$Data
            info <- analysis@rawData$Info
          }

          return(list(dat = dat,info = info))
        }
      })

      getLDA <- reactive({
       dat <- getData()
       info <- dat$info
       dat <- dat$dat
       lda <- nlda(dat,cl = unlist(info[,input$Class]),scale = T,center = T)
       return(list(dat = dat, info = info, lda = lda))
      })

      availObjects <- reactive({
        ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv),function(x){class(get(x,envir = .GlobalEnv))[1]}) %in% c('Workflow','Analysis')]
      })

      output$object <- renderUI({
        selectInput('Object','Object',availObjects())
      })

      availClasses <- reactive({
        d <- getData()
        cls <- apply(d$info,2,function(x){length(unique(x))})
        minCls <- apply(d$info,2,function(x){min(table(x))})
        rev(colnames(d$info)[cls > 1 & minCls > 1])
      })

      output$classes <- renderUI({
        selectInput('Class','Class',availClasses())
      })

      availDFS <- reactive({
        d <- getLDA()
        colnames(d$lda$x)
      })

      output$dfs1 <- renderUI({
        selectInput('ldaXaxis','X axis',choices = availDFS(),selected = 'DF1')
      })

      output$dfs2 <- renderUI({
        selectInput('ldaYaxis','Y axis',choices = availDFS(),selected = 'DF2')
      })

      output$lda <- renderPlotly({
        d <- getLDA()
        tw <- d$lda$Tw
        tw <- round(tw,2)
        res <- data.frame(X = d$lda$x[,input$ldaXaxis],Y = d$lda$x[,input$ldaYaxis],Class = factor(unlist(d$info[,input$Class])),d$info)
        p <- ggplot(res) +
          geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
          geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
          geom_point(aes_string(x = 'X',y = 'Y',colour = 'Class',shape = 'Class')) +
          xlab(paste(input$ldaXaxis,' (Tw: ',tw[input$ldaXaxis],')',sep = '')) +
          ylab(paste(input$ldaYaxis,' (Tw: ',tw[input$ldaYaxis],')',sep = '')) +
          theme_bw() +
          labs(colour = '')

        cls <- length(unique(res$Class))

        if (cls <= 12) {
          p <- p + scale_colour_ptol()
        } else {
          if (cls %% 12 == 0) {
            pal <- rep(ptol_pal()(12),cls / 12)
          } else {
            pal <- c(rep(ptol_pal()(12),floor(cls / 12)),ptol_pal()(12)[1:(cls %% 12)])
          }
          p <- p + scale_colour_manual(values = pal)
        }

        if (cls > 6) {
          sym <- 0:25
          if (cls / max(sym) == 1) {
            val <- sym
          }
          if (cls / max(sym) < 1) {
            val <- sym[1:cls]
          }
          if (cls / max(sym) > 1) {
            if (cls %% max(sym) == 0) {
              val <- rep(sym,cls / max(sym))
            } else {
              val <- c(rep(sym,floor(cls / max(sym))),sym[1:(cls %% max(sym))])
            }
          }
          p <- p + scale_shape_manual(values = val)
        }
        ggplotly(p)
      })

      output$LDA <- renderUI({
        if (!(is.null(getData()))) {
          plotlyOutput('lda')
        }
      })
      output$tw <- renderPlotly({
        d <- getLDA()
        s <- d$lda$Tw
        s <- data.frame(DF = 1:length(s), Tw = s)
        p <- ggplot(s,aes_string(x = 'DF',y = 'Tw')) +
          geom_bar(stat = 'identity',fill = ptol_pal()(1)) +
          theme_bw() +
          ggtitle('Eigenvalues')
        ggplotly(p)
      })

      output$Tw <- renderUI({
        if (!(is.null(getData()))) {
          plotlyOutput('tw')
        }
      })

      output$ldaLoadings <- renderPlotly({
        d <- getLDA()
        mz <- rownames(d$lda$loadings)
        res <- data.frame(X = d$lda$loadings[,input$ldaXaxis],Y = d$lda$loadings[,input$ldaYaxis],mz = mz)
        p <- ggplot(res,aes_string(x = 'X',y = 'Y',label = 'mz')) +
          geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
          geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
          geom_point(colour = ptol_pal()(1),alpha = 0.7) +
          theme_bw() +
          ggtitle('Loadings') +
          xlab(input$ldaXaxis) +
          ylab(input$ldaYaxis)
        ggplotly(p)
      })

      output$Loadings <- renderUI({
        if (!(is.null(getData()))) {
          plotlyOutput('ldaLoadings')
        }
      })

      output$export <- downloadHandler(
        filename = function() {
          'LDA.RData'
        },
        content = function(con){
          lda <- getLDA()
          save(lda,file = con)
        }
      )

      observe({
        if (input$close > 0) stopApp()
      })
    }
  )
}
