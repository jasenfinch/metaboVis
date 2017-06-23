#' PCA
#' @importFrom shiny shinyApp fluidPage sidebarPanel mainPanel
#' @importFrom shiny uiOutput tags fluidRow reactive renderUI
#' @importFrom shiny selectInput headerPanel
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline geom_point
#' @importFrom ggplot2 xlab ylab theme_bw ggtitle labs scale_shape_manual
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggthemes scale_colour_ptol ptol_pal

PCA <- function(){
  # options(shiny.sanitize.errors = TRUE)
  shinyApp(
    ui = (fluidPage(
      headerPanel('PCA'),
      sidebarPanel(
        uiOutput('object'),
        uiOutput('classes'),
        tags$hr(),
        uiOutput('pcs1'),
        uiOutput('pcs2'),
        tags$hr()
      ),
      mainPanel(
        fluidRow(
          uiOutput('PCA')
        ),
        tags$hr(),
        fluidRow(
          uiOutput('Loadings')
        )
      )
    )
    ),
    server = function(input, output) {
      getData <- reactive({
        analysis <- get(input$Object)
        if (length(analysis@preTreated) > 0) {
          dat <- analysis@preTreated$Data
          info <- analysis@preTreated$Info
        } else {
          dat <- analysis@rawData$Data
          info <- analysis@rawData$Info
        }
        pca <- prcomp(dat,center = T,scale. = T)
        return(list(dat = dat,info = info,pca = pca))
      })

      availObjects <- reactive({
        Filter( function(x) 'Analysis' %in% class( get(x,envir = .GlobalEnv) ), ls(envir = .GlobalEnv) )
      })

      output$object <- renderUI({
        selectInput('Object','Object',availObjects())
      })

      availClasses <- reactive({
        d <- getData()
        rev(colnames(d$info))
      })

      output$classes <- renderUI({
        selectInput('Class','Class',availClasses())
      })

      availPCS <- reactive({
        d <- getData()
        colnames(d$pca$x)
      })

      output$pcs1 <- renderUI({
        selectInput('pcaXaxis','X axis',choices = availPCS(),selected = 'PC1')
      })

      output$pcs2 <- renderUI({
        selectInput('pcaYaxis','Y axis',choices = availPCS(),selected = 'PC2')
      })

      output$pca <- renderPlotly({
        d <- getData()
        vars <- d$pca$sdev^2
        vars <- vars/sum(vars)
        names(vars) <- colnames(d$pca$rotation)
        vars <- round(vars * 100,2)
        res <- data.frame(X = d$pca$x[,input$pcaXaxis],Y = d$pca$x[,input$pcaYaxis],Class = factor(unlist(d$info[,input$Class])))
        p <- ggplot(res,aes_string(x = 'X',y = 'Y',colour = 'Class',shape = 'Class')) +
          geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
          geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
          geom_point() +
          xlab(paste(input$pcaXaxis,' (Var: ',vars[input$pcaXaxis],'%)',sep = '')) +
          ylab(paste(input$pcaYaxis,' (Var: ',vars[input$pcaYaxis],'%)',sep = '')) +
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

      output$PCA <- renderUI({
        plotlyOutput('pca')
      })

      output$pcaLoadings <- renderPlotly({
        d <- getData()
        mz <- rownames(d$pca$rotation)
        res <- data.frame(X = d$pca$rotation[,input$pcaXaxis],Y = d$pca$rotation[,input$pcaYaxis],mz = mz)
        p <- ggplot(res,aes_string(x = 'X',y = 'Y',label = 'mz')) +
          geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
          geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
          geom_point(colour = ptol_pal()(1),alpha = 0.7) +
          theme_bw() +
          ggtitle('Loadings') +
          xlab(input$pcaXaxis) +
          ylab(input$pcaYaxis)
        ggplotly(p)
      })

      output$Loadings <- renderUI({
        plotlyOutput('pcaLoadings')
      })
    }
  )
}
