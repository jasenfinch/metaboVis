#' PCA
#' @importFrom shiny shinyApp fluidPage sidebarPanel mainPanel
#' @importFrom shiny uiOutput tags fluidRow reactive renderUI
#' @importFrom shiny selectInput headerPanel downloadButton
#' @importFrom shinythemes shinytheme
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline geom_point
#' @importFrom ggplot2 xlab ylab theme_bw ggtitle labs scale_shape_manual
#' @importFrom ggplot2 scale_colour_manual geom_bar
#' @importFrom ggthemes scale_colour_ptol ptol_pal
#' @importFrom stats prcomp

PCA <- function(){
  # options(shiny.sanitize.errors = TRUE)
  shinyApp(
    ui = (fluidPage(theme = shinytheme('flatly'),
      headerPanel('PCA'),
      sidebarPanel(
        uiOutput('object'),
        uiOutput('classes'),
        tags$hr(),
        uiOutput('pcs1'),
        uiOutput('pcs2'),
        tags$hr(),
        downloadButton('export',label = 'Export Data')
      ),
      mainPanel(
        fluidRow(
          uiOutput('PCA')
        ),
        tags$hr(),
        fluidRow(
          uiOutput('Vars')
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
          pca <- prcomp(dat,center = T,scale. = T)
          return(list(dat = dat,info = info,pca = pca))
        }
      })

      availObjects <- reactive({
        ls()[sapply(ls(),function(x){class(get(x,envir = .GlobalEnv))[1]}) %in% c('Workflow','Analysis')]
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
        res <- data.frame(X = d$pca$x[,input$pcaXaxis],Y = d$pca$x[,input$pcaYaxis],Class = factor(unlist(d$info[,input$Class])),d$info)
        p <- ggplot(res) +
          geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
          geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
          geom_point(aes_string(x = 'X',y = 'Y',colour = 'Class',shape = 'Class')) +
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
        if (!(is.null(getData()))) {
          plotlyOutput('pca')
        }
      })
      output$vars <- renderPlotly({
        d <- getData()
        s <- d$pca$sdev^2
        s <- s/sum(s)*100
        s <- data.frame(PC = 1:length(s), Variance = s)
        p <- ggplot(s,aes_string(x = 'PC',y = 'Variance')) +
          geom_bar(stat = 'identity',fill = ptol_pal()(1)) +
          theme_bw() +
          ylab('%') +
          ggtitle('Variance Explained')
        ggplotly(p)
      })

      output$Vars <- renderUI({
        if (!(is.null(getData()))) {
          plotlyOutput('vars')
        }
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
        if (!(is.null(getData()))) {
          plotlyOutput('pcaLoadings')
        }
      })

      output$export <- downloadHandler(
        filename = function() {
          'PCA.RData'
        },
        content = function(con){
          pca <- getData()
          save(pca,file = con)
        }
      )
    }
  )
}
