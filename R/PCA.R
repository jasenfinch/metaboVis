#' visPCA
#' @importFrom shiny shinyApp pageWithSidebar sidebarPanel mainPanel
#' @importFrom shiny uiOutput tags fluidRow reactive renderUI selectInput
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_point
#' @importFrom ggplot2 xlab ylab theme_bw ggtitle
#' @export

visPCA <- function(analysis){
  if (class(analysis) != 'Analysis') {
    stop('Requires Analysis object')
  }
  if (length(analysis@preTreated) > 0) {
    dat <- analysis@preTreated$Data
    info <- analysis@preTreated$Info
  } else {
    dat <- analysis@rawData$Data
    info <- analysis@rawData$Info
  }
  shinyApp(
    ui = pageWithSidebar(
      'PCA',
      sidebarPanel(
        uiOutput('classes'),
        tags$hr(),
        uiOutput('pcs1'),
        uiOutput('pcs2'),
        tags$hr()
      ),
      mainPanel(
        fluidRow(
          plotlyOutput('pca')
        ),
        tags$hr(),
        fluidRow(
          plotlyOutput('pcaLoadings')
        )
      )
    ),
    server = function(input, output) {

      availClasses <- reactive({
        rev(colnames(info))
      })

      output$classes <- renderUI({
        selectInput('Class','Class',availClasses())
      })

      availPCS <- reactive({
        pca <- prcomp(dat,center = T,scale. = T)
        colnames(pca$x)
      })

      output$pcs1 <- renderUI({
        selectInput('pcaXaxis','X axis',choices = availPCS(),selected = 'PC1')
      })

      output$pcs2 <- renderUI({
        selectInput('pcaYaxis','Y axis',choices = availPCS(),selected = 'PC2')
      })

      output$pca <- renderPlotly({
        pca <- prcomp(dat,center = T,scale. = T)
        vars <- pca$sdev^2
        vars <- vars/sum(vars)
        names(vars) <- colnames(pca$rotation)
        vars <- round(vars * 100,2)
        res <- data.frame(X = pca$x[,input$pcaXaxis],Y = pca$x[,input$pcaYaxis],Class = factor(unlist(info[,input$Class])))
        print(head(res))
        p <- ggplot(res,aes(x = X,y = Y,colour = Class)) +
          geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
          geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
          geom_point() +
          xlab(paste(input$pcaXaxis,' (Var: ',vars[input$pcaXaxis],'%)',sep = '')) +
          ylab(paste(input$pcaYaxis,' (Var: ',vars[input$pcaYaxis],'%)',sep = '')) +
          theme_bw()
        p <- ggplotly(p)
      })

      output$pcaLoadings <- renderPlotly({
        pca <- prcomp(dat,center = T,scale. = T)
        mz <- rownames(pca$rotation)
        res <- data.frame(X = pca$rotation[,input$pcaXaxis],Y = pca$rotation[,input$pcaYaxis],mz = mz)
        p <- ggplot(res,aes_string(x = 'X',y = 'Y',label = 'mz')) +
          geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
          geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
          geom_point(colour = "#3399FF",alpha = 0.7) +
          theme_bw() +
          ggtitle('Loadings') +
          xlab(input$pcaXaxis) +
          ylab(input$pcaYaxis)
        p <- ggplotly(p)
      })
    }
  )
}
