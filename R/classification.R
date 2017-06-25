#' @importFrom tidyr gather spread
#' @importFrom dplyr bind_rows group_by summarise filter select
#' @importFrom tibble tibble
#' @importFrom shiny shinyApp fluidPage headerPanel sidebarPanel
#' @importFrom shiny  uiOutput tags downloadButton mainPanel fluidRow
#' @importFrom shiny reactive selectInput renderUI downloadHandler
#' @importFrom shiny plotOutput renderPlot
#' @importFrom shinythemes shinytheme
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom ggplot2 ggplot aes_string geom_errorbarh geom_point
#' @importFrom ggplot2 theme_bw facet_wrap theme element_text
#' @importFrom magrittr %>%
#' @importFrom ggdendro ggdendrogram

classification <- function(){
  shinyApp(
    ui = (fluidPage(theme = shinytheme('flatly'),
                    headerPanel('Classification'),
                    sidebarPanel(
                      uiOutput('object'),
                      uiOutput('method'),
                      tags$hr(),
                      downloadButton('export',label = 'Export Data')
                    ),
                    mainPanel(
                      fluidRow(
                        uiOutput('Plot')
                      ),
                      tags$hr(),
                      fluidRow(
                        plotOutput('dendrogram')
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
          classi <- analysis@classification

          classi <- lapply(classi,function(y){
            y <- lapply(y,function(z){
              z <- bind_rows(list(Accuracy = z$acc.iter,AUC = z$auc.iter,Margin = z$mar.iter), .id = 'Iteration')
              z$Iteration <- 1:nrow(z)
              z <- gather(z,'Measure','Value',-Iteration)
              return(z)
            })
            y <- bind_rows(y,.id = 'Method')
            return(y)
          })

          classi <- bind_rows(classi,.id = 'Pairwise') %>%
            group_by(Pairwise,Method,Measure) %>%
            summarise(Mean = mean(Value), SD = sd(Value))

          return(classi)
        }
      })

      availObjects <- reactive({
        ls()[sapply(ls(),function(x){class(get(x,envir = .GlobalEnv))[1]}) %in% c('Workflow','Analysis')]
      })

      output$object <- renderUI({
        selectInput('Object','Object',availObjects())
      })

      availMethods <- reactive({
        res <- getData()
        unique(res$Method)
      })

      output$method <- renderUI({
        selectInput('Method','Method',availMethods())
      })

      output$plot <- renderPlotly({
        res <- getData()
        print(head(res))
        res <- res[res$Method == input$Method,]
        p <- ggplot(res,aes_string(x = 'Mean',y = 'Pairwise',xmin = 'Mean - SD',xmax = 'Mean + SD')) +
          geom_errorbarh(colour = '#3399FF',height = 0.3) +
          geom_point() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          facet_wrap(~Measure,scales = 'free_x') +
          xlab('') +
          ylab('')

        ggplotly(p)
      })

      output$Plot <- renderUI(
        plotlyOutput('plot')
      )

      output$dendrogram <- renderPlot({
        classi <- getData()
        mar <- classi %>% filter(Measure == 'Margin') %>% select(Pairwise,Mean)
        pairs <- sapply(mar$Pairwise,strsplit,split = '~')
        mar <- bind_rows(tibble(P1 = sapply(pairs,function(x){x[1]}),P2 = sapply(pairs,function(x){x[2]}),Mean = mar$Mean),
                         tibble(P1 = sapply(pairs,function(x){x[2]}),P2 = sapply(pairs,function(x){x[1]}),Mean = mar$Mean))
        mar <- spread(mar,P1,Mean)
        rownames(mar) <- mar$P2
        mar$P2 <- NULL
        h <- hclust(as.dist(mar),method = 'ward.D2')
        ggdendrogram(h,rotate = T) + ggtitle('Margin Dendrogram')
      })

      output$export <- downloadHandler(
        filename = function() {
          'classification.RData'
        },
        content = function(con){
          classi <- getData()
          save(classi,file = con)
        }
      )
    }
  )
}




