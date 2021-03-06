#' @importFrom tidyr gather spread
#' @importFrom dplyr bind_rows group_by summarise filter select
#' @importFrom tibble tibble
#' @importFrom shiny shinyApp fluidPage headerPanel sidebarPanel
#' @importFrom shiny  uiOutput tags downloadButton mainPanel fluidRow
#' @importFrom shiny reactive selectInput renderUI downloadHandler
#' @importFrom shiny plotOutput renderPlot observe
#' @importFrom shinythemes shinytheme
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom ggplot2 ggplot aes_string geom_errorbarh geom_point
#' @importFrom ggplot2 theme_bw facet_wrap theme element_text
#' @importFrom magrittr %>%
#' @importFrom ggdendro ggdendrogram
#' @importFrom stats hclust as.dist sd

classification <- function(){
  shinyApp(
    ui = (fluidPage(theme = shinytheme('flatly'),
                    headerPanel('Classification'),
                    sidebarPanel(
                      uiOutput('object'),
                      uiOutput('method'),
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
          classi <- analysis@classification %>%
            group_by(Pairwise,Method,Measure) %>%
            summarise(Mean = mean(Value), SD = sd(Value))

          return(classi)
        }
      })

      availObjects <- reactive({
        ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv),function(x){class(get(x,envir = .GlobalEnv))[1]}) %in% c('Workflow','Analysis')]
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

      observe({
        if (input$close > 0) stopApp()
      })

    }
  )
}




