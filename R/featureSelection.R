
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom ggplot2 geom_boxplot guides
#' @importFrom stringr str_split str_replace_all
#' @importFrom dplyr bind_cols mutate arrange
#' @importFrom shiny stopApp

featureSelection <- function() {
  shinyApp(
    ui = (fluidPage(theme = shinytheme('flatly'),
                    headerPanel('Feature Selection'),
                    sidebarPanel(
                      uiOutput('object'),
                      uiOutput('method'),
                      uiOutput('pairwise'),
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
                      fluidRow(
                        dataTableOutput('table')
                      ),
                      fluidRow(
                        uiOutput('BoxPlot')
                      )
                    )
    )
    ),
    server = function(input,output) {
      getData <- reactive({
        if (is.null(input$Object) | input$Object == '') {

        } else {
          analysis <- get(input$Object)
          if (class(analysis) == 'Workflow') {
            analysis <- analysis@analysed
          }
          feat <- analysis@featureSelection
          return(feat)
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

      availPairwise <- reactive({
        res <- getData()
        unique(res$Pairwise)
      })

      output$pairwise <- renderUI({
        selectInput('Pairwise','Pairwise',availPairwise())
      })

      output$plot <- renderPlotly({
        p <- getData() %>%
          filter(Method == input$Method,Pairwise == input$Pairwise) %>%
          mutate(Mode = substr(Feature,1,1), `m/z` = as.numeric(str_replace_all(sapply(str_split(Feature,' '),function(x){x[1]}),'[:alpha:]','')), Score = -log10(Score)) %>%
          ggplot(aes_string(x = '`m/z`', y = 'Score', colour = 'Mode')) +
          geom_point() +
          scale_colour_ptol() +
          theme_bw() +
          facet_wrap(~Mode) +
          guides(colour = F) +
          ylab('-log10(Score)')

        ggplotly(p)
      })

      output$Plot <- renderUI(
        plotlyOutput('plot')
      )

      output$table <- renderDataTable({
        getData() %>%
          filter(Method == input$Method,Pairwise == input$Pairwise) %>%
          select(Feature,Score) %>%
          arrange(Score)
      },selection = 'single')

      output$boxplot <- renderPlot({
        r <- input$table_rows_selected
        if (!is.null(r)) {
          feat <- getData() %>%
            filter(Method == input$Method,Pairwise == input$Pairwise) %>%
            select(Feature,Score) %>%
            arrange(Score)
          feat <- feat[r,]
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
          dat <- bind_cols(Class = unlist(info[,analysis@parameters@featureSelection$cls]),dat) %>%
            gather('Feature','Intensity',-Class) %>%
            filter(Class %in% str_split(input$Pairwise,'~')[[1]], Feature == feat$Feature[1])
          p <- ggplot(dat,aes_string(x = 'Class', y = 'Intensity')) +
            geom_boxplot(outlier.shape = NA) +
            geom_point(aes_string(colour = 'Class'),position = 'jitter') +
            scale_colour_ptol() +
            theme_bw() +
            guides(colour = F) +
            xlab('')
          p
        }
      })

      output$BoxPlot <- renderUI(
        plotOutput('boxplot')
      )

      output$export <- downloadHandler(
        filename = function() {
          'featureSelection.RData'
        },
        content = function(con){
          feat <- getData()
          save(feat,file = con)
        }
      )

      observe({
        if (input$close > 0) stopApp()
      })

    }
  )
}
