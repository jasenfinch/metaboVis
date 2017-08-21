#' @importFrom reshape2 melt

 QC <- function(){
  shinyApp(
    ui = (
      fluidPage(
        theme = shinytheme('flatly'),
        headerPanel('QC'),
        sidebarPanel(
          uiOutput('object'),
          tags$hr(),
          tags$button(
            id = 'close',
            type = "button",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);",
            "Exit"
          )
        ),
        mainPanel(
          fluidRow(
            uiOutput('orders'),
            uiOutput('classes'),
            uiOutput('TIC')
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
            analysis <- analysis@processed
          }
            dat <- analysis@binnedData
            info <- analysis@info
          return(list(dat = dat,info = info))
        }
      })

      availObjects <- reactive({
        ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv),function(x){class(get(x,envir = .GlobalEnv))[1]}) %in% c('Workflow','Binalysis')]
      })

      output$object <- renderUI({
        selectInput('Object','Object',availObjects())
      })

      availOrders <- reactive({
        d <- getData()
        cols <- map(d$info,class)
        colnames(d$info)[cols == 'integer']
      })

      output$orders <- renderUI({
        selectInput('Order','Order',availOrders())
      })

      availClasses <- reactive({
        d <- getData()
        rev(colnames(d$info))
      })

      output$classes <- renderUI({
        selectInput('Class','Class',availClasses())
      })

      output$TIC <- renderUI({
        if (!(is.null(getData()))) {
          plotlyOutput('tic')
        }
      })

      output$tic <- renderPlotly({
        d <- getData()
        info <- d$info
        dat <- d$dat %>%
          map(rowSums) %>%
          bind_cols() %>%
          bind_cols(info[,input$Order],info[,input$Class]) %>%
          melt(id.vars = c(input$Order,input$Class),measure.vars = c('n','p'),variable.name = 'Mode',value.name = 'TIC')

        p <- ggplot(dat,aes_string(x = input$Order, y = 'TIC', colour = input$Class)) +
          geom_point() +
          facet_wrap(~Mode) +
          theme_bw()

        ggplotly(p)
      })



      observe({
        if (input$close > 0) stopApp()
      })

    }
  )
}
