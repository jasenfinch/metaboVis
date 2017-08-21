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
        mainPanel()
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
        ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv),function(x){class(get(x,envir = .GlobalEnv))[1]}) %in% c('Workflow','Binalysis')]
      })

      output$object <- renderUI({
        selectInput('Object','Object',availObjects())
      })

      observe({
        if (input$close > 0) stopApp()
      })

    }
  )
}
