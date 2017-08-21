#' @importFrom reshape2 melt
#' @importFrom shiny column
#' @importFrom metabolyseR analysisParameters metabolyse preTreatedData
#' @importFrom ggplot2 geom_histogram

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
            column(3,uiOutput('orders')),
            column(3,uiOutput('colour'))
          ),
          uiOutput('TIC'),
          tags$hr(),
          fluidRow(
            uiOutput('classes')
          ),
          uiOutput('classTIC'),
          tags$hr(),
          fluidRow(
            column(3,uiOutput('column')),
            column(3,textInput('QCindex','QC index','QC'))
          ),
          uiOutput('RSDhist')
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

      availColours <- reactive({
        d <- getData()
        rev(colnames(d$info))
      })

      output$colour <- renderUI({
        selectInput('Colour','Colour',availColours())
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
          bind_cols(info[,input$Order],info[,input$Colour]) %>%
          melt(id.vars = c(input$Order,input$Colour),measure.vars = c('n','p'),variable.name = 'Mode',value.name = 'TIC')
        dat[,input$Colour] <- factor(dat[,input$Colour])
        p <- ggplot(dat,aes_string(x = input$Order, y = 'TIC', colour = input$Colour)) +
          geom_point() +
          facet_wrap(~Mode) +
          theme_bw()

        cls <- length(unique(dat[,input$Colour]))

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
      })

      availClasses <- reactive({
        d <- getData()
        rev(colnames(d$info))
      })

      output$classes <- renderUI({
        selectInput('Class','Class',availClasses())
      })

      output$classTIC <- renderUI({
        if (!(is.null(getData()))) {
          plotlyOutput('classTic')
        }
      })

      output$classTic <- renderPlotly({
        d <- getData()
        info <- d$info
        dat <- d$dat %>%
          map(rowSums) %>%
          bind_cols() %>%
          bind_cols(info[,input$Class]) %>%
          melt(id.vars = c(input$Class),measure.vars = c('n','p'),variable.name = 'Mode',value.name = 'TIC')
        dat[,input$Class] <- factor(dat[,input$Class],levels = sort(unique(dat[,input$Class])))

        p <- ggplot(dat,aes_string(x = input$Class, y = 'TIC')) +
          geom_point(aes_string(colour = input$Class),position = 'jitter') +
          geom_boxplot() +
          facet_wrap(~Mode) +
          theme_bw()

        cls <- length(unique(dat[,input$Class]))

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
      })

      availColumns <- reactive({
        d <- getData()
        cls <- apply(d$info,2,function(x){length(unique(x))})
        minCls <- apply(d$info,2,function(x){min(table(x))})
        rev(colnames(d$info)[cls > 1 & minCls > 1])
      })

      output$column <- renderUI({
        selectInput('Column','Column',availColumns())
      })

      output$RSDhist <- renderUI({
        if (!(is.null(getData()))) {
          plotlyOutput('rsdHist')
        }
      })

      output$rsdHist <- renderPlotly({
        d <- getData()
        info <- d$info

        cls <- unlist(unique(info[,input$Column]))[!(unlist(unique(info[,input$Column])) %in% input$QCindex)] %>%
          as.character()

        parameters <- analysisParameters('preTreat')
        parameters@preTreat <- list(
          remove = list(class = list(classes = cls)),
          occupancyFilter = list(maximum = list(cls = input$Column,occupancy = 2/3)),
          impute = list(all = list(occupancy = 2/3)),
          transform = list(TICnorm = list())
          )

        dat <- d$dat %>%
          map(~{
            an <- metabolyse(.,info = info,parameters = parameters) %>%
              preTreatedData()
            an <- an$Data %>%
              gather('Feature','Intensity') %>%
              group_by(Feature) %>%
              summarise(RSD = sd(Intensity)/mean(Intensity))
            return(an)
            }) %>%
           bind_rows(.id = 'Mode')

        medians <- dat %>%
          group_by(Mode) %>%
          summarise(Median = median(RSD))

        p <- ggplot() +
          geom_histogram(data = dat,aes_string(x = 'RSD'),fill = ptol_pal()(5)[2],colour = 'black') +
          geom_vline(data = medians,aes_string(xintercept = 'Median'),linetype = 2,colour = 'red',size = 1) +
          theme_bw() +
          facet_wrap(~Mode)

      })

      observe({
        if (input$close > 0) stopApp()
      })

    }
  )
}
