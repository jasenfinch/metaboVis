#' A shiny application for the visualisation of HRfiemsWorkflow analyses
#' @name visualiseAnalysis
#' @param analysis analysis list object.
#' @details A tool that allows the visualisation of analysis results. Includes tools for TIC visualisation, PCA, LDA, classification and feature selection results visualisation and tools for putative annotation.
#' Annotation tools use those in the mzAnnotation package (\url{https://github.com/jasenfinch/mzAnnotation}).
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny shinyApp navbarPage tabPanel sidebarPanel selectInput
#' @importFrom shiny mainPanel uiOutput fluidRow tags plotOutput fluidPage
#' @importFrom shiny column verbatimTextOutput tabsetPanel numericInput checkboxInput
#' @importFrom shiny renderUI renderText reactive renderPlot
#' @importFrom ggplot2 ggplot aes_string aes geom_point geom_text theme_bw geom_smooth
#' @importFrom ggplot2 xlab ylab geom_boxplot theme position_dodge element_text
#' @importFrom ggplot2 ggtitle geom_errorbarh facet_wrap geom_density xlim geom_hline
#' @importFrom ggplot2 geom_vline
#' @importFrom plyr ddply summarise ldply
#' @importFrom reshape2 melt
#' @importFrom FIEmspro nlda 
#' @importFrom stringr str_replace_all
#' @importFrom rcdk parse.smiles view.image.2d
#' @importFrom mzAnnotation isoDistr PIPsearch generateMF relationshipPredictor
#' @importFrom stats prcomp dist hclust sd
#' @importFrom graphics rasterImage plot par
#' @importFrom plotly plotlyOutput renderPlotly ggplotly

visualiseAnalysis <- function(analysis){
	shinyApp(
		ui = navbarPage("visualiseAnalysis",
										tabPanel('Info',
														 tabsetPanel(
														 	tabPanel("Log",
														 					 verbatimTextOutput('Log')
														 	),
														 	tabPanel("Info",
														 					 dataTableOutput('Info')
														 	)
														 )
										),
										tabPanel('TIC',
														 sidebarPanel(
														 	uiOutput('modes1'),
														 	uiOutput('batches'),
														 	uiOutput('classes1')
														 ),
														 mainPanel(
														 	fluidRow(
														 		plotlyOutput('InjTIC')
														 	),
														 	tags$hr(),
														 	fluidRow(
														 		plotlyOutput('ClassTIC')
														 	)
														 )
										),
										tabPanel('QC'),
										tabPanel("PCA",
														 sidebarPanel(
														 	uiOutput('modes2'),
														 	uiOutput('classes2'),
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
										tabPanel("LDA",
														 sidebarPanel(
														 	uiOutput('modes3'),
														 	uiOutput('classes3'),
														 	uiOutput('dfs1'),
														 	uiOutput('dfs2')
														 ),
														 mainPanel(
														 	fluidRow(
														 		plotlyOutput('lda')
														 	),
														 	tags$hr(),
														 	fluidRow(
														 		plotlyOutput('ldaLoadings')
														 	),
														 	tags$hr(),
														 	fluidRow(
														 		plotOutput('hca')
														 	)
														 )),
										tabPanel("Classification",
														 sidebarPanel(
														 	uiOutput('modes4'),
														 	uiOutput('method1')
														 ),
														 mainPanel(
														 	fluidRow(
														 		plotOutput('classiRes')
														 	)
														 )
										),
										tabPanel("Feature Selection",
														 sidebarPanel(
														 	uiOutput('modes5'),
														 	uiOutput('pairwise')
														 ),
														 mainPanel(
														 	fluidRow(
														 		plotOutput('featurePlot')
														 	),
														 	fluidRow(
														 		dataTableOutput('featureTable')
														 	)
														 )
										),
										tabPanel("Annotation",
														 fluidPage(
														 	fluidRow(
														 		column(3,uiOutput('modes6')),
														 		column(3,uiOutput('feature'))
														 	),
														 	fluidRow(
														 		tabsetPanel(
														 			tabPanel("Accurate m/z",
														 							 fluidRow(
														 							 	dataTableOutput('accurateMZ')
														 							 )
														 			),
														 			tabPanel("Visualisation",
														 							 mainPanel(
														 							 	verbatimTextOutput('explanatoryPairwise'),
														 							 	fluidRow(
														 							 		plotOutput('boxplot')
														 							 	),
														 							 	fluidRow(
														 							 		plotOutput('binplot')
														 							 	)
														 							 )			 
														 			),
														 			tabPanel("Correlations",
														 							 fluidRow(
														 							 	dataTableOutput('correlations')
														 							 ),
														 							 fluidRow(
														 							 	dataTableOutput('corAccuratemzTable')
														 							 ),
														 							 fluidRow(
														 							 	dataTableOutput('corAdductPredict')
														 							 ),
														 							 fluidRow(
														 							 	plotOutput('corAccuratemzPlot')
														 							 )
														 			),
														 			tabPanel("Molecular Formulas",
														 							 fluidRow(
														 							 	column(3, uiOutput('accurateMass1')),
														 							 	column(2, numericInput("accuracy","PPM:",5)),
														 							 	column(2, uiOutput('charge')),
														 							 	column(2, checkboxInput("applygr", "Apply 7GR",value = T))
														 							 	
														 							 ),
														 							 fluidRow(
														 							 	column(2, numericInput("Cmax","C:",10)),
														 							 	column(2, numericInput("Hmax","H:",20)),
														 							 	column(2, numericInput("Nmax","N:",5)),
														 							 	column(2, numericInput("Omax","O:",5)),
														 							 	column(2, numericInput("Pmax","P:",0)),
														 							 	column(2, numericInput("Smax","S:",0)),
														 							 	column(2, numericInput("Namax","Na:",0)),
														 							 	column(2, numericInput("Kmax","K:",0)),
														 							 	column(2, numericInput("Clmax","Cl:",0)),
														 							 	column(2, numericInput("iCmax","C13:",0)),
														 							 	column(2, numericInput("iOmax","O18:",0)),
														 							 	column(2, numericInput("iKmax","K41:",0)),
														 							 	column(2, numericInput("iClmax","Cl37:",0))
														 							 ),
														 							 fluidRow(
														 							 	dataTableOutput('molecularFormulas')
														 							 ),
														 							 fluidRow(
														 							 	plotOutput('isoDistsPlot')
														 							 ),
														 							 fluidRow(
														 							 	dataTableOutput('isoDistsTable')
														 							 )
														 			),
														 			tabPanel("PIPs",
														 							 fluidRow(
														 							 	column(3, uiOutput('accurateMass2')),
														 							 	column(2, numericInput("ppm","PPM:",5)),
														 							 	column(2, checkboxInput("pipIso", "Isotopes",value = T))
														 							 ),
														 							 fluidRow(
														 							 	dataTableOutput(outputId = "PIPtable")
														 							 ),
														 							 fluidRow(
														 							 	plotOutput('Structure')
														 							 )
														 			)
														 		)
														 	)
														 )
										)
		),
		server = function(input, output) {
			##################################################
			## Info
			output$Log <- renderText({
				logs <- list()
				binnedLogs <- analysis$binnedData$log
				logs$binnedData <- c(' Spectral Binning\n',
														 paste('Date:',binnedLogs$Date,'\n'),
														 paste('Scans:',paste(min(binnedLogs$Scans),'-',max(binnedLogs$Scans),sep = ''),'\n'),
														 paste('Modes:',paste(binnedLogs$Modes,collapse = ', '),'\n'),
														 paste("Scan Ranges:",paste(lapply(binnedLogs$ScanRanges,paste,collapse = "-"),collapse = ", "),"\n"),
														 paste('No. Samples:',binnedLogs$Nsamples,"\n")
				)
				if ('preTreatedData' %in% names(analysis)) {
					preTreatLogs <- analysis$preTreatedData$log
					logs$preTreatedData <- c('\n Data Pre-treatment\n',
																	 paste('Date:',preTreatLogs$Date,'\n'),
																	 paste('Classes:',preTreatLogs$Class,'\n'),
																	 paste('Occupancy:',preTreatLogs$Occupancy,'\n'),
																	 paste('TIC Normalisation:',preTreatLogs$normTIC,'\n'),
																	 paste('Log10 Transformation:',preTreatLogs$logTrans,'\n'),
																	 if (preTreatLogs$logTrans == T) {
																	 	paste('Log10 Addition:',preTreatLogs$add,'\n')
																	 },
																	 if (!is.null(preTreatLogs$removeSample)) {
																	 	paste('Removed Samples:',paste(preTreatLogs$removeSample,collapse = ' '),'\n')
																	 },
																	 if (!is.null(preTreatLogs$removeSample)) {
																	 	paste('Removed Classes:',paste(preTreatLogs$removeClass,collapse = ' '),'\n')
																	 },
																	 paste(names(preTreatLogs$Nvariables)[1],'Mode:',preTreatLogs$Nvariables[1],'variables\n'),
																	 paste(names(preTreatLogs$Nvariables)[2],'Mode:',preTreatLogs$Nvariables[2],'variables\n')
					)
				}
				if ('classification' %in% names(analysis)) {
					ClassificationLogs <- analysis$classification$log
					logs$Classification <- c('\n Classification\n',
																	 paste('Date:',ClassificationLogs$Date,'\n'),
																	 paste('Classes:',ClassificationLogs$Classes,'\n'),
																	 paste('Methods:',paste(ClassificationLogs$Methods,collapse = ' '),'\n'),
																	 paste('Sampling:',ClassificationLogs$Parameters$sampling,'\n'),
																	 paste('No. Iterations:', ClassificationLogs$Parameters$niter, '\n'),
																	 paste('No. Repetitions:', ClassificationLogs$Parameters$nreps,'\n'),
																	 paste('Stratification:', ClassificationLogs$Parameters$strat, '\n'),
																	 paste('Training Proportion:', ClassificationLogs$Parameters$div, '\n'),
																	 paste('No. Pairwise Comparisons:',ClassificationLogs$Npairwises,'\n')
					)
				}
				if ('featureSelection' %in% names(analysis)) {
					FeatureSelectionLogs <- analysis$featureSelection$log
					logs$FeatureSelection <- c('\n Feature Selection\n',
																		 paste('Date:',FeatureSelectionLogs$Date,'\n'),
																		 paste('Classes:',FeatureSelectionLogs$Classes,'\n'),
																		 paste('Methods:',paste(FeatureSelectionLogs$Methods,collapse = ' '),'\n'),
																		 paste('Sampling:',FeatureSelectionLogs$Parameters$sampling,'\n'),
																		 paste('No. Iterations:', FeatureSelectionLogs$Parameters$niter, '\n'),
																		 paste('No. Repetitions:', FeatureSelectionLogs$Parameters$nreps,'\n'),
																		 paste('Stratification:', FeatureSelectionLogs$Parameters$strat, '\n'),
																		 paste('Training Proportion:', FeatureSelectionLogs$Parameters$div, '\n'),
																		 paste('No. Pairwise Comparisons:',FeatureSelectionLogs$Npairwises,'\n')
					)
				}
				if ('masterMixes' %in% names(analysis)) {
					MasterMixLogs <- analysis$masterMixes$log
					logs$MaterMix <- c('\n Master Mixes\n',
														 paste('Date:',MasterMixLogs$Date,'\n'),
														 paste('Classes:',MasterMixLogs$Classes,'\n')
					)
				}
				if ('accurateMZ' %in% names(analysis)) {
					AccurateMZLogs <- analysis$accurateMZ$log
					logs$MaterMix <- c('\n Accurate m/z\n',
														 paste('Date:',AccurateMZLogs$Date,'\n')
					)
				}
				if ('explanatoryFeatures' %in% names(analysis)) {
					ExplanatoryFeaturesLogs <- analysis$explanatoryFeatures$log
					logs$ExplanatoryFeatures <- c('\n Explanatory Features\n',
																				paste('Date:',ExplanatoryFeaturesLogs$Date,'\n'),
																				paste('Method:',ExplanatoryFeaturesLogs$Method,'\n'),
																				paste('Threshold:',ExplanatoryFeaturesLogs$Threshold,'\n'),
																				if (!is.null(ExplanatoryFeaturesLogs$Pairwises)) {
																					paste('Selected Pairwises:',paste(ExplanatoryFeaturesLogs$Pairwises,collapse = ' '),'\n')
																				},
																				paste(names(ExplanatoryFeaturesLogs$Nvariables)[1],'Mode:',ExplanatoryFeaturesLogs$Nvariables[1],'variables\n'),
																				paste(names(ExplanatoryFeaturesLogs$Nvariables)[2],'Mode:',ExplanatoryFeaturesLogs$Nvariables[2],'variables\n')
					)
				}
				if ('correlations' %in% names(analysis)) {
					CorrelationsLogs <- analysis$correlations$log
					logs$Correlations <- c('\n Correlations\n',
																 paste('Date:',CorrelationsLogs$Date,'\n'),
																 paste('P Value Threshold:',CorrelationsLogs$corPvalue,'\n')
					)
				}
				logs <- sapply(logs,function(x){paste(paste(x,collapse = ''),'\n\n',sep = '')})
				logs
			})
			
			output$Info <- DT::renderDataTable({
				analysis$Info
			},server = T,rownames = FALSE)
			
			##################################################
			## TIC
			availModes1 <- reactive({
				names(analysis$binnedData$`2`)
			})
			
			output$modes1 <- renderUI({
				selectInput('Mode1','Mode',availModes1())
			})
			
			availBatches <- reactive({
				colnames(analysis$Info)[4:5]
			})
			
			output$batches <- renderUI({
				selectInput('selectedBatch','Batch',availBatches())
			})
			
			availClasses1 <- reactive({
				colnames(analysis$Info)[6:ncol(analysis$Info)]
			})
			
			output$classes1 <- renderUI({
				selectInput('Class1','Class',availClasses1())
			})
			
			calcTIC <- reactive({
				dat <- analysis$binnedData$`2`
				dat <- dat[[input$Mode1]]
				TIC <- rowSums(dat)
				TIC
			})
			
			output$InjTIC <- renderPlotly({
				TIC <- calcTIC()
				TIC <- data.frame(InjOrder = analysis$Info$injOrder,TIC = TIC,Batch = factor(analysis$Info[,input$selectedBatch]))
				p <- ggplot(TIC,aes_string(x = 'InjOrder',y = 'TIC')) +
					geom_smooth() +
					geom_point(aes_string(colour = 'Batch')) +
					theme_bw() +
					xlab('Injection Order')
				p <- ggplotly(p)
			})
			
			output$ClassTIC <- renderPlotly({
				TIC <- calcTIC()
				TIC <- data.frame(Class = factor(analysis$Info[,input$Class1]),TIC = TIC)
				p <- ggplot(TIC,aes_string(x = 'Class',y = 'TIC',group = 'Class')) +
					geom_boxplot(aes_string(colour = 'Class')) +
					geom_point(position = position_dodge(0.3)) +
					theme_bw() +
					theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) 
				p <- ggplotly(p)
			})
			
			#################################################
			## PCA
			
			availModes2 <- reactive({
				names(analysis$preTreatedData)[1:2]
			})
			
			output$modes2 <- renderUI({
				selectInput('Mode2','Mode',availModes2())
			})
			
			availClasses2 <- reactive({
				rev(colnames(analysis$preTreatedData$Info)[4:ncol(analysis$preTreatedData$Info)])
			})
			
			output$classes2 <- renderUI({
				selectInput('Class2','Class',availClasses2())
			})
			
			availPCS <- reactive({
				dat <- analysis$preTreatedData
				pca <- prcomp(dat[[input$Mode2]],scale = T)
				colnames(pca$x)
			})
			
			output$pcs1 <- renderUI({
				selectInput('pcaXaxis','X axis',choices = availPCS(),selected = 'PC1')
			})
			
			output$pcs2 <- renderUI({
				selectInput('pcaYaxis','Y axis',choices = availPCS(),selected = 'PC2')
			})
			
			output$pca <- renderPlotly({
				dat <- analysis$preTreatedData
				pca <- prcomp(dat[[input$Mode2]],scale = T)
				vars <- pca$sdev^2
				vars <- vars/sum(vars)      
				names(vars) <- colnames(pca$rotation)
				vars <- round(vars * 100,2)
				res <- data.frame(X = pca$x[,input$pcaXaxis],Y = pca$x[,input$pcaYaxis],Class = factor(dat$Info[,input$Class2]),Sample = dat$Info$fileOrder)
				p <- ggplot(res,aes_string(x = 'X',y = 'Y',colour = 'Class',label = 'Sample')) +
					geom_hline(yintercept = 0, linetype = 2, colour = 'grey') +
					geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
					geom_point() +
					xlab(paste(input$pcaXaxis,' (Var: ',vars[input$pcaXaxis],'%)',sep = '')) +
					ylab(paste(input$pcaYaxis,' (Var: ',vars[input$pcaYaxis],'%)',sep = '')) +
					theme_bw()
				p <- ggplotly(p)
			})
			
			output$pcaLoadings <- renderPlotly({
				dat <- analysis$preTreatedData
				pca <- prcomp(dat[[input$Mode2]],scale = T)
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
			
			################################################
			## LDA
			
			output$modes3 <- renderUI({
				selectInput('Mode3','Mode',availModes2())
			})
			
			
			output$classes3 <- renderUI({
				selectInput('Class3','Class',availClasses2())
			})
			
			availDFS <- reactive({
				dat <- analysis$preTreatedData
				cls <- factor(dat$Info[,input$Class3])
				lda <- nlda(dat[[input$Mode3]],cls,scale = T)
				colnames(lda$x)
			})
			
			Nclasses <- reactive({
				dat <- analysis$preTreatedData
				cls <- length(unique(dat$Info[,input$Class3])) 
				cls
			})
			
			output$dfs1 <- renderUI({
				if (Nclasses() > 2) {
					selectInput('ldaXaxis','X axis',choices = availDFS(),selected = 'DF1')
				} else {
					NULL
				}
			})
			
			output$dfs2 <- renderUI({
				if (Nclasses() > 2) {
					selectInput('ldaYaxis','Y axis',choices = availDFS(),selected = 'DF2')
				} else {
					NULL
				}
			})
			
			output$lda <- renderPlotly({
				dat <- analysis$preTreatedData
				cls <- factor(dat$Info[,input$Class3])
				Ncls <- length(unique(cls))
				lda <- nlda(dat[[input$Mode3]],cls,scale = T)
				if (Ncls < 3) {
					res <- data.frame(X = lda$x[,'DF1'],Class = factor(cls),Sample = dat$Info$fileOrder)
					p <- ggplot(res,aes_string(x = 'Class',y = 'X',colour = 'Class',label = 'Sample')) +
						geom_text() +
						theme_bw() +
						theme(legend.position = "none") +
						ylab(paste('DF1 (Tw: ',round(lda$Tw['DF1'],2),')',sep = ''))
					p <- ggplotly(p)
					
				} else {
					res <- data.frame(X = lda$x[,input$ldaXaxis],Y = lda$x[,input$ldaYaxis],Class = factor(cls),Sample = dat$Info$fileOrder)
					p <- ggplot(res,aes_string(x = 'X',y = 'Y',colour = 'Class',label = 'Sample')) +
						geom_text() +
						theme_bw() +
						xlab(paste(input$ldaXaxis,' (Tw: ',round(lda$Tw[input$ldaXaxis],2),')',sep = '')) +
						ylab(paste(input$ldaYaxis,' (Tw: ',round(lda$Tw[input$ldaYaxis],2),')',sep = ''))
					p <- ggplotly(p)
				}
			})
			
			output$ldaLoadings <- renderPlotly({
				dat <- analysis$preTreatedData
				cls <- factor(dat$Info[,input$Class3])
				Ncls <- length(unique(cls))
				lda <- nlda(dat[[input$Mode3]],cls,scale = T)
				if (Ncls < 3) {
					mz <- as.numeric(str_replace_all(rownames(lda$loadings),'[:alpha:]',''))
					res <- data.frame(X = lda$loadings[,'DF1'],mz = mz)
					p <- ggplot(res,aes_string(x = 'mz',y = 'X')) +
						geom_point(colour = "#3399FF") +
						theme_bw() +
						ggtitle('Loadings') +
						ylab('DF1') +
						xlab('m/z')
					p <- ggplotly(p)
				} else {
					res <- data.frame(X = lda$loadings[,input$ldaXaxis],Y = lda$loadings[,input$ldaYaxis],mz = rownames(lda$loadings))
					p <- ggplot(res,aes_string(x = 'X',y = 'Y',label = 'mz')) +
						geom_text(colour = "#3399FF",alpha = 0.7) +
						theme_bw() +
						ggtitle('Loadings') +
						xlab(input$ldaXaxis) +
						ylab(input$ldaYaxis)
					p <- ggplotly(p)
				}
			})
			
			output$hca <- renderPlot({
				dat <- analysis$preTreatedData
				cls <- factor(dat$Info[,input$Class3])
				Ncls <- length(unique(cls))
				if (Ncls > 2) {
					lda <- nlda(dat[[input$Mode3]],cls,scale = T)
					mdist <- dist(lda$xmeans)
					hc <- hclust(mdist,'complete')
					plot(hc,ylab = "Mahalanobis distance", xlab = '',main = '',sub = '')
				} else {
					NULL
				}
			})
			
			################################################
			## Classification
			
			availModes3 <- reactive({
				names(analysis$classification$Results)
			})
			
			output$modes4 <- renderUI({
				selectInput('Mode4','Mode',availModes3())
			})
			
			availMethods1 <- reactive({
				names(analysis$classification$Results[[1]][[1]])
			})
			
			output$method1 <- renderUI({
				selectInput('Method1','Method',availMethods1())
			})
			
			output$classiRes <- renderPlot({
				res <- analysis$classification$Results
				res <- res[[input$Mode4]]
				res <- lapply(res,function(y,method){
					return(list(Accuracy = y[[method]]$acc.iter,AUC = y[[method]]$auc.iter,Margin = y[[method]]$mar.iter))
				},method = input$Method1)
				res <- lapply(res,as.data.frame)
				
				res <- ldply(res,.id = 'Pairwise')
				res <- melt(res)
				res <- ddply(res,~Pairwise+variable,summarise,Mean = mean(value),SD = sd(value))
				ggplot(res,aes(x = Mean,y = Pairwise,xmin = Mean - SD,xmax = Mean + SD)) +
					geom_errorbarh(colour = '#3399FF',height = 0.3) +
					geom_point() +
					theme_bw() +
					theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
					facet_wrap(~variable,scales = 'free_x')
			})
			
			################################################
			## Feature Selection
			
			availModes4 <- reactive({
				names(analysis$featureSelection$Results)
			})
			
			output$modes5 <- renderUI({
				selectInput('Mode5','Mode',availModes4())
			})
			
			availPairwises <- reactive({
				colnames(analysis$featureSelection$Results[[1]])
			})
			
			output$pairwise <- renderUI({
				selectInput('Pairwise','Pairwise',availPairwises())
			})
			
			output$featurePlot <- renderPlot({
				dat <- analysis$featureSelection$Results[[input$Mode5]][,input$Pairwise]
				dat <- data.frame(Feature = rownames(dat),Score = dat)
				dat$Feature <- as.numeric(str_replace_all(dat$Feature,'[:alpha:]',''))
				ggplot(dat,aes_string(x = 'Feature',y = 'Score')) +
					geom_point(colour = '#3399FF',alpha = 0.7) +
					xlab('m/z') +
					theme_bw()
			})
			
			output$featureTable <- renderDataTable({
				dat <- analysis$featureSelection$Results[[input$Mode5]][[input$Pairwise]][[input$Method2]]$fs.stats
				dat <- data.frame(Feature = names(dat),Score = dat)
				dat <- dat[order(dat$Score),]
			})
			
			
			################################################
			## Annotation
			
			availModes5 <- reactive({
				names(analysis$explanatoryFeatures$explanatoryFeatures)
			})
			
			output$modes6 <- renderUI({
				selectInput('Mode6','Mode',availModes5())
			})
			
			availFeatures <- reactive({
				unique(analysis$explanatoryFeatures$explanatoryFeatures[[input$Mode6]]$Feature)
			})
			
			output$feature <- renderUI({
				selectInput('Feature','Feature',availFeatures())
			})
			
			availAccurateMZ <- reactive({
				dat <- analysis$explanatoryFeatures$accurateMZ[[input$Mode6]]
				dat <- dat[which(dat$Bin == input$Feature),]
				dat$mz <- as.numeric(str_replace_all(dat$mz,'[:alpha:]',''))
				dat <- dat[,-1]
				dat
			})
			
			output$accurateMZ <- renderDataTable({
				availAccurateMZ()
			})
			
			output$boxplot <- renderPlot({
				dat <- data.frame(Class = analysis$preTreatedData$Info[,analysis$featureSelection$log$Classes],Intensity = analysis$preTreatedData[[input$Mode6]][,input$Feature])
				ggplot(dat,aes_string(x = 'Class',y = 'Intensity',group = 'Class',fill = 'Class')) + 
					geom_boxplot() +
					theme_bw() +
					theme(axis.text.x = element_text(angle = 90, hjust = 1))
			})
			
			output$binplot <- renderPlot({
				dat <- data.frame(Class = analysis$masterMixes[[input$Mode6]]$Class,analysis$masterMixes[[input$Mode6]][which(round(as.numeric(str_replace_all(names(analysis$masterMixes[[input$Mode6]])[-1],'[:alpha:]','')),2) == as.numeric(str_replace_all(input$Feature,'[:alpha:]','')))])
				dat <- melt(dat)
				dat$value <- round(dat$value,0)
				dat$variable <- as.numeric(str_replace_all(dat$variable,'[:alpha:]',''))
				dat <- dat[-which(dat$value == 0),]
				dat <- apply(dat,1,function(x){
					y <- data.frame(Class = x[1],mz = as.numeric(x[2]),stringsAsFactors = F)
					return(y[rep(seq_len(nrow(y)),each = x[3]),])
				})
				dat <- ldply(dat)
				ggplot(dat,aes_string('mz')) +
					geom_density() +
					xlim(as.numeric(str_replace_all(input$Feature,'[:alpha:]','')) - 0.005,as.numeric(str_replace_all(input$Feature,'[:alpha:]','')) + 0.005) +
					theme_bw() +
					theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
					facet_wrap(~Class)
			})
			
			output$explanatoryPairwise <- renderText({
				dat <- analysis$explanatoryFeatures$explanatoryFeatures[[input$Mode6]]
				dat <- dat$Pairwise[which(dat$Feature == input$Feature)]
				dat <- paste('Explanatory Pairwises:',paste(dat,collapse = ' '))
				dat
			})
			
			output$correlations <- renderDataTable({
				dat <- analysis$correlations$Correlations[[input$Mode6]][[input$Feature]]
				if (nrow(dat) > 0) {
					colnames(dat)[1] <- 'Bin' 
					rownames(dat) <- NULL
					dat$r <- round(dat$r,3)
					dat
				} else {
					NULL
				}
			},server = T,selection = 'single',rownames = FALSE)
			
			availCorAccurateMZ <- reactive({
				r <- input$correlations_rows_selected
				mz <- as.character(analysis$correlations$Correlations[[input$Mode6]][[input$Feature]][[input$Feature]][r])
				mz <- as.numeric(str_replace_all(mz,'[:alpha:]',''))
				dat <- analysis$masterMixes[[input$Mode6]]
				Hres <- round(as.numeric(str_replace_all(names(dat),'[:alpha:]','')),2)
				dat <- data.frame(Class = dat$Class,dat[,which(Hres == mz)])
				dat <- apply(dat,1,function(z,mz){
					return(data.frame(Class = z['Class'],mz = mz[which(as.numeric(z[-1]) == max(as.numeric(z[-1])))[1]],Intensity = as.numeric(z[-1])[which(as.numeric(z[-1]) == max(as.numeric(z[-1])))[1]]))
				},mz = names(dat))
				dat <- ldply(dat)
				dat$mz <- as.numeric(str_replace_all(dat$mz,'[:alpha:]',''))
				dat
			})
			
			output$corAccuratemzTable <- renderDataTable({
				r <- input$correlations_rows_selected
				if (!is.null(r)) {
					dat <- availCorAccurateMZ()
					dat
				} else {
					NULL
				}
			},server = T,selection = list(mode='single',selection = 1,target = 'row'),rownames = FALSE)
			
			availmz <- reactive({
				dat <- analysis$explanatoryFeatures$accurateMZ[[input$Mode6]]
				dat <- dat[which(dat$Bin == input$Feature),]
				mz <- as.numeric(str_replace_all(dat$mz,'[:alpha:]',''))
				mz <- mz[order(dat$Intensity,decreasing = T)]
				mz <- unique(mz)
				mz
			})
			
			output$corAdductPredict <- renderDataTable({
				r <- input$corAccuratemzTable_rows_selected
				if (!is.null(r)) {
					dat <- availCorAccurateMZ()
					mz <- dat$mz[r]
					parentMZ <- as.numeric(availmz()[1])
					if (input$Mode6 == 'Positive') {
						mode <- 'p'
					}
					if (input$Mode6 == 'Negative') {
						mode <- 'n'
					}
					mz <- c(parentMZ,mz)
					dat <- relationshipPredictor(mz,mode)
					dat$Error <- round(dat$Error,5)
					dat
				} else {
					NULL
				}
			},server = T,selection = 'single',rownames = FALSE)
			
			output$corAccuratemzPlot <- renderPlot({
				r <- input$correlations_rows_selected
				if (!is.null(r)) {
					mz <- as.character(analysis$correlations$Correlations[[input$Mode6]][[input$Feature]][[input$Feature]][r])
					dat <- data.frame(Class = analysis$masterMixes[[input$Mode6]]$Class,analysis$masterMixes[[input$Mode6]][which(round(as.numeric(str_replace_all(names(analysis$masterMixes[[input$Mode6]])[-1],'[:alpha:]','')),2) == as.numeric(str_replace_all(mz,'[:alpha:]','')))])
					dat <- melt(dat)
					dat$value <- round(dat$value,0)
					dat$variable <- as.numeric(str_replace_all(dat$variable,'[:alpha:]',''))
					dat <- dat[-which(dat$value == 0),]
					dat <- apply(dat,1,function(x){
						y <- data.frame(Class = x[1],mz = as.numeric(x[2]),stringsAsFactors = F)
						return(y[rep(seq_len(nrow(y)),each = x[3]),])
					})
					dat <- ldply(dat)
					ggplot(dat,aes_string('mz')) +
						geom_density() +
						xlim(as.numeric(str_replace_all(mz,'[:alpha:]','')) - 0.005,as.numeric(str_replace_all(mz,'[:alpha:]','')) + 0.005) +
						theme_bw() +
						theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
						facet_wrap(~Class)
				} else {
					NULL
				}
			})
			
			output$accurateMass1 <- renderUI({
				selectInput('AccurateMass1','Accurate m/z',availmz(),selected = availmz()[1])
			})
			
			availCharge <- reactive({
				if (input$Mode6 == 'Negative') {
					charge = -1
				}
				if (input$Mode6 == 'Positive') {
					charge = 1
				}
				charge
			})
			
			output$charge <- renderUI({
				numericInput('Charge','Charge:',availCharge())
			})
			
			getMF <- reactive({
				mz <- as.numeric(input$AccurateMass1)
				maxi <- c(C = input$Cmax,iC = input$iCmax,H = input$Hmax,iH = 0,N = input$Nmax,iN = 0,O = input$Omax,iO = input$iOmax,F = 0 ,Na = input$Namax,Si = 0,P = input$Pmax,S = input$Smax,Cl = input$Clmax,iCl = input$iClmax,Br = 0,iBr = 0,K = input$Kmax,iK = input$iKmax)
				
				res <- generateMF(mz,ppm = input$accuracy,charge = input$Charge,applygr = input$applygr,composition = maxi)
				
				res
			})
			
			output$molecularFormulas <- renderDataTable({
				res <- getMF()
				res
			},server = T,selection = 'single') 
			
			output$isoDistsPlot <- renderPlot({
				r <- input$molecularFormulas_rows_selected
				if (!is.null(r)) {
					mf <- getMF()
					mf <- mf[r,]
					res <- isoDistr(as.character(mf$MF),chrg = input$Charge)
					plot(res[,1],res[,3],type = "h",xlab = "m/z",ylab = "Relative Intensity",col = "Blue")
				} else {
					NULL
				}
			})
			
			output$isoDistsTable <- renderDataTable({
				r <- input$molecularFormulas_rows_selected
				if (!is.null(r)) {
					mf <- getMF()
					mf <- mf[r,]
					res <- isoDistr(as.character(mf$MF),chrg = input$Charge)
					res <- res[,-2]
					colnames(res)[2] <- "Relative Intensity"
					res
				} else {
					NULL
				}
			})
			
			output$accurateMass2 <- renderUI({
				selectInput('AccurateMass2','Accurate m/z',availmz(),selected = availmz()[1])
			})
			
			getPIPs <- reactive({
				mz <- as.numeric(input$AccurateMass2)
				
				if (input$Mode6 == 'Positive') {
					mode <- "p"
				}
				if (input$Mode6 == 'Negative') {
					mode <- "n"
				}
				
				if (input$pipIso) {
					iso <- c('C13','O18','S34')
				} else {
					iso <- NULL
				}
				
				pip_tab <- PIPsearch(mz,mode,input$ppm,iso = iso)
				pip_tab$`Adduct m/z` <- round(as.numeric(as.character(pip_tab$`Adduct m/z`)),5)
				pip_tab$`PPM Error` <- round(as.numeric(as.character(pip_tab$`PPM Error`)),3)
				pip_tab
			})
			
			output$PIPtable <- renderDataTable({
				pip_tab <- getPIPs()
				pip_tab
			},server = T,selection = 'single')
			
			output$Structure <- renderPlot({
				r <- input$PIPtable_rows_selected
				if (!is.null(r)) {
					res <- getPIPs()
					res <- res[r,]
					smile <- gsub('"','',res$Smiles)
					par(mar = c(0,0,0,0))
					sm <- parse.smiles(smile)[[1]]
					temp1 <- view.image.2d(sm,500,500)
					plot(NA,NA,xlim = c(1,100),ylim = c(1,100),xaxt = 'n',yaxt = 'n',xlab = '',ylab = '')
					rasterImage(temp1,1,1,100,100)
				} else {
					NULL
				}
			},width = 400,height = 400)
			
		}
	)
}

