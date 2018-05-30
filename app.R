#Author: Leo Manhanga
#Project: Automated Model Selection Proof of Concept
#Version: 1.0

# Load data from a CSV file in the local directory
#setwd("C:/Users/lt07/Documents/R/ins")

# list of packages used by project
packages <- c("ggplot2", "caret", "mlbench", "caretEnsemble", "shiny", "dplyr","reshape","plyr",
               "datasets", "corrplot", "knitr", "psych", "lattice","randomForest",
              "RWeka", "glmulti", "DAAG","bootstrap","MASS","leaps","car","relaimpo","sjPlot")

for (p in packages) {
  if(p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, repos='http://cran.rstudio.com/')
  }
}

#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape)
library(datasets)
library(knitr)
library(corrplot)
library(GGally)
library(psych)
library("plyr")
library(DAAG)
#library("PerformanceAnalytics")
ins <- read.csv("C:/Users/lt07/Documents/R/ins/insurance.csv", stringsAsFactors = FALSE)
#MBI_db <- dbPool(
#  RMySQL::MySQL(),
#  dbname = "shinydemo",
#  host = "shiny-demo.com",
#  username = "guest",
#  password = "guest"
#)

# get the first 5 rows:
#MBI_db %>% tbl("MBI_tbl") %>% head(5)

ui <- fluidPage(

# Application title
  titlePanel("Automated Regression Model Selection"),

# Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
	wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 800px",
      selectInput('dataset', 'Choose the dataset:',
                  choices = c('ins', 'RegDependent','mtcars')),
      #numericInput("obs", "Number of observations to view:", 10),
      # Copy the line below to make a slider range
	  sliderInput("age", "Age Range", min = 0,
                  max = 120, value = as.factor(c(18, 90))),
      numericInput('nfree', 'Variance Inflation Factor Threshold',5),
	  helpText("All the predictor variables with a variance Inflation Factor greater than the selected threshold are dropped from the model"),
      checkboxInput('summary','View summary of the dataset'),
	  helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
	  checkboxInput('correlation','View a correlation matrix'),
      helpText("Note: The correlation Matrix is based on the variables selected in the regression model below"),
      uiOutput('response'),
      checkboxInput("pred_type", 'Wishes to pick the predictors manually?'),
      conditionalPanel(
        condition = "input.pred_type == true",
        uiOutput('predictors')
      ),
      conditionalPanel(
        condition = "input.pred_type == false",
        helpText(" If this is not selected then the regression is directly ",
                 "applied on the top predictor")
      ),

      HTML('<hr style="color: purple;" size="20" >')
      , hr(),
      h3('Steps for running Regression'),
      p('This is a shiny app for Automated Regression Model Selection.'
      ),
      h4('Usage:'),
      p('1. Select the dataset of your wish. Default:ins'),
      p('2. Select the Range of ages to be viewed from the selected dataset'),
      p('3. Check the box if you wish to see the summary of the dataset'),
      p('4. Choose the response column (y value). Default: first column of the dataset'),
      p('5. If you wish to select the variable predictors variable of your own, select the predictor checkbox. By default the best predictor column of the datset with respect to the respose variable will be taken'),
      p('6. Check the linear model fit for the selected predictors and check the graph of the same'),

      hr(),
      hr(),
      h4('Author: Leonard Manhanga')

    )),

    # Show a plot of the generated analysis
    mainPanel(
      tabsetPanel(
      #######################################
      # Data 
      tabPanel(p(icon("table"), "Dataset"),
               dataTableOutput(outputId="dtable"),
               conditionalPanel(
                 condition = 'input.summary==true',
                 h4("Summary"),
                 verbatimTextOutput("summary")
               )
               ), # end of "Dataset" tab panel
      ######################################
      tabPanel(p(icon("line-chart"), "Visualize Regression Results"),
      #h4("Observations"),
      #tableOutput("view"),
   	  h4("Variance Inflation Factors"),
	    verbatimTextOutput("vf"),
	  #plotOutput('corPlot'),
      h4("Regression model fit"),
      verbatimTextOutput('model'),
      plotOutput('plt')
	      ),
	  ##########################################################
	  tabPanel(p(icon("correlation"), "Correlation Matrix"),
	           conditionalPanel(
                 condition = 'input.correlation==true',
               h4("Correlation Matrix"),
	          plotOutput('correlation')
	  )
      )
	 )
  )
)
)

# Load data processing file
#source("Global.R")
#age <- sort(unique(data$age))
server <- (function(input, output) {
  # Insert Data Location Here
# dataset = read.csv('C:/Users/182602/Documents/Insights/Marketing/BInsights/ins.csv')
  # Return the requesteed dataset
  datasetInputt <- reactive({
    switch (input$dataset,
            'ins' = ins,
            'RegDependent' = RegDependent,
            'mtcars' = mtcars
    )
  })
datasetInput <- reactive({
    filtered <- datasetInputt() %>%
      filter(age >= input$age[1],
             age <= input$age[2]
      )
  })
  
#Generate a VIF threshold
 values <- reactiveValues(n = sample(1:10,1))
  observeEvent(input$nfree, {
   values$n <- input$nfree
 })
 #################################################################################
 # Initialize reactive values
  xcol <- reactive({
    df <- datasetInput()
    y <- input$resp
    df <- df[!is.na(df[,y]),]
    y <- df[,input$resp]

    if(input$pred_type == F){
      x <- df[,!(names(df) %in% input$resp)]
      p <- ncol(x)
      pvalues <- numeric(p)
      for(i in seq_len(p)) {
        fit <- lm(y ~ x[, i])
        summ <- summary(fit)
        pvalues[i] <- summ$coefficients[2, 4]
      }
      ord <- order(pvalues)
      ord <- head(ord,1)
      xf <- as.data.frame(x[,ord])
      names(xf) <- names(x[ord])
      ages <- age %in% datasetInput()
    } else {
      xcol <- input$predx
      xf <- as.data.frame(df[,xcol])
      names(xf) <- xcol
    }
    xf
  })

  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)

  })

  # Generate a correlation Matrix from the dataset
  ##################################################
  output$rest_var_select<-renderUI({
    selectInput("other_var_select","Select Dataset Variables",choices =as.list(names(datasetInput()),multiple = TRUE))
  })
  
  output$response <- renderUI({
    resp <- names(datasetInput())[!sapply(ins, is.factor)]
    selectInput("resp", "Choose Response Variable", resp)
  })

  output$predictors <- renderUI({
    predx <- setdiff(names(datasetInput()), input$resp)
    checkboxGroupInput("predx", "Choose Predictor Variable(s)", predx)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
#################################################
  # Render data table
  output$dtable <- renderDataTable({
    filtered <- datasetInput() %>%
      filter(age >= input$age[1],
             age <= input$age[2]
      )
  }
  )

################################################
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  output$range <- renderPrint({ input$age })
  output$model <- renderPrint({
    df <- datasetInput()
    i <- input$resp
    y <- df[!is.na(df[,i]),i]
    cat('Regression is performed on the formula:',input$resp,'~',
        paste(names(xcol()),collapse='+') )
   fit <-  lm(y~., data= xcol())
   summary(fit)
    
  })
  
##############################################################
  output$vf <- renderPrint({
    df <- datasetInput()
    i <- input$resp
    y <- df[!is.na(df[,i]),i]
    cat('Regression is performed on the formula:',input$resp,'~',
        paste(names(xcol()),collapse='+') )
    fit2 <-  lm(y~., data= xcol())
    
    #Set the number of decimal places
    vif(fit2, digits= 4)
    
    # Set a VIF threshold. All the variables having higher VIF than threshold are dropped from the model
    threshold = values$n

    # Sequentially drop the variable with the largest VIF until all variables have VIF less than threshold
    drop=TRUE
    
    aftervif=data.frame()
    while(drop==TRUE) {
      vfit=vif(fit2)
      aftervif=rbind.fill(aftervif,as.data.frame(t(vfit)))
      if(max(vfit)>threshold) { fit2=
        update(fit2,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
      else { drop=FALSE }}
    
    # Model after removing correlated Variables
    print(fit2)
    
    # How variables removed sequentially
    t_aftervif= as.data.frame(t(aftervif))
    #edit(t_aftervif)
    
    # Final (uncorrelated) variables with their VIFs
    vfit_d= as.data.frame(vfit)
    vfit_d
    
  })
#############################################################
  output$plt <- renderPlot({
    df <- datasetInput()
    i <- input$resp
    y <- df[!is.na(df[,i]),i]
    x <- cbind(xcol(), y=y)
    x.m <- melt(x, 'y')
    tit <- paste('Segmentation Regression:',input$resp , ' vs other predictors')
    g <- ggplot(x.m, aes(x=y, y=value, color=variable)) + geom_point()
    g <- g + geom_smooth(method = 'lm')
    g <- g + facet_wrap(~variable, scales = 'free_y')
    g <- g + ylab('') + ggtitle(tit)
    g

  })
  #############################################################
  output$correlation <- renderPlot({
    df <- datasetInput()
    i <- input$resp
    y <- df[!is.na(df[,i]),i]
    x <- cbind(xcol(), y=y)
	z <- cbind(xcol(),i=i)
    x.m <- melt(x, 'y')
    tit <- paste('Pairs Matrix:',input$resp , ' With Predictor Variables')
    v <- ggpairs(z)
    v <- v + ggtitle(tit)
    v
    
  })

})
shinyApp(ui = ui, server = server)
