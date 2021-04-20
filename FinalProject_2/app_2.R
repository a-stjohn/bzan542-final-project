library(shiny)
library(caret)
library(parallel)
library(doParallel)
library(vip)
library(rpart)
library(rpart.plot)
library(regclass)
library(shinycssloaders)
library(dplyr)
library(regclass)
library(ggplot2)

load("542.RData")

ui2 <- fluidPage(
  titlePanel("App"),
  tabsetPanel(
    tabPanel(
      "Descriptive Statistics",
      sidebarLayout(
        sidebarPanel(
          selectInput("Variable", "Column (Descriptive Statistics)",
                      choices = colnames(DATA))
        ),
        mainPanel(
          plotOutput("Descriptive") %>% withSpinner(color = "#0dc5c1"),
          textOutput('type')
        )
      )
    ),
    
    tabPanel("Reg Tree",
             sidebarLayout(
               sidebarPanel(
                 numericInput(
                   "complexity",
                   "Please Input a Complexitity Parameter (Decision Tree):",
                   min = 0.005,
                   max = 0.2,
                   step = 0.005,
                   value = 0.03
                 )
               ),
               mainPanel(
                 plotOutput("DecisionTree") %>% withSpinner(color = "#0dc5c1"),
                 verbatimTextOutput("DecisionBest")
               )
             )),
    
    tabPanel("RF",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "mtry",
                   "Please Input an mtry Parameter (Random Forest):",
                   min = 1,
                   max = 31,
                   value = 20
                 ),
                 sliderInput(
                   "ntrees",
                   "Please Input a Number of Trees Parameter (Random Forest):",
                   min = 1,
                   max = 1000,
                   value = 500
                 )
               ),
               mainPanel(
                 verbatimTextOutput("rfSummary") %>% withSpinner(color = "#0dc5c1"),
                 plotOutput("rfAccuracy") %>% withSpinner(color = "#0dc5c1"),
                 plotOutput("rfImportance") %>% withSpinner(color = "#0dc5c1"),
               )
             )),
    tabPanel("SVM",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "svmDegree",
                   "Please Input a Degree (Support Vector Machine):",
                   min = 1,
                   max = 10,
                   value = 2,
                   # the optimal
                 ),
                 sliderInput(
                   "svmScale",
                   "Please Input a Scale (Support Vector Machine):",
                   min = 1e-04,
                   max = 0.9,
                   value = 0.0001,
                   # the optimal
                 ),
                 sliderInput(
                   "svmCost",
                   "Please Input a Cost (Support Vector Machine):",
                   min = 1,
                   max = 25,
                   value = 4,
                   # the optimal
                 )
               ),
               mainPanel(
                 verbatimTextOutput("svmSummary") %>% withSpinner(color = "#0dc5c1"),
                 plotOutput("svmAccuracy") %>% withSpinner(color = "#0dc5c1")
               )
             )),
    
    
    tabPanel("NN",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "range",
                   "Please Input a Range of Values for Size of Expand Grid (Neural Network):",
                   min = 1,
                   max = 10,
                   value = c(1, 4)
                 ),
                 selectInput(
                   "variable",
                   "Please Select Value(s) for Decay (Neural Network):",
                   c(.0001,
                     .0005, .001, .005, .01, .05, .1, .2, .3, .4, .5),
                   multiple = TRUE,
                   selected = c(.001, .01, .05)
                 )
                 
                 
               ),
               mainPanel(
                 plotOutput("NeuralNetwork") %>% withSpinner(color = "#0dc5c1"),
                 verbatimTextOutput("NeurBest")
               )
             ))
    
  )
)




server <- function(input, output) {
  output$Descriptive <- renderPlot({
    if (is.numeric(DATA[, input$Variable][[1]])) {
      # histogram for continuous variable
      ggplot(DATA, aes_string(x = input$Variable)) + geom_histogram(bins =
                                                                      10)
      
    } else {
      # barplot for categorical variable
      ggplot(DATA, aes_string(x = input$Variable)) + geom_bar()
      
    }
  })
  
  
  output$DecisionTree <- renderPlot({
    TREE <- rpart(GradeCat ~ .,
                  data = CLTRAIN,
                  cp = input$complexity)
    # draw the tree
    visualize_model(TREE)
  })
  
  
  output$DecisionBest <- renderPrint({
    TREE <-
      train(
        GradeCat ~ .,
        data = CLTRAIN,
        method = "rpart",
        tuneGrid = treeGrid,
        trControl = fitControl,
        preProc = c("center", "scale")
      )
    
    TREE$results[rownames(TREE$bestTune), ]
  })
  
  
  output$rfAccuracy <- renderPlot({
    # random forest params
    forestGrid <- expand.grid(mtry = seq(1, input$mtry, by = 1))
    ntrees <- input$ntrees
    
    # random forest model
    rfmod <-
      train(
        GradeCat ~ .,
        data = CLTRAIN,
        method = "rf",
        preProc = c("center", "scale"),
        trControl = fitControl,
        tuneGrid = forestGrid,
        importances = TRUE,
        ntree = ntrees
      )
    plot(rfmod)
  })
  
  output$rfImportance <- renderPlot({
    # random forest params
    forestGrid <- expand.grid(mtry = seq(1, input$mtry, by = 1))
    ntrees <- input$ntrees
    
    # random forest model
    rfmod <-
      train(
        GradeCat ~ .,
        data = CLTRAIN,
        method = "rf",
        preProc = c("center", "scale"),
        trControl = fitControl,
        tuneGrid = forestGrid,
        importances = TRUE,
        ntree = ntrees
      )
    vip(rfmod)
  })
  
  output$rfSummary <- renderPrint({
    # random forest params
    forestGrid <- expand.grid(mtry = seq(1, input$mtry, by = 1))
    ntrees <- input$ntrees
    
    # random forest model
    rfmod <-
      train(
        GradeCat ~ .,
        data = CLTRAIN,
        method = "rf",
        preProc = c("center", "scale"),
        trControl = fitControl,
        tuneGrid = forestGrid,
        importances = TRUE,
        ntree = ntrees
      )
    
    rfmod$results[which.max(rfmod$results$Accuracy),]
  })
  
  output$svmAccuracy <- renderPlot({
    # svm params
    svmPolyGrid <-
      expand.grid(
        degree = 1:input$svmDegree,
        scale = 1e-04:input$svmScale,
        C = 2 ^ (1:input$svmCost)
      )
    
    # svm mod
    SVMpoly <- train(
      GradeCat ~ .,
      data = CLTRAIN,
      method = 'svmPoly',
      trControl = fitControl,
      tuneGrid = svmPolyGrid,
      preProc = c("center", "scale")
    )
    plot(SVMpoly)
  })
  
  output$svmSummary <- renderPrint({
    # svm params
    svmPolyGrid <-
      expand.grid(
        degree = 1:input$svmDegree,
        scale = 1e-04:input$svmScale,
        C = 2 ^ (1:input$svmCost)
      )
    
    # svm mod
    SVMpoly <- train(
      GradeCat ~ .,
      data = CLTRAIN,
      method = 'svmPoly',
      trControl = fitControl,
      tuneGrid = svmPolyGrid,
      preProc = c("center", "scale")
    )
    
    SVMpoly$results[which.max(SVMpoly$results$Accuracy), ]
  })
  
  output$NeuralNetwork <- renderPlot({
    first <- input$range[1]
    second <- input$range[2]
    nnetGrid <-
      expand.grid(size = first:second, decay = input$variable)
    NNET <-
      train(
        GradeCat ~ .,
        data = CLTRAIN,
        method = 'nnet',
        trControl = fitControl,
        tuneGrid = nnetGrid,
        trace = FALSE,
        linout = FALSE,
        preProc = c("center", "scale")
      )
    # draw the tree
    plot(NNET)
    
    #output$NeurBest<-renderPrint({NNET$results[rownames(NNET$bestTune),]})
    
  })
  output$NeurBest <- renderPrint({
    first <- input$range[1]
    second <- input$range[2]
    nnetGrid <-
      expand.grid(size = first:second, decay = input$variable)
    NNET <-
      train(
        GradeCat ~ .,
        data = CLTRAIN,
        method = 'nnet',
        trControl = fitControl,
        tuneGrid = nnetGrid,
        trace = FALSE,
        linout = FALSE,
        preProc = c("center", "scale")
      )
    
    
    
    NNET$results[rownames(NNET$bestTune), ]
    
  })
  
  
  
}


shinyApp(ui = ui2, server = server)
