#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

DATA = read.csv("Data1.csv", header = T)
DATA = DATA[-c(1,2)]

#make clusters for cores
cluster <- makeCluster(detectCores() - 1)
cluster
registerDoParallel(cluster)

################### Code for Decision Tree ####################
nrows <- sample(nrow(DATA), size = 0.8 * nrow(DATA))
CLTRAIN <-
    DATA[nrows,-which(colnames(DATA) %in% c("G3", "intercept", "meanGradeCatabsences", "X"))]
CLTEST <-
    DATA[-nrows,-which(colnames(DATA) %in% c("G3", "intercept", "meanGradeCatabsences", "X"))]
fitControl <-
    trainControl(
        method = "cv",
        number = 4,
        classProbs = TRUE,
        allowParallel = T
    )
treeGrid <- expand.grid(cp = 10 ^ seq(-5,-1, length = 25))

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("FinalProject"),
    
    # Sidebar for input params
    sidebarLayout(
        sidebarPanel(
            numericInput(
                "complexity",
                "Please Input a Complexitity Parameter (Decision Tree):",
                min = 0.005,
                max = 0.2,
                step = 0.005,
                value = 0.03
            ),
            sliderInput(
                "mtry",
                "Please Input an mtry Parameter (Random Forest):",
                min = 1,
                max = 100,
                value = 24
            ),
            sliderInput(
                "ntrees",
                "Please Input a Number of Trees Parameter (Random Forest):",
                min = 1,
                max = 1000,
                value = 500
            ),
            sliderInput(
                "range",
                "Please Input a Range of Values for Size of Expand Grid (Neural Network):",
                min = 1,
                max = 10,
                value = c(1,4)
            ),
            selectInput("variable", "Please Select Value(s) for Decay (Neural Network):",
                        c(.0001,
                            .0005,.001,.005,.01,.05,.1,.2,.3,.4,.5),
                          multiple = TRUE,selected =.01),
            sliderInput(
                "svmDegree",
                "Please Input a Degree (Support Vector Machine):",
                min = 1,
                max = 10,
                value = 3, # the optimal
            ),
            sliderInput(
                "svmScale",
                "Please Input a Scale (Support Vector Machine):",
                min = 1e-04,
                max = 0.9,
                value = 0.001, # the optimal
            ),
            sliderInput(
                "svmCost",
                "Please Input a Cost (Support Vector Machine):",
                min = 1,
                max = 25,
                value = 8, # the optimal
            )
        ),
        
        # Create tabs
        tabsetPanel(
            type = "tabs",
            # decision tree tab
            tabPanel("Decision Tree", 
                     plotOutput("DecisionTree") %>% withSpinner(color="#0dc5c1"),
                     verbatimTextOutput("DecisionBest")
                     ), 
            # random forest tab
            tabPanel(
                "Random Forest",
                plotOutput("rfAccuracy") %>% withSpinner(color="#0dc5c1"),
                plotOutput("rfImportance") %>% withSpinner(color="#0dc5c1")
            ),
            tabPanel(
                "Neural Network",
                plotOutput("NeuralNetwork") %>% withSpinner(color="#0dc5c1"),
                verbatimTextOutput("NeurBest")),
            tabPanel(
                "Support Vector Machine",
                plotOutput("svmAccuracy") %>% withSpinner(color="#0dc5c1")
            )
            
        )
    ))

server <- function(input, output) {
    output$DecisionTree <- renderPlot({
        TREE <- rpart(GradeCat ~ .,
                      data = CLTRAIN,
                      cp = input$complexity)
        # draw the tree
        visualize_model(TREE)
    })
    
    output$DecisionBest <- renderPrint({
        TREE <- train(GradeCat~.,data=CLTRAIN,method="rpart", tuneGrid=treeGrid,trControl=fitControl, preProc = c("center", "scale"))
        
        TREE$results[rownames(TREE$bestTune),]
        
        
    })
    
    output$NeuralNetwork <- renderPlot({
        first<-input$range[1]
        second<-input$range[2]
        nnetGrid <- expand.grid(size=first:second,decay=input$variable)
        NNET <- train(GradeCat~.,data=CLTRAIN,method='nnet',trControl=fitControl,tuneGrid=nnetGrid,trace=FALSE,linout=FALSE,preProc = c("center", "scale"))
        # draw the tree
        plot(NNET)
       
        #output$NeurBest<-renderPrint({NNET$results[rownames(NNET$bestTune),]})
        
    })
    output$NeurBest <- renderPrint({
        first<-input$range[1]
        second<-input$range[2]
        nnetGrid <- expand.grid(size=first:second,decay=input$variable)
        NNET <- train(GradeCat~.,data=CLTRAIN,method='nnet',trControl=fitControl,tuneGrid=nnetGrid,trace=FALSE,linout=FALSE,preProc = c("center", "scale"))
        
        
        
        NNET$results[rownames(NNET$bestTune),]
        
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
}

# Run the application
shinyApp(ui = ui, server = server)