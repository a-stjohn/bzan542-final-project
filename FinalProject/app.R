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
                max = 0.05,
                step = 0.005,
                value = 0.04641589
            ),
            sliderInput(
                "mtry",
                "Please Input an mtry Parameter (Random Forest):",
                min = 1,
                max = 100,
                value = 25
            ),
            sliderInput(
                "ntrees",
                "Please Input a Number of Trees Parameter (Random Forest):",
                min = 1,
                max = 1000,
                value = 500
            )
        ),
        
        # Create tabs
        tabsetPanel(
            type = "tabs",
            # decision tree tab
            tabPanel("Decision Tree", plotOutput("DecisionTree")), 
            # random forest tab
            tabPanel(
                "Random Forest",
                plotOutput("rfAccuracy") %>% withSpinner(color="#0dc5c1"),
                plotOutput("rfImportance") %>% withSpinner(color="#0dc5c1")
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
}

# Run the application
shinyApp(ui = ui, server = server)
