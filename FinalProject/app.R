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
DATA=read.csv("Data1.csv",header = T)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FinalProject"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("complexity",
                        "Please Input a Complexitity Parameter:",
                        min = 0.005,
                        max = 0.05,
                        step = 0.005,
                        value = 0.04641589)
        ),

        # Show a plot of the generated distribution
        tabsetPanel(type = "tabs",
                    tabPanel("Decision Tree", plotOutput("DecisionTree"))
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$DecisionTree <- renderPlot({
        # generate bins based on input$bins from ui.R
        data1<-read.csv("Data1.csv")
        nrows<-sample(nrow(data1),size = 0.8*nrow(data1))
        CLTRAIN<-data1[nrows,-which(colnames(data1) %in% c("G3","intercept","meanGradeCatabsences","X"))]
        CLTEST<-data1[-nrows,-which(colnames(data1) %in% c("G3","intercept","meanGradeCatabsences","X"))]
        fitControl <- trainControl(method="cv",number=4,classProbs = TRUE, allowParallel = T) 
        treeGrid <- expand.grid(cp=10^seq(-5,-1,length=25))
        
        
        TREE = rpart(GradeCat~.,data = CLTRAIN,cp = input$complexity)
       

        # draw the histogram with the specified number of bins
        visualize_model(TREE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
