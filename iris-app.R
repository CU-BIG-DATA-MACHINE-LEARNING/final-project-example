install.packages(c("stats","dplyr","randomForest","shiny","shinythemes"))
library(shiny)
library(shinythemes)
library(stats)
library(dplyr)
library(randomForest)

View(iris)

ui<- fluidPage(theme=shinytheme("united"),
      headerPanel("Match The Iris"),
      sidebarPanel(
        h2("Input your iris measurements"),
        sliderInput('petalWidth', 'Petal Width', min=min(iris$Petal.Width), max=max(iris$Petal.Width), value=1),
        sliderInput('petalLength', 'Petal Length', min=min(iris$Petal.Length), max=max(iris$Petal.Length), value=1),
        sliderInput('sepalWidth', 'Sepal Width', min=min(iris$Sepal.Width), max=max(iris$Sepal.Width), value=3),
        sliderInput('sepalLength', 'Sepal Length', min=min(iris$Sepal.Length), max=max(iris$Sepal.Length), value=5),
        actionButton("submitBtn", "Predict Species", class="btn btn-primary")
      ),
      mainPanel(
        tags$label(h2('Iris Species Prediction')),
        verbatimTextOutput('contents')
      )
)


server<- function(input, output){
  index = sample(2, nrow(iris), replace=T, prob=(c(0.9, 0.1)))
  Training = iris[index==1,]
  RFM = randomForest(Species~., data = Training)
  
  output$contents <- renderPrint({
    if(input$submitBtn>0){ #when someone clicks the submit button
      inputData<- data.frame(Sepal.Length = input$sepalLength,
                             Sepal.Width = input$sepalWidth,
                             Petal.Length = input$petalLength,
                             Petal.Width = input$petalWidth)
      
      predict(RFM, inputData)
      
       
    }
  })
  
}


shinyApp(ui=ui, server=server)



