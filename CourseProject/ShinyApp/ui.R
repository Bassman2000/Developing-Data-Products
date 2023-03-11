library(shiny)
library(shinycssloaders)
library(dplyr)

listFractals <- function(){
  
  if(!exists('fractals')) load(file = 'GPRFDATA/fractals.RData')

  return(names(fractals))
}

listMatrices <- function(){
  
  if(!exists('matrices')) load(file = 'GPRFDATA/matrices.RData')
  
  return(names(matrices))
}


# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Fractal Visualiser"),
  
  # Sidebar with a drop down of available fractals
  sidebarLayout(
    sidebarPanel(
      radioButtons("fStyle","Choose Fractal Style", 
                   choices = list("Iterated Function", "Matrix"),
                   selected = "Iterated Function"),
      selectInput("fPatt", "Fractal Pattern:", 
                  choices = listFractals(), selected = 0),
    ),
    
    mainPanel(plotOutput("distPlot") %>% withSpinner(color = "purple"))
  )
)