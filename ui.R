#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Developing Data Products Project"),

  sidebarLayout(
    sidebarPanel(
        numericInput("numeric2", "Sequance length ?", value = 1000, min=1, max = 1000, step=1),
        numericInput("numeric3", "Knot length ?", value = 100, min=1, max = 20, step=1)
    ),

    mainPanel(
       h3("    Hodgepodge"),
       plotOutput("plot1")
    )
  )
))
