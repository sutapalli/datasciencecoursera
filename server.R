#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyServer(function(input, output) {
  output$plot1 <- renderPlot({
          n <- input$numeric2
          z <- input$numeric3
          x <- seq(0,4*pi, length = n); y <- sin(x) + rnorm( n, sd=0.3)
          knots <- seq(0, 8*pi, length = z)
          splineTerms <- sapply(knots, function(knot)(x > knot) * (x - knot))
          xMat <- cbind(1,x,splineTerms)
          yhat <- predict(lm(y ~ xMat - 1))
          plot(x, y, frame = FALSE, pch = 21, bg = "blue", cex = 2)
          lines(x, yhat, col = "red", lwd = 2)
  })
})
