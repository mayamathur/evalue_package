#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("startup.R")

ui = source("ui.R")
server = source("server.R")

# Run the application 
shinyApp(ui = ui$value, server = server$value)

