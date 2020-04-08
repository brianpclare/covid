#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Symptoms Checker"),

        # Show a plot of the generated distribution
        mainPanel(
           checkboxGroupInput(inputId = "symptoms", label = "Symptoms", 
                              choices = c("Cough", "Fever", "Trouble Breathing", "Body Aches",
                                          "Headache", "Fatigue", "Sore Throat", "Diarrhea",
                                          "Runny Nose", "Sneezing", "Watery Eyes")),
           
           submitButton(text = "Check Symptoms")
        ),
    tableOutput("data")
    # textOutput("test_class")
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    ref <- tibble(symptom = c("Cough", "Fever", "Trouble Breathing", "Body Aches",
                              "Headache", "Fatigue", "Sore Throat", "Diarrhea",
                              "Runny Nose", "Sneezing", "Watery Eyes"),
                  covid = c(0.8, 0.8, 0.8, 0.6, 0.6, 0.6, 0.6, 0.4, 0.2, 0.2, 0),
                  flu = c(0.8, 0.8, 0, 0.8, 0.8, 0.8, 0.6, 0.6, 0.6, 0, 0),
                  cold = c(0.4, 0.2, 0, 0.8, 0.2, 0.6, 0.8, 0, 0.8, 0.8, 0),
                  allergies = c(0.6, 0.6, 0.6, 0, 0.6, 0.6, 0, 0, 0.8, 0.8, 0.8))

    input_binary <- reactive({
        ref$symptom %in% input$symptoms
    })
    
    # output$test_class <- reactive({input$symptoms})
    
    cor_covid <- reactive({cor(ref$covid, input_binary())})
    cor_flu <- reactive({cor(ref$flu, input_binary())})
    cor_cold <- reactive({cor(ref$cold, input_binary())})
    cor_aller <- reactive({cor(ref$allergies, input_binary())})

    output$data <- renderTable({
        tibble(COVID = (cor_covid() + 1) * 50 ,
                Flu = (cor_flu() + 1) * 50,
                Cold =  (cor_cold() + 1) * 50,
                Allergies = (cor_aller() + 1) * 50)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
