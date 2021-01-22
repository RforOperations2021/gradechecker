library(shiny)
library(readxl)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(shinydashboard)

file <- "./grades/R Shiny 2021 Grades.xlsx"
passwords <- read_excel(file, sheet = 5)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
        dashboardHeader(
            title = "R Shiny for Operations Management Grade App"
        ),
        dashboardSidebar(
            useShinyjs(),
            # Login Div
            div(id = "auth",
                    textInput("andrewID",
                              "Andrew ID:",
                              placeholder = "Enter you Andrew ID"),
                    passwordInput("password",
                                  "Password:"),
                    disabled(actionButton("logIn", "Log In"))
            ),
            selectInput("assignment",
                        "View Grade for:",
                        choices = c("Course Grades", "Homework 1", "Homework 2", "Final Project"),
                        selected = "Homework 1")
            ),
        dashboardBody(
                fluidRow(
                    valueBoxOutput("gradeAverage")
                ),
                # Table with Grades
                fluidRow(
                    box(width = 12, 
                        DT::dataTableOutput("gradeTable"))
                )
            )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Set login to false and save passwords as reactive value
    values <- reactiveValues(login = FALSE, passwords = passwords)
    # Check if password fits
    observe({
        if (input$andrewID %in% values$passwords$`Andrew ID` & nchar(input$password) > 5 & grepl('[^[:alnum:]]', input$password, perl = TRUE)) {
            enable("logIn")
        } else {
            disable("logIn")
        }
    })
    # Authenticate Users
    observeEvent(input$logIn, {
        auth <- read_excel(file, sheet = 5) %>%
            filter(`Andrew ID` == input$andrewID)
        values$login <- auth$Password == input$password
        if (values$login) {
            showNotification("You have successfully logged in!")
            hide("auth")
        } else {
            showNotification("Could not match username or password!", type = "error")
        }
    })
    grades <- reactive({
        grades <- read_excel(file, sheet = input$assignment) %>%
            filter(!is.na(`Last Name`)) %>%
            mutate(`Final Grade` = as.numeric(`Final Grade`))
    })
    # Load Grade Sheet
    grade <- reactive({
        if (values$login) {
            grades() %>%
                filter(`Andrew ID` == input$andrewID)
        }
    })
    # Grade Average
    output$gradeAverage <- renderValueBox({
        value <- mean(grades()$`Final Grade`, na.rm = TRUE)
        subtitle <- paste("Avg for ", input$assignment)
        valueBox(value = round(value ,2), subtitle = subtitle, icon = icon("check-square"))
    })
    # Display grade
    output$gradeTable <- DT::renderDataTable({
        grade()
    }, extensions = 'Buttons', 
    rownames = FALSE,
    options = list(dom = 'Bt',
                   buttons = c('copy', 'csv', 'pdf')
                   )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
