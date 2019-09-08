library(shiny)
library(googlesheets)
library(shinyjs)
library(dplyr)
library(shinythemes)

# Connect to Google Sheet with Grades
gs_auth()
# Grading Sheet Key
sheet_key <- gs_key("1yZBck1ONc1EpR6g2dKBsQBYWIl2Dq2WtISbGdEUT_oE")
# Load passwords for name check
passwords <- gs_read(sheet_key, ws = 6)

# Define UI for application that draws a histogram
ui <- fluidPage(style = "margin: 5px;", theme = shinytheme("sandstone"),
    # Application title
    titlePanel("R Shiny for Operations Management Grade App"),
    useShinyjs(),
    absolutePanel(id = "auth", top = 60, left = 0, style = "z-index: 1000; min-width: 350px;",
                  # Login and Password Creation Panel
                  wellPanel(
                          textInput("andrewID",
                                    "Andrew ID:",
                                    placeholder = "Enter you Andrew ID"),
                          # Login Div
                          div(id = "login",
                              passwordInput("password",
                                            "Password:"),
                              column(8, 
                                     actionButton("makePass", 
                                           "Create Password")
                              ),
                              column(4, 
                                     disabled(
                                         actionButton("logIn",
                                                      "Log In")
                                     )
                              ),
                              br(), br()
                      ),
                      # Password creation div
                      hidden(
                          div(id = "createPass",
                              passwordInput("passwordNew",
                                            "Enter a new Password:"),
                              HTML('Passwords must be at least 7 characters long and contain at least 1 special character.<br>'),
                              disabled(
                                  actionButton("create",
                                               "Create Password")
                              )
                          )
                      )
                  )
    ),
    
    # Select Sheet to View
    fluidRow(
        selectInput("assignment",
                    "View Grade for:",
                    choices = c("Course Grades", "Homework 1", "Project 1", "Homework 2", "Project 2"),
                    selected = "Course Grades"
                    )
    ),
    # Table with Grades
    fluidRow(
        DT::dataTableOutput("gradeTable")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Set login to false and save passwords as reactive value
    values <- reactiveValues(login = FALSE, passwords = passwords)
    # Allow users to create login
    observeEvent(input$makePass, {
        show("createPass")
        hide("login")
    })
    # Check if new password fits
    observe({
        if (input$andrewID %in% values$passwords$`Andrew ID` & nchar(input$passwordNew) > 6 & grepl('[^[:alnum:]]', input$passwordNew, perl = TRUE)) {
            enable("create")
        } else {
            disable("create")
        }
    })
    # Check if password fits
    observe({
        if (input$andrewID %in% values$passwords$`Andrew ID` & nchar(input$password) > 6 & grepl('[^[:alnum:]]', input$password, perl = TRUE)) {
            enable("logIn")
        } else {
            disable("logIn")
        }
    })
    # Store new password in google sheet
    observeEvent(input$create, {
        # grades <- grades()
        if (input$andrewID %in% values$passwords$`Andrew ID`) {
            row <- which(grades$`Andrew ID` == input$andrewID) + 1
            gs_edit_cells(sheet_key, ws = 6, anchor = paste0("B", row), input = input$passwordNew, byrow = TRUE)
            showNotification("You have succesfully created a Password", type = "message")
            # Read in New Password
            values$passwords <- gs_read(sheet_key, ws = 6)
            hide("createPass")
            show("login")
        } else {
            showNotification("Please enter a valid Andrew ID", type = "error")
            updateTextInput("passwordNew", value = "")
        }
    })
    # Authenticate Users
    observeEvent(input$logIn, {
        auth <- gs_read(sheet_key, ws = 6) %>%
            filter(`Andrew ID` == input$andrewID)
        values$login <- auth$Password == input$password
        if (values$login) {
            showNotification("You have successfully logged in!")
            hide("auth")
        } else {
            showNotification("Could not match username or password!", type = "error")
        }
    })
    # Load Grade Sheet
    grade <- reactive({
        if (values$login) {
            grade <- gs_read(sheet_key, ws = input$assignment) %>%
                filter(`Andrew ID` == input$andrewID)
        }
    })
    # Display grade
    output$gradeTable <- DT::renderDataTable({
        grade()
    }, options = list(dom = 't')
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
