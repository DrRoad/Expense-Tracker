
library(shiny)
library(DBI)
library(dplyr)
library(scales)
library(lubridate)
library(openssl)
library(shinythemes)

# Database ----------------------------------------------------------------

DB_NAME <- "data.sqlite"
TBL_USER_DATA <- "users"

DB_test_connect <- function(){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  print("#######################")
  print("- Connected to Database")
  
  # If a user data table doesn't already exist, create one
  if(!(TBL_USER_DATA %in% dbListTables(db))){
    print("- Warning: No 'users' table found. Creating table...")
    df <- data.frame(ID = as.numeric(character()),
                     USER = character(),
                     HASH = character(),
                     cycle_end_date = as_date(character()),
                     theme = character(),
                     stringsAsFactors = FALSE)
    dbWriteTable(db, TBL_USER_DATA, df)
  } 
  
  print("- Table exists.")
  print("#######################")
  
  dbDisconnect(db)
}

DB_upload_csv <- function(filename, tblname){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  df <- read.csv(file = filename, header = T, row.names = F, stringsAsFactors = F)
  
  dbWriteTable(db, tblname, df)
  
  dbDisconnect(db)
}

DB_get_user <- function(user){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  users_data <- dbReadTable(db, TBL_USER_DATA)
  
  users_data <- filter(users_data, USER == user)
  
  dbDisconnect(db)
  
  return(users_data)
}
DB_add_user <- function(usr, hsh){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  df <- dbReadTable(db, TBL_USER_DATA)
  
  q <- paste("INSERT INTO", TBL_USER_DATA, "(ID, USER, HASH) VALUES (", paste("", nrow(df), ",", usr, ",", hsh, "", sep="'"), ")")
  
  #print(q)
  
  dbSendQuery(db, q)
  
  dbWriteTable(db, usr, data.frame(id = as.numeric(character()),          # Transaction ID
                                   date = as_date(character()),           # Date
                                   class = character(),                   # Expense, Withdrawal, Deposit
                                   amount = as.numeric(character()),      # Amount
                                   currency = character(),                # Currency
                                   conv_amount = as.numeric(character()), # Amount in base currency
                                   conv_currency = character(),           # Base currency 
                                   fx_rate = as.numeric(character()),     # FX rate on date
                                   category = character(),                # Category
                                   comments = character()                 # Comments on Expense
  ))
  
  suppressWarnings({dbDisconnect(db)})
  
}

DB_add_record <- function(usr, dt, clss, amt, curr, cnv_amt, cnv_curr, rate, ctgry, cmnts){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  if(usr %in% dbListTables(db)){
    df <- dbReadTable(db, usr)
    q <- paste("INSERT INTO", usr, "(id, date, class, amount, currency, conv_amount, conv_currency, fx_rate, category, comments)",
               "VALUES (", paste("", nrow(df), ",",
                                 dt, ",",
                                 clss, ",",
                                 amt, ",",
                                 curr, ",",
                                 cnv_amt, ",",
                                 cnv_curr, ",",
                                 rate, ",",
                                 ctgry, ",",
                                 cmnts, "", sep="'"), ")")
    print(paste("- Query:", q))
    dbSendQuery(db, q)
    print("- Record added")
  } else {
    print("- User's expense table does not exist")
  }
  
  dbDisconnect(db)
}
DB_add_expense <- function(usr, dt, amt, curr, cnv_amt, cnv_curr, rate, ctgry, cmnts){
  DB_add_record(usr = usr, dt = dt, clss = "Expense", amt = amt, curr = curr, cnv_amt = cnv_amt, cnv_curr = cnv_curr, rate = rate, ctgry = ctgry, cmnts = cmnts)
}
DB_add_deposit <- function(usr, dt, amt, curr, cnv_amt, cnv_curr, rate, ctgry, cmnts){
  DB_add_record(usr = usr, dt = dt, clss = "Deposit", amt = amt, curr = curr, cnv_amt = cnv_amt, cnv_curr = cnv_curr, rate = rate, ctgry = ctgry, cmnts = cmnts)
}

DB_get_records <- function(usr, clss = NULL, ID = NULL, ctgry = NULL){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  df <- NULL
  
  if(usr %in% dbListTables(db)){
    df <- dbReadTable(db, usr)
    
    if(!is.null(clss)){
      df <- filter(df, class %in% clss)
    }
  
    if(!is.null(ID)){
      df <- filter(df, id %in% ID)
    }
    
    if(!is.null(ctgry)){
      df <- filter(df, category %in% ctgry)
    }
    
  } else {
    print("- User's records do not exist")
  }
  
  dbDisconnect(db)
  return(df)
}
DB_get_deposit <- function(usr){
  DB_get_records(usr = usr, clss = c("Deposit"))
}
DB_get_expense <- function(usr){
  DB_get_records(usr = usr, clss = c("Expense"))
}

DB_update_user_field <- function(usr, hsh, field, value){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  df <- dbReadTable(db, TBL_USER_DATA)
  
  print(paste("- Attempting to update user field:", field, "with value:", value))

  df <- df[df$USER == usr & df$HASH == hsh,]

  if(nrow(df) > 0){
    q <- paste("UPDATE", TBL_USER_DATA, "SET", field, paste("= '", value, "' WHERE HASH = '", hsh, "'", " AND USER = '", usr, "'", sep=""))
    print(paste("- Query:", q))
    dbSendQuery(db, q)
    print(paste("- User: settings updated"))
  } else {
    print("- Invalid Login Information: no user found")
  }
  
  dbDisconnect(db)
}

DB_get_user_field <- function(usr, hsh, field){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  df <- dbReadTable(db, TBL_USER_DATA)
  
  df <- filter(df, (USER == usr && hsh == HASH))
  
  dbDisconnect(db)
  
  if(nrow(df) > 0){
    return(df[1,field])
  }
  
  return(NA)
}

# Init Database -----------------------------------------------------------

DB_test_connect()

# Functions and Presets ---------------------------------------------------------------

THEMES <- c("darkly", "cerulean")

get_FX_rate <- function(date, currency = "EUR"){
  df <- quantmod::getSymbols(paste(currency,"X", sep="="),src="yahoo",from=date, to=date, env = NULL)
  conversion_factor <- 1/quantmod::Cl(df)[1]
  return(conversion_factor)
}

# Server ------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # User App Information
  loggedIn <- reactiveVal(value = FALSE)
  user <- reactiveVal(value = NULL)
  cycle_end <- reactiveVal(value = NULL)
  ui_theme <- reactiveVal(value = NULL)
  ui_theme_str <- reactiveVal(value = NULL)
  user.base_currency <- reactiveVal(value = "USD")
  user.fx_currency <- reactiveVal(value = "EUR")
  
  # User App Events
  logout <- function(){
    user(NULL)
    loggedIn(FALSE)
    cycle_end(NULL)
    ui_theme(NULL)
    ui_theme_str(NULL)
    #user.base_currency(NULL)
    #user.base_currency(NULL)
    print("- User: logged out")
  }                                   # Logout function 
  set_ui <- function(s){
    if(is.null(s) || is.na(s)){
      ui_theme <- NULL
    } else {
      ui_theme(shinytheme(s))
    }
    ui_theme_str(s)
  }                                  # Set theme function
  
  login <- eventReactive(input$login, {
    
    user_data <- DB_get_user(input$username)
    
    if(nrow(user_data) > 0){ # If the active user is in the DB then logged in
      if(sha256(input$password) == user_data[1, "HASH"]){
        
        user(input$username)
        loggedIn(TRUE)
        cycle_end(user_data[1,"cycle_end_date"])
        
        if(is.null(user_data[1,"theme"]) || is.na(user_data[1,"theme"])){
          set_ui(NULL)
        } else {
          set_ui(user_data[1,"theme"])
        }
        
        print(paste("- User:", user(), "logged in"))
        
        return(TRUE)
      }
    }
    
    return(FALSE)
    
  })                  # Login observer
  register_user <- eventReactive(input$register_user, {
    
    users_data <- DB_get_user(input$new_user)
    
    if(nrow(users_data) > 0){
      return(span("User already exists", style = "color:red"))
    }
    
    new_hash <- sha256(input$new_pw)
    new_user <- input$new_user
    
    DB_add_user(new_user, new_hash)
    
    print("- New user added to database")
    
    return(span("New user registered", style = "color:green"))
    
  })  # New user event (adds to DB)
  observeEvent(input$create_login, {
    showModal(
      modalDialog(title = "Create Login", size = "m", 
                  textInput(inputId = "new_user", label = "Username"),
                  passwordInput(inputId = "new_pw", label = "Password"),
                  actionButton(inputId = "register_user", label = "Submit"),
                  p(input$register_user),
                  uiOutput("register_status")
                  
      )
    )
    
    register_user()
    
  })                     # New user event (opens dialogue)
  observeEvent(loggedIn()==TRUE, {
    updateDateInput(session = session, inputId = "settings.cycle_end", label = "Update Cycle End Date", value = cycle_end())
    updateSelectInput(session = session, inputId = "settings.theme", label = "Theme", choices = THEMES, selected = ui_theme_str())
  })                       # Post-Login App adjustments
  observeEvent(input$settings.update, {
    if(!is.null(input$settings.cycle_end) && input$settings.cycle_end != cycle_end()){
      end_date <- as_date(input$settings.cycle_end)
      days_left <- end_date - today()
      weeks_left <- days_left/7
      
      DB_update_user_field(usr = input$username, hsh = sha256(input$password), field = "cycle_end_date", value = end_date)
      
      cycle_end(end_date) #Store the cycle end in the reactive value container
    }
    
    if(!is.null(input$settings.theme) && (is.null(ui_theme_str()) || input$settings.theme != ui_theme_str())){
      set_ui(input$settings.theme)
      updateSelectInput(session = session, inputId = "settings.theme", label = "Theme", choices = THEMES, selected = ui_theme_str())
      DB_update_user_field(usr = input$username, hsh = sha256(input$password), field = "theme", value = input$settings.theme)
    }
    
  })                  # Update App settings
  observe({
    req(input$tabs)
    if(input$tabs == "Logout"){
      logout()
    }
  })                                              # Logout observer
  
  # App UI ------------------------------------------------------------
  observe({
    if(loggedIn()){
      output$App_Panel <- renderUI({
          navbarPage(title = "Expense Tracker", id = "tabs", 
                     # Overview Tab ------------------------------------------------------------
                     tabPanel( title = "Overview", icon = icon("hub"), 
                               sidebarLayout(
                                 sidebarPanel(width = 3,
                                              fluidRow(
                                                h4("Net Cash Available:", align = "center"),
                                                strong(textOutput("user.net_cash"), align = "center")), hr(),
                                              fluidRow(
                                                h4("Weeks Remaining in Cycle:", align = "center"),
                                                strong(textOutput("user.weeks_remaining"), align = "center")), hr(),
                                              fluidRow(
                                                h4("Net Cash per Week in Cycle:", align = "center"),
                                                strong(textOutput("user.weekly_net_cash"), align = "center")), hr(),
                                              fluidRow(
                                                h4("Foreign Net Cash per Week in Cycle:", align = "center"),
                                                strong(textOutput("user.weekly_net_fx"), align = "center"))
                                 ),
                                 mainPanel(
                                   column(width = 7,
                                          h4("Expense Record"),
                                          tableOutput("overview.expense_table")
                                   ),
                                   column(width = 5,
                                          h4("Historical Spending"),
                                          plotOutput("overview.historical_spending")
                                   )
                                 )
                               )
                     ),
                     # Expenses Tab ------------------------------------------------------------
                     tabPanel( title = "Expenses", icon = icon("credit-card"), 
                               fluidRow(
                                 column(7,
                                        h4("Expense History", align = "center"),
                                        dataTableOutput("expense.history_table")
                                 ),
                                 column(3,
                                        h4("Add Expense", align = "center"),
                                        hr(),
                                        fluidRow(
                                          column(5, selectInput(inputId = "expense_entry.currency", label = "Currency", 
                                                                choices = c("USD", "EUR"), selected = "USD", multiple = FALSE)),
                                          column(7, numericInput(inputId = "expense_entry.amount", label = "Amount",
                                                                 value = NULL, min = 0))
                                        ),
                                        fluidRow(
                                          column(5, dateInput(inputId = "expense_entry.date", label = "Date", 
                                                              value = today(), weekstart = 1)),
                                          column(7, textInput(inputId = "expense_entry.category", label = "Category"))
                                        ),
                                        fluidRow(
                                          column(12,
                                                 textAreaInput(inputId = "expense_entry.comment", label = "Comments", resize = "vertical"),
                                                 hr(),
                                                 actionButton(inputId = "expense_entry.add", label = "Submit"),
                                                 hr(),
                                                 textOutput(outputId = "expense_entry.status")
                                          )
                                        )
                                 )
                               )
                     ),
                     # Savings Tab ------------------------------------------------------------
                     tabPanel( title = "Savings", icon = icon("usd"), 
                               fluidRow(
                                 column(7,
                                        h4("Savings History", align = "center"),
                                        dataTableOutput("savings.history_table")
                                 ),
                                 column(3,
                                        h4("Add Deposit/Withdrawal", align = "center"),
                                        hr(),
                                        fluidRow(
                                          column(5, textOutput("user.base_currency")),
                                          column(7, numericInput(inputId = "savings_entry.amount", label = "Amount",
                                                                 value = 0, min = 0))
                                        ),
                                        fluidRow(
                                          column(5, dateInput(inputId = "savings_entry.date", label = "Date", 
                                                              value = today(), weekstart = 1)),
                                          column(7, textInput(inputId = "savings_entry.category", label = "Category"))
                                        ),
                                        fluidRow(
                                          column(12,
                                                 textAreaInput(inputId = "savings_entry.comment", label = "Comments", resize = "vertical"),
                                                 hr(),
                                                 actionButton(inputId = "savings_entry.add", label = "Submit"),
                                                 hr(),
                                                 textOutput(outputId = "savings_entry.status")
                                          )
                                        )
                                 )
                               )   
                     ),
                     # Menu Tabs ------------------------------------------------------------
                     navbarMenu( title = paste("Logged in as", user()), 
                                 tabPanel(title = "Logout", icon = icon("sign-out"), value = "Logout",
                                          p("logging out")
                                 ) ,
                                 tabPanel(title = "Settings", icon = icon("wrench"),
                                          h4(paste("Current Cycle End:", cycle_end() )),
                                          hr(),
                                          dateInput(inputId = "settings.cycle_end", label = "Update Cycle End Date", value = NULL, min = today(), weekstart = 1),
                                          selectInput(inputId = "settings.theme", label = "Theme", choices = THEMES, selected = NULL, multiple = FALSE),
                                          p("Theme changes will take effect upon reloading the app"),
                                          actionButton(inputId = "settings.update", label = "Update")
                                 )
                     ), theme = ui_theme()
          )
      })
    } else {
      
      # Login Page --------------------------------------------------------------
      
      output$App_Panel <- renderUI({
        fluidPage(theme = NULL,
          fluidRow(
            hr(),
            titlePanel(title = "Expense Tracker"), align = "center"
          ),
          fluidRow(
            column(4, offset = 4,
                   wellPanel(
                     h2("Login", align = "center"),
                     textInput(inputId = "username", label = "Username"),
                     passwordInput(inputId = "password", label = "Password"),
                     fluidRow(
                       column(4, offset = 4, actionButton(inputId = "login", label = "Login")),
                       column(4, offset = 4, actionLink(inputId = "create_login", label = "Create login")),
                       column(6, offset = 3, uiOutput(outputId = "login_status")
                       )
                     )
                   )
            )
          )
        )
      })
    }
  })
  
  # Outputs ------------------------------------------------------------
  
  output$register_status <- renderUI({
    if(input$register_user == 0){
      return(NULL)
    } else {
      register_user()
    }
  })
  output$login_status <- renderUI({
    if(input$login == 0){
      return(NULL)
    } else {
      if(!login()){
        return(span("The Username or Password is Incorrect", style = "color:red"))
      }
    }
  })
  output$user.base_currency <- reactive({user.base_currency()})
  output$user.fx_currency <- reactive({user.fx_currency()})
  
  # Overview
  user.net_cash <- reactive({
    df <- DB_get_records(usr = input$username)
    total_expenses <- sum(df[df$class=="Expense", "conv_amount"])
    total_deposits <- sum(df[df$class=="Deposit", "conv_amount"])
    return(total_deposits-total_expenses)
  })
  user.weeks_remaining <- reactive({
    round((as_date(cycle_end()) - today())/7, digits = 1)
  })
  user.weekly_net_cash <- reactive({
    user.net_cash() / as.numeric(user.weeks_remaining())
  })
  user.weekly_net_fx <- reactive({
    user.weekly_net_cash() / get_FX_rate(date = today(), currency = user.fx_currency())
  })
  
  output$user.net_cash <- reactive({
    paste(user.base_currency(), round(user.net_cash(),2))
    })
  output$user.weeks_remaining <- reactive({user.weeks_remaining()})
  output$user.weekly_net_cash <- reactive({
    paste(user.base_currency(), round(user.weekly_net_cash(),2))
    })
  output$user.weekly_net_fx <- reactive({
    paste(user.fx_currency(), round(user.weekly_net_fx(),2))
    })
  
  # Expenses
  output$expense_entry.status <- eventReactive(input$expense_entry.add, {
    validate(
      need(input$expense_entry.amount, "Missing 'Amount' input"),
      need(input$expense_entry.category, "Missing 'Category' input"),
      need(input$expense_entry.comment, "Missing 'Comments' input"),
      need(input$expense_entry.amount > 0, "'Amount' should be positive")
    )
    
    if(input$expense_entry.currency == "USD"){
      conversion_factor <- 1
    } else {
      conversion_factor <- get_FX_rate(as_date(input$expense_entry.date))
    }
    
    converted_amt <- conversion_factor * input$expense_entry.amount
    
    DB_add_expense(usr = input$username, dt = input$expense_entry.date, amt = input$expense_entry.amount, 
                   curr = input$expense_entry.currency, cnv_amt = converted_amt, cnv_curr = "USD", rate = conversion_factor, 
                   ctgry = input$expense_entry.category, cmnts = input$expense_entry.comment)
    
    return("Expense added")
    
  })
  output$expense.history_table <- renderDataTable(DB_get_expense(usr = input$username))
  
  # Savings
  output$savings_entry.status <- eventReactive(input$savings_entry.add, {
    validate(
      need(input$savings_entry.amount, "Missing 'Amount' input"),
      need(input$savings_entry.category, "Missing 'Category' input"),
      need(input$savings_entry.comment, "Missing 'Comments' input"),
      need(input$savings_entry.amount > 0, "'Amount' should be positive")
    )
    
    DB_add_deposit(usr = input$username,  dt = input$savings_entry.date, amt = input$savings_entry.amount, 
                   curr = "USD", cnv_amt = input$savings_entry.amount, cnv_curr = "USD", rate = 1, 
                   ctgry = input$savings_entry.category, cmnts = input$savings_entry.comment)
    
    return("Entry added")
  })
  output$savings.history_table <- renderDataTable(DB_get_deposit(usr = input$username))
})





