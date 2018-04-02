
library(shiny)
library(DBI)
library(data.table)
library(DT)
library(dplyr)
library(scales)
library(lubridate)
library(openssl)
library(shinythemes)
library(ggplot2)
library(reshape2)
library(plotly)
library(ggthemes)

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
                     cycle_start_date = as_date(character()),
                     theme = character(),
                     stringsAsFactors = FALSE)
    dbWriteTable(db, TBL_USER_DATA, df)
  } 
  
  print("- Table exists.")
  print("#######################")
  
  dbDisconnect(db)
}

DB_upload_df <- function(df, tblname){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  current_df <- dbReadTable(db, tblname) # Retrieve the current DB

  # Create IDs for new transactions
  if(nrow(current_df) == 0){
    df$id <- 1:nrow(df)
  } else {
    df$id <- max(current_df$id):(max(current_df$id) + nrow(df))
  }
  
  df <- df[,colnames(current_df)]
  
  print("### Data To Be Added ###")
  print(df)
  
  dbWriteTable(db, tblname, df, append = TRUE)
  
  print("- Data Added")
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

DB_remove_records <- function(usr, ids){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  if(usr %in% dbListTables(db)){
    df <- dbReadTable(db, usr)
    q <- paste("DELETE FROM", usr, "WHERE id IN", paste0("(", paste(ids, collapse = ","), ")") )
    print(paste("- Query:", q))
  
    dbSendStatement(db, q)
    print("- Transactions removed.")
  } else {
    print("- User's transaction table does not exist")
  }
  
  dbDisconnect(db)
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
    print("- User's transaction table does not exist")
  }
  
  dbDisconnect(db)
}
DB_add_expense <- function(usr, dt, amt, curr, cnv_amt, cnv_curr, rate, ctgry, cmnts){
  DB_add_record(usr = usr, dt = dt, clss = "Expense", amt = amt, curr = curr, cnv_amt = cnv_amt, cnv_curr = cnv_curr, rate = rate, ctgry = ctgry, cmnts = cmnts)
}
DB_add_deposit <- function(usr, dt, amt, curr, cnv_amt, cnv_curr, rate, ctgry, cmnts){
  DB_add_record(usr = usr, dt = dt, clss = "Deposit", amt = amt, curr = curr, cnv_amt = cnv_amt, cnv_curr = cnv_curr, rate = rate, ctgry = ctgry, cmnts = cmnts)
}

DB_modify_record <- function(usr, id, dt, clss, amt, curr, cnv_amt, cnv_curr, rate, ctgry, cmnts){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  if(usr %in% dbListTables(db)){
    
    q <- paste("UPDATE", usr, 
               paste("SET ",
                     "date = '", dt,"', ",
                     "class = '", clss,"', ",
                     "amount = ", amt,", ",
                     "currency = '", curr,"', ",
                     "conv_amount = ", cnv_amt,", ",
                     "conv_currency = '", cnv_curr,"', ",
                     "fx_rate = ", rate,", ",
                     "category = '", ctgry,"', ",
                     "comments = '", cmnts,"'", sep=""),
               "WHERE id =", id)
    print(paste("- Query:", q))
    dbSendStatement(db, q)
    print("- Transaction Updated")
  } else {
    print("- User's transaction table does not exist")
  }
  
  dbDisconnect(db)
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

THEMES <- c("basic","darkly", "cerulean")

get_FX_rate <- function(date, currency = "EUR"){
  df <- quantmod::getSymbols(paste(currency,"X", sep="="),src="yahoo",from=date, to=date, env = NULL)
  conversion_factor <- 1/quantmod::Cl(df)[1]
  return(conversion_factor)
}
summarise_expenses <- function(start_date, end_date, expense_tbl){
  
  start_date <- as_date(start_date) 
  end_date <- as_date(end_date)
  
  exp <- expense_tbl
  
  num_weeks <- ceiling(as.numeric(end_date - start_date)/7)
  
  tbl <- data.frame(week = 1:num_weeks,
                    total = 0)
  
  tbl[unique(exp$category)] <- 0
  
  for(week in 1:num_weeks){
    week_start <- start_date + days((week-1)*7) 
    week_end <- (start_date + days(6)) + days((week-1)*7)
    
    if(week_end > end_date){ week_end <- end_date} # If the cycle end predates the week's end, set week's end to cycle end
    
    exp_in_week <- exp[exp$date >= week_start & exp$date <= week_end,] # Retrieve the expenses for the current week
    
    aggr_tbl <- exp_in_week %>%              # Take expenses in the current week
      group_by(category) %>%                 # Group expenses by category
      summarise(cat_exp = sum(conv_amount))  # Aggregate expenses by category
    
    tbl[week, aggr_tbl$category] <- aggr_tbl$cat_exp # Assign expenditures back to Aggregate table
  }
  
  tbl$total <- rowSums(tbl[,-1*1:2], na.rm = TRUE)
  return(tbl)
}
get_overview_plot <- function(df, start_date, end_date){
  df <- df[,!(colnames(df) %in% "total")]
  plot.df <- melt(data = df, value.name = "spent", "week")
  colnames(plot.df)[2] <- "category"
  
  p2 <- ggplot(data = plot.df, mapping = aes(x = week, y = spent, color = category)) + 
    geom_line(mapping = aes(x = week, y = spent)) + theme_dark() + xlab("Week") + ylab("Amount (USD)") +
    ggtitle("Expense Trends by Week", paste("For the period", start_date, "to", end_date))
  
  plot.df$week <- as.factor(plot.df$week)
  
  p1 <- ggplot(data = plot.df, mapping = aes(x = week, y = spent, group = category, fill = category)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_dark() +
    scale_fill_discrete() + xlab("Week") + ylab("Amount (USD)") +
    ggtitle("Expenses by Week", paste("For the period", start_date, "to", end_date))
  
  return(list(bar = p1, line = p2))
}

# Server ------------------------------------------------------------------

#Global DT options
options(DT.options = list(autoWidth = TRUE))

shinyServer(function(input, output, session) {
  
  # User App Information
  loggedIn <- reactiveVal(value = FALSE)
  user <- reactiveVal(value = NULL)
  cycle_end <- reactiveVal(value = NULL)
  cycle_start <- reactiveVal(value = NULL)
  ui_theme <- reactiveVal(value = NULL)
  ui_theme_str <- reactiveVal(value = NULL)
  user.base_currency <- reactiveVal(value = "USD")
  user.fx_currency <- reactiveVal(value = "EUR")
  user.categories <- reactiveVal(value = NULL)
  
  ### User App Events
  logout <- function(){
    user(NULL)
    loggedIn(FALSE)
    cycle_end(NULL)
    cycle_start(NULL)
    ui_theme(NULL)
    ui_theme_str(NULL)
    #user.base_currency(NULL)
    #user.base_currency(NULL)
    user.categories(NULL)
    set_ui("basic")
    print("- User: logged out")
  }                                   # Logout function 
  set_ui <- function(s){
    if(s == "basic"){ s <- NULL}
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
        cycle_start(user_data[1,"cycle_start_date"])
        
        if(is.na(cycle_end())){cycle_end(NULL)}
        if(is.na(cycle_start())){cycle_start(NULL)}
        
        if(is.null(user_data[1,"theme"]) || is.na(user_data[1,"theme"])){
          set_ui("basic")
        } else {
          set_ui(user_data[1,"theme"])
        }
        
        user.categories(unique(DB_get_records(usr = user())[,"category"]))
        
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
    updateDateInput(session = session, inputId = "settings.cycle_start", label = "Update Cycle Start Date", value = cycle_start())
    updateSelectInput(session = session, inputId = "settings.theme", label = "Theme", choices = THEMES, selected = ui_theme_str())
  })                       # Post-Login App adjustments
  observeEvent(input$settings.update, {
    
    if(!is.null(input$settings.cycle_end) && (is.null(cycle_end()) || input$settings.cycle_end != cycle_end())){
      end_date <- as_date(input$settings.cycle_end)
      
      DB_update_user_field(usr = input$username, hsh = sha256(input$password), field = "cycle_end_date", value = end_date)
      
      cycle_end(end_date) #Store the cycle end in the reactive value container
    }
    if(!is.null(input$settings.cycle_start) && (is.null(cycle_start()) || input$settings.cycle_start != cycle_start())){
      start_date <- as_date(input$settings.cycle_start)
      
      DB_update_user_field(usr = input$username, hsh = sha256(input$password), field = "cycle_start_date", value = start_date)
      
      cycle_start(start_date) #Store the cycle start in the reactive value container
    }
    if(!is.null(input$settings.theme) && (is.null(ui_theme_str()) || input$settings.theme != ui_theme_str())){
      set_ui(input$settings.theme)
      updateSelectInput(session = session, inputId = "settings.theme", label = "Theme", choices = THEMES, selected = ui_theme_str())
      DB_update_user_field(usr = input$username, hsh = sha256(input$password), field = "theme", value = input$settings.theme)
    }
    
  })                  # Update App settings
  observeEvent(input$settings.add_category, {                 # Add category
    validate(
      need( !(input$settings.new_category_name %in% user.categories()), message = "Category already exists"),
      need( !(tolower(input$settings.new_category_name) %in% sapply(user.categories(), tolower)), message = "Category already exists" )
      )
    user.categories(c(user.categories(), input$settings.new_category_name))
    updateTextInput(session, inputId = "settings.new_category_name", label = "Enter New Category", value = "")
  })            # Add Expense/Deposit Category
  observe({
    req(input$tabs)
    if(input$tabs == "Logout"){
      logout()
    }
  })                                              # Logout observer
  
  # Display-Modal Events
  observeEvent(input$expense.add, {
    showModal(modal.expense_entry())
  })                      # "Add Expense" Dialogue
  observeEvent(input$savings.add, {
    showModal(modal.savings_entry())
  })                      # "Add Savings" Dialogue
  observeEvent(input$expense.delete, {
    showModal(modal.expense_remove())
  })                   # "Delete Expense" Dialogue
  observeEvent(input$expense.delete_confirm, {
    DB_remove_records(usr = input$username, ids = exp_tbl_last_selected()$id)
    removeModal()
  })           # Confirm and remove Expenses
  observeEvent(input$savings.delete, {
    showModal(modal.savings_remove())
  })                   # "Delete Savings" Dialogue
  observeEvent(input$savings.delete_confirm, {
    DB_remove_records(usr = input$username, ids = sav_tbl_last_selected()$id)
    removeModal()
  })           # Confirm and remove Deposits
  observeEvent(input$expense.modify, {
    showModal(modal.expense_edit())
  })                   # Modify Expense
  observeEvent(input$savings.modify, {
    showModal(modal.savings_edit())
  })                   # Modify Deposity
  
  # Global Event Functions
  update_datatables <- reactive({
    # List inputs to trigger updates
    input$expense_entry.add
    input$expense_modify.add
    input$expense.delete_confirm
    input$savings_entry.add
    input$savings_modify.add
    input$savings.delete_confirm
    new_transactions()
    return(NULL)
  })
  
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
                                                h4("Cycle Start:", align = "center"),
                                                strong(textOutput("user.cycle_start"), align = "center")), hr(),
                                              fluidRow(
                                                h4("Cycle End:", align = "center"),
                                                strong(textOutput("user.cycle_end"), align = "center")), hr(),
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
                                   column(width = 12,
                                          h4("Historical Spending"),
                                          plotlyOutput("overview.historical_spending"),
                                          br(),
                                          column(4, offset = 4,
                                                 selectInput("overview.plot_type", label = "Plot Type", choices = c("bar","line"), multiple = FALSE, width = "100%")
                                                 )
                                          ),
                                   column(width = 12,
                                          hr(),
                                          h4("Expense Record"),
                                          dataTableOutput("overview.expense_table")
                                   )
                                 )
                               )
                     ),
                     # Expenses Tab ------------------------------------------------------------
                     tabPanel( title = "Expenses", icon = icon("credit-card"), 
                               fluidRow(
                                 column(12,
                                        h4("Expense History", align = "center"),
                                        hr(),
                                        column(6, offset = 3,
                                               actionButton(inputId = "expense.add", label = "Add Expense", width = '30%'),
                                               actionButton(inputId = "expense.delete", label = "Delete Selected", width = '30%'),
                                               actionButton(inputId = "expense.modify", label = "Modify Selected", width = '30%')
                                        ),
                                        dataTableOutput("expense.history_table")
                                 )
                               )
                     ),
                     # Savings Tab ------------------------------------------------------------
                     tabPanel( title = "Savings", icon = icon("usd"), 
                               fluidRow(
                                 column(12,
                                        h4("Savings History", align = "center"),
                                        hr(),
                                        column(6, offset = 3,
                                               actionButton(inputId = "savings.add", label = "Add Deposit", width = '30%'),
                                               actionButton(inputId = "savings.delete", label = "Delete Selected", width = '30%'),
                                               actionButton(inputId = "savings.modify", label = "Modify Selected",  width = '30%')
                                        ),
                                        dataTableOutput("savings.history_table")
                                 )
                               )   
                     ),
                     # Menu Tabs ------------------------------------------------------------
                     navbarMenu( title = paste("Logged in as", user()), 
                                 tabPanel(title = "Logout", icon = icon("sign-out"), value = "Logout",
                                          p("logging out")
                                 ) ,
                                 tabPanel(title = "Settings", icon = icon("wrench"),
                                          column(4,
                                                 wellPanel(
                                                   h3("Main App Settings"),
                                                   hr(),
                                                   h4(paste("Current Cycle Start:", cycle_start() )),
                                                   h4(paste("Current Cycle End:", cycle_end() )),
                                                   hr(),
                                                   dateInput(inputId = "settings.cycle_start", label = "Update Cycle Start Date", value = cycle_start(), max = today(), weekstart = 1),
                                                   dateInput(inputId = "settings.cycle_end", label = "Update Cycle End Date", value = cycle_end(), min = today(), weekstart = 1),
                                                   selectInput(inputId = "settings.theme", label = "Theme", choices = THEMES, selected = NULL, multiple = FALSE),
                                                   p("Theme changes will take effect upon reloading the app"),
                                                   actionButton(inputId = "settings.update", label = "Update")
                                                 )),
                                          column(4,
                                                 wellPanel(
                                                   h3("Add Expense/Deposit Category"), 
                                                   hr(),
                                                   textOutput(outputId = "settings.current_categories"),
                                                   textInput(inputId = "settings.new_category_name", label = "Enter New Category"),
                                                   p("Categories must be added one at a time. If a new expense/deposit is not recorded, the category will not be remembered."),
                                                   actionButton(inputId = "settings.add_category", label = "Add Category")
                                                 )),
                                          column(4,
                                                 wellPanel(
                                                   h3("Download Database Records"),
                                                   hr(),
                                                   downloadButton(outputId = "settings.DL.all_records", label = "All Records"),
                                                   downloadButton(outputId = "settings.DL.expenses", label = "Expenses"),
                                                   downloadButton(outputId = "settings.DL.deposits", label = "Deposits")
                                                 ),
                                                 wellPanel(
                                                   h3("Upload to Database Records"),
                                                   hr(),
                                                   p("Download the template below to format your data to upload"),
                                                   downloadButton(outputId = "settings.DL.upload_template", label = "Template"),
                                                   hr(),
                                                   fileInput("settings.upload.new_records", label = "Upload (from template)", multiple = FALSE),
                                                   textOutput("settings.upload.new_records_status")
                                                 ))
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
                       column(4, offset = 4, actionButton(inputId = "login", label = "Login", width = "100%")),
                       column(4, offset = 4, actionLink(inputId = "create_login", label = "Create login", align = "center")),
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
  
  ## Overview
  user.net_cash <- reactive({
    update_datatables()
    df <- DB_get_records(usr = input$username)
    total_expenses <- sum(df[df$class=="Expense", "conv_amount"])
    total_deposits <- sum(df[df$class=="Deposit", "conv_amount"])
    return(total_deposits-total_expenses)
  })
  user.weeks_remaining <- reactive({
    validate(need(!is.null(cycle_end()), message = "Add Cycle Dates in Settings"))
    round((as_date(cycle_end()) - today())/7, digits = 1)
  })
  user.weekly_net_cash <- reactive({
    
    if(user.net_cash() < 0){ # If user has no positive net cash, the weekly cash going forward is 0
      return(0)
    }

    if(user.weeks_remaining() > 1){
      return(user.net_cash() / as.numeric(user.weeks_remaining()))
    } else {
      return(user.net_cash())
    }
  })
  user.weekly_net_fx <- reactive({
    user.weekly_net_cash() / get_FX_rate(date = today(), currency = user.fx_currency())
  })
  
  # Sidebar
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
  output$user.cycle_start <- renderText({
    validate(need(!is.null(cycle_start()), message = "Add Cycle Dates in Settings"))
    as.character(as_date(cycle_start()))
    })
  output$user.cycle_end <- renderText({
    validate(need(!is.null(cycle_end()), message = "Add Cycle Dates in Settings"))
    as.character(as_date(cycle_end()))
    })
  
  # Main Panel
  data.expense_table <- reactive({
    update_datatables()
    validate(
      need(!is.null(cycle_start()), "Add Cycle Dates in Settings"),
      need(!is.null(cycle_end()), "Add Cycle Dates in Settings"),
      need(nrow(DB_get_expense(user())) > 0, "No Expenses Recorded")
    )
    input$expense_entry.add
    summarise_expenses(start_date = cycle_start(), end_date = cycle_end(), expense_tbl = DB_get_expense(user()))
  })
  plot.overview <- reactive({
    input$expense_entry.add
    #selected_rows <- input$data.expense_table_rows_selected
    get_overview_plot(df = data.expense_table(), start_date = cycle_start(), end_date = cycle_end())
  })
  
  output$overview.expense_table <- renderDataTable({
    datatable(data.expense_table(), rownames = FALSE) %>%
      formatCurrency(colnames(data.expense_table())[-1], currency = "$", digits = 2) %>%
      formatStyle("total", backgroundColor = "lightgray")
  })
  output$overview.historical_spending <- renderPlotly({
    plot.overview()[[input$overview.plot_type]]
  })
  
  observe({
    print(paste("Selected Rows:", input$expense.history_table_rows_selected, collapse = ",", sep = " "))
    print(paste("Last Selected:", input$expense.history_table_row_last_clicked))
  })
  
  ## Expenses
  exp_tbl <- reactive({
    update_datatables()
    DB_get_expense(usr = input$username)
  })
  exp_tbl_selected <- reactive({
    selected_rows <- input$expense.history_table_rows_selected
    exp_tbl()[selected_rows,]
  })
  exp_tbl_last_selected <- reactive({
    selected_rows <- input$expense.history_table_row_last_clicked
    exp_tbl()[selected_rows,]
  })
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
  output$expense_modify.status <- eventReactive(input$expense_modify.add, {
    validate(
      need(input$expense_modify.amount, "Missing 'Amount' input"),
      need(input$expense_modify.category, "Missing 'Category' input"),
      need(input$expense_modify.comment, "Missing 'Comments' input"),
      need(input$expense_modify.amount > 0, "'Amount' should be positive")
    )
    
    if(input$expense_modify.currency == "USD"){
      conversion_factor <- 1
    } else {
      conversion_factor <- get_FX_rate(as_date(input$expense_modify.date))
    }
    
    converted_amt <- conversion_factor * input$expense_modify.amount
    
    DB_modify_record(usr = input$username, 
                   id = input$expense.history_table_row_last_clicked, 
                   dt = input$expense_modify.date, 
                   clss = "Expense",
                   amt = input$expense_modify.amount, 
                   curr = input$expense_modify.currency, 
                   cnv_amt = converted_amt, 
                   cnv_curr = "USD", 
                   rate = conversion_factor, 
                   ctgry = input$expense_modify.category, 
                   cmnts = input$expense_modify.comment)
    
    return("Expense Updated ")
  })
  output$expense.history_table <- renderDataTable({
    datatable(exp_tbl(), rownames = FALSE, 
              options=list(columnDefs = list(list(visible=FALSE, targets= c(2))),
                           order = list(list(0, 'desc')))) %>% 
      formatDate("date") %>%
      formatRound(c("amount", "conv_amount"), digits = 2) %>%
      formatRound("fx_rate", digits = 4) %>%
      formatCurrency("conv_amount")
    })
  output$expense.history_table_selected <- renderDataTable({
    datatable(exp_tbl_last_selected(), rownames = FALSE, 
              options=list(columnDefs = list(list(visible=FALSE, targets= c(0,2))),
                           order = list(list(0, 'desc')))) %>% 
      formatDate("date") %>%
      formatRound(c("amount", "conv_amount"), digits = 2) %>%
      formatRound("fx_rate", digits = 4) %>%
      formatCurrency("conv_amount")
  })
  output$expense.history_table_last_selected <- renderDataTable({
    datatable(exp_tbl_last_selected(), rownames = FALSE, 
              options=list(columnDefs = list(list(visible=FALSE, targets= c(0,2))),
                           order = list(list(0, 'desc')))) %>% 
      formatDate("date") %>%
      formatRound(c("amount", "conv_amount"), digits = 2) %>%
      formatRound("fx_rate", digits = 4) %>%
      formatCurrency("conv_amount")
  })
  
  ## Savings
  sav_tbl <- reactive({
    update_datatables()
    DB_get_deposit(usr = input$username)
  })
  sav_tbl_selected <- reactive({
    selected_rows <- input$savings.history_table_rows_selected
    sav_tbl()[selected_rows,]
  })
  sav_tbl_last_selected <- reactive({
    selected_rows <- input$savings.history_table_row_last_clicked
    sav_tbl()[selected_rows,]
  })
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
  output$savings_modify.status <- eventReactive(input$savings_modify.add, {
    validate(
      need(input$savings_modify.amount, "Missing 'Amount' input"),
      need(input$savings_modify.category, "Missing 'Category' input"),
      need(input$savings_modify.comment, "Missing 'Comments' input"),
      need(input$savings_modify.amount > 0, "'Amount' should be positive")
    )
    
    DB_modify_record(usr = input$username, 
                   id = input$savings.history_table_row_last_clicked,  
                   dt = input$savings_modify.date, 
                   clss = "Deposit",
                   amt = input$savings_modify.amount, 
                   curr = "USD", 
                   cnv_amt = input$savings_modify.amount, 
                   cnv_curr = "USD", rate = 1, 
                   ctgry = input$savings_modify.category, 
                   cmnts = input$savings_modify.comment)
    
    return("Deposit Updated")
  })
  output$savings.history_table <- renderDataTable({
    datatable(sav_tbl(), rownames = FALSE, 
              options=list(columnDefs = list(list(visible=FALSE, targets= c(2) )))) %>% 
      formatDate("date") %>%
      formatRound(c("amount", "conv_amount"), digits = 2) %>%
      formatRound("fx_rate", digits = 4) %>%
      formatCurrency("conv_amount")
    })
  output$savings.history_table_selected <- renderDataTable({
    datatable(sav_tbl_selected(), rownames = FALSE, 
              options=list(columnDefs = list(list(visible=FALSE, targets= c(0,2) )))) %>% 
      formatDate("date") %>%
      formatRound(c("amount", "conv_amount"), digits = 2) %>%
      formatRound("fx_rate", digits = 4) %>%
      formatCurrency("conv_amount")
  })
  output$savings.history_table_last_selected <- renderDataTable({
    datatable(sav_tbl_last_selected(), rownames = FALSE, 
              options=list(columnDefs = list(list(visible=FALSE, targets= c(0,2) )))) %>% 
      formatDate("date") %>%
      formatRound(c("amount", "conv_amount"), digits = 2) %>%
      formatRound("fx_rate", digits = 4) %>%
      formatCurrency("conv_amount")
  })
  
  ## Menu
  output$settings.current_categories <- renderText({
    paste("Current Categories:", paste(user.categories(), collapse = ", "))
  })
  output$settings.DL.all_records <- downloadHandler(    filename = function() {
      paste(user(),"all_records.csv", sep = "_")},    content = function(file) {
      write.csv(x = DB_get_records(usr = user()), file, row.names = FALSE)})
  output$settings.DL.expenses <- downloadHandler(    filename = function() {
      paste(user(),"expenses.csv", sep = "_")},    content = function(file) {
      write.csv(x = DB_get_expense(usr = user()), file, row.names = FALSE)})
  output$settings.DL.deposits <- downloadHandler(    filename = function() {
      paste(user(),"deposits.csv", sep = "_")},    content = function(file) {
      write.csv(x = DB_get_deposit(usr = user()), file, row.names = FALSE)})
  output$settings.DL.upload_template <- downloadHandler(filename = "template.csv", content = function(file){
      df <- data.frame(date = "Date in the format of YYYY-MM-DD",
                       class = "transaction is Expense or Deposit",
                       amount = "A numeric",
                       currency = "Currently only USD and EUR are supported",
                       category = "Describe the transaction in one word (food, drinks, transit, etc.)",
                       comments = "Delete this row before uploading.")
      write.csv(x = df, file, row.names = FALSE)
    })
  
  new_transactions <- reactive({
    inFile <- input$settings.upload.new_records
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    df <- df[complete.cases(df),]
    
    print(df)
    
    validate(
      need(
        all(colnames(df) %in% c("date", "class", "amount", "currency", "category", "comments")), 
        message = "File columns are invalid"),
      need(
        !any(sapply(names(sapply(as.character(df$date), as_date)), is.na)),  # Apply date to all strings, checks if results are NA. If any NAs then invalid
        message = "Date formats are invalid"
      ),
      need(
        all(df$class %in% c("Expense", "Deposit")),
        message = "Class formats are invalid"
      ),
      need(
        !any(sapply(as.numeric(df$amount), is.na)), # Apply numeric to all strings, if any are NA then invalid
        message = "Amount formats are invalid"
      ),
      need(
        all(df$currency %in% c("USD", "EUR")),
        message = "Currency formats are invalid"
      )
    )
    
    # Format existing columns
    df$date <- as_date(df$date)
    df$amount <- as.numeric(df$amount)

    # Initialize necessary DB columns
    df$conv_amount <- df$amount             # Assume that Converted Amount = Original Amount
    df$fx_rate <- 1                         # Set all fx rates to 1
    df$conv_currency <- user.base_currency()# Set "Converted to" currency as the user's base currency
    
    # Extract transactions for which the currency is NOT the user's base currency (we need to grab fx rates for these transactions)
    conv_date_amt <- df[df$currency != user.base_currency(), c("date", "currency", "amount")] 
    
    # Check if fx_rates need to be retrieved. Number of rows will be 0 if all transactions are in the base currency
    if(nrow(conv_date_amt) > 0){
      conv_date_amt$fx_rate <- apply(X = conv_date_amt, MARGIN = 1, FUN = get_FX_rate) # Grab the fx rate for each transaction
      conv_date_amt$conv_amt <- conv_date_amt$fx_rate * conv_date_amt$amount           # Convert amount to base currency
      
      # Add data to transactions in foreeign currencies
      df$conv_amount[df$currency != user.base_currency(), c("conv_amount", "fx_rate")] <- conv_date_amt[,c("conv_amt", "fx_rate")]
      
    }
    
    df$date <- as.character(df$date)
    
    DB_upload_df(df, tblname = user()) #Upload data
    
    return("Success")
    
  }) # Handles data uploaded via .CSV
  output$settings.upload.new_records_status <- renderText({new_transactions()})
  
  # Modals ------------------------------------------------------------------
  modal.expense_entry <- reactive({
    modalDialog(title = "Add Expense", size = "l", easyClose = T,
                fluidPage(
                  fluidRow(
                    column(5, selectInput(inputId = "expense_entry.currency", label = "Currency",
                                          choices = c("USD", "EUR"), selected = "USD", multiple = FALSE)),
                    column(7, numericInput(inputId = "expense_entry.amount", label = "Amount",
                                           value = NULL, min = 0))
                  ),
                  fluidRow(
                    column(5, dateInput(inputId = "expense_entry.date", label = "Date", max = today(),
                                        value = today(), weekstart = 1)),
                    column(7, selectInput(inputId = "expense_entry.category", label = "Category", choices = user.categories(), multiple = FALSE))
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
                ))})
  modal.savings_entry <- reactive({
    modalDialog(title = "Add Deposit", size = "l", easyClose = T,
                fluidPage(
                  h4("Add Deposit", align = "center"),
                  hr(),
                  fluidRow(
                    column(5, textOutput("user.base_currency")),
                    column(7, numericInput(inputId = "savings_entry.amount", label = "Amount",
                                           value = 0, min = 0))
                  ),
                  fluidRow(
                    column(5, dateInput(inputId = "savings_entry.date", label = "Date", max = today(),
                                        value = today(), weekstart = 1)),
                    column(7, selectInput(inputId = "savings_entry.category", label = "Category", choices = user.categories(), multiple = FALSE))
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
  })
  modal.expense_remove <- reactive({
    modalDialog(title = "Delete Selected Expenses", size = "l", easyClose = T,
                fluidPage(
                  fluidRow(
                    dataTableOutput("expense.history_table_selected"),
                    br(),
                    strong("Delete these transactions?", style = "color:red"),
                    actionButton("expense.delete_confirm", "Yes")
                  )
                )
    )
  })
  modal.savings_remove <- reactive({
    modalDialog(title = "Delete Selected Deposits", size = "l", easyClose = T,
                fluidPage(
                  fluidRow(
                    dataTableOutput("savings.history_table_selected"),
                    br(),
                    strong("Delete these transactions?", style = "color:red"),
                    actionButton("savings.delete_confirm", "Yes")
                  )
                )
    )
  })
  modal.expense_edit <- reactive({
    modalDialog(title = "Modify Expense", size = "l", easyClose = T,
                fluidPage(
                  fluidRow(
                    h3("Current Entry:"),
                    dataTableOutput("expense.history_table_last_selected")
                  ),
                  fluidRow(
                    h3("Modified Entry:"),
                    column(5, selectInput(inputId = "expense_modify.currency", label = "Currency",
                                          choices = c("USD", "EUR"), selected = "USD", multiple = FALSE)),
                    column(5, numericInput(inputId = "expense_modify.amount", label = "Amount",
                                           value = NULL, min = 0))
                  ),
                  fluidRow(
                    column(5, dateInput(inputId = "expense_modify.date", label = "Date", max = today(),
                                        value = today(), weekstart = 1)),
                    column(5, selectInput(inputId = "expense_modify.category", label = "Category", choices = user.categories(), multiple = FALSE))
                  ),
                  fluidRow(
                    column(12,
                           textAreaInput(inputId = "expense_modify.comment", label = "Comments", resize = "vertical"),
                           hr(),
                           actionButton(inputId = "expense_modify.add", label = "Submit"),
                           hr(),
                           textOutput(outputId = "expense_modify.status")
                    )
                  )
                ))
  })
  modal.savings_edit <- reactive({
    modalDialog(title = "Modify Deposit", size = "l", easyClose = T,
                fluidPage(
                  fluidRow(
                    h3("Current Entry:"),
                    dataTableOutput("savings.history_table_last_selected")
                  ),
                  fluidRow(
                    h3("Modified Entry:"),
                    column(5, selectInput(inputId = "savings_modify.currency", label = "Currency",
                                          choices = c("USD", "EUR"), selected = "USD", multiple = FALSE)),
                    column(5, numericInput(inputId = "savings_modify.amount", label = "Amount",
                                           value = NULL, min = 0))
                  ),
                  fluidRow(
                    column(5, dateInput(inputId = "savings_modify.date", label = "Date", max = today(),
                                        value = today(), weekstart = 1)),
                    column(5, selectInput(inputId = "savings_modify.category", label = "Category", choices = user.categories(), multiple = FALSE))
                  ),
                  fluidRow(
                    column(12,
                           textAreaInput(inputId = "savings_modify.comment", label = "Comments", resize = "vertical"),
                           hr(),
                           actionButton(inputId = "savings_modify.add", label = "Submit"),
                           hr(),
                           textOutput(outputId = "savings_modify.status")
                    )
                  )
                ))
  })
})







