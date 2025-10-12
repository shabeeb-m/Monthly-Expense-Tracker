library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(googlesheets4)

# Authenticate Google Sheets
gs4_auth(path = "shiny-app-474907-7b95793f24b7.json") 
sheet_url <- "https://docs.google.com/spreadsheets/d/1hhMqjUvVHiDPGZ-Xpc9gtJi_F_2oQraD68kFSng2kc8/edit#gid=75237655"
sheet_tab <- "Daily Expenses"

default_categories <- c(
  "Outside Food", "Personal", "Health", "To Home", "Meat & Seafood",
  "Groceries", "Leisure", "Finance", "Transport", "Sweets", "Utility"
)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "💰 Expense Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Monthly Expense", tabName = "monthly_exp", icon = icon("calendar")),
      menuItem("Category Expense", tabName = "category_exp", icon = icon("boxes")),
      menuItem("Sub-Category Expense", tabName = "subcat_exp", icon = icon("folder")),
      menuItem("Household Expenses", tabName = "home_exp", icon = icon("home")),
      menuItem("Personal Expense", tabName = "personal_exp", icon = icon("user")),
      menuItem("Loan / EMI", tabName = "emi_exp", icon = icon("credit-card"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { padding: 15px; }
        .btn-icon { font-size: 20px; width: 45px; height: 45px; border-radius: 50%; padding:0; margin-right:5px; }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('update_total_spend', function(value) {
          document.getElementById('personal_total_text').innerHTML = value;
        });
        Shiny.addCustomMessageHandler('total_home_spend', function(value) {
          document.getElementById('total_home_text').innerHTML = value;
        });
      "))
    ),
    
    tabItems(
      # ---------------- Data Table ----------------
      tabItem(tabName = "data_table",
              fluidRow(
                column(12,
                       div(style="display:flex; gap:10px; margin-bottom:10px;",
                           actionButton("open_modal", HTML("<b style='font-size:20px;'>+</b>"), class="btn-success btn-icon"),
                           actionButton("delete_mode_btn", HTML("🗑"), class="btn-danger btn-icon"),
                           downloadButton("download_data", label=NULL, class="btn-primary btn-icon", icon = icon("download"))
                       )
                )
              ),
              DTOutput("sheet_data")
      ),
      
      # ---------------- Monthly Expense ----------------
      tabItem(tabName = "monthly_exp",
              plotlyOutput("expense_plot", height = "400px")
      ),
      
      # ---------------- Category Expense ----------------
      tabItem(tabName = "category_exp",
              selectInput("month_select_cat", "Select Month-Year", choices = NULL, width = "100%"),
              plotlyOutput("category_plot", height = "400px")
      ),
      
      # ---------------- Sub-Category Expense ----------------
      tabItem(tabName = "subcat_exp",
              selectInput("month_select_subcat", "Select Month-Year", choices = NULL, width = "100%"),
              plotlyOutput("subcategory_plot", height = "400px")
      ),
      
      # ---------------- Household Expenses ----------------
      tabItem(tabName = "home_exp",
              selectInput("month_select_home", "Select Month-Year", choices = NULL, width = "100%"),
              tags$div(style="font-size:18px; font-weight:bold; margin-bottom:10px;",
                       HTML("💵 Total Household Spend: <span id='total_home_text'></span>")
              ),
              plotlyOutput("home_plot_ui", height = "400px")
      ),
      
      # ---------------- Personal Expenses ----------------
      tabItem(tabName = "personal_exp",
              selectInput("month_select_personal", "Select Month-Year", choices = NULL, width = "100%"),
              tags$div(style="font-size:18px; font-weight:bold; margin-bottom:10px;",
                       HTML("💵 Total Personal Spend: <span id='personal_total_text'></span>")
              ),
              plotlyOutput("personal_plot", height = "400px")
      ),
      
      # ---------------- Loan / EMI ----------------
      tabItem(tabName = "emi_exp",
              DTOutput("emi_data")
      )
    )
  )
)

server <- function(input, output, session) {
  
  data_trigger <- reactiveVal(0)
  
  # Read Google Sheet
  sheet_data <- reactive({
    data_trigger()
    df <- read_sheet(sheet_url, sheet = sheet_tab)
    df <- df %>%
      mutate(
        Date = as.Date(Date),
        Month = format(Date, "%b"),
        MonthYear = format(Date, "%b %Y"),
        MonthYearDate = as.Date(format(Date, "%Y-%m-01")),
        Expense = as.numeric(Expense)
      )
    df
  })
  
  categories <- reactiveVal(default_categories)
  current_month_year <- format(Sys.Date(), "%b %Y")
  
  # Update month selectors
  observe({
    months <- sheet_data() %>%
      arrange(MonthYearDate) %>%
      distinct(MonthYear) %>%
      pull(MonthYear)
    
    selected_month <- if (current_month_year %in% months) current_month_year else months[length(months)]
    
    updateSelectInput(session, "month_select_cat", choices = months, selected = selected_month)
    updateSelectInput(session, "month_select_subcat", choices = months, selected = selected_month)
    updateSelectInput(session, "month_select_home", choices = months, selected = selected_month)
    updateSelectInput(session, "month_select_personal", choices = months, selected = selected_month)
  })
  
  # ---------------- Data Table ----------------
  output$sheet_data <- renderDT({
    datatable(sheet_data() %>%
                select(Date, Month, Category, `Sub-Category`, Expense),
              options = list(pageLength = 8, scrollX = TRUE),
              class = "cell-border stripe hover compact")
  })
  
  # ---------------- EMI Data ----------------
  output$emi_data <- renderDT({
    datatable(sheet_data() %>% 
                filter(`Sub-Category` %in% c("Skillovilla EMI","Dental","Inverter")) %>% 
                group_by(`Sub-Category`) %>% 
                summarise(Paid = sum(Expense, na.rm = TRUE), .groups = "drop") %>% 
                mutate(Due = c(23000, 20000, 40539),
                       Item = `Sub-Category`,
                       Remaining = Due - Paid) %>% 
                select(Item, Due, Paid, Remaining) %>% 
                arrange(desc(Due)),
              options = list(pageLength = 8, scrollX = TRUE),
              class = "cell-border stripe hover compact")
  })
  
  # ---------------- Add Modal ----------------
  observeEvent(input$open_modal, {
    showModal(modalDialog(
      title = "Add New Data",
      dateInput("date", "Date", value = Sys.Date()),
      selectizeInput("category", "Category", choices = categories(),
                     options = list(create = TRUE), width = "100%"),
      textInput("subcategory", "Sub-Category"),
      numericInput("expense", "Expense", value = "", min = 0),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("add_btn_modal", "Add Data", class = "btn-success")
      ),
      size = "m", easyClose = TRUE
    ))
  })
  
  observeEvent(input$add_btn_modal, {
    new_category <- input$category
    if (!(new_category %in% categories())) {
      categories(c(categories(), new_category))
    }
    new_row <- data.frame(
      Date = as.Date(input$date),
      Month = format(as.Date(input$date), "%b"),
      Category = new_category,
      `Sub-Category` = input$subcategory,
      Expense = as.numeric(input$expense),
      stringsAsFactors = FALSE
    )
    sheet_append(sheet_url, new_row, sheet = sheet_tab)
    removeModal()
    data_trigger(data_trigger() + 1)
    showNotification("✅ Data added successfully!", type = "message")
  })
  
  # ---------------- Delete Data ----------------
  observeEvent(input$delete_mode_btn, {
    showModal(modalDialog(
      title = "Select Data to Delete",
      DTOutput("delete_table"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_btn", "Confirm Delete", class = "btn-danger")
      ),
      size = "l", easyClose = TRUE
    ))
  })
  
  output$delete_table <- renderDT({
    datatable(sheet_data(),
              selection = "multiple",
              options = list(pageLength = 8, scrollX = TRUE))
  })
  
  observeEvent(input$confirm_delete_btn, {
    selected <- input$delete_table_rows_selected
    if (length(selected) > 0) {
      all_data <- sheet_data() %>%
        select(Date, Month, Category, `Sub-Category`, Expense)
      all_data <- all_data[-selected, ]
      sheet_write(all_data, ss = sheet_url, sheet = sheet_tab)
      data_trigger(data_trigger() + 1)
      removeModal()
      showNotification("🗑️ Data deleted successfully!", type = "message")
    } else {
      showNotification("⚠️ No rows selected for deletion.", type = "error")
    }
  })
  
  # ---------------- Download ----------------
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Monthly_expense_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sheet_data()[ , c("Date", "Month", "Category", "Sub-Category", "Expense")],
                file, row.names = FALSE)
    }
  )
  
  # ---------------- Monthly Plot ----------------
  output$expense_plot <- renderPlotly({
    month_exp <- sheet_data() %>%
      group_by(MonthYear, MonthYearDate) %>%
      summarise(TotalExpense = sum(Expense, na.rm = TRUE), .groups = "drop") %>%
      arrange(MonthYearDate)
    
    p <- ggplot(month_exp, aes(x = MonthYearDate, y = TotalExpense,
                               text = paste("Month:", MonthYear, "<br>Total:", TotalExpense))) +
      geom_col(fill = "#0072B2") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # ---------------- Category Plot ----------------
  output$category_plot <- renderPlotly({
    req(input$month_select_cat)
    cat_exp <- sheet_data() %>%
      filter(MonthYear == input$month_select_cat) %>%
      group_by(Category) %>%
      summarise(TotalExpense = sum(Expense, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalExpense))
    
    p <- ggplot(cat_exp, aes(x = reorder(Category, TotalExpense), y = TotalExpense,
                             text = paste("Category:", Category, "<br>Total:", TotalExpense))) +
      geom_col(fill = "#009E73") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # ---------------- Subcategory Plot ----------------
  output$subcategory_plot <- renderPlotly({
    req(input$month_select_subcat)
    subcat_exp <- sheet_data() %>%
      filter(MonthYear == input$month_select_subcat) %>%
      group_by(`Sub-Category`) %>%
      summarise(TotalExpense = sum(Expense, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalExpense))
    
    p <- ggplot(subcat_exp, aes(x = TotalExpense, 
                                y = reorder(`Sub-Category`, TotalExpense),
                                text = paste("Sub-Category:", `Sub-Category`, "<br>Total:", TotalExpense))) +
      geom_col(fill = "#2c7fb8") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    ggplotly(p, height = max(400, nrow(subcat_exp)*25), tooltip = "text")
  })
  
  # ---------------- Household Plot ----------------
  output$home_plot_ui <- renderPlotly({
    req(input$month_select_home)
    home_exp <- sheet_data() %>%
      filter(MonthYear == input$month_select_home,
             Category %in% c("To Home","Meat & Seafood","Groceries","Utility")) %>%
      group_by(Category) %>%
      summarise(TotalExpense = sum(Expense, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalExpense))
    
    home_spend <- sum(home_exp$TotalExpense)
    session$sendCustomMessage("total_home_spend",
                              paste0("<b style='color:#1b5e20;'>₹ ",
                                     format(home_spend, big.mark=","), "</b>"))
    
    p <- ggplot(home_exp, aes(x = reorder(Category, TotalExpense),
                              y = TotalExpense,
                              text = paste("Category:", Category, "<br>Total:", TotalExpense))) +
      geom_col(fill = "#009E73") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  # ---------------- Personal Plot ----------------
  output$personal_plot <- renderPlotly({
    req(input$month_select_personal)
    personal_exp <- sheet_data() %>%
      filter(MonthYear == input$month_select_personal,
             Category %in% c("Outside Food","Personal","Transport","Sweets","Leisure")) %>%
      group_by(Category) %>%
      summarise(TotalExpense = sum(Expense, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalExpense))
    
    Personal_spend <- sum(personal_exp$TotalExpense)
    session$sendCustomMessage("update_total_spend",
                              paste0("<b style='color:#1b5e20;'>₹ ",
                                     format(Personal_spend, big.mark=","), "</b>"))
    
    p <- ggplot(personal_exp, aes(x = reorder(Category, TotalExpense),
                                  y = TotalExpense,
                                  text = paste("Category:", Category, "<br>Total:", TotalExpense))) +
      geom_col(fill = "#009E73") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
}

shinyApp(ui, server)
