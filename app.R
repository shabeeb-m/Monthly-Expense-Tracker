library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(googlesheets4)

# Authenticate with your Google account
gs4_auth(path = "shiny-app-474907-7b95793f24b7.json") 
sheet_url <- "https://docs.google.com/spreadsheets/d/1hhMqjUvVHiDPGZ-Xpc9gtJi_F_2oQraD68kFSng2kc8/edit#gid=75237655"
sheet_tab <- "Daily Expenses"  # <-- exact tab name

default_categories <- c(
  "Outside Food", "Personal", "Health", "To Home", "Meat & Seafood",
  "Groceries", "Leisure", "Finance", "Transport", "Sweets", "Utility"
)

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { font-size: 15px; padding: 8px; }
      .plotly { height: 400px !important; }
      .dataTables_wrapper { font-size: 13px; }
      .shiny-input-container { width: 100% !important; }
      .selectize-input { font-size: 14px; }
      .title-text { font-size: 22px; font-weight: bold; }
      .btn-row { display: flex; gap: 10px; align-items: center; margin-bottom: 10px; }
      .btn-icon { font-size: 20px; width: 45px; height: 45px; border-radius: 50%; padding:0; }
    "))
  ),
  
  titlePanel(div("💰 Monthly Expense Analysis", class = "title-text")),
  
  fluidRow(
    column(12,
           div(class = "btn-row",
               actionButton("open_modal", HTML("<b style='font-size:20px;'>+</b>"), class = "btn-success btn-icon"),
               actionButton("delete_mode_btn", HTML("🗑"), class = "btn-danger btn-icon"),
               downloadButton("download_data", label = NULL, class = "btn-primary btn-icon", icon = icon("download"))
           )
    )
  ),
  
  tabsetPanel(
    id = "tabs",
    type = "tabs",
    tabPanel("📋 Data Table", br(), DTOutput("sheet_data")),
    tabPanel("📊 Monthly Expense", br(), plotlyOutput("expense_plot", height = "400px")),
    tabPanel("📦 Category Expense",
             br(),
             selectInput("month_select_cat", "Select Month-Year", choices = NULL, width = "100%"),
             plotlyOutput("category_plot", height = "400px")),
    tabPanel("📂 Sub-Category Expense",
             br(),
             selectInput("month_select_subcat", "Select Month-Year", choices = NULL, width = "100%"),
             tags$div(style = "overflow-x: auto; width: 100%;", uiOutput("subcategory_plot_ui")))
  )
)

server <- function(input, output, session) {
  
  data_trigger <- reactiveVal(0)
  
  # Read Google Sheet
  sheet_data <- reactive({
    data_trigger()
    df <- read_sheet(sheet_url, sheet = sheet_tab)
    # Create MonthYear and MonthYearDate only in code
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
  })
  
  # Render DataTable
  output$sheet_data <- renderDT({
    datatable(sheet_data() %>%
                select(Date, Month, Category, `Sub-Category`, Expense),
              options = list(pageLength = 8, scrollX = TRUE),
              class = "cell-border stripe hover compact")
  })
  
  # Add new data modal
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
  
  # Add data to Google Sheet
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
  
  # Delete mode modal
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
  
  # Delete selected rows from Google Sheet
  observeEvent(input$confirm_delete_btn, {
    selected <- input$delete_table_rows_selected
    if (length(selected) > 0) {
      all_data <- sheet_data() %>%
        select(Date, Month, Category, `Sub-Category`, Expense)  # Keep only original columns
      all_data <- all_data[-selected, ]
      # Write back to correct tab
      sheet_write(all_data, ss = sheet_url, sheet = sheet_tab)
      data_trigger(data_trigger() + 1)
      removeModal()
      showNotification("🗑️ Data deleted successfully!", type = "message")
    } else {
      showNotification("⚠️ No rows selected for deletion.", type = "error")
    }
  })
  
  # Download CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Monthly_expense_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sheet_data()[ , c("Date", "Month", "Category", "Sub-Category", "Expense")],
                file, row.names = FALSE)
    }
  )
  
  # Monthly expense plot
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
  
  # Category plot
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
  
  # Subcategory plot UI
  output$subcategory_plot_ui <- renderUI({
    tags$div(style = "min-width: 900px;", plotlyOutput("subcategory_plot"))
  })
  
  # Subcategory plot
  output$subcategory_plot <- renderPlotly({
    req(input$month_select_subcat)
    
    subcat_count <- sheet_data() %>%
      filter(MonthYear == input$month_select_subcat) %>%
      distinct(`Sub-Category`) %>%
      nrow()
    plot_height <- max(400, subcat_count * 25)
    
    subcat_exp <- sheet_data() %>%
      filter(MonthYear == input$month_select_subcat) %>%
      group_by(`Sub-Category`) %>%
      summarise(TotalExpense = sum(Expense, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalExpense))
    
    p <- ggplot(subcat_exp, aes(x = TotalExpense, 
                                y = reorder(`Sub-Category`, TotalExpense),
                                text = paste("Sub-Category:", `Sub-Category`, "<br>Total Expense:", TotalExpense))) +
      geom_col(fill = "#2c7fb8") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    p %>% ggplotly(height = plot_height, tooltip = "text")
  })
}

shinyApp(ui, server)
