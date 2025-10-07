library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

data_file <- "Monthly_expense_data.csv"

# Initialize CSV if missing
if (!file.exists(data_file)) {
  write.csv(data.frame(
    Date = character(),
    Month = character(),
    Category = character(),
    Subcategory = character(),
    Expense = numeric()
  ), data_file, row.names = FALSE)
}

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
      .title-text { font-size: 24px; font-weight: bold; }
      .btn-row { display: flex; gap: 10px; align-items: center; margin-bottom: 10px; }
    "))
  ),
  
  titlePanel(div("💰 Monthly Expense Analysis", class = "title-text")),
  
  fluidRow(
    column(12,
           div(class = "btn-row",
               actionButton("open_modal", HTML("<b style='font-size:20px;'>+</b>"), 
                            class = "btn-success", style = "font-size:20px; width:45px; height:45px; border-radius:50%;"),
               actionButton("delete_mode_btn", HTML("🗑"), 
                            class = "btn-danger", style = "font-size:20px; width:45px; height:45px; border-radius:50%;")
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
  
  sheet_data <- reactive({
    data_trigger()
    read.csv(data_file, stringsAsFactors = FALSE) %>%
      mutate(
        Date = as.Date(Date),
        Month = format(as.Date(Date), "%b"),
        MonthYear = format(as.Date(Date), "%b %Y"),
        MonthYearDate = as.Date(format(as.Date(Date), "%Y-%m-01")),
        Expense = as.numeric(Expense)
      )
  })
  
  categories <- reactiveVal(default_categories)
  
  current_month_year <- format(Sys.Date(), "%b %Y")
  
  observe({
    months <- sheet_data() %>%
      arrange(MonthYearDate) %>%
      distinct(MonthYear) %>%
      pull(MonthYear)
    
    selected_month <- if (current_month_year %in% months) current_month_year else months[length(months)]
    
    updateSelectInput(session, "month_select_cat", choices = months, selected = selected_month)
    updateSelectInput(session, "month_select_subcat", choices = months, selected = selected_month)
  })
  
  output$sheet_data <- renderDT({
    datatable(sheet_data() %>%
                select(Date, Month, Category, Subcategory, Expense),
              options = list(pageLength = 8, scrollX = TRUE),
              class = "cell-border stripe hover compact")
  })
  
  observeEvent(input$open_modal, {
    showModal(modalDialog(
      title = "Add New Data",
      dateInput("date", "Date", value = Sys.Date()),
      selectizeInput("category", "Category", choices = categories(),
                     options = list(create = TRUE), width = "100%"),
      textInput("subcategory", "Subcategory"),
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
      Date = as.character(as.Date(input$date)),
      Month = format(as.Date(input$date), "%b"),
      Category = new_category,
      Subcategory = input$subcategory,
      Expense = as.numeric(input$expense),
      stringsAsFactors = FALSE
    )
    
    all_data <- read.csv(data_file, stringsAsFactors = FALSE)
    all_data <- rbind(all_data, new_row)
    write.csv(all_data, data_file, row.names = FALSE)
    removeModal()
    data_trigger(data_trigger() + 1)
    showNotification("✅ Data added successfully!", type = "message")
  })
  
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
      all_data <- read.csv(data_file, stringsAsFactors = FALSE)
      all_data <- all_data[-selected, ]
      write.csv(all_data, data_file, row.names = FALSE)
      data_trigger(data_trigger() + 1)
      removeModal()
      showNotification("🗑️ Data deleted successfully!", type = "message")
    } else {
      showNotification("⚠️ No rows selected for deletion.", type = "error")
    }
  })
  
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
  
  output$subcategory_plot_ui <- renderUI({
    tags$div(style = "min-width: 900px;", plotlyOutput("subcategory_plot"))
  })
  
  output$subcategory_plot <- renderPlotly({
    req(input$month_select_subcat)
    
    subcat_count <- sheet_data() %>%
      filter(MonthYear == input$month_select_subcat) %>%
      distinct(Subcategory) %>%
      nrow()
    plot_height <- max(400, subcat_count * 25)
    
    subcat_exp <- sheet_data() %>%
      filter(MonthYear == input$month_select_subcat) %>%
      group_by(Subcategory) %>%
      summarise(TotalExpense = sum(Expense, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalExpense))
    
    p <- ggplot(subcat_exp, aes(x = TotalExpense, 
                                y = reorder(Subcategory, TotalExpense),
                                text = paste("Subcategory:", Subcategory, "<br>Total Expense:", TotalExpense))) +
      geom_col(fill = "#2c7fb8") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    p %>% ggplotly(height = plot_height, tooltip = "text")
  })
}

shinyApp(ui, server)
