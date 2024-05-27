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
library(readxl)
library(leaflet)
library(purrr)
library(gtsummary)
library(gt)
library(DT)
library(plotly)
library(highcharter)
library(scales)
library(ggplot2)
library(ggrepel)
library(shinydashboard)

##### Определяем интерфейс #####
ui <- dashboardPage(
  dashboardHeader(title = "Shiny dashboard"),
  dashboardSidebar(
    # Меню страниц
    sidebarMenu(
      menuItem("Дашборд", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Набор данных", tabName = "dataset", icon = icon("table"))
    ),
    # Выбор файла для загрузки
    fileInput("file1", "Выберите Excel", accept = ".xlsx"),
    # Выбор группы пациентов
    selectInput(inputId = 'selPatgroup', label = 'Группа пациентов', 
                choices = c(Все = '.'), selected = "."),
    # Выбор города
    selectInput('selCity', 'Город', choices = c(Все = '.'), selected = c("."),
                multiple = TRUE),
    # Выбор даты
    dateRangeInput('selDateRange','Дата взятия образца',
                   start = Sys.Date(), end = Sys.Date(), min = Sys.Date(), max = Sys.Date(),
                   format = "yyyy-mm-dd",
                   startview = "month",
                   weekstart = 1,
                   language = "ru",
                   separator = " - ",
                   width = NULL,
                   autoclose = TRUE
    ),
    # Выбор возраста
    sliderInput('selAge', 'Возраст',
      min = 0, max =0, value = c(0, 0),
      step = 1,
      dragRange = TRUE
    ),
    # вывод количества отфильтрованных строк
    valueBoxOutput("data_count", width = 12)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                tabBox(width = 12, title = "Распределение по городам", id = "tabsetMap",
                       tabPanel("Карта", leafletOutput("map")),
                       tabPanel("Города", gt_output("table_cities")),
                       tabPanel("Диагнозы", gt_output("table_diags")),
                       tabPanel("Организмы", gt_output("table_orgs"))
                )
              ),
              fluidRow(
                box(plotlyOutput("diag")),
                box(plotOutput("org"))
              )
              ),
      tabItem(tabName = "dataset",
              fluidRow(
               box(width = 12,
                 dataTableOutput("table_data")
               ) 
              )
              )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ##### Загрузка датасета из файла Excel #####
  dataset <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload an Excel file"))
    dataset <- read_excel(file$datapath, sheet = "Пациенты")
    dataset$DATESTRAIN <- as.Date(dataset$DATESTRAIN)
    dataset$DATEBIRTH <- as.Date(dataset$DATEBIRTH)
    dataset$DATEFILL <- as.Date(dataset$DATEFILL)
    dataset
  })
  
  ##### Обновление контролов #####
  observe({ 
    if (!is.null(dataset())) {
      updateSelectInput(session, "selPatgroup",
                        choices = c(Все = ".",  sort(unique(dataset()$PAT_GROUP))),
                        selected = ".")
      updateSelectInput(session, "selCity",
                        choices = c(Все = ".",  sort(unique(dataset()$CITYNAME))),
                        selected = ".")
      updateDateRangeInput(session, 'selDateRange',
                           start = min(dataset()$DATESTRAIN, na.rm = TRUE), 
                           end = max(dataset()$DATESTRAIN, na.rm = TRUE), 
                           min = min(dataset()$DATESTRAIN, na.rm = TRUE), 
                           max = max(dataset()$DATESTRAIN, na.rm = TRUE)
      )
      updateSliderInput(session, 'selAge',
                        min = min(dataset()$AGE, na.rm = TRUE),
                        max = max(dataset()$AGE, na.rm = TRUE),
                        value = c(min(dataset()$AGE, na.rm = TRUE), max(dataset()$AGE, na.rm = TRUE))
      )
    }
    })
  
  ##### Отбор данных по фильтрам ##### 
  data <- reactive({
    if (is.null(dataset())) return(NULL);
    d <- dataset()
    if (input$selPatgroup != "." ) { 
      d <- d %>% filter(PAT_GROUP == input$selPatgroup)
    }
    if ( !("." %in% input$selCity ) ) { 
      d <- d %>% filter(CITYNAME %in% input$selCity)
    }
    if (length(input$selDateRange) == 2) {
      d <- d %>% filter(DATESTRAIN >= input$selDateRange[1] & DATESTRAIN <= input$selDateRange[2])
    }
    if (length(input$selAge) == 2) {
      d <- d %>% filter(AGE >= input$selAge[1] & AGE <= input$selAge[2])
    }
    
    d
  })
  
  
  ##### Расчет количества для value-box #####
  output$data_count <- renderValueBox({
    value <- 0
    if (!is.null(data())) { 
      value <-  nrow(data())
    }
    valueBox(value, "Образцов", icon = icon("thumbs-up", lib = "glyphicon"),
             color = "purple"
    )
  })
  
  ##### Карта #####
  output$map <-  renderLeaflet({ 
    data() %>% select(CITYNAME, LATITUDE, LONGITUDE) %>%
      group_by(CITYNAME, LATITUDE, LONGITUDE) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      leaflet() %>%
      addCircleMarkers(
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        stroke = FALSE,
        fillOpacity = 0.5,
        radius = ~ scales::rescale(sqrt(Count), c(1, 10)),
        label = ~ paste("<strong>" , CITYNAME, ": ", Count, "</strong>") %>% map(html),
        labelOptions = c(textsize = "15px")) %>%
      addTiles("http://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")
  })
  
  ##### Распределение по городам #####
  output$table_cities <- render_gt({
    df <- data() %>% 
      group_by(CITYNAME) %>% summarise(Count = n()) %>%
      ungroup()
    
    colnames(df) <- c("Город", "Образцов")
    
    df %>% 
      gt(rowname_col = "Город") %>% 
      tab_header(
        title = "Распределение пациентов",
        subtitle = paste("Среди", nrow(df), "городов", sep = " ")
      ) %>% 
      grand_summary_rows(
        columns = c("Образцов"),
        fns = list(Всего = ~sum(., na.rm = TRUE))
      ) %>% 
      tab_options(
        grand_summary_row.text_transform = "capitalize"
      ) %>%
      opt_row_striping(row_striping = TRUE)
  })
  
  ##### Диагнозы по городам #####
  output$table_diags <- render_gt({
    data() %>% 
      group_by(CITYNAME, mkb_name) %>% summarise(Count = n()) %>%
      ungroup() %>% 
      pivot_wider(names_from = "CITYNAME", values_from = "Count", values_fill = 0) %>%
      select(order(colnames(.))) %>% 
      mutate(Всего = rowSums(pick(where(is.numeric), -mkb_name))) %>% 
      gt(rowname_col = "mkb_name") %>% 
      tab_header(
        title = "Распределение диагнозов",
        subtitle = paste("Среди", nrow(df), "городов", sep = " ")
      ) %>% 
      grand_summary_rows(
        columns = colnames(df)[-1],
        fns = list(Всего = ~sum(., na.rm = TRUE))
      ) %>% 
      tab_options(
        grand_summary_row.text_transform = "capitalize"
      ) %>% 
      opt_row_striping(row_striping = TRUE)
  })
  
  ##### Организмы по городам #####
  output$table_orgs <- render_gt({
     data() %>% 
      group_by(CITYNAME, STRAIN) %>% summarise(Count = n()) %>%
      ungroup() %>% 
      pivot_wider(names_from = "CITYNAME", values_from = "Count", values_fill = 0) %>%
      select(order(colnames(.))) %>% 
      mutate(Всего = rowSums(pick(where(is.numeric), -STRAIN))) %>% 
      gt(rowname_col = "STRAIN") %>% 
      tab_header(
        title = "Распределение организмов",
        subtitle = paste("Среди", nrow(df), "городов", sep = " ")
      ) %>% 
      grand_summary_rows(
        columns = colnames(df)[-1],
        fns = list(Всего = ~sum(., na.rm = TRUE))
      ) %>% 
      tab_options(
        grand_summary_row.text_transform = "capitalize"
      ) %>% 
      opt_row_striping(row_striping = TRUE)
  })
  ##### График Диагнозов #####
  output$diag <- renderPlotly({
    data() %>%
      group_by(mkb_name) %>% summarize(count = n()) %>%
      plot_ly(labels = ~mkb_name, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Структура диагнозов",  showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5))             # put legend in center of x-axis
  })
  
  ##### График Организмов #####
  output$org <- renderPlot({
    df <- data() %>%
      group_by(STRAIN) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(Percent = round(100 * Count / sum(Count))) %>%
      arrange(desc(Percent)) %>%
      mutate(csum = rev(cumsum(rev(Count))),
             pos = Count/2 + lead(csum, 1),
             pos = if_else(is.na(pos), Count/2, pos)) 
    ggplot(df, aes(x = "" , y = Count, fill = fct_inorder(STRAIN))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Pastel1") +
      geom_label_repel(data = df,
                       aes(y = pos, label = paste0(Count, " (", Percent, "%)")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Организм")) +
      labs(title = "Структура организмов") +
      theme_void() + 
      theme(title = element_text(size = 14, face = "bold", hjust = 0.5 ))
  })
  
  ##### Таблица #####
  output$table_data <- renderDataTable({
    data() %>% 
      select(study_subject_id, PAT_GROUP, SEX, AGE, DATEBIRTH, 
             STRAIN, DATESTRAIN, 
             CENTER, CITYNAME, COUNTRY, 
             DATEFILL, DIAG_ICD, mkb_name, COMPL) %>% 
      rename("Id" = study_subject_id, 
             "Группа" = PAT_GROUP, 
             "Пол" = SEX, 
             "Возраст" = AGE, 
             "Дата рождения" = DATEBIRTH, 
             "Образец" = STRAIN, 
             "Дата выделения" = DATESTRAIN, 
             "№ центра" = CENTER, 
             "Город" = CITYNAME, 
             "Страна" = COUNTRY, 
             "Дата заполнения" = DATEFILL, 
             "Диагноз МКБ" = DATEFILL, 
             "Диагноз" = mkb_name, 
             "Осложнения" = COMPL) %>% 
      datatable(options = list(
        language = list(url = 'https://cdn.datatables.net/plug-ins/2.0.7/i18n/ru.json'),
        paging = TRUE, pageLength = 10,  
         scrollX = TRUE,  scrollY = TRUE,   
         autoWidth = TRUE, 
         server = FALSE,   
         dom = 'Bfrtip',
         buttons = c('csv', 'excel'),
         columnDefs = list(list(targets = '_all', className = 'dt-center'),
                           list(targets = c(0, 8, 9), visible = FALSE))
      ),
      extensions = 'Buttons',
      selection = 'single', 
      filter = 'top', 
      rownames = FALSE
      )  
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
