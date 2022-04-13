library(shiny)
library(shinyvalidate)
library(shinycssloaders)
library(thematic)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(lubridate)
library(tibble)
library(fs)
library(readr)

# TO DO
## Colour selected plot points: pg 113
## Download a parameterized report: pg 146
## Add reset button: pg 155

# royalty-free stock photographs https://unsplash.com/
species_images <- tribble(
  ~species, ~id, ~author,
  "Chinstrap", "ZZyK8GjFJ4Q", "rocinante_11",
  "Gentoo", "LAQ2QfYTpTY", "tamwarnerminton",
  "Adelie", "9k9tNQTwMEA", "dylanshaw"
)

# Dialog box
modal_confirm <- modalDialog(
  "Do you wish to display penguin measurements?",
  title = "Display measurements",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Display" , class = "btn-sm btn-primary")
  ))

### Shiny cheatsheet https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
### Shiny community: https://community.rstudio.com/c/shiny/8
### Awesome Shiny Extensions https://github.com/nanxstats/awesome-shiny-extensions

# Themes
## https://rstudio.github.io/bslib; bslib::bs_theme_preview()
## https://bootswatch.com/

# Layouts
## dashboardPage()
## dashboardHeader()
## dashboardSidebar(fluidRow(column())
## dashboardBody(fluidRow(column())
## tabs: tabsetPanel | navlistPanel | navbarPage (tabPanel(fluidRow(column())))
### Shinydashboard: http://rstudio.github.io/shinydashboard/index.html
### Shiny layout: https://shiny.rstudio.com/articles/layout-guide.html

# Upload data ------------------------------------------------------------------
ui_page_1 <- sidebarLayout(
  sidebarPanel(width = 3,

               h3("Upload data"),
               ### https://shiny.rstudio.com/articles/upload.html
               fileInput("penguins_upload", "Upload penguin data", accept = ".csv")
  ),
  mainPanel()
)

# Immediate and Delayed reactivity inputs --------------------------------------
ui_page_2 <- sidebarLayout(
  sidebarPanel(width = 3,

               h3("Immediate reactivity"),
               ### Dynamic selectInput https://shiny.rstudio.com/articles/selectize.html#server-side-selectize
               ### shinywidgets https://github.com/dreamRs/shinyWidgets
               ### colourpicker https://github.com/daattali/colourpicker
               ### sortable https://rstudio.github.io/sortable

               selectInput("species_selected", "Select a penguin species", choices = NULL),
               ### Using sliders https://shiny.rstudio.com/articles/sliders.html
               numericInput("species_year", "Select year", value = NULL, min = 0, max = 0),

               h3("Delayed reactivity"),
               actionButton("display_button", "Display species measurements",
                            class = "btn-sm btn-primary")

  ),
  mainPanel(
    fluidRow(
      column(2,
             htmlOutput("species_image_source"),
             imageOutput("species_image")
      ),
      column(5,
             textOutput("species_text"),


             ### plot input https://gallery.shinyapps.io/095-plot-interaction-advanced/
             #### click = clickOpts(id = "plot_click", ...)
             #### dblclick = dblClickOpts(id = "plot_dblclick", ...)
             #### hover = hoverOpts(id = "plot_hover", ...)
             #### brush = brushOpts(id = "plot_brush", ...)

             # Progress and spinner bar
             ## shiny withProgress: https://shiny.rstudio.com/articles/progress.html
             ## waiter: https://github.com/JohnCoene/waiter
             ## shinycssLoaders: https://github.com/daattali/shinycssloaders
             withSpinner(plotOutput("species_plot",
                                    brush = brushOpts(id = "plot_brush",
                                                      fill = "gold", stroke = "black",
                                                      resetOnNew = TRUE),
                                    height = "317px"))
      ),
      column(5,
             tableOutput("species_plot_selected")
      ),
      column(12,
             ### reactable tables https://glin.github.io/reactable/
             dataTableOutput("species_table")
      )
    )
  )
)


# Download data ----------------------------------------------------------------
ui_page_3 <- sidebarLayout(
  sidebarPanel(width = 3,

    h3("Download data"),
    downloadButton("download_button", "Download species measurements",
                   class = "btn-sm btn-primary")
  ),
  mainPanel()
)

# UI layout
ui <- fluidPage(
  h1("Example shiny app"),
  tabsetPanel(
    id = "wizard",
    type = "hidden",
    tabPanel("page_1",
             ui_page_1
    ),
    tabPanel("page_2",
             ui_page_2
    ),
    tabPanel("page_3",
             ui_page_3
    )
  )
)

server <- function(input, output, session) {

# update UI page ---------------------------------------------------------------
switch_page <- function(i) {
  updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
}

observeEvent(input$penguins_upload, switch_page(2))

# Upload data ------------------------------------------------------------------

  ## Reactive expressions
  penguins_data <- reactive({

    # Only proceed if penguin data is uploaded
    req(input$penguins_upload)

    # upload data
    read_csv(input$penguins_upload$datapath)
  })

# Immediate reactivity ---------------------------------------------------------

  ## Validate input values
  ### shinyValidate: https://rstudio.github.io/shinyvalidate/
  ### shinyFeedback: https://github.com/merlinoa/shinyFeedback

  ## initiate validation
  check_input <- InputValidator$new()

  ## Validation rules
  check_input$add_rule("species_selected", sv_optional())
  check_input$add_rule("species_year", sv_optional())
  check_input$add_rule("species_year", sv_between(
    left = 2007,
    right = 2009,
    inclusive = c(TRUE, TRUE),
    message_fmt = "measurements not available"))

  ## turn on validation
  check_input$enable()

  ## Notification
  notify <- function(msg, id = NULL, duration = NULL) {
    showNotification(msg, id = id, duration = duration,
                     closeButton = FALSE, type = "message")
  }

  ## Update input boxes
  ### species_selected choices
  species_list <- reactive({
    penguins_data()  %>%
      distinct(species) %>%
      arrange(species) %>%
      pull()
  })

  observeEvent(species_list(), {
    ### prevent ficker: freezeReactiveValue
    updateSelectInput(inputId = "species_selected", choices = species_list())
  })

  ### species_year choices
  year_list <- reactive({
    penguins_data() %>%
      distinct(year) %>%
      arrange(year) %>%
      pull()
  })

  observeEvent(year_list(), {
    ### prevent ficker: freezeReactiveValue
    updateNumericInput(inputId = "species_year",
                       value = min(year_list()),
                       min = min(year_list() - 1), max = max(year_list() + 1))
  })

  species_data_plot <- reactive({

    ### Only proceed if input values are valid else pause reactivity
    req(check_input$is_valid())

    ### Check combination of input values using shiny::validate
    #### https://shiny.rstudio.com/reference/shiny/0.14/validate.html
    if(input$species_selected == "Gentoo" && input$species_year == 2008) {
      validate("Gentoo measurements for 2008 are not available")
    }

    penguins_data() %>%
      filter(species == input$species_selected, year == input$species_year) %>%
      select(-starts_with("bill"))

  })

  ## side effects reactivity: Notifications
  observeEvent(input$species_selected, {

    # Only proceed if penguin data loaded
    req(penguins_data())

    id <- notify(str_glue("Getting {input$species_selected} penguin data"))
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)

    notify(str_glue("Getting {input$species_selected} penguin image"), id = id)
    Sys.sleep(1)

    notify(str_glue("Extracting measurements for {input$species_year}"), id = id)
    Sys.sleep(1)

    notify(str_glue("Creating {input$species_selected} penguin plot"), id = id)
    Sys.sleep(1)
  })

     ## Outputs
  output$species_image_source <- renderUI({

    ### Only proceed if penguin data loaded
    req(penguins_data())

    info <- species_images %>%
      filter(species == input$species_selected)
    HTML(str_glue("<p>
                              <a href='https://unsplash.com/photos/{info$id}'>original</a> by
                              <a href='https://unsplash.com/@{info$author}'>{info$author}</a>
                              </p>"))
  })

  ### https://shiny.rstudio.com/articles/images.html
  output$species_image <- renderImage({

    ### Only proceed if penguin data loaded
    req(penguins_data())

    list(
      src = path("images", str_glue("{input$species_selected}.jpg")),
      width = 211,
      height = 317
    )
  }, deleteFile = FALSE)

  output$species_text <- renderText({

    ### Only proceed if penguin data loaded
    req(penguins_data())

    ### Only proceed if input values are valid
    req(check_input$is_valid())

    input$species_selected
  })

  ### https://plotly-r.com/
  output$species_plot <- renderPlot({
    ggplot(data = penguins_data(), aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point(size = 2, colour = "grey", alpha = 0.6) +
      geom_point(data = species_data_plot(),
                 aes(x = flipper_length_mm, y = body_mass_g),
                 size = 2, colour = "steelblue") +
      ggplot2::theme_bw()
  }, res = 96)

  ### nearPoints: plot_click, plot_dblclick, plot_hover
  ### brushedPoints: plot_brush
  output$species_plot_selected <- renderTable({

    # Only proceed if input$plot_brush values are present
    req(input$plot_brush)
    brushedPoints(species_data_plot(), input$plot_brush)
  })

  # Delayed reactivity ---------------------------------------------------------

  ## eventReactive
  species_data_table <- eventReactive(input$display_button, {

    species_data_plot()
  })

  ### side effect reactivity: Dialog Box
  observeEvent(input$display_button, {
    showModal(modal_confirm)
  })

  observeEvent(input$ok, {
    id <- notify("Displaying measurements ...")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)

    ### DataTables options https://datatables.net/reference/option/
    output$species_table <- renderDataTable(
      species_data_table(), options= list(pageLength = 6)
    )

    removeModal()
  })

  observeEvent(input$cancel, {
    removeModal()
  })

# Download data ----------------------------------------------------------------
  #### Parameterized reports https://shiny.rstudio.com/articles/generating-reports.html
  output$download_button <- downloadHandler(

    filename = function() {
      str_glue("{input$species_selected}_{input$species_year}.csv")
    },
    content = function(file){
      write_csv(species_data_plot(), file)
    }
  )

# Timed reactivity -------------------------------------------------------------

  ## reactiveTimer
  timer <- reactiveTimer(6000) # milliseconds

  ## Triggered reaction
  observeEvent(timer(),
               notify("Monitoring ..............", duration = 1))
}

shinyApp(ui, server)
