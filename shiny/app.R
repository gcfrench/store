library(shiny)
library(shinydashboard)
library(thematic)
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(fs)
library(palmerpenguins)

# TO DO
## Colour selected plot points: pg 113

# royalty-free stock photographs https://unsplash.com/
species_images <- tribble(
  ~species, ~id, ~author,
  "Chinstrap", "ZZyK8GjFJ4Q", "rocinante_11",
  "Gentoo", "LAQ2QfYTpTY", "tamwarnerminton",
  "Adelie", "9k9tNQTwMEA", "dylanshaw"
)

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

ui <- dashboardPage(
        dashboardHeader(title = "Example Shiny App"),
        dashboardSidebar(

                ### Dynamic selectInput https://shiny.rstudio.com/articles/selectize.html#server-side-selectize
                ### shinywidgets https://github.com/dreamRs/shinyWidgets
                ### colourpicker https://github.com/daattali/colourpicker
                ### sortable https://rstudio.github.io/sortable

                fluidRow(
                        column(1),
                        column(11,

                               ## Immediate reactivity inputs ------------------
                               radioButtons("species_selected", "Select a penguin species",
                                            c("Adelie", "Gentoo", "Chinstrap")),
                               ### Using sliders https://shiny.rstudio.com/articles/sliders.html
                               sliderInput("year_range", "Select year range",
                                           value = c(2007, 2009), min = 2006, max = 2010,
                                           sep = "")
                        )
                ),
                fluidRow(
                        column(1),
                        column(11,
                               htmlOutput("species_image_source"),
                               imageOutput("species_image"),
                        )
                ),
                fluidRow(
                        column(1),
                        column(11,
                               ## Delayed reactivity inputs --------------------
                               actionButton("display_button", "Display species measurements",
                                            class = "btn-sm btn-primary")
                        )
                )
        ),
        dashboardBody(
                fluidRow(
                        column(6,
                               ## Immediate reactivity outputs -----------------
                               textOutput("species_text"),

                               ### plot input https://gallery.shinyapps.io/095-plot-interaction-advanced/
                               #### click = clickOpts(id = "plot_click", ...)
                               #### dblclick = dblClickOpts(id = "plot_dblclick", ...)
                               #### hover = hoverOpts(id = "plot_hover", ...)
                               #### brush = brushOpts(id = "plot_brush", ...)
                               plotOutput("species_plot",
                                          brush = brushOpts(id = "plot_brush",
                                                            fill = "gold", stroke = "black",
                                                            resetOnNew = TRUE),
                                          height = "550px"),
                        ),
                        column(6,
                               tableOutput("species_plot_selected"),
                        )
                ),
                fluidRow(
                        ## Delayed reactivity outputs -------------------
                        column(12,
                               ### reactable tables https://glin.github.io/reactable/
                               dataTableOutput("species_table"),
                        )

                )
        )

)


server <- function(input, output, session) {

        # Themes
        ## https://rstudio.github.io/thematic
        thematic_shiny()

        # Immediate Shiny reactivity -------------------------------------------

        ## Reactive expressions
        species_data_plot <- reactive({penguins %>%
                        filter(species == input$species_selected, between(year, input$year_range[1], input$year_range[2])) %>%
                        select(-starts_with("bill"))
        })

        ## Outputs
        ### https://shiny.rstudio.com/articles/images.html
        output$species_image <- renderImage({
                list(
                  src = path("images", str_glue("{input$species_selected}.jpg")),
                  width = 211,
                  height = 317
                )
        }, deleteFile = FALSE)

        output$species_image_source <- renderUI({
                info <- species_images %>%
                             filter(species == input$species_selected)
                HTML(str_glue("<p>
                              <a href='https://unsplash.com/photos/{info$id}'>original</a> by
                              <a href='https://unsplash.com/@{info$author}'>{info$author}</a>
                              </p>"))
        })

        output$species_text <- renderText(input$species_selected)
        ### https://plotly-r.com/
        output$species_plot <- renderPlot({
                ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
                        geom_point(size = 2, colour = "grey", alpha = 0.6) +
                        geom_point(data = species_data_plot(),
                                   aes(x = flipper_length_mm, y = body_mass_g),
                                   size = 2, colour = "steelblue") +
                        ggplot2::theme_bw()
        }, res = 96)

        ### nearPoints: plot_click, plot_dblclick, plot_hover
        ### brushedPoints: plot_brush
        output$species_plot_selected <- renderTable({
                req(input$plot_brush)
                brushedPoints(species_data_plot(), input$plot_brush)
        })


        # Delayed Shiny reactivity ---------------------------------------------

        ## eventReactive
        species_data_table <- eventReactive(input$display_button, {
                species_data_plot()

        })

        ## Outputs
        ### DataTables options https://datatables.net/reference/option/
        output$species_table <- renderDataTable(species_data_table(), options= list(pageLength = 6))

        # Side effect reactivity -----------------------------------------------

        ## observeEvent
        observeEvent(input$species_selected, {
                message(str_glue("{input$species_selected} penguin species selected"))
        })

        # Timed reactivity -----------------------------------------------------

        ## reactiveTimer
        timer <- reactiveTimer(5000) # milliseconds

        ## Triggered reaction
        observeEvent(timer(),
                     message("The app has been working for the past 5 seconds"))

}

shinyApp(ui, server)
