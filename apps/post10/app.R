
#git commit -m "post10 - prepping data for and deploying shiny app, done"


# Libraries ---------------------------------------------------------------

library(shiny); library(bslib); library(tidyverse) 

# Data load & clean -------------------------------------------------------

df <- readRDS("data/post10_data.rds")

# Shiny app ---------------------------------------------------------------

plot_sector <- function(data, sector_label) {
  data_sector <- data |> filter(sector == sector_label)
  req(nrow(data_sector) > 0)  # show nothing if that sector isn't present
  
  ggplot(data_sector, aes(x = year, y = value, color = age)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ sex, nrow = 2) +
    theme(
      plot.title = element_blank(),
      legend.position = "bottom",
    ) +
    labs(
      title = sector_label,
      x = NULL, color = "Age",
      y = NULL
    ) +
    theme_bw(base_size = 12) 
}

measures <- df |>
  distinct(measure) |>
  arrange(measure) |>
  pull(measure)

ui <- page_sidebar(
  title = "Hospital stays — dashboard",
  sidebar = tagList(
    selectInput("measure", "Measure", choices = measures, selected = measures[1]),
    helpText("All plots update to the selected measure."),
    helpText("Lines = time"),
    helpText("Color = age"),
    helpText("Facets = sex"),
    helpText("Data has been supplied by Statistics Denmark (table SBR04), which contains aggregate data on hospital usage in Denmark between 2017-2024.")
  ),
  # 3 cards side-by-side (wraps on narrow screens)
  layout_columns(width = 1,
                 card(
                   full_screen = TRUE,
                   card_header("Somatic"),
                   plotOutput("plot_somatic", height = 300)
                 ),
                 card(
                   full_screen = TRUE,
                   card_header("Psychiatric"),
                   plotOutput("plot_psychiatric", height = 300)
                 ),
                 card(
                   full_screen = TRUE,
                   card_header("Both"),
                   plotOutput("plot_both", height = 300)
                 )
  )
)

server <- function(input, output, session) {
  # Filter once by measure; reuse for all sectors
  dat_measure <- reactive({
    req(input$measure)
    df |> filter(measure == input$measure)
  })
  
  output$plot_somatic     <- renderPlot(plot_sector(dat_measure(), sector_label = "Somatic"))
  output$plot_psychiatric <- renderPlot(plot_sector(dat_measure(), sector_label = "Psychiatry"))
  output$plot_both        <- renderPlot(plot_sector(dat_measure(), sector_label = "Both somatic and psychiatry"))
  
}

shinyApp(ui, server)