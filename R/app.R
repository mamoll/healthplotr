#' Start a Shiny app to visualize Apple Health data
#' @param healthdata (1) a zip file with Apple Health,
#' (2) an RDS file with health data as returned by healthplotr::load_exported_health_data, or
#' (3) a health data object as returned by healthplotr::load_exported_health_data
#'
#' @export
healthplotr <- function(healthdata = NULL) {
  thematic::thematic_shiny()

  ui <- shiny::navbarPage(
    "Healthplotr",
    shiny::tabPanel(
      "Records",
      recordsUI("records"),
      value = "records",
      icon = shiny::icon("bar-chart")
    ),
    shiny::tabPanel(
      "Activity",
      activityUI("activity"),
      value = "activity",
      icon = shiny::icon("clock-rotate-left")
    ),
    shiny::tabPanel(
      "Workout",
      workoutUI("workout"),
      value = "workout",
      icon = shiny::icon("person-running")
    ),
    shiny::tabPanel(
      "Upload",
      shiny::div(
        class = "row",
        shiny::div(
          class = "col-sm-10 col-sm-offset-1",
          shiny::h2("Upload export.zip or an RDS file produced by healthplotr"),
          shiny::fileInput(
            "healthdata_file",
            label = NULL,
            accept = c("application/zip", ".zip", ".rds")
          )
        )
      ),
      value = "healthdata_file",
      icon = shiny::icon("upload")
    ),
    shiny::tabPanel(
      "Download",
      downloadUI("download"),
      value = "download",
      icon = shiny::icon("download")
    ),
    id = "navbar",
    header = shiny::tags$style(shiny::HTML("
      .navbar-brand::before {
        font-family: 'FontAwesome';
        content: '\\f0fa';
        padding-right: 10px;
      }
      .footer { padding-top: 30px; }")),
    footer = shiny::div(
      class = "footer",
      shiny::div(
        class = "container-fluid",
        shiny::p(
          shiny::HTML("&copy; 2023 "),
          shiny::a(href = "https://moll.ai", "Mark Moll"),
          shiny::HTML("&bull;"),
          shiny::a(
            href = "https://github.com/mamoll/healtplotr",
            shiny::span(shiny::icon("github"), " GitHub")
          )
        )
      )
    ),
    theme = shinythemes::shinytheme("cosmo")
  )

  server <- function(input, output, session) {
    data <- load_health_data(shiny::reactive(input$healthdata_file), healthdata)
    # Go straight to the upload page if there is no data
    shiny::observe({
      if (is.null(data())) {
        shiny::updateTabsetPanel(session, "navbar", selected = "healthdata_file")
      }
    })

    recordsServer("records", data)
    activityServer("activity", data)
    workoutServer("workout", data)
    downloadServer("download", data)
  }

  # set default max upload size to 200MB
  options(shiny.maxRequestSize = 200*1024^2)
  shiny::shinyApp(ui, server)
}
