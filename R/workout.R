#' Generate a plot of a Apple Health workout data
#'
#' @param data A tibble with Apple Health workout data
#' @param aggregate A time period string ("year", "month", "week", "day")
#' @return A ggplot object
#'
#' @export
plot_workout <- function(data, aggregate) {
  workout <- data |>
    dplyr::group_by(date = lubridate::floor_date(.data$startDate, unit = aggregate))
  ggplot2::ggplot(workout, ggplot2::aes(.data$date, .data$duration, color = .data$type, fill = .data$type)) +
    ggplot2::stat_summary(
      fun = "sum",
      geom = "bar",
      position = "stack"
    ) +
    ggplot2::xlab("date") +
    ggplot2::ylab(sprintf("workout duration total per %s (min.)", aggregate))
}

workoutUI <- function(id) {
  shiny::uiOutput(shiny::NS(id, "workout"))
}

workoutServer <- function(id, data) {
  stopifnot(shiny::is.reactive(data))

  shiny::moduleServer(id, function(input, output, session) {
    output$workout_plot <- plotly::renderPlotly(
      plot_workout(data()$workout, input$aggregate)
    )

    output$workout <- shiny::renderUI({
      shiny::validate(shiny::need(!is.null(data()), "Upload export.zip first."))
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput(
            shiny::NS(id, "aggregate"),
            label = shiny::h4("Aggregate by"),
            choices = aggregate,
            selected = "month"
          ),
          width = 3
        ),
        shiny::mainPanel(plotly::plotlyOutput(shiny::NS(id, "workout_plot")), width = 9)
      )
    })
  })
}
