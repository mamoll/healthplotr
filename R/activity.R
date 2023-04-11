#' Generate a plot of a Apple Health activity type
#' 
#' @param data A tibble with Apple Health activity data
#' @param type A string name of the activity type ("activeEnergyBurned", "appleExerciseTime", or "appleStandHours")
#' @param aggregate A time period string ("year", "month", "week", "day")
#' @return A plotly object
#' 
#' @export
plot_activity <- function(data, type, aggregate) {
  types <- c(type, sprintf("%sGoal", type))
  names(types) <- snakecase::to_sentence_case(types)
  if (type == "activeEnergyBurned") {
    ylabel = "average time per day burning active energy (mins.)"
  } else if (type == "appleExerciseTime") {
    ylabel = "average exercise time per day (mins.)"
  } else {
    ylabel = "average number hours per day standing"
  }
  data <- data |>
    dplyr::filter(.data$type %in% types) |>
    dplyr::group_by(date = lubridate::floor_date(.data$date, unit = aggregate))
  fig <- ggplot2::ggplot(data, ggplot2::aes(.data$date, .data$value, color = .data$type)) +
    ggplot2::stat_summary(fun = "mean", geom = "line") +
    ggplot2::labs(
      x = "date",
      y = ylabel
    )
  configure_plotly(fig)
}

activityUI <- function(id) {
  shiny::uiOutput(shiny::NS(id, "activity"))
}

activityServer <- function(id, data) {
  stopifnot(shiny::is.reactive(data))

  shiny::moduleServer(id, function(input, output, session) {
    output$active_energy_burned  <- plotly::renderPlotly({
      plot_activity(data()$activity, "activeEnergyBurned", input$aggregate)
    })

    output$exercise_time  <- plotly::renderPlotly({
      plot_activity(data()$activity, "appleExerciseTime", input$aggregate)
    })

    output$stand_hours  <- plotly::renderPlotly({
      plot_activity(data()$activity, "appleStandHours", input$aggregate)
    })

    output$activity <- shiny::renderUI({
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
        shiny::mainPanel(
          plotly::plotlyOutput(shiny::NS(id, "active_energy_burned")),
          plotly::plotlyOutput(shiny::NS(id, "exercise_time")),
          plotly::plotlyOutput(shiny::NS(id, "stand_hours")),
          width = 9
        )
      )
    })
  })
}
