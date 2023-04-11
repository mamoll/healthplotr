units_to_avg <-
  c(
    "ft",
    "lb",
    "count/min",
    "%",
    "mL/min\u00b7kg",
    "dBASPL",
    "mi/hr",
    "in",
    "HeartRateVariabilitySDNN"
  )

#' Generate a plot of a Apple Health records
#'
#' @param data A tibble with Apple Health records
#' @param type A string name of the record type (e.g., "BodyMass")
#' @param aggregate A time period string ("year", "month", "week", "day")
#' @return A plotly object
#'
#' @export
plot_records <- function(data, type, aggregate) {
  records <- data |>
    dplyr::filter(.data$type == {{type}}) |>
    dplyr::group_by(date = lubridate::floor_date(.data$startDate, unit = aggregate))
  shiny::validate(
    shiny::need(
      dplyr::n_distinct(records$unit) == 1,
      "Error: cannot handle different units for the same type of record"
    )
  )

  type <- as.character(records$type[1])
  unit <- records$unit[1]
  ylabel <- sprintf("%s (%s)", snakecase::to_sentence_case(type), unit)
  if (any(units_to_avg == unit) || type == "BodyMassIndex") {
    records <- dplyr::summarize(
      records,
      y = mean(.data$value),
      min = min(.data$value),
      max = max(.data$value)
    )
    fig <-
      ggplot2::ggplot(records, ggplot2::aes(.data$date, .data$y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_line(ggplot2::aes(y = .data$min, color = "")) +
      ggplot2::geom_line(ggplot2::aes(y = .data$max, color = "")) +
      ggplot2::xlab("date") +
      ggplot2::ylab(ylabel)
  } else {
    records <- dplyr::summarize(records, y = sum(.data$value))
    fig <-
      ggplot2::ggplot(records, ggplot2::aes(.data$date, .data$y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::xlab("date") +
      ggplot2::ylab(ylabel)
  }
  configure_plotly(fig)
}

recordsUI <- function(id) {
  shiny::uiOutput(shiny::NS(id, "records"))
}

recordsServer <- function(id, data) {
  stopifnot(shiny::is.reactive(data))

  shiny::moduleServer(id, function(input, output, session) {
    record_types <- shiny::reactive({
      types <- sort(data() |> purrr::pluck("records") |> dplyr::select(.data$type) |> dplyr::distinct() |> purrr::pluck("type"))
      names(types) <- snakecase::to_sentence_case(as.character(types))
      types
    })

    output$records_plot <- plotly::renderPlotly({
      plot_records(data()$records, input$type, input$aggregate)
    })

    output$records <- shiny::renderUI({
      shiny::validate(shiny::need(!is.null(data()), "Upload export.zip first."))
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput(
            shiny::NS(id, "type"),
            label = shiny::h4("Type"),
            choices = record_types(),
            selected = "BodyMass"
          ),
          shiny::selectInput(
            shiny::NS(id, "aggregate"),
            label = shiny::h4("Aggregate by"),
            choices = aggregate,
            selected = "month"
          ),
          width = 3
        ),
        shiny::mainPanel(plotly::plotlyOutput(shiny::NS(id, "records_plot")), width = 9)
      )
    })
  })
}
