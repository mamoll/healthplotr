aggregate <- c("year", "month", "week", "day")

configure_plotly <- function(fig) {
  plotly::ggplotly(fig, dynamicTicks = T) |>
    plotly::layout(
      dragmode = "pan",
      showlegend = F,
      xaxis = list(
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "1m",
              step = "month",
              stepmode = "backward"
            ),
            list(
              count = 6,
              label = "6m",
              step = "month",
              stepmode = "backward",
              active = T
            ),
            list(
              count = 1,
              label = "1y",
              step = "year",
              stepmode = "backward"
            ),
            list(step = "all")
          )
        )
      )
    )
}
