downloadUI <- function(id) {
  shiny::uiOutput(shiny::NS(id, "download"))
}

downloadServer <- function(id, data) {
  stopifnot(shiny::is.reactive(data))

  shiny::moduleServer(id, function(input, output, session) {
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste("healthdata-", Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(data(), file)
      }
    )
    output$download <- shiny::renderUI({
      shiny::validate(shiny::need(!is.null(data()), "Upload export.zip first."))
      shiny::div(
        class = "row",
        shiny::div(
          class = "col-sm-10 col-sm-offset-1",
          shiny::h2("Download Apple Health data as RDS file"),
          shiny::downloadButton(shiny::NS(id, "download_data"), "Download")
        )
      )
    })
  })
}
