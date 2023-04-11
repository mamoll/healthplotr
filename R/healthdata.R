#' Load Apple health data from a zip file and return a list of 3 tidy tibbles
#' with records, activity, and workout data
#' @param zip_file path to a zip file created by the iOS Health app.
#'
#' @export
load_exported_health_data <- function(zip_file) {
  data <- unz(zip_file, "apple_health_export/export.xml") |>
    xml2::read_xml(options = c("NOBLANKS", "COMPACT"))
  records <- data |>
    xml2::xml_find_all('//Record[starts-with(@type,"HKQuantityTypeIdentifier")]') |>
    purrr::map(xml2::xml_attrs) |>
    purrr::map_df(~ as.list(.)) |>
    dplyr::mutate(
      type = factor(gsub("HKQuantityTypeIdentifier", "", .data$type)),
      sourceName = factor(.data$sourceName),
      unit = factor(.data$unit),
      creationDate = lubridate::as_datetime(.data$creationDate),
      startDate = lubridate::as_datetime(.data$startDate),
      endDate = lubridate::as_datetime(.data$endDate),
      value = as.numeric(.data$value)
    )
  activity <- data |>
    xml2::xml_find_all("//ActivitySummary") |>
    purrr::map(xml2::xml_attrs) |>
    purrr::map_df(~ as.list(.)) |>
    dplyr::rename(date = .data$dateComponents) |>
    dplyr::select(-.data$activeEnergyBurnedUnit) |>
    dplyr::mutate(
      date = lubridate::as_date(.data$date),
      activeEnergyBurned = as.numeric(.data$activeEnergyBurned),
      activeEnergyBurnedGoal = as.numeric(.data$activeEnergyBurnedGoal),
      appleMoveTime = as.numeric(.data$appleMoveTime),
      appleMoveTimeGoal = as.numeric(.data$appleMoveTimeGoal),
      appleExerciseTime = as.numeric(.data$appleExerciseTime),
      appleExerciseTimeGoal = as.numeric(.data$appleExerciseTimeGoal),
      appleStandHours = as.numeric(.data$appleStandHours),
      appleStandHoursGoal = as.numeric(.data$appleStandHoursGoal)
    ) |>
    # remove some bogus data from 1969
    dplyr::filter(date > lubridate::as_date("1970-01-01")) |>
    tidyr::pivot_longer(cols = .data$activeEnergyBurned:.data$appleStandHoursGoal, names_to = "type") |>
    dplyr::mutate(type = factor(.data$type))
  workout <- data |>
    xml2::xml_find_all("//Workout") |>
    purrr::map(xml2::xml_attrs) |>
    purrr::map_df(~ as.list(.)) |>
    dplyr::rename(type = .data$workoutActivityType) |>
    dplyr::mutate(
      type = factor(gsub("HKWorkoutActivityType", "", .data$type)),
      duration = as.numeric(.data$duration),
      durationUnit = factor(.data$durationUnit),
      sourceName = factor(.data$sourceName),
      creationDate = lubridate::as_datetime(.data$creationDate),
      startDate = lubridate::as_datetime(.data$startDate),
      endDate = lubridate::as_datetime(.data$endDate)
    )
  list(
    records = records,
    activity = activity,
    workout = workout
  )
}

load_health_data <- function(healthdata_file, default_healthdata = NULL) {
  stopifnot(shiny::is.reactive(healthdata_file))

  shiny::reactive({
    if (is.null(healthdata_file()) || is.null(healthdata_file()$datapath)) {
      if (is.list(default_healthdata)) {
        return(default_healthdata)
      } else {
        healthdata_file <- default_healthdata
      }
    } else {
      healthdata_file <- healthdata_file()$datapath
    }
    if (!is.null(healthdata_file) && fs::file_exists(healthdata_file)) {
      if (fs::path_ext(healthdata_file) == "zip") {
        load_exported_health_data(healthdata_file)
      } else {
        readRDS(healthdata_file)
      }
    }
  })
}
