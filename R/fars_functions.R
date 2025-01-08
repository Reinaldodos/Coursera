#' Read a FARS Data File
#'
#' This function reads a Fatality Analysis Reporting System (FARS) dataset file.
#' The file must exist in the working directory.
#'
#' @param filename A string specifying the path to the FARS data file.
#' @return A tibble containing the data from the specified file.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a FARS File Name
#'
#' This function generates a standard FARS file name for a given year.
#'
#' @param year An integer representing the year.
#' @return A string containing the FARS file name for the given year.
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS Data for Multiple Years
#'
#' This function reads FARS data for multiple years and extracts the month and year columns.
#'
#' @param years A vector of integers representing the years of interest.
#' @return A list of tibbles, each containing the month and year data for a specified year.
#' Invalid years will result in warnings.
#' @importFrom dplyr mutate select
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize FARS Data by Year and Month
#'
#' This function summarizes the number of FARS observations by year and month.
#'
#' @param years A vector of integers representing the years of interest.
#' @return A tibble summarizing the number of observations by year and month.
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014))
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map FARS Data for a State
#'
#' This function plots the locations of FARS accidents for a given state and year.
#'
#' @param state.num An integer representing the state number.
#' @param year An integer representing the year of interest.
#' @return A map displaying the locations of accidents, or a message if no data is available.
#' Errors will occur if the state number is invalid.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
