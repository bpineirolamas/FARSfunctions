#' Read FARS data file
#'
#' This function reads a CSV file containing Fatality Analysis Reporting System (FARS)
#' data and converts it to a dplyr tibble.
#'
#' @param filename The name of the CSV file to read
#'
#' @return A tbl_df type of data containing the data from the CSV file
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' # Read 2013 accident data
#' data_2013 <- fars_read("accident_2013.csv")
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate FARS filenames
#'
#' This function generates a filename for FARS data based on the year (2013, 2014 or 2015)
#'
#' @param year A numeric value representing the year
#'
#' @return A filename "accident_xxxx.csv.bz2", where xxxx represents the year
#'
#' @examples
#' make_filename(2013)
#' make_filename(2014)
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data for multiple years
#'
#' This function reads FARS data files for multiple years and selects
#' the MONTH and year columns from each file.
#'
#' @param years A vector of years
#'
#' @return The information corresponding to each month of the selected years
#'
#' @details The function returns NULL for invalid years
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#' # Read data for multiple years
#' fars_13_14_15 <- fars_read_years(c(2013, 2014, 2015))
#' fars_13_9999 <- fars_read_years(c(2013, 9999))
#'
#' @export
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




#' Summarize FARS data by years and months
#'
#' This function creates a summary of the number of accidents by month and year
#'
#' @param years A vector of years
#'
#' @return A tibble where each row corresponds to a month (from 1=january to 12=december)
#' and each column is a year. It contains the number of accidents per month/year.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' fars_summarize_years(c(2013, 2014, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map FARS accidents by state
#'
#' This function provides a map for a scpecific state and year, that shows the location of
#' fatal accidents recorded in FARS.
#'
#' @param state.num FARS code of the state
#' @param year Year of interest
#'
#' @return A plot for the selected state and year showing the location of fatal accidents
#'
#' @details If the state number is not corect, the function does not return the plot.
#' Longitudes>900 and latitudes>90 are considered as missing data.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' # Map accidents in NY (code 36) for 2015
#' fars_map_state(36, 2015)
#'
#' @export
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
