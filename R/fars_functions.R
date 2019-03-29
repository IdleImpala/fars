#' Read file
#'
#' The function reads a file.
#'
#' @param filename name of the file.
#' @return The function returns a table of classes "tbl_df", "tbl", "data.frame".
#' @details The function checks if the file exists. if it exists,
#' it reads it (also suppresses any messages that might be printed),
#' if it doesn't exist, it throws an error message.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read(make_filename(2013))
#' }
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create filename
#'
#' The function creates a standard filename form a year input.
#'
#' @param year An integer number to be passed to the filename.
#' @return The function returns an object of class "character".
#' @details The function attempts to convert its argument to integer.
#' If unsuccessful, the function throws an error message.
#' If successful, the function passes the outcome to the sprintf function,
#' which then prints a standard filename that also contains the year which was specified.
#' @examples
#' make_filename(2013)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read years and months from data files
#'
#' The function reads data files based on a vector of years given.
#' For each data file, it adds the year to the data frame as a new variable,
#' and then extracts the MONTH, and year variables of the data frame.
#'
#' @param years An integer number to be passed to the standard filename.
#' @return The function returns a list of data frames.
#' @details The function uses lapply on the vector of arguments.
#' For each element of the vector, it attempts to create a filename with make_filename(),
#' read the file with fars_read(), add the year variable with dplyr::mutate(),
#' and finally select the variables with dplyr:select(). fars_read() and the dplyr functions
#' are wrapped in a tryCatch() function, which will print an error and return NULL when an
#' element of the vector of years is invalid.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' }
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

#' Summarize data files by years
#'
#' The function reads data by years, binds them by rows, groups by years, and months,
#' and summarizes for each month of each year.
#'
#' @param years A vector of years.
#' @return The function returns an object of classes "tbl_df", "tbl", "data.frame".
#' The object is a data frame with twelve rows (one for each month) and n+1 variables.
#' The first variable is the MONTH variable, and it is followed by n year variables,
#' where n is the number of different years occurring in the tables which were read.
#' @details The function will give an error fars_read_years() would give an error.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014,2015))
#' }
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#'Map State
#'
#'The function prints a map of accident locations for a given state
#'
#'@param state.num ID of the state.
#'@param year Year.
#'@return The function returns a printed map of the state with marked accident locations.
#'@details The function can give an error if the STATE number is invalid.
#'It can also produce an error if the year is invalid and so the data file cannot be
#'read. The function uses make_filename(), fars_read(), so it can produce an error also
#'whenever these funtions would.
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@examples
#'\dontrun{
#'fars_map_state(6,2013)
#'}
#'@export
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
