#' Download and Parse JSON Data from a URL
#'
#' This function retrieves data from the Kolada API for a specified year.
#' It validates the year input, constructs the appropriate URL, and
#' handles errors during the HTTP request and JSON parsing.
#'
#' @param year A numeric value representing the year for which data is to be retrieved.
#'             It must be between 2000 and 2023 (inclusive).
#'
#' @return A list containing the parsed JSON data if successful, or NULL if an error occurs.
#'
#' @examples
#' # Retrieve data for the year 2021
#' data_2021 <- getdata_api(2021)
#'
#' # Attempt to retrieve data for an invalid year
#' data_invalid <- getdata_api(1999)  # This will generate an error message
#'
#' @export
getdata_api <- function(year) {
  tryCatch({
    # Validate year
    if (!is.numeric(year) || year < 2000 || year > 2023) {
      stop("Invalid year. Please enter a year between 2000 and 2023.")
    }
    
    # Construct the URL
    url <- paste0("https://api.kolada.se/v2/data/kpi/N00945/year/", year)
    
    # Attempt to download the data using httr
    response <- httr::GET(url)
    
    # Check for HTTP errors
    if (httr::http_error(response)) {
      stop("HTTP error: ", httr::status_code(response))
    }
    
    # Read the content and parse JSON
    content <- httr::content(response, "text", encoding = "UTF-8")
    
    # Check if the content is empty
    if (nchar(content) == 0) {
      stop("No data in the downloaded content.")
    }
    
    # Parse the JSON string into an R object
    data <- jsonlite::fromJSON(content)
    
    return(data)
  }, error = function(e) {
    message("Error in downloading or parsing the data: ", e$message)
    return(NULL)
  }, warning = function(w) {
    message("Warning: ", w$message)
    return(NULL)  # Return NULL for warnings too
  })
}