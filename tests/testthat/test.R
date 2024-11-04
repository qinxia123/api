library(testthat)
library(httr) 

# Mock setup
mock_year_valid <- 2021
mock_year_invalid <- 1999
mock_year_known_empty <- 2024  

test_that("getdata_api successfully downloads and parses JSON data", {
  # Run the function with a valid year and check the structure of the returned data
  data <- suppressWarnings(getdata_api(mock_year_valid))
  
  # Check that data is a list
  expect_true(is.list(data), info = "The data should be a list.")
  
  # Add checks for expected fields if known
  # For example:
  # expect_true("specific_field" %in% names(data))
})

test_that("getdata_api handles invalid years gracefully", {
  # Suppress warnings while calling the function with an invalid year
  data <- suppressWarnings(getdata_api(mock_year_invalid))
  expect_null(data, info = "Data should be NULL for invalid years.")
})

test_that("getdata_api handles empty responses", {
  # Assuming that the year 2024 returns no data
  data <- suppressWarnings(getdata_api(mock_year_known_empty))
  
  # Ensure data is NULL or empty
  expect_true(is.null(data) || (is.list(data) && length(data) == 0), 
              info = "Data should be NULL or an empty list for known empty responses.")
})