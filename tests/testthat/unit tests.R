# Test Suite for FARS Functions

# Test for `fars_read`
# Verify that the function correctly reads a valid file
# and throws an appropriate error for a missing file.

test_that("fars_read reads valid files correctly", {
  filename <- tempfile(fileext = ".csv")
  write.csv(data.frame(MONTH = 1:12, YEAR = 2013), filename, row.names = FALSE)
  result <- fars_read(filename)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 12)
  unlink(filename)
})

test_that("fars_read throws an error for a missing file", {
  expect_error(fars_read("non_existent_file.csv"),
               "file 'non_existent_file.csv' does not exist")
})

# Test for `make_filename`
# Verify that the function correctly formats file names.

test_that("make_filename formats file names correctly", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename(2020), "accident_2020.csv.bz2")
})

# Test for `fars_read_years`
# Verify that the function processes multiple years correctly
# and handles invalid years gracefully.

test_that("fars_read_years reads valid years and handles invalid years", {
  valid_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(MONTH = 1:12, YEAR = 2013), valid_file, row.names = FALSE)
  file.rename(valid_file, "accident_2013.csv.bz2")

  result <- fars_read_years(c(2013, 9999))
  expect_length(result, 2)
  expect_s3_class(result[[1]], "tbl_df")
  expect_null(result[[2]])

  unlink("accident_2013.csv.bz2")
})

# Test for `fars_summarize_years`
# Verify that the function returns a tibble summarizing observations by month and year.

test_that("fars_summarize_years summarizes data correctly", {
  valid_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(MONTH = c(1, 1, 2), YEAR = c(2013, 2013, 2013)), valid_file, row.names = FALSE)
  file.rename(valid_file, "accident_2013.csv.bz2")

  result <- fars_summarize_years(c(2013))
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 2) # Columns: MONTH and 2013
  expect_equal(result$n[result$MONTH == 1], 2)

  unlink("accident_2013.csv.bz2")
})

# Test for `fars_map_state`
# Verify that the function produces a map or handles missing data gracefully.

test_that("fars_map_state handles valid and missing data", {
  valid_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(STATE = c(1, 1), LATITUDE = c(30, 40), LONGITUD = c(-90, -100)),
            valid_file, row.names = FALSE)
  file.rename(valid_file, "accident_2013.csv.bz2")

  expect_message(fars_map_state(1, 2013), NA) # Check for no error
  expect_error(fars_map_state(999, 2013), "invalid STATE number")

  unlink("accident_2013.csv.bz2")
})
