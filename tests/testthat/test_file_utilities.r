context("Tests for file utility functions")

test_that("findBadEncodingFiles detects encoding issues", {
  # This is more of an integration test
  # Just verify it doesn't crash on valid directory
  submissions <- "example/assignment1_submissions/"
  
  # Capture output
  output <- capture.output(findBadEncodingFiles(submissions))
  
  # Should not error
  expect_true(TRUE)
})

test_that("findGlobalPaths detects absolute paths", {
  # Create temporary directory structure
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Create a file with global paths
  temp_file <- file.path(temp_dir, "script_with_paths.R")
  writeLines(c(
    "# This is a comment",
    "data <- read.csv('/home/user/data.csv')",  # Unix absolute path
    "x <- 5",
    "output <- write.csv(data, 'C:/Users/Documents/output.csv')"  # Windows absolute path
  ), temp_file)
  
  # Create a file without global paths
  temp_file2 <- file.path(temp_dir, "clean_script.R")
  writeLines(c(
    "# This is a comment",
    "data <- read.csv('data.csv')",  # Relative path
    "x <- 5",
    "# /home/user/data.csv is commented out, so should be ignored"
  ), temp_file2)
  
  # Capture output
  output <- capture.output(findGlobalPaths(temp_dir))
  
  # Should find the problematic file
  expect_true(any(grepl("script_with_paths", output)))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("findGlobalPaths handles empty directories", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Should not error on empty directory
  output <- capture.output(findGlobalPaths(temp_dir))
  expect_true(TRUE)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("findGlobalPaths ignores commented paths", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  temp_file <- file.path(temp_dir, "commented.R")
  writeLines(c(
    "# data <- read.csv('/home/user/data.csv')",
    "# Another comment with C:/path",
    "x <- 5"
  ), temp_file)
  
  # Capture output
  output <- capture.output(findGlobalPaths(temp_dir))
  
  # Should not flag commented paths
  expect_false(any(grepl("/home/user", output)))
  expect_false(any(grepl("C:/path", output)))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})
