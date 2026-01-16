context("Integration tests for main grading functions")

test_that("calcGrades requires both arguments", {
  # Check that it throws error when you don't supply all arguments
  expect_error(calcGrades())
  expect_error(calcGrades(submission_dir = "example/assignment1_submissions/"))
  expect_error(calcGrades(your_test_file = "example/grade_hw1.R"))
})


test_that("calcGrades produces correct output structure", {
  submissions <- "example/assignment1_submissions/"
  my_test_file <- "example/grade_hw1.R"
  results <- calcGrades(submissions, my_test_file, suppress_warnings = TRUE)
  
  # Check structure
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 2)  # Two students
  expect_equal(ncol(results), 3)  # ID + 2 questions
  
  # Check student paths
  expect_equal(results[1, 1], "example/assignment1_submissions/student1/hw1.R")
  expect_equal(results[2, 1], "example/assignment1_submissions/student2/myhw1.r")
  
  # Check scores
  expect_equal(results[1, 2], 1)
  expect_equal(results[2, 2], 0)
  expect_equal(results[1, 3], 1)
  expect_equal(results[2, 3], 1)
  
  # Check column names
  expect_true("id" %in% names(results))
})


test_that("calcGrades handles multiple questions correctly", {
  submissions <- "example2/assignment2_submissions/"
  my_test_file <- "example2/grade_hw2.R"
  results <- calcGrades(submissions, my_test_file, suppress_warnings = TRUE)
  
  # Verify structure
  expect_equal(nrow(results), 2)
  expect_equal(ncol(results), 4)  # ID + 3 questions
  
  # First student - all correct
  expect_equal(results[1, 1], "example2/assignment2_submissions/student1/hw1.R")
  expect_equal(results[1, 2], 1)
  expect_equal(results[1, 3], 1)
  expect_equal(results[1, 4], 1)
  
  # Second student - mixed results
  expect_equal(results[2, 1], "example2/assignment2_submissions/student2/myhw1.r")
  expect_equal(results[2, 2], 0)
  expect_equal(results[2, 3], 1)
  expect_equal(results[2, 4], 0)
})


test_that("calcGrades handles multiple assertions per test", {
  submissions <- "example3/assignment3_submissions/"
  my_test_file <- "example3/grade_hw3.R"
  results <- calcGrades(submissions, my_test_file, suppress_warnings = TRUE)
  
  # Verify paths
  expect_equal(results[1, 1], "example3/assignment3_submissions/student1/hw1.R")
  expect_equal(results[2, 1], "example3/assignment3_submissions/student2/myhw1.r")
  
  # Both students should fail when any assertion fails
  expect_equal(results[1, 2], 0)
  expect_equal(results[2, 2], 0)
})

test_that("calcGrades verbose mode prints progress", {
  submissions <- "example/assignment1_submissions/"
  my_test_file <- "example/grade_hw1.R"
  
  # Capture output with verbose = TRUE
  output <- capture.output(
    results <- calcGrades(submissions, my_test_file, suppress_warnings = TRUE, verbose = TRUE)
  )
  
  # Should print grading progress
  expect_true(any(grepl("grading:", output)))
})

test_that("calcGrades returns zero scores for failed scripts", {
  # Create temporary directory with a broken script
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  broken_script <- file.path(temp_dir, "broken.R")
  writeLines(c(
    "x <- 5",
    "stop('intentional error')"
  ), broken_script)
  
  test_file <- "example/grade_hw1.R"
  
  # Should handle error gracefully
  output <- capture.output(
    results <- suppressMessages(calcGrades(temp_dir, test_file, suppress_warnings = TRUE))
  )
  
  # Should have one row with zeros
  expect_equal(nrow(results), 1)
  expect_true(all(results[1, -1] == 0))  # All scores should be 0
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("calcGrades handles empty submission directories", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  test_file <- "example/grade_hw1.R"
  
  # Should handle empty directory
  results <- calcGrades(temp_dir, test_file, suppress_warnings = TRUE)
  
  # Should return empty data frame with correct structure
  expect_equal(nrow(results), 0)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("calcGrades error handling with suppress_warnings", {
  submissions <- "example/assignment1_submissions/"
  my_test_file <- "example/grade_hw1.R"
  
  # Test with warnings suppressed
  results_suppressed <- calcGrades(submissions, my_test_file, suppress_warnings = TRUE)
  expect_true(is.data.frame(results_suppressed))
  
  # Test with warnings not suppressed
  results_not_suppressed <- calcGrades(submissions, my_test_file, suppress_warnings = FALSE)
  expect_true(is.data.frame(results_not_suppressed))
  
  # Results should be the same
  expect_equal(results_suppressed, results_not_suppressed)
})