context("Tests for Gradescope-specific functionality")

test_that("calcGradesForGradescope rejects bad arguments", {
  # Check that it throws error when you don't supply all arguments
  expect_error(calcGradesForGradescope("fake_file.R", "anotherfakeFile.R", "bad_arg"))
  
  # Missing args doesn't fly
  expect_error(calcGradesForGradescope())
})

test_that("calcGradesForGradescope validates file existence", {
  # Should error when submission file doesn't exist
  expect_error(
    calcGradesForGradescope("nonexistent_file.R", "example/grade_hw1.R", which_results = "testing"),
    "submission_file does not exist"
  )
})

test_that("calcGradesForGradescope creates JSON output", {
  # Create a simple test script
  temp_submission <- tempfile(fileext = ".R")
  writeLines(c(
    "x <- 5",
    "y <- 10"
  ), temp_submission)
  
  # Use existing test file
  test_file <- "example/grade_hw1.R"
  
  # Run with testing mode
  result <- calcGradesForGradescope(temp_submission, test_file, which_results = "testing", suppress_warnings = TRUE)
  
  # Check that results.json was created
  expect_true(file.exists("results.json"))
  
  # Read and validate JSON structure
  json_content <- jsonlite::fromJSON("results.json")
  expect_true("tests" %in% names(json_content))
  expect_true(is.list(json_content$tests))
  
  # Cleanup
  unlink(temp_submission)
  if(file.exists("results.json")) unlink("results.json")
})

test_that("calcGradesForGradescope supports Rmd files", {
  skip_if_not_installed("knitr")
  
  # Create a simple Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: \"Test\"",
    "---",
    "",
    "```{r}",
    "x <- 5",
    "y <- 10",
    "```"
  ), temp_rmd)
  
  test_file <- "example/grade_hw1.R"
  
  # Should handle Rmd extraction
  result <- suppressMessages(
    calcGradesForGradescope(temp_rmd, test_file, which_results = "testing", suppress_warnings = TRUE)
  )
  
  expect_true(file.exists("results.json"))
  
  # Cleanup
  unlink(temp_rmd)
  if(file.exists("results.json")) unlink("results.json")
})

test_that("getTestVisibility extracts visibility correctly", {
  expect_equal(getTestVisibility("Question 1 (visible)"), "visible")
  expect_equal(getTestVisibility("Question 2 (hidden)"), "hidden")
  expect_equal(getTestVisibility("Question 3 (after_due_date)"), "after_due_date")
  expect_equal(getTestVisibility("Question 4 (after_published)"), "after_published")
  expect_equal(getTestVisibility("Question 5"), "after_due_date")  # Default
})

test_that("visibility tags are case sensitive", {
  # Should not match if case is wrong
  expect_equal(getTestVisibility("Question (VISIBLE)"), "after_due_date")
  expect_equal(getTestVisibility("Question (Visible)"), "after_due_date")
})

test_that("visibility handles multiple tags", {
  # order of priority is visible > hidden > after_due_date > after_published
  expect_equal(getTestVisibility("Test (visible) (hidden)"), "visible")
  expect_equal(getTestVisibility("Test (hidden) (visible)"), "visible")
})
