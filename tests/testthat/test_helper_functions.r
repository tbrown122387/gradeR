context("Tests for internal helper functions")

test_that("runStudentScript executes valid scripts", {
  # Create a simple temporary script
  temp_script <- tempfile(fileext = ".R")
  writeLines(c(
    "x <- 5",
    "y <- 10",
    "result <- x + y"
  ), temp_script)
  
  # Run the script
  env <- runStudentScript(temp_script, suppress_warnings = TRUE)
  
  # Check results
  expect_false(is.null(env))
  expect_equal(env$x, 5)
  expect_equal(env$y, 10)
  expect_equal(env$result, 15)
  
  # Cleanup
  unlink(temp_script)
})

test_that("runStudentScript returns NULL on error", {
  # Create a script with an error
  temp_script <- tempfile(fileext = ".R")
  writeLines(c(
    "x <- 5",
    "stop('intentional error')"
  ), temp_script)
  
  # Capture output to suppress error messages
  env <- suppressWarnings(runStudentScript(temp_script, suppress_warnings = TRUE))
  
  # Should return NULL when script fails
  expect_null(env)
  
  # Cleanup
  unlink(temp_script)
})

test_that("runStudentScript handles warnings correctly", {
  # Create a script with warnings
  temp_script <- tempfile(fileext = ".R")
  writeLines(c(
    "x <- 5",
    "warning('test warning')",
    "y <- 10"
  ), temp_script)
  
  # With suppress_warnings = TRUE, should succeed
  env1 <- suppressWarnings(runStudentScript(temp_script, suppress_warnings = TRUE))
  expect_false(is.null(env1))
  expect_equal(env1$x, 5)
  expect_equal(env1$y, 10)
  
  # With suppress_warnings = FALSE, should also succeed (warnings don't stop execution)
  env2 <- suppressWarnings(runStudentScript(temp_script, suppress_warnings = FALSE))    
  expect_false(is.null(env2))
  expect_equal(env2$x, 5)
  expect_equal(env2$y, 10)
  
  # Cleanup
  unlink(temp_script)
})

test_that("parseTestResults correctly evaluates test outcomes", {
  # Create a mock ListReporter structure
  mock_results <- list(
    results = list(
      as_list = function() {
        list(
          # Test 1: All successes
          list(results = list(
            structure(list(), class = "expectation_success"),
            structure(list(), class = "expectation_success")
          )),
          # Test 2: Has failures
          list(results = list(
            structure(list(), class = "expectation_success"),
            structure(list(), class = "expectation_failure")
          )),
          # Test 3: All successes
          list(results = list(
            structure(list(), class = "expectation_success")
          ))
        )
      }
    )
  )
  
  scores <- parseTestResults(mock_results, 3)
  
  expect_equal(length(scores), 3)
  expect_equal(scores[1], 1)  # All passed
  expect_equal(scores[2], 0)  # Has failure
  expect_equal(scores[3], 1)  # All passed
})

test_that("getTestVisibility returns correct visibility", {
  expect_equal(getTestVisibility("Test name (visible)"), "visible")
  expect_equal(getTestVisibility("Test name (hidden)"), "hidden")
  expect_equal(getTestVisibility("Test name (after_due_date)"), "after_due_date")
  expect_equal(getTestVisibility("Test name (after_published)"), "after_published")
  expect_equal(getTestVisibility("Test with no visibility tag"), "after_due_date")
  expect_equal(getTestVisibility("Multiple (visible) (hidden) tags"), "visible")  # First match wins
})

test_that("extractCriterionLabel extracts labels correctly", {
  # Mock assertion with srcref containing label
  mock_assertion_with_label <- list(
    srcref = structure("expect_equal(x, 5, label = 'Check x value')", class = "srcref")
  )
  
  result <- extractCriterionLabel(mock_assertion_with_label, "Default Label")
  expect_equal(result, "Check x value")
  
  # Mock assertion without label
  mock_assertion_no_label <- list(
    srcref = structure("expect_equal(x, 5)", class = "srcref")
  )
  
  result <- extractCriterionLabel(mock_assertion_no_label, "Default Label")
  expect_equal(result, "Default Label")
  
  # Mock assertion with NULL srcref
  mock_assertion_null <- list(srcref = NULL)
  result <- extractCriterionLabel(mock_assertion_null, "Default Label")
  expect_equal(result, "Default Label")
})

test_that("extractPointValue extracts points and cleans message", {
  # Test with [2pts] format
  result1 <- extractPointValue("Question 1 [2pts]")
  expect_equal(result1$pts, 2)
  expect_equal(result1$cleaned_msg, "Question 1")
  
  # Test with (3pts) format
  result2 <- extractPointValue("Check output (3pts)")
  expect_equal(result2$pts, 3)
  expect_equal(result2$cleaned_msg, "Check output")
  
  # Test with [1pt] singular
  result3 <- extractPointValue("Simple test [1pt]")
  expect_equal(result3$pts, 1)
  expect_equal(result3$cleaned_msg, "Simple test")
  
  # Test without point specification
  result4 <- extractPointValue("No points specified")
  expect_equal(result4$pts, 1)  # Default
  expect_equal(result4$cleaned_msg, "No points specified")
  
  # Test with multiple digit points
  result5 <- extractPointValue("Big question [10pts]")
  expect_equal(result5$pts, 10)
  expect_equal(result5$cleaned_msg, "Big question")
})

test_that("processTestAssertions calculates scores correctly", {
  # Create mock assertions
  mock_assertions <- list(
    # Success with default 1 point
    structure(list(srcref = NULL), class = "expectation_success"),
    # Success with 2 points
    structure(list(
      srcref = structure("expect_equal(x, 5, label = 'Check x [2pts]')", class = "srcref")
    ), class = "expectation_success"),
    # Failure with 3 points
    structure(list(
      srcref = structure("expect_equal(y, 10, label = 'Check y [3pts]')", class = "srcref")
    ), class = "expectation_failure")
  )
  
  result <- processTestAssertions(mock_assertions)
  
  expect_equal(result$score, 3)  # 1 + 2 + 0
  expect_equal(result$max_score, 6)  # 1 + 2 + 3
  expect_true(grepl("\\+1 \\(test passed\\)", result$output))
  expect_true(grepl("\\+2 \\(test passed\\)", result$output))
  expect_true(grepl("\\+0 \\(\\*\\*\\*\\*\\*test failed\\*\\*\\*\\*\\*\\)", result$output))
})

test_that("processTestAssertions handles empty assertion list", {
  result <- processTestAssertions(list())
  
  expect_equal(result$score, 0)
  expect_equal(result$max_score, 0)
  expect_equal(result$output, "No criteria evaluated")
})

test_that("processTestAssertions handles all successes", {
  mock_assertions <- list(
    structure(list(srcref = NULL), class = "expectation_success"),
    structure(list(srcref = NULL), class = "expectation_success")
  )
  
  result <- processTestAssertions(mock_assertions)
  
  expect_equal(result$score, 2)
  expect_equal(result$max_score, 2)
  expect_false(grepl("failed", result$output))
})

test_that("processTestAssertions handles all failures", {
  mock_assertions <- list(
    structure(list(srcref = NULL), class = "expectation_failure"),
    structure(list(srcref = NULL), class = "expectation_failure")
  )
  
  result <- processTestAssertions(mock_assertions)
  
  expect_equal(result$score, 0)
  expect_equal(result$max_score, 2)
  expect_true(grepl("failed", result$output))
})
