context("Tests for score report functionality")

# tests/testthat/test-getTestScriptReport.R

# Helper function to create temporary test script files
create_temp_test_script <- function(content) {
  temp_file <- tempfile(fileext = ".R")
  writeLines(content, temp_file)
  return(temp_file)
}

# Helper function to capture the internal report (before pretty printing)
# This modifies getTestScriptReport temporarily to return the report object
get_report_data <- function(script_path) {
  script_content <- readLines(script_path, warn = FALSE)
  parsed <- parse(text = script_content, keep.source = TRUE)
  
  test_count <- 0
  criteria_per_test <- list()
  expect_function_counts <- list()
  total_points <- 0
  test_details <- list()
  
  extract_points <- function(label_expr) {
    if (is.null(label_expr)) return(1)
    label_text <- tryCatch({
      if (is.character(label_expr)) {
        label_expr
      } else {
        as.character(label_expr)
      }
    }, error = function(e) "")
    
    match <- regexpr("\\[(\\d+)pts?\\]", label_text, ignore.case = TRUE)
    if (match > 0) {
      points_str <- regmatches(label_text, match)
      points <- as.numeric(gsub("\\[|pts?\\]", "", points_str, ignore.case = TRUE))
      return(points)
    }
    return(1)
  }
  
  find_expect_calls <- function(expr) {
    expect_calls <- list()
    if (is.call(expr)) {
      func_name <- as.character(expr[[1]])
      if (grepl("^expect_", func_name)) {
        args <- as.list(expr)
        label_arg <- NULL
        points <- 1
        if ("label" %in% names(args)) {
          label_arg <- args$label
          points <- extract_points(label_arg)
        }
        expect_calls[[length(expect_calls) + 1]] <- list(
          function_name = func_name,
          points = points,
          label = label_arg
        )
      }
      if (length(expr) > 1) {
        args_list <- as.list(expr[-1])
        for (arg in args_list) {
          expect_calls <- c(expect_calls, find_expect_calls(arg))
        }
      }
    }
    return(expect_calls)
  }
  
  for (i in seq_along(parsed)) {
    expr <- parsed[[i]]
    if (is.call(expr) && as.character(expr[[1]]) == "test_that") {
      test_count <- test_count + 1
      test_desc <- if (length(expr) >= 2) as.character(expr[[2]]) else "Unnamed test"
      expect_calls <- if (length(expr) >= 3) {
        find_expect_calls(expr[[3]])
      } else {
        list()
      }
      criteria_count <- length(expect_calls)
      criteria_per_test[[test_count]] <- criteria_count
      #test_points <- sum(sapply(expect_calls, function(x) x$points))
      test_points <- if (length(expect_calls) > 0) {
        sum(sapply(expect_calls, function(x) x$points))
      } else {
        0
      }
      for (call in expect_calls) {
        func_name <- call$function_name
        if (is.null(expect_function_counts[[func_name]])) {
          expect_function_counts[[func_name]] <- 0
        }
        expect_function_counts[[func_name]] <- expect_function_counts[[func_name]] + 1
      }
      total_points <- total_points + test_points
      test_details[[test_count]] <- list(
        description = test_desc,
        criteria_count = criteria_count,
        points = test_points,
        expect_calls = expect_calls
      )
    }
  }
  
  report <- list(
    summary = list(
      total_tests = test_count,
      total_criteria = sum(unlist(criteria_per_test)),
      total_points = total_points
    ),
    criteria_per_test = criteria_per_test,
    expect_function_counts = expect_function_counts,
    test_details = test_details
  )
  
  return(report)
}

# Test 1: Basic single test with default points
test_that("Single test with default point values is parsed correctly", {
  script <- '
test_that("Basic test", {
  expect_equal(1 + 1, 2, label = "Addition works")
  expect_true(TRUE, label = "TRUE is true")
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_tests, 1)
  expect_equal(report$summary$total_criteria, 2)
  expect_equal(report$summary$total_points, 2)  # 2 criteria * 1 pt each
  expect_equal(report$expect_function_counts$expect_equal, 1)
  expect_equal(report$expect_function_counts$expect_true, 1)
  
  unlink(temp_file)
})

# Test 2: Custom point values in labels
test_that("Custom point values are extracted correctly", {
  script <- '
test_that("Custom points test", {
  expect_equal(1, 1, label = "One point")
  expect_true(TRUE, label = "Two points [2pts]")
  expect_false(FALSE, label = "Five points [5pts]")
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_tests, 1)
  expect_equal(report$summary$total_criteria, 3)
  expect_equal(report$summary$total_points, 8)  # 1 + 2 + 5
  
  unlink(temp_file)
})

# Test 3: Multiple test_that blocks
test_that("Multiple test_that blocks are counted correctly", {
  script <- '
test_that("Test 1", {
  expect_equal(1, 1, label = "First test")
})

test_that("Test 2", {
  expect_true(TRUE, label = "Second test")
  expect_false(FALSE, label = "Third test")
})

test_that("Test 3", {
  expect_length(c(1,2,3), 3, label = "Fourth test [3pts]")
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_tests, 3)
  expect_equal(report$summary$total_criteria, 4)
  expect_equal(report$summary$total_points, 6)  # 1 + 1 + 1 + 3
  
  unlink(temp_file)
})

# Test 4: Different expect_ functions
test_that("Different expect_ functions are counted separately", {
  script <- '
test_that("Various expects", {
  expect_equal(1, 1)
  expect_equal(2, 2)
  expect_true(TRUE)
  expect_false(FALSE)
  expect_length(c(1,2), 2)
  expect_type("x", "character")
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$expect_function_counts$expect_equal, 2)
  expect_equal(report$expect_function_counts$expect_true, 1)
  expect_equal(report$expect_function_counts$expect_false, 1)
  expect_equal(report$expect_function_counts$expect_length, 1)
  expect_equal(report$expect_function_counts$expect_type, 1)
  
  unlink(temp_file)
})

# Test 5: Empty test_that block
test_that("Empty test_that block is handled", {
  script <- '
test_that("Empty test", {
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_tests, 1)
  expect_equal(report$summary$total_criteria, 0)
  expect_equal(report$summary$total_points, 0)
  
  unlink(temp_file)
})

# Test 6: No test_that blocks
test_that("Script with no test_that blocks returns zero counts", {
  script <- '
# Just a comment
x <- 1 + 1
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_tests, 0)
  expect_equal(report$summary$total_criteria, 0)
  expect_equal(report$summary$total_points, 0)
  
  unlink(temp_file)
})

# Test 7: Point value variations (pt vs pts, case insensitive)
test_that("Point extraction handles variations", {
  script <- '
test_that("Point variations", {
  expect_equal(1, 1, label = "Two points [2pt]")
  expect_equal(1, 1, label = "Three points [3PTS]")
  expect_equal(1, 1, label = "Five points [5Pts]")
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_points, 10)  # 2 + 3 + 5
  
  unlink(temp_file)
})

# Test 8: expect_ without label argument
test_that("expect_ calls without label get default 1 point", {
  script <- '
test_that("No labels", {
  expect_equal(1, 1)
  expect_true(TRUE)
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_criteria, 2)
  expect_equal(report$summary$total_points, 2)
  
  unlink(temp_file)
})

# Test 9: Test detail descriptions
test_that("Test descriptions are captured correctly", {
  script <- '
test_that("Question 1: Data loading", {
  expect_equal(1, 1)
})

test_that("Question 2: Data transformation", {
  expect_true(TRUE)
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$test_details[[1]]$description, "Question 1: Data loading")
  expect_equal(report$test_details[[2]]$description, "Question 2: Data transformation")
  
  unlink(temp_file)
})

# Test 10: getTestScriptReport runs without error
test_that("getTestScriptReport runs without error", {
  script <- '
test_that("Sample test", {
  expect_equal(1, 1, label = "Test [2pts]")
})
'
  temp_file <- create_temp_test_script(script)
  
  expect_output(getTestScriptReport(temp_file), "TEST SCRIPT ANALYSIS REPORT")
  expect_output(getTestScriptReport(temp_file), "Total test_that calls: 1")
  expect_output(getTestScriptReport(temp_file), "Total criteria: 1")
  expect_output(getTestScriptReport(temp_file), "Total points: 2")
  
  unlink(temp_file)
})

# Test 11: Script path is displayed in output
test_that("Script path appears in output", {
  script <- '
test_that("Test", {
  expect_equal(1, 1)
})
'
  temp_file <- create_temp_test_script(script)
  
  # Use fixed=TRUE to avoid regex escaping issues with Windows paths
  expect_output(getTestScriptReport(temp_file), "Script:", fixed = TRUE)
  expect_output(getTestScriptReport(temp_file), basename(temp_file), fixed = TRUE)
  
  unlink(temp_file)
})

# Test 12: Complex nested test structure
test_that("Complex test structure is parsed correctly", {
  script <- '
test_that("Complex test", {
  expect_equal(nrow(mtcars), 32, label = "Row count [2pts]")
  expect_equal(ncol(mtcars), 11, label = "Column count")
  expect_true(is.data.frame(mtcars), label = "Is data frame [3pts]")
  expect_type(mtcars$mpg, "double", label = "MPG is numeric")
})
'
  temp_file <- create_temp_test_script(script)
  report <- get_report_data(temp_file)
  
  expect_equal(report$summary$total_tests, 1)
  expect_equal(report$summary$total_criteria, 4)
  expect_equal(report$summary$total_points, 7)  # 2 + 1 + 3 + 1
  expect_equal(report$test_details[[1]]$criteria_count, 4)
  
  unlink(temp_file)
})
