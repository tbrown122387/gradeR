#' This function finds unreadable files.
#'
#'  A function that finds student submissions with poorly encoded characters
#' @param submission_dir where the assignments are located
#' @keywords calcGrades findBadEncodingFiles
#' @export
#' @examples
#' \donttest{
#' # change paths to *your* paths
#' submissions <- "extdata/assignment1_submissions/"
#' findBadEncodingFiles(submissions) # perhaps ask these students to resubmit
#' }
findBadEncodingFiles <- function(submission_dir){
  scripts_to_grade <- list.files(path = submission_dir, 
                                 recursive = T, 
                                 pattern = "\\.r$", 
                                 ignore.case = T)  
  atLeastOneBadFile <- FALSE
  for(script in scripts_to_grade) {
    globalPath <- paste(submission_dir, script, sep = "")   
    lines <- readLines(globalPath, warn = F)
    badLines <- lines[!validUTF8(readLines(globalPath, warn = F))]
    
    if(length(badLines) > 0) {
      atLeastOneBadFile <- TRUE
      cat("======================================================================\n")
      cat("In ", script, "\n")
      for(elem in badLines) cat(elem, "\n")
    }
  }
  if(atLeastOneBadFile) 
    cat("======================================================================\n")
}


#' Run a student script in an isolated environment.
#'
#' Internal helper function that runs a student's R script in a separate process
#' with its own environment to avoid contaminating the current session.
#' 
#' @param script_path the full path to the student's R script
#' @param suppress_warnings logical; if TRUE, warnings are suppressed
#' @return The environment created by the script, or NULL if execution failed
#' @keywords internal
runStudentScript <- function(script_path, suppress_warnings = TRUE){
  rogueScript <- function(source_file_path){
    rogueEnv <- new.env()  
    source(source_file_path, rogueEnv)
    rogueEnv
  }
  
  scriptResults <- NULL
  if(suppress_warnings){
    tryCatch(
      suppressWarnings(scriptResults <- callr::r(rogueScript, 
                                                 args = list(script_path), 
                                                 show = TRUE, package = TRUE)),
      error = function(e){
        # Silently catch errors - caller will handle NULL return
      },
      message = function(m){
        # Suppress messages
      })
  }else{
    tryCatch(
      scriptResults <- callr::r(rogueScript, 
                                args = list(script_path), 
                                show = TRUE, package = TRUE),
      error = function(e){
        # Silently catch errors - caller will handle NULL return
      },
      message = function(m){
        # Suppress messages
      },
      warning = function(w){
        # Suppress warnings when not suppressing them globally
      })
  }
  
  return(scriptResults)
}


#' Parse test results for a single student.
#'
#' Internal helper function that evaluates test results and returns scores.
#' 
#' @param test_results the ListReporter results object
#' @param number_questions the number of questions/tests
#' @return A numeric vector of scores (0 or 1 for each question)
#' @keywords internal
parseTestResults <- function(test_results, number_questions){
  scores <- numeric(number_questions)
  
  for(q in seq_len(number_questions)){
    assertionResults <- test_results$results$as_list()[[q]]$results
    success <- all(sapply(assertionResults, 
                          methods::is, 
                          "expectation_success")) 
    scores[q] <- if(success) 1 else 0
  }
  
  return(scores)
}


#' Determine test visibility from test name.
#'
#' Internal helper function that extracts visibility setting from test names
#' based on Gradescope conventions.
#' 
#' @param test_name the name of the test
#' @return A string: "visible", "hidden", "after_due_date", or "after_published"
#' @keywords internal
getTestVisibility <- function(test_name){
  if(grepl("\\(visible\\)", test_name)){
    return("visible")
  }else if(grepl("\\(hidden\\)", test_name)){
    return("hidden")
  }else if(grepl("\\(after_due_date\\)", test_name)){
    return("after_due_date")
  }else if(grepl("\\(after_published\\)", test_name)){
    return("after_published")
  }else{
    return("after_due_date")
  }
}


#' Extract criterion label from assertion.
#'
#' Internal helper function that extracts custom labels from test expectations.
#' 
#' @param assertion the expectation assertion object
#' @param default_label fallback label if none found
#' @return A string with the criterion label
#' @keywords internal
extractCriterionLabel <- function(assertion, default_label){
  if(is.null(assertion$srcref)){
    return(default_label)
  }
  
  src_text <- paste(as.character(assertion$srcref), collapse = " ")
  
  if(!grepl('label\\s*=\\s*["\']', src_text)){
    return(default_label)
  }
  
  label_match <- regmatches(src_text, regexpr('label\\s*=\\s*["\']([^"\']+)["\']', src_text, perl = TRUE))
  if(length(label_match) > 0){
    sub('.*label\\s*=\\s*["\']([^"\']+)["\'].*', '\\1', label_match)
  }else{
    default_label
  }
}


#' Extract point value from criterion message.
#'
#' Internal helper function that extracts point values from criterion labels
#' formatted as "[2pts]" or "(2pts)".
#' 
#' @param criterion_msg the criterion message that may contain point specification
#' @return A list with components: pts (numeric) and cleaned_msg (string without point spec)
#' @keywords internal
extractPointValue <- function(criterion_msg){
  pts_match <- regmatches(criterion_msg, regexpr('\\[([0-9]+)pts?\\]|\\(([0-9]+)pts?\\)', criterion_msg, perl = TRUE))
  
  if(length(pts_match) > 0){
    pts <- as.numeric(gsub('\\[|\\]|\\(|\\)|pts?', '', pts_match))
    cleaned_msg <- gsub('\\s*\\[([0-9]+)pts?\\]|\\s*\\(([0-9]+)pts?\\)', '', criterion_msg)
  }else{
    pts <- 1
    cleaned_msg <- criterion_msg
  }
  
  list(pts = pts, cleaned_msg = cleaned_msg)
}


#' Process assertions for a single test.
#'
#' Internal helper function that processes all assertions for a test and
#' computes scores and messages.
#' 
#' @param assertion_results list of assertion results from testthat
#' @return A list with components: score, max_score, and output
#' @keywords internal
processTestAssertions <- function(assertion_results){
  test_score <- 0
  test_max_score <- 0
  criterion_messages <- c()
  
  for(j in seq_along(assertion_results)){
    assertion <- assertion_results[[j]]
    
    # Extract custom label or create default message
    custom_msg <- extractCriterionLabel(assertion, paste0("Criterion ", j))
    
    # Extract point value from label
    point_info <- extractPointValue(custom_msg)
    criterion_pts <- point_info$pts
    custom_msg <- point_info$cleaned_msg
    
    test_max_score <- test_max_score + criterion_pts
    
    # Determine pass/fail status and update score
    if(methods::is(assertion, "expectation_success")){
      test_score <- test_score + criterion_pts
      criterion_messages <- c(criterion_messages, 
                              paste0("+", criterion_pts, " (test passed): ", custom_msg))
    }else if(methods::is(assertion, "expectation_failure")){
      criterion_messages <- c(criterion_messages, 
                              paste0("+0 (*****test failed*****): ", custom_msg))
    }
  }
  
  # Build output string
  output_text <- if(length(criterion_messages) > 0){
    paste(criterion_messages, collapse = "\n")
  }else{
    "No criteria evaluated"
  }
  
  list(score = test_score, max_score = test_max_score, output = output_text)
}


#' This function finds files with global file paths.
#'
#'  A function that finds student submissions that refer to machine-specific file paths
#' @param submission_dir where the assignments are located
#' @keywords calcGrades findGlobalPaths
#' @export
#' @examples
#' \donttest{
#' # change paths to *your* paths
#' submissions <- "extdata/assignment1_submissions/"
#' findGlobalPaths(submissions) # perhaps ask these students to resubmit
#' }
findGlobalPaths <- function(submission_dir) {
  scripts_to_grade <- list.files(path = submission_dir, 
                                 recursive = T, 
                                 pattern = "\\.R$", 
                                 ignore.case = T)
  
  atLeastOneBadFile <- FALSE
  for(script in scripts_to_grade) {
    globalPath <- file.path(submission_dir, script)
    lines <- readLines(globalPath, warn = F)  
    badLines <- suppressWarnings(lines[grep("^[^#].+[\\\\/]+", lines)])
    
    if(length(badLines) > 0) {
      atLeastOneBadFile <- TRUE
      cat("======================================================================\n")
      cat("In ", script, "\n")
      for(elem in badLines) cat(elem, "\n")
    }
  }
  if(atLeastOneBadFile) 
    cat("======================================================================\n")
}


#' The grading function.
#'
#' This function grades a bunch of R script assignments 
#' @param submission_dir where the assignments are located
#' @param your_test_file the path to your testthat test file (e.g. grade_hw1.R)
#' @param suppress_warnings warning handlers prevent code from being run after they catch something. Suppress this behavior by setting this argument to TRUE.
#' @param verbose set to true if you want to print the name of the file as it's being ran
#' @return a data.frame of all the scores for each student
#' @keywords calcGrades
#' @export
#' @examples
#' \donttest{
#' # change paths to *your* paths
#' submissions <- "extdata/example/assignment1_submissions/"
#' my_test_file <- system.file("extdata/example", "grade_hw1.R", package = "gradeR")
#' results <- calcGrades(submissions, my_test_file)
#' }
calcGrades <- function(submission_dir, your_test_file, suppress_warnings = TRUE, verbose = FALSE){

  if(missing(submission_dir) | missing(your_test_file)) 
    stop("the first two arguments are required")
  
  paths <- list.files(path = submission_dir, 
                      recursive = T, 
                      pattern = "\\.r$", 
                      ignore.case = T)
  
  trial_test <- testthat::test_file(your_test_file, reporter = "minimal")
  number_questions <- length(trial_test)

  if(number_questions == 0)
    stop("You need at least one graded question")
  
  number_students <- length(paths)
  score_data <- data.frame("id" = vector(mode = "character", length = number_students), 
                           matrix(data = 0, nrow = number_students, 
                                  ncol = number_questions),
                           stringsAsFactors = F)
  
  student_num <- 1
  for(path in paths){
    tmp_full_path <- paste(submission_dir, path, sep = "")
    if(verbose) cat("grading: ", path, "\n")
    
    # Run student's submission in a separate process
    scriptResults <- runStudentScript(tmp_full_path, suppress_warnings)
    
    # Test the student's submission
    score_data[student_num, 1] <- tmp_full_path
    
    if(!is.null(scriptResults)){
      lr <- testthat::ListReporter$new()
      out <- testthat::test_file(your_test_file, 
                                 reporter = lr,
                                 env = scriptResults)
      
      # Parse the output and store scores
      scores <- parseTestResults(lr, number_questions)
      score_data[student_num, -1] <- scores
    }else{
      print("assigning all zeros for this student due to bug in submissions")
      # Scores already initialized to 0
    }
    
    student_num <- student_num + 1
  }
  
  # make the column names prettier before returning everything
  colnames(score_data)[-1] <- sapply(trial_test, `[[`, "test")
  return(score_data)
}



#' The grading function for Gradescope.
#'
#' This function grades one R script assignment submission and writes results out to a properly-formatted json file for Gradescope.
#' Supports R scripts (.r, .R) as well as R Markdown (.Rmd) and Quarto (.qmd) documents.
#' 
#' @param submission_file the path to the assignment submission file (e.g. "hw1.r", "hw1.Rmd", or "hw1.qmd"). For Rmd/Qmd files, R code will be automatically extracted.
#' @param test_file the path to the .r file with test_that tests (e.g. "hw1_tests.R")
#' @param which_results Choose either "testing" or "gradescope". If equal to "gradescope", the json file is written to /autograder/results/results.json. Otherwise, results.json is written to your current working directory.
#' @param suppress_warnings If FALSE, warnings are fatal; if set to TRUE, warnings will not prematurely terminate running of student submission scripts.
#' @return Invisibly returns NULL. The function's primary purpose is the side effect of writing a JSON results file.
#' @keywords calcGradesForGradescope Gradescope 
#' @export
#' @examples
#' \dontrun{
#' # For local testing
#' calcGradesForGradescope("student_hw1.r", "hw1_tests.R", which_results = "testing")
#' 
#' # For Gradescope autograder (inside Gradescope environment)
#' # calcGradesForGradescope("hw1.r", "hw1_tests.R", which_results = "gradescope")
#' 
#' # Works with R Markdown files too
#' # calcGradesForGradescope("student_hw1.Rmd", "hw1_tests.R", which_results = "testing")
#' }
calcGradesForGradescope <- function(submission_file, 
                                    test_file, 
                                    which_results = "gradescope",
                                    suppress_warnings = TRUE){
  
  if(!(which_results %in% c("gradescope", "testing")))
    stop("argument which_results incorrectly specified")
  json_file <- ifelse(which_results == "gradescope", "/autograder/results/results.json", "results.json")
  
  if(missing(test_file)) 
    stop("must have a test file")
  
  number_tests <- length(testthat::test_file(test_file, 
                                             reporter = "minimal"))
  if(number_tests == 0)
    stop("you need at least one graded question")
  
  # Validate submission file exists
  if(!file.exists(submission_file))
    stop("submission_file does not exist: ", submission_file)
  
  # Check if submission is a Qmd/Rmd document
  file_ext <- tools::file_ext(submission_file)
  is_rmd_qmd <- tolower(file_ext) %in% c("qmd","rmd")
  
  # If Quarto, extract R code to temporary file
  temp_r_file <- NULL
  if(is_rmd_qmd){
    temp_r_file <- tempfile(fileext = ".R")
    tryCatch({
      knitr::purl(submission_file, output = temp_r_file, quiet = TRUE)
      submission_file <- temp_r_file  # Use extracted R code for evaluation
    }, error = function(e){
      stop("Failed to extract R code from Rmd/Qmd document: ", e$message)
    })
  }
  
  # Ensure cleanup of temp file on exit
  if(!is.null(temp_r_file)){
    on.exit(unlink(temp_r_file), add = TRUE)
  }
  
  # Run student's submission in a separate process
  scriptResults <- runStudentScript(submission_file, suppress_warnings)
  
  # Test the student's submissions
  if(is.null(scriptResults)){
    # Don't write output - let Gradescope handle the failure
    return(invisible(NULL))
  }
  
  lr <- testthat::ListReporter$new()
  out <- testthat::test_file(test_file, 
                             reporter = lr, 
                             env = scriptResults)
  tests <- list()
  tests[["tests"]] <- list()
  raw_results <- lr$results$as_list()
  
  for(i in seq_len(number_tests)){
    test_name <- raw_results[[i]]$test
    test_visibility <- getTestVisibility(test_name)
    assertion_results <- raw_results[[i]]$results
    
    # Process all assertions for this test
    test_info <- processTestAssertions(assertion_results)
    
    tests[["tests"]][[i]] <- list(name = test_name,
                                  score = test_info$score,
                                  max_score = test_info$max_score,
                                  visibility = test_visibility,
                                  output = test_info$output)
  }
  
  # now write out all the stuff to a json file
  jsonlite::write_json(tests, path = json_file, auto_unbox = TRUE, pretty = TRUE)
}



#' The function for analyzing and summarizing R test scripts.
#'
#' This function scans a given test script and summarizes the number of tests and test criteria in the script, as well as point values.
#' @param script_path the name of the .r file containing tests tests (e.g. "hw1_tests.R")
#' @keywords getTestScriptReport getPrettyReport
#' @export
getTestScriptReport <- function(script_path) {
  # Read and parse the R script
  script_content <- readLines(script_path, warn = FALSE)
  parsed <- parse(text = script_content, keep.source = TRUE)
  
  # Initialize counters
  test_count <- 0
  criteria_per_test <- list()
  expect_function_counts <- list()
  total_points <- 0
  test_details <- list()
  
  # Helper function to extract points from label
  extract_points <- function(label_expr) {
    if (is.null(label_expr)) return(1)
    
    # Evaluate the label expression if it's a call
    label_text <- tryCatch({
      if (is.character(label_expr)) {
        label_expr
      } else {
        as.character(label_expr)
      }
    }, error = function(e) "")
    
    # Look for pattern like [2pts] or [3pts]
    match <- regexpr("\\[(\\d+)pts?\\]", label_text, ignore.case = TRUE)
    if (match > 0) {
      points_str <- regmatches(label_text, match)
      points <- as.numeric(gsub("\\[|pts?\\]", "", points_str, ignore.case = TRUE))
      return(points)
    }
    return(1)  # Default value
  }
  
  # Helper function to recursively find expect_ calls
  find_expect_calls <- function(expr) {
    expect_calls <- list()
    
    if (is.call(expr)) {
      func_name <- as.character(expr[[1]])
      
      # Check if this is an expect_ function
      if (grepl("^expect_", func_name)) {
        # Extract label argument if present
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
      
      # Recursively search in all arguments (only if there are arguments)
      if (length(expr) > 1) {
        args_list <- as.list(expr[-1])
        for (arg in args_list) {
          expect_calls <- c(expect_calls, find_expect_calls(arg))
        }
      }
    }
    
    return(expect_calls)
  }
  
  # Parse the expression tree
  for (i in seq_along(parsed)) {
    expr <- parsed[[i]]
    
    if (is.call(expr) && as.character(expr[[1]]) == "test_that") {
      test_count <- test_count + 1
      
      # Extract test description
      test_desc <- if (length(expr) >= 2) as.character(expr[[2]]) else "Unnamed test"
      
      # Find all expect_ calls within this test_that (check if body exists)
      expect_calls <- if (length(expr) >= 3) {
        find_expect_calls(expr[[3]])
      } else {
        list()
      }
      
      # Count criteria for this test
      criteria_count <- length(expect_calls)
      criteria_per_test[[test_count]] <- criteria_count
      
      # Sum points for this test
      #test_points <- sum(sapply(expect_calls, function(x) x$points))
      test_points <- if (length(expect_calls) > 0) {
        sum(sapply(expect_calls, function(x) x$points))
      } else {
        0
      }
      
      # Count each expect_ function type
      for (call in expect_calls) {
        func_name <- call$function_name
        if (is.null(expect_function_counts[[func_name]])) {
          expect_function_counts[[func_name]] <- 0
        }
        expect_function_counts[[func_name]] <- expect_function_counts[[func_name]] + 1
      }
      
      # Add to total points
      total_points <- total_points + test_points
      
      # Store test details
      test_details[[test_count]] <- list(
        description = test_desc,
        criteria_count = criteria_count,
        points = test_points,
        expect_calls = expect_calls
      )
    }
  }
  
  # Generate summary report
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
  
  pretty_report <- getPrettyReport(report, script_path)
  
  return(pretty_report)
}

#' The function prints a formatted test script analysis.
#'
#' This function is used inside the getTestScriptReport function to produce a formatted report as output.
#' @param report a list object that is produced from the getTestScriptReport function
#' @param script_path a string representing the relative path to the test script
#' @keywords internal
getPrettyReport <- function(report, script_path) {
  cat("=", rep("=", 60), "=\n", sep = "")
  cat("TEST SCRIPT ANALYSIS REPORT\n")
  cat("=", rep("=", 60), "=\n\n", sep = "")
  
  cat(sprintf("Script: %s\n\n", script_path))
  
  cat("SUMMARY:\n")
  cat(sprintf("  Total test_that calls: %d\n", report$summary$total_tests))
  cat(sprintf("  Total criteria: %d\n", report$summary$total_criteria))
  cat(sprintf("  Total points: %d\n\n", report$summary$total_points))
  
  cat("EXPECT_ FUNCTION USAGE:\n")
  for (func in names(report$expect_function_counts)) {
    cat(sprintf("  %s: %d\n", func, report$expect_function_counts[[func]]))
  }
  cat("\n")
  
  cat("TEST DETAILS:\n")
  for (i in seq_along(report$test_details)) {
    test <- report$test_details[[i]]
    cat(sprintf("  Test %d: %s\n", i, test$description))
    cat(sprintf("    Criteria: %d\n", test$criteria_count))
    cat(sprintf("    Points: %d\n", test$points))
  }
  
  cat("\n", rep("=", 62), "\n", sep = "")
}


# #' The averaging function.
# #'
# #' This function calculates each student's course average based on all of their assignments.
# #' @param table a `data.frame` of student grades. 
# #' @param categories a named list of named lists. See below for more information.
# #' @param studentNameCol a name of a column with student names.
# #' @param drop a list of named lists. See below for more information.
# #' @param traceCalcsForStudent Character scalar giving a student name, or NULL. If provided, additional intermediate calculations are printed for that student only (useful for debugging).
# #' @keywords calculateOverallAverage
# #' @examples
# #' \donttest{
# #' exampleDF <- data.frame(student = c('a','b','c'),
# #'                         test1 = c(50,50,NA),
# #'                         test1LateAdjustment = c(.99,1,1),
# #'                         hw1=c(45,44,43),
# #'                         hw1LateAdjustment = c(1,1,1),
# #'                         hw2=c(50,49,48),
# #'                         hw2LateAdjustment = c(1,1,1))
# #' 
# #' testCols <- data.frame(gradeNames = 'test1', 
# #'                        lateNames = 'test1LateAdjustment', 
# #'                        maxScores = 50)
# #' testCategory <- list(colNames=testCols,
# #'                      catWeight=.6)
# #' hwCols <- data.frame(gradeNames = c('hw1','hw2'), 
# #'                      lateNames = c('hw1LateAdjustment','hw2LateAdjustment'),
# #'                      maxScores = c(50,50))
# #' homeworkCategory <- list(colNames=hwCols,
# #'                          catWeight=.4)
# #' 
# #' cats <- list(tests=testCategory, homeworks=homeworkCategory)
# #' myDrops = list(numToDrop=1, cats='homeworks')
# #' calculateOverallAverage(exampleDF, cats, 'student', myDrops)
# #' calculateOverallAverage(exampleDF, cats, 'student', myDrops, traceCalcsForStudent = 'a')
# #' }
# calculateOverallAverage <- function(table, categories, studentNameCol, 
#                                     drop = NULL, 
#                                     traceCalcsForStudent = NULL){
  
#   ######################
#   # reusable variables #
#   ######################
#   numStudents <- nrow(table)
#   catNames <- names(categories)
#   allAssignmentNames <- unlist(lapply(lapply(categories, '[[', 'colNames'), '[[', 'gradeNames'))
#   allLateNames <- unlist(lapply(lapply(categories, '[[', 'colNames'), '[[', 'lateNames'))
  
#   ###################
#   # performs checks #
#   ###################
#   # check weights sum to 1
#   stopifnot(sum(sapply(categories, '[[', 'catWeight')) == 1)
#   # check dropped categories all exist 
#   if(!is.null(drop)) stopifnot(all(drop$cats %in% names(categories)))
#   # TODO: check the number of drops is strictly less than the number of assignments to drop from
#   # check assignment names in categories all exist in data table
#   assignmentNamesInCats <- unlist(sapply(categories, '[[', 'colNames'))
#   stopifnot(all(allAssignmentNames %in% colnames(table)))
#   stopifnot(all(allLateNames %in% colnames(table)))
#   # TODO check no assignment is double-listed in two categorie
#   #TODO check dimension of maxScores in categories (and everything else)
#   # TODO check they're vector too
#   # lateMultColNames
  
  
#   # check names of drop
#   # document return value?
#   # TODO: check breakdown is a partition
#   # add option to see specific student process
  
  
#   ###############################
#   # fill NA grades s with zeros #
#   ###############################
#   # TODO make this only work on grade columns
#   table[is.na(table)] <- 0
  
#   ##############################
#   # convert scores to percents #
#   # also deduct lateness       #
#   ##############################
#   for(myCat in categories){
    
#     gradeColumnNames <- myCat$colNames$gradeNames
#     for(i in seq_along(gradeColumnNames) ){
      
#       gradeColumnName <- gradeColumnNames[i]
#       maxScore <- myCat$colNames$maxScores[i]
#       stopifnot(length(maxScore) == 1)
      
#       haveLatenessCol <- !is.null(myCat$colNames$lateNames[i])
#       if( haveLatenessCol ){
#         lateMultColumnName <- myCat$colNames$lateNames[i]
#         table[[gradeColumnName]] <- table[[gradeColumnName]] * table[[lateMultColumnName]]
#         table[[gradeColumnName]] <- table[[gradeColumnName]] / maxScore *100
#       }else{
#         table[[gradeColumnName]] <- table[[gradeColumnName]]/ maxScore *100
#       }
#     }
#   }
#   gradesOnlyDF <- subset(table, select = allAssignmentNames)
  
#   ####################################
#   # group data frame into categories #
#   ####################################
#   # https://stackoverflow.com/a/70319902/1267833
#   breakdown <- lapply(lapply(categories, '[[', 'colNames'), '[[', 'gradeNames')
#   map <- rep.int(names(breakdown), lengths(breakdown))
#   names(map) <- unlist(breakdown)
#   indicesForBy <- map[allAssignmentNames]
#   gradesByCategory <- by(t(gradesOnlyDF), indicesForBy, function(x) x, simplify = FALSE)
  
#   ########################################
#   # create a data.frame for each student #
#   ########################################
#   gradesByStudent <- lapply(seq.int(numStudents),
#                             function(stud_num) lapply(gradesByCategory, '[', , stud_num ))
#   names(gradesByStudent) <- table[[studentNameCol]]
#   gradesByStudent <- lapply(gradesByStudent,
#                             function(x) as.data.frame(utils::stack(x)) )
#   if( is.character(traceCalcsForStudent) ){
#     print("---------------------")
#     print(paste("grades for", traceCalcsForStudent, "after lateness adjustment"))
#     print("---------------------")
#     print(gradesByStudent[[traceCalcsForStudent]])
#   }
  
  
#   ##################################################################
#   # sort each student by "negotiable points lost" for each student #
#   ##################################################################
#   addColsSort <- function(df){
#     # add category weights
#     catWeights <- sapply(categories[as.character(gradesByStudent[[1]]$ind)],
#                          '[[',
#                          'catWeight')
#     df$catWeights <- catWeights
    
#     # add dropable column
#     if( is.null(drop) ){
#       df$droppable <- FALSE
#     }else{ # have drops
#       df$droppable <- df$ind %in% drop$cats
#     }
    
#     # add points lost
#     # pointsLostNegotiably lost should be 0 if the assignment isn't droppable
#     df$ind <- as.character(df$ind)
#     numAssignmentsInEachCat <- tapply(df$values, df$ind, length)
#     numAssignmentsInEachCat <- numAssignmentsInEachCat[df$ind]
#     df$pointsLostNegotiably <- df$catWeights*(100-df$values)/numAssignmentsInEachCat
#     df$pointsLostNegotiably <- df$pointsLostNegotiably * df$droppable
    
#     # sort df, drop assignments, return scores only
#     df <- df[order(df$pointsLostNegotiably, decreasing = T),]
#   }
#   gradesByStudent <- lapply(gradesByStudent, addColsSort)
#   if( is.character(traceCalcsForStudent) ){
#     print("---------------------")
#     print(paste("grades for", traceCalcsForStudent))
#     print("---------------------")
#     print(gradesByStudent[[traceCalcsForStudent]])
#   }
  
  
#   ###################
#   # drop bad scores #
#   ###################
#   dropBadScores <- function(df){
#     if(is.null(drop)){
#       keptScores <-  1:(nrow(df))
#     }else{
#       keptScores <- (drop$numToDrop+1):(nrow(df))
#     }
#     df <- subset(df[keptScores,], select = c(values, ind, catWeights))
    
#     # recompute number of assignments in each category
#     # because some may have dropped
#     df$ind <- as.character(df$ind)
#     lengthEachCat <- tapply(df$values, df$ind, length)
#     df$numAssignmentsInEachCat <- lengthEachCat[df$ind]
    
#     df
#   }
#   gradesByStudent <- lapply(gradesByStudent, dropBadScores)
#   if( is.character(traceCalcsForStudent) ){
#     print("---------------------")
#     print(paste("grades for", traceCalcsForStudent))
#     print("---------------------")
#     print(gradesByStudent[[traceCalcsForStudent]])
#   }
  
#   ############################
#   # return  overall averages #
#   ############################
#   finalAverages <- sapply(gradesByStudent, function(df){
#     sum(df$values * df$catWeights / df$numAssignmentsInEachCat)
#   })
#   if( is.character(traceCalcsForStudent) ){
#     print("---------------------")
#     print(paste("final grade for", traceCalcsForStudent))
#     print("---------------------")
#     print(finalAverages[[traceCalcsForStudent]])
#   }
#   return(finalAverages)
# }

