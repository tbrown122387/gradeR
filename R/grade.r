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
                                 pattern = "\\.r$", 
                                 ignore.case = T)
  
  atLeastOneBadFile <- FALSE
  for(script in scripts_to_grade) {
    globalPath <- paste(submission_dir, script, sep = "")
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
  for(path in paths ){
    
    # run student's submission in a separate process 
    # https://stackoverflow.com/questions/63744905/attaching-packages-to-a-temporary-search-path-in-r/63746414#63746414
    tmp_full_path <- paste(submission_dir, path, sep = "")
    if(verbose) cat("grading: ", path, "\n")
    # run student's submission in a separate process
    # https://stackoverflow.com/a/63746414/1267833
    rogueScript <- function(source_file_path){
      rogueEnv <- new.env()  
      source(source_file_path, rogueEnv)
      rogueEnv
    }
    # remove previous scriptResults in case an error is triggered and it's never re-created
    if( exists("scriptResults") ) rm(scriptResults)
    
    if( suppress_warnings ){
      tryCatch(
        suppressWarnings(scriptResults <- callr::r(rogueScript, 
                                                   args = list(tmp_full_path), 
                                                   show = TRUE, package = TRUE)),
        error = function(e){
          print(paste0("error: ", e$parent$call))
          print(e$parent$trace)
        },
        message = function(m){
          print(paste0("message: ", m))
        })
    }else{ # not suppressing warnings
      tryCatch(
        scriptResults <- callr::r(rogueScript, 
                                  args = list(tmp_full_path), 
                                  show = TRUE, package = TRUE),
        error = function(e){
          print(paste0("error: ", e$parent$call))
          print(e$parent$trace)
        },
        message = function(m){
          print(paste0("message: ", m))
        },
        warning = function(w){
          print(paste0("warning: ", w))
        })
    }
    
    # test the student's submissions
    # note that scriptResults might not exist if there was an error in the tryCatch block
    if( exists("scriptResults") ){
      lr <- testthat::ListReporter$new()
      out <- testthat::test_file(your_test_file, 
                                 reporter = lr,
                                 env = scriptResults)
      
      # parse the output
      score_data[student_num,1] <- tmp_full_path
      for(q in (1:number_questions)){
        
        # true or false if question was correct
        assertionResults <- lr$results$as_list()[[q]]$results
        success <- all(sapply(assertionResults, 
                              methods::is, 
                              "expectation_success")) 
        
        # TODO incorporate point values
        if(success){
          score_data[student_num, q+1] <- 1
        }else{
          score_data[student_num, q+1] <- 0
        }
      }
      
    }else{
      print("assigning all zeros for this student due to bug in submissions")
      score_data[student_num,1] <- tmp_full_path
    }
    
    # increment 
    student_num <- student_num + 1
  }
  
  # make the column names prettier before returning everything
  colnames(score_data)[-1] <- sapply(trial_test, `[[`, "test")
  return(score_data)
}



#' The grading function for Gradescope.
#'
#' This function grades one R script assignment submission and writes results out to a properly-formatted json file for Gradescope. 
#' @param submission_file the name of the assignment submission file (e.g. "hw1.r")
#' @param test_file the name of the .r file with test_that tests (e.g. "hw1_tests.R")
#' @param which_results Choose either "testing" or "gradescope" If equal to "gradescope" then the json file is written out to the directory that Gradescope expects. Otherwise, results.json is written to your current working directory.
#' @param suppress_warnings If FALSE, warnings are fatal; if set to TRUE, then warnings will not prematurely terminate running of student submission scripts. 
#' @keywords calcGradesForGradescope Gradescope 
#' @export
calcGradesForGradescope <- function(submission_file, 
                                    test_file, 
                                    which_results = "gradescope",
                                    suppress_warnings = TRUE){
  
  if(!(which_results %in% c("gradescope", "testing")))
    stop("argument which_filename incorrectly specified")
  json_filename <- ifelse(which_results == "gradescope", "/autograder/results/results.json", "results.json")
  
  if(missing(test_file)) 
    stop("must have a test file")
  
  number_tests <- length(testthat::test_file(test_file, 
                                             reporter = "minimal"))
  if(number_tests == 0)
    stop("you need at least one graded question")
  
  # run student's submission in a separate process
  # https://stackoverflow.com/a/63746414/1267833
  rogueScript <- function(source_file_path){
    rogueEnv <- new.env()  
    source(source_file_path, rogueEnv)
    rogueEnv
  }
  if( suppress_warnings ){
    tryCatch(
      suppressWarnings(scriptResults <- callr::r(rogueScript, 
                                                 args = list(submission_file), 
                                                 show = TRUE, package = TRUE)),
      error = function(e){
        print(paste0("error: ", e$parent$call))
        print(e$parent$trace)
      },
      message = function(m){
        print(paste0("message: ", m))
      })
  }else{ # not suppressing warnings
    tryCatch(
      scriptResults <- callr::r(rogueScript, args = list(submission_file), show = TRUE, package = TRUE),
      error = function(e){
        print(paste0("error: ", e$parent$call))
        print(e$parent$trace)
      },
      message = function(m){
        print(paste0("message: ", m))
      },
      warning = function(w){
        print(paste0("warning: ", w))
      })
  }

  

  # test the student's submissions
  # for the time being, each test is worth one point
  lr <- testthat::ListReporter$new()
  out <- testthat::test_file(test_file, 
                             reporter = lr, 
                             env = scriptResults)
  tests <- list()
  tests[["tests"]] <- list()
  raw_results <- lr$results$as_list()
  for(i in 1:number_tests){
    test_name <- raw_results[[i]]$test
    if(  grepl("\\(visible\\)", test_name) ){
        test_visibility <- "visible"
    }else if( grepl("\\(hidden\\)", test_name) ){
        test_visibility <- "hidden"
    }else if(  grepl("\\(after_due_date\\)", test_name) ){
        test_visibility <- "after_due_date"
    }else if( grepl("\\(after_published\\)", test_name) ){
        test_visibility <- "after_published"
    }else{
        test_visibility <- "after_due_date"
    }
    test_max_score <- 1 # TODO generalize
    assertionResults <- raw_results[[i]]$results
    success <- all(sapply(assertionResults, methods::is, "expectation_success"))
    test_score <- ifelse(success, 1, 0)
    tests[["tests"]][[i]] <- list(name = test_name,
                                  score = test_score,
                                  max_score = test_max_score,
                                  visibility = test_visibility)
  }
  
  # now write out all the stuff to a json file
  write(jsonlite::toJSON(tests, auto_unbox = T), file = json_filename)
}


