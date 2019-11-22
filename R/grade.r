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
#' submissions <- "extdata/assignment1_submissions/"
#' my_test_file <- system.file("extdata", "grade_hw1.R", package = "gradeR")
#' results <- calcGrades(submissions, my_test_file)
#' }
calcGrades <- function(submission_dir, your_test_file, suppress_warnings = TRUE, verbose = FALSE){

  if(missing(submission_dir) | missing(your_test_file)) 
    stop("the first two arguments are required")
  
  paths <- list.files(path = submission_dir, 
                      recursive = T, 
                      pattern = "\\.r$", 
                      ignore.case = T)
  number_questions <- length(testthat::test_file(your_test_file, 
                                                 reporter = "minimal"))
  number_students <- length(paths)
  score_data <- data.frame("id" = vector(mode = "character", length = number_students), 
                           matrix(data = 0, nrow = number_students, 
                                  ncol = number_questions),
                           stringsAsFactors = F)
  
  student_num <- 1
  for(path in paths ){
    
    # run student's submission
    tmp_full_path <- paste(submission_dir, path, sep = "")  
    testEnv <- new.env()
    if(suppress_warnings){
      tryCatch({
        if(verbose)
          cat("grading: ", path, "\n")
        suppressWarnings(source(tmp_full_path, testEnv))
      }, error = function(e) {
        
        cat("Unable to run: ",  path, "\n")
        cat("Error message: \n")
        message(e)
        cat("\n")
      })
    } else { #not suppressing warnings
      tryCatch({
        if(verbose)
          cat("grading: ", path, "\n")
        source(tmp_full_path, testEnv)
      }, error = function(e) {
        
        cat("Unable to run: ",  path, "\n")
        cat("Error message: \n")
        message(e)
        cat("\n")
      }, warning = function(w){
        cat("Produced a warning: ", path, "\n")
        message(w)
        cat("\n")
      })
    }
    # test the student's submissions
    lr <- testthat::ListReporter$new()
    out <- testthat::test_file(your_test_file, 
                               reporter = lr,
                               env = testEnv)
    
    # parse the output
    score_data[student_num,1] <- tmp_full_path
    for(q in (1:number_questions)){
      
      # true or false if question was correct
      success <- methods::is(lr$results$as_list()[[q]]$results[[1]],"expectation_success") 
      
      # TODO incorporate point values
      if(success){
        score_data[student_num, q+1] <- 1
      }else{
        score_data[student_num, q+1] <- 0
      }
    }
    
    # clear out all of the student's data from global environment
    rm(list = setdiff(ls(testEnv), 
                      c("path", "paths", "submission_dir", 
                        "student_num", "number_questions", 
                        "number_students", "score_data",
                        "your_test_file", "path")), envir = testEnv)
    
    # increment 
    student_num <- student_num + 1
  }
  
  # make the column names prettier before returning everything
  colnames(score_data)[-1] <-  paste("q", as.character(1:number_questions), sep = "")
  
  return(score_data)
}




