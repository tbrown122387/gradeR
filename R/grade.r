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
  
  # Check if submission is a Qmd/Rmd document
  file_ext <- tools::file_ext(submission_file)
  is_rmd_qmd <- tolower(file_ext) %in% c("qmd","rmd")
  
  # If Quarto, extract R code to temporary file
  if(is_rmd_qmd){
    temp_r_file <- tempfile(fileext = ".R")
    tryCatch({
      knitr::purl(submission_file, output = temp_r_file, quiet = TRUE)
      submission_file <- temp_r_file  # Use extracted R code for evaluation
    }, error = function(e){
      stop("Failed to extract R code from Rmd/Qmd document: ", e$message)
    })
  }
  
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
    
    # if(  grepl("\\(visible\\)", test_name) ){
    #     test_visibility <- "visible"
    # }else if( grepl("\\(hidden\\)", test_name) ){
    #     test_visibility <- "hidden"
    # }else if(  grepl("\\(after_due_date\\)", test_name) ){
    #     test_visibility <- "after_due_date"
    # }else if( grepl("\\(after_published\\)", test_name) ){
    #     test_visibility <- "after_published"
    # }else{
    #     test_visibility <- "after_due_date"
    # }
    
    if(  grepl("\\(hidden\\)", test_name) ){
      test_visibility <- "hidden"
    }else{
      test_visibility <- "visible"
    }
    
    assertionResults <- raw_results[[i]]$results
    
    # Track scores at criterion level
    test_score <- 0
    test_max_score <- 0
    criterion_messages <- c()
    
    for(j in seq_along(assertionResults)){
      assertion <- assertionResults[[j]]
      
      # Extract custom label or create default message
      # The label is embedded in the srcref attribute
      custom_msg <- if(!is.null(assertion$srcref)){
        # Try to extract label from the source code
        src_text <- paste(as.character(assertion$srcref), collapse = " ")
        
        # Look for label parameter in the expectation
        if(grepl('label\\s*=\\s*["\']', src_text)){
          label_match <- regmatches(src_text, regexpr('label\\s*=\\s*["\']([^"\']+)["\']', src_text, perl = TRUE))
          if(length(label_match) > 0){
            sub('.*label\\s*=\\s*["\']([^"\']+)["\'].*', '\\1', label_match)
          } else {
            paste0("Criterion ", j)
          }
        } else {
          paste0("Criterion ", j)
        }
      } else {
        paste0("Criterion ", j)
      }
      
      # Extract point value from label (e.g., "[2pts]" or "(2pts)")
      # Default to 1 point if not specified
      pts_match <- regmatches(custom_msg, regexpr('\\[([0-9]+)pts?\\]|\\(([0-9]+)pts?\\)', custom_msg, perl = TRUE))
      if(length(pts_match) > 0){
        criterion_pts <- as.numeric(gsub('\\[|\\]|\\(|\\)|pts?', '', pts_match))
        # Remove the point specification from the display message
        custom_msg <- gsub('\\s*\\[([0-9]+)pts?\\]|\\s*\\(([0-9]+)pts?\\)', '', custom_msg)
      } else {
        criterion_pts <- 1
      }
      
      test_max_score <- test_max_score + criterion_pts
      
      # Determine pass/fail status and update score
      if(methods::is(assertion, "expectation_success")){
        test_score <- test_score + criterion_pts
        criterion_messages <- c(criterion_messages, 
                                paste0("+", criterion_pts, " (test passed): ", custom_msg))
      } else if(methods::is(assertion, "expectation_failure")){
        criterion_messages <- c(criterion_messages, 
                                paste0("+0 (*****test failed*****): ", custom_msg))
      }
    }
    
    # Build output string
    if(length(criterion_messages) > 0){
      output_text <- paste(criterion_messages, collapse = "\n")
    } else {
      output_text <- "No criteria evaluated"
    }
    
    tests[["tests"]][[i]] <- list(name = test_name,
                                  score = test_score,
                                  max_score = test_max_score,
                                  visibility = test_visibility,
                                  output = output_text)
  }
  
  # now write out all the stuff to a json file
  write(jsonlite::toJSON(tests, auto_unbox = T, pretty = TRUE), file = json_filename)
}


#' The averaging function.
#'
#' This function calculates each student's course average based on all of their assignments.
#' @param table a `data.frame` of student grades. 
#' @param categories a named list of named lists. See below for more information.
#' @param studentNameCol a name of a column with student names.
#' @param drop a list of named lists. See below for more information.
#' @return A \code{vector} of overall course averages, each element named with a student name.
#' @keywords calculateOverallAverage
#' @export
#' @examples
#' \donttest{
#' exampleDF <- data.frame(student = c('a','b','c'),
#'                         test1 = c(50,50,NA),
#'                         test1LateAdjustment = c(.99,1,1),
#'                         hw1=c(45,44,43),
#'                         hw1LateAdjustment = c(1,1,1),
#'                         hw2=c(50,49,48),
#'                         hw2LateAdjustment = c(1,1,1))
#' 
#' testCols <- data.frame(gradeNames = 'test1', 
#'                        lateNames = 'test1LateAdjustment', 
#'                        maxScores = 50)
#' testCategory <- list(colNames=testCols,
#'                      catWeight=.6)
#' hwCols <- data.frame(gradeNames = c('hw1','hw2'), 
#'                      lateNames = c('hw1LateAdjustment','hw2LateAdjustment'),
#'                      maxScores = c(50,50))
#' homeworkCategory <- list(colNames=hwCols,
#'                          catWeight=.4)
#' 
#' cats <- list(tests=testCategory, homeworks=homeworkCategory)
#' myDrops = list(numToDrop=1, cats='homeworks')
#' calculateOverallAverage(exampleDF, cats, 'student', myDrops)
#' calculateOverallAverage(exampleDF, cats, 'student', myDrops, traceCalcsForStudent = 'a')
#' }
calculateOverallAverage <- function(table, categories, studentNameCol, 
                                    drop = NULL, 
                                    traceCalcsForStudent = NULL){
  
  ######################
  # reusable variables #
  ######################
  numStudents <- nrow(table)
  catNames <- names(categories)
  allAssignmentNames <- unlist(lapply(lapply(categories, '[[', 'colNames'), '[[', 'gradeNames'))
  allLateNames <- unlist(lapply(lapply(categories, '[[', 'colNames'), '[[', 'lateNames'))
  
  ###################
  # performs checks #
  ###################
  # check weights sum to 1
  stopifnot(sum(sapply(categories, '[[', 'catWeight')) == 1)
  # check dropped categories all exist 
  if(!is.null(drop)) stopifnot(all(drop$cats %in% names(categories)))
  # TODO: check the number of drops is strictly less than the number of assignments to drop from
  # check assignment names in categories all exist in data table
  assignmentNamesInCats <- unlist(sapply(categories, '[[', 'colNames'))
  stopifnot(all(allAssignmentNames %in% colnames(table)))
  stopifnot(all(allLateNames %in% colnames(table)))
  # TODO check no assignment is double-listed in two categorie
  #TODO check dimension of maxScores in categories (and everything else)
  # TODO check they're vector too
  # lateMultColNames
  
  
  # check names of drop
  # document return value?
  # TODO: check breakdown is a partition
  # add option to see specific student process
  
  
  ###############################
  # fill NA grades s with zeros #
  ###############################
  # TODO make this only work on grade columns
  table[is.na(table)] <- 0
  
  ##############################
  # convert scores to percents #
  # also deduct lateness       #
  ##############################
  for(myCat in categories){
    
    gradeColumnNames <- myCat$colNames$gradeNames
    for(i in seq_along(gradeColumnNames) ){
      
      gradeColumnName <- gradeColumnNames[i]
      maxScore <- myCat$colNames$maxScores[i]
      stopifnot(length(maxScore) == 1)
      
      haveLatenessCol <- !is.null(myCat$colNames$lateNames[i])
      if( haveLatenessCol ){
        lateMultColumnName <- myCat$colNames$lateNames[i]
        table[[gradeColumnName]] <- table[[gradeColumnName]] * table[[lateMultColumnName]]
        table[[gradeColumnName]] <- table[[gradeColumnName]] / maxScore *100
      }else{
        table[[gradeColumnName]] <- table[[gradeColumnName]]/ maxScore *100
      }
    }
  }
  gradesOnlyDF <- subset(table, select = allAssignmentNames)
  
  ####################################
  # group data frame into categories #
  ####################################
  # https://stackoverflow.com/a/70319902/1267833
  breakdown <- lapply(lapply(categories, '[[', 'colNames'), '[[', 'gradeNames')
  map <- rep.int(names(breakdown), lengths(breakdown))
  names(map) <- unlist(breakdown)
  indicesForBy <- map[allAssignmentNames]
  gradesByCategory <- by(t(gradesOnlyDF), indicesForBy, function(x) x, simplify = FALSE)
  
  ########################################
  # create a data.frame for each student #
  ########################################
  gradesByStudent <- lapply(seq.int(numStudents),
                            function(stud_num) lapply(gradesByCategory, '[', , stud_num ))
  names(gradesByStudent) <- table[[studentNameCol]]
  gradesByStudent <- lapply(gradesByStudent,
                            function(x) as.data.frame(stack(x)) )
  if( is.character(traceCalcsForStudent) ){
    print("---------------------")
    print(paste("grades for", traceCalcsForStudent, "after lateness adjustment"))
    print("---------------------")
    print(gradesByStudent[[traceCalcsForStudent]])
  }
  
  
  ##################################################################
  # sort each student by "negotiable points lost" for each student #
  ##################################################################
  addColsSort <- function(df){
    # add category weights
    catWeights <- sapply(categories[as.character(gradesByStudent[[1]]$ind)],
                         '[[',
                         'catWeight')
    df$catWeights <- catWeights
    
    # add dropable column
    if( is.null(drop) ){
      df$droppable <- FALSE
    }else{ # have drops
      df$droppable <- df$ind %in% drop$cats
    }
    
    # add points lost
    # pointsLostNegotiably lost should be 0 if the assignment isn't droppable
    df$ind <- as.character(df$ind)
    numAssignmentsInEachCat <- tapply(df$values, df$ind, length)
    numAssignmentsInEachCat <- numAssignmentsInEachCat[df$ind]
    df$pointsLostNegotiably <- df$catWeights*(100-df$values)/numAssignmentsInEachCat
    df$pointsLostNegotiably <- df$pointsLostNegotiably * df$droppable
    
    # sort df, drop assignments, return scores only
    df <- df[order(df$pointsLostNegotiably, decreasing = T),]
  }
  gradesByStudent <- lapply(gradesByStudent, addColsSort)
  if( is.character(traceCalcsForStudent) ){
    print("---------------------")
    print(paste("grades for", traceCalcsForStudent))
    print("---------------------")
    print(gradesByStudent[[traceCalcsForStudent]])
  }
  
  
  ###################
  # drop bad scores #
  ###################
  dropBadScores <- function(df){
    if(is.null(drop)){
      keptScores <-  1:(nrow(df))
    }else{
      keptScores <- (drop$numToDrop+1):(nrow(df))
    }
    df <- subset(df[keptScores,], select = c(values, ind, catWeights))
    
    # recompute number of assignments in each category
    # because some may have dropped
    df$ind <- as.character(df$ind)
    lengthEachCat <- tapply(df$values, df$ind, length)
    df$numAssignmentsInEachCat <- lengthEachCat[df$ind]
    
    df
  }
  gradesByStudent <- lapply(gradesByStudent, dropBadScores)
  if( is.character(traceCalcsForStudent) ){
    print("---------------------")
    print(paste("grades for", traceCalcsForStudent))
    print("---------------------")
    print(gradesByStudent[[traceCalcsForStudent]])
  }
  
  ############################
  # return  overall averages #
  ############################
  finalAverages <- sapply(gradesByStudent, function(df){
    sum(df$values * df$catWeights / df$numAssignmentsInEachCat)
  })
  if( is.character(traceCalcsForStudent) ){
    print("---------------------")
    print(paste("final grade for", traceCalcsForStudent))
    print("---------------------")
    print(finalAverages[[traceCalcsForStudent]])
  }
  return(finalAverages)
}

