#library(testthat)
#submission_dir <- "~/gradeR/assignment1_submissions/"
#your_test_file <- "~/gradeR/my_tests/grade_hw1.R"


#' A grading function!
#'
#' This function grades a bunch of R script assignments 
#' @param submission_dir where the assignments are located
#' @param your_test_file the path to your testthat test file (e.g. grade_hw1.R)
#' @keywords cats
#' @export
#' @examples
#' submissions <- "~/gradeR/assignment1_submissions/"
#' my_test_file <- "~/gradeR/my_tests/grade_hw1.R"
#' results <- gradeR(submissions, my_test_file)
gradeR <- function(submission_dir, your_test_file){

  paths <- list.files(path = submission_dir, recursive = T)
  number_questions <- length(test_file(your_test_file, reporter = "minimal"))
  number_students <- length(paths)
  score_data <- data.frame("id" = vector(mode = "character", length = 2), 
                           matrix(data = "blank", nrow = number_students, 
                                  ncol = number_questions),
                           stringsAsFactors = F)
  student_num <- 1
  for(path in paths ){
    
    tmp_full_path <- paste(submission_dir, path, sep = "")    
    source(tmp_full_path)
    lr <- ListReporter$new()
    out <- test_file("~/gradeR/my_tests/grade_hw1.R", 
                     reporter = lr)
    
    # parse the output
    score_data[student_num,1] <- tmp_full_path
    for(q in (1:number_questions)){
      
      # true or false if question was correct
      success <- is(lr$results$as_list()[[q]]$results[[1]],"expectation_success") 
      
      # TODO incorporate point values
      if(success){
        score_data[student_num, q+1] <- "1"
      }else{
        score_data[student_num, q+1] <- "0"
      }
    }
    
    # clear out all of the student's data from global environment
    rm(list=setdiff(ls(), 
                    c("path", "paths", "submission_dir", "student_num", 
                      "number_questions", "number_students", "score_data")))
    
    # increment 
    student_num <- student_num + 1
  }
  
  return(score_data)
}




