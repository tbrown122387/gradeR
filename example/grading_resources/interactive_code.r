# load in the package
library(gradeR)

# set working directory to place where you have
# a. your grading file with the tests
# b. all external .csv files that students use for their assignment
setwd("~/gradeR/example/grading_resources/")

# this is the directory with all of the student submissions
submissionDir <- "../submissions/"

# get the grades
calcGrades(submission_dir = submissionDir, 
           your_test_file = "assignment1_grading_file.r")


