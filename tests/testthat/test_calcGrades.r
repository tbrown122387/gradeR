context("Tests the grading function")

test_that("test missing args for calcGrades", {

  # check that it throws error when you don't supply all arguments
  expect_error(calcGrades())

})


test_that("test correct output for calcGrades", {

  submissions <- "example/assignment1_submissions/"
  my_test_file <- "example/grade_hw1.R"
  results <- calcGrades(submissions,my_test_file, TRUE)
  expect_equal(results[1,1], "example/assignment1_submissions/student1/hw1.R")
  expect_equal(results[2,1], "example/assignment1_submissions/student2/myhw1.r")
  expect_equal(results[1,2], 1)
  expect_equal(results[2,2], 0)
  expect_equal(results[1,3], 1)
  expect_equal(results[2,3], 1)

})

test_that("test bad args for gradescope function", {

  # check that it throws error when you don't supply all arguments
  expect_error(calcGradesForGradescope("fake_file.R", "anotherfakeFile.R", "bad_arg"))

  # missing args doesn't fly
  expect_error(calcGradesForGradescope())

})
