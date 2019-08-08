
# you should have all data files in the same directory as this file!

test_that("first", {

  expect_equal( sum(myVector), 6) #22.17444, tolerance=1e-4)

})

test_that("second", {

  expect_true(is.character(myString))
  expect_true(len(myString) > 2)

})

test_that("third", {

  expect_equal(nrow(myDataFrame), 2)
  expect_equal(myDataFrame[1,1], 700.01, tolerance=1e-3)

})
