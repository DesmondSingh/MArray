test_that("The 'marray()' method works correctly.", {

  ma <- MakeMArrayDataSet()
  ma <- MArray(marray = ma$marray, pmeta = ma$pmeta, fmeta = ma$fmeta)

  expect_true(is.matrix(marray(ma)))
  expect_equal( nrow(marray(ma)), nrow(fmeta(ma)))
  expect_equal( ncol(marray(ma)), nrow(pmeta(ma)))

})

test_that("The 'marray()' method yields expected errors.", {

  testString <- "testString"
  testInt <- 1L
  testFloat <- 1.45
  testVect <- 1:9
  testDataframe <- data.frame(1)

  expect_error(marray(testString))
  expect_error(marray(testInt))
  expect_error(marray(testFloat))
  expect_error(marray(testVect))
  expect_error(marray(testDataframe))

})


