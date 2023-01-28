test_that("The 'fmeta()' method works correctly.", {

  ma <- MakeMArrayDataSet()
  ma <- MArray(marray = ma$marray, pmeta = ma$pmeta, fmeta = ma$fmeta)

  expect_true(is.data.frame(fmeta(ma)))
  expect_equal(nrow(fmeta(ma)),  nrow(marray(ma)))

})

test_that("The 'fmeta()' method yields expected errors.", {

  testString <- "testString"
  testInt <- 1L
  testFloat <- 1.45
  testVect <- 1:9
  testMat <- matrix(1)

  expect_error(fmeta(testString))
  expect_error(fmeta(testInt))
  expect_error(fmeta(testFloat))
  expect_error(fmeta(testVect))
  expect_error(fmeta(testMat))

})


