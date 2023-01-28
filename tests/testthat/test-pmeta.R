test_that("The 'pmeta()' method works correctly.", {

  ma <- MakeMArrayDataSet()
  ma <- MArray(marray = ma$marray, pmeta = ma$pmeta, fmeta = ma$fmeta)

  expect_true(is.data.frame(pmeta(ma)))
  expect_equal(nrow(pmeta(ma)),  ncol(marray(ma)))

})

test_that("The 'pmeta()' method yields expected errors.", {

  testString <- "testString"
  testInt <- 1L
  testFloat <- 1.45
  testVect <- 1:9
  testMat <- matrix(1)

  expect_error(pmeta(testString))
  expect_error(pmeta(testInt))
  expect_error(pmeta(testFloat))
  expect_error(pmeta(testVect))
  expect_error(pmeta(testMat))

})


