test_that("The 'MArray()' constructor works as expected.", {

  ma <- MakeMArrayDataSet()
  ma <- MArray(marray = ma$marray, pmeta = ma$pmeta, fmeta = ma$fmeta)

  expect_true(isS4(ma))
  expect_equal(class(ma)[1],"MArray")
  expect_true(validObject(ma))

})


test_that("The 'MArray()' yields expected errors.", {

  ma <- MakeMArrayDataSet()

  expect_error(MArray(marray = ma$marray,
                      pmeta = data.framema$pmeta,
                      fmeta = data.frame(ma$fmeta)))

  expect_error(MArray(
                      marray = ma$marray,
                      pmeta = ma$pmeta[1:5,],
                      fmeta = ma$fmeta
                      ))

  expect_error(MArray(
                      marray = ma$marray,
                      pmeta = ma$pmeta,
                      fmeta = ma$fmeta[1:5,]
                      ))

})



