test_that("MakeMArray works when it should.", {

  expect_no_error(MakeMArrayDataSet())
  expect_no_error(MakeMArrayDataSet(n_samples = 10L,
                                    n_features = 6L,
                                    with_seed = NULL))

  expect_no_error(MakeMArrayDataSet(n_samples = 10L,
                                    n_features = 6L,
                                    with_seed = 12453))

  expect_equal(length(MakeMArrayDataSet()), 3)
  expect_equal(is.list(MakeMArrayDataSet()), T)

  ma <- MakeMArrayDataSet()
  expect_equal(is.matrix(ma[["marray"]]),T)
  expect_equal(is.data.frame(ma[["fmeta"]]),T)
  expect_equal(is.data.frame(ma[["pmeta"]]),T)
  expect_equal(nrow(ma[["marray"]]), nrow(ma[["fmeta"]]))
  expect_equal(ncol(ma[["marray"]]), nrow(ma[["pmeta"]]))

})



test_that("MakeMArray doesn't work when it shouldn't.", {

  expect_error(MakeMArrayDataSet(n_samples = 9L), "Inputs must be divisible by 2")
  expect_error(MakeMArrayDataSet(n_features = 5L), "Inputs must be divisible by 2")

  expect_error(MakeMArrayDataSet(n_samples = 10), "Inputs must be integer values")
  expect_error(MakeMArrayDataSet(n_features = 6), "Inputs must be integer values")

  expect_error(MakeMArrayDataSet(n_samples = -10L), "Inputs must be positive values")
  expect_error(MakeMArrayDataSet(n_features = -6L), "Inputs must be positive values")

  expect_error(MakeMArrayDataSet(n_samples = 27L), "'n_samples' cannot be larger than 26")

  expect_error(MakeMArrayDataSet(n_samples = "toy"), "Inputs must be numeric values")
  expect_error(MakeMArrayDataSet(n_features = "toy"), "Inputs must be numeric values")
  expect_error(MakeMArrayDataSet(with_seed = "toy"), "'with_seed' must be null or numeric type")

})



