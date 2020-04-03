####  test training  ###########################################################
# use <generate_train_data> function
context("train_depth")

test_that("expect S3 class halfspace & halfspaces", {
  tasks <- generate_train_data(seed0 = 345)
  for (task in tasks) {
    trained_hs <- do.call(train_depth, task)
    expect_s3_class(trained_hs, "halfspaces")
    expect_equal(validate_halfspaces(trained_hs), trained_hs)
    expect_length(trained_hs, n = task$n_halfspace)
    expect_named(attributes(trained_hs), expected = c("train_data", "subsample",
                                                      "scope", "scale", "class")
                 )
    lapply(trained_hs, expect_s3_class, class = "halfspace")
  }
})

test_that("expect reproducability only if seed is set", {
  tasks <- generate_train_data(seed = 345)
  for (task in tasks) {
    expect_identical(do.call(train_depth, task), do.call(train_depth, task))
    expect_false(identical( train_depth(task[[1]]), train_depth(task[[1]])))
  }
})
