#####  Test update.halfspaces  #################################################
# use <generate_train_data> function to generate data

context("update.halfspaces")

test_that("expect S3 class halfspace & halfspaces", {
  tasks <- generate_train_data(seed0 = 345)
  for (task in tasks) {
    trained_hs <- do.call(train_depth, task)
    updated_hs <- update(trained_hs, task[[1]])
    expect_s3_class(updated_hs, "halfspaces")
    expect_equal(validate_halfspaces(updated_hs), updated_hs)
    expect_length(updated_hs, n = task$n_halfspace)
    expect_named(attributes(updated_hs), expected = c("train_data", "subsample",
                                                      "scope", "scale", "class")
    )
    lapply(updated_hs, expect_s3_class, class = "halfspace")
  }
})

test_that("check for reproducability in train_depth", {
  tasks <- generate_train_data(scope = 20, subsample = 0.5, seed = 29341)
  for (task in tasks) {
    data <- task[[1]]
    trained_hs <- do.call(train_depth, task)
    updated_hs <- update(trained_hs, data)
    expect_identical(trained_hs, updated_hs)

    warning1 <- "Output Object contains a subset of the Input Object"
    expect_warning(update(trained_hs, data), regexp = warning1)

    kit <- update(train_depth(data[1:10, ], seed = 246),
                  data[11:nrow(data), ], add = TRUE)
    expect_identical(kit, train_depth(data, seed = 246))

    warning2 <- "Due to new modified Data, new splitpoints are drawn randomly"
    expect_warning(update(trained_hs, data[20:30, ]), regexp = warning2)
  }
})

test_that("check on n_halfspace in update", {
  tasks <- generate_train_data(n_halfspace = 100, subsample = 0.8, seed = 29341)
  for (task in tasks) {
    trained_hs <- do.call(train_depth, task)
    update50 <- update(trained_hs, n_halfspace = 50)
    update150 <- update(trained_hs, n_halfspace =  150)
    expect_true(all(update50 %in% trained_hs))
    expect_true(all(trained_hs %in% update150))
    expect_identical(update50[[1]], trained_hs[[51]])
    expect_identical(trained_hs[[100]], update150[[100]])
  }
})
