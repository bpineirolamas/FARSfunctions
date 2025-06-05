test_that("make_filename works correctly", {
  # Test 2013
  result_2013 <- make_filename(2013)
  expect_equal(result_2013, "accident_2013.csv.bz2")
})
