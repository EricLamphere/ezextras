# test %~%
testthat::test_that("%~% works properly", {
  testthat::expect_equal(
    c("a", "abc", "abcde") %~% "b",
    c(FALSE, TRUE, TRUE)
  )
})

# test %not~%
testthat::test_that("%not~% works properly", {
  testthat::expect_equal(
    c("a", "abc", "abcde") %not~% "b",
    c(TRUE, FALSE, FALSE)
  )
})


# test %&%
testthat::test_that("%&%% works properly", {
  testthat::expect_equal(
    "a" %&% "b",
    "ab"
  )
})


# test %notin%
testthat::test_that("%notin% works properly", {
  testthat::expect_true(
    "a" %notin% c("b", "c")
  )
})

testthat::test_that("%del% works properly", {
  testthat::expect_equal(
    "abcd" %del% "a",
    "bcd"
  )
})


