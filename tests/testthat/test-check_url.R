
test_that("check_url returns TRUE for a valid URL", {
  expect_true(check_url("https://www.facebook.com/"))
})

test_that("check_url returns FALSE for an invalid URL", {
  expect_false(check_url("https://nonexistent.website.abc"))
})

test_that("check_url handles errors gracefully and returns FALSE", {
  expect_false(check_url("https://"))
})

test_that("check_url returns FALSE if URL cannot be accessed (e.g., 404)", {
  expect_false(check_url("https://www.example.com/nonexistentpage"))
})
