test_that("logo", {
  root <- rprojroot::find_package_root_file()
  fLogo <- file.path(root, "man", "figures", "logo.png")
  expect_true(file.exists(fLogo))
})