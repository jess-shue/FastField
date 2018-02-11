context("xl_sheets_to_csv")

test_that("errs without excel file", {
  msg <- "must contain at least one excel file"
  expect_error(
    xl_sheets_to_csv(tempdir()),
    msg
  )
})

test_that("errs if input_dir does not exist", {
  expect_error(
    xl_sheets_to_csv("invalid_dir"),
    "must match a valid directory"
  )
})

test_that("errs with informative message with input of wrong type", {
  not_a_string <- 1
  expect_error(xl_sheets_to_csv(not_a_string))
  expect_error(xl_sheets_to_csv("./sheets", not_a_string))
})

test_that("works as expected", {
  path_to_example <- system.file("extdata", "example.xlsx", package = "fgeo.tool")
  path_to_extdata <- sub(basename(path_to_example), "", path_to_example)
  input <- path_to_extdata
  output <- tempdir()

  expect_silent(
    xl_sheets_to_csv(input, output)
  )

  files <- dir(output)
  matching_file <- file <- files[grepl("^example.*csv$", files)]
  expect_true(length(matching_file) > 0)

  matching_path <- fs::path(output, matching_file)
  output_files <- names(suppressMessages(readr::read_csv(matching_path)))
  expect_true("sheet" %in% output_files)
})







