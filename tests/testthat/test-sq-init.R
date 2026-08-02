test_that("sq_init creates the required SQLite tables", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_true(sq_init(con))
  expect_true(sq_init(con))

  expect_true(DBI::dbExistsTable(con, "shinyquota_clients"))
  expect_true(DBI::dbExistsTable(con, "shinyquota_sessions"))
})
