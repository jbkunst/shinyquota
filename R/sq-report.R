#' Render a basic usage report
#'
#' Reads the shinyquota tables and renders the package's small Quarto report.
#' The report is intentionally basic and is meant for quick operational review.
#'
#' @param con A DBI connection or a `pool::Pool` object.
#' @param app_id Optional application identifier to filter the report.
#' @param output_file Path for the generated HTML file.
#' @param quiet Whether to suppress Quarto output.
#'
#' @return The normalized report path, invisibly.
#' @export
sq_report <- function(
    con,
    app_id = NULL,
    output_file = "shinyquota-report.html",
    quiet = TRUE) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package `quarto` is required to render the report.", call. = FALSE)
  }

  lease <- sq_checkout(con)
  on.exit(lease$release(), add = TRUE)
  db <- lease$con

  sessions <- DBI::dbReadTable(db, "shinyquota_sessions")
  clients <- DBI::dbReadTable(db, "shinyquota_clients")

  if (!is.null(app_id)) {
    sessions <- sessions[sessions$app_id == app_id, , drop = FALSE]
    clients <- clients[clients$app_id == app_id, , drop = FALSE]
  }

  work_dir <- tempfile("shinyquota-report-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sessions_file <- file.path(work_dir, "sessions.rds")
  clients_file <- file.path(work_dir, "clients.rds")
  saveRDS(sessions, sessions_file)
  saveRDS(clients, clients_file)

  template <- system.file("quarto", "report.qmd", package = "shinyquota")

  if (!nzchar(template)) {
    stop("The Quarto report template was not found.", call. = FALSE)
  }

  report_input <- file.path(work_dir, "report.qmd")
  file.copy(template, report_input, overwrite = TRUE)

  output_file <- normalizePath(path.expand(output_file), winslash = "/", mustWork = FALSE)
  output_dir <- dirname(output_file)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  old_wd <- setwd(work_dir)
  on.exit(setwd(old_wd), add = TRUE)

  quarto::quarto_render(
    input = "report.qmd",
    output_file = basename(output_file),
    execute_params = list(
      sessions_file = sessions_file,
      clients_file = clients_file,
      app_id = app_id %||% "All applications"
    ),
    quiet = quiet
  )

  rendered <- file.path(work_dir, basename(output_file))
  file.copy(rendered, output_file, overwrite = TRUE)
  invisible(normalizePath(output_file, winslash = "/", mustWork = TRUE))
}
