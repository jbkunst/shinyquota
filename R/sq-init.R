#' Initialize the shinyquota tables
#'
#' Creates the two small tables used by shinyquota. The operation is
#' idempotent and can be called when the application starts.
#'
#' @param con A DBI connection or a `pool::Pool` object.
#'
#' @return `TRUE`, invisibly.
#' @export
sq_init <- function(con) {
  lease <- sq_checkout(con)
  on.exit(lease$release(), add = TRUE)
  db <- lease$con

  if (!DBI::dbIsValid(db)) {
    stop("`con` is not a valid DBI connection.", call. = FALSE)
  }

  if (inherits(db, "SQLiteConnection")) {
    try(DBI::dbExecute(db, "PRAGMA journal_mode = WAL"), silent = TRUE)
    try(DBI::dbExecute(db, "PRAGMA busy_timeout = 5000"), silent = TRUE)
  }

  DBI::dbExecute(db, paste(
    "CREATE TABLE IF NOT EXISTS shinyquota_clients (",
    "app_id TEXT NOT NULL,",
    "ip_hash TEXT NOT NULL,",
    "first_seen_at TEXT NOT NULL,",
    "last_seen_at TEXT NOT NULL,",
    "access_started_at TEXT,",
    "access_expires_at TEXT,",
    "blocked_until TEXT,",
    "attempt_count INTEGER NOT NULL DEFAULT 0,",
    "allowed_count INTEGER NOT NULL DEFAULT 0,",
    "blocked_count INTEGER NOT NULL DEFAULT 0,",
    "last_user_agent TEXT,",
    "PRIMARY KEY (app_id, ip_hash)",
    ")"
  ))

  DBI::dbExecute(db, paste(
    "CREATE TABLE IF NOT EXISTS shinyquota_sessions (",
    "session_id TEXT PRIMARY KEY,",
    "app_id TEXT NOT NULL,",
    "ip_hash TEXT NOT NULL,",
    "client_ip TEXT,",
    "started_at TEXT NOT NULL,",
    "ended_at TEXT,",
    "access_expires_at TEXT,",
    "blocked_until TEXT,",
    "status TEXT NOT NULL,",
    "end_reason TEXT,",
    "user_agent TEXT",
    ")"
  ))

  DBI::dbExecute(
    db,
    paste(
      "CREATE INDEX IF NOT EXISTS shinyquota_sessions_app_started_idx",
      "ON shinyquota_sessions (app_id, started_at)"
    )
  )

  DBI::dbExecute(
    db,
    paste(
      "CREATE INDEX IF NOT EXISTS shinyquota_sessions_app_ip_idx",
      "ON shinyquota_sessions (app_id, ip_hash)"
    )
  )

  invisible(TRUE)
}
