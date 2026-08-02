sq_read_client <- function(db, app_id, ip_hash) {
  sql <- sq_interpolate(
    db,
    paste(
      "SELECT * FROM shinyquota_clients",
      "WHERE app_id = ?app_id AND ip_hash = ?ip_hash"
    ),
    app_id = app_id,
    ip_hash = ip_hash
  )

  result <- DBI::dbGetQuery(db, sql)

  if (!nrow(result)) NULL else result[1L, , drop = FALSE]
}

sq_increment_client <- function(db, app_id, ip_hash, column) {
  allowed_columns <- c("attempt_count", "allowed_count", "blocked_count")

  if (!column %in% allowed_columns) {
    stop("Invalid counter column.", call. = FALSE)
  }

  sql <- sq_interpolate(
    db,
    paste0(
      "UPDATE shinyquota_clients SET ", column, " = ", column, " + 1 ",
      "WHERE app_id = ?app_id AND ip_hash = ?ip_hash"
    ),
    app_id = app_id,
    ip_hash = ip_hash
  )

  DBI::dbExecute(db, sql)
}

sq_register_attempt <- function(
    con,
    session_id,
    app_id,
    ip_hash,
    client_ip,
    user_agent,
    access_seconds,
    cooldown_seconds,
    store_raw_ip) {
  lease <- sq_checkout(con)
  on.exit(lease$release(), add = TRUE)
  db <- lease$con

  now <- sq_now()
  now_text <- sq_format_time(now)

  insert_client_sql <- sq_interpolate(
    db,
    paste(
      "INSERT INTO shinyquota_clients (",
      "app_id, ip_hash, first_seen_at, last_seen_at, last_user_agent",
      ") VALUES (",
      "?app_id, ?ip_hash, ?now, ?now, ?user_agent",
      ") ON CONFLICT (app_id, ip_hash) DO NOTHING"
    ),
    app_id = app_id,
    ip_hash = ip_hash,
    now = now_text,
    user_agent = user_agent
  )

  DBI::dbExecute(db, insert_client_sql)

  update_seen_sql <- sq_interpolate(
    db,
    paste(
      "UPDATE shinyquota_clients SET",
      "last_seen_at = ?now, last_user_agent = ?user_agent",
      "WHERE app_id = ?app_id AND ip_hash = ?ip_hash"
    ),
    now = now_text,
    user_agent = user_agent,
    app_id = app_id,
    ip_hash = ip_hash
  )

  DBI::dbExecute(db, update_seen_sql)
  sq_increment_client(db, app_id, ip_hash, "attempt_count")

  client <- sq_read_client(db, app_id, ip_hash)
  access_expires_at <- sq_parse_time(client$access_expires_at)
  blocked_until <- sq_parse_time(client$blocked_until)
  allowed <- FALSE

  if (!is.na(access_expires_at) && now < access_expires_at) {
    allowed <- TRUE
  } else if (!is.na(blocked_until) && now < blocked_until) {
    allowed <- FALSE
  } else {
    access_started_at <- now
    access_expires_at <- now + access_seconds
    blocked_until <- access_expires_at + cooldown_seconds

    open_window_sql <- sq_interpolate(
      db,
      paste(
        "UPDATE shinyquota_clients SET",
        "access_started_at = ?access_started_at,",
        "access_expires_at = ?access_expires_at,",
        "blocked_until = ?blocked_until",
        "WHERE app_id = ?app_id AND ip_hash = ?ip_hash",
        "AND (blocked_until IS NULL OR blocked_until <= ?now)"
      ),
      access_started_at = sq_format_time(access_started_at),
      access_expires_at = sq_format_time(access_expires_at),
      blocked_until = sq_format_time(blocked_until),
      app_id = app_id,
      ip_hash = ip_hash,
      now = now_text
    )

    updated <- DBI::dbExecute(db, open_window_sql)

    if (updated == 1L) {
      allowed <- TRUE
    } else {
      client <- sq_read_client(db, app_id, ip_hash)
      access_expires_at <- sq_parse_time(client$access_expires_at)
      blocked_until <- sq_parse_time(client$blocked_until)
      allowed <- !is.na(access_expires_at) && now < access_expires_at
    }
  }

  sq_increment_client(
    db,
    app_id,
    ip_hash,
    if (allowed) "allowed_count" else "blocked_count"
  )

  raw_ip_sql <- if (isTRUE(store_raw_ip)) client_ip else DBI::SQL("NULL")
  status <- if (allowed) "allowed" else "blocked"
  ended_at_sql <- if (allowed) DBI::SQL("NULL") else now_text
  end_reason_sql <- if (allowed) DBI::SQL("NULL") else "blocked"

  insert_session_sql <- sq_interpolate(
    db,
    paste(
      "INSERT INTO shinyquota_sessions (",
      "session_id, app_id, ip_hash, client_ip, started_at, ended_at,",
      "access_expires_at, blocked_until, status, end_reason, user_agent",
      ") VALUES (",
      "?session_id, ?app_id, ?ip_hash, ?client_ip, ?started_at, ?ended_at,",
      "?access_expires_at, ?blocked_until, ?status, ?end_reason, ?user_agent",
      ")"
    ),
    session_id = session_id,
    app_id = app_id,
    ip_hash = ip_hash,
    client_ip = raw_ip_sql,
    started_at = now_text,
    ended_at = ended_at_sql,
    access_expires_at = sq_format_time(access_expires_at),
    blocked_until = sq_format_time(blocked_until),
    status = status,
    end_reason = end_reason_sql,
    user_agent = user_agent
  )

  DBI::dbExecute(db, insert_session_sql)

  list(
    allowed = allowed,
    status = status,
    access_expires_at = access_expires_at,
    blocked_until = blocked_until
  )
}

sq_finish_session <- function(con, session_id, status, reason) {
  lease <- sq_checkout(con)
  on.exit(lease$release(), add = TRUE)
  db <- lease$con

  sql <- sq_interpolate(
    db,
    paste(
      "UPDATE shinyquota_sessions SET",
      "ended_at = ?ended_at, status = ?status, end_reason = ?reason",
      "WHERE session_id = ?session_id AND ended_at IS NULL"
    ),
    ended_at = sq_format_time(sq_now()),
    status = status,
    reason = reason,
    session_id = session_id
  )

  invisible(DBI::dbExecute(db, sql))
}
