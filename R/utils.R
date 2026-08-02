`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) y else x
}

sq_checkout <- function(con) {
  if (inherits(con, "Pool")) {
    if (!requireNamespace("pool", quietly = TRUE)) {
      stop("Package `pool` is required when `con` is a pool object.", call. = FALSE)
    }

    checked_con <- pool::poolCheckout(con)

    return(list(
      con = checked_con,
      release = function() pool::poolReturn(checked_con)
    ))
  }

  list(
    con = con,
    release = function() invisible(NULL)
  )
}

sq_now <- function() {
  as.POSIXct(Sys.time(), tz = "UTC")
}

sq_format_time <- function(x) {
  format(x, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

sq_parse_time <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }

  as.POSIXct(
    sub("Z$", "", x),
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
}

sq_client_info <- function(session) {
  forwarded_for <- session$request[["HTTP_X_FORWARDED_FOR"]] %||% ""
  remote_addr <- session$request[["REMOTE_ADDR"]] %||% "unknown"
  user_agent <- session$request[["HTTP_USER_AGENT"]] %||% "unknown"

  ip <- if (nzchar(forwarded_for)) {
    trimws(strsplit(forwarded_for, ",", fixed = TRUE)[[1L]][1L])
  } else {
    remote_addr
  }

  list(ip = ip, user_agent = user_agent)
}

sq_hash_ip <- function(ip, app_id) {
  salt <- Sys.getenv("SHINYQUOTA_SALT", unset = app_id)
  digest::digest(paste(ip, salt, sep = "::"), algo = "sha256", serialize = FALSE)
}

sq_duration_label <- function(seconds) {
  seconds <- max(0L, as.integer(ceiling(seconds)))
  hours <- seconds %/% 3600L
  minutes <- (seconds %% 3600L) %/% 60L
  secs <- seconds %% 60L

  if (hours > 0L) {
    return(sprintf("%02d:%02d:%02d", hours, minutes, secs))
  }

  sprintf("%02d:%02d", minutes, secs)
}

sq_interpolate <- function(con, sql, ...) {
  DBI::sqlInterpolate(con, sql, ...)
}
