#' Apply a simple access quota to a Shiny session
#'
#' Opens or resumes an access window for the session IP. The window timestamps
#' live in the database, so refreshing the browser does not restart the quota.
#' When access expires, a blocking modal is shown automatically and, by default,
#' the Shiny session is closed from the server.
#'
#' @param session The current Shiny session.
#' @param con A DBI connection or a `pool::Pool` object.
#' @param app_id A short identifier for the application.
#' @param access_minutes Length of the access window in minutes.
#' @param cooldown_minutes Waiting period after the access window, in minutes.
#' @param store_raw_ip Whether to store the raw client IP. The default stores
#'   only a salted hash.
#' @param close_session Whether to close the Shiny session when access is
#'   blocked or expires. Defaults to `TRUE`.
#' @param title Title displayed in the blocking modal.
#'
#' @return A `shinyquota_access` object with reactive methods `allowed()`,
#'   `status()`, `remaining_seconds()`, and `require()`.
#' @export
sq_access <- function(
    session,
    con,
    app_id,
    access_minutes = 10,
    cooldown_minutes = 60,
    store_raw_ip = FALSE,
    close_session = TRUE,
    title = "Access temporarily unavailable") {
  if (missing(session) || is.null(session)) {
    stop("`session` must be a Shiny session.", call. = FALSE)
  }

  if (!is.character(app_id) || length(app_id) != 1L || !nzchar(app_id)) {
    stop("`app_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.numeric(access_minutes) || length(access_minutes) != 1L || access_minutes <= 0) {
    stop("`access_minutes` must be greater than zero.", call. = FALSE)
  }

  if (!is.numeric(cooldown_minutes) || length(cooldown_minutes) != 1L || cooldown_minutes < 0) {
    stop("`cooldown_minutes` must be zero or greater.", call. = FALSE)
  }

  if (!is.logical(close_session) || length(close_session) != 1L || is.na(close_session)) {
    stop("`close_session` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  client <- sq_client_info(session)
  ip_hash <- sq_hash_ip(client$ip, app_id)
  session_id <- paste(app_id, session$token, sep = "::")

  decision <- sq_register_attempt(
    con = con,
    session_id = session_id,
    app_id = app_id,
    ip_hash = ip_hash,
    client_ip = client$ip,
    user_agent = client$user_agent,
    access_seconds = access_minutes * 60,
    cooldown_seconds = cooldown_minutes * 60,
    store_raw_ip = store_raw_ip
  )

  allowed <- shiny::reactiveVal(decision$allowed)
  status <- shiny::reactiveVal(decision$status)
  modal_shown <- FALSE
  session_finished <- FALSE
  session_close_scheduled <- FALSE

  remaining_seconds <- shiny::reactive({
    shiny::invalidateLater(1000, session)
    target <- if (isTRUE(allowed())) decision$access_expires_at else decision$blocked_until
    max(0, as.numeric(difftime(target, sq_now(), units = "secs")))
  })

  countdown_id <- paste0(
    "shinyquota_countdown_",
    gsub("[^A-Za-z0-9_]", "", session$token)
  )

  countdown_ui <- function() {
    seconds <- max(
      0,
      as.numeric(difftime(decision$blocked_until, sq_now(), units = "secs"))
    )
    deadline_ms <- floor(as.numeric(decision$blocked_until) * 1000)

    script <- sprintf(
      paste0(
        "(function() {",
        "const el = document.getElementById('%s');",
        "if (!el) return;",
        "const deadline = %s;",
        "let timer = null;",
        "const formatTime = function(ms) {",
        "const total = Math.max(0, Math.ceil(ms / 1000));",
        "const hours = Math.floor(total / 3600);",
        "const minutes = Math.floor((total %% 3600) / 60);",
        "const seconds = total %% 60;",
        "if (hours > 0) return hours + ':' + String(minutes).padStart(2, '0') + ':' + String(seconds).padStart(2, '0');",
        "return minutes + ':' + String(seconds).padStart(2, '0');",
        "};",
        "const tick = function() {",
        "const remaining = deadline - Date.now();",
        "if (remaining <= 0) {",
        "el.textContent = 'available now';",
        "if (timer) window.clearInterval(timer);",
        "return;",
        "}",
        "el.textContent = formatTime(remaining);",
        "};",
        "tick();",
        "timer = window.setInterval(tick, 1000);",
        "})();"
      ),
      countdown_id,
      format(deadline_ms, scientific = FALSE, trim = TRUE)
    )

    shiny::tags$div(
      style = paste(
        "margin-top: 0.75rem;",
        "font-size: 0.875rem;",
        "color: var(--bs-secondary-color, #6c757d);"
      ),
      "Try again in ",
      shiny::tags$span(
        id = countdown_id,
        style = "font-variant-numeric: tabular-nums; font-weight: 500;",
        sq_duration_label(seconds)
      ),
      shiny::tags$script(shiny::HTML(script))
    )
  }

  show_blocked_modal <- function() {
    if (isTRUE(modal_shown)) {
      return(invisible(NULL))
    }

    shiny::showModal(
      shiny::modalDialog(
        title = title,
        shiny::tags$p("This IP has reached its current usage limit."),
        countdown_ui(),
        footer = NULL,
        easyClose = FALSE,
        fade = FALSE
      ),
      session = session
    )

    modal_shown <<- TRUE
    invisible(NULL)
  }

  close_blocked_session <- function() {
    if (!isTRUE(close_session) || isTRUE(session_close_scheduled)) {
      return(invisible(NULL))
    }

    session_close_scheduled <<- TRUE
    session$onFlushed(
      function() session$close(),
      once = TRUE
    )

    invisible(NULL)
  }

  if (!isTRUE(decision$allowed)) {
    show_blocked_modal()
    close_blocked_session()
  }

  shiny::observe({
    shiny::invalidateLater(1000, session)

    if (isTRUE(allowed()) && sq_now() >= decision$access_expires_at) {
      allowed(FALSE)
      status("blocked")
      sq_finish_session(con, session_id, status = "expired", reason = "expired")
      session_finished <<- TRUE
      show_blocked_modal()
      close_blocked_session()
    }
  })

  session$onSessionEnded(function() {
    if (!session_finished && isTRUE(decision$allowed)) {
      sq_finish_session(con, session_id, status = "ended", reason = "disconnect")
      session_finished <<- TRUE
    }
  })

  structure(
    list(
      allowed = function() allowed(),
      status = function() status(),
      remaining_seconds = function() remaining_seconds(),
      require = function() shiny::req(isTRUE(allowed()), cancelOutput = TRUE),
      session_id = session_id,
      ip_hash = ip_hash,
      access_expires_at = decision$access_expires_at,
      blocked_until = decision$blocked_until
    ),
    class = "shinyquota_access"
  )
}

#' @export
print.shinyquota_access <- function(x, ...) {
  cat("<shinyquota_access>\n")
  cat("  session:", x$session_id, "\n")
  cat("  expires:", format(x$access_expires_at, tz = "UTC"), "UTC\n")
  cat("  blocked until:", format(x$blocked_until, tz = "UTC"), "UTC\n")
  invisible(x)
}
