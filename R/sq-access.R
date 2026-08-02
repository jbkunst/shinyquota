#' Apply a simple access quota to a Shiny session
#'
#' Opens or resumes an access window for the session IP. The window timestamps
#' live in the database, so refreshing the browser does not restart the quota.
#' When access expires, a blocking modal is shown automatically.
#'
#' @param session The current Shiny session.
#' @param con A DBI connection or a `pool::Pool` object.
#' @param app_id A short identifier for the application.
#' @param access_minutes Length of the access window in minutes.
#' @param cooldown_minutes Waiting period after the access window, in minutes.
#' @param store_raw_ip Whether to store the raw client IP. The default stores
#'   only a salted hash.
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

  remaining_seconds <- shiny::reactive({
    shiny::invalidateLater(1000, session)
    target <- if (isTRUE(allowed())) decision$access_expires_at else decision$blocked_until
    max(0, as.numeric(difftime(target, sq_now(), units = "secs")))
  })

  modal_output_id <- paste0(
    "shinyquota_remaining_",
    gsub("[^A-Za-z0-9_]", "", session$token)
  )

  session$output[[modal_output_id]] <- shiny::renderUI({
    seconds <- remaining_seconds()

    if (seconds > 0) {
      shiny::tags$div(
        shiny::tags$p("You can access this application again in:"),
        shiny::tags$div(
          style = "font-size: 2rem; font-weight: 600; text-align: center;",
          sq_duration_label(seconds)
        )
      )
    } else {
      shiny::tags$p("You can access the application again. Refresh this page to continue.")
    }
  })

  show_blocked_modal <- function() {
    if (isTRUE(modal_shown)) {
      return(invisible(NULL))
    }

    shiny::showModal(
      shiny::modalDialog(
        title = title,
        shiny::tags$p("This IP has reached its current usage limit."),
        shiny::uiOutput(modal_output_id),
        footer = NULL,
        easyClose = FALSE,
        fade = FALSE
      ),
      session = session
    )

    modal_shown <<- TRUE
    invisible(NULL)
  }

  if (!isTRUE(decision$allowed)) {
    show_blocked_modal()
  }

  shiny::observe({
    shiny::invalidateLater(1000, session)

    if (isTRUE(allowed()) && sq_now() >= decision$access_expires_at) {
      allowed(FALSE)
      status("blocked")
      sq_finish_session(con, session_id, status = "expired", reason = "expired")
      session_finished <<- TRUE
      show_blocked_modal()
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
