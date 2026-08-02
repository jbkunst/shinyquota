# shinyquota

`shinyquota` provides a small and straightforward way to limit access to a Shiny application by client IP. It is intended for public demos and small applications where usage time or external service costs need basic protection.

The package is deliberately limited in scope. It does not provide authentication, user management, roles, advanced rate limiting, or fraud prevention. Its goal is to offer a simple implementation based on an access window, a cooldown period, and persistent session records.

## How it works

For each application and IP hash, `shinyquota` stores:

- when the access window started;
- when access expires;
- when the IP may enter again;
- allowed and blocked session attempts.

The timestamps live in the database. Refreshing the browser creates a new Shiny session, but it does not restart the access window.

## Installation

```r
pak::pak("jbkunst/shinyquota")
```

## Minimal use

Initialize the tables once when the application starts:

```r
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  "shinyquota.sqlite"
)

shinyquota::sq_init(con)
```

Create the access object inside `server()`:

```r
server <- function(input, output, session) {
  access <- shinyquota::sq_access(
    session = session,
    con = con,
    app_id = "censo-ia",
    access_minutes = 10,
    cooldown_minutes = 60
  )

  observeEvent(input$send_message, {
    access$require()

    # Call the external service here.
  })
}
```

`sq_access()` obtains the client IP, opens or resumes its access window, and displays a blocking modal when the window is unavailable. Protect each operation that consumes tokens or another paid resource with `access$require()`.

## SQLite and PostgreSQL

The package uses `DBI`, so the same functions work with SQLite and PostgreSQL connections. SQLite is useful for local development and a single persistent Shiny process. PostgreSQL is the safer choice for a public deployment with multiple processes or instances.

A `pool::Pool` object can also be passed as `con`.

## Basic Quarto report

```r
shinyquota::sq_report(
  con = con,
  app_id = "censo-ia",
  output_file = "shinyquota-report.html"
)
```

The bundled report summarizes session attempts, blocked attempts, distinct IP hashes, recent activity, and the most active IP hashes.

## Important limitations

IP-based limits are only a basic barrier against casual overuse. Shared corporate networks, mobile IP rotation, proxies, and VPNs can produce false positives or bypass the limit. `shinyquota` should not be treated as authentication, authorization, fraud prevention, or a complete cost-control system.

By default, raw IP addresses are not stored. Set `SHINYQUOTA_SALT` in the application environment so hashes are not based only on the application identifier. Raw IP storage is optional through `store_raw_ip = TRUE` and should be used only when appropriate for the application's privacy requirements.
