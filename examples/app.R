library(shiny)
library(shinyquota)

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  file.path(tempdir(), "shinyquota-example.sqlite")
)

sq_init(con)
onStop(function() DBI::dbDisconnect(con))

ui <- fluidPage(
  titlePanel("shinyquota example"),
  p("This button simulates an operation that should be protected."),
  actionButton("run", "Run protected operation"),
  verbatimTextOutput("result")
)

server <- function(input, output, session) {
  access <- sq_access(
    session = session,
    con = con,
    app_id = "example-app",
    access_minutes = 10,
    cooldown_minutes = 60
  )

  observeEvent(input$run, {
    access$require()
    output$result <- renderText(sprintf("Protected operation at %s", Sys.time()))
  })
}

shinyApp(ui, server)
