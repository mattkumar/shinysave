library(shiny)
library(haven)

# ui
ui <- fluidPage(
  titlePanel("Shiny Save"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Name", ""),
      tags$hr(),
      radioButtons("upload_type", "Upload type:", choices = c(
        "New .XPT" = "xpt",
        "Restore Previous Session" = "rdata"
      )),
      conditionalPanel(
        condition = "input.upload_type == 'xpt'",
        fileInput("upload_xpt", "Upload a .XPT file")
      ),
      conditionalPanel(
        condition = "input.upload_type == 'rdata'",
        fileInput("upload_rdata", "Upload a .RData file")
      ),
      tags$hr(),
      downloadButton("save", "Save Current Session")
    ),
    mainPanel(
      h3("Output"),
      tableOutput("table"),
      h3("Contents"),
      verbatimTextOutput("debug")
    )
  )
)

# server
server <- function(session, input, output) {
  
  # Tracker
  tracker <- reactiveValues()
  
  # New XPT logic
  observeEvent(input$upload_xpt,{
    req(input$upload_xpt)
    tracker$raw_xpt <- readBin(input$upload_xpt$datapath, "raw", file.info(input$upload_xpt$datapath)$size)
    tmp_file <- tempfile(fileext = ".xpt")
    writeBin(tracker$raw_xpt, tmp_file)
    tracker$processed_xpt <- haven::read_xpt(tmp_file)
  })
  
  # Saved Session logic
  observeEvent(input$upload_rdata, {
    req(input$upload_rdata)
    saved_state <- readRDS(input$upload_rdata$datapath)
    tracker$raw_xpt <- saved_state$tracker$raw_xpt
    updateTextInput(session, inputId = "name", value = saved_state$name)
    tmp_file <- tempfile(fileext = ".xpt")
    writeBin(tracker$raw_xpt, tmp_file)
    tracker$processed_xpt <- haven::read_xpt(tmp_file)
  })
  
  # Save logic
  output$save <- downloadHandler(
    filename = function() {
      "saved_session.RData"
    },
    content = function(file) {
      saveRDS(list(tracker = reactiveValuesToList(tracker), name = input$name), file)
    }
  )
  
  # Display logic
  output$table <- renderTable({
    req(tracker$processed_xpt)
    head(tracker$processed_xpt)
  })
  
  # Display contents
  output$debug <- renderPrint({
    reactiveValuesToList(tracker)
  })
  
}

# run app
shinyApp(ui, server)
