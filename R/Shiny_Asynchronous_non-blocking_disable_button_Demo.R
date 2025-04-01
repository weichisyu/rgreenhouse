library(shiny)
library(future)
library(promises)
plan(multisession)  # Enables async processing

ui <- fluidPage(titlePanel("Shiny Asynchronous non-blocking disable button Demo"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("set_text_value", "Set text value"),
                    actionButton("count_btn", "Sleep for 5 sec")
                  ),
                  mainPanel(
                    verbatimTextOutput("session_detail"),
                    h1(textOutput("timer")),
                    verbatimTextOutput("text_value")
                  )
                ))

server <- function(input, output, session) {
  output$session_detail <- renderText({
    paste(
      "Session Token:",
      session$token,
      '\n',
      "Protocol:",
      session$clientData$url_protocol,
      '\n',
      "Host Name:",
      session$clientData$url_hostname,
      '\n',
      "Port:",
      session$clientData$url_port,
      '\n'
    )
  })

  output$timer <- renderText({
    if(sleep_reactive()){
      updateActionButton(session, "count_btn",
                         disable=TRUE)
      print("stop")
    } else{
      updateActionButton(session, "count_btn",
                         disable=FALSE)
      invalidateLater(1000)
      format(Sys.time(), "%H:%M:%S %p")
    }

  })

  sleep_reactive <- reactiveVal(FALSE)
  sleep_trigger <- reactiveVal(NULL)
  output$text_value <- renderText(input$set_text_value)

  observeEvent(input$count_btn, {
    sleep_reactive(TRUE)
    sleep_trigger(TRUE)
  })

  observeEvent(sleep_trigger(), {
    ExtendedTask$new(future_promise({
      Sys.sleep(5)
    }) %...>% {
      sleep_reactive(FALSE)  # Reset state after completion
      sleep_trigger(NULL)
    } %...!% {
      sleep_reactive(FALSE)  # Ensure state reset on error
      sleep_trigger(NULL)
    })
  })
}

shinyApp(ui, server)
