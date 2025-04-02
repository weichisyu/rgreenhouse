library(shiny)
library(future)
library(promises)
library(bslib)
plan(multisession)  # Enables async processing

ui <- fluidPage(titlePanel("Shiny Asynchronous non-blocking input task button Demo"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("set_text_value", "Set text value"),
                    bslib::input_task_button("count_btn", "Sleep for 5 sec")
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
      print("stop")
    } else{
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

  start_sleep <- ExtendedTask$new(function()(future_promise({
    Sys.sleep(5)
  }) %...>% {
    sleep_reactive(FALSE)  # Reset state after completion
    sleep_trigger(NULL)
  } %...!% {
    sleep_reactive(FALSE)  # Ensure state reset on error
    sleep_trigger(NULL)
  })) |> bslib::bind_task_button("count_btn")


  observeEvent(sleep_trigger(), {
    start_sleep$invoke()
  })

}

shinyApp(ui, server)
