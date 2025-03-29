library(shiny)

ui <- fluidPage(titlePanel("Shiny observeEvent Sequential Execution Demo"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("set_text_value", "Set text value"),
                    actionButton("count_btn1", "Test1: step1 -> step2 -> step1 -> step2..."),
                    actionButton("count_btn2", "Test2: step1+step2 -> step1+step2..."),
                    actionButton("count_btn3", "Test3: step1 -> step2+step1 -> step2+step1..."),
                  ),
                  mainPanel(
                    verbatimTextOutput("session_detail"),
                    h1(textOutput("timer")),
                    verbatimTextOutput("text_observeEvent_Test1",placeholder = TRUE),
                    verbatimTextOutput("text_observeEvent_Test2",placeholder = TRUE),
                    verbatimTextOutput("text_observeEvent_Test3",placeholder = TRUE),
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
    if(sleep_reactive1()||sleep_reactive2()||sleep_reactive3()){
      print("stop")
    } else{
      invalidateLater(1000)
      format(Sys.time(), "%H:%M:%S %p")
    }

  })

  sleep_reactive1 <- reactiveVal(FALSE)
  sleep_reactive2 <- reactiveVal(FALSE)
  sleep_reactive3 <- reactiveVal(FALSE)
  sleep_trigger1 <- reactiveVal(NULL)
  sleep_trigger2 <- reactiveVal(NULL)
  sleep_trigger3 <- reactiveVal(NULL)
  t1 <- reactiveVal(0)
  t2 <- reactiveVal(0)
  t3 <- reactiveVal(0)
  step1_sleep_time <- 3
  step2_sleep_time <- 5
  output$text_value <- renderText(input$set_text_value)

  #Test1, step1 -> step2 -> step1 -> step2...
  observeEvent(input$count_btn1, {
    t1(0)
    sleep_reactive1(TRUE)
    sleep_trigger1(TRUE)
  })
  observeEvent(sleep_trigger1(), {
    for (i in 1:step1_sleep_time){
      Sys.sleep(1)
      t1(t1() + 1)
    }
    output$text_observeEvent_Test1 <- renderText(paste("sleep_step1:",t1(),"s"))
    print("sleep_step1")
    sleep_reactive1(FALSE)
    sleep_trigger1(NULL)
  })
  observeEvent(sleep_trigger1(), {
    for (i in 1:step2_sleep_time){
      Sys.sleep(1)
      t1(t1() + 1)
    }
    output$text_observeEvent_Test1 <- renderText(paste("sleep_step2:",t1(),"s"))
    print("sleep_step2")
    sleep_reactive1(FALSE)
    sleep_trigger1(NULL)
  })

  #Test2, step1+step2 -> step1+step2...
  observeEvent(input$count_btn2, {
    t2(0)
    sleep_reactive2(TRUE)
    sleep_trigger2(TRUE)
  })
  observeEvent(sleep_trigger2(), {
    for (i in 1:step1_sleep_time){
      Sys.sleep(1)
      t2(t2() + 1)
    }
    output$text_observeEvent_Test2 <- renderText(paste("sleep_step1:",t2(),"s"))
    print("sleep_step1")
  })
  observeEvent(sleep_trigger2(), {
    for (i in 1:step2_sleep_time){
      Sys.sleep(1)
      t2(t2() + 1)
    }
    output$text_observeEvent_Test2 <- renderText(paste("sleep_step2:",t2(),"s"))
    print("sleep_step2")
    sleep_reactive2(FALSE)
    sleep_trigger2(NULL)
  })

  #Test3, step1 -> step2+step1 -> step2+step1...
  observeEvent(input$count_btn3, {
    t3(0)
    sleep_reactive3(TRUE)
    sleep_trigger3(TRUE)
  })
  observeEvent(sleep_trigger3(), {
    for (i in 1:step1_sleep_time){
      Sys.sleep(1)
      t3(t3() + 1)
    }
    output$text_observeEvent_Test3 <- renderText(paste("sleep_step1:",t3(),"s"))
    print("sleep_step1")
    sleep_reactive3(FALSE)
    sleep_trigger3(NULL)
  })
  observeEvent(sleep_trigger3(), {
    for (i in 1:step2_sleep_time){
      Sys.sleep(1)
      t3(t3() + 1)
    }
    output$text_observeEvent_Test3 <- renderText(paste("sleep_step2:",t3(),"s"))
    print("sleep_step2")
  })
}

shinyApp(ui, server)
