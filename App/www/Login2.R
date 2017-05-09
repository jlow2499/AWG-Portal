#### Log in module ###
USER2 <- reactiveValues(Logged = F)


output$uiLogin2 <- renderUI({
  if (USER2$Logged == FALSE) {
    wellPanel(
      textInput("userName2", "User Name:"),
      passwdInput("passwd2", "Password:"),
      br(),
      actionButton("Login2", "Login")
    )
  }
})

output$pass2 <- renderText({  
  if (USER2$Logged == FALSE) {
    if (!is.null(input$Login2)) {
      if (input$Login2 > 0) {
        Username <- isolate(input$userName2)
        Password <- isolate(input$passwd2)
        Id.username <- which(PASSWORD$Brukernavn == Username)
        Id.password <- which(PASSWORD$Passord    == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER2$Logged <- TRUE
          } 
        } else  {
          "User name or password failed!"
        }
      } 
    }
  }
})
