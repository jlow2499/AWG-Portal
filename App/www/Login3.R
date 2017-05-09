#### Log in module ###
USER3 <- reactiveValues(Logged = F)


output$uiLogin3 <- renderUI({
  if (USER3$Logged == FALSE) {
    wellPanel(
      textInput("userName3", "User Name:"),
      passwdInput("passwd3", "Password:"),
      br(),
      actionButton("Login3", "Login")
    )
  }
})

output$pass3 <- renderText({  
  if (USER3$Logged == FALSE) {
    if (!is.null(input$Login3)) {
      if (input$Login3 > 0) {
        Username <- isolate(input$userName3)
        Password <- isolate(input$passwd3)
        Id.username <- which(PASSWORD$Brukernavn == Username)
        Id.password <- which(PASSWORD$Passord    == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER3$Logged <- TRUE
          } 
        } else  {
          "User name or password failed!"
        }
      } 
    }
  }
})
