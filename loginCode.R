credentials <- reactiveVal()
mongoUsers <- mongo(collection="users", db="rmeDB", url= connectionLink)
#function that inserts a new user, taking in username, password, permission, name, contact info, and organization
insertUser <- function(username, password, permission, name, contact_info, organization){
  user <- c(paste('{"username": "', username, '",',
                  '"password": "', password, '",', 
                  '"permission": "', permission, '",', 
                  '"name": "', name, '",', 
                  '"contact info": "', contact_info, '",',
                  '"organization": "', organization, '"}', sep=""))
  
  #add it to mongodb
  mongoUsers$insert(user)
}

#update the values in credentials/login whenever a new user is added
observeEvent(reactiveUsers(), {
  #the information of the user taken from the google sheet
  user_base <- data.frame(
    user = reactiveUsers()$username,
    password = sapply(reactiveUsers()$password, sodium::password_store),
    permissions = reactiveUsers()$permission,
    name = reactiveUsers()$name,
    contactInfo = reactiveUsers()$`contact info`,
    org = reactiveUsers()$`organization`
  )

  #shinyauthr login server. Uses the user information to facilitate the login functionality
  cred <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init()),
    reload_on_logout = TRUE
  )
  
  #shinyauthr logout server. Facilitates the logout functionality
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(cred()$user_auth)
  )
  
  #create a variable that tells us if the user is logged in and can be accessed by the UI to be used in conditional panel 
  output$userLoggedIn <- reactive({
    cred()$user_auth
  })
  outputOptions(output, "userLoggedIn", suspendWhenHidden = FALSE)
  
  #closes the modal when the login button is clicked inside the modal (given the user has been properly logged in)
  observeEvent(input$`login-button`, {
    if (cred()$user_auth) {
      #set the credential value so it can be accessed from outside of this scope
      credentials(cred())
      
      #hides the login button right away when logged in (was previously visible for a second)
      shinyjs::hide("showLogin")
      
      #removes the modal
      removeModal() 
    }
  })
})

#the modal opened when you click login
observeEvent(input$showLogin, {
  showModal(
    modalDialog(
      loginUI("login"),
      footer = NULL,
      easyClose = TRUE
    )
  )
  #overrides the default behavior of the shinyauthr login display being hidden inside the modal
  shinyjs::runjs('$("#login-panel").removeClass("shinyjs-hide");')
})

#when the register button is clicked, open the registration form modal
observeEvent(input$register, {
  showModal(
    modalDialog(
      title= "Register",
      textInput("username", "Enter a Username"),
      uiOutput("usernameError"),
      textInput("password", "Enter Your Password"),
      textInput("name", "Your Name"),
      textInput("email", "Contact Information", placeholder = "example@gmail.com"),
      textInput("organization", "Your Organization"),
      div(actionButton("addUser", "Register", class="btn-success"), style="display:flex; justify-content: center;"),
      uiOutput("invalidEntry"),
      footer = NULL,
      easyClose = TRUE
    )
  )
})

#when the add user button is clicked in the registration modal, vertify the info entered and add the user
observeEvent(input$addUser, {
  if (input$username %in% reactiveUsers()$username) {
    output$usernameError <- renderUI({
      p("This username already exists, select another one.", style = "color:red;")
    })
  } else if (nchar(input$username) < 1 || nchar(input$password) < 1 || nchar(input$name) < 1 || nchar(input$email) < 1 || nchar(input$organization) < 1) {
    output$invalidEntry <- renderUI({
      p("Make sure all the fields are filled in.")
    })
    output$usernameError <- renderUI({})
  } else {
    output$usernameError <- renderUI({})
    output$invalidEntry <- renderUI({})

    insertUser(trimws(input$username),
               trimws(input$password),
               "standard",
               trimws(input$name),
               trimws(input$email),
               trimws(input$organization))

    #closes the modal when it is done
    removeModal()
  }
})