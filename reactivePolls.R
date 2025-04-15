#Reactive Polls to get data from google sheets/mongodb

#function to get all the user info from mongodb
allUsers <- function(){
  tryCatch(
    {
      mongoUsers <- mongo(collection="users", db="rmeDB", url= connectionLink)
      return(mongoUsers$find(query = '{}', fields = '{ "_id": false}'))
    },
    error = function(cond) {
      message(conditionMessage(cond))
      output$urlerror <- renderText({
        "We are currently unable to access mongoDB. You will be unable to register or login."
      })
      return(NULL)
    },
    warning = function(cond) {
      message(conditionMessage(cond))
      output$urlerror <- renderText({
        "We are currently unable to access mongoDB. You will be unable to register or login."
      })
      return(NULL)
    })
  
  
}

#gets the user information
reactiveUsers <- reactivePoll(
  intervalMillis = 1000,
  session = session,
  checkFunc = function() {
    return(allUsers())
  },
  valueFunc = function() {
    users <- allUsers()
    if (length(users) < 1){
      users <- data.frame("user1", "pass1", "admin", "User 1", "n/a", "NRC BTM")
    }
    colnames(users) <- c("username",
                         "password",
                         "permission",
                         "name",
                         "contact info",
                         "organization")

    
    return(users)
  }
)