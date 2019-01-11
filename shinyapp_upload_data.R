library(shiny)
# idea from: https://gist.github.com/ncarchedi/2ef57039c238ef074ee7

ui <- function(){
  fluidPage(
    fluidRow(
      uiOutput("allInputs"),
      numericInput("no.factors", "Number of factors",value=1,min=1,max=100,step=1),
      actionButton("action","Set number of factors"),
      verbatimTextOutput("lavaan.code")
    )
  )
}




server <- shinyServer(function(input, output) {
  
  # Initialize list of inputs
  inputTagList <- tagList()
  
  observeEvent(input$action,{
  
    output$allInputs <- renderUI({
      
  
      inputTagList <- tagList()
      test(i = input$no.factors)
      
    })
  }) 
    

  test <-  function(i){
    
    i <- input$no.factors
    # Return if button not pressed yet
    if(is.null(i) || i < 1) return()
    # Define unique input id and label
    for(ii in 1:i){
      newInputId <- paste0("input", ii)
      newInputLabel <- paste("Number of variable for factor", ii)
      # Define new input
      newInput <- sliderInput(newInputId, newInputLabel, min=1,max=10,value=3)
      # Append new input to list of existing inputs
      inputTagList <<- tagAppendChild(inputTagList, newInput)
      # Return updated list of inputs
    }
    inputTagList
  }
  
lavaan.code <- function(n.factors){
  # empty variable declaration needed in following pieces of code
  output <- list()
  n.factors <- input$no.factors
  
  
  # identifying the amount of variables that are set (per factor)
  names <- paste0("input$input",1:n.factors)
  n.variables <- c() # number per factor
  for(i in 1:n.factors){
    n.variables[i] <- eval(parse(text = names[i]))
  }
  no.variables <- sum(n.variables) # total number
  print(n.variables)
  
  
  
  for(i in 1:n.factors){
    out <-  paste0("f",i," =~ ")
    out2 <- out
    for(ii in 1:(n.variables[i]-1)){
      if(i == 1) { # for first factor start counting x from x1
      out2 <- paste0(out2,"lambda",ii,ii,"*",
                     "x",ii," + ")
      } else{ # otherwise, x_n.previousx's+1
        out2 <- paste0(out2,
                       "x",sum(n.variables[1:(i-1)])+ii," + ")  
      }
    }
    if(i ==1){
    out3 <- paste0(out2, "x",n.variables[i])
    } else{
    out3 <- paste0(out2, "x",sum(n.variables[1:i]))  
    }
    output[i] <- out3
    
  }
  print(output)
  

  
  
   
}  

observeEvent(input$action,{
output$lavaan.code <- renderPrint(
  lavaan.code()
)
}
)
  
})

shinyApp(ui,server)

#list(paste("selectInput(newInputId",1:3,",newInputLabel",1:3,",c('Option 1', 'Option 2', 'Option 3'))", sep = "" ))


