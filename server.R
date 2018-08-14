#Testing Game Bingo
library(shiny)
library(DT)
library(shinyjs)
library(shinyBS)
library(V8)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
#disable actionButton function
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

#Define the function to disable all the button
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}


shinyServer(function(input, output, session) {
  
  #card column reactive values so that it will only restart the card when you select a new card
  
  ##################################################################
  #
  #
  #
  #
  #
  #
  #
  #
  ##################################################################

  
  output$BingoTableQuestions <- renderDataTable({
    validate(
      need(input$newBingo,
           message = "")
    )
    BINGO <- matrix(c(W$ccolumn,I$ccolumn,N$ccolumn),byrow = FALSE,nrow = 3,ncol = 3)
    colnames(BINGO) <- c("W","I","N")
    BINGO <-datatable(BINGO,options = list(dom='t',ordering=F),selection = 'none') %>% formatStyle(
      c('W','I','N'),
      backgroundColor = styleEqual(list1$allnums, c(rep('blue',50)))
    )
    
    BINGO
    
  })
  
  ##################################################################
  #
  #
  #
  #
  #
  #
  #
  #
  ##################################################################
  #GOOOOOOO
  val <- reactiveValues(subQ = c())
  observe({
    val$subQ[1] = input$submitQ1
    val$subQ[2] = input$submitQ2
    val$subQ[3] = input$submitQ3
    val$subQ[4] = input$submitQ4
    val$subQ[5] = input$submitQ5
    val$subQ[6] = input$submitQ6
    val$subQ[7] = input$submitQ7
    val$subQ[8] = input$submitQ8
    val$subQ[9] = input$submitQ9
    val$subQ[10] = input$submitQ10
    val$subQ[11] = input$submitQ11
    val$subQ[12] = input$submitQ12
    val$subQ[13] = input$submitQ13
    val$subQ[14] = input$submitQ14
    val$subQ[15] = input$submitQ15
    val$subQ[16] = input$submitQ16
    val$subQ[17] = input$submitQ17
    val$subQ[18] = input$submitQ18

    val$subQ[19] = input$submitQ20
    val$subQ[20] = input$submitQ21
    
    val$subQ[21] = input$newBingo
  })
  
  
  # observeEvent(input$newBingo, {
  #   updateTabItems(session, "tabs", "qqq")
  # }
  # )
  
  
  #This is for giving each question a number
  #Can be randomized to give random order of the questions
  QNum <- reactiveValues(value = 1)
  observeEvent(input$nextButton,({
    QNum$value = QNum$value + 1
  }))
  observeEvent(input$prevButton,({
    QNum$value = QNum$value - 1
  }))
  
  #Output the question
  output$CurrentQuestion <- renderUI({
    if(QNum$value == 1){
      #"Q1:"
      numericInput(inputId = "Q1", "If the null hypothesis is true with a significance level of 5%, how many tests would you expect to be significant out of 100?", min = 0 , max = 100, val = 0 )
    }
    
    else if (QNum$value == 2){
      #"Q2:"
      numericInput(inputId = "Q2", "If the null hypothesis is true with a significance level of 10%, how many tests would you expect to be NOT significant out of 100?", min = 0 , max = 100, val = 0 )
    }
    else if (QNum$value == 3){
      #Q3
      radioButtons(inputId = "Q3", label = "Based on the Confidence Interval in the picture, would you reject the null hypothesis?", choices = c("Yes","No"),selected = character(0))
    }
    else if (QNum$value == 4){
      #Q4
      radioButtons(inputId = "Q4", label = "Based on the Confidence Interval in the picture, would you reject the null hypothesis?", choices = c("Yes","No"),selected = character(0))
    }
    else if (QNum$value == 5){
      #Q5
      "Which alternative hypothesis corresponds to the research question?"
    }
    else if (QNum$value == 6){
      #Q6
      "Which null hypothesis corresponds to the research question?"
    }
    else if (QNum$value == 7){
      #Q7
      radioButtons(inputId = "Q7", label = "Does the p-value get larger or smaller when the sample size increases", choices = c("Larger", "Smaller"),selected = character(0))
      
    }
    else if (QNum$value == 8){
      #Q8
      "Which alternative hypothesis corresponds to the research question?"
    }
    else if (QNum$value == 9){
      #Q9
      "Which null hypothesis corresponds to the research question?"
    }
    else if (QNum$value == 10){
      #Q10
      radioButtons(inputId = "Q10", label = "Does the p-value get larger or smaller when the effect size increases", choices = c("Larger", "Smaller"),selected = character(0))
      
    }
    else if (QNum$value == 11){
      #Q11
      radioButtons(inputId = "Q11", label = "What size p-value suggests that the null hypothesis provides a reasonable explanation for the data", choices = c("Larger", "Smaller"),selected = character(0))
      
    }
    else if (QNum$value == 12){
      #Q12
      radioButtons(inputId = "Q12", label = "The purpose of a test statistic is to measure the difference between the data and what is expected under the null hypothesis.", choices = c("True", "False"),selected = character(0))
    }
    else if (QNum$value == 13){
      #Q13
      radioButtons(inputId = "Q13", label = "If the null hypothesis is false then, ", choices = c("A significance test will always reject the null" = "A", "A significance test may still fail to reject the null" = "B","A significance test will be one sided" = "C", "A significance test will be two sided" = "D"),selected = character(0))
    }
    else if (QNum$value == 14){
      #Q13
      radioButtons(inputId = "Q14", label = "Which p-value might be considered significant ", choices = c("0.99" = "A", "0.32" = "B","0.57" = "C", "0.02" = "D"),selected = character(0))
    }
    else if (QNum$value == 15){
      #Q8
      "Which alternative hypothesis corresponds to the research question?"
    }
    else if (QNum$value == 16){
      #16
      radioButtons(inputId = "Q16", label = "Which p-value might be considered significant ", choices = c("0.999" = "A", "0.7" = "B","0.4" = "C", "0.001" = "D"),selected = character(0))
    }
    else if (QNum$value == 17){
      #17
      withMathJax(
        
        h4(sprintf(
          "What is \\(\\hat{p}\\) ? "
        )
        ))
    }
    else if (QNum$value == 18){
      #17
      withMathJax(
        
        h4(sprintf(
          "What is \\(\\bar{x}\\) ? "
        )
        ))
    }
    else if (QNum$value == 19){
      radioButtons(inputId = "Q19", label = "Increasing the sample size: ", choices = c("Makes the interval wider" = "A", "Makes the interval smaller" = "B","Has no affect on the length of the interval" = "C"),selected = character(0))
      
    }
  })
  
  #This is for if I need to output extra information for the quesion
  output$CurrentQuestion2 <- renderUI({
    if(QNum$value == 3){
      #"Q3:"
      img(src="CIQ3.png",height = 150,width =500,align = "middle")
    }
    
    else if (QNum$value == 4){
      #"Q4:"
      img(src="CIQ4.png",height = 150,width =400,align = "middle")
    }
    else if (QNum$value == 5){
      #"Q5:"
      radioButtons(inputId = "Q5", label = "Research Question: Does the data suggest females are more likely than males to go to the movies", choices = c("The gender effect is not equal to zero" = "A","The gender effect is equal to zero" = "B","Females are more likely than males to go to the movies" = "C","Males are more likely than females to go to the movies" = "D"),selected = character(0))
    }
    else if (QNum$value == 6){
      #"Q5:"
      radioButtons(inputId = "Q6", label = "Research Question: Are basketall players more likely to be left handed than the general population", choices = c("Basketball players are no more likely to be left handed than the general population" = "A","Basketball players are more likely to be left handed than the general population" = "B","Basketball players are less likely to be left handed than the general population" = "C","Basketball players are not equally likely to be left handed than the general population" = "D"),selected = character(0))
    }
    else if (QNum$value == 8){
      #"Q9:"
      radioButtons(inputId = "Q8", label = "Research Question: Suppose there are two different drugs for the common cold; drug A and drug B. Does the data suggest that, on the average, drug A works faster than drug B", choices = c("Drug A works slower than drug B" = "A","Drug A and drug B take the same amount of time to work" = "B","Drug A and drug B take different amounts of time to work" = "C","Drug A works faster than drug B" = "D"),selected = character(0))
      
    }
    else if (QNum$value == 9){
      #"Q9:"
      radioButtons(inputId = "Q9", label = "Research Question: Is Lebron James shooting percentage different from the league average", choices = c("Lebron James shooting percentage is greater than the league average" = "A","Lebron James shooting percentage is not different from the league average" = "B","Lebron James shooting percentage is different from the league average" = "C","Lebron James shooting percentage is less than the league average" = "D"),selected = character(0))
    }
    else if (QNum$value == 15){
      #"Q5:"
      radioButtons(inputId = "Q15", label = "Research Question: Does the color of a car (red or blue) have an affect on happiness", choices = c("Red cars increase the population average happiness" = "A","Blue cars increase the population average happiness" = "B","The color has no affect on population average happiness" = "C","The color has an affect on population average happiness" = "D"),selected = character(0))
    }
    else if (QNum$value == 17){
      #Q17
      radioButtons(inputId = "Q17", label = "", choices = c("The population proportion" = "A", "The sample proportion" = "B", "The population mean" = "C"),selected = character(0))
    }
    else if (QNum$value == 18){
      #Q18
      radioButtons(inputId = "Q18", label = "", choices = c("The population proportion" = "A", "The sample proportion" = "B", "The population mean" = "C","The sample mean" = "D"),selected = character(0))
    }
  })

  
  # output$CurrentQuestionSubmit <- renderUI({
  #   if(QNum$value == 1){
  #     #"Q1:"
  #     bsButton(inputId = "submitQ1",label = "Submit Answer")
  #   }
  #   
  #   else if (QNum$value == 2){
  #     #"Q2:"
  #     bsButton(inputId = "submitQ2", label = "Submit Answer")
  #   }
  #   else if(QNum$value == 3){
  #     #"Q1:"
  #     actionButton(inputId = "submitQ3",label = "Submit Answer")
  #   }
  #   
  #   else if (QNum$value == 4){
  #     #"Q2:"
  #     actionButton(inputId = "submitQ4", label = "Submit Answer")
  #   }
  #   else if (QNum$value == 5){
  #     #"Q2:"
  #     actionButton(inputId = "submitQ5", label = "Submit Answer")
  #   }
  # })
  #RIGHT
 checkerQ1 <- reactiveValues(val = 0)
  observeEvent(input$submitQ1,({
    if(QNum$value == 1 ){
      #"Q1:"
      if(input$Q1 == 5){
        #paste("Correct!")
        checkerQ1$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ1val = 2
      }
    }
  }))
  
  output$Question1Check <- renderText({
    validate(
      need(input$submitQ1 == TRUE,
           message = ""
           )
    )
    
    if(QNum$value == 1 ){
      #"Q1:"
      if(checkerQ1$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ2 <- reactiveValues(val = 0)
  observeEvent(input$submitQ2,({
    if(QNum$value == 2 ){
      #"Q1:"
      if(input$Q2 == 90){
        #paste("Correct!")
        checkerQ2$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ2$val = 2
      }
    }
  }))
  output$Question2Check <- renderText({
    validate(
      need(input$submitQ2 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 2 ){
      #"Q1:"
      if(checkerQ2$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ3 <- reactiveValues(val = 0)
  observeEvent(input$submitQ3,({
    if(QNum$value == 3 ){
      #"Q1:"
      if(input$Q3 == 'No'){
        #paste("Correct!")
        checkerQ3$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ3$val = 2
      }
    }
  }))
  output$Question3Check <- renderText({
    validate(
      need(input$submitQ3 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 3 ){
      #"Q1:"
      if(checkerQ3$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ4 <- reactiveValues(val = 0)
  observeEvent(input$submitQ4,({
    if(QNum$value == 4 ){
      #"Q1:"
      if(input$Q4 == 'Yes'){
        #paste("Correct!")
        checkerQ4$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ4$val = 2
      }
    }
  }))
  output$Question4Check <- renderText({
    validate(
      need(input$submitQ4 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 4 ){
      #"Q1:"
      if(checkerQ4$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  checkerQ5 <- reactiveValues(val = 0)
  observeEvent(input$submitQ5,({
    if(QNum$value == 5 ){
      #"Q1:"
      if(input$Q5 == 'C'){
        #paste("Correct!")
        checkerQ5$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ5$val = 2
      }
    }
  }))
  output$Question5Check <- renderText({
    validate(
      need(input$submitQ5 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 5 ){
      #"Q1:"
      if(checkerQ5$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
    

  checkerQ6 <- reactiveValues(val = 0)
  observeEvent(input$submitQ6,({
    if(QNum$value == 6 ){
      #"Q1:"
      if(input$Q6 == 'A'){
        #paste("Correct!")
        checkerQ6$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ6$val = 2
      }
    }
  }))
  output$Question6Check <- renderText({
    validate(
      need(input$submitQ6 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 6 ){
      #"Q1:"
      if(checkerQ6$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ7 <- reactiveValues(val = 0)
  observeEvent(input$submitQ7,({
    if(QNum$value == 7 ){
      #"Q1:"
      if(input$Q7 == 'Smaller'){
        #paste("Correct!")
        checkerQ7$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ7$val = 2
      }
    }
  }))
  output$Question7Check <- renderText({
    validate(
      need(input$submitQ7 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 7 ){
      #"Q1:"
      if(checkerQ7$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  
  checkerQ8 <- reactiveValues(val = 0)
  observeEvent(input$submitQ8,({
    if(QNum$value == 8 ){
      #"Q1:"
      if(input$Q8 == 'D'){
        #paste("Correct!")
        checkerQ8$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ8$val = 2
      }
    }
  }))
  output$Question8Check <- renderText({
    validate(
      need(input$submitQ8 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 8 ){
      #"Q1:"
      if(checkerQ8$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ9 <- reactiveValues(val = 0)
  observeEvent(input$submitQ9,({
    if(QNum$value == 9 ){
      #"Q1:"
      if(input$Q9 == 'B'){
        #paste("Correct!")
        checkerQ9$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ9$val = 2
      }
    }
  }))
  output$Question9Check <- renderText({
    validate(
      need(input$submitQ9 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 9 ){
      #"Q1:"
      if(checkerQ9$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  
  checkerQ10 <- reactiveValues(val = 0)
  observeEvent(input$submitQ10,({
    if(QNum$value == 10 ){
      #"Q1:"
      if(input$Q10 == 'Smaller'){
        #paste("Correct!")
        checkerQ10$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ10$val = 2
      }
    }
  }))
  output$Question10Check <- renderText({
    validate(
      need(input$submitQ10 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 10 ){
      #"Q1:"
      if(checkerQ10$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ11 <- reactiveValues(val = 0)
  observeEvent(input$submitQ11,({
    if(QNum$value == 11 ){
      #"Q1:"
      if(input$Q11 == 'Larger'){
        #paste("Correct!")
        checkerQ11$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ11$val = 2
      }
    }
  }))
  output$Question11Check <- renderText({
    validate(
      need(input$submitQ11 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 11 ){
      #"Q1:"
      if(checkerQ11$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ12 <- reactiveValues(val = 0)
  observeEvent(input$submitQ12,({
    if(QNum$value == 12 ){
      #"Q1:"
      if(input$Q12 == 'True'){
        #paste("Correct!")
        checkerQ12$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ12$val = 2
      }
    }
  }))
  output$Question12Check <- renderText({
    validate(
      need(input$submitQ12 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 12 ){
      #"Q1:"
      if(checkerQ12$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ13 <- reactiveValues(val = 0)
  observeEvent(input$submitQ13,({
    if(QNum$value == 13 ){
      #"Q1:"
      if(input$Q13 == 'B'){
        #paste("Correct!")
        checkerQ13$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ13$val = 2
      }
    }
  }))
  output$Question13Check <- renderText({
    validate(
      need(input$submitQ13 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 13 ){
      #"Q1:"
      if(checkerQ13$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ14 <- reactiveValues(val = 0)
  observeEvent(input$submitQ14,({
    if(QNum$value == 14 ){
      #"Q1:"
      if(input$Q14 == 'D'){
        #paste("Correct!")
        checkerQ14$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ14$val = 2
      }
    }
  }))
  output$Question14Check <- renderText({
    validate(
      need(input$submitQ14 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 14 ){
      #"Q1:"
      if(checkerQ14$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ15 <- reactiveValues(val = 0)
  observeEvent(input$submitQ15,({
    if(QNum$value == 15 ){
      if(input$Q15 == 'D'){
        #paste("Correct!")
        checkerQ15$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ15$val = 2
      }
    }
  }))
  output$Question15Check <- renderText({
    validate(
      need(input$submitQ15 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 15 ){
      if(checkerQ15$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ16 <- reactiveValues(val = 0)
  observeEvent(input$submitQ16,({
    if(QNum$value == 16 ){
      if(input$Q16 == 'D'){
        #paste("Correct!")
        checkerQ16$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ16$val = 2
      }
    }
  }))
  output$Question16Check <- renderText({
    validate(
      need(input$submitQ16 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 16 ){
      if(checkerQ16$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ17 <- reactiveValues(val = 0)
  observeEvent(input$submitQ17,({
    if(QNum$value == 17 ){
      if(input$Q17 == 'B'){
        #paste("Correct!")
        checkerQ17$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ17$val = 2
      }
    }
  }))
  output$Question17Check <- renderText({
    validate(
      need(input$submitQ17 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 17 ){
      if(checkerQ17$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  checkerQ18 <- reactiveValues(val = 0)
  observeEvent(input$submitQ18,({
    if(QNum$value == 18 ){
      if(input$Q18 == 'D'){
        #paste("Correct!")
        checkerQ18$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ18$val = 2
      }
    }
  }))
  output$Question18Check <- renderText({
    validate(
      need(input$submitQ18 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 18 ){
      if(checkerQ18$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  
  #Question 20
  checkerQ19 <- reactiveValues(val = 0)
  observeEvent(input$submitQ19,({
    if(QNum$value == 19 ){
      if(input$Q19 == 'B'){
        #paste("Correct!")
        checkerQ19$val = 1
        nums$number <- sample(1:15,1)
      }
      else{
        #paste("Wrong")
        checkerQ19$val = 2
      }
    }
  }))
  output$Question19Check <- renderText({
    validate(
      need(input$submitQ19 == TRUE,
           message = ""
      )
    )
    if(QNum$value == 19 ){
      if(checkerQ19$val == 1){
        paste("Correct!")
      }
      else{
        paste("Wrong")
      }
    }
  })
  # observe({
  #   if (input$submitQ1 == TRUE){
  #     #disableActionButton("submitQ1",session)
  #     updateButton(session,submitQ1, style = "danger", disabled = TRUE)
  #   }
  # })


  #THIS is for disabling all the submit buttons after it is pressed once so that they can't submit after they do it once.
  observeEvent(val$subQ,({
    #updateButton(session,"sumbitQ1", style = "color: white;background-color: #1C2C5B;", disabled = TRUE)
    #disableActionButton("submitQ1",session)
    if(val$subQ[1] == 1){
      updateButton(session,paste("submitQ",1,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[2] == 1){
      updateButton(session,paste("submitQ",2,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[3] == 1){
      updateButton(session,paste("submitQ",3,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[4] == 1){
      updateButton(session,paste("submitQ",4,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[5] == 1){
      updateButton(session,paste("submitQ",5,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[6] == 1){
      updateButton(session,paste("submitQ",6,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[7] == 1){
      updateButton(session,paste("submitQ",7,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[8] == 1){
      updateButton(session,paste("submitQ",8,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[9] == 1){
      updateButton(session,paste("submitQ",9,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[10] == 1){
      updateButton(session,paste("submitQ",10,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[11] == 1){
      updateButton(session,paste("submitQ",11,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[12] == 1){
      updateButton(session,paste("submitQ",12,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[13] == 1){
      updateButton(session,paste("submitQ",13,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[14] == 1){
      updateButton(session,paste("submitQ",14,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[15] == 1){
      updateButton(session,paste("submitQ",15,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[16] == 1){
      updateButton(session,paste("submitQ",16,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[17] == 1){
      updateButton(session,paste("submitQ",17,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[18] == 1){
      updateButton(session,paste("submitQ",18,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    if(val$subQ[19] == 1){
      updateButton(session,paste("submitQ",19,sep = ""), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
    }
    #GOOOOOO
    if(val$subQ[21] == 1){
      updateButton(session,paste("newBingo"), style = "color: white;
                   background-color: #1C2C5B;", disabled = TRUE)
      #updateTabItems(session, "tabs", "qqq")
      #updateTabsetPanel(session, "tabView ",selected = "qqq")
      #newtab <- switch(input$tabs, "one" = "two","two" = "one")
      updateTabItems(session, "tabs", "qqq")
    }
  }))
  
  
  
  
  
  
  
  ##############################
  # 
  # 
  # 
  # 
  ##############################
  time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)
  W <- reactiveValues(ccolumn = numeric(3) )
  observeEvent(input$newBingo,({
    W$ccolumn <- sample(1:5,3)
    #time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=TRUE)#start timer
    time$started<-TRUE
  }))
  
  I <- reactiveValues(ccolumn = numeric(3) )
  observeEvent(input$newBingo,({
    I$ccolumn <- sample(6:10,3)
  }))
  
  N <- reactiveValues(ccolumn = numeric(3) )
  observeEvent(input$newBingo,({
    N$ccolumn <- sample(11:15,3)
  }))
  
  # G <- reactiveValues(ccolumn = numeric(5) )
  # observeEvent(input$newBingo,({
  #   G$ccolumn <- sample(31:40,5)
  # }))
  # 
  # O <- reactiveValues(ccolumn = numeric(5) )
  # observeEvent(input$newBingo,({
  #   O$ccolumn <- sample(41:50,5)
  # }))
  
  
  
  nums <- reactiveValues(number = numeric(1) )
  observeEvent(input$newNumber,({
    nums$number <- sample(1:15,1)
    
  }))
  
  
  #RIGHTHERE
  #reactiveValues for the count of numbers(points on the card) obtained
  #This also helps with input for giving a bingo number if correct answer
  count <- reactiveValues(c1 = 0)
  observeEvent(input$newNumber,({
    count$c1 = count$c1+1
  }))
  observeEvent(input$submitQ1,({
    if(checkerQ1$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ2,({
    if(checkerQ2$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ3,({
    if(checkerQ3$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ4,({
    if(checkerQ4$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ5,({
    if(checkerQ5$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ6,({
    if(checkerQ6$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ7,({
    if(checkerQ7$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ8,({
    if(checkerQ8$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ9,({
    if(checkerQ9$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ10,({
    if(checkerQ10$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ11,({
    if(checkerQ11$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ12,({
    if(checkerQ12$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ13,({
    if(checkerQ13$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ14,({
    if(checkerQ14$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ15,({
    if(checkerQ15$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ16,({
    if(checkerQ16$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ17,({
    if(checkerQ17$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ18,({
    if(checkerQ18$val == 1){
      count$c1 = count$c1+1
    }
  }))
  observeEvent(input$submitQ19,({
    if(checkerQ19$val == 1){
      count$c1 = count$c1+1
    }
  }))
  #This helps with the input for giving a bingo number if they get the question RIGHT
  
  list1 <- reactiveValues(allnums = numeric(50) )
  observeEvent(input$newNumber,({
    list1$allnums[count$c1] = nums$number
  }))
  observeEvent(input$submitQ1,({
    if(checkerQ1$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ2,({
    if(checkerQ2$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ3,({
    if(checkerQ3$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ4,({
    if(checkerQ4$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ5,({
    if(checkerQ5$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ6,({
    if(checkerQ6$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ7,({
    if(checkerQ7$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ8,({
    if(checkerQ8$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ9,({
    if(checkerQ9$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ10,({
    if(checkerQ10$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ11,({
    if(checkerQ11$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ12,({
    if(checkerQ12$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ13,({
    if(checkerQ13$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ14,({
    if(checkerQ14$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ15,({
    if(checkerQ15$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ16,({
    if(checkerQ16$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ17,({
    if(checkerQ17$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ18,({
    if(checkerQ18$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  observeEvent(input$submitQ19,({
    if(checkerQ19$val == 1){
      list1$allnums[count$c1] = nums$number
    }
  }))
  output$numberText <- renderText({
    numbers1 <-nums$number
    paste("The numbers are ", numbers1, "And the count is", count$c1)
  })
  
  output$allNumText <- renderText({
    numbers2 <-list1$allnums
    numbers2
  })
  
  #Put the columns into the table (card) and ouput it
  #make it a dataTable to color individual cells easiers
  output$BingoTable <- renderDataTable({
    validate(
      need(input$newBingo,
           message = "")
    )
    BINGO <- matrix(c(W$ccolumn,I$ccolumn,N$ccolumn),byrow = FALSE,nrow = 3,ncol = 3)
    colnames(BINGO) <- c("W","I","N")
    BINGO <-datatable(BINGO,options = list(dom='t',ordering=F),selection = 'none') %>% formatStyle(
      c('W','I','N'),
      backgroundColor = styleEqual(list1$allnums, c(rep('blue',50)))
    )
    
    BINGO
    
  })
  
  
  # #For checking if the user has a BINGO
  # BingoCheck <- reactive({
  #   #B$ccolumn,I$ccolumn,N$ccolumn,G$ccolumn,O$ccolumn
  #   #Vertical BINGOS
  #   if(B$ccolumn[1] %in% list1$allnums && B$ccolumn[2] %in% list1$allnums && B$ccolumn[3] %in% list1$allnums && B$ccolumn[4] %in% list1$allnums && B$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(I$ccolumn[1] %in% list1$allnums && I$ccolumn[2] %in% list1$allnums && I$ccolumn[3] %in% list1$allnums && I$ccolumn[4] %in% list1$allnums && I$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(N$ccolumn[1] %in% list1$allnums && N$ccolumn[2] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums && N$ccolumn[4] %in% list1$allnums && N$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(G$ccolumn[1] %in% list1$allnums && G$ccolumn[2] %in% list1$allnums && G$ccolumn[3] %in% list1$allnums && G$ccolumn[4] %in% list1$allnums && G$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(O$ccolumn[1] %in% list1$allnums && O$ccolumn[2] %in% list1$allnums && O$ccolumn[3] %in% list1$allnums && O$ccolumn[4] %in% list1$allnums && O$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   #Horizonal BINGOS
  #   else if(B$ccolumn[1] %in% list1$allnums && I$ccolumn[1] %in% list1$allnums && N$ccolumn[1] %in% list1$allnums && G$ccolumn[1] %in% list1$allnums && O$ccolumn[1] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(B$ccolumn[2] %in% list1$allnums && I$ccolumn[2] %in% list1$allnums && N$ccolumn[2] %in% list1$allnums && G$ccolumn[2] %in% list1$allnums && O$ccolumn[2] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(B$ccolumn[3] %in% list1$allnums && I$ccolumn[3] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums && G$ccolumn[3] %in% list1$allnums && O$ccolumn[3] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(B$ccolumn[4] %in% list1$allnums && I$ccolumn[4] %in% list1$allnums && N$ccolumn[4] %in% list1$allnums && G$ccolumn[4] %in% list1$allnums && O$ccolumn[4] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(B$ccolumn[5] %in% list1$allnums && I$ccolumn[5] %in% list1$allnums && N$ccolumn[5] %in% list1$allnums && G$ccolumn[5] %in% list1$allnums && O$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   #Diagonal BINGOS
  #   else if(B$ccolumn[1] %in% list1$allnums && I$ccolumn[2] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums && G$ccolumn[4] %in% list1$allnums && O$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   else if(B$ccolumn[5] %in% list1$allnums && I$ccolumn[4] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums && G$ccolumn[2] %in% list1$allnums && O$ccolumn[1] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   #Four Corners BINGO
  #   else if(B$ccolumn[1] %in% list1$allnums && B$ccolumn[5] %in% list1$allnums && O$ccolumn[1] %in% list1$allnums && O$ccolumn[5] %in% list1$allnums){
  #     return(TRUE)
  #   }
  #   #ELSE NO BINGO
  #   else{
  #     return(FALSE)
  #   }
  # })
  
  #For checking if the user has a BINGO
  BingoCheck <- reactive({
    #B$ccolumn,I$ccolumn,N$ccolumn,G$ccolumn,O$ccolumn
    #Vertical BINGOS
    if(W$ccolumn[1] %in% list1$allnums && W$ccolumn[2] %in% list1$allnums && W$ccolumn[3] %in% list1$allnums){
      return(TRUE)
    }
    else if(I$ccolumn[1] %in% list1$allnums && I$ccolumn[2] %in% list1$allnums && I$ccolumn[3] %in% list1$allnums){
      return(TRUE)
    }
    else if(N$ccolumn[1] %in% list1$allnums && N$ccolumn[2] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums){
      return(TRUE)
    }
    #Horizonal BINGOS
    else if(W$ccolumn[1] %in% list1$allnums && I$ccolumn[1] %in% list1$allnums && N$ccolumn[1] %in% list1$allnums){
      return(TRUE)
    }
    else if(W$ccolumn[2] %in% list1$allnums && I$ccolumn[2] %in% list1$allnums && N$ccolumn[2] %in% list1$allnums){
      return(TRUE)
    }
    else if(W$ccolumn[3] %in% list1$allnums && I$ccolumn[3] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums){
      return(TRUE)
    }
    #Diagonal BINGOS
    else if(W$ccolumn[1] %in% list1$allnums && I$ccolumn[2] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums){
      return(TRUE)
    }
    else if(W$ccolumn[5] %in% list1$allnums && I$ccolumn[4] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums){
      return(TRUE)
    }
    #Four Corners BINGO
    else if(W$ccolumn[1] %in% list1$allnums && W$ccolumn[3] %in% list1$allnums && N$ccolumn[1] %in% list1$allnums && N$ccolumn[3] %in% list1$allnums){
      return(TRUE)
    }
    #ELSE NO BINGO
    else{
      return(FALSE)
    }
  })
  
  observeEvent(input$checkBingo,({
    checker <- BingoCheck()
    if(checker == TRUE){
      {time$timer<-reactiveTimer(Inf)} #end timer
    }
  }))
  output$BingoText <- renderText({
    checker <- BingoCheck()
    if(checker == TRUE){
      paste("You WIN!")
      #{time$timer<-reactiveTimer(Inf)} #end timer
    }
    else{
      paste("You do NOT WIN")
    }
  })
  observe({
    time$timer()
    if(isolate(time$started))
      time$inc<-isolate(time$inc)+1
  })
  output$timer1 <- renderPrint({
    cat("you have used:", time$inc, "secs")})
})




