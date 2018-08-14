#Testing Game BINGO

library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)
library(shinyjs)
library(V8)
library(discrimARTs)


jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Testing Game",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(id = "tabs",
                  
                  menuItem("Intro", tabName = "rules", icon = icon("university")),
                  #menuItem("Part 1", tabName = "first", icon = icon("table")),
                  menuItem("Questions", tabName = "qqq", icon = icon("pencil-square"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$style(
                  type = "text/css",
                  ".content-wrapper,.right-side {
                  background-color: white;
                  }"
                ),
                tabItems(
                  
                  tabItem(tabName = "rules",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(12,
                                   h3("About:"),
                                   h4("In this app the goal is to quiz your knowledge of hypothesis testing."),
                                   # h3("Background:"),
                                   # h4(""),
                                   h3("Instructions:"),
                                   h4("1. Press Go."),
                                   h4("2. Answer the question that shows up."),
                                   h4("3. Click the 'Submit' button when you are ready to submit the answer."),
                                   h4("4. Click the 'Next' button to go to the next question"),
                                   h4("5. Each correct answer will give you a number. If the number is on your card then the space will turn blue. "),
                                   h4("6. When you have 3 numbers line up horizontally, vertically, or diagonally then press the 'WIN' button and your time will end."),
                                   br(),
                                   h4("The goal is to win in the fastest time. "),
                                   h4("When you are ready to  start the game press Go.")
                                   
                            )
                          ),
                          fluidRow(
                            
                            column(5,
                                   br()),
                            column(4,
                                   tags$head(
                                     tags$style(HTML('#newBingo{background-color:orange}'))
                                   ),
                                   bsButton(inputId = "newBingo", label = "Go", width='30%')
                                   )
                          )
                          
                  ),
                  
                  #This tab not used in output version.
                  #Only keep it for debugging
                  tabItem(tabName = "first",
                          sidebarLayout(
                            sidebarPanel(

                              #actionButton(inputId = "newBingo", label = "Go"),
                              br(),
                              #actionButton(inputId = "newTic", label = "New Game"),
                              #br(),
                              
                              actionButton(inputId = "newNumber", label = "Get a new Number"),
                              textOutput("numberText"), br(),
                              textOutput("allNumText"),br()
                              #actionButton(inputId = "checkBingo", label = "Check For Win!"),
                              # conditionalPanel("input.checkBingo == true",
                              #                  textOutput("BingoText")
                              # )
                              #selectInput(inputId = "boardSize", label = "Select the size of the board", choices = c("3x3","4x4","5x5"))
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              absolutePanel(
                                dataTableOutput("BingoTable"),draggable = TRUE
                              )
                              
                            )
                          )
                  ),

                  tabItem(tabName = "qqq",
                          br(),
                          br(),
                          fluidRow(
                          column(3,
                                 #actionButton(inputId = "newNumber", label = "Get a new Number")
                                 
                                 #actionButton(inputId = "prevButton", label = "Previous Question"),

                                 tags$style(type='text/css', '#timer1 {background-color:#2C3E50; font-size: 30px; 
                                   color:white;font-weight: bold;font family:Sans-serif;text-align: center; border-radius: 100px}'),
                                 textOutput("timer1"),
                                 br(),
                                 absolutePanel(
                                   #DRAGGABLE OR MOVABLE
                                   dataTableOutput("BingoTableQuestions"),draggable = TRUE
                                 )
                                 ),
                          column(9,
                                 uiOutput("CurrentQuestion"),
                                 uiOutput("CurrentQuestion2")
                                 #uiOutput("CurrentQuestionSubmit"),

                                
                          )
                          ),
                          fluidRow(
                            column(3,
                                   " "
                                   ),
                            column(2,
                                   actionButton(inputId = "checkBingo", label = "Check for Win")
                                   ),
                            column(2,
                                   conditionalPanel("input.nextButton == 0",
                                                    bsButton(inputId = "submitQ1",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 1",
                                                    bsButton(inputId = "submitQ2",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 2",
                                                    bsButton(inputId = "submitQ3",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 3",
                                                    bsButton(inputId = "submitQ4",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 4",
                                                    bsButton(inputId = "submitQ5",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 5",
                                                    bsButton(inputId = "submitQ6",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 6",
                                                    bsButton(inputId = "submitQ7",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 7",
                                                    bsButton(inputId = "submitQ8",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 8",
                                                    bsButton(inputId = "submitQ9",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 9",
                                                    bsButton(inputId = "submitQ10",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 10",
                                                    bsButton(inputId = "submitQ11",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 11",
                                                    bsButton(inputId = "submitQ12",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 12",
                                                    bsButton(inputId = "submitQ13",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 13",
                                                    bsButton(inputId = "submitQ14",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 14",
                                                    bsButton(inputId = "submitQ15",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 15",
                                                    bsButton(inputId = "submitQ16",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 16",
                                                    bsButton(inputId = "submitQ17",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 17",
                                                    bsButton(inputId = "submitQ18",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 18",
                                                    bsButton(inputId = "submitQ19",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 19",
                                                    bsButton(inputId = "submitQ20",label = "Submit Answer")
                                   ),
                                   conditionalPanel("input.nextButton == 20",
                                                    bsButton(inputId = "submitQ21",label = "Submit Answer")
                                   ),
                                   # 
                                   # br(),
                                   # 
                                   # br(),br(),
                                   # 
                                   # br(),br(),
                                   
                                   
                                   textOutput("Question1Check"),
                                   textOutput("Question2Check"),
                                   textOutput("Question3Check"),
                                   textOutput("Question4Check"),
                                   textOutput("Question5Check"),
                                   textOutput("Question6Check"),
                                   textOutput("Question7Check"),
                                   textOutput("Question8Check"),
                                   textOutput("Question9Check"),
                                   textOutput("Question10Check"),
                                   textOutput("Question11Check"),
                                   textOutput("Question12Check"),
                                   textOutput("Question13Check"),
                                   textOutput("Question14Check"),
                                   textOutput("Question15Check"),
                                   textOutput("Question16Check"),
                                   textOutput("Question17Check"),
                                   textOutput("Question18Check"),
                                   textOutput("Question19Check"),
                                   textOutput("Question20Check")
                            ),
                            column(1,
                                   actionButton(inputId = "nextButton",label = "Next Question")
                                   )
                            
                          ),
                          fluidRow(
                            column(3,
                                   ""
                                   ),
                            column(4,
                                   conditionalPanel("input.checkBingo == true",
                                                    textOutput("BingoText"),
                                                    tags$head(tags$style("#BingoText{color: red;font-size: 30px;font-style: italic;}"))
                                   )
                                   )
                            
                          )
                          
                          )
                )
                ))



