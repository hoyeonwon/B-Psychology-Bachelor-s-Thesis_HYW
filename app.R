library(shiny)
library(shinyjs)
library(car)
library(plot.matrix)
library(shinyWidgets)


#redirect to website
jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'https://www.lab.uva.nl/lab/index.php/projects/end_online_project/';});"


ui <- fluidPage(
  useShinyjs(),
  div(id="information", style="font-size:1.5vw",
      titlePanel( 
        h1("Information Letter", align = "center")),
      h4("Thank you for your interest in our study. Please use your laptop to participate in this study.",align="center"),
      h4(em("Please take your time to read this information letter and the informed consent form.", br(), "In this 
        letter the crucial information retaining to the nature, purpose of the study and its methods 
        will be explained.",style="color:red"),align="center"),
      p(strong("What is the purpose of this study?"), style="font-size:2vw"),
      tags$ul(tags$li("This study aims to understand individuals' behavior when playing online game.", style="font-size:1.5vw")),
      p(strong("What kind of data will be collected?", style="font-size:2vw")),
      tags$ul(style="font-size:1.5vw",
              tags$li("The answers given to all the questions asked during the experiment will be collected."),
              tags$li("The (in-game) actions made by the participant during the experiment will be collected.")
      ),
      p(strong("Additional Information:"), style="font-size:2vw"),
      tags$ul(style="font-size:1.5vw",
              tags$li(strong("Reward:"),"By completing this study, you will be rewarded with 0.5 research credits & a chance to enter a raffle for 50-euros bol.com voucher"),
              tags$li(strong("Time required:"), "This study will take approximately 20 minutes to complete."),
              tags$li(strong("Confidentiality:"), "Your participation in this study will remain confidential. The data will be shared publicly in anonymized form. No personally identifiable information will be associated with your data. This will not be shared with any third parties."),
              tags$li(strong("Participation and withdrawal:"), "Your participation in this study is completely voluntary. You may retract your consent to participate in this study at any 
moment and do not have to give any explanation for doing so.")
      ),
      p("",br(),"",style="font-size:2vw"),
      fluidRow(
        column(6, offset = 10,
               actionButton("Next0",strong("Next Page"),width=150)
        )
      )
  ),
  hidden(div(
    id="consent_form", style="font-size:1.5vw",
    titlePanel( 
      h1("Consent Form", align = "center")),
    p("Should you have questions about this study at any given moment, please contact any of the responsible 
researchers below."),
    tags$ul(
      tags$li("Jill de Ron: j.deron@uva.nl")
    ),
    p("Formal complains about this study can be addressed to the Ethics Review Board:"),
    tags$ul(
      tags$li("Wery van den Wildenberg (Chair): W.P.M.vandenWildenberg@uva.nl"), 
      tags$li("Joost van der Meer (Secretary): j.j.w.vandermeer@uva.nl"), 
      tags$li("Lourens Waldorp (Psychological Methods): L.J.Waldorp@uva.nl ")
    ),
    p("For  questions  or  complaints  about  the  processing  of  your  personal  data  you  can  also  contact  the  data 
protection officer of the University of Amsterdam via fg@uva.nl."),
    strong("By continuing the study, you agree to all the nature and methods of this study.",align = "center",style ="color:blue"),
    tags$ul(em(
      tags$li("I am 16 or older;"), 
      tags$li("I am fluent in English"),
      tags$li("I have read and understand the information letter;"), 
      tags$li("I agree to participate in this study and I agree with the use of the data that are collected;"),
      tags$li("I reserve the right to withdraw my participation from the study at any moment without providing 
any reason.")
    )),
    fluidRow(
      column(6, offset = 10, actionButton("Accept",strong("Accept and Proceed"),width=170)))
  )), #include withdraw buttons in every section (how to quit experiment?)
  hidden(div(id="Demographics", style="font-size:1.5vw",
             titlePanel( 
               h1("Demographics", align = "center")),
             h4("Following questions are intended to gather general demographics of participants:"),
             sliderInput("age","Please indicate your age", min=0, max=90, value=0, step=1,width=500),
             radioButtons("gender", "Please indicate your gender",
                          choices = list("Female" = 1, "Male" = 2,
                                         "Prefer Not to Say" = 3),selected = 1, width=500),
             radioButtons("occupation","Are you currently a student?",
                          choices = list("Yes" = 1, "No" = 2), selected=1, width=500),
             fluidRow(
               column(6, offset = 10,
                      actionButton("Nextd",strong("Next Page"),width=150)))
  )),
  hidden(div(id="threeparts",style="font-size:1.5vw",
             titlePanel( 
               h1("General Instruction", align = "center")),
             p("This study consists of three parts in investigating how people behave when playing games:",br(),
               "I. Online game with an instruction given at the beginning",br(),
               "II. Questions about the game played in part I", br(),
               "III. Two questionnaires about your daily behavior", align="center"),
             fluidRow(
               column(6, offset = 10, actionButton("Nextt",strong("Next Page"))))
  )),
  hidden(div(id="Instruction", style="font-size:1.5vw",
             titlePanel( 
               h1("Part I. Instruction", align = "center")),
             sidebarLayout(position = "right",
                           sidebarPanel(
                             h4("Please answer the questions below to continue with the experiment."),
                             radioButtons(
                               inputId="Instructionq1",
                               label="1. What happens after the light gray square is selected?",
                               choiceNames = list(
                                 "a.	Changes to dark grey","b.	Remains as light grey","c. Payoff is shown", "d. Both a & c", "e. All of the above"
                               ),
                               choiceValues = list(
                                 "a.	Changes to dark grey","b.	Remains as light grey","c. Payoff is shown", "d. Both a & c", "e. All of the above"
                                 
                               )),
                             radioButtons(
                               inputId = "Instructionq2",
                               label = "2. Can you select the dark grey square again?",
                               choiceNames = list ("a. Yes", "b. No"),
                               choiceValues = list ("a. Yes","b. No")
                             ),
                             actionButton("Next1",strong("Next Page"))),
                           mainPanel(
                             h3(strong("Please be familiar with these instructions")),
                             p(strong("1)"),"A grid of 120 light-gray square keys will be presented in 12 x 10 pattern", 
                               tags$br(),
                               strong("2)"),"The selected square will turn dark grey.",
                               tags$br(),
                               strong("3)"),"Either light gray or dark grey squares can be selected by clicking on them.",
                               tags$br(),
                               strong("4)"), "A selected square will show a payoff in points."),
                             p(strong("Your goal is to collect as many points as possible!")),
                             
                           )
             ), 
             span(textOutput("txt"),style ="color:red"))
  ),
  hidden(div(
    id="beforemain",
    style="font-size:1.5vw",
    titlePanel(h1("Are you ready?", align = "center")),
    p("Now, the first part of the experiment will start!", br(),
      "As explained, the second part of the experiment is closely related to the first part,",br(), 
      "so please pay attention to what's going on in the game while playing.", br(),
      "Also, please be aware that the game will automatically end and move to the next part once enough selections are made!", br(),
      "Please click the 'Start' button to start the game! Good luck :) ", align="center"),
    fluidRow(
      column(6, offset = 6, actionButton("Startgame",strong("Start"))))
  )),
  hidden(
    div(id="main",style="font-size:1.5vw",
        titlePanel( 
          h1("Part I. Online Game Task", align = "center")),
        # the grid
        fluidRow(
          column(8, offset = 3,
                 plotOutput("gridPlot", click = "plot_click", width = 600, height = 600)))
    )),
  hidden(div(id="Indirect", style="font-size:1.2 vw",
             sidebarLayout(
               sidebarPanel(
                 h4("If you have no clue about the chance of each payoff, put 25 for every question"),
                 h4("Please make sure that percentages add up to 100"),
                 sliderInput("zero","What is the chance (in percentage) of 0 being the outcome of the marked square?",value = 0,min = 0,max = 100,step = 1),
                 sliderInput("plus_10","What is the chance (in percentage) of +10 being the outcome of the marked square?",value = 0,min = 0,max = 100,step = 1),
                 sliderInput("minus_1","What is the chance (in percentage) of -1 being the outcome of the marked square?",value = 0,min = 0,max = 100,step = 1),
                 sliderInput("plus_11","What is the chance (in percentage) of +11 being the outcome of the marked square?",value = 0,min = 0,max = 100,step = 1),    
                 actionButton("Next2",strong("Next Page"))
               ),
               mainPanel(
                 h1("Part II. Prediction Task 1"),
                 h4(textOutput("plot_title")),
                 fluidRow(column(8,offset=2,plotOutput("BoardPlot", width = 550,height=550) ))
                 
               ),
               position = "right"
             ),
             verbatimTextOutput("error"))),
  hidden(div (id="Indirect2", style="font-size:1.2 vw",
              sidebarLayout(
                sidebarPanel(
                  numericInput("total_score_guess",label = "What do you think the total score of this participant was at that moment?",value = 0),
                  actionButton("NextPage",strong("Next Page"))
                ),
                mainPanel(
                  h1("Part II. Prediction Task 2"),
                  h4(textOutput("plotTitle")),
                  fluidRow(column(8,offset=2, plotOutput("BoardPlot_tot", width = 550, height =550)))
                ),
                position = "right"
              ),
  )),
  hidden(div(id="Direct", style="font-size:1.5vw",
             titlePanel( 
               h1("Part II. Questionnaire", align = "center")),
             p("You have completed 100 selections in the game in the beginning of this experiment. This means that you have clicked on the grid 100 times."),
             p(strong("Please answer the following questions."), br(),
               "Please choose one of the options in the slider:"),
             fluidRow(column(8,offset=3,
                             sliderTextInput("DirPC1",
                                             "During the first 25 selections, I felt control over what payoff I could get on that selection", 
                                             choices = list("Strongly Disagree", "2","3","4","5","6","7","8","9","Strongly Agree"),
                                             selected = "Strongly Disagree", width=600),
                             sliderTextInput("DirPC2","During the second 25 selections, I felt control over what payoff I could get on that selection", 
                                             choices = list("Strongly Disagree", "2","3","4","5","6","7","8","9","Strongly Agree"),
                                             selected = "Strongly Disagree", width= 600),
                             sliderTextInput("DirPC3","During the third 25 selections, I felt control over what payoff I could get on that selection", 
                                             choices = list("Strongly Disagree", "2","3","4","5","6","7","8","9","Strongly Agree"),
                                             selected = "Strongly Disagree", width=600),
                             sliderTextInput("DirPC4","During the fourth 25 selections, I felt control over what payoff I could get on that selection", 
                                             choices = list("Strongly Disagree", "2","3","4","5","6","7","8","9","Strongly Agree"),
                                             selected = "Strongly Disagree", width=600))),
             fluidRow(
               column(6, offset = 10, 
                      actionButton("Next3",strong("Next Page"),width=150))))),
  hidden(div(id="covariate1",style="font-size:1.5vw",
             titlePanel( 
               h1("Part III. Questionnaire 1", align = "center")),
             p(strong("Please answer the following questions according to how you feel/think in general."), br(),
               "Please choose one of the options in the slider:"),
             fluidRow(column(8,offset=3,
                             sliderTextInput(
                               inputId = "Lcq1",
                               label = "I have little control over the things that happen to me",
                               choices = list("Strongly Disagree", "Disagree","Neutral","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Lcq2",
                               label = "There really is no way I can solve some of the problems I have",
                               choices = list("Strongly Disagree", "Disagree","Neutral","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Lcq3",
                               label = "I can do just about anything I really set my mind to do",
                               choices = list("Strongly Disagree", "Disagree","Neutral","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Lcq4",
                               label = "I often feel helpless in dealing with the problems of life",
                               choices = list("Strongly Disagree", "Disagree","Neutral","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Lcq5",
                               label = "Sometimes I feel like I am being pushed around in life",
                               choices = list("Strongly Disagree", "Disagree","Neutral","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Lcq6",
                               label = "What happens to me in the future mostly depends on me",
                               choices = list("Strongly Disagree", "Disagree","Neutral","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Lcq7",
                               label = "There is little I can do to change many of the important things in my life",
                               choices = list("Strongly Disagree", "Disagree","Neutral","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ))),
             fluidRow(
               column(6, offset = 10, actionButton("Next4",strong("Next Page"),width=150)))
  )),
  hidden(div(id="covariate2",style="font-size:1.5vw",
             titlePanel( 
               h1("Part III. Questionnaire 2", align = "center")),
             p(strong("Please answer the following questions according to how you feel/think in general."), br(),
               "Please choose one of the options in the slider:"),
             fluidRow(column(8,offset=3,
                             sliderTextInput(
                               inputId = "Cq1",
                               label = "I like situations, in which I can find out how capable I am",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq2",
                               label = "When I am confronted with a problem, which I can possibly solve, I am enticed to start working on it immediately",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq3",
                               label = "I enjoy situations, in which I can make use of my abilities",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq4",
                               label = "I am appealed by situations allowing me to test my abilities",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq5",
                               label = "I am attracted by tasks, in which I can test my abilities",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq6",
                               label = "I am afraid of failing in somewhat difficult situations, when a lot depends on me",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq7",
                               label = "I feel uneasy to do something if I am not sure of succeeding",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq8",
                               label = "Even if nobody would notice my failure, I'm afraid of tasks, which I'm not able to solve",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq9",
                               label = "Even if nobody is watching, I feel quite anxious in new situations",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ),
                             sliderTextInput(
                               inputId = "Cq10",
                               label = "If I do not understand a problem immediately I start feeling anxious",
                               choices = list("Strongly Disagree", "Disagree","Agree","Strongly Agree" ),
                               selected = "Strongly Disagree",
                               width=600
                             ))),
             fluidRow(
               column(6, offset = 10, actionButton("Next5",strong("Next Page"),width=150)))
  )),
  hidden(div(id="debrief", style="font-size:1.5vw",
             titlePanel( 
               h1("Debrief", align = "center")),
             p("This study aims to directly replicate the study by Teodorescu and Erev (2014). The purpose is to analyse individuals' behavior when they perceived uncontrollability over enviornment, reward frequency, and/or average reward size."), 
             tags$ul(
               tags$li("Participants are presented with a 10x12 grid that they either explore or do not explore.Non-exploration always leads to no reward.
             The reward frequency variable divides participants into four groups based on a probability with which exploration of the grid leads to a reward (p = .1, .2, .4 and 1) of 10 points.
             The control versus yoked variable divides participants into those who can determine their reward by exploring the 10x12 grid 
             (control group) and those whose rewards are independent of their exploration of the grid (yoked group) in the first 50 out of 100 selections. 
             In the second set of 50 selections, both control and yoked groups have control over their rewards."),
               tags$li("All participants then complete explicit and implicit measures of perceived control. 
             In an explicit measure, participants are directly asked about their perceived control. 
             In two implicit measures, participants are shown a board of another player t-selections into the task and they are 
             asked to guess the total score and the reward of that player on selection t+1. 
             Participants then answer questionnaires on locus of control and on achievement motivation to participate"),
               tags$li("Final scores of participants will be adjusted according to the p condition, so the participants assigned to lower p condition have the same chance of winning the voucher as the participants in the higher p condition.")),
             p("If you have any other questions or concerns about the experiment, please let one of the researchers know about it."),
             tags$ul(
               tags$li("Jill de Ron: j.deron@uva.nl")
             ),
             textInput("email", "If you want to participate in the raffle for a 50-euro bol.com voucher, please write down your email address (This is optional). The better you perform on the task, the higher the chance you will win the voucher!", value = "",width = 1000),
             verbatimTextOutput("value"),
             h3("Thank you for your participation :)",align="center"),
             tags$head(tags$script(jscode)),     
             fluidRow(
               column(6, offset = 10, actionButton("Finish",strong("Save & Finish"),width=150)))
  ))
)

################## SERVER ################## 

server <- function(input, output,session) {
  #sample unique number for ID
  id  <- gsub(" ","",gsub(":","",gsub("-","",as.character(Sys.time()))))
  start_time = Sys.time() 
  
  observeEvent(input$Next0,{
    shinyjs::hide("information")
    shinyjs::show("consent_form")
  })
  
  
  observeEvent(input$Accept, {
    {
      shinyjs::hide("consent_form")
      shinyjs::show("Demographics")
      
    }
  })
  
  #demographics
  demographic<- reactive({
    data.frame(
      ID = id,
      Age = as.character(input$age),
      Gender = as.character(input$gender),
      Occupation = as.character(input$occupation),
      stringsAsFactors = FALSE
    )})
  
  pemail<- reactive({
    data.frame (
      ID =id,
      emails = input$email,
      Total_score = vals$total,
      Reward_frequency = rew_prob,
      Condition = cond
    )
  })
  
  observeEvent(input$Nextd, {
    write.csv(demographic(),file=paste0("demographics",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
    {shinyjs::hide("Demographics")
      shinyjs::show("threeparts")}
    
  })
  observeEvent(input$Nextt, {
    shinyjs::hide("threeparts")
    shinyjs::show("Instruction")
  })
  data<-eventReactive(input$Next1,{
    if (input$Instructionq1 == "d. Both a & c" & input$Instructionq2 == "a. Yes") 
      paste("")
    else paste("Please read the instructions again")
  })
  output$txt <- renderText({
    data()
  })
  observeEvent(input$Next1,{
    if (input$Instructionq1 == "d. Both a & c" & input$Instructionq2 == "a. Yes"){
      start_main <<- Sys.time()
      shinyjs::hide("Instruction")
      shinyjs::show("beforemain")
    }
    else {shinyjs::hide("threeparts")}
  })
  
  observeEvent(input$Startgame, {
    shinyjs::hide("beforemain")
    shinyjs::show("main")
  })
  
  ##################  Main Page################## 
  # allocation of participants into conditions
  rew_prob <- sample(c(0.1, 0.2, 0.4, 1), 1, replace = T ) # sampling a reward frequency condition for a specific participant; include statement about whether the group is full (e.g. 25 participants) then sample from only three other?
  cond <- sample(c("CTRL", "YOK"), 1, replace = T) # sampling a control condition for a specific participant
  
  vals <- reactiveValues(
    grid = matrix(0, nrow = 10, ncol = 12),
    score = 0,
    x = NA,
    y = NA,
    counter = 0,
    total = 0,
    exploration = data.frame(initial = 1), # to be filled with participant id, their reward frequency condition, their control condition, their reward per trial, whether they explored or not per trial
    times_clicks = data.frame(initial = 0)
  )
  
  
  # Handle clicks on the plot:
  observeEvent(input$plot_click, {
    
    # Get the coordinates:
    vals$x <- round(as.numeric(input$plot_click$x))
    vals$y <- car::recode(round(as.numeric(input$plot_click$y)),
                          "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
    vals$counter <- vals$counter + 1 # counting the number of clicks
    vals$exploration <- cbind(vals$exploration, abs(vals$grid[vals$y,vals$x] - 1)) # indicate whether participant explored in this trial or not (1 = explored, 0 = not explored)
    vals$times_clicks <- cbind(vals$times_clicks, Sys.time()) 
    
    # yoked condition on the first 50 trials
    if(vals$counter <= 50 & cond == "YOK") {
      
      # p_reward = 0.1
      if(rew_prob == 0.1) {
        reward <- sample(c(11, 0), 1, prob=c(rew_prob*0.67, 1-(rew_prob*0.67)))
        if (vals$grid[vals$y,vals$x] == 0) {
          vals$grid[vals$y,vals$x] <- 1
          vals$score <- reward - 1
        } else {
          vals$score <- reward
        }
      }
      
      # p_reward = 0.2
      if(rew_prob == 0.2) {
        reward <- sample(c(11, 0), 1, prob=c(rew_prob*0.72, 1-(rew_prob*0.72)))
        if (vals$grid[vals$y,vals$x] == 0) {
          vals$grid[vals$y,vals$x] <- 1
          vals$score <- reward - 1
        } else {
          vals$score <- reward
        }
      }
      
      # p_reward = 0.4
      if(rew_prob == 0.4) {
        reward <- sample(c(11, 0), 1, prob=c(rew_prob*0.83, 1-(rew_prob*0.83)))
        if (vals$grid[vals$y,vals$x] == 0) {
          vals$grid[vals$y,vals$x] <- 1
          vals$score <- reward - 1
        } else {
          vals$score <- reward
        }
      }
      
      # p_reward = 1
      if(rew_prob == 1) {
        reward <- sample(c(11, 0), 1, prob=c(rew_prob*0.95, 1-(rew_prob*0.95)))
        if (vals$grid[vals$y,vals$x] == 0) {
          vals$grid[vals$y,vals$x] <- 1
          vals$score <- reward - 1
        } else {
          vals$score <- reward
        }
      }
      
      vals$exploration <- cbind(vals$exploration, vals$score)
      vals$total <- vals$total + vals$score
      
      
      # yoked condition on the last 50 trials and control condition throughout the whole experiment       
    } else if (vals$counter <=99) { 
      if (vals$grid[vals$y,vals$x] == 0) {
        vals$grid[vals$y,vals$x] <- 1
        vals$score <- sample(c(10, -1), 1, prob=c(rew_prob, 1-rew_prob))
      } else {
        vals$score <- 0
      }
      vals$exploration <- cbind(vals$exploration, vals$score)
      vals$total <- vals$total + vals$score
      
      # when the 100 experimental trials are over
    } else {
      
      vals$exploration <- cbind(vals$exploration, vals$score)
      vals$total <- vals$total + vals$score
      
      
      main_time <<- as.numeric(difftime(Sys.time(),start_main),units = "mins")
      vals$exploration <- cbind(id, rew_prob, cond, vals$exploration[,2:201], main_time)
      names(vals$exploration) <- c("ID", "Reward Frequency","Control Condition", c(rbind(paste("Exploration Trial", 1:100), paste("Reward Trial", 1:100))), "Duration of main")
      write.csv(isolate(vals$exploration),file=paste0("exploration",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE) # logging the data
      
      
      for(i in 1:99) { # calculating the time difference between clicks (in seconds)
        vals$times_clicks[i] <- vals$times_clicks[i+2] - vals$times_clicks[i+1]
      }
      
      
      vals$times_clicks <- cbind(id, rew_prob, cond, vals$times_clicks[,1:99])
      names(vals$times_clicks) <- c("ID", "Reward Frequency","Control Condition", c(paste("Before Trial", 2:100)))
      write.csv(isolate(vals$times_clicks), file=paste0("times_clicks",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
      {
        start_indirect1 <<- Sys.time()
        shinyjs::hide("main")
        shinyjs::show("Indirect") #move to next page when the experiment is done
      }
    } 
    
  })
  
  
  output$gridPlot <- renderPlot({ 
    if(vals$counter == 0) {
      plot(vals$grid, key = NULL, col = c("light grey"), main = paste("Selection 1, ", "Total score: 0", sep = ""), xlab = "", ylab = "", axis.col = NULL, axis.row = NULL)
      
    } else {
      plot(vals$grid, key = NULL, col = c("light grey", "dark grey"), main = paste("Selection ", vals$counter + 1, ", ", "Total score: ", vals$total, sep = ""), xlab = "", ylab = "", axis.col = NULL, axis.row = NULL)
      text(x = vals$x, y = 11 - vals$y, labels = vals$score)
    }
    
  })
  
  ################## Indirect measure ################## 
  
  perceived_control1 <- data.frame()
  
  
  #Create function simulate board states
  board_sim <- function(trial){
    exploration_rate <- runif(1,min=0.3,max=0.95)
    sample(c(sample(c(1,0),trial,replace = TRUE,prob = c(exploration_rate,(1-exploration_rate))),rep.int(0,120-trial))) 
  }
  
  trial <- 22                                   #non reactive for usage in formulas
  trialSHOW <- reactiveVal(value = trial)       #reactive for display                    
  
  rvals <- reactiveValues(
    column = sample(1:12,1),
    row = sample(1:10,1),
    board = matrix(board_sim(trial), nrow = 10, ncol = 12),       #Create the reactive grid
    test_count = 1                                               #keep track of how many test we have already given
  )
  
  #Plot Title
  output$plot_title <- renderText({paste0("This was the state of the board of a previous participant after ", trialSHOW() , " selections. The next selection the participant made is indicated by the symbol X.")})
  
  #Render the Grid
  output$BoardPlot <- renderPlot({
    plot(rvals$board, key = NULL, col = c("light grey", "dark grey"), main = "",
         xlab = "", ylab = "", axis.col = NULL, axis.row = NULL)
    text(x = rvals$column, y = 11 - rvals$row, labels = "X")
  })
  
  ### Go to Next Page ###     
  observeEvent(input$Next2,{
    
    #check if probabilities add up
    if(sum(input$zero,input$plus_10, input$minus_1, input$plus_11) == 100){
      
      perceived_control1 <<- rbind(perceived_control1,
                                   c(id,isolate(input$zero), isolate(input$plus_10), isolate(input$plus_11), 
                                     isolate(input$minus_1),gsub(", ","", toString(as.vector(isolate(rvals$board)))),  #BOARD ID
                                     toString(c(isolate(rvals$column),isolate(rvals$row)))
                                   ) 
      )
      names(perceived_control1) <<- c("ID","0guess","+10guess","+11guess","-1guess","board ID","Xkey")
      
      #add 1 to the page count
      rvals$test_count <- rvals$test_count + 1
      
      #do things depending on which page we are on
      if(rvals$test_count == 5){
        trial <<- 31
        trialSHOW(trial)
        rvals$board <- matrix(board_sim(trial), nrow = 10, ncol = 12)   #New Grid
      }
      if(rvals$test_count == 9){
        trial <<- 75
        trialSHOW(trial)
        rvals$board <- matrix(board_sim(trial), nrow = 10, ncol = 12)   #New Grid
      }
      if(rvals$test_count == 13){
        trial <<- 88
        trialSHOW(trial)
        rvals$board <- matrix(board_sim(trial), nrow = 10, ncol = 12)   #New Grid
      }
      if(rvals$test_count == 17){
        dataHOLDER <<- cbind(perceived_control1,as.numeric(difftime(Sys.time(),start_indirect1,units = "mins")))
        names(dataHOLDER) <<- c("ID","0guess","+10guess","+11guess","-1guess","board ID","Xkey","TimeTaken")                                                  
        write.csv(dataHOLDER,file=paste0("perceived_control1",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
        #Go to next page
        { 
          start_indirect2 <<- Sys.time()
          shinyjs::hide("Indirect")
          shinyjs::show("Indirect2")
        }
      }
      if(sum(rvals$test_count == c(5,9,13)) == 0){
        trial <<- trial +1
        trialSHOW(trial)
      }
      
      # if the x is on a unexplored spot make it explored
      if(rvals$board[rvals$row,rvals$column] == 0){
        rvals$board[rvals$row,rvals$column] <- 1
      }
      
      # sample a new X spot
      rvals$column <- sample(1:12,1)
      rvals$row <- sample(1:10,1)
      
      #reset inputs
      updateSliderInput(session,"zero",value = 0)
      updateSliderInput(session,"plus_10",value = 0)
      updateSliderInput(session,"minus_1",value = 0)
      updateSliderInput(session,"plus_11",value = 0)
      
      output$error <- renderText({""})
      
    } else {
      output$error <- renderText({"percentages must add up to 100"})
    }
  })
  
  
  ################### indirect 2 ################## 
  test_count <- 1
  perceived_control2 <- data.frame()
  
  #Create function simulate board states #THIS ONE IS DOUBLE
  board_sim <- function(trial){
    exploration_rate <- runif(1,min=0.3,max=0.95)
    sample(c(sample(c(1,0),trial,replace = TRUE,prob = c(exploration_rate,(1-exploration_rate))),rep.int(0,120-trial))) 
  }
  
  trial_tot <- sample(20:85,1)                                  #non reactive for usage in formulas
  trialSHOW2 <- reactiveVal(value = trial_tot)                  #reactive for display                    
  
  rvals_tot <- reactiveValues(
    board2 = matrix(board_sim(trial_tot), nrow = 10, ncol = 12)       #Create the reactive grid
  )
  
  #Plot Title
  output$plotTitle <- renderText({paste0("This was the state of the board of a previous participant after ", trialSHOW2(), " selections")})
  
  #Render the Grid
  output$BoardPlot_tot <- renderPlot({
    plot(rvals_tot$board2, key = NULL, col = c("light grey", "dark grey"), main = "",
         xlab = "", ylab = "", axis.col = NULL, axis.row = NULL)
  })
  
  ### Go to Next Page ###     
  observeEvent(input$NextPage,{
    
    perceived_control2 <<- rbind(perceived_control2, c(id,isolate(input$total_score_guess),gsub(", ","", toString(as.vector(isolate(rvals_tot$board2))))))
    
    names(perceived_control2) <<- c("ID","TotalScoreGuess","board ID")
    
    #add 1 to the page count
    test_count <<- test_count + 1
    
    #new trial
    trial_tot <<- sample(20:85,1)
    trialSHOW2(trial_tot)
    rvals_tot$board2 <- matrix(board_sim(trial_tot), nrow = 10, ncol = 12)   #New Grid
    
    if(test_count == 17){
      dataHOLDER2 <<- cbind(perceived_control2,as.numeric(difftime(Sys.time(),start_indirect2,units = "mins")))
      names(dataHOLDER2) <<- c("ID","TotalScoreGuess","board ID","TimeTaken")
      write.csv(dataHOLDER2,file=paste0("perceived_control2",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
      {
        shinyjs::hide("Indirect2")
        shinyjs::show("Direct")
      }
    }
    
    #reset input
    updateNumericInput(session,"total_score_guess",value = 0)    
    
  }
  )
  
  #For the direct measure of perceived control
  sliderDirect<- reactive({
    data.frame(
      ID = id,
      DirpC1 = as.character(input$DirPC1),
      DirpC2 = as.character(input$DirPC2),
      DirpC3 = as.character(input$DirPC3),
      DirpC4 = as.character(input$DirPC4),
      stringsAsFactors = FALSE
    )})
  
  observeEvent(input$Next3,{
    write.csv(sliderDirect(),file=paste0("DirectPC",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
    {
      shinyjs::hide("Direct")
      shinyjs::show("covariate1")
    }
  })     
  
  #For locus of control
  sliderLOC<- reactive({
    data.frame(
      ID = id,
      LcQ1 = as.character(input$Lcq1),
      LcQ2 = as.character(input$Lcq2),
      LcQ3 = as.character(input$Lcq3),
      LcQ4 = as.character(input$Lcq4),
      LcQ5 = as.character(input$Lcq5),
      LcQ6 = as.character(input$Lcq6),
      LcQ7 = as.character(input$Lcq7),
      stringsAsFactors = FALSE
    )})
  
  observeEvent(input$Next4,{
    write.csv(sliderLOC(),file=paste0("LCQ",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
  })
  
  #For motivation
  slidermotivation<- reactive({
    data.frame(
      ID = id,
      Cq1 = as.character(input$Cq1),
      Cq2 = as.character(input$Cq2),
      Cq3 = as.character(input$Cq3),
      Cq4 = as.character(input$Cq4),
      Cq5 = as.character(input$Cq5),
      Cq6 = as.character(input$Cq6),
      Cq7 = as.character(input$Cq7),
      Cq8 = as.character(input$Cq8),
      Cq9 = as.character(input$Cq9),
      Cq10 = as.character(input$Cq10),
      stringsAsFactors = FALSE
    )})
  
  observeEvent(input$Next5,{
    write.csv(slidermotivation(),file=paste0("MQ",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
  })
  
  #Questionnaires (covariate)
  
  observeEvent(input$Next4,{
    shinyjs::hide("covariate1")
    shinyjs::show("covariate2")
  })
  observeEvent(input$Next5,{
    shinyjs::hide("covariate2")
    shinyjs::show("debrief")
  })
  
  #debrief & finish the experiment
  observeEvent(input$Finish,{
    write.csv(pemail(),file=paste0("Voucher_Info",gsub(':','-',id,fixed=TRUE),'.csv'),row.names=FALSE)
    shinyjs::hide("debrief")
    session$sendCustomMessage("mymessage", "mymessage")
  })
}

shinyApp(ui=ui, server=server)
