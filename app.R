
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(reactable)
library(shinyjs)
library(htmltools)
library(reactablefmtr)
library(plotly)
library(htmlwidgets)
library(waiter)
library(ggimage)

source("./speed_it_up.R")

# Define UI for application that draws a histogram

ui <- dashboardPage(
  

  header = dashboardHeader(title = '', titleWidth = 230), # title = 'MLB Hitters'), ## top left title
  sidebar = dashboardSidebar(collapsed = TRUE, minified = TRUE, 
                             sidebarMenu(
                               menuItem('Pitcher Dashboard', icon = icon('baseball-ball'), tabName = 'pitcher_tool'),
                               menuItem('Team Leaderboards', icon = icon('baseball-ball'), tabName = 'leaderboard'),
                               menuItem('User lists', icon = icon('baseball-ball'), tabName = 'UserCheckdown')
                               
                               
                             ) # sidebar menu
  ),
  body = dashboardBody(

    # placing of tags
    tags$style(HTML("
                        .box.box-solid.box-danger>.box-header {
                        background:#D50032
                        }
                        box.box-solid.box-success>.box-header {
                        background:#028A0F
                        }
                        .box.box-solid.box-orange>.box-header {
                        border:#F58216;
                        background:#F58216;
                        }
                        .bar-cell {
                          display: flex;
                          align-items: center;
                        }
                        .number {
                          font-size: 13.5px;
                          white-space: pre;
                        }
                        .bar-chart {
                          flex-grow: 1;
                          margin-left: 6px;
                          height: 22px;
                        }
                        .bar {
                          height: 100%;
                        }
                                      
                        /* logo */
                        .skin-blue .main-header .logo {
                        background-color: #990c1a;
                        }

                        /* logo when hovered */
                        .skin-blue .main-header .logo:hover {
                        background-color: #990c1a;
                        }

                      /* navbar (rest of the header) */
                        .skin-blue .main-header .navbar {
                        background-color: #990c1a;
                        }        

                        /* main sidebar */
                        .skin-blue .main-sidebar {
                        background-color: #990c1a;
                        }
                      
                        /* main sidebar collapsed */
                        .skin-blue .main-sidebar-collapse {
                        width: 200px;
                        background-color: #990c1a;
                        }
                        .tag {
                          display: inline-block;
                          padding: 0.80rem 1.35rem;
                          border-radius: 50px;
                          font-weight: 700;
                          font-size: 1.10rem;
                        }
                        
                        .status-high {
                          background: hsl(0, 100%, 50%);
                          color: hsl(50, 0%, 100%);
                        }
                        
                        .status-med {
                          background: hsl(50, 7%, 83%);
                          color: hsl(230, 45%, 30%);
                        }
                        
                        .status-low {
                          background: hsl(240, 100%, 50%);
                          color:  hsl(50, 0%, 100%);
                        }
                                        "  )),
    tabItems(
      tabItem(tabName = 'pitcher_tool',

              useWaiter(),
              useWaitress(),

              fluidRow(
                bsModal(
                  id = 'infoModal',
                  title = h1(strong('Welcome to Slambio!'), align = 'center'),
                  trigger = 'infomodalbutton', size = 'large',

                  HTML("<h3>
                       <b>Slambio!</b>
                       <br>
                       <h3> We are excited to have you here with us.</h3>
                       <h4><em> Okay, so what is Slambio?</em><h4>
                       <h4> Slambio is a pitching tool software that goes into heavy depth to better evaluate pitchers all throughout the MLB and MiLB </h4>
                       <h4> You may be asking yourself... okay, but,<em> what does Slambio even mean?</em></h4>
                       <h4><b> Funny Story...</b> The name Slambio comes from Jose Leclerc... ho called his changeup a 'Slambio'..
                       The funny part about it, is that his changeup was not even a changeup, it was just a regular slider. Since he called it a different name,
                       the people who would tag the pitch didn't understand what he was trying to say... so they tagged the pitch as a Splitter...</h4>
                       <h4>Jose, you monster</h4>
                       <h4>So now, we have a regular slider being referred to as a changeup but tagged a Splitter and was named a Slambio...but either way, Welcome!</h4>")
                )
              ),
          column(4,actionButton("infomodalbutton", "How To Navigate Dashboard?")),
          column(4, h1(div("SLAMBIO",
                  style = "font-size: 50px; color: #990c1a; text-shadow: 3px 0px 3px #F2C83F; font-weight: bolder"), align = 'center')
         ),
          column(2, offset = 2, align = 'right',
                 img(src = "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg",
                     width = 150, height = 80)
          ),
          hr(),
          column(12, h2(div(HTML('<font size=5><b><em>Player Information</em></b></font>')), align = 'center')),
          fluidRow(
            column(2, align = 'center',
                   dropdownButton(
                     inputId = 'dropdown2',
                     label = 'Panel Info',
                     icon = icon('baseball-ball'),
                     circle = FALSE,
                     width = "600px",
                     HTML("
                    <h4><b>There are three sections in this panel.</b></h4>
                    <br>
                    <h5> If the pitcher is a top prospect, you can find their ranking on the left side of the panel.</h5>
                    <ul><li><h5>PL: MLB Pipeline </h5></li>
                     <li><h5>FG: Fangraphs</h5></li>
                     <li><h5>BP: Baseball Prospectus</h5></li>
                     <li><h5>BA: Baseball America</h5></li></ul>
                    <br>
                   <h5>You will find the player's information, including their age, throwing arm, height and weight towards the middle</h5>
                    <h5> If the pitcher is in the MLB, you will find his accumulated WAR for the 2022 season.</h5>
                    <h5> Below his accumulated WAR, you will find the pitcher's 20th, 50th, and 80th percentile projetions, and whether he has surpassed them, he is on pace for them, or he is not on pace for them.
                   A  <FONT COLOR=\"GREEN\"><b> green </b></FONT> checkmark means that the pitcher has surpassed/is on pace for his xth percentile projection, while a  <FONT COLOR=\"RED\"><b>red </b></FONT>\"X\" means he is not on pace for it.</h5>
                     ")
                   ) ## dropdown
            ),

          column(4, offset = 2, align = 'center',
                 selectizeInput(
                   'Pitcher',
                   'Select a Player',
                   choices =  c('Type a Player' = '', choices_pop()),
                   selected = "Gray, Jon (TEX)",
                   multiple = FALSE,
                   options = list(placeholder = "Type a Player")
                 )
              )
          ),
          box(width = 12, solidHeader = TRUE, collapsible = TRUE,

            fluidRow(
              column(4, align = 'center',
                     uiOutput("rankings"),
                     fluidRow(
                      column(12,
                       prettyRadioButtons(
                                        inputId = "interest",
                                        label = "Level of Interest:",
                                        icon = icon("baseball-ball"),
                                        choices = c("No Interest", "Low Interest", "Some Interest", "High Interest"),
                                        selected = character(0),
                                        animation = "tada",
                                        inline = TRUE,
                                        status = "info"
                                    )
                       ) #col
                     ), #row
                     fluidRow(
                       column(6,
                        selectInput("Username", 
                                    "Name of User:", 
                                    choices = c(" ", "User 1", "User 2", "User 3"),
                                    selected = NULL
                                    
                                    )
                              ),
                       bsModal(
                         id = 'NotesPit',
                         title = h1(strong('Thoughts on this Player?'), align = 'center'), 
                         trigger = 'infomodalbutton_4', size = 'medium',
                         textInput("player_notes",
                                   label = "") 
                       ),
                       column(6, align = 'center',
                              actionBttn(
                                inputId = "infomodalbutton_4",
                                label = "Press to Take Notes",
                                style = "bordered", 
                                color = "primary",
                                icon = icon("baseball-ball"),
                                size = "xs"
                              ),
                              h4("         \n            "),
                              actionBttn(
                                inputId = "Save_info",
                                label = "Submit",
                                style = "unite", 
                                color = "primary"
                              )
                          )
                       # col
                     ) # row
                    ), #col
               column(4, align = 'center',
                     uiOutput("bio")
                     ), ##col
               column(4, align = 'center',
                      uiOutput("TeamnPercs")
                      ) # col
                  ) ## row
              ), ## box
          fluidRow(
            column(2,
                   dropdownButton(
                     inputId = 'dropdown2',
                     label = 'Panel Info',
                     icon = icon('baseball-ball'),
                     circle = FALSE,
                     width = "600px",
                     HTML("
                    <h4><b>There are three sections in this panel.</b></h4>
                    <br>
                    <h5>After choosing your player, you can now pick what stats you want to see from that player. These stats are broken down into
                    year, team, and level. If you want to check out a player's complete stats for the whole year, select 'All' under Team.</h5>
                    <br>
                   <h5>The first box displays player's overall performance. </h5>
                    <ul><li><h5>G: Games </h5></li>
                     <li><h5>GS: Games Started</h5></li>
                     <li><h5>IP: Innings Pitched</h5></li>
                     <li><h5>wOBA: weighted On-Base Average</h5></li>
                     <li><h5>FIP: Fielding Independent Pitching (ERA-scale)</h5></li></ul>
                    <h5> The second box displays a pitcher's overall contact rates against.</h5>
                   <ul><li><h5>BABIP: Baating Average on Balls in Play </h5></li>
                     <li><h5>LD%: Linedrive Rate</h5></li>
                     <li><h5>GB%: Groundball rate</h5></li>
                     <li><h5>FB%: Flyball Rate</h5></li>
                     <li><h5>HR/FB: Homerun per Flyball</h5></li>
                     <li><h5>Nitro%: Nitro Rate</h5></li>
                     <li><h5>EV70: 70th Percentile Exit Velocity</h5></li></ul>
                    <h5> The third box displays a pitcher's overall results.</h5>
                     <ul><li><h5>ERA: Earned un Average </h5></li>
                     <li><h5>BB%: Walk Rate</h5></li>
                     <li><h5>SO%: Strikeout Rate</h5></li>
                     <li><h5>SWM%: Swing and Miss Rate</h5></li>
                     <li><h5>CH%: Chase Rate</h5></li></ul>
                    <h5> The percentiles are measured based on the year and the player's current level.</h5>
                     ")
                   ) ## dropdown
                ),
            column(6,offset = 1, h2(div(HTML('<font size=5><b><em>Pitcher Results</em></b></font>'))), align = 'center'),
          ),
            fluidRow(
            box(width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                fluidRow(
                column(2, offset = 3, selectInput("player_year",
                                      label = "Year:",
                                      choices = NULL)
                        ),
                column(2, selectInput("player_team",
                                      label = "Team:",
                                      choices = NULL)
                       ),
                column(2, selectInput("player_lvl",
                                      label = "Level",
                                      choices = NULL)
                       )
                ),
                br(),
                fluidRow(
                column(4, align = 'center',
                       uiOutput("performace_box")
                ),#col
                column(4, align = 'center',
                       uiOutput("contact_box")
                ),#col
                column(4, align = 'center',
                       uiOutput("results_box")
                )#col
              )# row
             ) #box
            ), #row
          fluidRow(
            column(2,
                   dropdownButton(
                     inputId = 'dropdown3',
                     label = 'Panel Info',
                     icon = icon('baseball-ball'),
                     circle = FALSE,
                     width = "600px",
                     HTML("
                    <h4><b>There are four sections in this panel.</b></h4>
                    <br>
                    <h5> In this panel you will learn more about the player's overall game, his arsenal, and what makes him different from the rest</h5>
                    <h5><em> Graphs:</em></h5>
                    <ul><li><h5>Pitch Usage: In this Donut chart, you will discover the rates at which each pitchers throws their arsenal. </h5></li>
                     <li><h5>Fastball Rise: In this graph, you will a regression between the Induced Vertical Break of a pitchers Fastball, and the height it was released.
                    This graph helps us get a quick undertanding on whether this pitcher can create rise behind his FB. Being aboce the line  is good!</h5></li>
                     <li><h5>Location Value Added: This plot will tell us whether that pitcher has good command behind each of his pitches. Note: Cutters and Splitters are still being worked on in the Model.</h5></li>
                     <li><h5>Pitch Breakdown: Based on the selected year and level, tables will populate with the pitcher's stats based on each pitch. If you click on the pitch,
                    you will find the different subtypes and discover what really separates him from the rest.</h5></li></ul>
                    <br>
                     ")
                   ) ## dropdown
            ), #col
            column(6,offset = 1, h2(div(HTML('<font size=5><b><em>Pitcher Arsenal</em></b></font>'))), align = 'center'),
             ),#row
          box(
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            fluidRow(
                column(2, offset = 3, selectInput("player_year_two",
                                                  label = "Year:",
                                                  choices = NULL)
                      ),
                column(2, offset = 1, selectInput("player_lvl_two",
                                                  label = "Level:",
                                                  choices = NULL)
                ),
            ),# row
            br(),

            fluidRow(
              column(12, uiOutput("charts_graphs")),
                    ), # row
            hr(),
            fluidRow(
              column(12, h2(
                            div(
                              HTML('<font size=5><b><em>Pitch Breakdown</em></b></font>')
                               )
                            ), align = 'center')
            ),
           fluidRow(
             column(11, offset = 1, h2(
               div(
                 HTML('<font size=3><b><em>2022</em></b></font>')
               )
             ), align = 'left') #col
             ), #row
           fluidRow(
              column(12, align = 'center',
                     reactableOutput('subtype_table_22')
                     )#col
              ), #row
           fluidRow(
             column(11, offset = 1, h2(
               div(
                 HTML('<font size=3><b><em>2021</em></b></font>')
                 )
               ), align = 'left') # col
             ), # row
             fluidRow(
               column(12, align = 'center',
                      reactableOutput('subtype_table_21')
               ) #col
             ) #row
           ), #box
         fluidRow(
           column(2,
                  dropdownButton(
                    inputId = 'dropdown3',
                    label = 'Panel Info',
                    icon = icon('baseball-ball'),
                    circle = FALSE,
                    width = "600px",
                    HTML("
                    <h4><b>There are several parts to this panel.</b></h4>
                    <br>
                    <h5> On the left side, you can see the pitcher's Vertical Approach Angle on FB up and SI down. Rule of thumb: closer to 0 for FB, more negative for SI</h5>
                    <h5><Strikezones:</h5>
                    <ul><li><h5>Pitch Usage: this heatmap will give you a good idea on where this pitcher threw a specific pitchs the most.</h5></li>
                    <li><h5>Stats per Zone: This graph will provide different statistics</h5></li></ul>
                    <br>
                   <h5>You will find the player's information, including theur age, throwing arm, height and weight towards the middle</h5>
                    <h5> If the pitcher is in the MLB, you will find his accumulated WAR in the 2022 season.</h5>
                    <h5> Below his accumulated WAR, you will find the pitcher's 20th, 50th, and 80th percentile projetions, and whether he has achieved them or not.
                   A  <FONT COLOR=\"GREEN\"><b> green </b></FONT> checkmark means that the pitcher has surpassed his xth percentile projection, while a  <FONT COLOR=\"RED\"><b>red </b></FONT>\"X\" means he has not.</h5>
                     ")
                  ) ## dropdown
           ), #col
           column(6, offset =1, h2(div(HTML('<font size=5><b><em>Pitch Location</em></b></font>'))), align = 'center')
          ), #row
         fluidRow(
           box(width = 12,
               collapsible = TRUE,
               solidHeader = TRUE,
               fluidRow(
                 column(2, offset = 3, align = 'center', selectInput(
                                                         "year3",
                                                         label = "Select a Year",
                                                         choices = c(2021,2022),
                                                         selected = 2022)
                        ),
                 column(2, align = 'center', selectInput(
                                                "lvl3",
                                                label = "Select a Level",
                                                choices = NULL)
                       )
                ),
               fluidRow(
               column(3, align = 'center',
                      uiOutput("vaa_fb_up"),
                      uiOutput('vaa_si_dw')
                      ),
               column(9, align = 'center',

                      fluidRow(
                        column(2, offset = 1, align = 'center', selectInput(
                        "Pitch_Type",
                        label = "Select a Pitch",
                        choices = NULL
                          )),
                      column(2, align = 'center', selectInput(
                        "metric",
                        label = "Select a Metric",
                        choices = c("SWM%", "CH%", "Str%", "Nitro%", "GB%", "PG")
                      ))
                      ),
                      fluidRow(
                        #column(5, plotOutput("KZone_usage")),
                        column(5, plotOutput('Kzone'))

                         )

                       )
                    )#row
                 )#box
             )#row
        ), #tab

        tabItem(tabName = 'leaderboard',
                useWaiter(),
                fluidRow(
                  bsModal(
                    id = 'infoModal2',
                    title = h1(strong('Navigating our Leaderboards:'), align = 'center'),
                    trigger = 'infomodalbutton_2', size = 'large',
                    HTML("
                       <h4> In this page you will be able to scroll through every pitcher in professional baseball </h3>
                       <h4> There are <em>two</em> tables<h3>
                       <h4> The first table consists of simple statistics (SWM%, CH%, K%, BB%), and you can filter through the year
                            the organization, the level of competetition, MNFA season, and whether they are in the 40 an roster or not. </h4>
                       <h4> The last two filters <b>only</b> work for the first table.<h4>
                       <h4> The second table describes specific pitch types for every pitcher</h4>
                       <h4> If you want to check out who throw the fastest fastball, or who gets the most spin on his slider, or
                            who gets the most vertical break on his curveball, and so on, use this table as well as the pitch buttons</h4>
                       <h4><b> Quick Note: <em> Be patient</em> </b>, a lot of information to be display, so it takes some time</h4>
                       ")
                  ),

                column(4,actionButton("infomodalbutton_2", "How To Navigate Leaderboards?")),
                column(4, h1(div("SLAMBIO",
                                 style = "font-size: 50px; color: #990c1a; text-shadow: 3px 0px 3px #F2C83F; font-weight: bolder"), align = 'center')
                ),
                column(2, offset = 2, align = 'right',
                       img(src = "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg",
                           width = 150, height = 80)
                )
              ),
              br(),
              fluidRow(
                  column(12, align = 'center', uiOutput("Team_2"))
                ),
              br(),
              fluidRow(
                  column(12,  align = 'center',
                                         selectInput("team_lead",
                                         label = "Select an Organization:",
                                         choices = c("All", unique(data$TEAM)),
                                         selected = "TEX")
                         )
              ),
              fluidRow
              (
                column(8, offset = 2, align = 'center', uiOutput("box_filters"))
              ), #row
              fluidRow(
              column(12, align = 'center', reactableOutput("main_table"))
              ),
              hr(),
              fluidRow(
                column(12, align= 'center',prettyRadioButtons(
                  inputId = "pitch_type_table",
                  label = "Select a Pitch:",
                  thick = TRUE,
                  choices = c("Fastball", "Slider", "Curveball",
                              "Changeup", "Cutter", "Sinker",
                              "Splitter"),
                  selected = "Fastball",
                  animation = "pulse",
                  status = "info",
                  inline = TRUE
                 )
                )
              ),
              fluidRow(
                column(12, align = 'center', reactableOutput("pitch_table"))
              )
      ), # tab
      tabItem(tabName = "UserCheckdown",
              fluidRow(
                bsModal(
                  id = 'infoModal5',
                  title = h1(strong('Navigating your Input:'), align = 'center'),
                  trigger = 'infomodalbutton_3', size = 'large',

                  HTML("
                       <h4> In this section, you can see who are some other targets by other Ranger employees</h4>
                       <h4> You have the option to select a team, as well as an evaluator (in case you want to check out
                            which players are high on someone else's list). </h4>
                       <h4> This table will provide  the specific time in which each evalation was made, who evaluated this player,
                       the player's current organization, the name of the player, and the level of interest</h4>
                       <h4><em> You can dig further by clicking on a player's row. A writen evaluation will come up. </em></h4>
                       <h5> We suggest keeping these evaluations short and concise</h5>
                       ")
                ),
                column(4,actionButton("infomodalbutton_3", "How To Navigate Inputs?")
                       ), #col
                column(4, h1(div("SLAMBIO",
                                 style = "font-size: 50px; color: #990c1a; text-shadow: 3px 0px 3px #F2C83F; font-weight: bolder"),
                             align = 'center')
                ), #col
                column(2, offset = 2, align = 'right',
                       img(src = "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg",
                           width = 150, height = 80)
                ) # col
              ), #row
              br(),
              fluidRow(
                    column(12, align = 'center', uiOutput("Team_3"))
                      ), #row
              br(),
              fluidRow(
                    column(12,  align = 'center',
                       selectInput("team_inp",
                                   label = "Select an Organization:",
                                   choices = c("All", unique(data$TEAM)),
                                   selected = "All")
                          ) #col
                      ), #row
              fluidRow(
                column(12,  align = 'center',
                       selectInput("users",
                                   label = "Select a User:",
                                   choices = c("All", "User 1", "User 2", "User 3"),
                                   selected = "All")
                ) #col
              ), #row
            fluidRow(
            column(2,
                   dropdownButton(
                     inputId = 'dropdown9',
                     label = 'Panel Info',
                     icon = icon('baseball-ball'),
                     circle = FALSE,
                     width = "600px",
                     HTML("
                    <h4>On the right side, you will see a interest ranking. This ranking will be displayed based on the organization -- or not-- included.</h4>
                    <h4> The following is the point system for this ranking </h4>
                    <ul><li><h4><FONT COLOR=\"RED\">High Interest: 3 Points</FONT></h4></li>
                    <li><h4>Some interest: 2 Points</h4></li>
                    <li><h4><FONT COLOR=\"BLUE\">Low interest: 1 Point</FONT></h4></li></ul>
                    <h4> There can only be one vote per user. If the user votes twice for the same player, the software will acquire and use the user's most recent vote </h4>
                    <h4><em> If the user decides that he is no longer interested in a player, please proceed to his Slambio main page and submit 'No Interest'.</em></h4>
                     ")
                   ) ## dropdown
              )
            ),
            fluidRow(
              
              column(12, align = 'center',
                     prettyRadioButtons(
                       inputId = "int_lvl",
                       label = "Level of Interest",
                       choices = c("NA", "High Interest", "Some Interest", "Low Interest"),
                       selected = "NA",
                       shape = "round",
                       status = "danger",
                       fill = TRUE,
                       inline = TRUE
                     ))
            ),
            hr(),
            fluidRow(

                      
              column(12,
                     uiOutput("most_liked_boxes"))
            )
          )# tab
       )#tabs
    )#body
)#page

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
####################### PLAYER INTERESTS USER INPUT #########################################
  observeEvent(input$Save_info, {
    req(input$Pitcher)
    
    `%notin%` <- Negate(`%in%`)
    
    flag = ifelse(is_empty(input$interest) , TRUE, FALSE)
    flag_u = ifelse(input$Username == " ", TRUE, FALSE)
    

    if(flag_u == TRUE | flag == TRUE)
    {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "There was an error.\n Check that all fields have been filled.",
        type = "error"
      )
    }else{
      sendSweetAlert(
      session = session,
      title = "Success!",
      text = "Your submission has been saved.\n Please go to the third page to confirm your player's interest.",
      type = "success"
    )
    }
    
  })
  
  output$most_liked_boxes <- renderUI({
    
    team <- input$team_inp
    
    hexcol <-ifelse(team == "All", "#002D72" , teams$Color[teams$Abbrev == team])
    
    tags$div(
      class = "another_one", id = "likes",
      box(width = 12, 
          solidHeader = TRUE,
          title = "",
          status = "primary",
          fluidRow(
          column(8,reactableOutput("players_liked"), align = 'center'),
          column(4, plotOutput("highest_interest"), align = 'center')
          )
      ), 
      tags$style(HTML(paste0("
                        #likes .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        "))
                 )   
          )
    
  })
  
  output$highest_interest <- renderPlot({
    
    data_int <- sample_data()
    team  = input$team_inp

    if(team == "All")
    {
      data_int %>% 
        mutate(rank = ifelse(Interest == "High Interest", 3,
                             ifelse(Interest == "Some Interest", 2, 1))) %>%
        select(Player, rank, Team) %>%
          group_by(Player) %>%
            mutate(tot = sum(rank)) %>%
              ungroup() %>%
                select(Player, Team, tot) %>%
                  distinct() %>% 
                    arrange(desc(tot)) %>%
                      slice(1:5) -> use
      
      data_int %>% 
        mutate(rank = ifelse(Interest == "High Interest", 3,
                             ifelse(Interest == "Some Interest", 2, 1))) %>%
          select(UserName, Player, rank, Team) %>%
            group_by(Player) %>%
              mutate(tot = sum(rank)) %>%
                ungroup() %>%
                  arrange(desc(tot), desc(rank)) %>%
                    filter(Player %in% use$Player) -> plot_chart
                    
    }else{
    
    data_int %>% 
      filter(Team == team) %>%
        mutate(rank = ifelse(Interest == "High Interest", 3,
                             ifelse(Interest == "Some Interest", 2, 1))) %>%
            select(Player, rank, Team) %>%
              group_by(Player) %>%
               mutate(tot = sum(rank)) %>%
                ungroup() %>%
                  select(Player, tot, Team) %>%
                    distinct() %>%
                      arrange(desc(tot)) %>%
                        slice(1:5) -> use
      
      data_int %>% 
        filter(Team == team) %>%
          mutate(rank = ifelse(Interest == "High Interest", 3,
                             ifelse(Interest == "Some Interest", 2, 1))) %>%
           select(UserName, Player, rank, Team) %>%
            group_by(Player) %>%
              mutate(tot = sum(rank)) %>%
                ungroup() %>%
                  arrange(desc(tot), desc(rank)) %>%
                    filter(Player %in% use$Player) -> plot_chart
      
      
    }
    if(dim(use)[1] == 0)
    {
      return(NULL)
    }
    
    face_df <- data.frame(
      Player = use$Player
    )  
    
    
    IDs = c(
      as.numeric(data$ID[data$TEAM == use$Team[1] & data$NAME == use$Player[1]][1]),
      as.numeric(data$ID[data$TEAM == use$Team[2] & data$NAME == use$Player[2]][1]),
      as.numeric(data$ID[data$TEAM == use$Team[3] & data$NAME == use$Player[3]][1]),
      as.numeric(data$ID[data$TEAM == use$Team[4] & data$NAME == use$Player[4]][1]),
      as.numeric(data$ID[data$TEAM == use$Team[5] & data$NAME == use$Player[5]][1])
    )
    
    IDs <- IDs[!is.na(IDs)]
    
   face_df <- get_facedf(face_df, IDs, team) 
   
   plot_chart %>% left_join(face_df) -> plot_chart
   plot_chart$Player =  sub("(\\w+),\\s(\\w+)","\\2 \\1", plot_chart$Player)
   
    ggplot(plot_chart, aes(x = reorder(Player, -tot), y = rank)) + 
     geom_bar(aes( fill = Team), stat = "identity", show.legend = FALSE) +
     geom_image( mapping =  aes(x = Player, y = tot-1,image = Link), size = .13, byh = "height") + 
     scale_fill_manual(values = teams$Color[teams$Abbrev %in% plot_chart$Team] ) +  
     ggtitle("Top 5 Interest") +
     ylab("Interest Points") +
     theme(
       panel.background = element_rect(fill = "#FFFFFF"),
       axis.ticks.x = element_blank(),
       axis.text.x  = element_text(angle  = 0, face = "bold"), 
       axis.title.x = element_blank()) 
   
  })
  
  observeEvent(input$delete_int, {
    rows <- getReactableState("players_liked", "selected")
    req(rows)

    data <- sample_data()
    
    data = data[-rows,]
    
    
  })
  
  # Acquire the data that we have plus the one recently added
  sample_data <- eventReactive(input$Save_info,{
    
    req(input$Username)
    req(input$interest)
  
    data <- read.csv("Player_interests.csv")
    
    # get date
    today = as.character(Sys.time()) 
    
    #get user
    user <- input$Username
    
    #get team
    pitcher <- input$Pitcher
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1) 

    #get player
    player <-  substr(pitcher, 0, str_length(pitcher)-6) #player name
    
    #get interest
    interest <- input$interest
    notes <- input$player_notes
    
    vec <- data.frame(
      Date = today,
      UserName = user,
      Team = team,
      Player = player,
      Interest = interest,
      Notes = notes
      )

    data <- rbind(data, vec)
    
    data %>%
      group_by(Player, UserName) %>%
        filter(Date == max(Date)) %>%
          ungroup() %>% 
            filter(Interest != "No Interest") -> data
    
    write.csv(data, "Player_interests.csv", row.names = FALSE)
    data
  })
  

  observeEvent(c(input$Pitcher),{
    
    updateTextInput(session, 
                    "player_notes",
                    label = NULL,
                    value = "")
  }, ignoreNULL = FALSE)
  
  output$players_liked <- renderReactable({

      req(input$team_inp)
      req(input$users)
      req(input$int_lvl)
      
      use <- sample_data()
      team <- input$team_inp
      user <- input$users
      inte <- input$int_lvl
      
      hexcol <- ifelse(team == "All", "#002D72",teams$Color[teams$Abbrev == team])
      
      if(team == "All")
      {
        if(user == "All")
        {
          final <- use
        }else{
          final <- use %>% filter(UserName == user)
        }
      }else
      {
        temp <- use %>% filter(Team == team)
        if(user == "All")
        {
          final <- temp
        }else
        {
          final <- temp %>% filter(UserName == user)
        }
      }
      
      if(inte != "NA")
      {
        final %>% filter(Interest == inte) -> final
      }
      
      final %>%  arrange(desc(Date)) -> final
      notes <- final

    
        final %>% 
          select(Date, UserName, Team, Player, Interest) %>%
              reactable(
                pageSizeOptions = 7,
                columns = list(
                  Date = colDef(name = "Time of Submission"),
                  UserName = colDef(name = "Name of User"),
                  Team = colDef(name = "Team",
                                cell = function(value) {
                                  num <- teams$number[teams$Abbrev == value]
                                  link <- paste("https://www.mlbstatic.com/team-logos/team-cap-on-light/", num, ".svg", sep ="")
                                  image <- img(src = link, style = "height: 24px;", alt = "")
                                  tagList(
                                    div(style = "display: inline-block; width: 45px;", image), value)
                                }),
                  Player = colDef(name = "Pitcher"),
                  Interest = colDef(name = "Level of Interest",
                                    cell = function(value) {
                                    temp <- ifelse(value == "High Interest", "high",
                                                   ifelse(value == "Some Interest", "med", "low"))
                                    class <- paste0("tag status-", tolower(value))
                                    div(class = class, value)
                                    })
                                   ),
                
                details = function(index)
                {
                  notes %>% 
                    filter(UserName == final[index,]$UserName) %>%
                      filter(Player == final[index,]$Player) %>%
                        pull(Notes) -> notes
                  
                  name <- sub("(\\w+),\\s(\\w+)","\\2 \\1", final[index,]$Player)
                  
                as.character(div("Details for <b>row %s</b>")) 
                        
                paste0(final[index,]$UserName, "'s Thoughts on ", name, ": ",  notes, sep = "")
                
                },
                defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                style = list(zIndex = 1),
                headerStyle = list(background = hexcol,
                                   color = ifelse(team %in% 
                                                    c( "CWS", "KC", "MIL", "NYY", "TB", "PIT", "OAK"), 
                                                  "#000000","#f6f8fa"))
              ),
              theme = reactableTheme(
                borderColor = hexcol,
                stripedColor = "#f6f8fa",
                cellPadding = "8px 12px",
                selectStyle = list(background = hexcol)
              )
            )
      
    })
  
  logo_2 <- Waiter$new(
    id = "Team_3",
    html = spin_loaders(2, color = "#D50032"),
    color = transparent(.8)
  )
  
  output$Team_3 <- renderUI({
 
    logo_2$show()
    
    req(input$team_inp)
    Sys.sleep(.6)
    
    
    team <- input$team_inp
    
    if(team == "All")
    {
      link <- "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg"
      tags$img(src = link,
               height=150, 
               width=230)
    }else{
      num <- teams$number[teams$Abbrev == team]
      link <- paste("https://www.mlbstatic.com/team-logos/team-cap-on-light/", num, ".svg", sep ="")
      tags$img(src = link,
               height=180, 
               width=180)
    }
    
  })

  ###############################################################################################
  ###############################################################################################
  ###############################################################################################
  ###############################################################################################
  ################################### PITCH TABLE ################################################
  
  table_2 <- Waiter$new(
    id = "pitch_table",
    html = spin_loaders(2, color = "#D50032"),
    color = transparent(.7)
  )
  
  output$pitch_table <- renderReactable({
    
    table_2$show()
    
    validate(
      need(input$team_lead != "", "Select a Team")
    )
    validate(
      need(input$year_lead != "", "Select a year")
    )
    validate(
      need(input$lvl_lead != "", "Select a Level")
    )
    
    team <- input$team_lead
    year <- input$year_lead
    lvl <- input$lvl_lead
    pitch <- input$pitch_type_table
    
    colnames(data)[7] <- "LVL"
    
     pitch_type_data %>%
      left_join(data %>% select(ID, NAME, YEAR, LVL, TEAM), by = c("ID", "NAME", "YEAR", "LVL")) %>%
      filter(PITCH_TYPE == pitch) %>%
        filter(LVL == lvl) %>%
          filter(YEAR == year) %>%
            select(ID, NAME, TEAM, PITCHER_THROWS, PG, VELO, LVA, HM, IVB, SPINRATE, SWM, CHASERATE) -> demo
    
    if(pitch %in% c("Sinker", "Fastball"))
    {
      if(pitch == "Fastball")
      {
        location %>% 
         filter(PITCH_TYPE == pitch) %>%
          filter(PLATEHT == "Upper-Third") %>% 
            filter(LVL == lvl) %>%
              filter(YEAR == year) %>%
                  mutate(vaa_n = cume_dist(VAA)) %>%
                    select(ID, NAME, VAA, vaa_n) -> df
                
      }
      if(pitch == "Sinker")
      {
        location %>% 
          filter(PITCH_TYPE == pitch) %>%
           filter(PLATEHT == "Bottom-Third") %>% 
            filter(LVL == lvl) %>%
              filter(YEAR == year) %>%
                mutate(vaa_n = cume_dist(desc(VAA))) %>%
                  select(ID, NAME, VAA, vaa_n) -> df
      }
      
       demo %>% 
         group_by(PITCHER_THROWS) %>%
          mutate(hm_n = cume_dist(abs(HM))) %>%
            ungroup() %>%
              mutate(pg_n = cume_dist(PG),
                     velo_n = cume_dist(VELO),
                     LVA_n = cume_dist(LVA),
                     IVB_n = cume_dist(IVB),
                     spin_n = cume_dist(SPINRATE),
                     swm_n = cume_dist(SWM),
                     ch_n = cume_dist(CHASERATE)
                     ) %>% left_join(df, by = c("ID", "NAME")) -> use
       
       if(team == "All")
       {
         stab <- sinker_fb_tab(team, pitch, use)
       }else{
         stab <- sinker_fb_tab(team, pitch, use %>% filter(TEAM == team))
       }

       return(stab)
       
    }else{
      
      demo %>% 
        group_by(PITCHER_THROWS) %>%
        mutate(hm_n = cume_dist(abs(HM))) %>%
        ungroup() %>%
        mutate(pg_n = cume_dist(PG),
               velo_n = cume_dist(VELO),
               LVA_n = cume_dist(LVA),
               IVB_n = cume_dist(IVB),
               spin_n = cume_dist(SPINRATE),
               swm_n = cume_dist(SWM),
               ch_n = cume_dist(CHASERATE)) -> use
      
      if(team == "All")
      {
        stab <- other_pitches(team, pitch, use)
      }else{
        stab <- other_pitches(team, pitch, use %>% filter(TEAM == team))
      }
      return(stab)
    }
  })
  
  
  ######################################## MAIN TABLE ##############################################
  table_1 <- Waiter$new(
    id = "main_table",
    html = spin_loaders(2, color = "#D50032"),
    color = transparent(.7)
  )

  output$main_table <- renderReactable({
    
    table_1$show()
    
    validate(
      need(input$team_lead != "", "Select a Team")
    )
    validate(
      need(input$year_lead != "", "Select a year")
    )
  validate(
    need(input$lvl_lead != "", "Select a Level")
    )

    
    team <- input$team_lead
    year <- input$year_lead
    lvl <- input$lvl_lead
    mnfa_ind <- input$mnfa
    roster <- input$yes40

   # filter by lvl
    main_ldr %>%
      filter(YEAR == year) %>%
        filter(LVL %in% lvl) %>%
          mutate(swm_n = cume_dist(SWM),
                 ch_n = cume_dist(CHASERATE),
                 bb_n = cume_dist(desc(BB_RATE)),
                 k_n = cume_dist(K_RATE)) -> main_next
    
    if(team == "All")
     {
       main_mid <- main_next
     }else{
       main_next %>% filter(TEAM == team) -> main_mid
     }
    
    colnames(mnfa)[1]<- "ID"
    colnames(mnfa)[2] <- "NAME"
    main_mid %>% merge(mnfa, by = c("ID", "NAME")) -> main_last
    
    # check for mnfa
    if("NA" %in% mnfa_ind)
    {
      main_last <- main_last
    }else{
      main_last %>%
        filter(MNFAseason %in% mnfa_ind) -> main_last
    }
    
    # check for 40
    if(roster != "NA")
    {
      roster_b <- ifelse(roster == "Yes", TRUE, FALSE)
      main_last %>%
        filter(on40manroster == roster_b) -> main_final
    }else{
      
      main_final <- main_last
    }
    
    main_final$swm_n = round(main_final$swm_n*100)
    main_final$ch_n = round(main_final$ch_n*100)
    main_final$k_n = round(main_final$k_n*100)
    main_final$bb_n = round(main_final$bb_n*100)
    
    df_col <- data.frame(
      ntile_swm = main_final$swm_n,
      ntile_ch = main_final$ch_n,
      ntile_k = main_final$k_n,
      ntile_bb = main_final$bb_n
    ) %>% mutate(col_Swm = BuYlRd(ntile_swm/100),
                 col_ch =  BuYlRd(ntile_ch/100),
                 col_k =   BuYlRd(ntile_k/100),
                 col_bb =  BuYlRd(ntile_bb/100)
                 ) -> df_col
    
    df_col_swm <- df_col[order(df_col[,1]),]
    df_col_ch <- df_col[order(df_col[,2]),]
    df_col_k <- df_col[order(df_col[,3]),]
    df_col_bb <- df_col[order(df_col[,4]),]
    
    if(nrow(main_final) == 0)
    {
      return(NULL)
    }
    
    main_final$SWM <- paste0(main_final$SWM, "%", sep = "")
    main_final$CHASERATE <- paste0(main_final$CHASERATE, "%", sep = "")
    main_final$BB_RATE <- paste0(main_final$BB_RATE, "%", sep = "")
    main_final$K_RATE <- paste0(main_final$K_RATE, "%", sep = "")
    
  main_final %>% 
      select(TEAM, LVL, NAME, SWM,swm_n, CHASERATE, ch_n, BB_RATE, bb_n, K_RATE, k_n) %>%
      reactable(
        columns = list(
          TEAM = colDef(name = "", minWidth = 70, align = 'center',
            cell = function(value) {
                num <- teams$number[teams$Abbrev == value]
                link <- paste("https://www.mlbstatic.com/team-logos/team-cap-on-light/", num, ".svg", sep ="")
                image <- img(src = link, style = "height: 24px;", alt = "")
                tagList(
                  div(style = "display: inline-block; width: 45px;", image))
              }),
          LVL = colDef(name = "Level", minWidth = 80),
          NAME = colDef(name = "Name", minWidth = 170),
          SWM = colDef(name = "SWM%", align = 'center'),
          swm_n = colDef(name = "n", align = 'center',
                         cell = data_bars(., 
                                          text_size = 13, 
                                          box_shadow = TRUE,
                                          text_position = 'center',
                                          force_outside = c(0,27),
                                          min_value = 0,
                                          max_value = 100,
                                          fill_color = df_col_swm$col_Swm
                         )),
          CHASERATE = colDef(name = "CH%", align = 'center'),
          ch_n = colDef(name = "n", align = 'center',
                        cell = data_bars(., 
                                         text_size = 13, 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         force_outside = c(0,27),
                                         min_value = 0,
                                         max_value = 100,
                                         fill_color = df_col_ch$col_ch
                        )),
          BB_RATE = colDef(name = "BB%", align = 'center'),
          bb_n = colDef(name = "n",  align = 'center',
                        cell = data_bars(., 
                                         text_size = 13, 
                                         box_shadow = TRUE,
                                         text_position = 'center',
                                         force_outside = c(0,27),
                                         min_value = 0,
                                         max_value = 100,
                                         fill_color = df_col_bb$col_bb
                          )
                        ),
          K_RATE = colDef(name = "K%", align = 'center'),
          k_n = colDef(name = "n", align = 'center',
                          cell = data_bars(., 
                                   text_size = 13, 
                                   box_shadow = TRUE,
                                   text_position = 'center',
                                   force_outside = c(0,27),
                                   min_value = 0,
                                   max_value = 100,
                                   fill_color = df_col_k$col_k
                          )
                        )
        ),
        width = 1150,
        filterable = TRUE,
        bordered = TRUE,
        defaultColDef = colDef(
          header = function(value) gsub(".", " ", value, fixed = TRUE),
          style = list(zIndex = 1),
          headerStyle = list(background = ifelse(team == "All","#D50032" ,teams$Color[teams$Abbrev == team]),
                             color = ifelse(teams$Abbrev[teams$Abbrev == team] %in% 
                                              c( "CWS", "KC", "MIL", "NYY", "TB", "PIT", "OAK"), 
                                            "#000000","#f6f8fa"))
        ),
        theme = reactableTheme(
          borderColor = ifelse(team == "All", "#002D72" ,teams$Color[teams$Abbrev == team] ),
          stripedColor = "#f6f8fa",
          cellPadding = "8px 12px",
          selectStyle = list(background = teams$Color[teams$Abbrev == team])
        )
      )

  })
  
  ###################### SET UP FILTERS #############################################
  
  logo <- Waiter$new(
    id = "Team_2",
    html = spin_loaders(2, color = "#D50032"),
    color = transparent(.8)
  )
  
  output$Team_2 <- renderUI({

    logo$show()
    
    req(input$team_lead)
    Sys.sleep(.6)

    team <- input$team_lead
    
    if(team == "All")
    {
      link <- "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg"
      tags$img(src = link,
               height=150, 
               width=230)
    }else{
    num <- teams$number[teams$Abbrev == team]
    link <- paste("https://www.mlbstatic.com/team-logos/team-cap-on-light/", num, ".svg", sep ="")
    tags$img(src = link,
             height=180, 
             width=180)
    }
  })

  output$box_filters <- renderUI({

    team <- input$team_lead

    hexcol <- ifelse(team == "All", "#002D72",teams$Color[teams$Abbrev == team])

    tags$div(
      class = "another_one", id = "filters",
      box(width = 12,
          solidHeader = TRUE,
          collapsible = FALSE,
          title = "Please Select from the below filters",
          status = "primary",
          fluidRow(
            column(2, selectInput("year_lead",
                                              label = "Select a Year",
                                              choices = c(2022, 2021),
                                              selected = 2022)
            ),
            
            column(2, selectInput("lvl_lead",
                                  label = "Select a Level",
                                  multiple = TRUE,
                                  choices = c("MLB", "AAA", "AA", "A+", "A", "Rk", "Rk-"),
                                  selected = "MLB")
            ),
            column(5, checkboxGroupInput("mnfa",
                                         label = "Select Minor League FA season",
                                         choices = c("NA", "2022", "2023", "2024", "2025"),
                                         inline = TRUE,
                                         selected = "NA" )),
            column(3, radioButtons("yes40",
                                    label = "On the 40 man?",
                                    choices = c("NA", "Yes", "No"),
                                    inline = TRUE,
                                    selected = "NA"
            ))
          )
      ),
      tags$style(HTML(paste0("
                        #filters .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        "))
                )
              )
  })

  
  
  
######################################### 2nd tab begins here ##############################################

  
####################################FOURTH PANEL #######################################################
####################################FOURTH PANEL #######################################################
####################################FOURTH PANEL #######################################################
####################################FOURTH PANEL #######################################################

  
  ######################################## HEAT MAP PITCH UAGE ########################################
  
  output$KZone_usage <- renderPlot({
    
    pitcher <- input$Pitcher
    pitch <- input$Pitch_Type
    year_ <- input$year3
    lvl_ <- input$lvl3
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])

    coord %>%
      filter(ID == ID_) %>%
        filter(YEAR == year_) %>%
          filter(LVL == lvl_) %>%
            filter(PITCH_TYPE == pitch) -> use
              
    
    ggplot(use, aes(SIDE, HEIGHT)) + geom_point() +
    stat_density2d(aes(fill = ..density..), geom = 'tile', contour = FALSE, show.legend = FALSE)+
    scale_fill_distiller(palette = 'YlOrRd', direction = 1) +
    geom_segment(aes(x = -.708, y = 1.55, xend = .708, yend = 1.55), size = .9, color = 'black', linetype = 1)+
    geom_segment(aes(x = -.708, y = 3.42, xend = .708, yend = 3.42), size = .9, color = 'black', linetype = 1)+
    geom_segment(aes(x = -.708, y = 1.55, xend = -.708, yend = 3.42), size = .9, color = 'black', linetype = 1)+
    geom_segment(aes(x = .708, y = 1.55, xend = .708, yend = 3.42), size = .9, color = 'black', linetype = 1)  + 
    xlim(c(-1.8, 1.8)) + ylim(c(1,4)) +
    ggtitle("Pitch Usage Location") + labs(caption = paste0("Number of ", unique(use$PITCH_TYPE), "s: ", nrow(use), sep = ""), 
                                           subtitle = "Catcher's Point of view")+
    theme( plot.title = element_text(color="black", size=14, face="bold.italic"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_rect(fill = "#FFFFFF"),
           panel.border = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           axis.text.y = element_blank(),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.ticks.y = element_blank())
    
  })
  
############################# TOP MID BOT STATS ##################################################################
  
  output$Kzone <- renderPlot({
    
    req(input$Pitcher)
    req(input$metric)
    req(input$Pitch_Type)
    req(input$year3)
    req(input$lvl3)
    
    pitcher <- input$Pitcher
    metric_ <- input$metric
    pitch <- input$Pitch_Type
    year_ <- input$year3
    lvl_ <- input$lvl3
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
    hexcol <- teams$Color[teams$Abbrev == team]
    df_metrics <- data.frame("metric" = c("SWM%", "CH%", "Str%", "Nitro%", "GB%", "PG"),
                             rank = c("SWM", "CHASERATE", "STR_RATE", "NITRORATE", "GBRATE", "PG"),
                             num =c(8, 9, 12, 7, 11, 14),
                             direc = c(1,1,1,0,0,1))
    
    col <- df_metrics %>% filter(metric == metric_)
    
    location %>%
      filter(YEAR == year_) %>%
        filter(LVL == lvl_) %>%
          filter(PITCH_TYPE == pitch) %>%
            select(ID, NAME,PLATEHT,col$num , TOT ) -> use
    
    colnames(use)[4]<- "stat"
    
    use %>%
      filter(TOT > 25) %>%
        filter(!is.na(stat)) %>%
        group_by(PLATEHT) %>%
          mutate("col_ntile" = cume_dist(stat)) %>%
            filter(ID == ID_) -> EV
    
    # Fix up a couple things with Nitro rates
    if(metric_ %in% c("Nitro%"))
    {
      if(!is.na(EV$col_ntile[1]))
      {
        EV$col_ntile[1] = 1 - EV$col_ntile[1]
      }
      if(!is.na(EV$col_ntile[2]))
      {
        EV$col_ntile[2] = 1 - EV$col_ntile[2]
      }
      if(!is.na(EV$col_ntile[3]))
      {
        EV$col_ntile[3] = 1 - EV$col_ntile[3]
      }
    }
    
    if(metric_ %in% c("SWM%", "CH%", "GB%", "Nitro%"))
    {
      EV$stat = paste0(EV$stat, "%", sep = "")
    }

    if(metric_ %in% c("Str%"))
    {
      EV$stat = paste0(round(EV$stat * 100), "%", sep ="")
    }
    if(metric_ %in% c("PG"))
    {
      EV$stat = round(EV$stat,2)
    }

    EV %>% filter(PLATEHT == "Upper-Third") -> up
    EV %>% filter(PLATEHT == "Middle-Third") -> mid
    EV %>% filter(PLATEHT == "Bottom-Third") -> bot
    
    ggplot() +
      geom_ribbon(mapping = aes(x = c(-15,15), ymin = 2.83, ymax = 3.8), fill = ifelse(is_empty(up$col_ntile), "#FFFFFF", BuYlRd(up$col_ntile)),alpha=0.5)+
      geom_ribbon(mapping = aes(x = c(-15,15), ymin = 2.17, ymax = 2.83), fill =ifelse(is_empty(mid$col_ntile), "#FFFFFF", BuYlRd(mid$col_ntile)),alpha=0.5)+
      geom_ribbon(mapping = aes(x = c(-15,15), ymin = 1.3, ymax = 2.17), fill =ifelse(is_empty(bot$col_ntile), "#FFFFFF", BuYlRd(bot$col_ntile)),alpha=0.5)+
      geom_segment(aes(x = -8.5, y = 3.42, xend = 8.5, yend = 3.42), size = 1.2) + 
      geom_segment(aes(x = -8.5, y = 1.55, xend = 8.5, yend = 1.55), size = 1.2) + 
      geom_segment(aes(x = -8.5, y = 3.42, xend = -8.5, yend = 1.55), size = 1.2) + 
      geom_segment(aes(x = 8.5, y = 1.55, xend = 8.5, yend = 3.42), size = 1.2) + 
      geom_segment(aes(x = -15, y = 2.83, xend = 15, yend = 2.83), size = 1.2) +
      geom_segment(aes(x = -15, y = 2.17, xend = 15, yend = 2.17), size = 1.2) +
      geom_segment(aes(x = -2.83, y = 1.3, xend = -2.83, yend = 3.8), size = .7, linetype = 2)+
      geom_segment(aes(x = 2.83, y = 1.3, xend = 2.83, yend = 3.8), size = .7, linetype = 2)+
      xlim(-15,15) + ylim(1.3,3.8) + 
      geom_label(data = up, mapping = aes(x = 0, y = 3.2, 
                                   label = ifelse(is_empty(stat), "Not Enough Pitches",paste0(stat, '\n nth:', round(col_ntile*100),'', sep = ""))), 
                                   fill = ifelse(is_empty(up$col_ntile), "#FFFFFF", BuYlRd(up$col_ntile))) +
      geom_label(data = mid, mapping = aes(x = 0, y = 2.55, 
                                    label = ifelse(is_empty(stat), "Not Enough Pitches",paste0(stat, '\n nth:', round(col_ntile*100),'', sep = ""))), 
                                    fill =ifelse(is_empty(mid$col_ntile), "#FFFFFF", BuYlRd(mid$col_ntile))) +
      geom_label(data = bot, mapping = aes(x = 0, y = 1.9, 
                                    label = ifelse(is_empty(stat), "Not Enough Pitches",paste0(stat, '\n nth:', round(col_ntile*100),'', sep = ""))), 
                                    fill = ifelse(is_empty(bot$col_ntile), "#FFFFFF", BuYlRd(bot$col_ntile))) +
      ggtitle(paste0(metric_, " per Zone (Top, Middle, Bottom)", sep = "")) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
      
  })
  
####################################### vaa FASTBALL ########################################################  
  
  output$vaa_fb_perc <- renderReactable({
    
    req(input$Pitcher)
    
    pitcher <- input$Pitcher
    lvl_ <- input$lvl3
    year_ <- input$year3
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1]) 
    
    location %>%
      filter(PITCH_TYPE == "Fastball") %>%
        filter(PLATEHT == "Upper-Third") %>% 
          filter(LVL == lvl_) %>%
           filter(TOT > 30) %>%
             mutate(vaa_ntile = cume_dist(VAA)) %>%
              filter(ID == ID_) %>%
                filter(YEAR == year_) %>%
                  select(NAME, YEAR, LVL, TOT, VAA, vaa_ntile) -> use
    
    use$vaa_ntile <- round(use$vaa_ntile*100)
    use$VAA <- round(use$VAA,2)
    
    use %>%
      select(VAA, vaa_ntile) %>%
        reactable(
          columns = list(
            VAA = colDef(name = "VAA", align = 'center',
                         style = function(value, index)
                         {
                           ntile = use[index,]$vaa_ntile/100
                           color = BuYlRd(ntile)
                           list(background = color, 
                                color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))
                         }),
            vaa_ntile = colDef(name = "Percentile", align = 'center',
                               style = function(value, index)
                               {
                                 ntile = value/100
                                 color = BuYlRd(ntile)
                                 list(background = color, 
                                      color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))
                               })
          )
        )
    
  })
  
########################### VAA FB BPX ###############################################################
  output$vaa_fb_up <- renderUI({
    
    req(input$Pitcher)
    
    team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
    hexcol <- teams$Color[teams$Abbrev == team]
    
    tags$div(
      class = "another_one", id = "vaa_fb",
      box(width = 12,
          solidHeader = TRUE,
          title = "VAA on FB Up",
          status = "primary",
          reactableOutput("vaa_fb_perc")
      ), 
      tags$style(HTML(paste0("
                        #vaa_fb .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        "))
          )   
      )
  })
  
############################## VAA SINKER ###########################################################  
  output$vaa_sink <- renderReactable({
    
    req(input$Pitcher)
    
    pitcher <- input$Pitcher
    lvl_ <- input$lvl3
    year_ <- input$year3
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1]) 
    
    location %>%
      filter(PITCH_TYPE == "Sinker") %>%
      filter(PLATEHT == "Bottom-Third") %>% 
      filter(LVL == lvl_) %>%
      filter(TOT > 30) %>%
      mutate(vaa_ntile = cume_dist(desc(VAA))) %>%
      filter(ID == ID_) %>%
      filter(YEAR == year_) %>%
      select(NAME, YEAR, LVL, TOT, VAA, vaa_ntile) -> use
    
    use$vaa_ntile <- round(use$vaa_ntile*100)
    use$VAA <- round(use$VAA,2)
    
    use %>%
      select(VAA, vaa_ntile) %>%
      reactable(
        columns = list(
          VAA = colDef(name = "VAA", align = 'center',
                       style = function(value, index)
                       {
                         ntile = use[index,]$vaa_ntile/100
                         color = BuYlRd(ntile)
                         list(background = color, 
                              color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))
                       }),
          vaa_ntile = colDef(name = "Percentile", align = 'center',
                             style = function(value, index)
                             {
                               ntile = value/100
                               color = BuYlRd(ntile)
                               list(background = color, 
                                    color = ifelse(!between(ntile, .20, .80), "#FFFFFF", "#000000"))
                             })
        )
      )
  })
###################### VAA SINKER BOX #####################################################  
  output$vaa_si_dw <- renderUI({
    
    req(input$Pitcher)
    
    team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
    hexcol <- teams$Color[teams$Abbrev == team]
    
    tags$div(
      class = "another_one", id = "vaa_si",
      box(width = 12,
          solidHeader = TRUE,
          title = "VAA on SI Down",
          status = "primary",
          reactableOutput("vaa_sink")
      ), 
      tags$style(HTML(paste0("
                        #vaa_si .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        "))
      )   
    )
  })

############################# INPUTS THINGS ###################################################
  observeEvent(c(input$Pitcher, input$year3), {
    
    req(input$Pitcher)
    pitcher <- input$Pitcher
    year_ <- input$year3
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1]) 
    
    pitch_type_data %>%
      filter(ID == ID_) %>%
        filter(YEAR == year_) %>%
          select(LVL) %>% unique() -> choices_
    
    level_order <- data.frame(
      lvls = c("MLB", "AAA", "AA", "A+", "A","A-", "Rk", "Rk-")
    ) %>% mutate(rank = 1:8)
    
    choices_ %>% left_join(level_order, by = c("LVL" = "lvls")) -> use
    
    use <- use[with(use, order(rank)),]
    
    updateSelectInput(session, 
                      inputId = "lvl3",
                      label = "Select a Level:",
                      choices = use$LVL,
                      selected = use$LVL[1])
    
  })
  
  observeEvent(c(input$Pitcher,input$year3, input$lvl3),{
    
    req(input$Pitcher)
    pitcher <- input$Pitcher
    year_ <- input$year3
    lvl_ <- input$lvl3
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1]) 
    
    pitch_type_data %>%
      filter(ID == ID_) %>%
        filter(YEAR == year_) %>%
          filter(LVL == lvl_) %>%
            select(PITCH_TYPE, TOT) %>% unique() -> types
    
    updateSelectInput(session,
                      "Pitch_Type",
                      choices = types$PITCH_TYPE,
                      selected = types$PITCH_TYPE[types$TOT == max(types$TOT)])
  })
  
#################################THIRD PANEL######################################################
#################################THIRD PANEL##############################################33#####
##################################THIRD PANEL####################################################
##################################THIRD PANEL####################################################3
   


##################################### THE LVA GGPLOT ##########################################################
  
  output$charts_graphs <- renderUI({
    
    validate(
      need(input$Pitcher != "",  "")
    )
    
    team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
    player <-  substr(input$Pitcher, 0, str_length(input$Pitcher)-6)
    hexcol <- teams$Color[teams$Abbrev == team]
    
    name = sub("(\\w+),\\s(\\w+)","\\2 \\1", player)
    tags$div(
      class = "another_one", id = "charts",
      column(4,  align = 'center',
             box(width = 12,
                    solidHeader = TRUE,
                    title = paste0(name, "'s Pitch Usage"),
                    status = "primary",
                    plotlyOutput("arsenal"))
      ),
      column(4,  align = 'center',
             box(width = 12,
                    solidHeader = TRUE,
                    title = "Fastball Rise: IVB vs Release Height",
                    status = "primary",
                    plotlyOutput("fb_check"))
      ),
      column(4, align = 'center',
             box(width = 12,
               solidHeader = TRUE,
               title = "Location Value Added:",
               status = "primary",
               plotOutput("tilt_chart"))
      ),
      tags$style(HTML(paste0("
                        #charts .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        "))
      )   
    )
  })
  
  output$tilt_chart <- renderPlot({
    
    req(input$Pitcher)
    req(input$player_lvl_two)

    
    pitcher = input$Pitcher
    lvl_ = input$player_lvl_two
    year_ = input$player_year_two

    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
    hexcol <- teams$Color[teams$Abbrev == team]
    
    lva_data %>%
       filter(YEAR == year_) %>%
        filter(LVL == lvl_) %>%
            group_by(PITCH_TYPE, YEAR, LVL) %>% 
              mutate(lva_n = cume_dist(LVA))%>%
                ungroup() %>% 
                  filter(ID == ID_) %>%
                   select(PITCH_TYPE, LVA, lva_n) -> use
    
    validate(
      need(nrow(use) >= 1, "This player does not have enough pitches at this Level to properly acquire his LVA values.")
    )
    
    if(nrow(use) < 1)
    {
      return(NULL)
    }
    if(lvl_ == "Rk-")
    {
      return(NULL)
    }

    df_1 <- data.frame(x = use$PITCH_TYPE,  y = 100 )
    df_2 <- data.frame(x =  use$PITCH_TYPE,  y = 0 )
    df_3 <- data.frame(x =  use$PITCH_TYPE, y = 50 )
    
    ggplot() +
      geom_bar(data = use, aes(PITCH_TYPE, 100), 
               stat = 'identity', width = .1, fill = "#D3D3D3" 
               ) + 
      geom_point(data = df_1, aes(x = x, y = y),size = 8, color = "#D3D3D3") +
      geom_point(data = df_2, aes(x = x, y = y), size = 8, color = "#D3D3D3") +
      geom_point(data = df_3, aes(x = x, y = y), size = 8, color = "#D3D3D3") +
      geom_point(aes(x = use$PITCH_TYPE, y = use$lva_n*100 ),color = BuYlRd(use$lva_n), size = 10) +
      geom_text(aes(x = use$PITCH_TYPE, y = use$lva_n*100, label = round(use$lva_n*100)), color = ifelse(!between(use$lva_n*100, .20,.80), "#000000", "#FFFFFF")) +
      coord_flip() + 
      theme(panel.background = element_rect(fill = "#FFFFFF"),
            title = element_text(size = 14),
            panel.border = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank()
           ) 
  })
  
  
  ################################### FB CHART ##########################################################################
  
  fb <- Waiter$new(
    id = "fb_check",
    html = div(
      style="color:#990c1a;",
      spin_whirly()
      ),
    color = "#FFFFFF"
  )
  
  output$fb_check <- renderPlotly({
    
    req(input$Pitcher)
    
    pitcher = input$Pitcher
    lvl_ = input$player_lvl_two
    year_ = input$player_year_two

    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
    hexcol <- teams$Color[teams$Abbrev == team]
    
    pitch_type_data %>%
      filter(YEAR == year_) %>%
        filter(LVL == lvl_) %>%
          filter(PITCH_TYPE == "Fastball") %>%
              group_by(ID, NAME, YEAR, LVL) %>% 
               mutate(relht_act = RELHT * TOT / sum(TOT)) %>% 
                  mutate(IVB_act = IVB*TOT/sum(TOT)) %>%
                     mutate(RELHT = sum(relht_act),
                            IVB = sum(IVB_act)) %>%
                        select(ID, NAME, RELHT, IVB) -> use
    
    validate(
      need(nrow(use) >= 1, "This player does not have enough fastballs at this Level to properly display his FB Rise.")
    )
    
    if(nrow(use) < 1)
    {
      return(NULL)
    }
             
    p <- ggplot() +
    geom_point(data = use %>% filter(NAME != player), aes(RELHT, IVB, 
                                                    text = paste0('NAME: ',NAME,
                                                                  '<br>RELHT: ', round(RELHT,1), ' in',
                                                                  '<br>IVB: ', round(IVB,1), ' in' ))) +
      geom_smooth(data = use %>% filter(NAME != player), aes(RELHT, IVB,), se = FALSE, size = 3, 
                  color = hexcol, show.legend = FALSE) + 
      geom_point(data = use %>% filter(NAME == player), aes(x = RELHT, y = IVB),  color = "red",
                 shape = 23, fill = hexcol, size = 4, show.legend = FALSE) +
      xlab("Release Height") + ylab("Induced Vertical Break") +
      xlim(4.5, 7) + ylim(9,23) +
      theme(panel.background = element_rect(fill = "#FFFFFF"),
            panel.border = element_blank()) 
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    
  })
  
  ############################### PITCH SUBTYPPE####################################################################
  
  output$subtype_table_22 <- renderReactable({
    
    pitcher = input$Pitcher
    lvl_ = input$player_lvl_two
    # types = input$pitch_types
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
    hexcol <- teams$Color[teams$Abbrev == team]
    
    # Find percs for types and subtypes
    pitch_type_data %>% 
      filter(YEAR == 2022) %>%
        group_by(YEAR, LVL, PITCH_TYPE) %>%
          mutate(swm_ntile = cume_dist(SWM),
                 velo_ntile = cume_dist(VELO),
                 ch_ntile = cume_dist(CHASERATE),
                 spin_ntile= cume_dist(SPINRATE),
                 pg_ntile = cume_dist(PG),
                 gb_ntile = cume_dist(GBRATE)) %>%
                  ungroup () -> pitch_types_use
    
    pitch_subtype_data %>%
      filter(YEAR == 2022) %>%
      group_by(YEAR, LVL, PITCH_TYPE, PITCH_SUBTYPE) %>%
        mutate(swm_ntile = cume_dist(SWM),
               velo_ntile = cume_dist(VELO),
               ch_ntile = cume_dist(CHASERATE),
               spin_ntile= cume_dist(SPINRATE),
               pg_ntile = cume_dist(PG),
               gb_ntile = cume_dist(GBRATE)) %>%
                 ungroup() -> pitch_subtypes_use
    
    # get types and subtypes for player asked
    pitch_types_use %>%
          filter(LVL == lvl_) %>%
              filter(ID == ID_) %>%
                select(YEAR, PITCH_TYPE, 
                       TOT, VELO, HM, 
                       IVB,  SPINRATE, SWM,
                       CHASERATE, STR_RATE, 
                       PG, VAA, BIP, 
                       NITRORATE, GBRATE)-> use
    
    pitch_subtypes_use %>%
        filter(LVL == lvl_) %>%
          filter(ID == ID_) %>%
            select(YEAR,PITCH_TYPE, PITCH_SUBTYPE, 
                   TOT, VELO, HM, 
                   IVB,  SPINRATE, SWM,
                   CHASERATE, STR_RATE, 
                   PG, VAA, BIP, 
                   NITRORATE, GBRATE) -> use_subtypes
    
    if(nrow(use) < 1)
    {
      return(NULL)
    }
    
    # fix values for types
    use$HM <- round(use$HM,1)
    use$VELO <- round(use$VELO, 1)
    use$SWM <- paste(use$SWM, "%", sep = "")
    use$CHASERATE <- paste(use$CHASERATE, "%", sep = "")
    use$IVB <- round(use$IVB,1)
    use$SPINRATE <- round(use$SPINRATE)
    use$STR_RATE <- round(use$STR_RATE * 100, 1)
    use$PG <- round(use$PG,2)
    use$GBRATE <- paste(round(use$GBRATE), "%", sep = "")
    use$VAA <- round(use$VAA, 2)
    use$NITRORATE <- paste(use$NITRORATE, "%", sep = "")
    use %>% filter(TOT > 25) -> use
    use <- use[order( desc(use$YEAR), desc(use %>% group_by(PITCH_TYPE) %>% mutate(tot_t = sum(TOT)) %>% pull(tot_t))),] 

    #fix values for subtypes
    use_subtypes$HM <- round(use_subtypes$HM,1)
    use_subtypes$VELO <- round(use_subtypes$VELO, 1)
    use_subtypes$SWM <- paste(use_subtypes$SWM, "%", sep = "")
    use_subtypes$CHASERATE <- paste(use_subtypes$CHASERATE, "%", sep = "")
    use_subtypes$IVB <- round(use_subtypes$IVB,1)
    use_subtypes$SPINRATE <- round(use_subtypes$SPINRATE)
    use_subtypes$STR_RATE <- round(use_subtypes$STR_RATE * 100, 1)
    use_subtypes$PG <- round(use_subtypes$PG,2)
    use_subtypes$GBRATE <- paste(round(use_subtypes$GBRATE), "%", sep = "")
    use_subtypes$VAA <- round(use_subtypes$VAA, 2)
    use_subtypes$NITRORATE <- paste(use_subtypes$NITRORATE, "%", sep = "")
    
    
    tab <- pitch_type_table(ID_, lvl_, pitch_types_use, pitch_subtypes_use, use, use_subtypes, hexcol, team)
    return(tab)
    
  })
 
  output$subtype_table_21 <- renderReactable({
    
    pitcher = input$Pitcher
    lvl_ = input$player_lvl_two
    # types = input$pitch_types
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
    hexcol <- teams$Color[teams$Abbrev == team]
    
    # Find percs for types and subtypes
    pitch_type_data %>% 
      filter(YEAR == 2021) %>%
      group_by(YEAR, LVL, PITCH_TYPE) %>%
      mutate(swm_ntile = cume_dist(SWM),
             velo_ntile = cume_dist(VELO),
             ch_ntile = cume_dist(CHASERATE),
             spin_ntile= cume_dist(SPINRATE),
             pg_ntile = cume_dist(PG),
             gb_ntile = cume_dist(GBRATE)) %>%
      ungroup () -> pitch_types_use
    
    pitch_subtype_data %>%
      filter(YEAR == 2021) %>%
      group_by(YEAR, LVL, PITCH_TYPE, PITCH_SUBTYPE) %>%
      mutate(swm_ntile = cume_dist(SWM),
             velo_ntile = cume_dist(VELO),
             ch_ntile = cume_dist(CHASERATE),
             spin_ntile= cume_dist(SPINRATE),
             pg_ntile = cume_dist(PG),
             gb_ntile = cume_dist(GBRATE)) %>%
      ungroup() -> pitch_subtypes_use
    
    # get types and subtypes for player asked
    pitch_types_use %>%
      filter(LVL == lvl_) %>%
      filter(ID == ID_) %>%
      select(YEAR, PITCH_TYPE, 
             TOT, VELO, HM, 
             IVB,  SPINRATE, SWM,
             CHASERATE, STR_RATE, 
             PG, VAA, BIP, 
             NITRORATE, GBRATE)-> use
    
    pitch_subtypes_use %>%
      filter(LVL == lvl_) %>%
      filter(ID == ID_) %>%
      select(YEAR,PITCH_TYPE, PITCH_SUBTYPE, 
             TOT, VELO, HM, 
             IVB,  SPINRATE, SWM,
             CHASERATE, STR_RATE, 
             PG, VAA, BIP, 
             NITRORATE, GBRATE) -> use_subtypes
    
    if(nrow(use) < 1)
    {
      return(NULL)
    }
    
    # fix values for types
    use$HM <- round(use$HM,1)
    use$VELO <- round(use$VELO, 1)
    use$SWM <- paste(use$SWM, "%", sep = "")
    use$CHASERATE <- paste(use$CHASERATE, "%", sep = "")
    use$IVB <- round(use$IVB,1)
    use$SPINRATE <- round(use$SPINRATE)
    use$STR_RATE <- round(use$STR_RATE * 100, 1)
    use$PG <- round(use$PG,2)
    use$GBRATE <- paste(round(use$GBRATE), "%", sep = "")
    use$VAA <- round(use$VAA, 2)
    use$NITRORATE <- paste(use$NITRORATE, "%", sep = "")
    use %>% filter(TOT > 25) -> use
    use <- use[order( desc(use$YEAR), desc(use %>% group_by(PITCH_TYPE) %>% mutate(tot_t = sum(TOT)) %>% pull(tot_t))),] 
    
    #fix values for subtypes
    use_subtypes$HM <- round(use_subtypes$HM,1)
    use_subtypes$VELO <- round(use_subtypes$VELO, 1)
    use_subtypes$SWM <- paste(use_subtypes$SWM, "%", sep = "")
    use_subtypes$CHASERATE <- paste(use_subtypes$CHASERATE, "%", sep = "")
    use_subtypes$IVB <- round(use_subtypes$IVB,1)
    use_subtypes$SPINRATE <- round(use_subtypes$SPINRATE)
    use_subtypes$STR_RATE <- round(use_subtypes$STR_RATE * 100, 1)
    use_subtypes$PG <- round(use_subtypes$PG,2)
    use_subtypes$GBRATE <- paste(round(use_subtypes$GBRATE), "%", sep = "")
    use_subtypes$VAA <- round(use_subtypes$VAA, 2)
    use_subtypes$NITRORATE <- paste(use_subtypes$NITRORATE, "%", sep = "")
    
    tab <- pitch_type_table(ID_, lvl_, pitch_types_use, pitch_subtypes_use, use, use_subtypes, hexcol, team)
    return(tab)
    
  })
  ###################################### PIE CHART #################################################
  output$arsenal <- renderPlotly({

  req(input$Pitcher)
    
  pitcher = input$Pitcher
  lvl_ = input$player_lvl_two
  year_ = input$player_year_two

  team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
  player <-  substr(pitcher, 0, str_length(pitcher)-6)
  ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])

  pitch_type_data %>%
    filter(ID == ID_) %>%
      filter(LVL == lvl_) %>%
        filter(YEAR == year_) -> use
  
  validate(
    need(nrow(use) >= 1, "This player does not have enough pitches at this Level to properly provide his correct Arsenal.")
  )
  
  if(nrow(use) < 1)
  {
    return(NULL)
  }
  name <- sub("(\\w+),\\s(\\w+)","\\2 \\1", player)
  use %>%
    plot_ly(labels = ~PITCH_TYPE, values = ~TOT) %>%
      add_pie(hole = .4)  %>%
        layout( 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(orientation = "h",   # show entries horizontally
                              xanchor = "center",# use center of legend as anchor
                              yanchor = "bottom", 
                              x = .5,
                             y  = -0.1,
                             font = list(
                               family = "Arial/Helvetica",
                               color = "black",
                               style = "bold"
                             ), bgcolor = 'rgba(256,256,256,0.5)')) %>% config(displayModeBar = FALSE)


  })

  ############################# OBSERVE THE EVENTS CHANGE INPUTS ######################################################
  
  observeEvent(c(input$player_year_two,input$Pitcher, input$player_lvl_two),
               {
                 pitcher = input$Pitcher
                 lvl_ = input$player_lvl_two
                 year_ = input$player_year_two
                 
                 team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
                 player <-  substr(pitcher, 0, str_length(pitcher)-6)
                 ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
                 
                 pitch_type_data %>%
                   filter(ID == ID_) %>%
                   filter(LVL == lvl_) %>%
                   filter(YEAR == year_) -> use
                 
                 updateCheckboxGroupInput(session,
                                          "subtype",
                                          label = "Select a Pitch Type",
                                          choices = unique(use$PITCH_TYPE),
                                          inline = TRUE)
               })
  #select input of years
  observeEvent(input$Pitcher, {
    
    pitcher <- input$Pitcher
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
    
    data %>% filter(ID == ID_) -> choices_
    
    if(nrow(choices_) < 1)
    {
      return(NULL)
    }
    updateSelectInput(session, "player_year_two", 
                      label = "Year:",
                      choices = sort(unique(choices_$YEAR), decreasing = TRUE)
    )
  })
  
  #select input of teams hes been in
  observeEvent(c(input$player_year_two,input$Pitcher),{
    
    pitcher <- input$Pitcher
    year <- input$player_year_two
    
    team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
    player <-  substr(pitcher, 0, str_length(pitcher)-6)
    ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
    
    data %>% 
      filter(ID == ID_) %>%
        select(ID) -> use_id
    
    use_id <- use_id[1,]
    
    performance %>%
      filter(ID == use_id) %>%
        filter(YEAR == year) %>% 
            select(LVL) %>%
                mutate(rank = ifelse(LVL == "MLB", 1, 
                    ifelse(LVL == "AAA", 2, 
                        ifelse(LVL == "AA", 3, 
                           ifelse(LVL == "A+", 4, 
                              ifelse(LVL == "A", 5, 
                                 ifelse(LVL == "Rk", 6,
                                    ifelse(LVL == "Rk-", 7, NA)))))))) -> teams
    
    updateSelectInput(session, "player_lvl_two",
                      label = paste("Level:"),
                      choices = teams[order(teams$rank),]$LVL)
  })


################################################# 2nd PANEL DOWN BELOW ###################################################
################################################# 2nd PANEL DOWN BELOW ###################################################
################################################# 2nd PANEL DOWN BELOW ###################################################
################################################# 2nd PANEL DOWN BELOW ###################################################
    
##################################### RESULTS BOX #########################################################
  
      observeEvent(input$Pitcher, {
        
        pitcher <- input$Pitcher
        team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
        player <-  substr(pitcher, 0, str_length(pitcher)-6)
        ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
        
        data %>% filter(ID == ID_) -> choices_
        
        if(nrow(choices_) < 1)
        {
          return(NULL)
        }
        updateSelectInput(session, "player_year", 
                    label = "Year",
                    choices = sort(unique(choices_$YEAR), decreasing = TRUE)
        )
      })

      observeEvent(c(input$player_year,input$Pitcher,input$player_team),{

        pitcher <- input$Pitcher
        year <- input$player_year
        teams <- input$player_team
        
        team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
        player <-  substr(pitcher, 0, str_length(pitcher)-6)
        ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
        
        data %>% 
           filter(ID == ID_) %>%
              select(ID) -> use_id
        
        use_id <- use_id[1,]

          performance %>%
            filter(ID == use_id) %>%
              filter(YEAR == year) %>% 
                filter(ORGS == teams) %>%
                  select(LVL) %>%
                   mutate(rank = ifelse(LVL == "MLB", 1, 
                                          ifelse(LVL == "AAA", 2, 
                                                 ifelse(LVL == "AA", 3, 
                                                        ifelse(LVL == "A+", 4, 
                                                               ifelse(LVL == "A", 5, 
                                                                      ifelse(LVL == "Rk", 6,
                                                                             ifelse(LVL == "Rk-", 7, NA)))))))) -> teams

          
        updateSelectInput(session, "player_lvl",
                          label = paste("Level:"),
                          choices = teams[order(teams$rank),]$LVL
                          
        )
      })
  
    observeEvent(c(input$player_year,input$Pitcher),{
      
      pitcher <- input$Pitcher
      year <- input$player_year
      
      team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
      player <-  substr(pitcher, 0, str_length(pitcher)-6)
      ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
      
      data %>% 
        filter(ID == ID_) %>%
          select(ID) -> use_id
      
      use_id <- use_id[1,]
      
      performance %>%
          filter(ID == use_id) %>%
              filter(YEAR == year) %>% 
                select(ORGS) -> teams
      
      updateSelectInput(session, "player_team",
                        label = paste("Team:"),
                        choices = teams$ORGS
      )
          
    })
  

    output$results_box <- renderUI({
      
      validate(
        need(input$Pitcher != "",  "")
      )

      team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
      hexcol <- teams$Color[teams$Abbrev == team]
      
      tags$div(
        class = "another_one", id = "result",
        box(width = 12,
            solidHeader = TRUE,
            title = "Results",
            status = "primary",
            reactableOutput("results")
            ), 
        tags$style(HTML(paste0("
                        #result .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        "))
                   )   
              )
    })
    
    w3 <- Waiter$new(
      id = "results",
      html = div(
        style="color:#990c1a;",
        spin_loaders(2, color = "#990c1a"),
        h4("Loading Player Results")
      ),
      color = "#FFFFFF"
    )
    
    output$results <- renderReactable({
      
################ ERROR HANDLING, getting player info #########################
      
      req(input$Pitcher)
      pitcher <- input$Pitcher
      team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
      hexcol <- teams$Color[teams$Abbrev == team]
      player <-  substr(pitcher, 0, str_length(pitcher)-6)
      ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
      
      performance %>% filter(ID == ID_) %>% pull(YEAR) -> year_check
      req(input$player_year)
      validate(
        need(input$player_year %in%  year_check, "")
      )
     
      performance %>% filter(ID == ID_) %>% 
        filter(YEAR == input$player_year) %>% 
        pull(ORGS) -> team_check
      validate(
        need(input$player_team %in% team_check, "")
      )
      
      performance %>% filter(ID == ID_) %>% 
        filter(YEAR == input$player_year) %>% 
        filter(ORGS == input$player_team) %>% 
        pull(LVL) -> levels_check
      req(input$player_lvl)
      validate(
        need(input$player_lvl %in%  levels_check, "")
      )
      
      w3$show()
      Sys.sleep(.4)
      
#############^^^^^^^^^^ ERROR HANDLING, getting player info ^^^^^^######################
      
      data %>% filter(ID == ID_) %>% 
        filter(YEAR == 2022) -> choices_
      
      if(nrow(choices_) < 1)
      {
        return(NULL)
      }
      
      year <- input$player_year
      team <- input$player_team
      lvl <- input$player_lvl

      performance %>% 
        filter(LVL == lvl) %>%
          filter(YEAR == year) %>%
            mutate("era_ntile" = cume_dist(desc(ERA))) %>%
              mutate("BB_ntile" = cume_dist(desc(BB_RATE))) %>%
                mutate("SO_ntile" = cume_dist((SO_RATE))) %>%
                  mutate("swm_ntile" = cume_dist(SWM)) %>%
                    mutate("ch_ntile" = cume_dist(CHASERATE)) %>%
                      filter(ID == ID_) %>% 
                        filter(ORGS == team)-> use
      
      use %>% select(G, ERA, BB_RATE, SO_RATE, SWM, CHASERATE) -> perf
   
      if(nrow(perf) > 1)
      {
        use %>% filter(G == max(G)) -> use 
        perf %>% filter(G == max(G)) %>% select(ERA, BB_RATE, SO_RATE, SWM, CHASERATE) -> perf
      }
      
      df <- data.frame(
        "st" = c("ERA", "BB%", "SO%", "SWM%", "CH%"),
        "va" = c(
          paste(" ", round(perf$ERA, 2), " ", sep = ""), 
          paste(round(perf$BB_RATE * 100, 1),"%", sep = ""), 
          paste(round(perf$SO_RATE * 100, 1), "%", sep = ""), 
          paste(round(perf$SWM, 1),"%", sep = ""),
          paste(round(perf$CHASERATE , 1), "%", sep = "")
           ),
        plot = round(c(use$era_ntile, use$BB_ntile, use$SO_ntile , use$swm_ntile, use$ch_ntile)*100)
      )
      
      df_col <- data.frame(
      ntile = round(c(use$era_ntile, use$BB_ntile, use$SO_ntile , use$swm_ntile, use$ch_ntile)*100),
      col = c(BuYlRd(use$era_ntile), BuYlRd(use$BB_ntile), BuYlRd(use$SO_ntile), BuYlRd(use$swm_ntile), BuYlRd(use$ch_ntile))
      )
      df_col <- df_col[order(df_col[,1]),]
      
      gas <- df %>% 
        reactable(
          columns = list(
            st = colDef(name = " "),
            va = colDef(name = " "),
            plot = colDef(name = " ",
              cell = data_bars(., text_size = 13, 
                          box_shadow = TRUE,
                          text_position = 'center',
                          force_outside = c(0,27),
                          min_value = 0,
                          max_value = 100,
                          fill_color = df_col$col,
                 )
               )
            )
          )
      return(gas)
    
    })
    
################################# CONTACT BOX ##########################################################
    output$contact_box <- renderUI({
      
      validate(
        need(input$Pitcher != "", "Please Select a Pitcher.")
      )
      
      team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
      hexcol <- teams$Color[teams$Abbrev == team]
      
      tags$div(
        class = "another_one", id = "contact",
        box(width = 12,
            solidHeader = TRUE,
            title = "Contact",
            status = "primary",
            reactableOutput("contact")
            ), 
        tags$style(HTML(paste0("
                        #contact .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )   
      )
    })
    
    w2 <- Waiter$new(
      id = "contact",
      html = div(
        style="color:#990c1a;",
        spin_loaders(2, color = "#990c1a"),
        h4("Loading Player Contact")
      ),
      color = "#FFFFFF"
    )
      
    output$contact <- renderReactable({
      
################### ERROR HANDLING, getting player info #########################
      
      req(input$Pitcher)
      pitcher <- input$Pitcher
      team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
      hexcol <- teams$Color[teams$Abbrev == team]
      player <-  substr(pitcher, 0, str_length(pitcher)-6)
      ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
      
      performance %>% filter(ID == ID_) %>% pull(YEAR) -> year_check
      req(input$player_year)
      validate(
        need(input$player_year %in%  year_check, "")
      )
      
      performance %>% filter(ID == ID_) %>% 
        filter(YEAR == input$player_year) %>% 
        pull(ORGS) -> team_check
      validate(
        need(input$player_team %in% team_check, "")
      )
      
      performance %>% filter(ID == ID_) %>% 
        filter(YEAR == input$player_year) %>% 
        filter(ORGS == input$player_team) %>% 
        pull(LVL) -> levels_check
      req(input$player_lvl)
      validate(
        need(input$player_lvl %in%  levels_check, "")
      )
      
      w2$show()
      Sys.sleep(.4)
      
##############^^^^^^ ERROR HANDLING, getting player info ^^^^^^6###########
      
      data %>% filter(ID == ID_) %>% 
        filter(YEAR == 2022) -> choices_
      
      if(nrow(choices_) < 1)
      {
        return(NULL)
      }
      year <- input$player_year
      team <- input$player_team
      lvl <- input$player_lvl
      
     performance %>% 
       filter(LVL == lvl) %>%
        filter(YEAR == year) %>%
          mutate("babip_ntile" = cume_dist(desc(BABIP))) %>%
            mutate("LD_ntile" = cume_dist(desc(LD_RATE))) %>%
              mutate("GB_ntile" = cume_dist(GB_RATE)) %>%
                mutate("FB_ntile" = cume_dist(desc(FB_RATE))) %>%
                  mutate("hrfb_ntile" = cume_dist(HR_FB)) %>%
                    mutate("Nitro_ntile" = cume_dist(desc(NITRORATE))) %>%
                      mutate("ev_ntile" = cume_dist(desc(EV_70))) %>%
                        filter(ID == ID_) %>% 
                           filter(ORGS == team)-> use


      use %>% select(G, BABIP, LD_RATE, GB_RATE, FB_RATE, HR_FB, NITRORATE, EV_70) -> perf
      
      if(nrow(perf) > 1)
      {
        use %>% filter(G == max(G)) -> use 
        perf %>% filter(G == max(G)) %>% select(BABIP, LD_RATE, GB_RATE, FB_RATE, HR_FB, NITRORATE, EV_70) -> perf
      }

      df <- data.frame(
        "st" = c("BABIP", "LD%", "GB%", 
                 "FB%", "HR/FB", "NITRO%", 
                 "EV70"),
        "va" = c(
          ifelse(is.na(perf$BABIP), NA, round(perf$BABIP,3) ),
          ifelse(is.na(perf$LD_RATE), NA, paste(round(100*perf$LD_RATE,1),"%",sep = "") ),
          ifelse(is.na(perf$GB_RATE), NA, paste(round(100*perf$GB_RATE,1), "%",sep = "") ),
          ifelse(is.na(perf$FB_RATE), NA, paste(round(100*perf$FB_RATE,1), "%", sep ="") ), 
          ifelse(is.na(perf$HR_FB), NA, paste(round(100*perf$HR_FB,1),"%", sep ="") ) ,  
          ifelse(is.na(perf$NITRORATE), NA, paste(round(perf$NITRORATE,1), "%", sep  = "") ),
          ifelse(is.na(perf$EV_70), NA, round(perf$EV_70,1))
        ),
        plot = round(c(
          ifelse(is.na(use$babip_ntile), NA,  use$babip_ntile*100),
          ifelse(is.na(use$LD_ntile)   , NA, use$LD_ntile*100),
          ifelse(is.na(use$GB_ntile)   , NA, use$GB_ntile*100),
          ifelse(is.na(use$FB_ntile)   , NA, use$FB_ntile*100),
          ifelse(is.na(use$hrfb_ntile) , NA, use$hrfb_ntile*100),
          ifelse(is.na(use$Nitro_ntile), NA, use$Nitro_ntile*100),
          ifelse(is.na(use$ev_ntile)   , NA, use$ev_ntile*100)
          ))
      )
      df_col <- data.frame(
        ntile = round(c(use$babip_ntile, use$LD_ntile, use$GB_ntile , use$FB_ntile, use$hrfb_ntile,
                         use$Nitro_ntile, use$ev_ntile)*100),
        col = c(BuYlRd(use$babip_ntile), BuYlRd(use$LD_ntile), BuYlRd(use$GB_ntile), BuYlRd(use$FB_ntile), BuYlRd(use$hrfb_ntile),
                BuYlRd(use$Nitro_ntile),BuYlRd(use$ev_ntile))
      )
      df_col <- df_col[order(df_col[,1]),]
      
      gas <- df %>% 
        reactable(
          columns = list(
            st = colDef(name = " "),
            va = colDef(name = " "),
            plot = colDef(name = " ",
                          cell = data_bars(., text_size = 13, 
                                           # fill_color = viridis(5), 
                                           box_shadow = TRUE,
                                           text_position = 'center',
                                           min_value = 0,
                                           max_value = 100,
                                           force_outside = c(0,27),
                                           fill_color = df_col$col,
                                          )
                              )
                  )
        )
     
      return(gas)
      
    })
    
    
######################################### PERFORMANCE BOX ########################################################
    
    output$performace_box <- renderUI({
     
      validate(
        need(input$Pitcher != "", "")
      )
      
      team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
      hexcol <- teams$Color[teams$Abbrev == team]
      
      tags$div(
        class = "another_one", id = "performance",
        box(width = 12,
            solidHeader = TRUE,
            title = "Performace",
            status = "primary",
            reactableOutput("performance")
        ), 
        tags$style(HTML(paste0("
                        #performance .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )   
      )
      
    })
    
    w <- Waiter$new(
      id = "performance",
      html = div(
        style="color:#990c1a;",
        spin_loaders(2, color = "#990c1a"),
        h4("Loading Player Performance")
      ),
      color = "#FFFFFF"
    )

    output$performance <- renderReactable({
      
################################ ERROR HANDLING, getting player info ################
      
      req(input$Pitcher)
      pitcher <- input$Pitcher
      team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
      hexcol <- teams$Color[teams$Abbrev == team]
      player <-  substr(pitcher, 0, str_length(pitcher)-6)
      ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
     
      performance %>% filter(ID == ID_) %>% pull(YEAR) -> year_check
      req(input$player_year)
      validate(
        need(input$player_year %in%  year_check, "")
      )
      
      performance %>% filter(ID == ID_) %>% 
          filter(YEAR == input$player_year) %>% 
            pull(ORGS) -> team_check
      validate(
        need(input$player_team %in% team_check, "")
      )
      
      performance %>% filter(ID == ID_) %>% 
                        filter(YEAR == input$player_year) %>% 
                           filter(ORGS == input$player_team) %>% 
                             pull(LVL) -> levels_check
      req(input$player_lvl)
      validate(
        need(input$player_lvl %in%  levels_check, "")
      )
      
      w$show()
      Sys.sleep(.4)
      
################################ ERROR HANDLING, getting player info ################
      
      data %>% filter(ID == ID_) %>% 
        filter(YEAR == 2022) -> choices_
      
      if(nrow(choices_) < 1)
      {
        return(NULL)
      }
      year <- input$player_year
      team <- input$player_team
      lvl <- input$player_lvl
      
      performance %>% 
        filter(LVL == lvl) %>%
          filter(YEAR == year) %>%
            mutate("fip_ntile" = cume_dist(desc(FIP))) %>%
              mutate("wOBA_ntile" = cume_dist(desc(WOBA))) %>%
                filter(ID == ID_) %>% 
                  filter(ORGS == team)-> use
  
      use %>% select(G, GS, IP, WOBA, wOBA_ntile, FIP, fip_ntile) -> perf

      if(nrow(perf) > 1)
      {
        use %>% filter(G == max(G)) -> use 
        perf %>% filter(G == max(G)) %>% select(G, GS, IP, WOBA,  FIP) -> perf
      }
      
      df <- data.frame(
        "st" = c("G", "GS", "IP", "WOBA",   "FIP"),
        "va" = c(round(perf$G), round(perf$GS), perf$IP, 
                 round(perf$WOBA,3),  round(perf$FIP,2)
        ),
        plot = round(c(0,0,0 , use$wOBA_ntile, 
                       use$fip_ntile)*100)
      )
      
      df_col <- data.frame(
        ntile = round(c(0,0,0 , use$wOBA_ntile,
                        use$fip_ntile)*100),
        col = c("#FFFFFF","#FFFFFF","#FFFFFF", BuYlRd(use$wOBA_ntile), 
                BuYlRd(use$fip_ntile))
      )
      df_col <- df_col[order(-df_col[,1]),]
      
      gas <- df %>% 
        reactable(
          columns = list(
            st = colDef(name = " "),
            va = colDef(name = " "),
            plot = colDef(name = " ",
                          cell = data_bars(., text_size = 0, 
                                           # fill_color = viridis(5), 
                                           box_shadow = TRUE,
                                           min_value = 1,
                                           max_value = 100,
                                           text_position = 'center',
                                           force_outside = c(0.2,27),
                                           fill_color = rev(df_col$col),
                          )
                        )
                      )
                  )
      
      
      return(gas)
      
    })
  
  
########################################################### FIRST PANEL DOWN BELOW #########################################################################  
########################################################### FIRST PANEL DOWN BELOW #########################################################################  
########################################################### FIRST PANEL DOWN BELOW #########################################################################  
########################################################### FIRST PANEL DOWN BELOW #########################################################################  
  
  ######################## RANKINGS ##########################################################
    
    output$rankings <- renderUI({
      
      validate(
        need(input$Pitcher != "", "")
      )
      
      team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
      hexcol <- teams$Color[teams$Abbrev == team]
      player <-  substr(input$Pitcher, 0, str_length(input$Pitcher)-6)
      ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
      pipe <- player_rank(ID_)$pipe
      if(rapportools::is.empty(pipe))
        pipe = "NR"
      fg <- player_rank(ID_)$fg
      if(rapportools::is.empty(fg))
        fg = "NR"
      ba <- player_rank(ID_)$ba
      if(rapportools::is.empty(ba))
        ba = "NR"
      bp <- player_rank(ID_)$bp
      if(rapportools::is.empty(bp))
        bp = "NR"
      avg <- round(player_rank(ID_)$mean,1)
      if(rapportools::is.empty(avg))
        avg = "NA"
      
      
      tags$div(
      class = "another_one", id = "rankings",
        box(width = 12,
          solidHeader = TRUE,
          title = "Prospect Ranking",
          status = "primary",
          column(12, align = 'center',
                 box(width = 3, 
                     solidHeader = TRUE,
                     status = "danger",
                     title = "PL", 
                     pipe), #box
                 box(width = 3, 
                     solidHeader = TRUE,
                     status = "success",
                     title = "FG",
                     fg), #box
                 box(width = 3, 
                     solidHeader = TRUE,
                     status = "info",
                     title = "BP",
                     bp), #box
                 box(width = 3, 
                     solidHeader = TRUE,
                     status = "warning",
                     title = "BA",
                     ba), #box
                 column(12, offset = 3, 
                        box(width = 6, align = 'center',
                            solidHeader = TRUE,
                            status = "orange",
                            title = "Average",
                           avg)# box
                        ) #col
          )# col
        ),  # box
      tags$style(HTML(paste0("
                        #rankings .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", hexcol,
                        "}

                        .box.box-solid.box-primary {
                        border-bottom-color:", hexcol,
                        "border-left-color:", hexcol,
                        "border-right-color:", hexcol,
                        "border-top-color:", hexcol,
                        "}

                        ") ) )   
      )
    })
  
  ###########################################################################################################
    
    
    ####################### FACE & BIO OF PITCHER ##########################################################
    output$bio <- renderUI({
      
      validate(
        need(input$Pitcher != "", "")
      )
      
      tags$div(class = "another-box", id = "bio",
               
              
                   column(12, align = 'center',
                          uiOutput('face'),
                          br(),
                          htmlOutput("biotext")
                   ) ## col
                ## box

            )
      
      })
      
################################# BIO  #################################################################################
    output$biotext <- renderText({
      
      validate(
        need(input$Pitcher != "", "")
      )
      
      player <-  substr(input$Pitcher, 0, str_length(input$Pitcher)-6)
      team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
      ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
      
      data %>% filter(ID == ID_) %>% 
        filter(LASTDAY == max(LASTDAY)) %>% 
          select(AWAY_COMPETITION_LEVEL_ABBREV) -> lvl
      bio_text %>% filter(PlayerID == ID_) -> him
      
      mnfa %>% filter(id == ID_) -> use
      
      paste0(
        '<font size=4> <b>', player, '</b></font>',
         '<br>',
         '<h4><b> Level: </b>', lvl$AWAY_COMPETITION_LEVEL_ABBREV, '</h4>',
         '<br>',
         '<h4><b>Age:</b> ', him$AgeDecimal, 
         ' | <b>Throws:</b> ', him$Throws,
         ' | <b>Height:</b> ', him$HeightFormatted,
         ' | <b>Weight:</b> ', him$Weight, '</h4>',
        '<br>',
        '<h4><b>MNFA Season: ', ifelse(lvl$AWAY_COMPETITION_LEVEL_ABBREV %in% "MLB", "NA (Debuted)",  use$MNFAseason), '</b></h4>',
        '<h4><b> On 40 man?: ', ifelse(use$on40manroster, "Yes", "No"), '</b></h4>'
       )
    })
   #################################################  FACE#########################################################################

      output$face <- renderUI({

        validate(
          need(input$Pitcher != "", "")
        )

      pitcher <- input$Pitcher
      player <-  substr(pitcher, 0, str_length(pitcher)-6)
      team <- substr(pitcher,  str_length(pitcher) - 3, str_length(pitcher)-1)
      ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
      mlbid <- data$MLBID[data$ID == ID_][1]

      lvls <- data %>% 
                filter(ID == ID_) %>%
                  select(AWAY_COMPETITION_LEVEL_ABBREV) %>%
                       mutate("rank" = ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "MLB", 1, 
                             ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "AAA", 2, 
                                    ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "AA", 3, 
                                           ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "A+", 4, 
                                                  ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "A", 5, 
                                                         ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "Rk", 6,
                                                                ifelse(AWAY_COMPETITION_LEVEL_ABBREV == "Rk-", 7, NA)))))))) %>%
                            unique()
                            
      main <- lvls[order(lvls$rank),]$AWAY_COMPETITION_LEVEL[1]
      
      if(main == "MLB")
      {
          link <- paste("https://midfield.mlbstatic.com/v1/people/", mlbid, "/spots/240?zoom=1.2", sep = "")
          tags$img(src = link,
                   height="50%",
                   width="50%")
      }else if(main == "Rk-" || main == "Rk")
      {
        link <- paste("https://i.ebayimg.com/images/g/B9sAAOSwf25iOdgH/s-l500.png")
        tags$img(src = link,
                 height=200,
                 width=250)
      }else{

          link <- paste("https://img.mlbstatic.com/mlb-photos/image/upload/c_fill,g_auto/w_180/v1/people/", mlbid, "/headshot/milb/current", sep = "")
            tags$img(src = link,
                   height=200,
                   width=150)
      }
    })
      
      
      output$TeamnPercs <- renderUI({
        
        validate(
          need(input$Pitcher != "", "")
        )

              column(12, 
                     uiOutput("Team"),
                     htmlOutput("22WAR"),
                     reactableOutput("projs"))
      })
      
  ##################################################### WARRRRRRRRRRRRR###################################################### 
      output$'22WAR' <- renderText({
        validate(
          need(input$Pitcher != "", "")
        )
        
        player <-  substr(input$Pitcher, 0, str_length(input$Pitcher)-6)
        team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
        ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
       
        if("MLB" %in% data$AWAY_COMPETITION_LEVEL_ABBREV[data$YEAR == 2022 & data$ID == ID_])
        {
          war %>% filter(TXR_PLAYER_ID == ID_) -> use_war
          projs %>% filter(playerid == ID_ & projyear == 2022) -> use_projs
          tot_war <- sum(use_war$WAR)

          if(nrow(use_projs) > 0)
          {
            l <- paste0('<br>',
                        '<font size=3> <b> 2022 WAR: ', round(tot_war,1), '</b></font>',
                        '<br>',
                        '<h4><em>Percentile Projections</em></h4>')
           
          }else{
          l <- paste0(
            '<br>,
              <font size=3> <b> 2022 WAR: ', round(tot_war,1), '</b></font>',
            '<br>')
          }
        }else
        {
          l <- paste(" ")
        }
      
        
})

 ###################################################### PROJECTIONS ##############################################################
      output$projs <- renderReactable({
        
        validate(
          need(input$Pitcher != "", "")
        )

        player <-  substr(input$Pitcher, 0, str_length(input$Pitcher)-6)
        team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
        ID_ <- as.numeric(data$ID[data$TEAM == team & data$NAME == player][1])
        
        
        if("MLB" %in% data$AWAY_COMPETITION_LEVEL_ABBREV[data$YEAR == 2022 & data$ID == ID_])
        {
          projs %>% filter(playerid == ID_ & projyear == 2022) -> use_projs
          
          if(nrow(use_projs) == 0)
          {
            return(NULL)
            
          }else{
          war %>% filter(TXR_PLAYER_ID == ID_) -> use_war
          tot_war <- sum(use_war$WAR)
          tot_ip <- sum(use_war$PLAYER_IP)
          tw_p <- tot_war*use_projs$ip[use_projs$percentile == "pct20"]/tot_ip
          ft_p <- tot_war*use_projs$ip[use_projs$percentile == "pct50"]/tot_ip
          at_p <- tot_war*use_projs$ip[use_projs$percentile == "pct80"]/tot_ip
          
          
          df <- data.frame("TWth" = c(round(use_projs$war[use_projs$percentile == "pct20"],2),
                                      ifelse(tot_war > use_projs$war[use_projs$percentile == "pct20"],"Surpassed",
                                             ifelse(tw_p > use_projs$war[use_projs$percentile == "pct20"], "On Pace", "Not on Pace"
                                             )
                                          )
                                      ),
                           "FTth" = c(round(use_projs$war[use_projs$percentile == "pct50"],2),
                                      ifelse(tot_war > use_projs$war[use_projs$percentile == "pct50"],"Surpassed",
                                             ifelse(ft_p > use_projs$war[use_projs$percentile == "pct50"], "On Pace", "Not on Pace"
                                             )
                                      )
                           ),
                           "ATth" =  c(round(use_projs$war[use_projs$percentile == "pct80"],2),
                                       ifelse(tot_war > use_projs$war[use_projs$percentile == "pct80"],"Surpassed",
                                              ifelse(tw_p > use_projs$war[use_projs$percentile == "pct80"], "On Pace", "Not on Pace"
                                              )
                                       )
                           )
)
          cur <- use_war$WAR
                   
          df %>% 
            reactable(
              columns = list(
                TWth = colDef(name = "20th", align = 'center',
                              cel = function(value)
                              {
                                 if(value == "Not on Pace")
                                 {
                                   paste(value, "\u274c")
                                 }else if(value == "On Pace" | value == "Surpassed"){
                                   paste(value, "\u2714\ufe0f")
                                 }else{
                                   paste(value)
                                 }
                              }
                          ),
                FTth = colDef(name = "50th", align = 'center',
                              cel = function(value)
                              {
                                if(value == "Not on Pace")
                                {
                                  paste(value, "\u274c")
                                }else if(value == "On Pace" | value == "Surpassed"){
                                  paste(value, "\u2714\ufe0f")
                                }else{
                                  paste(value)
                                }
                              }
                ),
                ATth = colDef(name = "80th",align = 'center',
                              cel = function(value)
                              {
                                if(value == "Not on Pace")
                                {
                                  paste(value, "\u274c")
                                }else if(value == "On Pace" | value == "Surpassed"){
                                  paste(value, "\u2714\ufe0f")
                                }else{
                                  paste(value)
                                }
                              }
                           )
                        )
                   )
          }
        }else{
          return(NULL)
        }
      })
  
################################################## TEAM LOGO#####################################################################
      output$Team <- renderUI({
        
       validate(
          need(input$Pitcher != "", "")
        )
        
        team <- substr(input$Pitcher,  str_length(input$Pitcher) - 3, str_length(input$Pitcher)-1)
        num <- teams$number[teams$Abbrev == team]
        
        link <- paste("https://www.mlbstatic.com/team-logos/team-cap-on-light/", num, ".svg", sep ="")
        tags$img(src = link,
                 height=220, 
                 width=220)
      })

}

# Run the application 
shinyApp(ui = ui, server = server)
