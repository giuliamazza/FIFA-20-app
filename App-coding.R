#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

########################################### LIBRARIES ####################################################                    

library(tidyverse)
library(plotly)
library(ggrepel)
library(cowplot)
library(pipeR)
library(dplyr)
library(gapminder)
library(gifski)
library(fmsb)
library(ggiraphExtra)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(leaflet)
library(data.table)
library(formattable)
library(tidyr)
library(gganimate)
library(DT)
library(googleVis)
library(leaflet)
library(maps)


#loading the datasets

load("players_20.RData")
load("players_tot.RData")

#fixing a couple of variables

players_tot$short_name[players_tot$short_name == "Neymar"] <- "Neymar Jr"  # in the first dataset the name of Neymar was different, so I had to make it the same in the whole dataset

x <- as.factor(players_20$team_position)    # there are too many positions in the dataset, so I decided to group them
levels(x) <- list(GK  = c("GK"), 
                  DEF = c("LWB", "LB", "CB", "RB", "RWB", "RDM", "LDM", "RCB", "LCB"), 
                  MID = c("LW","LM","CDM","CM","CAM","RM","RW", "LAM", "RAM" , "LCM", "RCM"), 
                  FWD = c("CF", "ST", "RS", "LS", "LF", "RF"))
players_20 <- mutate(players_20, team_position = x)

overall_top_3 <- top_n(players_20, 3, overall)   #creating a smaller dataset for the top 3 players overall

########################################### UI ####################################################                    

title1 <- (tags$a(tags$img(src="fifa-ea-sports.png", height = '50')))


ui<- dashboardPage(dashboardHeader(title = title1,
                                   tags$li(class="dropdown",tags$a(href="http://bit.ly/giuliamazza-linkedin" ,icon("linkedin"), "My Profile", target="_blank")),
                                   tags$li(class="dropdown",tags$a(href="https://www.kaggle.com/stefanoleone992/fifa-20-complete-player-dataset", icon("kaggle"), "Source Code", target="_blank"))),
                   
                   dashboardSidebar(radioButtons("type_filter", "What do you want to analyse?", choices = list("Top 3 Players" = "top_3_tab",
                                                                                                               "Single Player" = "single_player_tab",
                                                                                                               "Players comparison" = "player_comp",
                                                                                                               "Style Analysis per Club" = "style_club_tab",
                                                                                                               "Style Analysis per Country" = "style_country_tab"
                   )),
#UI SIDEBAR
                   conditionalPanel(
                     condition = "input.type_filter == 'top_3_tab'", 
                     sidebarMenuOutput("top_3_filters_role"),
                     sidebarMenuOutput("top_3_filters_country")
                   ),
                   conditionalPanel(
                     condition = "input.type_filter == 'single_player_tab'", 
                     sidebarMenuOutput("single_player_filters")
                   ),
                   conditionalPanel(
                     condition = "input.type_filter == 'player_comp'", 
                     sidebarMenuOutput("player1_filters"),
                     sidebarMenuOutput("player2_filters")
                   ),
                   conditionalPanel(
                     condition = "input.type_filter == 'style_club_tab'", 
                     sidebarMenuOutput("style_filters_club")
                   ),
                    conditionalPanel(
                      condition = "input.type_filter == 'style_country_tab'"
                    )
                   
                   ),
# UI BODY
                   dashboardBody(fixedPage(
########################################### TOP 3 ####################################################                    
                     
                     conditionalPanel(condition = "input.type_filter == 'top_3_tab'", 
                                      
                                      fluidRow (column(3, ""),
                                                column(6,box(align = "center", width = 15, h2("TOP PLAYERS OVERALL"),
                                                    # TOP 3 OVERALL
                                                    fluidRow(
                                                      column(4, align = 'center', 
                                                             h3(strong("#1")), 
                                                             h4(overall_top_3$short_name[1]), 
                                                             plotOutput("overall_top_1_barstack", height = 30),
                                                             style="text-align:center"),
                                                      
                                                      column(4, align = 'center', 
                                                             h3(strong("#2")),
                                                             h4(overall_top_3$short_name[2]), 
                                                             plotOutput("overall_top_2_barstack", height = 30),
                                                             style="text-align:center"),
                                                      
                                                      
                                                      column(4, align = 'center', 
                                                             h3(strong("#3")), 
                                                             h4(overall_top_3$short_name[3]), 
                                                             plotOutput("overall_top_3_barstack", height = 30),
                                                             style="text-align:center")
                                                    ))),
                                                column(3, "")),
                                      
                                      fluidRow(box(align = "center", width = 15, h2(textOutput("title")),
                                               # TOP 3 FILTERED
                                               fluidRow(
                                                 column(4, align = 'center', h2(strong("#1")), 
                                                        h2(textOutput("name1")), 
                                                        textOutput("overall_top_1_info"),
                                                        plotOutput("top_1_barstack", height = 40),
                                                        plotlyOutput("overall_top_1_radar", height = 200),
                                                        style="background-color"),
                                                 
                                                 column(4, align = 'center', 
                                                        h2(strong("#2")), 
                                                        h2(textOutput("name2")),  
                                                        textOutput("overall_top_2_info"),
                                                        plotOutput("top_2_barstack", height = 40),
                                                        plotlyOutput("overall_top_2_radar", height = 200),
                                                        style="text-align:center"),
                                                 
                                                 
                                                 column(4, align = 'center', 
                                                        h2(strong("#3")), 
                                                        h2(textOutput("name3")),  
                                                        textOutput("overall_top_3_info"),
                                                        plotOutput("top_3_barstack", height = 40),
                                                        plotlyOutput("overall_top_3_radar", height = 200),
                                                        style="text-align:center")
                                               )
                                      ))
                     ),
########################################### SINGLE PLAYER ####################################################                    

                     conditionalPanel(condition = "input.type_filter == 'single_player_tab'",
                                      fluidRow(h2("SINGLE PLAYER STATS"),
                                               fluidRow(
                                                 column(5, 
                                                        fluidRow( box( width = NULL, 
                                                                       column(4, align = 'center',
                                                                              div(style = "height: 140px", h1(textOutput("single_player_tshirt"), icon("tshirt")))),
                                                                       column(8,
                                                                              fluidRow(column(12, align = 'center',
                                                                                              div(style = "height: 60px",
                                                                                                  h1(textOutput("single_player_name")))
                                                                              )), 
                                                                              fluidRow(column(6, align = 'left',
                                                                                              div(style = "height: 80px", 
                                                                                                  fluidRow("Club: "),
                                                                                                  fluidRow("Nationality: "),
                                                                                                  fluidRow("Age: "),
                                                                                                  fluidRow("Position: ")
                                                                                              )
                                                                                        ),
                                                                                        column(6, align = 'right',
                                                                                               div(style = "height: 80px",
                                                                                                   textOutput("single_player_club"),
                                                                                                   textOutput("single_player_nationality"),
                                                                                                   textOutput("single_player_age"),
                                                                                                   textOutput("single_player_position")
                                                                                               )
                                                                                               ))
                                                                              ))),
                                                        fluidRow( tabBox(title = 'Abilities', width = '300px',
                                                                         tabPanel(
                                                                           "Attacking", plotlyOutput("single_player_attacking_rad", 
                                                                                                     height = 250), "This graph allows you to understand better the ability of the player divided per category."), 
                                                                         tabPanel(
                                                                           "Skill", plotlyOutput("single_player_skill_rad", 
                                                                                                 height = 250), "This graph allows you to understand better the ability of the player divided per category."), 
                                                                         tabPanel("Movement", plotlyOutput("single_player_movement_rad", 
                                                                                                           height = 250), "This graph allows you to understand better the ability of the player divided per category."), 
                                                                         tabPanel("Power", plotlyOutput("single_player_power_rad", 
                                                                                                        height = 250), "This graph allows you to understand better the ability of the player divided per category."), 
                                                                         tabPanel("Mentality", plotlyOutput("single_player_mentality_rad", 
                                                                                                            height = 250), "This graph allows you to understand better the ability of the player divided per category."), 
                                                                         tabPanel("Defending", plotlyOutput("single_player_defending_rad", 
                                                                                                            height = 250), "This graph allows you to understand better the ability of the player divided per category."), 
                                                                         tabPanel("Goalkeeping", plotlyOutput("single_player_goalkeeping_rad", 
                                                                                                              height = 250), "This graph allows you to understand better the ability of the player divided per category.")
                                                        )
                                                        )
                                                 ),
                                                 column(7,box( width = 14,
                                                        fluidRow(column(6, plotlyOutput("single_player_overtime", height = 250)), 
                                                                 column(6, formattableOutput("timeline_table", height = 225))),
                                                        fluidRow(column(8, plotlyOutput("single_player_wagevalue", height = 320)),
                                                                 column(4,
                                                                        fluidRow(div(style = "height:100px; width:100px; background-color: white;", "")),
                                                                        fluidRow("This graph makes a comparison between the value of the player and how much he gets paid."),
                                                                        fluidRow("Use the hover to see how the other players are doing."),
                                                                        fluidRow("Did you see how much Oblak is paid compared to his value???")))
                                                 ))
                                               ))
                                               ),
########################################### PLAYERS COMPARISON ####################################################                    

                     conditionalPanel(condition = "input.type_filter == 'player_comp'",
                                      fluidRow(h2("PLAYERS COMPARISON")),
                                      fluidRow(column(6, box( width = NULL, 
                                                              column(5, align = 'center',
                                                                     div(style = "height: 200px", h1(textOutput("player1_tshirt")))
                                                              ),
                                                              column(7,
                                                                     fluidRow(column(12, align = 'center',
                                                                                     div(style = "height: 100px",
                                                                                         h1(textOutput("player1_name"))
                                                                                     ))),
                                                                     fluidRow(column(6, align = 'left',
                                                                                     div(style = "height: 80px", 
                                                                                         fluidRow("Club: "),
                                                                                         fluidRow("Nationality: "),
                                                                                         fluidRow("Age: "),
                                                                                         fluidRow("Position: ")
                                                                                     )
                                                                     ),
                                                                     column(6, align = 'right',
                                                                            div(style = "height: 80px",
                                                                                textOutput("player1_club"),
                                                                                textOutput("player1_nationality"),
                                                                                textOutput("player1_age"),
                                                                                textOutput("player1_Position")
                                                                            )
                                                                     ))
                                                                     ))
                                      ),
                                      column(6, box( width = NULL, 
                                                     column(5, align = 'center',
                                                            div(style = "height: 200px", h1(textOutput("player2_tshirt")))
                                                     ),
                                                     column(7,
                                                            fluidRow(column(12, align = 'center',
                                                                            div(style = "height: 100px",
                                                                                h1(textOutput("player2_name"))
                                                                            ))),
                                                            fluidRow(column(6, align = 'left',
                                                                            div(style = "height: 80px", 
                                                                                fluidRow("Club: "),
                                                                                fluidRow("Nationality: "),
                                                                                fluidRow("Age: "),
                                                                                fluidRow("Position: ")
                                                                            )
                                                            ),
                                                            column(6, align = 'right',
                                                                   div(style = "height: 80px",
                                                                       textOutput("player2_club"),
                                                                       textOutput("player2_nationality"),
                                                                       textOutput("player2_age"),
                                                                       textOutput("player2_Position")
                                                                   )
                                                            ))
                                                            ))
                                      )
                                      ),
                                      fluidRow(tabBox(title = 'Abilities', width = '300px',
                                                      tabPanel("Attacking", plotlyOutput("player_comp_attacking_rad", 
                                                                                         height = 250)), 
                                                      tabPanel("Skill", plotlyOutput("player_comp_skill_rad", 
                                                                                     height = 250)), 
                                                      tabPanel("Movement", plotlyOutput("player_comp_movement_rad", 
                                                                                        height = 250)), 
                                                      tabPanel("Power", plotlyOutput("player_comp_power_rad", 
                                                                                     height = 250)), 
                                                      tabPanel("Mentality", plotlyOutput("player_comp_mentality_rad", 
                                                                                         height = 250)), 
                                                      tabPanel("Defending", plotlyOutput("player_comp_defending_rad", 
                                                                                         height = 250)), 
                                                      tabPanel("Goalkeeping", plotlyOutput("player_comp_goalkeeping_rad", 
                                                                                           height = 250))
                                      ) 
                                      
                                      )
                     ),

###########################################STYLE ANALYSIS CLUB ####################################################                    
                     conditionalPanel(condition = "input.type_filter == 'style_club_tab'",
                                      fluidRow(h2("STYLE ANALYSIS PER CLUB")),
                                      fluidRow(infoBoxOutput("overall_club_infobox"), 
                                               infoBoxOutput("aggressive_club_infobox"), 
                                               infoBoxOutput("dribbling_club_infobox")),
                                      fluidRow( plotOutput("style_club_analysis"))
                                      
                     ),

###########################################STYLE ANALYSIS COUNTRY ####################################################                    


                    conditionalPanel(condition = "input.type_filter == 'style_country_tab'",
                                     fluidRow(h2("STYLE ANALYSIS PER COUNTRY")),
                                     tabBox(width = 15, tabPanel("Overall", htmlOutput("mapOVERALL", width = 1200, height = 600)),
                                            tabPanel("Passing", htmlOutput("mapPASSING", width = 1200, height = 600)),
                                            tabPanel("Dribbling", htmlOutput("mapDRIBBLING")),
                                            tabPanel("Deefending", htmlOutput("mapDEFENDING")),
                                            tabPanel("Physic", htmlOutput("mapPHYSIC")),
                                            tabPanel("Sprint", htmlOutput("mapSPRINT")),
                                            tabPanel("Strenght", htmlOutput("mapSTRENGHT")),
                                            tabPanel("Aggression", htmlOutput("mapAGGRESSION")),
                                            tabPanel("Penalties", htmlOutput("mapPENALTIES")),
                                            tabPanel("Marking", htmlOutput("mapMARKING"))
                                     ),
                                     dataTableOutput("country_style_table")

)
                   )
                   )
)



########################################### server ####################################################                    


server <- function(input, output){
  overall_top_3 <- top_n(players_20, 3, overall)
  
  
  
  ############################################ TOP PLAYERS TAB#############################################
  top_by_role <- reactive({                
    players_20 %>%                 
      filter(team_position %in% as.character(input$role), nationality %in% as.character(input$country)) %>%
      arrange(desc(overall))
  }) 
  
  #SIDEBAR
  output$top_3_filters_role <- renderMenu({
    top_by_role <- selectInput("role", "Select role", choices = sort(players_20$team_position), selected = "DEF", multiple = TRUE)
  })
  
  output$top_3_filters_country <- renderMenu({ 
    top_by_role <- selectInput("country", "Select nationality of the player", choices = players_20$nationality, selected = c("Argentina", "Portugal"), multiple = TRUE)
  })
  

  #OVERALL
  #TITLE 
output$title <- renderText(
  print(paste("Best ", paste(input$role, collapse = ", "), "in", paste(input$country, collapse =", ")))
)
  #OVERALL.BARSTACKS
  
  # function
  static_barstack <- function(x){
    ggplot(data = overall_top_3[x,], aes(x = short_name, y = overall), xaxs = 0, yaxs = 100) + 
      annotate("rect", ymin = 0, ymax = 0.01, xmin = (100 - overall_top_3$overall[x]), xmax = 100,
               alpha = 0.7) +
      annotate("rect", ymin = 0, ymax = 0.01, xmin = 0, xmax = (overall_top_3$overall[x]), fill = "blue") +
      annotate("text", y = 0.006, x = 50, label = overall_top_3$overall[x], color = "white") +
      
      # removing unnecessary elements from the graph
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank()
      )}
  #graphs
  output$overall_top_1_barstack <- renderPlot({static_barstack(1)}, bg ="#dedede")
  output$overall_top_2_barstack <- renderPlot({static_barstack(2)}, bg ="#dedede")
  output$overall_top_3_barstack <- renderPlot({static_barstack(3)}, bg ="#dedede")
  
  #FILTERED
  #NAMES 
  output$name1 <- renderText(top_by_role()$short_name[1])
  output$name2 <- renderText(top_by_role()$short_name[2])
  output$name3 <- renderText(top_by_role()$short_name[3])
  
  ##INFO
  output$overall_top_1_info <- renderText({paste(top_by_role()$club[1], ", ", top_by_role()$team_position[1])})
  output$overall_top_2_info <- renderText({paste(top_by_role()$club[2], ", ", top_by_role()$team_position[2])})
  output$overall_top_3_info <- renderText({paste(top_by_role()$club[3], ", ", top_by_role()$team_position[3])})
  
  ##BARSTACK
  #function
  filtered_barstack <- function(x){
    ggplot(data = top_by_role()[x,], aes(x = short_name, y = overall), xaxs = 0, yaxs = 100) + 
      annotate("rect", ymin = 0, ymax = 0.01, xmin = (100 - top_by_role()$overall[x]), xmax = 100,
               alpha = 0.7) +
      annotate("rect", ymin = 0, ymax = 0.01, xmin = 0, xmax = (top_by_role()$overall[x]), fill = "blue") +
      annotate("text", y = 0.006, x = 50, label = top_by_role()$overall[x], color = "white", size = 9) +
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank()
      ) 
  }
  
  #graphs
  output$top_1_barstack <- renderPlot({filtered_barstack(1)})
  output$top_2_barstack <- renderPlot({filtered_barstack(2)})
  output$top_3_barstack <- renderPlot({filtered_barstack(3)})
  
  ##RADAR GENERAL
  #function 
  filtered_radar <- function(x){
    plot_ly( 
      type = 'scatterpolar',
      r = c(top_by_role()$pace[x], 
            top_by_role()$shooting[x], 
            top_by_role()$passing[x], 
            top_by_role()$dribbling[x], 
            top_by_role()$defending[x], 
            top_by_role()$physic[x],
            top_by_role()$pace[x]
      ),
      theta = c(colnames(top_by_role())[13],
                colnames(top_by_role())[14],
                colnames(top_by_role())[15], 
                colnames(top_by_role())[16], 
                colnames(top_by_role())[17],
                colnames(top_by_role())[18],
                colnames(top_by_role())[13]
      ),
      fill = 'toself'
    )}
  ##RADAR GK
  #function 
  filtered_radarGK <- function(x){
    plot_ly( 
      type = 'scatterpolar',
      r = c(top_by_role()$gk_diving[x], 
            top_by_role()$gk_handling[x], 
            top_by_role()$gk_kicking[x], 
            top_by_role()$gk_reflexes[x], 
            top_by_role()$gk_speed[x], 
            top_by_role()$gk_positioning[x],
            top_by_role()$gk_diving[x]
      ),
      theta = c('diving', 'handling','kicking','reflex','speed','positioning','diving'
      ),
      fill = 'toself'
    )%>%
      layout( 
        font = list(size = 10),
        polar = list(
          radialaxis = list(
            visible = F
          )
        ),
        showlegend = F
      )
      
    }
  #graphs
  output$overall_top_1_radar <- renderPlotly({
    if (top_by_role()$team_position[1] == "GK") {
      filtered_radarGK(1)
    } else {
      filtered_radar(1)  
    }
  })
  
  output$overall_top_2_radar <- renderPlotly({
    if (top_by_role()$team_position[2] == "GK") {
      filtered_radarGK(2)
    } else {
      filtered_radar(2)  
    }
  })
  
  output$overall_top_3_radar <- renderPlotly({
    if (top_by_role()$team_position[3] == "GK") {
      filtered_radarGK(3)
    } else {
      filtered_radar(3)  
    }
  })

  ############################################ SINGLE PLAYER TAB ####################################################
  single_player <- reactive({                
    players_20 %>%                 
      filter(short_name == as.character(input$player))
  })
  
  #SIDEBAR
  output$single_player_filters <- renderMenu( 
    single_player <- selectInput("player", "Select Player", choices = players_20$short_name, selected = "L. Messi"))
  
  ##TSHIRT NUMBER
  output$single_player_tshirt <- renderText(print(single_player()$team_jersey_number[1], font.size = 1000))
  ##NAME
  output$single_player_name <- renderText(single_player()$short_name[1])
  ##INFO
  output$single_player_club <- renderText(single_player()$club[1])
  output$single_player_nationality <- renderText(single_player()$nationality[1])
  output$single_player_age <- renderText(single_player()$age[1])
  output$single_player_position <- renderText(as.character(single_player()$team_position[1]))
  
  #RADAR
    #attacking
    output$single_player_attacking_rad <- renderPlotly({
      plot_ly( 
        type = 'scatterpolar',
        r = c(single_player()$attacking_crossing[1], 
              single_player()$attacking_finishing[1], 
              single_player()$attacking_heading_accurancy[1], 
              single_player()$attacking_short_passing[1], 
              single_player()$attacking_volleys[1], 
              single_player()$attacking_crossing[1]
        ),
        theta = c('crossing', 'finishing', 'head accurancy', 'short passing', 'volleys'),
        fill = 'toself', textfont=list(size=3)) %>%
        layout(
          title = strong('Attacking'), 
          font = list(
            size = 10),
          polar = list(
            radialaxis = list(
              visible = F,
              range = c(0,100)
            )
          ),
          showlegend = F
        )
    })
  #skill
  output$single_player_skill_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar',
      r = c(single_player()$skill_dribbling[1], 
            single_player()$skill_curve[1], 
            single_player()$skill_fk_accurancy[1], 
            single_player()$skill_long_passing[1], 
            single_player()$skill_ball_control[1], 
            single_player()$skill_dribbling[1]
      ),
      theta = c('dribbling', 'curve', 'fk accurancy', 'long passing', 'ball control'),
      fill = 'toself', textfont=list(size=3)) %>%
      layout(
        title = strong('Skill'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
  })
  #movement
  output$single_player_movement_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar',
      r = c(single_player()$movement_acceleration[1], 
            single_player()$movement_sprint_speed[1], 
            single_player()$movement_agility[1], 
            single_player()$movement_reactions[1], 
            single_player()$movement_balance[1], 
            single_player()$movement_acceleration[1]
      ),
      theta = c('acceleration', 'sprint speed', 'agility', 'reactions', 'balance', 'acceleration'),
      fill = 'toself', textfont=list(size=3)) %>%
      layout(
        title = strong('Movement'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
  })
  #power
  output$single_player_power_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar',
      r = c(single_player()$power_jumping[1], 
            single_player()$power_stamina[1], 
            single_player()$power_strength[1], 
            single_player()$power_long_shots[1], 
            single_player()$power_jumping[1]
      ),
      theta = c('jumping', 'stamina', 'strength', 'long shots', 'jumping'),
      fill = 'toself', textfont=list(size=3)) %>%
      layout(
        title = strong('Power'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
  })
  #mentality
  output$single_player_mentality_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', mode = 'lines', hoverinfo = "r+theta", 
      r = c(single_player()$mentality_aggression[1], 
            single_player()$mentality_interceptions[1], 
            single_player()$mentality_positioning[1], 
            single_player()$mentality_vision[1], 
            single_player()$mentality_penalities[1], 
            single_player()$mentality_composure[1], 
            single_player()$mentality_aggression[1]
      ),
      theta = c('aggression', 'interceptions', 'positioning', 'vision', 'penalities', 'composure'),
      fill = 'toself', textfont=list(size=3)) %>%
      layout(
        title = strong('Mentality'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
  })
  #defending
  output$single_player_defending_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar',
      r = c(single_player()$defending_marking[1], 
            single_player()$defending_standing_tackle[1], 
            single_player()$defending_sliding_tackle[1], 
            single_player()$defending_marking[1]
      ),
      theta = c('marking', 'standing tackle', 'sliding tackle', 'marking'),
      fill = 'toself', textfont=list(size=3)) %>%
      layout(
        title = strong('Defending'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
  })
  #goalkeeping
  output$single_player_goalkeeping_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar',
      r = c(single_player()$goalkeeping_diving[1], 
            single_player()$goalkeeping_handling[1], 
            single_player()$goalkeeping_kicking[1], 
            single_player()$goalkeeping_positioning[1], 
            single_player()$goalkeeping_reflexes[1], 
            single_player()$goalkeeping_diving[1]
      ),
      theta = c('diving', 'handling', 'kicking', 'positioning', 'reflexes', 'diving'),
      fill = 'toself', textfont=list(size=3)) %>%
      layout(
        title = strong('Goalkeeping'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
  })
  #GRAPH OVERTIME
  #data
  single_player_overtime_data <- reactive({                
    players_tot %>%                 
      filter(short_name == as.character(input$player))%>%
      select(short_name, year_reference, pace,passing,dribbling, defending, physic, gk_diving, gk_handling, gk_kicking, gk_reflexes, gk_speed, gk_positioning, overall)%>%
      gather( key = ability, value = value, -year_reference, -short_name)
  })
  
  output$single_player_overtime <- renderPlotly({
    
    p <- ggplot(single_player_overtime_data(), aes(x = year_reference, y = value, group = ability)) + 
      geom_line(color = "grey") + geom_point(size = 0.5, color = "grey")+ 
      geom_line(data = single_player_overtime_data() %>% filter(ability == "overall"), color = "red")+
      theme(legend.title = element_text(size = 7), 
            legend.text = element_text(size = 7)) +
      xlab("") + ylab("") +
      ggtitle("Ability Timeline")
    ggplotly(p)
  })
  
  #TABLE TIMELINE
  #data
  timeline_data <- reactive({             #creating the reactive data for the table    
      players_tot %>%                 
      filter(short_name == input$player, year_reference %in% c(2015, 2019,2020))%>%
      select(c(year_reference, pace,passing,dribbling, defending, physic, overall))%>%
      gather(key = ability, value = value, - year_reference) %>%
      spread(key = year_reference, value = value)%>%
      mutate (`Improvement 2015-2020 (%)` =  round((`2020`-`2015`)/`2015`*100,2),
              `Improvement 2019-2020 (%)` =  round((`2020`-`2019`)/`2019`*100,2))%>%
      select(ability, `Improvement 2015-2020 (%)`, `Improvement 2019-2020 (%)`)
  })
  
  #output
  improvement_formatter <- formatter("span",      # streating the style for the formatting of the table
                                     style = x ~ style(font.weight = "bold", 
                                                       color = ifelse(x > 0, "#71CA97", ifelse(x < 0, "#ff7f7f", "black"))), 
                                     x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
  )
  
  output$timeline_table <- renderFormattable({ 
    formattable(timeline_data(),
              align = c("l", "r", "r"),
              list(`ability` = formatter(
                "span", style = ~ style(color = "grey",font.weight = "bold"),
                `2015` = NULL, `2020` = NULL),
                `Improvement 2015-2020 (%)` = improvement_formatter,
                `Improvement 2019-2020 (%)` = improvement_formatter
              ))
  })
  


  # GRAPH VALUE-WAGE
  output$single_player_wagevalue <- renderPlotly({
    
    p <- ggplot(data = players_20, aes(x = wage_eur/1000, y = value_eur/1000,  text=short_name)) +
      geom_point(size = 0.3, color ="grey") +  
      geom_point(data = players_20 %>% filter(short_name == input$player), color = "red")  +
      labs(title = "Wage-Value comparison", x = "Wage (K euro)", y = "Value (K euro)")
    ggplotly(p, tooltip= c("text", "wage_eur/1000", "value_eur/1000"))
  })
  
  
  ############################################ PLAYERS COMPARISON ###################################################
  player1 <- reactive({                
    players_20 %>%                 
      filter(short_name == as.character(input$player1))
  })
  player2 <- reactive({                
    players_20 %>%                 
      filter(short_name == as.character(input$player2))
  })
  #SIDEBAR
  output$player1_filters <- renderMenu( 
    player1 <- selectInput("player1", "Select Player1", choices = players_20$short_name, selected = "L. Messi"))
  output$player2_filters <- renderMenu( 
    player2 <- selectInput("player2", "Select Player2", choices = players_20$short_name, selected = "Cristiano Ronaldo"))
  
  ##TSHIRT NUMBER
  output$player1_tshirt <- renderText(print(player1()$team_jersey_number[1], font.size = 100))
  output$player2_tshirt <- renderText(print(player2()$team_jersey_number[1], font.size = 100))
  ##NAME
  output$player1_name <- renderText(player1()$short_name[1])
  output$player2_name <- renderText(player2()$short_name[1])
  ##INFO
  output$player1_club <- renderText(player1()$club[1])
  output$player1_nationality <- renderText(player1()$nationality[1])
  output$player1_age <- renderText(player1()$age[1])
  output$player1_Position <- renderText(as.character(player1()$team_position[1]))
  output$player2_club <- renderText(player2()$club[1])
  output$player2_nationality <- renderText(player2()$nationality[1])
  output$player2_age <- renderText(player2()$age[1])
  output$player2_Position <- renderText(as.character(player2()$team_position[1]))
  
  #RADARS
  #attacking
  output$player_comp_attacking_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', hoverinfo = "r+theta",
      r = c(player1()$attacking_crossing[1], 
            player1()$attacking_finishing[1], 
            player1()$attacking_heading_accurancy[1], 
            player1()$attacking_short_passing[1], 
            player1()$attacking_volleys[1], 
            player1()$attacking_crossing[1]
      ),
      theta = c('crossing', 'finishing', 'head accurancy', 'short passing', 'volleys'), name = player1()$short_name,
      fill = 'toself', textfont=list(size=3))%>%
      add_trace(r =c(player2()$attacking_crossing[1], 
                     player2()$attacking_finishing[1], 
                     player2()$attacking_heading_accurancy[1], 
                     player2()$attacking_short_passing[1], 
                     player2()$attacking_volleys[1], 
                     player2()$attacking_crossing[1]
      ),
      theta = c('crossing', 'finishing', 'head accurancy', 'short passing', 'volleys'), 
      hoverinfo = "r+theta",
      name = player2()$short_name)%>%
      layout(
        title = strong('Attacking'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = T
      )
  })
  #skill
  output$player_comp_skill_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', hoverinfo = "r+theta",
      r = c(player1()$skill_dribbling[1], 
            player1()$skill_curve[1], 
            player1()$skill_fk_accurancy[1], 
            player1()$skill_long_passing[1], 
            player1()$skill_ball_control[1], 
            player1()$skill_dribbling[1]
      ),
      theta = c('dribbling', 'curve', 'fk accurancy', 'long passing', 'ball control'), name = player1()$short_name,
      fill = 'toself', textfont=list(size=3)) %>%
      add_trace(r = c(player1()$skill_dribbling[1], 
                      player2()$skill_curve[1], 
                      player2()$skill_fk_accurancy[1], 
                      player2()$skill_long_passing[1], 
                      player2()$skill_ball_control[1], 
                      player2()$skill_dribbling[1]
      ),
      theta = c('dribbling', 'curve', 'fk accurancy', 'long passing', 'ball control'), 
      hoverinfo = "r+theta",
      name = player2()$short_name) %>%
      layout(
        title = strong('Skill'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = T
      )
  })
  #movement
  output$player_comp_movement_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', hoverinfo = "r+theta",
      r = c(player1()$movement_acceleration[1], 
            player1()$movement_sprint_speed[1], 
            player1()$movement_agility[1], 
            player1()$movement_reactions[1], 
            player1()$movement_balance[1], 
            player1()$movement_acceleration[1]
      ),
      theta = c('acceleration', 'sprint speed', 'agility', 'reactions', 'balance', 'acceleration'), name = player1()$short_name,
      fill = 'toself', textfont=list(size=3)) %>%
      add_trace(r = c(player2()$movement_acceleration[1], 
                      player2()$movement_sprint_speed[1], 
                      player2()$movement_agility[1], 
                      player2()$movement_reactions[1], 
                      player2()$movement_balance[1], 
                      player2()$movement_acceleration[1]
      ),
      theta = c('acceleration', 'sprint speed', 'agility', 'reactions', 'balance', 'acceleration'), 
      hoverinfo = "r+theta",
      name = player2()$short_name) %>%
      layout(
        title = strong('Movement'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = T
      )
  })
  #power
  output$player_comp_power_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', hoverinfo = "r+theta",
      r = c(player1()$power_jumping[1], 
            player1()$power_stamina[1], 
            player1()$power_strength[1], 
            player1()$power_long_shots[1], 
            player1()$power_jumping[1]
      ),
      theta = c('jumping', 'stamina', 'strength', 'long shots', 'jumping'), name = player1()$short_name,
      fill = 'toself', textfont=list(size=3)) %>%
      add_trace(r = c(player2()$power_jumping[1], 
                      player2()$power_stamina[1], 
                      player2()$power_strength[1], 
                      player2()$power_long_shots[1], 
                      player2()$power_jumping[1]
      ),
      theta = c('jumping', 'stamina', 'strength', 'long shots', 'jumping'), 
      hoverinfo = "r+theta",
      name = player2()$short_name) %>%
      layout(
        title = strong('Power'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = T
      )
  })
  #mentality
  output$player_comp_mentality_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', hoverinfo = "r+theta", 
      r = c(player1()$mentality_aggression[1], 
            player1()$mentality_interceptions[1], 
            player1()$mentality_positioning[1], 
            player1()$mentality_vision[1], 
            player1()$mentality_penalities[1], 
            player1()$mentality_composure[1], 
            player1()$mentality_aggression[1]
      ),
      theta = c('aggression', 'interceptions', 'positioning', 'vision', 'penalities', 'composure'), name = player1()$short_name,
      fill = 'toself', textfont=list(size=3)) %>%
      add_trace(r = c(player2()$mentality_aggression[1], 
                      player2()$mentality_interceptions[1], 
                      player2()$mentality_positioning[1], 
                      player2()$mentality_vision[1], 
                      player2()$mentality_penalities[1], 
                      player2()$mentality_composure[1], 
                      player2()$mentality_aggression[1]
      ),
      theta = c('aggression', 'interceptions', 'positioning', 'vision', 'penalities', 'composure'),
      hoverinfo = "r+theta", 
      name = player2()$short_name)%>%
      layout(
        title = strong('Mentality'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = T
      )
  })
  #defending
  output$player_comp_defending_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', hoverinfo = "r+theta",
      r = c(player1()$defending_marking[1], 
            player1()$defending_standing_tackle[1], 
            player1()$defending_sliding_tackle[1], 
            player1()$defending_marking[1]
      ),
      theta = c('marking', 'standing tackle', 'sliding tackle', 'marking'), name = player1()$short_name,
      fill = 'toself', textfont=list(size=3)) %>%
      add_trace(r = c(player2()$defending_marking[1], 
                      player2()$defending_standing_tackle[1], 
                      player2()$defending_sliding_tackle[1], 
                      player2()$defending_marking[1]
      ),
      theta = c('marking', 'standing tackle', 'sliding tackle', 'marking'), hoverinfo = "r+theta", name = player2()$short_name)%>%
      layout(
        title = strong('Defending'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = T
      )
  })
  #goalkeeping
  output$player_comp_goalkeeping_rad <- renderPlotly({
    plot_ly( 
      type = 'scatterpolar', hoverinfo = "r+theta",
      r = c(player1()$goalkeeping_diving[1], 
            player1()$goalkeeping_handling[1], 
            player1()$goalkeeping_kicking[1], 
            player1()$goalkeeping_positioning[1], 
            player1()$goalkeeping_reflexes[1], 
            player1()$goalkeeping_diving[1]
      ),
      theta = c('diving', 'handling', 'kicking', 'positioning', 'reflexes', 'diving'), name = player1()$short_name,
      fill = 'toself', textfont=list(size=3)) %>%
      add_trace(r = c(player2()$goalkeeping_diving[1], 
                      player2()$goalkeeping_handling[1], 
                      player2()$goalkeeping_kicking[1], 
                      player2()$goalkeeping_positioning[1], 
                      player2()$goalkeeping_reflexes[1], 
                      player2()$goalkeeping_diving[1]
      ),
      theta = c('diving', 'handling', 'kicking', 'positioning', 'reflexes', 'diving'), hoverinfo = "r+theta", name = player2()$short_name)%>%
      layout(
        title = strong('Goalkeeping'), 
        font = list(
          size = 10),
        polar = list(
          radialaxis = list(
            visible = F,
            range = c(0,100)
          )
        ),
        showlegend = T
      )
  })
  
  ############################################ STYLE ANALYSIS CLUB #######################################################
  #SIDEBAR
  output$style_filters_club <- renderMenu( selectInput("club", "Select Clubs", 
                                                       choices = players_20$club, 
                                                       selected = c("Juventus", "Manchester City"), 
                                                       multiple = TRUE)) 

   #VALUE BOXES
  #overall
  style_club_analysis_overall_datareac <- reactive(
    players_20%>%
      filter(club %in% input$club)%>%
      group_by(club)%>%
      summarise(avg_overall = mean(overall), na.rm = TRUE)%>%
      arrange(desc(avg_overall))
  )
  output$overall_club_infobox <- renderInfoBox(
    infoBox(value = style_club_analysis_overall_datareac()$club[1], 
            title = "The best overall is ", 
            subtitle = paste("Value: ",round(style_club_analysis_overall_datareac()$avg_overall[1],2)),
            icon = shiny::icon("trophy"))
  )
  #aggressive
  style_club_analysis_aggressive_datareac <- reactive(
    players_20%>%
      filter(club %in% input$club)%>%
      group_by(club)%>%
      summarise(avg_aggressive = mean(mentality_aggression), na.rm = TRUE)%>%
      arrange(desc(avg_aggressive))
  )
  
  output$aggressive_club_infobox <- renderInfoBox(
    infoBox(value = style_club_analysis_aggressive_datareac()$club[1], 
            title = "The most aggressive is ", 
            subtitle = paste("Value: ",round(style_club_analysis_aggressive_datareac()$avg_aggressive[1],2)),
            icon = shiny::icon("bomb"))
  )
  #dribbling
  style_club_analysis_dribbling_datareac <- reactive(
    players_20%>%
      filter(club %in% input$club)%>%
      group_by(club)%>%
      summarise(avg_dribbling = mean(skill_dribbling), na.rm = TRUE)%>%
      arrange(desc(avg_dribbling))
  )
  
  output$dribbling_club_infobox <- renderInfoBox(
    infoBox(value = style_club_analysis_dribbling_datareac()$club[1], 
            title = "The one that dribbles the most is ", 
            subtitle = paste("Value: ",round(style_club_analysis_dribbling_datareac()$avg_dribbling[1],2)),
            icon = shiny::icon("futbol"))
  )
  
  #GRAPH
  #data
  style_club_analysis_graph_data <- reactive(
    players_20%>%
    filter(club %in% input$club)%>%
      group_by(club)%>%
      summarise(PASSING = mean(passing, na.rm=TRUE),
                DRIBBLING = mean(dribbling, na.rm=TRUE),
                DEFENDING = mean(defending, na.rm=TRUE),
                PHYSIC = mean(physic,na.rm=TRUE),
                SPRINT = mean(movement_sprint_speed, na.rm = TRUE),
                STREGHT = mean(power_strength, na.rm = TRUE),
                AGGRESSION = mean(mentality_aggression, na.rm = TRUE),
                PENALTIES = mean(mentality_penalties, na.rm = TRUE),
                MARKING = mean(defending_marking, na.rm = TRUE)
      )%>%
      gather(key = Ability, value = Value, -club)
  )
  #graph 
  output$style_club_analysis <- renderPlot({ 
     style_club_analysis_graph_data() %>%
             ggplot(aes(x = Ability, y = as.numeric(Value), fill = club)) + 
             geom_boxplot() + geom_point(size = 3) + geom_text_repel(aes(label = club), vjust = 0, nudge_y = 0.5)
  })
  
  
  ############################################ STYLE ANALYSIS COUNTRY ##########################################################

  #MAP
  #data
  style_country_analysis_graph_data <-  players_20%>%
      #filter(nationality %in% input$country_style)%>%
      group_by(nationality)%>%
      summarise(OVERALL = mean(overall, na.rm = TRUE),
                PASSING = mean(passing, na.rm=TRUE),
                DRIBBLING = mean(skill_dribbling, na.rm=TRUE),
                DEFENDING = mean(defending, na.rm=TRUE),
                PHYSIC = mean(physic,na.rm=TRUE),
                SPRINT = mean(movement_sprint_speed, na.rm = TRUE),
                STRENGHT = mean(power_strength, na.rm = TRUE),
                AGGRESSION = mean(mentality_aggression, na.rm = TRUE),
                PENALTIES = mean(mentality_penalties, na.rm = TRUE),
                MARKING = mean(defending_marking, na.rm = TRUE)
      )
  
  #maps 
  output$mapOVERALL <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "OVERALL", 
                 colorvar = "OVERALL",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })
  
  output$mapPASSING <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "PASSING", 
                 colorvar = "PASSING",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  output$mapDRIBBLING <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "DRIBBLING", 
                 colorvar = "DRIBBLING",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  output$mapDEFENDING <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "DEFENDING", 
                 colorvar = "DEFENDING",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  output$mapPHYSIC <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "PHYSIC", 
                 colorvar = "PHYSIC",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  output$mapSPRINT <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "SPRINT", 
                 colorvar = "SPRINT",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  output$mapSTRENGHT <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "STRENGHT", 
                 colorvar = "STRENGHT",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  output$mapAGGRESSION <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "AGGRESSION", 
                 colorvar = "AGGRESSION",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })    
  output$mapPENALTIES <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "PENALTIES", 
                 colorvar = "PENALTIES",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  output$mapMARKING <- renderGvis({
    gvisGeoChart(style_country_analysis_graph_data, "nationality", hovervar = "MARKING", 
                 colorvar = "MARKING",
                 options=list(colorAxis="{colors:['red','yellow','green']}")
    )
  })  
  
  style_country_analysis_data <- players_20%>%
    filter(nationality != "Jordan")%>% # I had to eliminate Jordan because too many values were missing
    group_by(nationality)%>%
    summarise(OVERALL = round(mean(overall, na.rm = TRUE),1),
              PASSING = round(mean(passing, na.rm=TRUE),1),
              DRIBBLING = round(mean(skill_dribbling, na.rm=TRUE),1),
              DEFENDING = round(mean(defending, na.rm=TRUE),1),
              PHYSIC = round(mean(physic,na.rm=TRUE),1),
              SPRINT = round(mean(movement_sprint_speed, na.rm = TRUE),1),
              STRENGHT = round(mean(power_strength, na.rm = TRUE),1),
              AGGRESSION = round(mean(mentality_aggression, na.rm = TRUE),1),
              PENALTIES = round(mean(mentality_penalties, na.rm = TRUE),1),
              MARKING = round(mean(defending_marking, na.rm = TRUE),1)
    )

  customGreen = "#71CA97"
  customRed = "#ff7f7f"
  
  output$country_style_table <- renderDataTable(
    as.datatable( formattable(style_country_analysis_data,
                            align = c("l", "c", "r", "r", "r", "r", "r", "r", "r", "r", "r" ),
                            list(`nationality` = formatter("span", 
                                                           style = ~ style(color = "grey",font.weight = "bold")), 
                                 `OVERALL` = formatter("span",
                                                       style = x ~ style(font.weight = "bold", color = ifelse(x > 50, "black", ifelse(x < 50, "#ff7f7f", "black")))),
                                 `PASSING`= color_tile(customRed, customGreen),
                                 `DRIBBLING`= color_tile(customRed, customGreen),
                                 `DEFENDING`= color_tile(customRed, customGreen),
                                 `PHYSIC`= color_tile(customRed, customGreen),
                                 `SPRINT`= color_tile(customRed, customGreen),
                                 `STRENGHT`= color_tile(customRed, customGreen),
                                 `AGGRESSION`= color_tile(customRed, customGreen),
                                 `PENALTIES`= color_tile(customRed, customGreen),
                                 `MARKING`= color_tile(customRed, customGreen)
                            ))
  )  
  )
  
}

shinyApp(ui = ui, server = server)

