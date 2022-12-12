library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

#data
#load unemployment benefits data

benefits15 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnempBenefits2015.csv")
benefits16 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnempBenefits2016.csv")
benefits19 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnempBenefits2019.csv")
benefits20 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnempBenefits2020.csv")

benefits15 <- benefits15 %>% select("State", "X2014.2015.Max.Weekly.Amt", "Year")
benefits16 <- benefits16 %>% select("State", "Max..Weekly.Benefit.Amount", "Year")
benefits19 <- benefits19 %>% select("State", "Max..Weekly.Benefit.Amount", "Year")
benefits20 <- benefits20 %>% select("State", "Max..Weekly.Benefit.Amount", "Year")

colnames(benefits15)[2] <- "Max..Weekly.Benefit.Amount"

benefits <- rbind(benefits15, benefits16, benefits19, benefits20)

colnames(benefits)[2] <- "MaxWeeklyBenefits"

#load unemployment rate data
rates15 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnemploymentRates2015.csv")
rates16 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnemploymentRates2016.csv")
rates19 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnemploymentRates2019.csv")
rates20 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/UnemploymentRates2020.csv")

rates <- rbind(rates15,rates16,rates19,rates20)

#merge rates and benefits
unemployment_df <- merge(x=benefits,y=rates,
                         by.x= c("State","Year"),
                         by.y= c("State","Year"))

#load election data 

# needed for 2015 data
election12 <- read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/Election2012.csv")
election16 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/Election2016.csv")
election20 <-read.csv("https://raw.githubusercontent.com/gabbypaola/DATA608/main/Final%20Project/Election2020.csv")

#2012 battleground states
#needed for 2015 data to show how the state voted in previous election 
election12 <- election12 %>% select("State", "Obama", "Romney", "Other")

election12$Obama = as.numeric(sub("%", "",election12$Obama))
election12$Romney = as.numeric(sub("%", "",election12$Romney))
election12$Other = as.numeric(sub("%", "",election12$Other))

election12$StateResult <- colnames(select(election12, "Obama", "Romney",
                                          "Other"))[max.col(select(election12, "Obama", "Romney","Other"))]

battleground_states12 =  data.frame("State"=c("Nevada","Colorado","Iowa",
                                              "Wisconsin", "Ohio","Virginia", 
                                              "North Carolina", "Florida",
                                              "New Hampshire"))

election12$SwingState <- ifelse(election12$State %in% 
                                  battleground_states12$State, "Yes", "No")

election12$StateElectedParty <- ifelse(election12$StateResult == "Obama", 
                                       "Democrat", "Republican")

#using 2015 as year to add standing president 
election12$Year = 2015

election12$ElectionResult = "Democrat"

election12 <- election12 %>% select("State", "Year", "StateResult", 
                                    "SwingState", "StateElectedParty", 
                                    "ElectionResult")

#2016 battleground states
election16 <- election16 %>% select("State", "Clinton", "Trump", "Johnson",
                                    "Other")
#str(election16)

election16$Clinton = as.numeric(sub("%", "",election16$Clinton))
election16$Trump = as.numeric(sub("%", "",election16$Trump))
election16$Johnson = as.numeric(sub("%", "",election16$Johnson))
election16$Other = as.numeric(sub("%", "",election16$Other))

election16$StateResult <- colnames(select
                                   (election16,"Clinton", "Trump", "Johnson", 
                                     "Other"))[max.col(select(election16, 
                                                              "Clinton", "Trump", 
                                                              "Johnson","Other"))]

battleground_states16 = data.frame("State"=c("Arizona",
                                             "Nevada","Colorado","Iowa",
                                             "Wisconsin","Michigan","Ohio",
                                             "Pennsylvania","Virginia",
                                             "North Carolina", "Florida",
                                             "New Hampshire"))

election16$SwingState <- ifelse(election16$State %in% 
                                  battleground_states16$State, "Yes", "No")

election16$StateElectedParty <- ifelse(election16$StateResult == "Trump", 
                                       "Republican","Democrat")

election16$Year = 2016

election16$ElectionResult = "Republican"

election16 <- election16 %>% select("State", "Year", "StateResult", 
                                    "SwingState", "StateElectedParty", 
                                    "ElectionResult")

election16$State <- sub("D. C.", "District of Columbia",election16$State)

#2019 standing president
standing_pres19 <- election16

#changing year to 2019 to reflect president elect for that year
standing_pres19["Year"]=2019

#2020 battlegroundstates
election20 <- election20 %>% select("State", "Biden", "Trump", "Other")

election20$Biden = as.numeric(sub("%", "",election20$Biden))
election20$Trump = as.numeric(sub("%", "",election20$Trump))
election20$Other = as.numeric(sub("%", "",election20$Other))

election20$StateResult <- colnames(
  select(election20, "Biden", "Trump", "Other"))[max.col(
    select(election20, "Biden", "Trump", "Other"))]

battleground_states20 = data.frame("State"=c("Arizona","Florida",
                                             "Georgia","Iowa","Michigan", 
                                             "Minnesota","Nevada",
                                             "New Hampshire","North Carolina",
                                             "Ohio","Pennsylvania","Texas",
                                             "Wisconsin"))

election20$SwingState <- ifelse(election20$State %in% 
                                  battleground_states20$State, "Yes", "No")

election20$StateElectedParty <- ifelse(election20$StateResult == "Trump", 
                                       "Republican","Democrat")

election20$Year = 2020

election20$ElectionResult = "Democrat"

election20 <- election20 %>%  select("State", "Year", "StateResult", 
                                     "SwingState", "StateElectedParty", 
                                     "ElectionResult")

election20$State <- sub("D. C.", "District of Columbia",election20$State)

#merge all election data
election <- rbind(election12, election16, standing_pres19, election20)

#merge `unemployment_df` with `election`
unemp_elec_df <- unemployment_df %>% full_join(election,
                                               by= c("State","Year"))

colnames(unemp_elec_df)[4] <- "UnemploymentRate"

colnames(unemp_elec_df)[5] <- "UnemploymentRank"

regions <- read.csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

regions <- regions %>% select("State","State.Code","Region")

###FULL DATA###
unemp_elec_df <- unemp_elec_df %>% full_join(regions, by="State")

#split data up into pre-election year (previous election results)

###PRE-ELECTION###
pre_election_df <- unemp_elec_df %>% filter(Year %in% c(2015,2019))

###ELECTION###
election_result_df <- unemp_elec_df %>% filter(Year %in% c(2016,2020))

#end data
#----------------------------------------------------------------------------


#ui
ui <- dashboardPage( 
  dashboardHeader(title = "DATA608 Final Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Unemployment Rates vs. Unemployment Benefits", tabName = "scatterplot"),
      menuItem("Regional Unemployment Benefits", tabName = "region_bene"),
      menuItem("State Unemployment Benefits", tabName = "state_bene"),
      menuItem("Regional Unemployment Rates", tabName = "region_rates"),
      menuItem("State Unemployment Rates", tabName = "state_rates")
    )
  ),
  dashboardBody(
    tabItems(
      #tab 1
      tabItem("scatterplot", 
              fluidPage(
                titlePanel(
                  "Examining relationship between Unemployment and Elected Political Parties"
                ),
                sidebarLayout(
                  sidebarPanel(
                    #filter 1
                    selectInput(
                      inputId = 'year1', 
                      label = 'Year:', 
                      choices = unique(unemp_elec_df$Year),
                      selected = 2015), #sets the default display 2015
                  ),
                  mainPanel(
                    plotOutput('plot1', height = "800px")
                  )
                )
              )
      ),
      #tab 2
      tabItem("region_bene", 
              fluidPage(
                titlePanel(
                  "Examining relationship between Unemployment and Elected Political Parties"
                ),
                sidebarLayout(
                  sidebarPanel(
                    #filter 1
                    selectInput(
                      inputId = 'year2', 
                      label = 'Pre-Election Year:', 
                      choices = unique(pre_election_df$Year),
                      selected = 2015),
                    #filter 2
                    selectInput(
                      inputId = 'year3', 
                      label = 'Election Year:', 
                      choices = unique(election_result_df$Year),
                      selected = 2016),
                  ),
                  mainPanel(
                    #displays graphs side by side in a row
                    fluidRow(splitLayout( cellWidths = c("50%", "50%"),
                                         plotOutput('plot2'), 
                                         plotOutput('plot3')))
                  )
                )
                
              )
      ),
      #tab 3
      tabItem("state_bene",
              fluidPage(
                titlePanel(
                  "Examining relationship between Unemployment and Elected Political Parties"
                ),
                sidebarLayout(
                  sidebarPanel(
                    #filter1
                    selectInput(
                      inputId = 'year4',
                      label = 'Pre-Election Year:',
                      choices = unique(pre_election_df$Year),
                      selected = 2015),
                    #filter2
                    selectInput(
                      inputId = 'swingstate4',
                      label = 'Previous Election Swing State:',
                      choices = unique(pre_election_df$SwingState),
                      multiple = TRUE,
                      selected = c("Yes","No")),
                    #filter3
                    selectInput(
                      inputId = 'year5',
                      label = 'Election Year:',
                      choices = unique(election_result_df$Year),
                      selected=2016),
                    #filter4
                    selectInput(
                      inputId = 'swingstate5',
                      label = 'Election Swing State:',
                      choices = unique(election_result_df$SwingState),
                      multiple = TRUE,
                      selected = c("Yes","No")),
                  ),
                  mainPanel(
                    #displays graphs side by side in a row
                    fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                         plotOutput('plot4'),
                                         plotOutput('plot5')))
                  )
                )
              )
      ),
      #tab 4
      tabItem("region_rates",
              fluidPage(
                titlePanel(
                  "Examining relationship between Unemployment and Elected Political Parties"
                ),
                sidebarLayout(
                  sidebarPanel(
                    #filter 1
                    selectInput(
                      inputId = 'year6', 
                      label = 'Pre-Election Year:', 
                      choices = unique(pre_election_df$Year),
                      selected = 2015),
                    #filter 2
                    selectInput(
                      inputId = 'year7', 
                      label = 'Election Year:', 
                      choices = unique(election_result_df$Year),
                      selected = 2016),
                  ),
                  mainPanel(
                    #displays graphs side by side in a row
                    fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                         plotOutput('plot6'),
                                         plotOutput('plot7')))
                  )
                )

              )
      ),
      #tab 5
      tabItem("states_rates",
              fluidPage(
                titlePanel(
                  "Examining relationship between Unemployment and Elected Political Parties"
                ),
                sidebarLayout(
                  sidebarPanel(
                    #filter1
                    selectInput(
                      inputId = 'year8',
                      label = 'Pre-Election Year:',
                      choices = unique(pre_election_df$Year),
                      selected = 2015),
                    #filter2
                    selectInput(
                      inputId = 'swingstate8',
                      label = 'Previous Election Swing State:',
                      choices = unique(pre_election_df$SwingState),
                      multiple = TRUE,
                      selected = c("Yes","No")),
                    #filter3
                    selectInput(
                      inputId = 'year9',
                      label = 'Election Year:',
                      choices = unique(election_result_df$Year),
                      selected=2016),
                    #filter4
                    selectInput(
                      inputId = 'swingstate9',
                      label = 'Election Swing State:',
                      choices = unique(election_result_df$SwingState),
                      multiple = TRUE,
                      selected = c("Yes","No")),
                  ),
                  mainPanel(
                    #displays graphs side by side in a row
                    fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                         plotOutput('plot8'),
                                         plotOutput('plot9')))
                  )
                )

              )
      )
    )
  )
)


#end ui

#server
server <- function(input, output) {
  #TAB 1 plot1 begin
  
  #reactive data
  scatterplot_data <-  reactive({filter(unemp_elec_df, Year== input$year1)
    })
  
  #code for plot  
  output$plot1 <- renderPlot({
    ggplot(scatterplot_data(), aes(x=UnemploymentRate, y=MaxWeeklyBenefits, 
               color=StateElectedParty)) +
    geom_point(size= 2) +
    geom_smooth(method="lm",se = FALSE)+
    scale_color_manual(values= c("Democrat" = "#4575b4",
                                 "Republican" = "#d73027")) +
    guides(color = guide_legend(title = "Elected Party")) +
    labs(title= "Unemployment Rate vs. Unemployment Weekly Benefit Amounts",
         subtitle= input$year1,
         x= "Unemployment Rate", 
         y = "Weekly Benefit Amount")+
    theme(plot.title = element_text(color = "#000500"),
          plot.subtitle = element_text(color = "#484e4c"),
          panel.grid.minor.y = element_blank())
  })
  
  #TAB 1 plot1 end---------------------------------------------
  
  
  #TAB 2 plot2 begin REGIONAL AMOUNTS Pre Election
  
  plot2_data <- reactive({
  filter(pre_election_df, Year== input$year2) %>% 
    group_by(StateElectedParty, Region) %>% 
    summarise(AvgBenefits = round(mean(MaxWeeklyBenefits),2)) 
  })
    
  output$plot2 <- renderPlot({
    ggplot(plot2_data(), aes(fill=StateElectedParty, y= AvgBenefits, 
               x=reorder(Region, AvgBenefits)))+
    geom_col(stat="identity", width=0.7,
             position=position_dodge(width = 0.8))+ 
    scale_fill_manual(values= c("Democrat" = "#4575b4",
                                "Republican" = "#d73027"))+
    guides(fill=guide_legend(title="Elected Party"))+
    coord_flip()+
    labs(title="Regional Previous Election Results and Average Unemployment Benefits",
         subtitle= input$year2,
         y= "Average Weekly Benefits", 
         x = "Region")+
    theme(plot.title = element_text(size = 12, color = "#000500"),
          plot.subtitle = element_text(size = 9, color = "#484e4c"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  })
  #TAB 2 plot2 end--------------------------------------------------------------
  
  
  #TAB 2 plot3 begin Election
  plot3_data <- reactive({
    filter(election_result_df, Year== input$year3) %>% 
      group_by(StateElectedParty, Region) %>% 
      summarise(AvgBenefits = round(mean(MaxWeeklyBenefits),2)) 
  })
  
  output$plot3 <- renderPlot({
    ggplot(plot3_data(), aes(fill=StateElectedParty, y= AvgBenefits, 
               x=reorder(Region, AvgBenefits)))+
      geom_col(stat="identity", width=0.7,
               position=position_dodge(width = 0.8))+ 
      scale_fill_manual(values= c("Democrat" = "#4575b4",
                                  "Republican" = "#d73027"))+
      guides(fill=guide_legend(title="Elected Party"))+
      coord_flip()+
      labs(title="Regional Election Results and Average Unemployment Benefits",
           subtitle= input$year3,
           y= "Average Weekly Benefits", 
           x = "Region")+
      theme(plot.title = element_text(size = 12, color = "#000500"),
            plot.subtitle = element_text(size = 9, color = "#484e4c"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  #TAB 2 plot3 end--------------------------------------------------------------
  
  #TAB 3 plot4 begin STATE AMOUNTS Pre Election

 plot4_data <- reactive({
    filter(pre_election_df, Year==input$year4, SwingState=input$swingstate4) %>%
    group_by(StateElectedParty, Region) %>%
    mutate(avg.benefit = mean(MaxWeeklyBenefits))
 })

    ##plot
  output$plot4 <- renderPlot({
    ggplot(plot4_data(), aes(x=reorder(State,MaxWeeklyBenefits),y=MaxWeeklyBenefits,
               color=StateElectedParty,
               group=StateElectedParty,
               fill=StateElectedParty))+
    facet_wrap(~Region, scales='free_y')+
    geom_col(width= 0.5, colour=NA) +
    scale_fill_manual(values= c("Democrat" = "#4575b4",
                                "Republican" = "#d73027"))+
    theme(axis.line=element_line())+
    labs(y= "Maximum Weekly Benefits", x = "State")+
    guides(fill=guide_legend(title="Elected Party"))+
    coord_flip()+
    labs(title="Previous Election Results and Average Unemployment Benefits",
         subtitle= input$year4,
         y= "Average Weekly Benefits",
         x = "Region")+
    theme(plot.title = element_text(size = 12, color = "#000500"),
          plot.subtitle = element_text(size = 9, color = "#484e4c"),
          #panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  })

  #TAB 3 plot4 end--------------------------------------------------------------

  #TAB 3 plot5 begin STATE AMOUNTS Election

    ##data
  plot5_data <- reactive({
    filter(election_result_df, Year==input$year5, SwingState=input$swingstate5) %>%
      group_by(StateElectedParty, Region) %>%
      mutate(avg.benefit = mean(MaxWeeklyBenefits))
    })

    ##plot
  output$plot5 <- renderPlot({
    ggplot(plot5_data(), aes(x=reorder(State,MaxWeeklyBenefits),y=MaxWeeklyBenefits,
               color=StateElectedParty,
               group=StateElectedParty,
               fill=StateElectedParty))+
      facet_wrap(~Region, scales='free_y')+
      geom_col(width= 0.5, colour=NA) +
      scale_fill_manual(values= c("Democrat" = "#4575b4",
                                  "Republican" = "#d73027"))+
      theme(axis.line=element_line())+
      labs(y= "Maximum Weekly Benefits", x = "State")+
      guides(fill=guide_legend(title="Elected Party"))+
      coord_flip()+
      labs(title= paste(input$year5 , "Election Results and Average Unemployment Benefits"),
           subtitle= "Average Weekly benefit Amounts",
           y= "Average Weekly Benefits",
           x = "Region")+
      theme(plot.title = element_text(size = 12, color = "#000500"),
            plot.subtitle = element_text(size = 9, color = "#484e4c"),
            #panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  #TAB 3 plot 5 end-----------------------------------------------------------

  ###Unemployment Rates

  #TAB 4 plot 6 being REGIONAL Unemployment Rates Pre-Election
  plot6_data <- reactive({
    filter(pre_election_df, Year==input$year6) %>%
      group_by(StateElectedParty, Region) %>%
      summarise(AvgUnemp = round(mean(UnemploymentRate),2))
  })

  output$plot6 <- renderPlot({
      ggplot(plot6_data(), aes(fill=StateElectedParty, y= AvgUnemp,
                 x=reorder(Region, AvgUnemp)))+
      geom_col(stat="identity", width=0.7,
               position=position_dodge(width = 0.8))+ 
      scale_fill_manual(values= c("Democrat" = "#4575b4",
                                  "Republican" = "#d73027"))+
      guides(fill=guide_legend(title="Elected Party"))+
      labs(title="Previous Election Results and Unemployment Rates",
           subtitle= input$year6,
           y= "Average Unemployment Rate",
           x = "Region")+
      coord_flip()+
      theme(plot.title = element_text(size = 12, color = "#000500"),
            plot.subtitle = element_text(size = 9, color = "#484e4c"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })

  #TAB 4 Plot 7 end REGIONAL Unemployment Rates Election
  plot7_data <- reactive({
    filter(election_result_df, Year==input$year7) %>%
      group_by(StateElectedParty, Region) %>%
      summarise(AvgUnemp = round(mean(UnemploymentRate),2))
  })

  output$plot7 <- renderPlot({
    ggplot(plot7_data(), aes(fill=StateElectedParty, y= AvgUnemp,
                             x=reorder(Region, AvgUnemp)))+
      geom_col(stat="identity", width=0.7,
               position=position_dodge(width = 0.8))+ 
      scale_fill_manual(values= c("Democrat" = "#4575b4",
                                  "Republican" = "#d73027"))+
      guides(fill=guide_legend(title="Elected Party"))+
      labs(title="Election Results and Unemployment Rates",
           subtitle= input$year7,
           y= "Average Unemployment Rate",
           x = "Region")+
      coord_flip()+
      theme(plot.title = element_text(size = 12, color = "#000500"),
            plot.subtitle = element_text(size = 9, color = "#484e4c"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })

  ##############################

  #TAB 5 plot 8 being STATE Unemployment Rates Pre-Election

  plot8_data <- reactive({
  filter(pre_election_df, Year==input$year8, SwingState==input$swingstate8) %>%
    group_by(StateElectedParty, Region) %>%
    mutate(avg.unemp = mean(UnemploymentRate))
  })

  output$plot8 <- renderPlot({
    ggplot(plot8_data(), aes(x=reorder(State,UnemploymentRate),
                             y=UnemploymentRate,
               color=StateElectedParty,
               group=StateElectedParty,
               fill=StateElectedParty))+
    facet_wrap(~Region, scales='free_y')+
    geom_col(width= 0.5, colour=NA) +
    scale_fill_manual(values= c("Democrat" = "#4575b4",
                                "Republican" = "#d73027"))+
    theme(axis.line=element_line(),
          plot.title = element_text(size = 12, color = "#000500"),
          plot.subtitle = element_text(size = 9, color = "#484e4c"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())+
    labs(title="Previous Election Results and Unemployment Rates",
         subtitle= input$year8,
         y= "Average Unemployment Rate",
         x = "State")+
    guides(fill=guide_legend(title="Elected Party"))+
    coord_flip()
  })

  #TAB 5 Plot 9 end STATE Unemployment Rates Election
  plot9_data <- reactive({
    filter(election_result_df, Year==input$year9,
           SwingState==input$swingstate9) %>%
      group_by(StateElectedParty, Region) %>%
      mutate(avg.unemp = mean(UnemploymentRate))
  })

  output$plot9 <- renderPlot({
    ggplot(plot9_data(), aes(x=reorder(State,UnemploymentRate),y=UnemploymentRate,
               color=StateElectedParty,
               group=StateElectedParty,
               fill=StateElectedParty))+
      facet_wrap(~Region, scales='free_y')+
      geom_col(width= 0.5, colour=NA) +
      scale_fill_manual(values= c("Democrat" = "#4575b4",
                                  "Republican" = "#d73027"))+
      theme(axis.line=element_line(),
            plot.title = element_text(size = 12, color = "#000500"),
            plot.subtitle = element_text(size = 9, color = "#484e4c"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())+
      labs(title="Previous Election Results and Unemployment Rates",
           subtitle= input$year9,
           y= "Average Unemployment Rate",
           x = "State")+
      guides(fill=guide_legend(title="Elected Party"))+
      coord_flip()
  })


  
} 

shinyApp(ui, server)
