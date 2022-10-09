library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)

#begin load data
df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv")

names(df) <- lapply(names(df), tolower)

natl_avg <- df %>% 
  group_by(icd.chapter, year) %>% 
  mutate(crude.rate= round((sum(deaths) / sum(population)) * 100000, 1)) %>% 
  mutate(state= "US") %>% 
  select(icd.chapter, state, year, crude.rate) %>% 
  distinct(icd.chapter, year, .keep_all = TRUE) %>% 
  ungroup()

df_avg <- df %>% 
  select(icd.chapter,state,year,crude.rate) %>% 
  rbind(natl_avg)

# end load data

ui <- dashboardPage(
  dashboardHeader(title = "Module 3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Question 1 Bar Graph", tabName = "question1bar"),
      menuItem("Question 2 Line Graph", tabName = "question2")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("question1bar", 
              fluidPage(
                titlePanel(
                  "Examining Causes of Death Across the US from 1999-2010"
                ),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = 'year', 
                      label = 'Year:', 
                      choices = unique(df$year),
                      selected = 2010), #sets the default display 2010 chosen to answer the question
                    selectInput(
                      inputId = 'cause', 
                      label = 'Cause of Death:', 
                      choices = unique(df$icd.chapter),
                      selected = "Certain infectious and parasitic diseases"),
                    selectInput(
                      inputId = 'topN', #controls the displayed amount of values
                      label = 'Choose Top States to display', 
                      choices = c(10,25,51), #display 10, 25 or 51 at a time
                      selected = 10), #default is 10
                  ),
                  mainPanel(
                    plotOutput('plot1', height = "800px")
                  )
                )
              )
      ),
      tabItem("question2",
              fluidPage(
                titlePanel(
                  "Examining Causes of Death Across the US from 1999-2010"
                ),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = 'cause', 
                      label = 'Cause of Death:', 
                      choices = unique(df_avg$icd.chapter),
                      selected = "Certain infectious and parasitic diseases"),
                    selectInput(
                      inputId = 'state', 
                      label = 'State:', 
                      multiple = TRUE,
                      choices = unique(df_avg$state),
                      selected='AL'),
                  ),
                  mainPanel(
                    plotOutput(outputId = 'lineplot')
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output){
  #bar begin
  plot_data <- reactive({
    filter(df, year == input$year & icd.chapter == input$cause) %>% 
      top_n(n=as.integer(input$topN),wt=crude.rate)
  })
  output$plot1 <- renderPlot({
    ggplot(plot_data(), aes(x=reorder(state, crude.rate), y=crude.rate))+
      geom_col(fill='#f69697') + 
      coord_flip() +
      geom_text(aes(label=crude.rate), size = 4, hjust = -0.1)+
      labs(title=paste('Crude Mortality Rates for',input$cause),
           subtitle=paste('Top', input$topN, 'states'),
           x = "State", 
           y = "Crude Rate")+
      theme_minimal()
  }
  )
  #bar end
  
  #line begin
  line_data <- reactive({
      filter(df_avg, icd.chapter == input$cause & 
               (state == 'US' | state == input$state)) 
  })
  
  output$lineplot <- renderPlot({
    ggplot(line_data(), aes(x=line_data()$year,
                            y=line_data()$crude.rate)) +
      geom_line(aes(color = state)) +
      labs(x= "Year", y = "Crude Rate")+
      theme_minimal() 
  }
  )
  #line end
}

shinyApp(ui, server)