library(shiny)
library(plotly)
library(dplyr)

df <- read.csv("plot_df.csv", stringsAsFactors = F)

ui <- fluidPage(
      headerPanel("AactionButton test"),
      h4(textOutput("president")),
      h4(textOutput("state_num")),
      plotlyOutput('mainplot'),
      hr(),
      
      fluidRow(
            column(4,
                  sliderInput(inputId = 'election_year',
                  label = 'Year',
                  value = 2016, min = 1976, max = 2016,step = 4)
                  )
      )
)
            


server <- function(input,output){
      #Apply filters
      election <- reactive({
            # Filter by year
            selected_year <- input$election_year
            plot_df <-  subset(df,df$year == selected_year & df$state_abb != 'United States')
            plot_df
      })
      
      
      
      #Generate President Name
      president <- reactive({
            p <- election()$President[1]
            p_string <- paste('\n President Elected:',p)
            p_string
      })
      
      output$president <- renderText({president()})
                                     
      #Generate # Unpredictable state
      state_num <- reactive({
           states <- election() %>% 
                  filter( (relative_personal_income>1 & margin_percent < 0) |
                           (relative_personal_income <1 & margin_percent >0)) 
           nrow(states)
      })
      
      output$state_num <- renderText({state_num()})
                                     
      #Generate plot
      output$mainplot <- renderPlotly({
           ggplot(election()) +
                  geom_point(aes(x = relative_personal_income,y = margin_percent,size = margin_votes,color = win),alpha = 0.5) +
                  scale_size_continuous(range = c(1,20),guide = FALSE) + 
                  scale_color_manual(values= c(Republican = '#E91D0E',Democratic = '#232066'),guide = FALSE)+
                  geom_text(aes(x = relative_personal_income,y = margin_percent,label = state_abb)) +
                  geom_vline(xintercept = 1, size = 0.1) +
                  geom_hline(yintercept = 0,size = 0.1) + 
                  xlim(0.7,1.7) +
                  ylim(-50,100) + 
                  xlab('Per Capita Personal Income As of National Per Capita Income') +
                  ylab('Vote Margin (%, Democratic - Republican)') + 
                  theme_bw()

      })
}

shinyApp(ui = ui, server = server)