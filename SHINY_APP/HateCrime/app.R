#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
######################################################################################
#LOADING UP LIBRARIES

library("ggplot2")
library("ggExtra")
library("bslib")



######################################################################################
#LOADING UP AND TRANSFORMING THE DATASET
HateCrime <- read.csv("hate_crimes.csv")
#HateCrime <- read.csv("C:\\Users\\bruno\\OneDrive\\Desktop\\COURSERA\\DS_JOHNSHOPKINS\\COURSE09\\PROJECT\\SHINY_APP\\hate_crimes.csv")
HateCrimeTrimmed <- HateCrime[!is.na(HateCrime$avg_hatecrimes_per_100k_fbi),]
HateCrimeTrimmed <- HateCrimeTrimmed[HateCrimeTrimmed$avg_hatecrimes_per_100k_fbi<10,]
HC <- HateCrimeTrimmed[,-which(names(HateCrimeTrimmed) =="hate_crimes_per_100k_splc")]



ui <- page_sidebar(
  titlePanel("Hate Crime Statistics per 100.000 population, FBI + SPLC"),
  theme = bs_theme(bootswatch = "lux"),
  mainPanel(
    p("These are the statistics of hate crimes across US states between 2012 and 2015, as compiled by the FBI, the Southern Poverty Law Center and the Kaiser Family foundations.", style = "font-family: 'times'; font-si16pt"),
    p("This app explores visually the correlation between various independent variables of the aforementioned study, mainly socioeconomical variables, and the number of hate crimes commited in the period.", style = "font-family: 'times'; font-si16pt"),
    strong("You can change the variables correlated, as well as see the effects of different socioeconomic conditions on the slides to the left.", style = "font-family: 'times'; font-si16pt"),
  ),
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", HC, selected = "avg_hatecrimes_per_100k_fbi"),
    varSelectInput("yvar", "Y variable", HC, selected = "median_household_income"),
    hr(), # Add a horizontal rule
    sliderInput("gini_index", "GINI Index",
                min = min(HC$gini_index), max = max(HC$gini_index), 
                value = c(min(HC$gini_index), max(HC$gini_index))),
    sliderInput("share_unemployed_seasonal", "Seasonal Unemployment %",
                min = min(HC$share_unemployed_seasonal), max = max(HC$share_unemployed_seasonal), 
                value = c(min(HC$share_unemployed_seasonal), max(HC$share_unemployed_seasonal))),
    sliderInput("share_population_with_high_school_degree", "% Population with High School Degree",
                min = min(HC$share_population_with_high_school_degree), max = max(HC$share_population_with_high_school_degree), 
                value = c(min(HC$share_population_with_high_school_degree), max(HC$share_population_with_high_school_degree))),    
    hr(), # Add a horizontal rule
    checkboxInput("show_margins", "Show marginal plots", TRUE),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$gini_index,input$share_unemployed_seasonal,input$share_population_with_high_school_degree)
    HC <- subset(HC, gini_index>=input$gini_index[1] & gini_index <=input$gini_index[2])
    HC <- subset(HC, share_unemployed_seasonal>=input$share_unemployed_seasonal[1] & share_unemployed_seasonal<=input$share_unemployed_seasonal[2])
    HC <- subset(HC, share_population_with_high_school_degree>=input$share_population_with_high_school_degree[1] & share_population_with_high_school_degree<=input$share_population_with_high_school_degree[2])
    #HC <- HC %>% filter(gini_index>=input$gini_index[1] & gini_index <=input$gini_index[2])
    #HC <- HC %>% filter(share_unemployed_seasonal>=input$share_unemployed_seasonal[1] & share_unemployed_seasonal<=input$share_unemployed_seasonal[2])
    #HC <- HC %>% filter(share_population_with_high_school_degree>=input$share_population_with_high_school_degree[1] & share_population_with_high_school_degree<=input$share_population_with_high_school_degree[2])
    
    return(HC)
  })
  
  output$scatter <- renderPlot({
    p <- ggplot(data = subsetted(), aes(!!input$xvar, !!input$yvar, colour="#006666", alpha=0.5)) + list(
      theme(legend.position = "bottom",         
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent')),
      geom_point(),
      geom_smooth()
    )
    if (input$show_margins) {
      p <- ggExtra::ggMarginal(p, type = "density", margins = "both", size = 8,groupColour = TRUE, groupFill = TRUE)
    }
    p 
  }, res = 100)
}

shinyApp(ui, server)