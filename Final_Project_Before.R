#https://www.biostars.org/p/436837/
#https://www.statology.org/ggplot-point-size/
#https://www.tutorialspoint.com/how-to-increase-the-x-axis-labels-font-size-using-ggplot2-in-r

library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)

financial_df <- read.csv("Financials_Full_Data_data.csv")

findf_cut <- financial_df[-c(44, 45, 46), ]

get_netProfit <- function(yearS)
{
  netYear <- filter(findf_cut, Year == yearS)
  Profit <- select(netYear, Net.Profit)
  return(Profit)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Choosen_year",
        label = "Choose the Year You Wish to See",
        choices = findf_cut$Year
      ),
      textOutput(outputId = "finYear")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "yearPlot")),
        tabPanel("Table", tableOutput(outputId = "yearTable"))
      )
    )
  ),
  h3("Timeline of the Fall and Rise of US Airline Industry"),
  p("The development of air travel marks an important turning point in human history, marking the start of a revolutionary epoch that has fundamentally altered how people transit, interconnect, and interact with the global terrain. Beginning with the pioneering experiments in powered flight pioneered by visionaries such as the Wright brothers, the history of air travel has unfolded as a narrative marked by constant innovation, major technological developments, and a simultaneous transformation in societal dynamics. The journey of air travel stands as a tribute to the enduring spirit of discovery and advancement that has carried humanity to new heights, from those early forays into the skies to the sleek and highly sophisticated planes of the twenty-first century."),
  p("Over the course of its history, the U.S. airline industry has experienced numerous fluctuations, witnessing periods of decline and resurgence."),
  p(strong("1978: "), br(), a("Airline Deregulation: When Everything Changed", href = "https://airandspace.si.edu/stories/editorial/airline-deregulation-when-everything-changed"), " states the U.S. Congress enacted the Airline Deregulation Act, eliminating government-imposed constraints on airline routes and pricing. This move aimed to foster heightened competition, facilitate the entry of new carriers, and result in reductions in airfares. This legislation granted airlines the freedom to select their routes and set prices based on market demand. In response, both established and new airlines swiftly sought to secure routes, intensifying competition and contributing to a notable reduction in fare charges."), 
  p(strong("1981: "),br(), "According to the " , a("The Legacy of the Crushed 1981 PATCO Strike", href = "https://jacobin.com/2021/08/reagan-patco-1981-strike-legacy-air-traffic-controllers-union-public-sector-strikebreaking#:~:text=Forty%20years%20ago%20today%2C%2013%2C000,re%20still%20dealing%20with%20today"), ", the Professional Air Traffic Controllers Organization (PATCO), consisting of 13,000 air traffic controllers, initiated a strike to press for higher wages, equipment upgrades, and a reduced workweek. However, President Ronald Reagan responded decisively by terminating 11,345 striking controllers. This culminated in a failed strike, resulting in the dismissal and lifetime ban from federal employment for many air traffic controllers. It wasn't until 1993 that President Bill Clinton lifted the ban, allowing the affected individuals to return to federal employment."),
  p(strong("1990-1991: "), br(), "The U.S. airline industry experienced a significant downturn in net profit, primarily attributed to the repercussions of the ", a("Persian Gulf War", href = "https://www.latimes.com/archives/la-xpm-1990-11-06-mn-3993-story.html") ," that transpired between 1990 and 1991. The crisis triggered a surge in jet fuel prices, constituting the second-largest expense for carriers and accounting for approximately 15% of their operating costs. As reflected in the data table, the industry's fuel expenses escalated from 3,000 million dollars in 1989 to 1990."),
  p(strong("2001: "), br(), "
Following the ", a("impact of the 9/11 attack", href="https://apnews.com/article/how-sept-11-changed-flying-1ce4dc4282fb47a34c0b61ae09a024f4") , ", the airline industry faced a substantial decline in net profit, plummeting to a negative figure of -8,008 million dollars. The aftermath of the attack prompted heightened security measures, resulting in increased tension and more rigorous security checks at airports. To address these challenges, President George W. Bush signed legislation establishing the Transportation Security Administration (TSA), replacing the private companies previously hired by airlines to handle security. The primary goal was to prevent a recurrence of such attacks. The implementation of new security requirements necessitated passengers to arrive at the airport well in advance to navigate through the intensified security checkpoints."),
  p(strong("2005: "), br(), "The U.S. airline industry faced a significant downturn, reaching a new nadir primarily attributable to the combination of ", a("soaring fuel costs and diminished fares", href="https://www.denverpost.com/2005/10/17/airlines-profits-sink-as-fuel-prices-go-up/") ," that pushed many airlines into bankruptcy. During this year, the industry witnessed an unprecedented spike in fuel expenses, surging to an all-time high of 27,221 million dollars. Even after four years, the aftermath of the 9/11 attacks continues to cast a long shadow over the sector. Security concerns spurred bigger investments to improve security measures and processes, raising operational costs even higher. Concurrently, natural disasters, such as Hurricane Katrina, had a localized but considerable impact on specific airline routes and operations. The combination of these elements created a difficult operating environment, highlighting the long-term consequences of both economic and security challenges in the United States airline business."),
  p(strong("2006: "), br(), "The U.S. airline industry embarked on a path to recovery, marking a significant turnaround from the challenges faced in the preceding year. Several factors converged to propel the industry to a new high in net profit. An economic recovery, characterized by increased consumer spending and heightened business activities, played a pivotal role. Airlines strategically invested in more efficient operational methods, fostering profitability, and selectively focused on more lucrative routes, contributing to the notable upswing in net profits. This confluence of factors ushered in a positive trajectory for the U.S. airline industry in 2006."),
  p(strong("2008: "), br(), "The onset of the ", a("Great Recession", href="https://www.forbes.com/advisor/investing/great-recession/") ," triggered a decline in both consumer spending and business activities, leaving numerous individuals facing financial constraints that made travel unaffordable. Widespread job losses and the struggle to secure employment became prevalent, affecting the financial well-being of people and limiting their capacity to partake in travel-related expenses. Amid ongoing economic uncertainties, a substantial portion of the population encountered challenges in affording travel, signifying a noteworthy alteration in individuals' readiness and capability to embark on journeys amid this economically challenging period."),
  p(strong("Beyond: "), br(), "From 1977 onward, the airline industry has experienced various setbacks interspersed with occasional periods of slight improvement. Confronted by the dynamic challenges inherent in the sector, some airlines encountered difficulties in adapting and chose either to merge with other carriers or succumb to bankruptcy. Meanwhile, other more resilient airlines endured, showcasing their adaptability in the midst of the diverse challenges that define the continually changing landscape of the aviation industry.")
)

server <- function(input, output)
{
  output$finYear <- renderText({
    paste("The Net Profit for ", input$Choosen_year ," is ", get_netProfit(input$Choosen_year), " in millions.")
    })
  
  output$yearPlot <- renderPlot({
    ggplot(data = findf_cut, aes(x = Year, y = Net.Profit)) +
      geom_point() +
      theme(
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15)
      ) + 
      labs(
        x = "Year",
        y = "Net Profit in millions",
        title = "Financial Results for US Passengers Airlines"
      ) +
      geom_point(data=findf_cut %>% filter(findf_cut$Year == input$Choosen_year), color = "red", size = 5)
  })
  
  output$yearTable <- renderTable(
    yearSelected <- filter(findf_cut, Year == input$Choosen_year)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)