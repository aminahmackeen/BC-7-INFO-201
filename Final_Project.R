#https://www.biostars.org/p/436837/
#https://www.statology.org/ggplot-point-size/
#https://www.tutorialspoint.com/how-to-increase-the-x-axis-labels-font-size-using-ggplot2-in-r

library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("BC-7_Data_Wrangling.R")

findf_cut <- fdf[-c(44, 45, 46), ]

get_netProfit <- function(yearS)
{
  netYear <- filter(findf_cut, Year == yearS)
  Profit <- select(netYear, Net.Profit)
  return(Profit)
}

ui <- fluidPage(
  
  titlePanel("COVID-19 VS the Travel Industry"),
  
  navlistPanel(
    "Introduction",
    
    ## intro page
    tabPanel("Purpose", 
             h3("Why be interested in this topic?"), 
             p("During the pandemic, we saw how negatively the airline industry 
               was impacted as country after country went into lockdown. The 
               unfathomable outbreak at the time barred anyone from even going 
               out of their houses as scientists continuously looked for solutions 
               to minimize the risk of the virus. People who frequently traveled 
               from city to city for work were now constrained in their homes. 
               Those who flew to visit their relatives could only meet through 
               their screens. Those who planned their vacations had to scrap 
               their plans of a relaxing trip overseas. Consequently, airline 
               travel hit an all-time low ever since its rise. Businesses 
               formulated significant changes to accommodate the new lifestyle 
               every one adopted during the pandemic, including the airline industry. 
               As vaccinations became more accessible, businesses began to open up 
               their operations again centering around the new norm. As the world 
               slowly works its way through this new norm, the after-effects of 
               the pandemic are still ongoing. For the airline industry, the 
               number of passengers traveling by air for the year 2023 has yet 
               to reach the number of passengers before the pandemic. "),
             br(),
             p("With that, we focused our research on the effects of the pandemic 
               on the airline industry in the United States as there have been 
               tremendous changes, apparent or not, indirectly impacting us, the 
               consumers, as well. At one point, more and more people are 
               familiarizing themselves again with flying and compensating for 
               the time they were restricted from doing so. Airlines have seemed 
               to offer more leniency and options with the selling of their tickets. 
               Despite that, the number of travelers post-pandemic have yet to 
               catch up to those of pre-pandemic. We would like to find the 
               correlation between the COVID-19 cases and the number of passengers, 
               domestically and internationally, utilizing the data sets we found 
               on both topics respectively. We aim to recognize and seek answers 
               to the patterns we observe from them in order to come up with a 
               conclusion to the topic of our research.")),
    
    
    "Looking further",
    
    ## JIN
    tabPanel("Pre-COVID-19 conditions",
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
             ), ## end of p1
    
## BIG WALL FOR MY SANITY
    
    
    ## ANDY
    tabPanel("COVID-19 conditions"),
    

## BIG WALL FOR MY SANITY
    
    ## p3
    tabPanel("Post-COVID-19 conditions",
             h1("A look into the situation after lockdown"),
             h3("A lingering question might be: 
                How have the norms changed? What are the current rules for travel?"),
             p("To say that that the travel industry has been heavily affected is an understatement."),
             br(),
             p("The changes in the airline industry can be observed financially, logistically, socially, etc. 
               To understand the impacts that the COVID-19 pandemic and lockdown had on the travel industry after its disruption, 
               pick an aspect to look further into it!"),
             br(),
             selectInput(inputId = "post", 
                         label = "Choose an aspect for more information:",
                         choices = list("Financial" = 1, 
                                        "Logistics" = 2,
                                        "Social" = 3)),
             htmlOutput(outputId = "post_out")
             ) ## end of p3
  ) ## end of navlistPanel
) ## end of UI

server <- function(input, output){
  
  #### page 1
  output$finYear <- renderText({
    paste("The Net Profit for ", input$Choosen_year ," is ", get_netProfit(input$Choosen_year), " in millions.")
  })
  ##1
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
  ##1
  output$yearTable <- renderTable(
    yearSelected <- filter(findf_cut, Year == input$Choosen_year)
  )
  #### end of p1
  
  
  
#### BIG WALL FOR MY SANITY
  
  
  
  #### page 3
  output$post_out <- renderUI({
    if (input$post == 1) {
      text_out <- (p(("As reported by "), a("the US Department of Treasury", 
                                           href = "https://home.treasury.gov/policy-issues/coronavirus/assistance-for-industry/loans-to-air-carriers-eligible-businesses-and-national-security-businesses"),
                    (" in 2021, the 4003 Loan Program (established by the CARES Act) were authorized to give out loans to eligible businesses in regards to the losses they
                    faced due to the pandemic. Household names of the national airline industry like Alaska Airlines and United Airlines were named as some of the businesses 
                    that benefitted from this program to keep their businesses running during the lockdown. As people were banned from travelling, where else could they source 
                    their income from? These airline companies being in debt and taking out loans are part of the reasons that airplane tickets are higher than they previously 
                    were. They have to make enough to cover the losses incurred while simultaneously pay back the loan while also struggle to gain profit. As business travelers have
                    yet to be back in its full force, airlines have to find new pricing ways to accommodate this change in target audience.")))
    } else if (input$post == 2) {
      text_out <- (p(("An after-effect of COVID-19 restrictions where social distancing was encouraged and sharing of belongings were discouraged, contactless payment became prominent as
                      a means for transaction to avoid the spread of the disease. Both people and businesses have accustomed to this method, hence this payment method is still utilized 
                     even after lockdown restrictions were lifted due to the convenience for both parties. For the airline industry, businesses abide by these restrictions by introducing
                     contactless payment methods and managed to come up with new business implementations, such as the in-flight meal preorder options. "), a("Although United Airlines rolled 
                     this option out a little before the virus shocked the world,", href = "https://simpleflying.com/united-airlines-meal-preorder/"), (" it definitely popularized as a common 
                     practice for airlines due to the pandemic.")))
    
      } else if (input$post == 3) {
      text_out <- (p(("For about two years since the start of the pandemic, social distancing measures were enforced. As a result of this enforcement along with the scare of getting the virus,
                      people generally were afraid to be outside of the comforts of their homes, where the disease is guaranteed not to be as long as no one gets infected. This isolation for 
                     a long period of time after having used to going out every day even for the smallest of errands affected people's confidence in interacting with others, what more with traveling
                      to another place where they have to share a cabin for the whole travel time. Even though an increase in international travel compared to 2020 is seen, "), a("arrivals in America 
                      only account for about 50% of the pre-pandemic levels.", href = "https://www.economicsobservatory.com/update-how-is-covid-19-affecting-international-travel-and-tourism"), ("Another 
                     important thing to note is the rise of virtual meetings. To adapt to the circumstances at the time, video calling became a core solution to the communication issue that arised. Those
                      who previously traveled every three days for business meetings now only have to wake up, switch on their laptops and join the meeting link provided before going about their usual 
                     business. Ever since, corporates have seen how cost-saving and efficient these virtual meetings are that most of them proceeded with this method even after the lockdown. No more needing
                      to constantly travel just for one meeting. Indirectly, the travel industry lost these valuable patrons as business travelers now travel less frequently than they have before.")))
    } 
    
    return(text_out)
  }) ## end of p3 drop down menu
  
  
} ## end of server

shinyApp(ui = ui, server = server)
