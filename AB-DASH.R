library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)


library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "SEMISTER ABSENTIES ",
                          titleWidth = 350,
                          dropdownMenu(type = "message",
                                       #we also upload directly from notepad#
                                    messageItem(from = "ANU",message ="1st-3rd-5th sem supply exam ",time = "22:00"),
                                    messageItem(from = "HOD",message ="BBC Student has 100% attendance", icon = icon("address-card"),time = "01-01-2018"),
                                    messageItem(from = "PRINCIPAL",message ="students has to requested to pay fee",time = "01-01-2018")
                                       ),
                          dropdownMenu(type = "notification",
                                       notificationItem(
                                         text = "Exams coming soon",
                                         icon = icon("dashboard"),
                                         status = "success"
                                       ),
                                       notificationItem(
                                         text = "maintain deciplane in campus",
                                         icon = icon("warning"),
                                         status = "warning"
                                       )),
                                       
                          dropdownMenu(type = "tasks",
                                      taskItem(
                                       value = 100,
                                       color = "aqua",
                                       "BBC"
                                       ),
                                      taskItem(
                                        value = 80,
                                        color = "red",
                                        "BCA"
                                      ),
                                      taskItem(
                                        value = 75,
                                        color = "yellow",
                                        "MPC"
                                      ),
                                      taskItem(
                                        value = 65,
                                        color = "green",
                                        "MPCS"
                                      ),
                                      taskItem(
                                        value = 60,
                                        color = "blue",
                                        "MSCS"
                                      )           
                                      )
                                       
          
)     
                          

sidebar <- dashboardSidebar(
  width = 350,
  # Custom CSS to hide the default logout panel
 
  # The dynamically-generated user panel
 
  sidebarMenu(
   
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
   
    menuItem("Absenties SEM-1", tabName = "Absenties1", icon = icon("bar-chart"), badgeLabel = "SEM1", badgeColor = "green"),
    menuItem("Absenties SEM-2", tabName = "Absenties2", icon = icon("bar-chart"), badgeLabel = "SEM2", badgeColor = "green"),
    menuItem("Absenties SEM-3", tabName = "Absenties3", icon = icon("bar-chart"), badgeLabel = "SEM3", badgeColor = "green"),
    menuItem("Absenties SEM-4", tabName = "Absenties4", icon = icon("bar-chart"), badgeLabel = "SEM5", badgeColor = "green"),
    menuItem("Absenties SEM-5", tabName = "Absenties5", icon = icon("bar-chart"), badgeLabel = "SEM5", badgeColor = "green"),
     menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
             href = "https://vignandegreecollege.com/tproforma/reg12.php"),
    sliderInput("bins","Number of Breaks",1,100,50),
    textInput("text", "Text input:")
  )
)

frow2 <- fluidRow( 
  
  box(
    title = "ABSENTIES sem-1", background = "black"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("absenties", height = "300px")
  )
)
frow3 <- fluidRow(
  box(
    title = "ABSENTIES sem-2", background = "purple"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("absentis", height = "300px")
  ) 
)
frow4 <- fluidRow(
  box(
    title = "ABSENTIES sem-3", background = "blue"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("absents", height = "300px")
  ) 
)
frow5 <- fluidRow(
  box(
    title = "ABSENTIES sem-4", background = "maroon"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("absens", height = "300px")
  ) 
  )
frow6 <- fluidRow(
  box(
    title = "ABSENTIES sem-5", background = "red"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("absen", height = "300px")
  )
  )


body <- dashboardBody(
  tags$head(tags$style(HTML('
  .main-header .logo {
    font-family: "Georgia", Times, "Times New Roman", serif;
    font-weight: bold;
    font-size: 24px;
  }
  '))),
  
  tabItems(
    tabItem(tabName = "Absenties1",
            h2("SEM-1 ABSENTIES"),
            frow2
    ),
    
    tabItem(tabName = "Absenties2",
            h2("SEM-2 ABSENTIES"),
            frow3
    ),
    
    tabItem(tabName = "Absenties3",
            h2("SEM-3 ABSENTIES"),
            frow4
    ),
    
    tabItem(tabName = "Absenties4",
            h2("SEM-4 ABSENTIES"),
            frow5
    ),
    
    tabItem(tabName = "Absenties5",
            h2("SEM-5 ABSENTIES"),
            frow6
  )
 
  )
  
  )
ui <- dashboardPage(title = "MY PROJECT", header, sidebar, body, skin='red')

 
#server begins

server <- function(input, output) {
  
  
  #ggplot for sem1
  
  output$absenties <- renderPlot({
    ggplot(data=firstsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()+ ggtitle("BOYS AND GIRLS FIRST SEM \n ABSENTIES(5 GROUPS)")+
      theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
             axis.title.x = element_text(color="blue", size=14, face="bold"),
             axis.title.y = element_text(color="#993333", size=14, face="bold"))
    })
 
  #ggplot for sem2
  
  output$absentis <- renderPlot({
  ggplot(data=secondsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5)+
    scale_fill_brewer(palette="Paired")+
    theme_minimal()+ ggtitle("BOYS AND GIRLS SECOND SEM\n ABSENTIES(5 GROUPS)")+
    theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
           axis.title.x = element_text(color="blue", size=14, face="bold"),
           axis.title.y = element_text(color="#993333", size=14, face="bold"))
  })
  
  #ggplot for sem3
  
  output$absents <- renderPlot({
    ggplot(data=thirdsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()+ ggtitle("BOYS AND GIRLS THIRD SEM\n ABSENTIES(5 GROUPS)")+
      theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
             axis.title.x = element_text(color="blue", size=14, face="bold"),
             axis.title.y = element_text(color="#993333", size=14, face="bold"))
   
  })
 
  #ggplot for sem4
  
  output$absens <- renderPlot({
    ggplot(data=fourthsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()+ ggtitle("BOYS AND GIRLS FOURTH SEM\n ABSENTIES(5 GROUPS)")+
      theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
             axis.title.x = element_text(color="blue", size=14, face="bold"),
             axis.title.y = element_text(color="#993333", size=14, face="bold"))
    
  })
  
  #ggplot for sem5
  
  output$absen <- renderPlot({
    ggplot(data=fifthsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()+ ggtitle("BOYS AND GIRLS FIFTH SEM\n PASS PERCENTAGES(5 GROUPS)")+
      theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
             axis.title.x = element_text(color="blue", size=14, face="bold"),
             axis.title.y = element_text(color="#993333", size=14, face="bold"))
    
  })
}

shinyApp(ui, server)
