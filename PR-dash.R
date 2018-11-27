
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)


library(shiny)
library(shinydashboard)
library(shiny)


header <-dashboardHeader(title = "SEMESTER PRESENTIES ",
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
  tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
  
  # Custom CSS to hide the default logout panel

  # The dynamically-generated user panel

  sidebarMenu(

   
        
    menuItem("Presenties SEM-1", tabName = "Presenties1", icon = icon("bar-chart"), badgeLabel = "SEM1", badgeColor = "green"),
    menuItem("Presenties SEM-2", tabName = "Presenties2", icon = icon("bar-chart"), badgeLabel = "SEM2", badgeColor = "green"),
    menuItem("Presenties SEM-3", tabName = "Presenties3", icon = icon("bar-chart"), badgeLabel = "SEM3", badgeColor = "green"),
    menuItem("Presenties SEM-4", tabName = "Presenties4", icon = icon("bar-chart"), badgeLabel = "SEM5", badgeColor = "green"),
    menuItem("Presenties SEM-5", tabName = "Presenties5", icon = icon("bar-chart"), badgeLabel = "SEM5", badgeColor = "green"),
    menuItem("Absenties data", icon = icon("send",lib='glyphicon'),
             href = "https://myprojec.shinyapps.io/RDASH")
  )
 
)

frow2 <- fluidRow(
  
 

  box(
    title = "Presenties sem-1",width = "500px", background = "black"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Presenties",width = "970px", height = "500px")
  )
  

)
frow3 <- fluidRow(
  box(
    title = "Presenties sem-2", background = "purple"
    ,status = "primary"
    
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Presentie", height = "300px")
  )
)
frow4 <- fluidRow(

        
          
  
  box(
    title = "Presenties sem-3", background = "blue"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Presenti", height = "300px")
  )
)
frow5 <- fluidRow(
  box(
    title = "Presenties sem-4", background = "maroon"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Present", height = "300px")
  )
)
frow6 <- fluidRow(
  box(
    title = "Presenties sem-5", background = "red"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Presen", height = "300px")
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
    tabItem(tabName = "Presenties1",
            h2("SEM-1 Presenties"),
            frow2
    ),

    tabItem(tabName = "Presenties2",
            h2("SEM-2 Presenties"),
            frow3
    ),

    tabItem(tabName = "Presenties3",
            h2("SEM-3 Presenties"),
            frow4
    ),

    tabItem(tabName = "Presenties4",
            h2("SEM-4 Presenties"),
            frow5
    ),

    tabItem(tabName = "Presenties5",
            h2("SEM-5 Presenties"),
            frow6
    )

  )

  )
ui <-dashboardPage(title = "MY PROJECT", header, sidebar, body, skin='yellow')


#server begins

server <- function(input, output) {




  #ggplot for sem1
  output$Presenties <- renderPlot({
    
  ggplot(data=firstsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5)+
    scale_fill_brewer(palette="Paired")+
    theme_minimal()+ ggtitle("BOYS AND GIRLS FIRST SEM \n PASS PERCENTAGES(5 GROUPS)")+
    theme( plot.title = element_text(color="red", size=14, face="bold.italic"),
           axis.title.x = element_text(color="blue", size=14, face="bold"),
           axis.title.y = element_text(color="#993333", size=14, face="bold"))

  })

  #ggplot for sem2

  output$Presentie <- renderPlot({
    ggplot(data=secondsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()+ ggtitle("BOYS AND GIRLS SECOND SEM\n PASS PERCENTAGES(5 GROUPS)")+
      theme( plot.title = element_text(color="red", size=14, face="bold.italic"),
             axis.title.x = element_text(color="blue", size=14, face="bold"),
             axis.title.y = element_text(color="#993333", size=14, face="bold"))

  })

  #ggplot for sem3

  output$Presenti <- renderPlot({

    ggplot(data=thirdsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()+ ggtitle("BOYS AND GIRLS THIRD SEM\n PASS PERCENTAGES(5 GROUPS)")+
      theme( plot.title = element_text(color="red", size=14, face="bold.italic"),
             axis.title.x = element_text(color="blue", size=14, face="bold"),
             axis.title.y = element_text(color="#993333", size=14, face="bold"))



  })

  #ggplot for sem4

    output$Present <- renderPlot({
      ggplot(data=fourthsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette="Paired")+
        theme_minimal()+ ggtitle("BOYS AND GIRLS FOURTH SEM\n PASS PERCENTAGES(5 GROUPS)")+
        theme( plot.title = element_text(color="red", size=14, face="bold.italic"),
               axis.title.x = element_text(color="blue", size=14, face="bold"),
               axis.title.y = element_text(color="#993333", size=14, face="bold"))


  })

  #ggplot for sem5

  output$Presen<- renderPlot({
    ggplot(data=fifthsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()+ ggtitle("BOYS AND GIRLS FIFTH SEM\n PASS PERCENTAGES(5 GROUPS)")+
      theme( plot.title = element_text(color="red", size=14, face="bold.italic"),
             axis.title.x = element_text(color="blue", size=14, face="bold"),
             axis.title.y = element_text(color="#993333", size=14, face="bold"))
  })




  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
}
shinyApp(ui, server)
