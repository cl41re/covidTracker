library(shiny)
library(dplyr)
library(qdap)
library(ggplot2)

cases = read.csv('https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_state.csv')
cases = tail(cases, n = 112)
cases <- cases[-c(7:25)]
cases$cases_active <- abs(cases$cases_active)


death = read.csv('https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/deaths_state.csv')
death = tail(death, n=112)
death$death = rowSums(death[,c(3:11)])
death <- death[-c(3:11)]

hospital = read.csv('https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/hospital.csv')
hospital = tail(hospital, n = 112)
hospital <- hospital[-c(3:5,12:14)]

cluster = read.csv('https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/clusters.csv')
cluster = tail(cluster, n = 5)
cluster <- cluster[-c(5:6, 8:15)]

Stateindex <- data.frame(ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
                         ,statename=c("Johor","Kedah","Kelantan","Melaka","Negeri Sembilan","Pahang","Pulau Pinang","Perak","Perlis","Selangor","Terengganu","Sabah","Sarawak","Kuala Lumpur","Labuan","Putrajaya"))
cluster <- cluster %>%  
  mutate(state = as.numeric(state))
cluster$state <- lookup(cluster$state, Stateindex)




ui<- fluidPage(
  shinyUI(navbarPage("Covid Tracker",
                     tabPanel("About",
                              mainPanel(textOutput("about"),
                                        textOutput("about2"))
                     ),
                     
                     
                     
                     tabPanel("Covid Details",
                              sidebarPanel( 
                                selectInput("State", label = "State", choices = unique(cases$state), selected = "Johor")
                              ),
                              
                              mainPanel(h2("Information of the selected states"),
                                        verbatimTextOutput("State"),
                                        
                                        h3("Cases detail:"),
                                        tableOutput('table'),
                                        h4("Average Active Case in a week:"),
                                        textOutput('averagecase'),
                                        h4("Highest Active Case in a week:"),
                                        textOutput('maxcase'),
                                        h4("Lowest Active Case in a week:"),
                                        textOutput('mincase'),
                                        
                                    
                                        
                                        
                              )
                     ),
                     
                     tabPanel("Death",
                              sidebarPanel( 
                                selectInput("DeathState", label = "State", choices = unique(death$state), selected = "Johor")
                              ),
                              mainPanel(h2("Information of the selected states"),
                                         verbatimTextOutput("DeathState"),
                                        h3("Death Detail:"),
                                        plotOutput('Deathplot'),
                                       
                                        h4("Highest Death in a week:"),
                                        textOutput('maxdeath'),
                                        h4("Lowest Death in a week:"),
                                        textOutput('mindeath'),)
                     ),
                     
                     tabPanel("Hospital Utilization",
                              sidebarPanel( 
                                selectInput("HospitalState", label = "State", choices = unique(hospital$state), selected = "Johor")
                              ),
                              mainPanel(h2("Information of the selected states"),
                                        verbatimTextOutput("HospitalState"),
                                        h3("Hospitalization:"),
                                        plotOutput('Hospitalplot'),
                                        
                                        h4("Highest admitted in a week:"),
                                        textOutput('maxhospital'),
                                        h4("Lowest admitted in a week:"),
                                        textOutput('minhospital'),)
                     ),
                     
                     tabPanel("Cluster",
                              mainPanel(h2("In this panel, we show you the details of 5 latest cluster of Covid-19 in Malaysia"),
                                        h3("Cluster 1:"),
                                        h4("Name"),
                                        textOutput('Cluster1name'),
                                        h4("State"),
                                        textOutput('Cluster1state'),
                                        h4("District"),
                                        textOutput('Cluster1district'),
                                        h4("Date"),
                                        textOutput('Cluster1date'),
                                        h4("Status"),
                                        textOutput('Cluster1status'),
                                        h4("Summary"),
                                        textOutput('Cluster1summary'),
                                        
                                        h3("Cluster 2:"),
                                        h4("Name"),
                                        textOutput('Cluster2name'),
                                        h4("State"),
                                        textOutput('Cluster2state'),
                                        h4("District"),
                                        textOutput('Cluster2district'),
                                        h4("Date"),
                                        textOutput('Cluster2date'),
                                        h4("Status"),
                                        textOutput('Cluster2status'),
                                        h4("Summary"),
                                        textOutput('Cluster2summary'),
                                        
                                        h3("Cluster 3:"),
                                        h4("Name"),
                                        textOutput('Cluster3name'),
                                        h4("State"),
                                        textOutput('Cluster3state'),
                                        h4("District"),
                                        textOutput('Cluster3district'),
                                        h4("Date"),
                                        textOutput('Cluster3date'),
                                        h4("Status"),
                                        textOutput('Cluster3status'),
                                        h4("Summary"),
                                        textOutput('Cluster3summary'),
                                        
                                        h3("Cluster 4:"),
                                        h4("Name"),
                                        textOutput('Cluster4name'),
                                        h4("State"),
                                        textOutput('Cluster4state'),
                                        h4("District"),
                                        textOutput('Cluster4district'),
                                        h4("Date"),
                                        textOutput('Cluster4date'),
                                        h4("Status"),
                                        textOutput('Cluster4status'),
                                        h4("Summary"),
                                        textOutput('Cluster4summary'),
                                        
                                        h3("Cluster 5:"),
                                        h4("Name"),
                                        textOutput('Cluster5name'),
                                        h4("State"),
                                        textOutput('Cluster5state'),
                                        h4("District"),
                                        textOutput('Cluster5district'),
                                        h4("Date"),
                                        textOutput('Cluster5date'),
                                        h4("Status"),
                                        textOutput('Cluster5status'),
                                        h4("Summary"),
                                        textOutput('Cluster5summary'),)
                     ),
                     
                     tabPanel("Prediction",
                              sidebarPanel( 
                                selectInput("PredictState", label = "State", choices = unique(cases$state), selected = "Johor")
                              ),
                              mainPanel(h2("In this panel, we show you the prediction Covid-19 in Malaysia in the next day"),
                                        h3("Selected State:"),
                                        textOutput('PredictState'),
                                        h3("Prediction of the next case detail:"),
                                        h4("New Case:"),
                                        textOutput('prenew'),
                                        )
                     ),
  ))
)



Date <- function () {return(death[1,1])}



CaseStateDetail <- function(st){
  casedetail <- filter(cases, state == st)
  colnames(casedetail) <- c('Date','State','New_Cases', 'Import_Case', 'Recovered', 'Active_Case' )
  return (casedetail)
}

CaseStateAverageDetail <- function(st){
  casedetail <- filter(cases, state == st)
  average <- mean(casedetail$cases_active)
  average <- trunc(average)
  return (average)
}

CaseStateModDetail <- function(st){
  casedetail <- filter(cases, state == st)
  Max <- max(casedetail$cases_active)
  
  return (Max)
}

CaseStateMinDetail <- function(st){
  casedetail <- filter(cases, state == st)
  Min <- min(casedetail$cases_active)

  return (Min)
}

DeathStateModDetail <- function(st){
  deathdetail <- filter(death, state == st)
  Max <- max(deathdetail$death)
  return (Max)
}

DeathStateMinDetail <- function(st){
  deathdetail <- filter(death, state == st)
  Min <- min(deathdetail$death)
  return (Min)
}

DeathStateplot <- function(st){
  deathplotdetail <- filter(death, state == st)
  deathplotdetail$date <- as.Date(deathplotdetail$date)
  
  plots <- ggplot(deathplotdetail, aes(x = date, y=death, color = state)) + 
    geom_line(lwd = 1.5) + 
    scale_x_date(date_labels="%b %d",date_breaks  ="1 day") +
    labs(title = "Number of Death against the Date",
         x = "Date",
         y = "Death",
         colour = "State")
    
  
  return(plots)
}

HospitalStateplot <- function(st){
  hospitalplotdetail <- filter(hospital, state == st)
  hospitalplotdetail$date <- as.Date(hospitalplotdetail$date)
  
  plots <- ggplot(hospitalplotdetail, aes(x = date, y=admitted_total, color = state)) + 
    geom_line(lwd = 1.5) + 
    scale_x_date(date_labels="%b %d",date_breaks  ="1 day") +
    labs(title = "Number of Hospitalize against the Date",
         x = "Date",
         y = "Admitted",
         colour = "State")
  
  return(plots)
}

HospitalStateModDetail <- function(st){
  hospitaldetail <- filter(hospital, state == st)
  Max <- max(hospitaldetail$admitted_total)
  return (Max)
}

HospitalStateMinDetail <- function(st){
  hospitaldetail <- filter(hospital, state == st)
  Min <- min(hospitaldetail$admitted_total)
  return (Min)
}

Predictnew <- function(st){
  casedetail <- filter(cases, state == st)
  
  model <- lm(cases_new ~ cases_active, data=casedetail)
  
  prevalue = predict(model, newdata = data.frame(cases_active = casedetail$cases_active))
  
  return(mean(prevalue))

}



server <- function(input,output) {
  output$about <- renderText("Welcome to the Covid Tracker. In this app, we keep track of the COvid-19 cases and detail
                             in Malaysia. This app was created by Group 16 of Introduction to Data Science under the guidance
                             of our lecturer, Dr Salimah. The data that is used in this apps will be always up-to-date since the
                             data we used is from the Malaysia Government Open Data. Hope you enjoy our apps!")
  
  output$about2 <- renderText("The app has 5 tabs which are Covid Details, Death, Hospital Utillization, Cluster and prediction.
                                In the covid details tab, user should pick the state they wish to view.")
  output$State <- renderPrint(input$State)
  output$averagecase <- renderText(CaseStateAverageDetail(input$State))
  output$maxcase <- renderText((CaseStateModDetail(input$State)))
  output$mincase <- renderText(CaseStateMinDetail(input$State))
  
  output$Date<- renderPrint({Date()})
  output$table <- renderTable(CaseStateDetail(input$State))
  
  output$Death <- renderTable(DeathStateDetail(input$DeathState))
  output$Deathplot <- renderPlot(DeathStateplot(input$DeathState))
  output$maxdeath <- renderText(DeathStateModDetail(input$DeathState))
  output$mindeath <- renderText(DeathStateMinDetail(input$DeathState))
  
  output$Hospitalplot <- renderPlot(HospitalStateplot(input$HospitalState))
  output$maxhospital <- renderText(HospitalStateModDetail(input$HospitalState))
  output$minhospital <- renderText(HospitalStateMinDetail(input$HospitalState))
  
  output$Cluster1name <- renderText(cluster[1,1])
  output$Cluster1state <- renderText(cluster[1,2])
  output$Cluster1district <- renderText(cluster[1,3])
  output$Cluster1date <- renderText(cluster[1,4])
  output$Cluster1status <- renderText(cluster[1,5])
  output$Cluster1summary <- renderText(cluster[1,6])
  
  output$Cluster2name <- renderText(cluster[2,1])
  output$Cluster2state <- renderText(cluster[2,2])
  output$Cluster2district <- renderText(cluster[2,3])
  output$Cluster2date <- renderText(cluster[2,4])
  output$Cluster2status <- renderText(cluster[2,5])
  output$Cluster2summary <- renderText(cluster[2,6])
  
  output$Cluster3name <- renderText(cluster[3,1])
  output$Cluster3state <- renderText(cluster[3,2])
  output$Cluster3district <- renderText(cluster[3,3])
  output$Cluster3date <- renderText(cluster[3,4])
  output$Cluster3status <- renderText(cluster[3,5])
  output$Cluster3summary <- renderText(cluster[3,6])
  
  output$Cluster4name <- renderText(cluster[4,1])
  output$Cluster4state <- renderText(cluster[4,2])
  output$Cluster4district <- renderText(cluster[4,3])
  output$Cluster4date <- renderText(cluster[4,4])
  output$Cluster4status <- renderText(cluster[4,5])
  output$Cluster4summary <- renderText(cluster[4,6])
  
  output$Cluster5name <- renderText(cluster[5,1])
  output$Cluster5state <- renderText(cluster[5,2])
  output$Cluster5district <- renderText(cluster[5,3])
  output$Cluster5date <- renderText(cluster[5,4])
  output$Cluster5status <- renderText(cluster[5,5])
  output$Cluster5summary <- renderText(cluster[5,6])
  
  output$prenew <- renderText(Predictnew(input$PredictState))
  output$preimport <- renderText(Predictnew(input$PredictState))
  output$preactive <- renderText(Predictnew(input$PredictState))
  
}

shinyApp(ui=ui, server=server) 