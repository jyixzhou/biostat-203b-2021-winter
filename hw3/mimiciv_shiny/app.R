# Shiny app for data visualization of MIMIC-IV

library(shiny)
library(ggplot2)
library(tidyverse)
library(stringr)
library(scales)
#library(RColorBrewer)

icu <- readRDS("./icu_cohort.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("MIMIC-IV Visualization"),
  
  tabsetPanel(
    
    tabPanel("Demographics", 
             sidebarLayout(
               
               sidebarPanel(
                 
                 helpText("Choose a demographic characteristic and examine its distribution."),
                 selectInput("demo", 
                             label = "Choose from the following:",
                             choices = c("age", "gender", "ethnicity", 
                                         "language", "marital_status", 
                                         "insurance")
                 )
               ),
               
               mainPanel(
                 plotOutput("demoPlot"),
                 plotOutput("demoPlot2")
               )
             )
    ),
    
    
    tabPanel("Labs", 
             sidebarLayout(
               
               sidebarPanel(
                 
                 helpText("Choose a routine lab measurement and examine its distribution."),
                 selectInput("lab",
                             label = "Choose from the following:",
                             choices = c("bicarbonate", "calcium", "chloride", 
                                         "creatinine", "glucose", "magnesium", 
                                         "potassium", "sodium", "hemotocrit", 
                                         "wbc", "lactate")
                 ),
                 
                 radioButtons("labBy",
                              label = "Analyze by:",
                              choices = c("gender", 
                                          "age_group",
                                          "death_within_30days"
                              )
                 ),
                 
                 checkboxInput("log", 
                               label = "log transformation", 
                               value = FALSE)
                 
                 
               ),
               
               mainPanel(
                 textOutput("labNAs"),
                 plotOutput("labBoxplot"),
                 plotOutput("labBoxBy")
               )
             )
    ),
    
    
    tabPanel("Vitals", 
             sidebarLayout(
               
               sidebarPanel(
                 
                 helpText("Choose a routine vital measurement and examine its distribution."),
                 
                 selectInput("vital",
                             label = "Choose from the following lab measurements:",
                             choices = c("heart_rate", 
                                         "respiratory_rate", 
                                         "temperature_fahrenheit", 
                                         "non_invasive_blood_pressure_systolic", 
                                         "non_invasive_blood_pressure_mean", 
                                         "arterial_blood_pressure_systolic", 
                                         "arterial_blood_pressure_mean" )
                 ),
                 
                 
                 radioButtons("vitalBy",
                              label = "Analyze by:",
                              choices = c("gender", 
                                          "age_group",
                                          "death_within_30days")
                 ),
                 
                 checkboxInput("logv", 
                               label = "log transformation", 
                               value = FALSE)
                 
               ),
               
               mainPanel(
                 textOutput("vitalNAs"),
                 plotOutput("vitalBoxplot"),
                 plotOutput("vitalBoxBy")
               )
             )
    )
  )
  
)


server <- function(input, output) {
  
  #draw histogram/barplots + piecharts for the demographics tab
  output$demoPlot <- renderPlot({
    
    if (input$demo == "age") {
      
      ggplot(icu, aes(age_at_adm)) + 
        geom_bar(fill = "paleturquoise4") +
        labs(x = "age at dmission")
      
    } else {
      
      ggplot(icu, aes_string(input$demo)) +
        geom_bar(aes_string(fill = input$demo)) +
        theme(legend.position = "none") +
        coord_flip()
      
    }
  })
  
  
  #draw piecharts for the demographics tab
  output$demoPlot2 <- renderPlot({
    
    if (input$demo == "age") {
      
      icu_age <- icu %>%
        select(subject_id, age_at_adm) %>%
        mutate(age_group = case_when(
          age_at_adm < 30 ~ "18 ~ 30",
          age_at_adm >= 30 & age_at_adm < 50 ~ "30 ~ 50",
          age_at_adm >= 50 & age_at_adm < 70 ~ "50 ~ 70",
          age_at_adm >= 70 & age_at_adm < 90 ~ "70 ~ 90",
          age_at_adm >= 90 ~ "90+"))
      
      ggplot(icu_age, aes(x = factor(1))) +
        geom_bar(aes(fill = age_group), width = 1) +
        coord_polar("y") +
        theme_void()
      
    } else {
      
      ggplot(icu, aes(x = factor(1))) +
        geom_bar(aes_string(fill = input$demo), width = 1) +
        coord_polar("y") +
        theme_void()
      
    }
  })
  
  
  
  #labs tab: display number of NAs
  
  output$labNAs <- renderText({
    
    str_c("There are", 
          icu %>%
            select(toString(input$lab)) %>%
            summarize(sum(is.na(.))) %>%
            toString(),
          "missing values for the", 
          toString(input$lab), 
          "measurement.",
          sep = " ")
    
  })
  
  #labs tab: display overall boxplot
  output$labBoxplot <- renderPlot({
    
    pob <- ggplot(icu, aes_string(input$lab)) +
      geom_boxplot(fill = "paleturquoise4")
    
    if (input$log) {
      pob <- pob + scale_x_log10()
    }
    
    pob
    
  })
  
  #labs tab: display boxplot by group
  output$labBoxBy <- renderPlot({
    
    if (input$labBy == "age_group") {
      
      icu_age <- icu %>%
        select(subject_id, age_at_adm, toString(input$lab)) %>%
        mutate(age_group = case_when(
          age_at_adm < 30 ~ "18 ~ 30",
          age_at_adm >= 30 & age_at_adm < 50 ~ "30 ~ 50",
          age_at_adm >= 50 & age_at_adm < 70 ~ "50 ~ 70",
          age_at_adm >= 70 & age_at_adm < 90 ~ "70 ~ 90",
          age_at_adm >= 90 ~ "90+"))
      
      pbb <- ggplot(icu_age, aes_string(input$labBy, input$lab)) +
        geom_boxplot(aes_string(fill = input$labBy)) +
        theme(legend.position = "none")
      
      if (input$log) {
        pbb <- pbb + scale_y_log10()
      }
      
      pbb
      
    }else{
      
      pbb <- ggplot(icu, aes_string(input$labBy, input$lab)) +
        geom_boxplot(aes_string(fill = input$labBy)) +
        theme(legend.position = "none")
      
      if (input$log) {
        pbb <- pbb + scale_y_log10()
      }
      
      pbb
      
    }
    
    
  })
  
  #vitals tab: display number of NAs
  
  output$vitalNAs <- renderText({
    
    str_c("There are", 
          icu %>%
            select(toString(input$vital)) %>%
            summarize(sum(is.na(.))) %>%
            toString(),
          "missing values for the", 
          toString(input$vital), 
          "measurement.",
          sep = " ")
    
  })
  
  #vitals tab: display overall boxplot
  output$vitalBoxplot <- renderPlot({
    
    pvo <- ggplot(icu, aes_string(input$vital)) +
      geom_boxplot(fill = "paleturquoise4")
    
    if (input$logv) {
      pvo <- pvo + scale_x_log10()
    }
    
    pvo
    
  })
  
  #vitals tab: display boxplot by group
  output$vitalBoxBy <- renderPlot({
    
    if (input$vitalBy == "age_group") {
      
      icu_age <- icu %>%
        select(subject_id, age_at_adm, toString(input$vital)) %>%
        mutate(age_group = case_when(
          age_at_adm < 30 ~ "18 ~ 30",
          age_at_adm >= 30 & age_at_adm < 50 ~ "30 ~ 50",
          age_at_adm >= 50 & age_at_adm < 70 ~ "50 ~ 70",
          age_at_adm >= 70 & age_at_adm < 90 ~ "70 ~ 90",
          age_at_adm >= 90 ~ "90+"))
      
      pvb <- ggplot(icu_age, aes_string(input$vitalBy, input$vital)) +
        geom_boxplot(aes_string(fill = input$vitalBy)) +
        theme(legend.position = "none")
      
      if (input$logv) {
        pvb <- pvb + scale_y_log10()
      }
      
      pvb
      
    }else{
      
      pvb <- ggplot(icu, aes_string(input$vitalBy, input$vital)) +
        geom_boxplot(aes_string(fill = input$vitalBy)) +
        theme(legend.position = "none")
      
      if (input$logv) {
        pvb <- pvb + scale_y_log10()
      }
      
      pvb
      
      
    }
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
