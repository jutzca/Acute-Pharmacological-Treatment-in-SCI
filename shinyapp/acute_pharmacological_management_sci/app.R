## ---------------------------
##
## Script name: 1_shiny_web_application
##
## Purpose of script: Code underlying the shiny web application to visualize the acute pharmacological management after spinal cord injury.
##                    
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-01-08
##
## Copyright (c) Catherine Jutzeler, 2021
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: Sygen clinical trial and SCI Rehab study
##
## Notes: This app accompanies the publication of Jutzeler et al, 2021 published in XX. [add link here]
##      
## ---------------------------
##
## Load up the packages required
##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)
library(DT)
library(shinyWidgets)
library(png)
library(plotly)
library(splitstackshape)
library(RColorBrewer)
library(stringr)
library(ggnetwork)
library(networkD3)
library(igraph)
library(intergraph)
library(sna)
library(shinyjs)
library(metathis)
library(r2d3)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(shiny)){install.packages("shiny")}
# if(!require(shinydashboard)){install.packages("shinydashboard")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(tidyr)){install.packages("tidyr")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(stats)){install.packages("stats")}
# if(!require(ggthemes)){install.packages("ggthemes")}
##
## ---------------------------
##
## R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
##
## ---------------------------
##
## Set working directory 
setwd("/Users/jutzelec/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/shinyapp/acute_pharmacological_management_sci/")
##
## ---------------------------
##
## Set output directorypaths
outdir_figures='/Users/jutzelec/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures'
outdir_tables='/Users/jutzelec/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables'
##
## ---------------------------
##
#Set local system
Sys.setlocale('LC_ALL','C') 
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####


#---------- Source helper functions ----------# 
source("helper_functions_2.R")


#### ---------------------------
# load data:
# data creation not possible in docker container (i.e., on server)
# if (!file.exists("data/shinyDataAggregated.RData")) {
#   data_prep()
# }

#load("data/shinyDataAggregated.RData")
#load("data/shinyDataLongitudinal.RData")

# Load data
sygen_summary_stats<- read.csv("/Users/jutzelec/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/shinyapp/data/sygen_summary_stats_for_app.csv", sep = ',', header = T)




# Default

ui <- dashboardPage(

  #----Dashboard header----
  
  # Define title, icon, and width of titel
  title = "Pharmacological Management of Spinal Cord Injury Project",
  dashboardHeader(title=span(icon("prescription"), "Pharmacological Management of Spinal Cord Injury"),
                  titleWidth = 500), #HTML(paste(icon("virus"), "PsyCorona Data Tool"))
 
  # Select 'skin' color: blue, black, green, purple, red, yellow
  skin = "purple",
  
  
  
  
  
  #----Dashboard sidebar----
  # Set up the sidebar of the dashboard
  dashboardSidebar(width = 350,
    sidebarMenu(id = "sidebarMenu",
                menuItem("About", tabName = "about", icon = icon("info-circle")),
                #menuItem("Cohorts", tabName = "cohort", icon = icon("users"),
                menuItem('Sygen Trial', tabName = 'sygentrial', icon = icon("hospital-user"), 
                         menuSubItem("Cohort", tabName = "cohort_sygen", icon = icon("users")), 
                         menuSubItem("Medications", tabName = "medication_sygen", icon = icon("prescription")),
                         menuSubItem("Polypharmacy", tabName = "polypharmacy_sygen", icon = icon("dice-d20")),
                         menuSubItem("Drug administration pattern", tabName = "drug_pattern_sygen", icon = icon("chart-bar"))),
                menuItem('SCIRehab', tabName = 'scirehab', icon=icon("database"),
                         menuSubItem("Cohort", tabName = "cohort_scirehab", icon = icon("users")), 
                         menuSubItem("Medications", tabName = "medication_scirehab", icon = icon("prescription")),
                         menuSubItem("Polypharmacy", tabName = "polypharmacy_scirehab", icon = icon("dice-d20")),
                         menuSubItem("Drug administration pattern", tabName = "drug_pattern_scirehab", icon = icon("chart-bar"))),
                menuItem(HTML(paste0("Contact for collaborations ", icon("external-link"))), icon=icon("envelope"), href = "mailto:catherine.jutzeler@bsse.ethz.ch"),
                uiOutput("dynamic_content")),
    shinyjs::useShinyjs(),
    tags$footer(HTML("<strong>Copyright &copy; 2020 <a href=\"Dr. Catherine Jutzeler\" target=\"_blank\">Data Science for Health Lab</a>.</strong> 
                     <br>This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\">Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License</a>.
                     <br><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png\" /></a>
                     "),
                #latest.DateTime,
                id = "sideFooter",
                align = "left",
                style = "
                position:absolute;
                bottom:0;
                width:100%;
                padding: 10px;
                ")
  ),
  
  #----Dashboard body----
  
  # Set up the body of the dashboard
  dashboardBody(
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$head(tags$meta(name = "viewport", content = "width=1600"), uiOutput("body")),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    #tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/JannisCodes/PsyCorona-WebApp/master/www/faviconData.png")),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    tags$style(
      type = 'text/css',
      '.bg-aqua {background-color: #605ca8!important; }
      .bttn-simple.bttn-primary {background-color: #605ca8!important; }
      .btn.radiobtn.btn-primary {float: center!important;
      display: block;
      width: 160px}
      '
    ),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.13.0/css/all.css);"),
    tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
    tags$script(HTML("
                     var openTab = function(tabName){
                     $('a', $('.sidebar')).each(function() {
                     if(this.getAttribute('data-value') == tabName) {
                     this.click()
                     };
                     });
                     };
                     $('.sidebar-toggle').attr('id','menu');
                     var dimension = [0, 0];
                     $(document).on('shiny:connected', function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange('dimension', dimension);
                     });
                     $(window).resize(function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange('dimension', dimension);
                     });
                     ")),
    
    
    # Customize color for the box status 'primary' and 'success' to match the skin color
    tags$style(HTML("
                      .btn-primary.btn {
                    color: #605ca8;
                    background-color: #fff;
                    border: 2px #605ca8 solid;
                    }
                    .btn-primary.btn:hover {
                    color: #fff;
                    background-color: #605ca8;
                    }
                    .btn-primary.active {
                    color: #fff;
                    background-color: #605ca8;
                    border-color: #605ca8;
                    }
                    .btn-outline-primary:focus,
                    .btn-outline-primary.focus{
                    color: #fff;
                    background-color: #605ca8;
                    border-color: #605ca8;
                    }
                    

                    .btn.btn-success {
                     color: #fff;
                    background-color: #605ca8;
                    border-color: #605ca8;
                    }
                    .btn.btn-success.focus,
                    .btn.btn-success:focus {
                    color: #fff;
                    background-color: #605ca8;
                    border-color: #605ca8;
                    outline: none;
                    box-shadow: none;
                    }
                    .btn.btn-success:hover {
                    color: #fff;
                    background-color: #605ca8;
                    border-color: #605ca8;
                    outline: none;
                    box-shadow: none;
                    }
                    .btn.btn-success.active,
                    .btn.btn-success:active {
                    color: #fff;
                    background-color: #605ca8;
                    border-color: #605ca8;
                    outline: none;
                    }
                    .btn.btn-success.active.focus,
                    .btn.btn-success.active:focus,
                    .btn.btn-success.active:hover,
                    .btn.btn-success:active.focus,
                    .btn.btn-success:active:focus,
                    .btn.btn-success:active:hover {
                    color: #fff;
                    background-color: #8f8cc2 ;
                    border-color: #8f8cc2 ;
                    outline: none;
                    box-shadow: none;
                    }

                  ")),

    # Create function to hyperlink a text with the tab links
    tags$script(HTML("
                            var openTab = function(tabName){
                     $('a', $('.sidebar')).each(function() {
                     if(this.getAttribute('data-value') == tabName) {
                     this.click()
                     };
                     });
                     };
                     $('.sidebar-toggle').attr('id','menu');
                     var dimension = [0, 0];
                     $(document).on('shiny:connected', function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange('dimension', dimension);
                     });
                     $(window).resize(function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange('dimension', dimension);
                     });
                     ")),
    
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "about",
              h3("Welcome to the",strong("Pharmacological Management of Spinal Cord Injury"), "Project"),
              br(),
              fluidRow(
                box(#title = "Explore The Data", 
                  width = 8, 
                  heigth = "500px",
                  solidHeader = TRUE,
                  
                  h4("Study description"),
                  "Complications arising from acute traumatic spinal cord injury (SCI) are routinely managed by various pharmacological interventions. 
                  Despite decades of clinical application, the potential impact on neurological recovery has been largely overlooked. The goal of this
                  study was to highlight drugs with potential disease modifying effects, and, in doing so, identify a novel translational path to enhancing 
                  function for individuals with acute injury.
                  Nearly every individual sustaining spinal cord injury receives multiple types and classes of medications to manage a litany of problems 
                  associated with traumatic spinal cord injury. We performed an analysis of available clinical trial and observational data to determine 
                  what constitutes standards of acute pharmacological care after traumatic spinal cord injury. The goal of this study was to determine 
                  the types of medications commonly administered, alone or in combination, in the acute phase of spinal cord injury. To this end, we conducted 
                  an analysis of available clinical trial and observational data to determine what constitutes standards of acute pharmacological care
                  after spinal cord injury. Concomitant medication use (i.e., non-randomized medications), including dosage, timing and reason for administration, 
                  was tracked through the duration of the trial and observational study. Descriptive statistics were used to describe the medications administered 
                  within the first 90 days after spinal cord injury. R Statistical Software was used for all statistical analyses and to create plots for data visualization.
                  Over 770 unique medications were administered within the first month after injury. On average, patients received 20 unique medications (range 1-58), 
                  often in a combinatorial or overlapping fashion (i.e., polypharmacy). Approximately 10% of medications were administered acutely as prophylaxis 
                  (e.g., pain, infections). Our study revealed a high degree of polypharmacy in the acute stages of spinal cord injury, with potential to both positively and negatively impact neurological recovery.",
                  br(),
                  br(),
                  h4("Study team"),
                  strong("Principal Investigator:"), 
                  tags$ul(
                    tags$li("Dr. Catherine Jutzeler, Research Group Leader, Department of Biosystems Science and Engineering, Swiss Federal Institute of Technology (ETH Zurich).",
                            tags$a(href="mailto:Catherine.Jutzeler@bsse.ethz.ch", 
                            target="_blank",
                            icon("envelope")))),
                  strong("Co-investigators:"), 
                  tags$ul(
                    tags$li("Dr. John Kramer, Department of Anesthesiology, Pharmacology, and Therapeutics, Faculty of Medicine, University of British Columbia, Canada."
                            ),
                    tags$li("Dr. Jacquelyn Cragg, Faculty of Pharmaceutical Sciences, University of British Columbia, Vancouver, Canada."
                    )),
                  br(),
                  h4("Ethics statement"),
                  "Approval for this study (secondary analysis) was received by an institutional ethical standards committee on human experimentation at the University of 
                  British Columbia. The original Sygen clinical trial (results published elsewhere) also received ethical approval, but was conducted before clinical trials 
                  were required to be registered (i.e., no clinicaltrial.gov identifier available). Each participating center of the SCIRehab study received institutional 
                  review board approval for this study and obtained informed consent from each patient (or their parent/guardian).
                  If you have any questions or concerns regarding the study please do not hesitate to contact the Principal
                  Investigator, Dr. Catherine Jutzeler",
                  tags$a(href="mailto:catherine.jutzeler@bsse.ethz.ch", 
                         target="_blank",
                         icon("envelope")),
                  ".",
                  br(),
                  br(),
                  h4("What You Can Do Here:"),
                  "This applet has ",
                  tags$b("four main interactive sections"),
                  " that enable visitors to directly interact with the PsyCorona data: ",
                  tags$ul(
                    tags$li("The cohort tab provides information on the patients that were enrolled in the", 
                            a("Sygen clinical trial", onclick = "openTab('cohort_sygen')", href="#"), 'or',
                            a("SCIRehab study", onclick = "openTab('cohort_scirehab')", href="#"),
                            ".")),
                  tags$ul(
                    tags$li("The ",
                            a("Data Sources", onclick = "openTab('scirehab')", href="#"),
                            "tab offers an insight into the diversity of our participants. We share compound information on some demographic variables, as well as the number of respondents in each country. 
                            Please note that to protect the privacy and anonymity of our participants, data visualizations are only available for selections of more than 20 people."),
                    tags$li("The ",
                            a("Psychological Variables", onclick = "openTab('medication_scirehab')", href="#"),
                            " tab offers an interactive interface to explore the psychological variables we collect in the initiative's baseline survey. 
                            This survey is open to anyone interested at",
                            tags$a(href="https://nyu.qualtrics.com/jfe/form/SV_6svo6J4NF7wE6tD", 
                                   target="_blank",
                                   "tiny.cc/corona_survey"),
                            "and currently includes over 50 000 participants. You can explore psychological reactions to the coronavirus at five different levels: 
                            (1) Governmental Response, (2) Community Response, (3) Cognitive Response, (4) Behavioral Response, as well as (5) Emotional Response. 
                            Additionally, we offer a tool to explore the mean level relationship between different variables for different countries. Please note that to protect the 
                            privacy and anonymity of our participants we only provide country-level visualizations once we have data for more than 20 people from any particular country."),
                    tags$li("The ",
                            a("Development", onclick = "openTab('medication_scirehab')", href="#"),
                            " tab gives you the possibility to interactively explore how different areas are evolving over time. This section is currently partly under
                            construction, but will be fully available soon.")
                    ),
                  br(),
                  h4("Funding:"),
                  p('This project is supported by the ',
                    a('Swiss National Science Foundation', href = 'http://p3.snf.ch/project-186101', target="_blank"),
                    ' (Ambizione Grant, #PZ00P3_186101), ',
                    a('Wings for Life Research Foundation', href = 'https://www.wingsforlife.com/en/', target="_blank"),
                    ' (#2017_044), the ',
                    a('Craig H Neilsen', href = 'https://chnfoundation.org/', target="_blank"),
                    '.', align = "justify")
                    ),
                box(width = 4,
                    HTML("<a class=\"twitter-timeline\" data-height=\"600\" href=\"https://twitter.com/DatSci_4_health\">A Twitter List by FortuneMagazine</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                )
                    ),
              fluidRow(
                valueBox(prettyNum(1895, big.mark=" ", scientific=FALSE), "Patients", icon = icon("user-edit"), width = 3, color = "purple"),
                valueBox(prettyNum(770, big.mark=" ", scientific=FALSE), "Unique drugs", icon = icon("pills"), width = 3,  color = "purple"),
                valueBox(tagList("10", tags$sup(style="font-size: 20px", "%")),
                         "Prophylactic drug use", icon = icon("prescription"),  width = 3,  color = "purple"
                ),
                #valueBox(prettyNum(10, big.mark="", scientific=FALSE), "Prophylaxis", icon = icon("heartbeat"), width = 3,  color = "purple"),
                valueBox("XX", "Clinical sites", icon = icon("clinic-medical"), width = 3,  color = "purple")#,
                #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              )
           ),
      
      
    tabItem(tabName = "cohort_sygen",
            h3("Description of Sygen Trial Cohort"),
                        fluidRow(
                         box(width = 12,
                         div(style="display:inline-block;width:100%;text-align:center;",
                          radioGroupButtons(
                          inputId = "var", 
                          label = "Patient characteristics:", 
                          selected = "languages",
                          status = "success",
                          #justified = T, #if true, all boxes have the same length
                          individual = T, #if false, then the boxes are connected
                          choiceNames = c("Sex", "Age", "Injury Severity", "Injury Level", "Tetra- or paraplegia"),
                          choiceValues = c("sex", "age", "baseline.ais", "nli", "plegia")
                          #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                        )
                  ),
                  h3(textOutput("sample.bar.NA"), align = "center"),
                  r2d3::d3Output("d3.bar"),
                  textOutput("SampleTxt"), align = "center")
              #)
            )
            
            
    ),    tabItem(tabName = "cohort_scirehab",
                  h3("Description of SCIRehab Study Cohort"),
                  fluidRow(
                    box(width = 12,
                        div(style="display:inline-block;width:100%;text-align:center;",
                            radioGroupButtons(
                              inputId = "var", 
                              label = "Patient characteristics:", 
                              selected = "languages",
                              status = "success",
                              #justified = T, #if true, all boxes have the same length
                              individual = T, #if false, then the boxes are connected
                              choiceNames = c("Sex", "Age", "Injury Severity", "Injury Level", "Tetra- or paraplegia"),
                              choiceValues = c("sex", "age", "baseline.ais", "nli", "plegia")
                              #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                            )
                        ),
                        h3(textOutput("sample.bar.NA"), align = "center"),
                        r2d3::d3Output("d3.bar"),
                        textOutput("SampleTxt"), align = "center")
                    #)
                  )
    )
    
    )
  )
)
                  

    
    



server <- function(input, output) {

  
  output$cohort <- renderMenu({
    sidebarMenu(
      menuItem("Cohort description", icon = icon("users"))
    )
  })
  
  # Gender 
  output$d3.bar <- renderD3({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    #input <- list(var = "gender", sample_country_selection = c("Poland", "Romania", "Albania"))
    
    dem <- reactive_ctry.scales() %>%
      filter(coded_country %in% input$sample_country_selection) %>%
      select(starts_with(input$var)) %>%
      t() %>%
      as.data.frame()
    # colnames(dem) <- input$sample_country_selection
    dem %>%
      mutate(n = rowSums(., na.rm=TRUE),
             label = str_replace(rownames(.), ".*_", "")) %>%
      arrange(desc(n)) %>%
      filter(n > 0,
             label != "<NA>") %>%
      mutate(y = n,
             ylabel = scales::percent(n/sum(n), accuracy = 0.01), #prettyNum(n/sum(n)*100, big.mark = ",", format = "f", digits = 2),
             fill = "#3b738f", #ifelse(label != input$val, "#E69F00", "red"),
             mouseover = "#2a5674") %>%
      r2d3(r2d3_file)
  })
  
  
  

}

shinyApp(ui, server)
