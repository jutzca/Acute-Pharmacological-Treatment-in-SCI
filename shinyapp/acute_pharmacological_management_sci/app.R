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
library(shinyalert)
library(shinyBS)
library(devtools)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(shiny)){install.packages("shiny")}
# if(!require(shinydashboard)){install.packages("shinydashboard")}
# if(!require(shinythemes)){install.packages("shinythemes")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(tidyr)){install.packages("tidyr")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(stats)){install.packages("stats")}
# if(!require(DT)){install.packages("DT")}
# if(!require(shinyWidgets)){install.packages("shinyWidgets")}
# if(!require(png)){install.packages("png")}
# if(!require(plotly)){install.packages("plotly")}
# if(!require(splitstackshape)){install.packages("splitstackshape")}
# if(!require(RColorBrewer)){install.packages("RColorBrewer")}
# if(!require(stringr)){install.packages("stringr")}
# # if(!require(ggnetwork)){install.packages("ggnetwork")}
# if(!require(igraph)){install.packages("igraph")}
# if(!require(intergraph)){install.packages("intergraph")}
# if(!require(sna)){install.packages("sna")}
# if(!require(shinyjs)){install.packages("shinyjs")}
# if(!require(metathis)){install.packages("metathis")}
# if(!require(r2d3)){install.packages("r2d3")}
# if(!require(Shinyalert)){install.packages("Shinyalert")}
# if(!require(bsAlert)){install.packages("bsAlert")}
# if(!require(devtools)){install.packages("devtools")}
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
setwd("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/shinyapp/acute_pharmacological_management_sci/")
##
## ---------------------------
##
## Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables'
##
## ---------------------------
##
#Set local system
Sys.setlocale('LC_ALL','C') 
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####


#---------- Add sources ----------# 
source("helper_functions_2.R")
# source("R/dependencies.R")
# source("R/input-multi.R")
# source("R/utils.R")

#### ---------------------------
# load data:
# data creation not possible in docker container (i.e., on server)
# if (!file.exists("data/shinyDataAggregated.RData")) {
#   data_prep()
# }

#load("data/shinyDataAggregated.RData")
#load("data/shinyDataLongitudinal.RData")

########## Data sets ##########

#---------- Data set #1: Sygen baseline characteristics ---------- 
sygen_baseline<- read.csv("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/shinyapp/data/sygen_summary_stats_for_app_new.csv", sep = ',', header = T)

#---------- Data set #2: SCIRehab baseline characteristics ---------- 

scirehab_baseline<- read.csv("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/shinyapp/data/rehab_summary_stats_for_app_new.csv", sep = ',', header = T)

#---------- updateMultiInput_2 function ---------- 

updateMultiInput_2 <- function (session, inputId, label = NULL, selected = NULL, choices = NULL, choiceValues = NULL, choiceNames = NULL) {
  if (is.null(choices)) {
    if (is.null(choiceValues))
      stop("If choices = NULL, choiceValues must be not NULL")
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
    choiceValues <- as.list(choiceValues)
    choiceNames <- as.list(choiceNames)
    choices_2 <- tagList(
      lapply(
        X = seq_along(choiceNames),
        FUN = function(i) {
          htmltools::tags$option(value = choiceValues[[i]], as.character(choiceNames[[i]]),
                                 selected = if(choiceValues[[i]] %in% selected) "selected")
        }
      )
    )
  }
  else {
    choices_2 <- if (!is.null(choices))
      choicesWithNames(choices_2)
  }
  if (!is.null(selected))
    selected <- validateSelected(selected, choices_2, inputId)
  options <- as.character(makeChoices(choices = choices, choiceValues = choiceValues, choiceNames = choiceNames, selected = selected))
  message <- dropNulls(list(label = label, options = options, value = selected))
  session$sendInputMessage(inputId, message)
}


r2d3_script <- "
// !preview r2d3 data= data.frame(y = 0.1, ylabel = '1%', fill = '#E69F00', mouseover = 'green', label = 'one', id = 1)
function svg_height() {return parseInt(svg.style('height'))}
function svg_width()  {return parseInt(svg.style('width'))}
function col_top()  {return svg_height() * 0.05; }
function col_left() {return svg_width()  * 0.25;} 
function actual_max() {return d3.max(data, function (d) {return d.y; }); }
function col_width()  {return (svg_width() / actual_max()) * 0.60; }
function col_heigth() {return svg_height() / data.length * 0.95; }
var bars = svg.selectAll('rect').data(data);
bars.enter().append('rect')
.attr('x',      170)
.attr('y',      function(d, i) { return i * col_heigth() + col_top(); })
.attr('width',  function(d) { return d.y * col_width(); })
.attr('height', col_heigth() * 0.9)
.attr('fill',   function(d) {return d.fill; })
.attr('id',     function(d) {return (d.label); })
.on('click', function(){
Shiny.setInputValue('bar_clicked', d3.select(this).attr('id'), {priority: 'event'});
})
.on('mouseover', function(){
d3.select(this).attr('fill', function(d) {return d.mouseover; });
})
.on('mouseout', function(){
d3.select(this).attr('fill', function(d) {return d.fill; });
});
bars.transition()
.duration(500)
.attr('x',      170)
.attr('y',      function(d, i) { return i * col_heigth() + col_top(); })
.attr('width',  function(d) { return d.y * col_width(); })
.attr('height', col_heigth() * 0.9)
.attr('fill',   function(d) {return d.fill; })
.attr('id',     function(d) {return d.label; });
bars.exit().remove();

// Identity labels
var txt = svg.selectAll('text').data(data);
txt.enter().append('text')
.attr('x', width * 0.01)
.attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
.text(function(d) {return d.label; })
.style('font-family', 'sans-serif');
txt.transition()
.duration(1000)
.attr('x', width * 0.01)
.attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
.text(function(d) {return d.label; });
txt.exit().remove();

// Numeric labels
var totals = svg.selectAll().data(data);
totals.enter().append('text')
.attr('x', function(d) { return ((d.y * col_width()) + 170) * 1.01; })
.attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
.style('font-family', 'sans-serif')
.text(function(d) {return d.ylabel; });
totals.transition()
.duration(1000)
.attr('x', function(d) { return ((d.y * col_width()) + 170) * 1.01; })
.attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
.attr('d', function(d) { return d.x; })
.text(function(d) {return d.ylabel; });
totals.exit().remove();
"


r2d3_file <- tempfile()
writeLines(r2d3_script, r2d3_file)







#----------  Shiny app ui  ---------- 

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
                         menuSubItem("About", tabName = "about_sygen", icon = icon("info-circle")),
                         menuSubItem("Cohort", tabName = "cohort_sygen", icon = icon("users")), 
                         menuSubItem("Medications", tabName = "medication_sygen", icon = icon("prescription")),
                         menuSubItem("Polypharmacy", tabName = "polypharmacy_sygen", icon = icon("dice-d20")),
                         menuSubItem("Drug administration pattern", tabName = "drug_pattern_sygen", icon = icon("chart-bar"))),
                menuItem('SCIRehab', tabName = 'scirehab', icon=icon("database"),
                         menuSubItem("About", tabName = "about_scirehab", icon = icon("info-circle")),
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
                valueBox("34", "Clinical sites", icon = icon("clinic-medical"), width = 3,  color = "purple")#,
                #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              )
           ),
          
          tabItem(tabName = "about_sygen",
                  h3(strong("The Sygen Clinical Trial")),
                  br(),
                  fluidRow(
                    box(#title = "Explore The Data", 
                      width = 8, 
                      heigth = "500px",
                      solidHeader = TRUE,
                      
                      h4("Objectives of original study"),
                      "To determine efficacy and safety of monosialotetrahexosylganglioside GM1 (i.e., Sygen) in acute spinal cord injury.",
                      br(),
                      h4("Methods"),
                      strong("Monosialotetrahexosylganglioside GM1"),
                      "Sygen (monosialotetrahexosylganglioside GM1 sodium salt) is a naturally occurring compound in cell membranes of mammals and is especially abundant in the membranes of the central nervous system. 
                      Acute neuroprotective and longer-term regenerative effects in multiple experimental models of ischemia and injury have been reported. The proposed mechanisms of action of GM1 include 
                      anti-excitotoxic activity, apoptosis prevention, and potentiation of neuritic sprouting and the effects of nerve growth factors.",
                      br(),
                      br(),
                      strong("Study design."), "Randomized, double-blind, sequential,
                      multicenter clinical trial of two doses Sygen (i.e., low-dose GM-1: 300 mg intravenous loading dose followed by 100 mg/d x 56 days or high-dose GM-1:00 mg intravenous loading dose followed by 200 mg/d x 56 days) versus
                      placebo. All patients received the National Acute Spinal Cord Injury Study (NASCIS-2) protocol dosage of methylprednisolone. Based on a potential adverse interaction between concomitant MPSS and GM-1 administration, 
                      the initial dose of GM-1 was delayed until after the steroids were given (mean onset of study medication, 54.9 hours).",
                      br(),
                      br(),
                      strong("Inclusion/exclusion criteria."), "For inclusion in Sygen, patients were required to have at least one lower extremity with a substantial motor deficit. Patients with spinal cord transection 
                      or penetration were excluded, as were patients with a cauda equina, brachial or lumbosacral plexus, or peripheral nerve injury. Multiple trauma cases were included as long as they were not so severe
                      as to preclude neurologic evaluation. It is notable that this requirement of participating in a detailed neurologic exam excluded major head trauma cases and also intubated 
                      chest trauma cases.",
                      br(),
                      br(),
                      strong("Assessments."), "Baseline neurologic assessment included both the AIS and detailed American Spinal Injury Association (ASIA) motor and
                      sensory examinations. Additionally, the Modified Benzel Classification and the ASIA motor and
                      sensory examinations were performed at 4, 8, 16, 26, and 52 weeks after injury. The Modified Benzel Classification was used for post-baseline measurement because it rates walking
                      ability and, in effect, subdivides the broad D category of the AIS. Because most patients have an unstable spinal fracture at
                      baseline, it is not possible to assess walking ability at that time; hence the use of different baseline and follow-up scales.
                      Marked recovery was defined as at least a two-grade equivalent improvement in the Modified Benzel Classification from the
                      baseline AIS. The primary efficacy assessment was the proportion of patients with marked recovery at week 26. The secondary efficacy assessments included the time course of marked recovery and
                      other established measures of spinal cord function (the ASIA motor and sensory scores, relative and absolute sensory levels of impairment, and assessments of bladder and bowel
                      function).",
                      br(),
                      br(),
                      strong("Concomitant medications."), "The use of medications delivered alongside the study medication (i.e., GM-1) was rigorously tracked. 
                      For each concomitant medication administered during the trial, the dosage, reason for administration, and the timing of administration were recorded.",
                      br(),
                      br(),
                      strong("Results."), "Of 797 patients recruited, 760 were included in the analysis. The prospectively planned analysis at the prespecified endpoint time for all patients was negative.
                      The negative finding of the Sygen study is considered Class I Medical Evidence by the spinal cord injury Committee of the 
                      American Association of Neurological Surgeons (AANS) and the Congress of Neurological Surgeons (CNS). Subsequent analyses of the Sygen 
                      data have been performed to characterize the trajectory and extent of spontaneous recovery from acute spinal cord injury.",
                      br()
                       ), # close box
                    
                    fluidRow(
                      valueBox(prettyNum(797, big.mark=" ", scientific=FALSE), "Patients", icon = icon("user-edit"), width = 3, color = "purple"),
                      valueBox(prettyNum(489, big.mark=" ", scientific=FALSE), "Unique concomittant medication", icon = icon("pills"), width = 3,  color = "purple"),
                      valueBox(tagList("10", tags$sup(style="font-size: 20px", "%")),
                               "Prophylactic medication use", icon = icon("prescription"),  width = 3,  color = "purple"
                      ),
                      #valueBox(prettyNum(10, big.mark="", scientific=FALSE), "Prophylaxis", icon = icon("heartbeat"), width = 3,  color = "purple"),
                      valueBox("28", "North American clinical sites", icon = icon("clinic-medical"), width = 3,  color = "purple"),
                      valueBox("1991-1997", "Running time", icon = icon("calendar-alt"), width = 3,  color = "purple")#,
                      #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
                    )
                  ), # close fluid row
                  fluidRow(
                    box(#title = "Explore The Data", 
                      width = 8, 
                      heigth = "500px",
                      solidHeader = TRUE,
                      
                      h4("References"),
                      
                      tags$ul(
                        tags$li(a('Geisler et al, 2001', href = 'https://europepmc.org/article/med/11805612', target="_blank"), "Recruitment and early treatment in a multicenter study of acute spinal cord injury. Spine (Phila Pa 1976)."),
                        tags$li(a('Geisler et al, 2001', href = 'https://journals.lww.com/spinejournal/Fulltext/2001/12151/The_Sygen_R__Multicenter_Acute_Spinal_Cord_Injury.15.aspx', target="_blank"), "The Sygen multicenter acute spinal cord injury study. Spine (Phila Pa 1976)")
                        ) # close tags
                        ) # close box
                    ) # close fluid row
                 
          ), # close tab item
    # Tab: Sygen Cohort  
    tabItem(tabName = "cohort_sygen",
            
            # # Create alert
            # shinyalert::useShinyalert(),
            # h3("Description of Sygen Trial Cohort"),
            # shinyBS::bsAlert("dataAlert"),
            # 
            fluidRow(
                  box(width = 12,
                      div(style="display:inline-block;width:100%;text-align:center;",
                          radioGroupButtons(
                          inputId = "var", 
                          label = "Patient characteristics:", 
                          selected = "sex",
                          status = "success",
                          #justified = T, #if true, all boxes have the same length
                          individual = T, #if false, then the boxes are connected
                          choiceNames = c("Sex", "Age", "Injury Severity", "Injury Level", "Etiology"),
                          choiceValues = c("sex", "age", "baseline.ais", "nli", "etiology")
                          ) # Close radioGroupButtons bracket
                  ), # Close div bracket
                  
                  div(plotlyOutput("bar.plot.baseline.characteristic.sygen", width = "50%",
                                             height = "600px",
                                           inline = FALSE), align='center')
                
             
                  ) #close box bracket
              )  #close fluid row
            ),   #close tabitem
    
    tabItem(tabName = "about_scirehab",
            h3(strong("Spinal Cord Injury Rehabilitation Study")),
            br(),
            fluidRow(
              box(#title = "Explore The Data", 
                width = 8, 
                heigth = "300px",
                solidHeader = TRUE,
                
                h4("Objectives"),
                "In an effort to understand the relationship between the rehabilitation process and outcomes, the SCIRehab study collected data about rehabilitation interventions across 7 disciplines during the inpatient rehabilitation of 1,376 people with spinal cord injury. 
                This study used practice-based evidence methods to relate the details of the rehabilitation process to outcomes after controlling for 
                individual demographic and injury characteristics",
                br(),
                h4("Methods"),
                strong("Study design."), "Longitudinal, observational, prospective event-based cohort study.",
                br(),
                br(),
                strong("Inclusion/ exlusion criteria"), "The SCIRehab facilities enrolled all patients who were 12 years of age or older, gave (or whose parent/guardian gave) informed consent, and were admitted to the facility's SCI unit for initial rehabilitation following traumatic SCI. 
                Duration of the acute-hospital inpatient admission preceding rehabilitation was not an enrollment criterion. Patients requiring transfer to acute care units or facilities during their rehabilitation program were retained in the study, no matter how long they spent in acute 
                care before returning to the rehabilitation unit, but their acute care days were not counted as part of the rehabilitation stay. To restrict the study to initial rehabilitation cases, a small number of patients were excluded who spent more than 2 weeks in another rehabilitation 
                center prior to admission to the SCIRehab facility. To ensure complete rehabilitation data, patients who spent more than a week of their rehabilitation stay on a non-SCI rehabilitation unit in the SCIRehab facility (staff of the non-SCI units were not trained in the data collection methods)
                also were excluded. There were no other exclusion criteria.",
                br(),
                br(),
                strong("Assessments."),
                "Patients were followed for first year 
                post-injury and were excluded if they spent two or more weeks at a non-participating rehabilitation center. Patient demographics and injury characteristics were extracted from the patient medical record (part of the National Institute on Disability and Rehabilitation 
                Research Spinal Cord Injury Model Systems Form I). The International Standards of Neurological Classification of SCI (ISNCSCI) and its American Spinal Injury Association Impairment Scale (AIS) were used to describe the neurologic level and completeness of injury; the Functional Independence Measure (FIM)
                served to describe a patient's functional independence in motor and cognitive tasks at admission and discharge, and monitor functional gains; and the Comprehensive Severity Index (CSI) was used to provide an overall summary measure of how ill (extent of deviation from normal)
                a patient was over time during the stay in the center.",
                br(),
                br(),
                strong("Commonly administered medications."), "The SCIRehab study rigorously tracked the use of all commonly administered medications. For each medication administered, route, dosage and dosing 
                                   (i.e., start and end date, frequency) were abstracted directly from medical records. However, medication indication was not recorded. The medication data has not been published."
              
               ), # close box
              
              fluidRow(
                valueBox(prettyNum(1225, big.mark=" ", scientific=FALSE), "Patients", icon = icon("user-edit"), width = 3, color = "purple"),
                valueBox(prettyNum(575, big.mark=" ", scientific=FALSE), "Unique medications to treat secondary complications", icon = icon("pills"), width = 3,  color = "purple"),
                #valueBox(prettyNum(10, big.mark="", scientific=FALSE), "Prophylaxis", icon = icon("heartbeat"), width = 3,  color = "purple"),
                valueBox("6", "North American clinical sites", icon = icon("clinic-medical"), width = 3,  color = "purple"),
                valueBox("2007-2010", "Running time", icon = icon("calendar-alt"), width = 3,  color = "purple")#,
                #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              )
            ), # close fluid row
            fluidRow(
              box(#title = "Explore The Data", 
                width = 8, 
                heigth = "500px",
                solidHeader = TRUE,
                h4('Website'),
                tags$a("ADDEP SCIRehab Data Source", href="https://www.icpsr.umich.edu/web/ADDEP/studies/36724", 
                       target="_blank",
                       icon("external-link")),
                h4("References"),
                tags$ul(
                  tags$li(a('Whiteneck et al, 2009; ', href = 'https://pubmed.ncbi.nlm.nih.gov/19810627/', target="_blank"), "New approach to study the contents and outcomes of spinal cord injury rehabilitation: the SCIRehab Project. J Spinal Cord Med."),
                  tags$li(a('Whiteneck et al, 2011', href = 'https://pubmed.ncbi.nlm.nih.gov/21675353/', target="_blank"), "Inpatient treatment time across disciplines in spinal cord injury rehabilitation. J Spinal Cord Med")
                ) # close tags
              ) # close box
            ) # close fluid row
            
    ),
    
    
    
    
    
    
    tabItem(tabName = "cohort_scirehab",
                  h3("Description of SCIRehab Study Cohort"),
                  fluidRow(
                    box(width = 12,
                        div(style="display:inline-block;width:100%;text-align:center;",
                            radioGroupButtons(
                              inputId = "var1", 
                              label = "Patient characteristics:", 
                              selected = "sex",
                              status = "success",
                              #justified = T, #if true, all boxes have the same length
                              individual = T, #if false, then the boxes are connected
                              choiceNames = c("Sex", "Age", "Injury Severity", "Injury Level", "Etiology"),
                              choiceValues = c("sex", "age", "baseline.ais", "nli", "etiology")
                                                          ) #close box bracket
                        ), #close divstyle
                        
                        div(plotlyOutput("bar.plot.baseline.characteristic.scirehab", width = "50%",
                                         height = "600px",
                                         inline = FALSE), align='center')
                   ) #close box bracket
               ) #close fluid row
    
          ) #close tabitem
        ) # close tabitems
    ) # close dashboard body
) # close ui
    
    



server <- function(input, output, session) {

  
  output$cohort <- renderMenu({
    sidebarMenu(
      menuItem("Cohort description", icon = icon("users"))
    )
  })
  
# Create Data Alert
  createAlert(session = session,
              anchorId = "dataAlert",
              #alertId="a1",
              title = paste(icon("warning"),"Data Notification"),
              content="To protect the privacy of all patients, this application only uses aggregate, anonymized data (i.e., no individual person is identifiable). 
              For further information, see our <a href='#' onclick=\"openTab('data')\">data description section</a>.",
              style = "warning")
  
  
# #------- Plot GM1---------- 
#   output$gm1 <- renderImage({
#     return(list(src = "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/shinyapp/acute_pharmacological_management_sci/www/gm1.png",
#                 type = "image/png",alt = "gm1", width = "200ptx"))
#   }, deleteFile = FALSE) #where the src is wherever you have the picture
#   
  
#------- Plot baseline characteristics of Sygen patients ----------
  
  output$bar.plot.baseline.characteristic.sygen <- renderPlotly({
    
    if (input$var == "sex")  {

    width = c(0.8, 0.8)
    
    baseline.sex <- sygen_baseline%>%
      dplyr::count(Sex)%>% 
      dplyr::mutate(frequency=sprintf("%0.1f", n/793*100))%>% 
      as.data.frame()%>%
      plotly::plot_ly(y = ~Sex,
                      x =  ~as.numeric(frequency))%>%
      plotly::add_bars(
        marker = list(color = 'rgb(96,92,168)'),
        width = ~width,
        text = ~paste("Sex:", Sex,
                      '</br></br>', "N:", n,
                      '</br>', "Frequency:", frequency, '%'),
        #text = ~n,
        hoverinfo = "text")%>%
      layout(title = '', font=list(size = 12)) %>%
      layout(xaxis = list(title = 'Proportion [%]')) %>%
      layout( xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14)),
              yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14)) )
    baseline.sex}
    
   
     else if (input$var == "age")  {
       width.age.group = c(0.8, 0.8, 0.8, 0.8)
     
     sygen_baseline$agegroup=factor(sygen_baseline$agegroup, levels = c("60+ yrs", "41-60 yrs", "21-40 yrs", "0-20 yrs" ))
     
     baseline.age.grp<- sygen_baseline%>%
       dplyr::count(agegroup)%>% 
       dplyr::mutate(frequency=sprintf("%0.1f", n/793*100))%>% 
       as.data.frame()%>%
       plotly::plot_ly(y = ~ agegroup,
                       x =  ~as.numeric(frequency))%>%
       plotly::add_bars(
         marker = list(color = 'rgb(96,92,168)'),
         width = ~width.age.group,
         text = ~paste("Age Group:", agegroup,
                       '</br></br>', "N:", n,
                       '</br>', "Frequency:", frequency, '%'),
         hoverinfo = "text")%>%
       plotly::layout(xaxis = list(title = "Proportion [%]"),
                      yaxis = list(title = ""))
     baseline.age.grp}  
    
    
    else if (input$var == "baseline.ais")  {
    
              width.ais = c(0.8, 0.8, 0.8, 0.8, 0.8)
              
              sygen_baseline$AIS=factor(sygen_baseline$AIS, levels = c('Unknown', "AIS D", 'AIS C', "AIS B", "AIS A"))
              
              baseline.ais <- sygen_baseline%>%
                dplyr::count(AIS)%>% 
                dplyr::mutate(frequency=sprintf("%0.1f", n/793*100))%>% 
                as.data.frame()%>%
                plotly::plot_ly(y = ~AIS,
                                x =  ~as.numeric(frequency))%>%
                plotly::add_bars(
                  marker = list(color = 'rgb(96,92,168)'),
                  width = ~width.ais,
                  text = ~paste("Injury Severity:", AIS,
                                '</br></br>', "N:", n,
                                '</br>', "Frequency:", frequency, '%'),
                  hoverinfo = "text")%>%
                plotly::layout(xaxis = list(title = "Proportion [%]"),
                               yaxis = list(title = ""))
              baseline.ais}  
   
    
    else if (input$var == "nli")  {width.nli = c(0.8, 0.8)
    
    sygen_baseline$NLI=factor(sygen_baseline$NLI, levels = c('Thoracic', "Cervical"))
  
    baseline.nli <- sygen_baseline%>%
      dplyr::count(NLI)%>% 
      dplyr::mutate(frequency=sprintf("%0.1f", n/793*100))%>% 
      as.data.frame()%>%
      plotly::plot_ly(y = ~NLI,
                      x =  ~as.numeric(frequency))%>%
      plotly::add_bars(
        marker = list(color = 'rgb(96,92,168)'),
        width = ~width.nli,
        text = ~paste("Injury Level:", NLI,
                      '</br></br>', "N:", n,
                      '</br>', "Frequency:", frequency, '%'),
        hoverinfo = "text")%>%
      plotly::layout(xaxis = list(title = "Proportion [%]"),
                     yaxis = list(title = ""))
    baseline.nli}
    
    
    else if (input$var == "etiology")  {
      
      width.cause = c(0.8, 0.8, 0.8, 0.8,0.8, 0.8, 0.8, 0.8, 0.8)
    
      sygen_baseline$Cause=factor(sygen_baseline$Cause, levels = c('Others', "Water related", "Pedestrian", "Other sports", "Motorcycle", "Gun shot wound", "Fall", "Blunt trauma", "Automobile" ))
      
      baseline.cause<- sygen_baseline%>%
        dplyr::count(Cause)%>% 
        dplyr::mutate(frequency=sprintf("%0.1f", n/793*100))%>% 
        as.data.frame()%>%
        plotly::plot_ly(y = ~Cause,
                        x =  ~as.numeric(frequency))%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~width.cause,
          text = ~paste("Etiology:", Cause,
                        '</br></br>', "N:", n,
                        '</br>', "Frequency:", frequency, '%'),
          hoverinfo = "text")%>%
        plotly::layout(xaxis = list(title = "Proportion [%]"),
                       yaxis = list(title = ""))
      baseline.cause}
    
    
    
  })
  
#------- Plot baseline characteristics of SCIRehab patients ----------
  
  output$bar.plot.baseline.characteristic.scirehab <- renderPlotly({
    
    if (input$var1 == "sex")  {
      
      width = c(0.8, 0.8)
      
      baseline.sex <- scirehab_baseline%>%
        dplyr::count(Sex)%>% 
        dplyr::mutate(frequency=sprintf("%0.1f", n/1225*100))%>% 
        as.data.frame()%>%
        plotly::plot_ly(y = ~Sex,
                        x =  ~as.numeric(frequency))%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~width,
          text = ~paste("Sex:", Sex,
                        '</br></br>', "N:", n,
                        '</br>', "Percentage:", frequency, '%'),
          #text = ~n,
          hoverinfo = "text")%>%
        layout(title = '', font=list(size = 12)) %>%
        layout(xaxis = list(title = 'Percentage [%]')) %>%
        layout( xaxis = list(titlefont = list(size = 16), tickfont = list(size = 14)),
                yaxis = list(titlefont = list(size = 16), tickfont = list(size = 14)) )
      baseline.sex}
    
    
    else if (input$var1 == "age")  {
      width.age.group = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
      
      scirehab_baseline$Age=factor(scirehab_baseline$Age, levels = c("80+ yrs", "70-79 yrs", "60-69 yrs", "50-59 yrs", "40-49 yrs", "30-39 yrs", "20-29 yrs", "0-19 yrs" ))
      
      baseline.age<- scirehab_baseline%>%
        dplyr::count(Age)%>% 
        dplyr::mutate(frequency=sprintf("%0.1f", n/1225*100))%>% 
        as.data.frame()%>%
        plotly::plot_ly(y = ~ Age,
                        x =  ~as.numeric(frequency))%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~width.age.group,
          text = ~paste("Age Group:", Age,
                        '</br></br>', "N:", n,
                        '</br>', "Percentage:", frequency, '%'),
          hoverinfo = "text")%>%
        plotly::layout(xaxis = list(title = "Percentage [%]"),
                       yaxis = list(title = ""))
      baseline.age}  
    
    
    else if (input$var1 == "baseline.ais")  {
      
      width.ais = c(0.8, 0.8, 0.8, 0.8, 0.8)
      
      scirehab_baseline$AIS=factor(scirehab_baseline$AIS, levels = c('Unknown', "AIS D", 'AIS C', "AIS B", "AIS A"))
      
      baseline.ais <- scirehab_baseline%>%
        dplyr::count(AIS)%>% 
        dplyr::mutate(frequency=sprintf("%0.1f", n/1225*100))%>% 
        as.data.frame()%>%
        plotly::plot_ly(y = ~AIS,
                        x =  ~as.numeric(frequency))%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~width.ais,
          text = ~paste("Injury Severity:", AIS,
                        '</br></br>', "N:", n,
                        '</br>', "Percentage:", frequency, '%'),
          hoverinfo = "text")%>%
        plotly::layout(xaxis = list(title = "Percentage [%]"),
                       yaxis = list(title = ""))
      baseline.ais}  
    
    
    else if (input$var1 == "nli")  {
      
      width.nli = c(0.8, 0.8, 0.8, 0.8)
      
      scirehab_baseline$NLI=factor(scirehab_baseline$NLI, levels = c('Unknown',"Lumbar", 'Thoracic', "Cervical"))
      
      baseline.nli <- scirehab_baseline%>%
        dplyr::count(NLI)%>% 
        dplyr::mutate(frequency=sprintf("%0.1f", n/1225*100))%>% 
        as.data.frame()%>%
        plotly::plot_ly(y = ~NLI,
                        x =  ~as.numeric(frequency))%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~width.nli,
          text = ~paste("Injury Level:", NLI,
                        '</br></br>', "N:", n,
                        '</br>', "Percentage:", frequency, '%'),
          hoverinfo = "text")%>%
        plotly::layout(xaxis = list(title = "Percentage [%]"),
                       yaxis = list(title = ""))
      baseline.nli}
    
    
    else if (input$var1 == "etiology")  {
      
      width.cause = c(0.8, 0.8, 0.8, 0.8,0.8, 0.8, 0.8, 0.8, 0.8)
      
      scirehab_baseline$Cause=factor(scirehab_baseline$Cause, levels = c("Others", "Water related","Person-to-person contact", "Pedestrian", "Other sports", "Motorcycle", "Gun shot wound", "Fall", "Automobile" ))
      
      baseline.cause<- scirehab_baseline%>%
        dplyr::count(Cause)%>% 
        dplyr::mutate(frequency=sprintf("%0.1f", n/1225*100))%>% 
        as.data.frame()%>%
        plotly::plot_ly(y = ~Cause,
                        x =  ~as.numeric(frequency))%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~width.cause,
          text = ~paste("Etiology:", Cause,
                        '</br></br>', "N:", n,
                        '</br>', "Percentage:", frequency, '%'),
          hoverinfo = "text")%>%
        plotly::layout(xaxis = list(title = "Percentage [%]"),
                       yaxis = list(title = ""))
      baseline.cause}
    
  })
  
  
  
  #------- other ----------- 
  
  shinyjs::onclick("menu",
                   shinyjs::toggle(id = "sideFooter", anim = F))
  
  shiny:::flushReact()
  
}

# Run the application 
shinyApp(ui = ui, server = server)

