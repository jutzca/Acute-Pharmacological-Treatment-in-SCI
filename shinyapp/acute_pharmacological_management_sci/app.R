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
library(rsconnect) ##
library(shiny) ##
library(shinydashboard) ##
library(shinythemes)
library(dplyr) ##
library(tidyr) ##
library(ggplot2) ##
library(stats) ##
library(DT) ##
library(shinyWidgets) ##
library(png) ##
library(plotly) ###### pb when removing ######
library(splitstackshape) ##
library(RColorBrewer) ##
library(stringr) ##
library(ggnetwork) ##
library(networkD3) ##
library(igraph) ##
library(intergraph) ##
library(sna) ##
library(shinyjs) ##
library(metathis) ##
library(r2d3) ##
library(shinyalert) ##
library(shinyBS) ##
library(devtools) ##
library(testthat) ##
library(gridExtra) ##
library(grid) ##
library(lattice) ##
library(tibble) ##
library(data.table) ##
library(tidygraph) ##
library(ggraph) ##
library(crosstalk)
library(extrafont)
library(shinybusy)

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
# if(!require(ggnetwork)){install.packages("ggnetwork")}
# if(!require(networkD3)){install.packages("networkD3")}
# if(!require(igraph)){install.packages("igraph")}
# if(!require(intergraph)){install.packages("intergraph")}
# if(!require(sna)){install.packages("sna")}
# if(!require(shinyjs)){install.packages("shinyjs")}
# if(!require(metathis)){install.packages("metathis")}
# if(!require(r2d3)){install.packages("r2d3")}
# if(!require(Shinyalert)){install.packages("Shinyalert")}
# if(!require(bsAlert)){install.packages("bsAlert")}
# if(!require(shinyBS)){install.packages("shinyBS")}
# if(!require(devtools)){install.packages("devtools")}
# if(!require(testthat)){install.packages("testthat")}
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
#setwd('/Volumes/bs-dfs/group/borgwardt/Projects/SCI_Drugs/shinyapp/acute_pharmacological_management_sci')

#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####


#---------- Add sources ----------# 
source("helper_functions_2.R")

########## Data sets ##########

#---------- Data sets for Sygen ---------- 
network_data_sygen <- read.csv('data/edges_for_graph.csv', header = T, sep = ',')
data_network_sygen <- read.csv('data/nr.of.patients.per.drug.per.day.csv', header=TRUE, sep=',')
load("data/sygen_baseline.RData")
load('data/acute_pharmacol_management.data.sygen.RData')
load("data/sygen_acute_pharmacol_management.data.ind.sygen.RData")
load("data/df_heatmap_sygen_drug.RData")
load("data/df_heatmap_sygen_indication.RData")

colnames(df_heatmap_sygen_drugs_copy)[which(names(df_heatmap_sygen_drugs_copy) == "Days after injury")] <- "Days_after_injury"
vec_drug_sygen <- names(df_heatmap_sygen_drugs_copy)[7:dim(df_heatmap_sygen_drugs_copy)[2]]

#---------- Data sets for SCIRehab ---------- 
network_data_scirehab <- read.csv('data/edges_for_graph.scirehab.csv', header = T, sep = ',')
data_network_scirehab <- read.csv('data/nr.of.patients.per.drug.per.day.scirehab.csv', header=TRUE, sep=',')
load("data/scirehab_baseline.RData")
load("data/acute_pharmacol_management.data.scirehab.RData")
load("data/acute_pharmacol_management.data.per.ais.grade.RData")
#load("data/df_heatmap_scirehab_drug.RData")

#font_import("Times")

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
  
  # Define title, icon, and width of title
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
                                        menuSubItem("Medications", tabName = "drug_sygen", icon = icon("prescription")),
                                        menuSubItem("Polypharmacy", tabName = "polypharmacy_sygen", icon = icon("dice-d20")),
                                        menuSubItem("Drug administration pattern", tabName = "drug_pattern_sygen", icon = icon("chart-bar"))),
                               menuItem('SCIRehab', tabName = 'scirehab', icon=icon("database"),
                                        menuSubItem("About", tabName = "about_scirehab", icon = icon("info-circle")),
                                        menuSubItem("Cohort", tabName = "cohort_scirehab", icon = icon("users")), 
                                        menuSubItem("Medications", tabName = "drug_scirehab", icon = icon("prescription")),
                                        menuSubItem("Polypharmacy", tabName = "polypharmacy_scirehab", icon = icon("dice-d20")),
                                        menuSubItem("Drug administration pattern", tabName = "drug_pattern_scirehab", icon = icon("chart-bar"))),
                               menuItem("Abbreviations", tabName = "abbreviations", icon = icon("language")),
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
                valueBox(prettyNum(2040, big.mark=" ", scientific=FALSE), "Patients", icon = icon("user-edit"), width = 3, color = "purple"),
                valueBox(prettyNum(770, big.mark=" ", scientific=FALSE), "Unique drugs", icon = icon("pills"), width = 3,  color = "purple"),
                valueBox(tagList("10", tags$sup(style="font-size: 20px", "%")),
                         "Prophylactic drug use", icon = icon("prescription"),  width = 3,  color = "purple"
                ),
                #valueBox(prettyNum(10, big.mark="", scientific=FALSE), "Prophylaxis", icon = icon("heartbeat"), width = 3,  color = "purple"),
                valueBox("34", "Clinical sites", icon = icon("clinic-medical"), width = 3,  color = "purple")#,
                #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              ),
              
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
                  Nearly every individual sustaining spinal cord injury receives multiple types and classes of drugs to manage a litany of problems 
                  associated with traumatic spinal cord injury. We performed an analysis of available clinical trial and observational data to determine 
                  what constitutes standards of acute pharmacological care after traumatic spinal cord injury. The goal of this study was to determine 
                  the types of drugs commonly administered, alone or in combination, in the acute phase of spinal cord injury. To this end, we conducted 
                  an analysis of available clinical trial and observational data to determine what constitutes standards of acute pharmacological care
                  after spinal cord injury. Concomitant drug use (i.e., non-randomized drugs), including dosage, timing and reason for administration, 
                  was tracked through the duration of the trial and observational study. Descriptive statistics were used to describe the drugs administered 
                  within the first 90 days after spinal cord injury. R Statistical Software was used for all statistical analyses and to create plots for data visualization.
                  Over 770 unique drugs were administered within the first month after injury. On average, patients received 20 unique drugs (range 1-58), 
                  often in a combinatorial or overlapping fashion (i.e., polypharmacy). Approximately 10% of drugs were administered acutely as prophylaxis 
                  (e.g., pain, infections). Our study revealed a high degree of polypharmacy in the acute stages of spinal cord injury, with potential to both positively and negatively impact neurological recovery.",
                  br(),
                  br(),
                  h4("Study team"),
                  h5("Principal Investigator:"), 
                  tags$ul(
                    tags$li(strong("Dr. Catherine Jutzeler,"), "Research Group Leader, Department of Biosystems Science and Engineering, Swiss Federal Institute of Technology (ETH Zurich).",
                            tags$a(href="mailto:Catherine.Jutzeler@bsse.ethz.ch", 
                                   target="_blank",
                                   icon("envelope")))),
                  h5("Co-investigators:"), 
                  tags$ul(
                    tags$li(strong("Lucie Bourguignon,"), "Department of Biosystems Science and Engineering, ETH Zurich and SIB Swiss Institute of Bioinformatics, Basel, Switzerland."
                    ),
                    tags$li(strong("Prof. John Kramer, "), "Department of Anesthesiology, Pharmacology, and Therapeutics, Faculty of Medicine, University of British Columbia, Canada."
                    ),
                    tags$li(strong("Prof. Jacquelyn Cragg,"), "Faculty of Pharmaceutical Sciences, University of British Columbia, Vancouver, Canada."
                    )),
                  h5("Collaborators:"), 
                  tags$ul(
                    
                    tags$li(strong("Dr. Lukas Grassner,"), "Department of Neurosurgery, Medical University Innsbruck, Innsbruck, Austria."
                    ),
                    tags$li(strong("Dr. Fred Geisler,"), "University of Saskatchewan, Saskatoon, Saskatchewan, Canada"
                    ),
                    tags$li(strong("Bobo Tong,"), "International Collaboration on Repair Discoveries (ICORD), University of British Columbia, Vancouver, Canada."
                    ),
                    tags$li(strong("Dr. Elias Ronca,"), "Swiss Paraplegic Research, Notwil, Switzerland."
                    ),
                    tags$li(strong("Dr. Noam Y. Harel,"), "James J Peters Veterans Affairs Medical Center, Bronx, New York; Icahn School of Medicine at Mount Sinai, New York,  USA."
                    ),
                    tags$li(strong("Prof. Adam Ferguson,"), "Brain and Spinal Injury Center, Weill Institute for Neurosciences, University of California San Francisco (UCSF), San Francisco, USA."
                    ),
                    tags$li(strong("Prof. Brian Kwon,"), "International Collaboration on Repair Discoveries (ICORD), University of British Columbia, Vancouver, Canada.")),
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
                  tags$b("five main interactive sections per dataset"),
                  "that enables users to directly interact with the RxSCI data: ",
                  tags$ul(
                    tags$li("The", tags$b("About tabs"), "provide a detailed description (e.g., objective of study, 
                            inclusion and exclusion criteria, results) of the original",
                            a("Sygen clinical trial", onclick = "openTab('about_sygen')", href="#"), 
                            "or",
                            a("SCIRehab study", onclick = "openTab('about_scirehab')", href="#"), 
                            ", respectively."),
                    tags$li("The", tags$b("Cohort tabs"), '(',
                            a("Sygen", onclick = "openTab('cohort_sygen')", href="#"), "&",
                            a("SCIRehab", onclick = "openTab('cohort_scirehab')", href="#"),
                            ") offers an insight into the diversity of the patients of both data sources. 
                            We share information on demographics and injury characteristics. Please note 
                            that to protect the privacy and anonymity of the patients, data visualizations 
                            are only available for the group level (i.e., aggregated values for age)."),
                    tags$li("The", tags$b("Medication tabs"), '(',
                            a("Sygen", onclick = "openTab('drug_sygen')", href="#"), "&",
                            a("SCIRehab", onclick = "openTab('drug_scirehab')", href="#"),
                            ") give the user the possibility to interactively explore the number of drugs 
                            that were administered to patients on a daily dose. For the Sygen cohort, the 
                            drugs per indication can be explored. The user can choose to visualize the 
                            result of the whole cohort or customized subgroups."),
                    tags$li("The ",tags$b("Polypharmacy tabs"), '(',
                            a("Sygen", onclick = "openTab('polypharmacy_sygen')", href="#"), "&",
                            a("SCIRehab", onclick = "openTab('polypharmacy_scirehab')", href="#"),
                            ") offer an interactive interface to explore the combinations of drugs that were 
                            administered to patients. This group-level data is provided for everyday up to 
                            60 days post injury. In addition, the user can also choose to look at specific 
                            patient examples of polypharmacy."),
                    tags$li("The ",tags$b("Drug administration pattern tabs"), '(',
                            a("Sygen", onclick = "openTab('drug_pattern_sygen')", href="#"), "&",
                            a("SCIRehab", onclick = "openTab('drug_pattern_scirehab')", href="#"),
                            ") provide the user with the possibility to explore how the user-selected drug 
                            was administered to the different patients of each data source.")
                  ),
                  br(),
                  h4("Funding:"),
                  p('This project is supported by the ',
                    a('Swiss National Science Foundation', href = 'http://p3.snf.ch/project-186101', target="_blank"),
                    ' (Ambizione Grant, #PZ00P3_186101), ',
                    a('Wings for Life Research Foundation', href = 'https://www.wingsforlife.com/en/', target="_blank"),
                    ' (#2017_044), and the ',
                    a('Craig H Neilsen Foundation', href = 'https://chnfoundation.org/', target="_blank"),
                    '.', align = "justify")
                ),
                box(width = 4,
                    HTML("<a class=\"twitter-timeline\" data-height=\"600\" href=\"https://twitter.com/DatSci_4_health\">A Twitter List by Data Science for Health Research Group</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                )
              ),
              # fluidRow(
              #   valueBox(prettyNum(2040, big.mark=" ", scientific=FALSE), "Patients", icon = icon("user-edit"), width = 3, color = "purple"),
              #   valueBox(prettyNum(770, big.mark=" ", scientific=FALSE), "Unique drugs", icon = icon("pills"), width = 3,  color = "purple"),
              #   valueBox(tagList("10", tags$sup(style="font-size: 20px", "%")),
              #            "Prophylactic drug use", icon = icon("prescription"),  width = 3,  color = "purple"
              #   ),
              #   #valueBox(prettyNum(10, big.mark="", scientific=FALSE), "Prophylaxis", icon = icon("heartbeat"), width = 3,  color = "purple"),
              #   valueBox("34", "Clinical sites", icon = icon("clinic-medical"), width = 3,  color = "purple")#,
              #   #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              # )
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
                  valueBox(prettyNum(489, big.mark=" ", scientific=FALSE), "Unique concomittant medications to treat secondary complications", icon = icon("pills"), width = 3,  color = "purple"),
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
      ),   #close tabitem (cohort sygen)
      
      # Tab: Sygen drug
      tabItem(tabName = "drug_sygen",
              fluidRow(
                column(width = 8,
                       
                       box(width = NULL,
                           
                           div(style="display:inline-block;width:100%;text-align:center;margin-top:7px;margin-bottom:7px;",
                               radioGroupButtons(
                                 inputId = "pharmacol_management_sygen", 
                                 label = "Visualise pharmacological management at different scales", 
                                 selected = 'full_cohort_sygen',
                                 status = "success",
                                 individual = T, #if false, then the boxes are connected
                                 choiceNames = c("Group level", "Customized Subgroups"),
                                 choiceValues = c('full_cohort_sygen', 'sbgrps_sygen')
                               ) # Close radioGroupButtons bracket
                           ), # Close div bracket
                       ),
                       
                       conditionalPanel(condition = "input.pharmacol_management_sygen == 'full_cohort_sygen' ",
                                        
                                        
                                        box(title = "Overview of drugs per day and indication", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            dataTableOutput('table'),
                                            #downloadButton('downloadData',"Download the data")
                                        ), # end box
                                        
                                        
                                        box(title = "Unique drugs per day", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            
                                            
                                            div(plotlyOutput("plot_pharmacol_management_sygen", width = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                                        
                                        
                                        box(title = "Unique drugs per day and indication", 
                                            width = NULL, 
                                            div(style="display:inline-block;width:100%;text-align:center;",
                                                selectInput(inputId = "select_indication_pharmacol_management_sygen",
                                                            label = "Select an indication",
                                                            choices = list("Blood and lymphatic system disorders" = "Blood and lymphatic system disorders",
                                                                           "Cardiac disorders" = "Cardiac disorders",
                                                                           "Ear and labyrinth disorders" = "Ear and labyrinth disorders",
                                                                           "Eye disorders" = "Eye disorders",
                                                                           "Gastrointestinal disorders" = "Gastrointestinal disorders",
                                                                           "General disorders and administration site conditions" = "General disorders and administration site conditions",
                                                                           "Immune system disorders" = "Immune system disorders",
                                                                           "Infections and infestations" = "Infections and infestations",
                                                                           "Injury, poisoning and procedural complications" = "Injury, poisoning and procedural complications",
                                                                           "Metabolism and nutrition disorders" = "Metabolism and nutrition disorders",
                                                                           "Musculoskeletal and connective tissue disorders" = "Musculoskeletal and connective tissue disorders",
                                                                           "Nervous system disorders" = "Nervous system disorders",
                                                                           "Pain" = "Pain",
                                                                           "Psychiatric disorders" = "Psychiatric disorders",
                                                                           "Renal and urinary system disorders" = "Renal and urinary system disorders",
                                                                           "Respiratory, thoracic and mediastinal disorders" = "Respiratory, thoracic and mediastinal disorders",
                                                                           "Skin and subcutaneous tissue disorders" = "Skin and subcutaneous tissue disorders",
                                                                           "Surgical and medical procedures" = "Surgical and medical procedures",
                                                                           "Vascular disorders" = "Vascular disorders",
                                                                           "Sygen Protocol" = "Sygen Protocol",
                                                                           "Unknown" = "Unknown"),
                                                            selected = c("Blood and lymphatic system disorders"),
                                                            multiple = F) # Close radioGroupButtons bracket
                                                
                                            ), # Close div bracket
                                            div(plotlyOutput("plot_pharmacol_management_by_indication_sygen", width = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                                        
                                        box(title = "Number of unique drugs per patient (mean [min -max])", 
                                            width = NULL, 
                                            heigth = 800,
                                            solidHeader = TRUE,
                                            
                                            
                                            div(plotlyOutput("plot_pharmacol_management_ind_sygen", width = "100%", height = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                                        
                                        
                                        
                       ), # end conditionalPanel
                       
                       
                       conditionalPanel(condition = "input.pharmacol_management_sygen != 'full_cohort_sygen' ",
                                        
                                        box(title = "Overview of drugs per day and indication", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            dataTableOutput('table_srgp'),
                                            # downloadButton('downloadData',"Download the data")
                                        ), # end box
                                        
                                        box(title = "Unique drugs per day", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            
                                            
                                            div(plotlyOutput("plot_pharmacol_management_srgp_sygen", width = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                                        
                                        
                                        box(title = "Unique drugs per day and indication", 
                                            width = NULL, 
                                            div(style="display:inline-block;width:100%;text-align:center;",
                                                selectInput(inputId = "select_indication_pharmacol_management_sgrp_sygen",
                                                            label = "Select an indication",
                                                            choices = list("Blood and lymphatic system disorders" = "Blood and lymphatic system disorders",
                                                                           "Cardiac disorders" = "Cardiac disorders",
                                                                           "Ear and labyrinth disorders" = "Ear and labyrinth disorders",
                                                                           "Eye disorders" = "Eye disorders",
                                                                           "Gastrointestinal disorders" = "Gastrointestinal disorders",
                                                                           "General disorders and administration site conditions" = "General disorders and administration site conditions",
                                                                           "Immune system disorders" = "Immune system disorders",
                                                                           "Infections and infestations" = "Infections and infestations",
                                                                           "Injury, poisoning and procedural complications" = "Injury, poisoning and procedural complications",
                                                                           "Metabolism and nutrition disorders" = "Metabolism and nutrition disorders",
                                                                           "Musculoskeletal and connective tissue disorders" = "Musculoskeletal and connective tissue disorders",
                                                                           "Nervous system disorders" = "Nervous system disorders",
                                                                           "Pain" = "Pain",
                                                                           "Psychiatric disorders" = "Psychiatric disorders",
                                                                           "Renal and urinary system disorders" = "Renal and urinary system disorders",
                                                                           "Respiratory, thoracic and mediastinal disorders" = "Respiratory, thoracic and mediastinal disorders",
                                                                           "Skin and subcutaneous tissue disorders" = "Skin and subcutaneous tissue disorders",
                                                                           "Surgical and medical procedures" = "Surgical and medical procedures",
                                                                           "Vascular disorders" = "Vascular disorders",
                                                                           "Sygen Protocol" = "Sygen Protocol",
                                                                           "Unknown" = "Unknown"),
                                                            selected = c("Blood and lymphatic system disorders"),
                                                            multiple = F) # Close radioGroupButtons bracket
                                                
                                            ), # Close div bracket
                                            div(plotlyOutput("plot_pharmacol_management_by_indication_sgrp_sygen", width = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                       ), # end conditionalPanel 
                ),# close column
                
                
                
                
                conditionalPanel(condition = "input.pharmacol_management_sygen != 'full_cohort_sygen' ",
                                 
                                 
                                 column(width = 4, # create second column for second type of user inputs (filters)
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_sex_pharmacol_management_sbgrp_sygen",
                                                        label = "Select sex",
                                                        choices = list("Male" = "Male", "Female" = "Female", "Unknown" = "Unknown"),
                                                        selected = c("Male"))
                                        ), # end box
                                        
                                        box(width = NULL, # create box
                                            sliderInput("select_age_pharmacol_management_sbgrp_sygen",
                                                        label = "Select age at injury",
                                                        min = 10, max = 100,
                                                        value = c(20,80)),
                                            
                                            
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_ais_pharmacol_management_sbgrp_sygen",
                                                        label = "Select baseline AIS grade",
                                                        choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E", "Unknown" = "Unknown"),
                                                        selected = c("AIS A"))
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_nli_pharmacol_management_sbgrp_sygen",
                                                        label = "Select injury level",
                                                        choices = list("Cervical", "Thoracic"),
                                                        selected = c("Cervical"))
                                        )#, # end box
                                 ) #end column
                )#end conditional Panel
              )#close fluid row
              
      ), # Close tab item (drug_sygen)
      
      # Tab: Sygen Medication
      tabItem(tabName = "polypharmacy_sygen",
              fluidRow(
                column(width = 8,
                       
                       box(width = NULL,
                           
                           div(style="display:inline-block;width:100%;text-align:center;margin-top:7px;margin-bottom:7px;",
                               radioGroupButtons(
                                 inputId = "poly_sygen_ind", 
                                 label = "Visualise polypharmacy plot at different scales", 
                                 selected = 'gp_sygen',
                                 status = "success",
                                 individual = T, #if false, then the boxes are connected
                                 choiceNames = c("Group level", "Individual level"),
                                 choiceValues = c('gp_sygen', 'ind_sygen')
                               ) # Close radioGroupButtons bracket
                           ), # Close div bracket
                       ),
                       
                       # box(width = NULL,
                       #     checkboxInput(inputId = "poly_sygen_ind", 
                       #                   label = "Plot polypharmacy at individual level", 
                       #                   value = FALSE, width = NULL)),
                       
                       box(title = "Explore polypharmacy per day after injury", 
                           width = NULL, 
                           heigth = "300px",
                           solidHeader = TRUE,
                           sliderInput("day_polypharmacy_sygen", "Day after injury:",
                                       min = 0, max = 60, value = 7),
                           
                           conditionalPanel(condition = "input.poly_sygen_ind == 'gp_sygen' ",
                                            div(plotOutput("plot_polypharmacy_sygen", width = "100%",
                                                           inline = FALSE), 
                                                align='center')),
                           conditionalPanel(condition = "input.poly_sygen_ind != 'gp_sygen' ",
                                            div(plotOutput("plot_poly_ind_sygen", width = "100%",
                                                           inline = FALSE), 
                                                align='center')),
                       ), # end box 
                       
                       box(title = "How to intepret this visualization", 
                           width = NULL,
                           solidHeader = TRUE,
                           conditionalPanel(condition = "input.poly_sygen_ind == 'gp_sygen' ",
                                            p('This plot represents the ',
                                              strong('network of medications administered in combination', align='justify'),
                                              '. The nodes of the network represent the medications. 
                                                            The size of the nodes represents the number of patients that 
                                                            have received this particular medication the day you selected (default is day 7). 
                                                            Medications that were administered together 
                                                            on that specific day are connected via an edge. 
                                                            The width of the edge represents the number of patients that 
                                                            have received the two medications (e.g. acetaminophen and ketorolac) 
                                                            in combination on the day of interest.', align='justify')
                           ),
                           conditionalPanel(condition = "input.poly_sygen_ind != 'gp_sygen' ",
                                            p('This plot shows examples of longitudinal medication profiles 
                                                            for four patients in the first days post injury. The four patients 
                                                            share the characteristics chosen from the right panel.')
                           ),
                       ), # end box 
                       
                ),# close column
                
                conditionalPanel(condition = "input.poly_sygen_ind != 'gp_sygen' ",
                                 column(width = 4, # create second column for second type of user inputs (filters)
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_sex_poly_ind_sygen",
                                                        label = "Select sex",
                                                        choices = list("Male" = "Male", "Female" = "Female", "Unknown" = "Unknown"),
                                                        selected = c("Unknown"))
                                        ), # end box
                                        
                                        box(width = NULL, # create box
                                            sliderInput("select_age_poly_ind_sygen",
                                                        label = "Select age at injury",
                                                        min = 10, max = 100,
                                                        value = c(20,80)),
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_ais_poly_ind_sygen",
                                                        label = "Select baseline AIS grade",
                                                        choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E", "Unknown" = "Unknown"),
                                                        selected = c("Unknown"))
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_nli_poly_ind_sygen",
                                                        label = "Select injury level",
                                                        choices = list("Unknown", "Cervical", "Thoracic"),
                                                        selected = c("Unknown"))
                                        )#, # end box
                                 ) #end column
                )#end conditional Panel
              )#close fluid row
              
      ), # Close tab item (Sygen Polypharmacy)
      
      tabItem(tabName = "drug_pattern_sygen",
              fluidRow(
                column(width = 8, # create first column for boxplot
                       
                       div(style="display:inline-block;width:100%;text-align:center;margin-top:7px;margin-bottom:7px;",
                           radioGroupButtons(
                             inputId = "type_drug_sygen", 
                             label = "Visualise drug pattern plot acording tp", 
                             selected = 'cat_drug_sygen',
                             status = "success",
                             individual = T, #if false, then the boxes are connected
                             choiceNames = c("Drug Indication", "Specific drug"),
                             choiceValues = c('cat_drug_sygen', 'spe_drug_sygen')
                           ) # Close radioGroupButtons bracket
                       ), # Close div bracket
                       
                       
                       box(width = NULL, # create box to display plot
                           align="center", # center the plot
                           #conditionalPanel(condition = "input.type_drug_sygen == 'cat_drug_sygen' ",
                                            plotOutput('plot_drug_pattern_sygen', height = 660)#),
                           #conditionalPanel(condition = "input.type_drug_sygen == 'spe_drug_sygen' ",
                                            #plotOutput('plot_drug_pattern_sygen', height = 660))
                                            #img(src="work_in_progress.png", height="80%", width="80%"))
                           ),
                ), # end column
                
                column(width = 4, # create second column for second type of user inputs (filters)
                       box(width=NULL,
                           
                           conditionalPanel(condition = "input.type_drug_sygen == 'cat_drug_sygen' ",
                                            selectInput("select_cat_drug_sygen",
                                                        label = "Select an indication:",
                                                        choices = list("Blood and lymphatic system disorders" = "Blood and lymphatic system disorders",
                                                                       "Cardiac disorders" = "Cardiac disorders",
                                                                       "Ear and labyrinth disorders" = "Ear and labyrinth disorders",
                                                                       "Eye disorders" = "Eye disorders",
                                                                       "Gastrointestinal disorders" = "Gastrointestinal disorders",
                                                                       "General disorders and administration site conditions" = "General disorders and administration site conditions",
                                                                       "Immune system disorders" = "Immune system disorders",
                                                                       "Infections and infestations" = "Infections and infestations",
                                                                       "Injury, poisoning and procedural complications" = "Injury, poisoning and procedural complications",
                                                                       "Metabolism and nutrition disorders" = "Metabolism and nutrition disorders",
                                                                       "Musculoskeletal and connective tissue disorders" = "Musculoskeletal and connective tissue disorders",
                                                                       "Nervous system disorders" = "Nervous system disorders",
                                                                       "Pain" = "Pain",
                                                                       "Psychiatric disorders" = "Psychiatric disorders",
                                                                       "Renal and urinary system disorders" = "Renal and urinary system disorders",
                                                                       "Respiratory, thoracic and mediastinal disorders" = "Respiratory, thoracic and mediastinal disorders",
                                                                       "Skin and subcutaneous tissue disorders" = "Skin and subcutaneous tissue disorders",
                                                                       "Surgical and medical procedures" = "Surgical and medical procedures",
                                                                       "Vascular disorders" = "Vascular disorders",
                                                                       "Sygen Protocol" = "Sygen Protocol",
                                                                       "Unknown" = "Unknown"),
                                                        selected = "Blood and lymphatic system disorders"
                                            )
                           ), #end conditionalPanel
                           
                           conditionalPanel(condition = "input.type_drug_sygen == 'spe_drug_sygen' ",
                                            selectInput("select_spe_drug_sygen",
                                                        label = "Select a specific drug:",
                                                        choices = vec_drug_sygen,
                                                        #choices = 'aspirin',
                                                        selected = 'aspirin'
                                            )
                           ), #end conditionalPanel
                           
                       ), # end box
                       
                       box(width = NULL,
                           sliderInput("day_drug_sygen", "Day after injury:",
                                       min = 0, max = 60, value = 7)), # end Box
                       
                       box(width = NULL, # create a new box
                           selectInput("select_sex_drug_pattern_sygen",
                                       label = "Select sex",
                                       choices = list("Male" = "Male", "Female" = "Female", "Unknown" = "Unknown"),
                                       selected = c("Unknown"))
                       ), # end box
                       
                       box(width = NULL, # create box
                           sliderInput("select_age_drug_pattern_sygen",
                                       label = "Select age at injury",
                                       min = 10, max = 100,
                                       value = c(20,80)),
                       ), # end box
                       
                       box(width = NULL, # create a new box
                           selectInput("select_ais_drug_pattern_sygen",
                                       label = "Select baseline AIS grade",
                                       choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E", "Unknown" = "Unknown"),
                                       selected = c("Unknown"))
                       ), # end box
                       
                       box(width = NULL, # create a new box
                           selectInput("select_nli_drug_pattern_sygen",
                                       label = "Select injury level",
                                       choices = list("Unknown",
                                                      "Cervical",
                                                      "Thoracic"),
                                       selected = c("Unknown"))
                       ), # end box
                ) #end column
                
              )#close fluid row
              
      ), # Close tab item (Sygen Drug Pattern)
      
      
      tabItem(tabName = "about_scirehab",
              h3(strong("Spinal Cord Injury Rehabilitation Study")),
              br(),
              fluidRow(
                box(#title = "Explore The Data", 
                  width = 8, 
                  heigth = "300px",
                  solidHeader = TRUE,
                  
                  h4("Objectives"),
                  "In an effort to understand the relationship between the rehabilitation process and outcomes, the SCIRehab study collected data about rehabilitation interventions across 7 disciplines during the inpatient rehabilitation of over 1200 people with spinal cord injury. 
                                This study used practice-based evidence methods to relate the details of the rehabilitation process to outcomes after controlling for 
                                individual demographic and injury characteristics",
                  br(),
                  h4("Methods"),
                  strong("Study design."), "Longitudinal, observational, prospective event-based cohort study.",
                  br(),
                  br(),
                  strong("Inclusion/ exlusion criteria."), "The SCIRehab facilities enrolled all patients who were 12 years of age or older, gave (or whose parent/guardian gave) informed consent, and were admitted to the facility's SCI unit for initial rehabilitation following traumatic SCI. 
                                Duration of the acute-hospital inpatient admission preceding rehabilitation was not an enrollment criterion. Patients requiring transfer to acute care units or facilities during their rehabilitation program were retained in the study, no matter how long they spent in acute 
                                care before returning to the rehabilitation unit, but their acute care days were not counted as part of the rehabilitation stay. To restrict the study to initial rehabilitation cases, a small number of patients were excluded who spent more than 2 weeks in another rehabilitation 
                                center prior to admission to the SCIRehab facility. To ensure complete rehabilitation data, patients who spent more than a week of their rehabilitation stay on a non-spinal cord injury rehabilitation unit in the SCIRehab facility (staff of the non-SCI units were not trained in the data collection methods)
                                also were excluded. There were no other exclusion criteria.",
                  br(),
                  br(),
                  strong("Assessments."),
                  "Patients were followed for first year 
                                post-injury and were excluded if they spent two or more weeks at a non-participating rehabilitation center. Patient demographics and injury characteristics were extracted from the patient medical record (part of the National Institute on Disability and Rehabilitation 
                                Research Spinal Cord Injury Model Systems Form I). The International Standards of Neurological Classification of SCI (ISNCSCI) and its American Spinal Injury Association Impairment Scale (AIS) were used to describe the neurologic level and completeness of injury; the Functional Independence Measure (FIM)
                                served to describe a patient's functional independence in motor and cognitive tasks at admission and discharge, and monitor functional gains; and the Comprehensive Severity Index (CSI) was used to provide an overall summary measure of how ill (i.e., extent of deviation from normal)
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
              
      ), #close tabitem
      
      tabItem(tabName = "drug_scirehab",
              fluidRow(
                column(width = 8,
                       
                       box(width = NULL,
                           
                           div(style="display:inline-block;width:100%;text-align:center;margin-top:7px;margin-bottom:7px;",
                               radioGroupButtons(
                                 inputId = "pharmacol_management_scirehab", 
                                 label = "Visualise pharmacological management at different scales", 
                                 selected = 'full_cohort_scirehab',
                                 status = "success",
                                 individual = T, #if false, then the boxes are connected
                                 choiceNames = c("Group level", "Customized Subgroups"),
                                 choiceValues = c('full_cohort_scirehab', 'sbgrps_scirehab')
                               ) # Close radioGroupButtons bracket
                           ), # Close div bracket
                       ), # Close box
                       
                       conditionalPanel(condition = "input.pharmacol_management_scirehab == 'full_cohort_scirehab' ",
                                        
                                        box(title = "Overview of drugs per day and indication", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            dataTableOutput('table_scirehab'),
                                            #downloadButton('downloadData',"Download the data")
                                        ), # end box
                                        
                                        box(title = "Unique drugs per day", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            
                                            
                                            div(plotlyOutput("plot_pharmacol_management_scirehab", width = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                                        
                                        box(title = "Number of unique drugs per patient (mean [min -max])", 
                                            width = NULL, 
                                            heigth = 800,
                                            solidHeader = TRUE,
                                            
                                            
                                            div(plotlyOutput("plot_pharmacol_management_per_day_scirehab", width = "100%", height = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                       ), # end conditionalPanel
                       
                       
                       conditionalPanel(condition = "input.pharmacol_management_scirehab != 'full_cohort_scirehab' ",
                                        
                                        box(title = "Overview of drugs per day and indication", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            dataTableOutput('table_srgp_scirehab'),
                                            # downloadButton('downloadData',"Download the data")
                                        ), # end box
                                        
                                        box(title = "Unique drugs per day", 
                                            width = NULL, 
                                            heigth = "300px",
                                            solidHeader = TRUE,
                                            
                                            
                                            div(plotlyOutput("plot_pharmacol_management_srgp_scirehab", width = "100%",
                                                             inline = FALSE),
                                                align='center')
                                        ), # end box
                                        
                       ), # end conditionalPanel    
                ),# close column
                
                
                conditionalPanel(condition = "input.pharmacol_management_scirehab != 'full_cohort_scirehab' ",
                                 
                                 
                                 column(width = 4, # create second column for second type of user inputs (filters)
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_sex_pharmacol_management_sbgrp_scirehab",
                                                        label = "Select sex",
                                                        choices = list("Male" = "Male", "Female" = "Female"),
                                                        selected = c("Male"))
                                        ), # end box
                                        
                                        box(width = NULL, # create box
                                            selectInput("select_age_pharmacol_management_sbgrp_scirehab",
                                                        label = "Select age at injury",
                                                        choices = list("0-19 yrs", "20-29 yrs", "30-39 yrs","40-49 yrs", "50-59 yrs", 
                                                                       "60-69 yrs", "70-79 yrs", "80+ yrs"),
                                                        selected = c("20-29 yrs"))
                                            
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_ais_pharmacol_management_sbgrp_scirehab",
                                                        label = "Select baseline AIS grade",
                                                        choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                        selected = c("AIS A"))
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_nli_pharmacol_management_sbgrp_scirehab",
                                                        label = "Select injury level",
                                                        choices = list("Cervical", "Thoracic", "Lumbar"),
                                                        selected = c("Cervical"))
                                        )#, # end box
                                 ) #end column
                )#end conditional Panel
              )#close fluid row
              
      ), # Close tab item (drug_scirehab)
      
      tabItem(tabName = "polypharmacy_scirehab",
              fluidRow(
                column(width = 8,
                       
                       box(width = NULL,
                           
                           div(style="display:inline-block;width:100%;text-align:center;margin-top:7px;margin-bottom:7px;",
                               radioGroupButtons(
                                 inputId = "poly_scirehab_ind", 
                                 label = "Visualise polypharmacy plot at different scales", 
                                 selected = 'gp_scirehab',
                                 status = "success",
                                 individual = T, #if false, then the boxes are connected
                                 #choiceNames = c("Group level"),
                                 #choiceValues = c('gp_scirehab'),
                                 choiceNames = c("Group level", "Individual level"),
                                 choiceValues = c('gp_scirehab', 'ind_scirehab')
                               ) # Close radioGroupButtons bracket
                           ), # Close div bracket
                       ),
                       
                       # box(width = NULL,
                       #     checkboxInput(inputId = "poly_scirehab_ind", 
                       #                   label = "Plot polypharmacy at individual level", 
                       #                   value = FALSE, width = NULL)),
                       
                       box(title = "Explore polypharmacy per day after injury", 
                           width = NULL, 
                           heigth = "300px",
                           solidHeader = TRUE,
                           sliderInput("day_polypharmacy_scirehab", "Day after injury:",
                                       min = 0, max = 60, value = 7),
                           
                           conditionalPanel(condition = "input.poly_scirehab_ind == 'gp_scirehab' ",
                                            div(plotOutput("plot_polypharmacy_scirehab", width = "100%",
                                                           inline = FALSE), 
                                                align='center')),
                           conditionalPanel(condition = "input.poly_scirehab_ind != 'gp_scirehab' ",
                                            img(src="work_in_progress.png", height="80%", width="80%")
                                            ),
                                            #div(plotOutput("plot_poly_ind_scirehab", width = "100%",
                                            #               inline = FALSE), 
                                            #    align='center')),
                       ), # end box 
                       
                       box(title = "How to intepret this visualization", 
                           width = NULL,
                           solidHeader = TRUE,
                           conditionalPanel(condition = "input.poly_scirehab_ind == 'gp_scirehab' ",
                                            p('This plot represents the ',
                                              strong('network of medications administered in combination', align='justify'),
                                              '. The nodes of the network represent the medications. 
                                                            The size of the nodes represents the number of patients that 
                                                            have received this particular medication the day you selected (default is day 7). 
                                                            Medications that were administered together 
                                                            on that specific day are connected via an edge. 
                                                            The width of the edge represents the number of patients that 
                                                            have received the two medications (e.g. acetaminophen and ketorolac) 
                                                            in combination on the day of interest.', align='justify')
                           ),
                           conditionalPanel(condition = "input.poly_scirehab_ind != 'gp_scirehab' ",
                                            p('This plot shows examples of longitudinal medication profiles 
                                                            for four patients in the first days post injury. The four patients 
                                                            share the characteristics chosen from the right panel.')
                           ),
                       ), # end box 
                       
                ),# close column
                
                conditionalPanel(condition = "input.poly_scirehab_ind != 'gp_scirehab' ",
                                 column(width = 4, # create second column for second type of user inputs (filters)
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_sex_poly_ind_scirehab",
                                                        label = "Select sex",
                                                        choices = list("Male" = "Male", "Female" = "Female", "Unknown" = "Unknown"),
                                                        selected = c("Unknown"))
                                        ), # end box
                                        
                                        box(width = NULL, # create box
                                            sliderInput("select_age_poly_ind_scirehab",
                                                        label = "Select age at injury",
                                                        min = 10, max = 100,
                                                        value = c(20,80)),
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_ais_poly_ind_scirehab",
                                                        label = "Select baseline AIS grade",
                                                        choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E", "Unknown" = "Unknown"),
                                                        selected = c("Unknown"))
                                        ), # end box
                                        
                                        box(width = NULL, # create a new box
                                            selectInput("select_nli_poly_ind_scirehab",
                                                        label = "Select injury level",
                                                        choices = list("Unknown", "Cervical", "Thoracic"),
                                                        selected = c("Unknown"))
                                        )#, # end box
                                 ) #end column
                )#end conditional Panel
              )#close fluid row
              
      ), # Close tab item (scirehab Polypharmacy)
      
      # Tab: SCI Rehab drugs patterns
      tabItem(tabName = "drug_pattern_scirehab",
              img(src="work_in_progress.png", height="80%", width="80%")
              # fluidRow(
              #   column(width = 8, # create first column for boxplot
              #          
              #          box(width = NULL, # create box to display plot
              #              align="center", # center the plot
              #              plotOutput('plot_drug_pattern_scirehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box
              #   ), # end column
              #   
              #   column(width = 4, # create second column for second type of user inputs (filters)
              #          box(width=NULL,
              #              selectInput("select_drug_scirehab",
              #                          label = "Select a specific drug:",
              #                          choices = gsub('_', ' ', levels(factor(drug_pattern_scirehab_ind$generic_name))),
              #                          selected = levels(factor(drug_pattern_scirehab_ind$generic_name))[7]
              #              )
              #          ), # end box
              #          
              #          box(width = NULL,
              #              sliderInput("day_drug_scirehab", "Day after injury:",
              #                          min = 0, max = 60, value = 7)
              #          ), # end Box
              #          
              #          box(width = NULL, # create a new box
              #              selectInput("select_sex_drug_pattern_scirehab",
              #                          label = "Select sex",
              #                          choices = list("Male" = "Male", "Female" = "Female", "Unknown" = "Unknown"),
              #                          selected = c("Unknown"))
              #          ), # end box
              #          
              #          box(width = NULL, # create box
              #              sliderInput("select_age_drug_pattern_scirehab",
              #                          label = "Select age at injury",
              #                          min = 10, max = 100,
              #                          value = c(20,80)),
              #          ), # end box
              #          
              #          box(width = NULL, # create a new box
              #              selectInput("select_ais_drug_pattern_scirehab",
              #                          label = "Select baseline AIS grade",
              #                          choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E", "Unknown" = "Unknown"),
              #                          selected = c("Unknown"))
              #          ), # end box
              #          
              #          box(width = NULL, # create a new box
              #              selectInput("select_nli_drug_pattern_scirehab",
              #                          label = "Select injury level",
              #                          choices = list("Unknown",
              #                                         "Cervical",
              #                                         "Thoracic"),
              #                          selected = c("Unknown"))
              #          ), # end box
              #          
              #   ) #end column
              #   
              # )#close fluid row
      ), # Close tab item (SCI Rehab drugs patterns)
      
      tabItem(tabName = "abbreviations",
              titlePanel(strong("Dictionary of abbreviations")),
              fluidRow(
                column(width = 6,
                       box(width = NULL, status = "primary",
                           h4(strong('General')),
                           p(strong('SCI'), 'spinal cord injury'),
                           p(strong(a('ASIA', href ="https://asia-spinalinjury.org/", target="_blank")), 'american spinal injury association'),
                           p(strong(a('EMSCI', href ="https://www.emsci.org/", target="_blank")), 'european multicenter study about spinal cord injury'),
                           p(strong('PBE'), 'practice-based evidence')
                       ),
                       
                       box(width = NULL, status = "primary",
                           h4(strong('Functional outcomes')),
                           p(strong(a('WISCI', href = "http://www.spinalcordcenter.org/research/wisci_guide.pdf", target="_blank")), 'walking index for spinal cord injury'),
                           p(strong(a('test_6min', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), '6 minutes walking test'),
                           p(strong(a('test_10m', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), '10 meters walking test'),
                           p(strong(a('TUG', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), 'timed up and go test'),
                           p(strong(a('SCIM2', href = "https://www.emsci.org/index.php/project/the-assessments/independence", target="_blank")), 'spinal cord independence measure type 2'),
                           p(strong(a('SCIM3', href = "https://www.emsci.org/index.php/project/the-assessments/independence", target="_blank")), 'spinal cord independence measure type 3'),
                           p(strong('benzel'), 'modified benzel classification')
                       )
                       
                ), # end column
                
                column(width = 6,
                       box(width = NULL, status = "primary",
                           h4(strong(a('Neurological outcomes', href ="https://asia-spinalinjury.org/wp-content/uploads/2016/02/International_Stds_Diagram_Worksheet.pdf", target="_blank"))),
                           p(strong(a('AIS', href ='https://www.icf-casestudies.org/introduction/spinal-cord-injury-sci/american-spinal-injury-association-asia-impairment-scale#:~:text=The%20American%20Spinal%20Injury%20Association,both%20sides%20of%20the%20body', target="_blank")), 'ASIA impairment scale'),
                           p(strong('UEMS'), 'upper extremity motor score'),
                           p(strong('RUEMS'), 'right upper extremity motor score'),
                           p(strong('LUEMS'), 'left upper extremity motor score'),
                           p(strong('LEMS'), 'lower extremity motor score'),
                           p(strong('RLEMS'), 'right lower extremity motor score'),
                           p(strong('LLEMS'), 'left lower extremity motor score'),
                           p(strong('RMS'), 'right motor score'),
                           p(strong('LMS'), 'left motor score'),
                           p(strong('TMS'), 'total motor score'),
                           p(strong('RPP'), 'right pin prick'),
                           p(strong('LPP'), 'left pin prick'),
                           p(strong('TPP'), 'total pin prick'),
                           p(strong('RLT'), 'right light touch'),
                           p(strong('LLT'), 'left light touch'),
                           p(strong('TLT'), 'total light touch')
                       )
                       
                ) # end column
              ) # end fluidRow
      ) # end tabItem
      
      
    ) # close tabitems
  ) # close dashboard body
) # close ui


server <- function(input, output, session) {
  
  # in server
  show_modal_spinner() # show the modal window
  remove_modal_spinner() # remove it when done
  
  output$cohort <- renderMenu({
    sidebarMenu(
      menuItem("Cohort description", icon = icon("users"))
    )
  })
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))})
  
  # Create Data Alert
  # createAlert(session = session,
  #             anchorId = "dataAlert",
  #             #alertId="a1",
  #             title = paste(icon("warning"),"Data Notification"),
  #             content="To protect the privacy of all patients, this application only uses aggregate, anonymized data (i.e., no individual person is identifiable). 
  #             For further information, see our <a href='#' onclick=\"openTab('data')\">data description section</a>.",
  #             style = "warning")
  # 
  
  # #------- Plot GM1---------- 
  #   output$gm1 <- renderImage({
  #     return(list(src = "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/shinyapp/acute_pharmacological_management_sci/www/gm1.png",
  #                 type = "image/png",alt = "gm1", width = "200ptx"))
  #   }, deleteFile = FALSE) #where the src is wherever you have the picture
  #   
  
  #------- Plot baseline characteristics of Sygen patients ----------
  
  output$bar.plot.baseline.characteristic.sygen <- renderPlotly({
    
    if (input$var == "sex")  {
      
      width = c(0.8, 0.8, 0.8)
      
      sygen_baseline$Sex=factor(sygen_baseline$Sex, levels = c("Unknown", "Male", "Female" ))
      
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
      width.age.group = c(0.8, 0.8, 0.8, 0.8, 0.8)
      
      sygen_baseline$agegroup=factor(sygen_baseline$agegroup, levels = c("Unknown", "61+ yrs", "41-60 yrs", "21-40 yrs", "0-20 yrs" ))
      
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
    
    
    else if (input$var == "nli")  {width.nli = c(0.8, 0.8, 0.8)
    
    sygen_baseline$NLI=factor(sygen_baseline$NLI, levels = c('Unknown','Thoracic', "Cervical"))
    
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
      
      width.cause = c(0.8, 0.8, 0.8, 0.8, 0.8,0.8, 0.8, 0.8, 0.8, 0.8)
      
      sygen_baseline$Cause=factor(sygen_baseline$Cause, levels = c('Unknown','Others', "Water related", "Pedestrian", "Other sports", "Motorcycle", "Gun shot wound", "Fall", "Blunt trauma", "Automobile" ))
      
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
  
  # Overview figure of pharmacological management - Sygen entire cohort
  output$plot_pharmacol_management_sygen <- renderPlotly({ 
    if (input$pharmacol_management_sygen == "full_cohort_sygen"){ #full cohort selected
      
      plot <- fct_acute_pharmacol_management_sygen(input_day, acute_pharmacol_management.data.sygen)
      plot}
    
  })
  
  # Overview figure if pharmacological management - Sygen entire cohort
  output$plot_pharmacol_management_indication_sygen <- renderPlotly({ 
    if (input$pharmacol_management_sygen == "full_cohort_sygen"){ #full cohort selected
      
      plot <- fct_acute_pharmacol_management_sygen(input_day)
      plot}
    
  })
  
  # Table providing an overview of the drugs per day and indication - Sygen entire cohort
  output$table <- renderDataTable({
    if (input$pharmacol_management_sygen == "full_cohort_sygen"){ #full cohort selected
      
      table.for.crosstalk <- acute_pharmacol_management.data.sygen %>%
        dplyr::filter(dose != 0)%>%
        dplyr::group_by(day_x, generic_name,indication)%>%
        ungroup()%>%
        dplyr::select(generic_name, day, indication)%>%
        dplyr::distinct(generic_name,day,indication)%>%
        dplyr::rename(Day=day)%>%
        dplyr::rename(Indication=indication)%>%
        dplyr::rename(`Drug name`=generic_name)%>%
        datatable(rownames = FALSE)
      
    }
  })
  
  # Figure providing an overview of the drugs per day and indication - Sygen entire cohort
  output$plot_pharmacol_management_by_indication_sygen<-renderPlotly({ 
    if (input$pharmacol_management_sygen == "full_cohort_sygen"){
      
      fct_acute_pharmacol_management_by_indication_sygen<- function(day){
        
        # Create data set
        nr.of.patients.per.drug.per.day.per.indication <- acute_pharmacol_management.data.sygen %>%
          filter(indication == input$select_indication_pharmacol_management_sygen)%>%
          dplyr::filter(dose != 0)%>%
          dplyr::group_by(day, generic_name, indication) %>%
          dplyr::select(generic_name, day,indication)%>%
          ungroup()%>%
          dplyr::count(day,indication)%>% 
          dplyr::group_by(day,indication)
        nr.of.patients.per.drug.per.day.per.indication
        
        
        # Create plotly
        acute_pharmacol_management.by.indication.plot<-nr.of.patients.per.drug.per.day.per.indication%>%
          highlight_key(~day)%>%
          plotly::plot_ly(y = ~n,
                          x =  ~day)%>%
          plotly::add_bars(
            marker = list(color = 'rgb(96,92,168)'),
            width = ~0.9,
            text = ~paste("Days post injury:", day,
                          '</br></br>', "Number of drugs:", n
            ),
            hoverinfo = "text")%>%
          plotly::layout(xaxis = list(title = "Days post injury"),
                         yaxis = list(title = "Numbers of unique drugs"))%>%
          highlight(on = "plotly_hover", off = "plotly_doubleclick")
        
        acute_pharmacol_management.by.indication.plot
        
        return(acute_pharmacol_management.by.indication.plot)
        
      }
      
      
      plot <- fct_acute_pharmacol_management_by_indication_sygen()
      plot
      
    }
    
    
  }) #close function
  
  # Figure of drugs per patient (mean, max, min)  - Sygen entire cohort
  output$plot_pharmacol_management_ind_sygen <- renderPlotly({ 
    if (input$pharmacol_management_sygen == "full_cohort_sygen"){ #full cohort selected
      
      # Create color list  
      color_list <- c("#FFA500", "#EE6677", "#228833", "#4477AA", "#4B0082")
      
      # Create plot  
      number.of.drug.perday.sygen.plot <- ggplot(acute_pharmacol_management.data.ind.sygen, aes(x=day, y=mean, color = ais1))+
        geom_line(aes(x=day, y=mean, color=ais1), size=1)+
        geom_ribbon(aes(ymin=min,ymax=max,fill=ais1),color="grey",alpha=0.4) +  theme_bw(base_size = 12, base_family = "Open Sans") + xlim(1,60) +
        scale_fill_manual(values=color_list) + scale_color_manual(values=color_list) +
        facet_wrap(.~ais1, ncol = 1)+ 
        theme(legend.position="none", axis.text = element_text(color = 'black'), 
              axis.title = element_text(color = 'black'), 
              strip.text = element_text(color = 'black'))+
        xlab('Days post injury')+ylab("")
      number.of.drug.perday.sygen.plot
      
      ggplotly(number.of.drug.perday.sygen.plot, tooltip=c("x", "y", 'min', "max"), height = 800, width=800)
      
    }
    
  })
  
  # Table providing an overview of the drugs per day and indication  - Sygen customized subgroups
  output$table_srgp <- renderDataTable({
    if (input$pharmacol_management_sygen == "sbgrps_sygen"){ 
      
      # Subset the data for selected age range
      range = c(input$select_age_pharmacol_management_sbgrp_sygen[1] : input$select_age_pharmacol_management_sbgrp_sygen[2]) 
      acute_pharmacol_management.data.sygen2 <- acute_pharmacol_management.data.sygen[acute_pharmacol_management.data.sygen$Age %in% range, ]
      
      table.for.crosstalk <- acute_pharmacol_management.data.sygen2 %>%
        dplyr::filter(Sex== input$select_sex_pharmacol_management_sbgrp_sygen &
                        ais1 == input$select_ais_pharmacol_management_sbgrp_sygen&
                        NLI == input$select_nli_pharmacol_management_sbgrp_sygen)%>%
        #dplyr::filter(dose != 0)%>%
        #dplyr::select(-"indication")%>%
        dplyr::group_by(day_x, generic_name,indication)%>%
        ungroup()%>%
        dplyr::select(generic_name, day, indication)%>%
        dplyr::distinct(generic_name,day,indication)%>%
        dplyr::rename(Day=day)%>%
        dplyr::rename(Indication=indication)%>%
        dplyr::rename(`Drug name`=generic_name)%>%
        datatable(rownames = FALSE)
      
    }
  })
  
  # Overview figure of pharmacological management - Sygen customized subgroups
  output$plot_pharmacol_management_srgp_sygen <- renderPlotly({ 
    if (input$pharmacol_management_sygen == "sbgrps_sygen"){ # subgroup selected
      
      # Subset the data for selected age range
      range = c(input$select_age_pharmacol_management_sbgrp_sygen[1] : input$select_age_pharmacol_management_sbgrp_sygen[2]) 
      acute_pharmacol_management.data.sygen2 <- acute_pharmacol_management.data.sygen[acute_pharmacol_management.data.sygen$Age %in% range, ]
      
      nr.of.patients.per.drug.per.day <- acute_pharmacol_management.data.sygen2 %>%
        dplyr::filter(Sex == input$select_sex_pharmacol_management_sbgrp_sygen & 
                        ais1 == input$select_ais_pharmacol_management_sbgrp_sygen&
                        NLI == input$select_nli_pharmacol_management_sbgrp_sygen)%>%
        dplyr::filter(dose != 0)%>%
        dplyr::select(-"indication")%>%
        dplyr::group_by(day, generic_name) %>%
        dplyr::select(generic_name, day)%>%
        ungroup()%>%
        dplyr::count(day)%>% 
        dplyr::group_by(day)
      nr.of.patients.per.drug.per.day
      
      
      acute_pharmacol_management.plot<-nr.of.patients.per.drug.per.day%>%
        plotly::highlight_key(~day)%>%
        plotly::plot_ly(y = ~n,
                        x =  ~day)%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~0.9,
          text = ~paste("Days post injury:", day,
                        '</br></br>', "Number of drugs:", n
          ),
          hoverinfo = "text")%>%
        plotly::layout(xaxis = list(title = "Days post injury"),
                       yaxis = list(title = "Numbers of unique drugs"))%>%
        plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick")
      
    }
  }) # Close function
  
  # Overview figure of pharmacological management by indication - Sygen customized subgroups
  output$plot_pharmacol_management_by_indication_sgrp_sygen<-renderPlotly({ 
    if (input$pharmacol_management_sygen == "sbgrps_sygen"){
      
      
      # Subset the data for selected age range
      range = c(input$select_age_pharmacol_management_sbgrp_sygen[1] : input$select_age_pharmacol_management_sbgrp_sygen[2]) 
      acute_pharmacol_management.data.sygen2 <- acute_pharmacol_management.data.sygen[acute_pharmacol_management.data.sygen$Age %in% range, ]
      
      
      fct_acute_pharmacol_management_by_indication_sygen<- function(day){
        
        # Create data set
        nr.of.patients.per.drug.per.day.per.indication <- acute_pharmacol_management.data.sygen2 %>%
          dplyr::filter(Sex == input$select_sex_pharmacol_management_sbgrp_sygen & 
                          ais1 == input$select_ais_pharmacol_management_sbgrp_sygen &
                          NLI == input$select_nli_pharmacol_management_sbgrp_sygen)%>%
          dplyr::filter(indication == input$select_indication_pharmacol_management_sgrp_sygen)%>%
          dplyr::filter(dose != 0)%>%
          dplyr::group_by(day, generic_name, indication) %>%
          dplyr::select(generic_name, day,indication)%>%
          ungroup()%>%
          dplyr::count(day,indication)%>% 
          dplyr::group_by(day,indication)
        nr.of.patients.per.drug.per.day.per.indication
        
        
        # Create figure
        acute_pharmacol_management.by.indication.plot<-nr.of.patients.per.drug.per.day.per.indication%>%
          highlight_key(~day)%>%
          plotly::plot_ly(y = ~n,
                          x =  ~day)%>%
          plotly::add_bars(
            marker = list(color = 'rgb(96,92,168)'),
            width = ~0.9,
            text = ~paste("Days post injury:", day,
                          '</br></br>', "Number of drugs:", n
            ),
            hoverinfo = "text")%>%
          plotly::layout(xaxis = list(title = "Days post injury"),
                         yaxis = list(title = "Numbers of unique drugs"))%>%
          highlight(on = "plotly_hover", off = "plotly_doubleclick")
        
        acute_pharmacol_management.by.indication.plot
        
        return(acute_pharmacol_management.by.indication.plot)
      }
      
      plot <- fct_acute_pharmacol_management_by_indication_sygen()
      plot 
    }
  }) # Close function
  
  # Table providing an overview of the drugs per day and indication - SCIRehab entire cohort
  output$table_scirehab <- renderDataTable({
    if (input$pharmacol_management_scirehab == "full_cohort_scirehab"){ #full cohort selected
      
      nr.of.patients.per.drug.per.day.table <- acute_pharmacol_management.data.scirehab %>%
        dplyr::filter(prevalence != 0)%>%
        dplyr::group_by(day, generic_name) %>%
        dplyr::select(generic_name, day)%>%
        dplyr::arrange(day)%>%
        dplyr::distinct()%>%
        ungroup()%>%
        dplyr::rename(Day=day)%>%
        dplyr::rename(`Drug name`=generic_name)%>%
        datatable(rownames = FALSE)
    }
  }) # Close function
  
  # Overview figure of pharmacological management - SCIRehab entire cohort
  output$plot_pharmacol_management_scirehab <- renderPlotly({ 
    if (input$pharmacol_management_scirehab == "full_cohort_scirehab"){ #full cohort selected
      
      plot <- fct_acute_pharmacol_management.data.scirehab(input_day, acute_pharmacol_management.data.scirehab)
      plot
    }
  }) # Close function
  
  # Figure of drugs per patient (mean, max, min)  - SCIRehab entire cohort
  output$plot_pharmacol_management_per_day_scirehab <- renderPlotly({ 
    if (input$pharmacol_management_scirehab == "full_cohort_scirehab"){ #full cohort selected
      
      # Create color list  
      color_list <- c("#FFA500", "#EE6677", "#228833", "#4477AA", "#4B0082")
      
      
      acute_pharmacol_management.data.per.ais.grade2 <-acute_pharmacol_management.data.per.ais.grade%>%
        dplyr::rename(Day=day)%>%
        dplyr::rename(Mean=mean, 
                      Min=min,
                      Max=max)%>%
        dplyr::mutate(as.numeric(Day, Mean))
      
      # Create plot  
      number.of.drug.perday.scirehab.plot <- ggplot(acute_pharmacol_management.data.per.ais.grade2, aes(x=Day, y= Mean, color = AIS))+
        geom_line(aes(x=Day, y=Mean, color=AIS), size=1)+
        geom_ribbon(aes(ymin=Min,ymax=Max,fill=AIS),color="grey",alpha=0.4) +  theme_bw(base_size = 12, base_family = "Open Sans") + xlim(1,60) +
        scale_fill_manual(values=color_list) + scale_color_manual(values=color_list) +
        facet_wrap(.~AIS, ncol = 1)+ theme(legend.position="none", 
                                           axis.text = element_text(color = 'black'), 
                                           axis.title = element_text(color = 'black'), 
                                           strip.text = element_text(color = 'black'))+
        xlab('Days post injury')+ylab("")
      number.of.drug.perday.scirehab.plot
      
      plotly::ggplotly(number.of.drug.perday.scirehab.plot, tooltip=c("x", "y", 'Min', "Max"),  height = 800, width=800)
    }
  }) # Close function
  
  # Table providing an overview of the drugs per day and indication - SCIRehab customized subgroups
  output$table_srgp_scirehab <- renderDataTable({
    if (input$pharmacol_management_scirehab == "sbgrps_scirehab"){ # customized subgroups
      
      nr.of.patients.per.drug.per.day.table <- acute_pharmacol_management.data.scirehab %>%
        dplyr::filter(Age == input$select_age_pharmacol_management_sbgrp_scirehab &
                        Sex == input$select_sex_pharmacol_management_sbgrp_scirehab & 
                        AIS == input$select_ais_pharmacol_management_sbgrp_scirehab &
                        NLI == input$select_nli_pharmacol_management_sbgrp_scirehab)%>%
        dplyr::filter(prevalence != 0)%>%
        dplyr::group_by(day, generic_name) %>%
        dplyr::select(generic_name, day)%>%
        dplyr::arrange(day)%>%
        dplyr::distinct()%>%
        ungroup()%>%
        dplyr::rename(Day=day)%>%
        dplyr::rename(`Drug name`=generic_name)%>%
        datatable(rownames = FALSE)
    }
  }) # Close function
  
  # Overview figure of pharmacological management - SCIRehab entire cohort
  output$plot_pharmacol_management_srgp_scirehab <- renderPlotly({ 
    if (input$pharmacol_management_scirehab == "sbgrps_scirehab"){ #full cohort selected
      
      nr.of.patients.per.drug.per.day <- acute_pharmacol_management.data.scirehab %>%
        dplyr::filter(Age == input$select_age_pharmacol_management_sbgrp_scirehab &
                        Sex == input$select_sex_pharmacol_management_sbgrp_scirehab & 
                        AIS == input$select_ais_pharmacol_management_sbgrp_scirehab &
                        NLI == input$select_nli_pharmacol_management_sbgrp_scirehab)%>%
        dplyr::filter(prevalence != 0)%>%
        dplyr::group_by(day, generic_name) %>%
        dplyr::select(generic_name, day)%>%
        dplyr::arrange(day)%>%
        dplyr::distinct()%>%
        ungroup()%>%
        dplyr::count(day)%>% 
        dplyr::group_by(day)
      nr.of.patients.per.drug.per.day
      
      
      acute_pharmacol_management.scirehab.plot<-nr.of.patients.per.drug.per.day%>%
        plotly::highlight_key(~day)%>%
        plotly::plot_ly(y = ~n,
                        x =  ~day)%>%
        plotly::add_bars(
          marker = list(color = 'rgb(96,92,168)'),
          width = ~0.9,
          text = ~paste("Days post injury:", day,
                        '</br></br>', "Number of drugs:", n
          ),
          hoverinfo = "text")%>%
        plotly::layout(xaxis = list(title = "Days post injury"),
                       yaxis = list(title = "Numbers of unique drugs"))%>%
        plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick")
      acute_pharmacol_management.scirehab.plot
      
    }
  }) # Close function
  
  output$plot_poly_ind_sygen <- renderPlot({
    input_sex <- unique(input$select_sex_poly_ind_sygen)[1]
    input_age <- c(unique(input$select_age_poly_ind_sygen)[1]:unique(input$select_age_poly_ind_sygen)[2])
    input_age_str <- as.character(input_age)
    input_age_str <- paste0(input_age_str, '.csv')
    #print(input_age_str)
    input_ais <- unique(input$select_ais_poly_ind_sygen)[1]
    input_nli <- unique(input$select_nli_poly_ind_sygen)[1]
    
    file_list <- list.files('data/pid_graphs')
    
    vec_filter <- c()
    if (input_sex != 'Unknown') {
      vec_filter <- append(vec_filter, input_sex)
    }
    if (input_ais != 'Unknown') {
      vec_filter <- append(vec_filter, input_ais)
    }
    if (input_nli != 'Unknown') {
      vec_filter <- append(vec_filter, input_nli)
    }
    
    
    if (length(vec_filter) == 1){
      sub_list <- file_list[grep(file_list , pattern = vec_filter[1])]
    } else if (length(vec_filter) == 2){
      sub_list <- file_list[intersect(grep(file_list , pattern = vec_filter[1]),
                                      grep(file_list , pattern = vec_filter[2]))]
    } else if (length(vec_filter) == 3){
      temp_list <- file_list[intersect(grep(file_list , pattern = vec_filter[1]),
                                       grep(file_list , pattern = vec_filter[2]))]
      sub_list <- temp_list[grep(temp_list, pattern = vec_filter[3])]
    } else if (length(vec_filter) == 0){
      sub_list <- file_list
    }
    
    sub_list_filtered <- c()
    for (name in sub_list){
      age_str <- substr(name, nchar(name)-5, nchar(name))
      if (age_str %in% input_age_str){
        sub_list_filtered <- append(sub_list_filtered, name)
      }
    }
    
    if (length(sub_list_filtered) == 0){
      plot <- plot_error_line()
    } else if (length(sub_list_filtered) == 1){
      plot <- fct_poly_ind_sygen(sub_list_filtered[1])
    } else if (length(sub_list_filtered) == 2){
      plot.1 <- fct_poly_ind_sygen(sub_list_filtered[1])
      plot.2 <- fct_poly_ind_sygen(sub_list_filtered[2])
      plot <- plot_grid(plot.1, plot.2, ncol=2)
    } else if (length(sub_list_filtered) == 3){
      plot.1 <- fct_poly_ind_sygen(sub_list_filtered[1])
      plot.2 <- fct_poly_ind_sygen(sub_list_filtered[2])
      plot.3 <- fct_poly_ind_sygen(sub_list_filtered[3])
      plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
    } else if (length(sub_list_filtered) > 3){
      sub_only4 <- sample(sub_list_filtered, 4)
      plot.1 <- fct_poly_ind_sygen(sub_only4[1])
      plot.2 <- fct_poly_ind_sygen(sub_only4[2])
      plot.3 <- fct_poly_ind_sygen(sub_only4[3])
      plot.4 <- fct_poly_ind_sygen(sub_only4[4])
      plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
    }
    
  })
  
  output$plot_drug_pattern_sygen <- renderPlot({
    
    input_sex <- unique(input$select_sex_drug_pattern_sygen)[1]
    input_age <- c(unique(input$select_age_drug_pattern_sygen)[1]:unique(input$select_age_drug_pattern_sygen)[2])
    input_age_str <- as.character(input_age)
    input_ais <- unique(input$select_ais_drug_pattern_sygen)[1]
    input_nli <- unique(input$select_nli_drug_pattern_sygen)[1]
    
    input_type <- unique(input$type_drug_sygen)[1]
    varnames <- c("ID", "Days_after_injury", "Sex", "Age", "AIS", "NLI")
    if (input_type == 'cat_drug_sygen'){
      varnames <- c(varnames, input$select_cat_drug_sygen)
      feat <- input$select_cat_drug_sygen
      data_modified <- df_heatmap_sygen_indication[names(df_heatmap_sygen_indication)[names(df_heatmap_sygen_indication) %in% varnames] ]
    } else if (input_type == 'spe_drug_sygen'){
      varnames <- c(varnames, input$select_spe_drug_sygen)
      feat <- input$select_spe_drug_sygen
      print(feat)
      data_modified <- df_heatmap_sygen_drugs_copy[names(df_heatmap_sygen_drugs_copy)[names(df_heatmap_sygen_drugs_copy) %in% varnames] ]
    } 
    
    if (!('Unknown' %in% input_sex)){data_modified <- data_modified[data_modified$Sex %in% input_sex, ]}
    if (!('Unknown' %in% input_age)){data_modified <- data_modified[data_modified$Age %in% input_age_str, ]}
    if (!('Unknown' %in% input_ais)){data_modified <- data_modified[data_modified$AIS %in% input_ais, ]}
    if (!('Unknown' %in% input_nli)){data_modified <- data_modified[data_modified$NLI_raw %in% input_nli, ]}
    
    input_day <- input$day_drug_sygen[1]
    input_day_vec <- c(0:input_day)
    input_day_str1 <- as.character(input_day_vec)
    input_day_str2 <- paste0('X',input_day_str1)
    
    print(names(data_modified))
    
    data_modified <- data_modified[data_modified$Days_after_injury %in% input_day_str2, ]
    
    names(data_modified) <- gsub(" ", ".", names(data_modified))
    feat <- gsub(" ", ".", feat)
    
    print(names(data_modified))
    print(feat)
    
    colors <- colorRampPalette(c("white", "#0000ff"))(7)
    plot <- ggplot(data = data_modified, aes(x = Days_after_injury, y = ID))+
      geom_tile(aes_string(fill = feat)) +
      #geom_tile(aes_string(fill = 'Pain')) +
      theme(axis.ticks.y=element_blank(),
            axis.text.y=element_blank()) +
      scale_fill_manual(name = 'Number of drugs', values=colors) + 
      scale_x_discrete(name ="Days after injury", labels=input_day_str1) +
      scale_y_discrete(name ="1 row = 1 patient") +
      labs(title = paste("Number of patients: ", length(levels(factor(data_modified$ID)))))
    
    plot
  })
  
  output$plot_drug_pattern_scirehab <- renderPlot({
    
    varnames <- c("ID", "Days_after_injury", "Sex", "Age", "AIS", "NLI", unique(input$select_drug_scirehab)[1])
    data_modified <- df_heatmap_scirehab[names(df_heatmap_scirehab)[names(df_heatmap_scirehab) %in% varnames] ]
    
    input_sex <- unique(input$select_sex_drug_pattern_scirehab)[1]
    input_age <- c(unique(input$select_age_drug_pattern_scirehab)[1]:unique(input$select_age_drug_pattern_scirehab)[2])
    input_age_str <- as.character(input_age)
    input_ais <- unique(input$select_ais_drug_pattern_scirehab)[1]
    input_nli <- unique(input$select_nli_drug_pattern_scirehab)[1]
    
    if (!('Unknown' %in% input_sex)){data_modified <- data_modified[data_modified$Sex %in% input_sex, ]}
    if (!('Unknown' %in% input_age)){data_modified <- data_modified[data_modified$Age %in% input_age_str, ]}
    if (!('Unknown' %in% input_ais)){data_modified <- data_modified[data_modified$AIS %in% input_ais, ]}
    if (!('Unknown' %in% input_nli)){data_modified <- data_modified[data_modified$NLI_raw %in% input_nli, ]}
    
    input_day <- input$day_drug_scirehab[1]
    input_day_vec <- c(0:input_day)
    input_day_str1 <- as.character(input_day_vec)
    input_day_str2 <- paste0('X',input_day_str1)
    
    data_modified <- data_modified[data_modified$Days_after_injury %in% input_day_str2, ]
    
    if (dim(data_modified)[1] == 0){
      plot <- plot_error_data()
    } else {
      colors <- colorRampPalette(c("white", "#0000ff"))(max(as.numeric(df_heatmap$Number_of_doses)))
      plot <- ggplot(data = data_modified, aes(x = Days_after_injury, y = ID))+
        geom_tile(aes_string(fill = input$select_drug_scirehab)) +
        theme(axis.ticks.y=element_blank(),
              axis.text.y=element_blank()) +
        scale_fill_manual(name = 'Drug prescribed? (0:"No")', values=colors) + 
        scale_x_discrete(name ="Days after injury", labels=input_day_str1) +
        scale_y_discrete(name ="1 row = 1 patient") +
        labs(title = paste("Number of patients: ", length(levels(factor(data_modified$ID)))))
    }
    
    plot
  })
  
  output$plot_polypharmacy_sygen <- renderPlot({ 
    input_day <- input$day_polypharmacy_sygen[1]
    #print(input_day)
    plot <- fct_poypharmacy_sygen(input_day, data_network_sygen, network_data_sygen, 'sygen')
    plot
  })
  
  output$plot_polypharmacy_scirehab <- renderPlot({
    input_day <- input$day_polypharmacy_scirehab[1]
    #print(input_day)
    plot <- fct_poypharmacy_sygen(input_day, data_network_scirehab, network_data_scirehab, 'scirehab')
    plot
  })
  
  #------- other ----------- 
  
  shinyjs::onclick("menu",
                   shinyjs::toggle(id = "sideFooter", anim = F))
  
  shiny:::flushReact()
  
}

# Run the application 
shinyApp(ui = ui, server = server)