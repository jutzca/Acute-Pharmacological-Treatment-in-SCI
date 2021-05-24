# -----------------------------------------------------------------------------------
# Building a Shiny app for visualisation of neurological outcomes in SCI
# Helper functions
# 
# July 15, 2020
# L. Bourguignon
# -----------------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------------

plot_error <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "Please choose 2 filters")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}

plot_error_data <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "No data fit all criteria")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}

plot_error_value <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "Please enter a valid value for patient's score, between 0 and 50")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}

plot_error_line <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "No patient fit all criteria in our current dataset")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}

#---------- fct_acute_pharmacol_management_sygen function ---------- 

fct_acute_pharmacol_management_sygen<- function(day, acute_pharmacol_management.data.sygen){
  
  nr.of.patients.per.drug.per.day <- acute_pharmacol_management.data.sygen %>%
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
  
  return(acute_pharmacol_management.plot)
  
}

fct_acute_pharmacol_management.data.scirehab<- function(day, acute_pharmacol_management.data.scirehab){
  
  nr.of.patients.per.drug.per.day <- acute_pharmacol_management.data.scirehab %>%
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
  
  return(acute_pharmacol_management.scirehab.plot)
  
}

# -----------------------------------------------------------------------------------

fct_poly_ind_sygen <- function(file){
  
  data1 <- read.csv(paste('data/pid_graphs/',file, sep=''), header=TRUE, sep=',')
  
  cols_to_change = c(5:368)    #change columns 4:368 to numerics class format
  for(j in cols_to_change){
    data1[, j][(data1[, j]>0)] <- 1
    class(data1[, j]) = "numeric"
  }
  
  # Sum up all lines with same drugs per patient
  datan<-plyr::ddply(data1, c('ID', 'generic.name', 'indication','Sex', 'Age', 'AIS', 'Cause', 'NLI', 'NLI_raw', 'YEARDOI','Time_wks', 'New_timeline'), function(x) colSums(x[,-c(1,2,3,4,66:382)], na.rm = TRUE))
  
  # Reformat data from wide to long
  data_long<-datan%>%
    gather(Day, daily_dose, X0:X60)
  
  data_long$Day<- sub("X","",data_long$Day)
  data_long$Day<- as.numeric(data_long$Day)
  
  
  ais.grade.plot <-unique(data_long$AIS)
  sex.plot <-unique(data_long$Sex)
  cause.plot <-unique(data_long$Cause)
  plegia.plot <-unique(data_long$NLI)
  nli.plot <-unique(data_long$NLI_raw)
  
  # colors <- colorRampPalette(c("white", "#bca0dc", "#b491c8", "#663a82", "#3c1361"))(8)
  colors <- colorRampPalette(c("white", "#0000ff"))(7)
  
  # Create plot  
  myplot1<- ggplot(data_long, aes(Day, generic.name, fill=as.factor(daily_dose)))+geom_tile(color = "white") +
    scale_fill_manual(values=colors)+theme_linedraw()+scale_x_continuous(expand = c(0, 0), breaks = c(0,15,30,45,60))+ 
    #ggtitle(paste(sex.plot,", ",ais.grade.plot,", ",plegia.plot," (",nli.plot,"), ",cause.plot, sep = ""))+ 
    labs(x="Days Post-Injury", fill = "Number of\n Doses")+ 
    theme(panel.grid.major = element_blank(),axis.title.x = element_text(size = 12),# family = 'Times'),
          #plot.title =  element_text(size = 14, family = 'Times', face='bold'),
          axis.text.x = element_text(color="black", size=10), # family = 'Times'), 
          axis.text.y = element_text( color="black", size=10)) # family = 'Times'))#, 
  #axis.title.y  = element_blank(), legend.key = element_rect(fill = "black", color = NA))
  return(myplot1)
  
}

fct_poypharmacy_sygen <- function(nb, data_network_sygen, network_data_sygen, name){

  code_day <- paste('X', as.character(nb), sep='')

  if (name == 'sygen'){
    nr.of.patients.per.drug.per.day.X7 <- data_network_sygen %>% subset(day==code_day)%>%
      as.data.frame() %>%
      select(-c("day", "dose"))
  } else if (name == 'scirehab'){
    nr.of.patients.per.drug.per.day.X7 <- data_network_sygen %>% subset(day==nb)%>%
      as.data.frame()%>%
      select(-c("day"))
    print(nr.of.patients.per.drug.per.day.X7)
  }
  print('test1')
  # 1. Node list
  
  # Create source
  source <- network_data_sygen %>% subset(day==code_day & value > 20 )%>% 
    distinct(Source) %>%
    dplyr::rename(label = Source)
  print(head(source))
  print('source ok')
  
  # Create target
  target <- network_data_sygen %>%subset(day==code_day & value > 20 )%>% 
    distinct(Target) %>%
    dplyr::rename(label = Target)
  print(head(target))
  print('target ok')

  
  # To create a single dataframe with a column with the unique locations we need to use a full join
  nodes <- full_join(source, target, by = "label")
  print(head(nodes))
  print('nodes ok')
  
  # To have unique IDs for each city, we add an ???id??? column
  nodes <- nodes %>% rowid_to_column("id")
  print(nodes)
  
  nodes2 <- merge(nodes, nr.of.patients.per.drug.per.day.X7, by.x = "label", by.y = "generic_name")
  print(nodes2)
  print('nodes2 ok')
  
  # 2. Edge list
  edge.data <- network_data_sygen %>%  subset(day==code_day & value > 20 )%>% 
    group_by(Source, Target) %>%
    dplyr::summarise(weight = value) %>% 
    ungroup()
  print(head(edge.data))
  print('edge.data ok')
  
  edges <- edge.data %>% 
    left_join(nodes, by = c("Source" = "label")) %>% 
    dplyr::rename(from = id) %>% 
    left_join(nodes, by = c("Target" = "label")) %>% 
    dplyr::rename(to = id) %>% 
    select(from, to, weight)
  print(head(edges))

  edges$weight.grp <- cut(edges$weight, c(-1,50,100,150),
                          labels=c("0-50", "51-100","l"))
  print(edges)

  # 3. Creating network objects
  set.seed(100)
  
  igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                      'randomly', 'fr', 'kk', 'drl', 'lgl')
  
  color_list <- c("#FFA500", "#EE6677", "#228833", "#4477AA", "#4B0082")
  
  nodes3 <- nodes2
  
  print(nodes)
  
  print('test2')
  
  if (name == 'sygen'){
    names(nodes3)[names(nodes3) == 'n.source'] <- 'degree'
    
    #int_test <-tbl_graph(nodes = nodes2, edges = edges, directed = FALSE)
    
    g <- tbl_graph(nodes = nodes3, edges = edges, directed = FALSE)%>%
      #mutate(degree = n.source)%>%
      ggraph(layout = "kk") +
      geom_edge_link2(aes(width = weight),
                      color='gray',
                      alpha = 0.8) +
      scale_edge_width(range = c(0.1, 2)) +
      geom_node_point(aes(size = degree),color='red') +
      geom_node_text(aes(#size = n.source, 
        label = label), repel = TRUE, 
        max.overlaps = getOption("ggrepel.max.overlaps", default = 100), 
        family = "Times"
      ) +  
      ggtitle(paste('Day', as.character(nb)))+
      theme_graph(title_size = 18,
                  title_face = "bold",
                  title_margin = 10)+
      theme(legend.text = element_text(family = "Times"),
            legend.title = element_text(family = "Times"),
            plot.title = element_text(family = "Times"))
  } else if (name == 'scirehab'){
    g <- tbl_graph(nodes = nodes3, edges = edges, directed = FALSE)%>%
      mutate(degree = n.source)%>%
      ggraph(layout = "kk") +
      geom_edge_link2(aes(width = weight),
                     color='gray',
                     alpha = 0.8) +
      scale_edge_width(range = c(0.1, 2)) +
      geom_node_point(aes(size = degree),color='red') +
      geom_node_text(aes(#size = n.source,
        label = label), repel = TRUE,
        max.overlaps = getOption("ggrepel.max.overlaps", default = 100),
        family = "Times"
      ) +
      ggtitle(paste('Day', as.character(nb)))+
      theme_graph(title_size = 18,
                  title_face = "bold",
                  title_margin = 10) +
      theme(legend.text = element_text(family = "Times"),
            legend.title = element_text(family = "Times"),
            plot.title = element_text(family = "Times"))
  }


  return(g)
}


