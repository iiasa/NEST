library( rgdal )
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
## IMPORTANT
# To display new variables, go and add them to datasetInput manually

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ---
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Simple integer interval ----
      sliderInput("year", "Year:",
                  min = 1990, max = 2060,
                  value = c(2020,2050), step = 10),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c('River flow','Canal flow', 'Electricity transmission'
                  ) ),
      
      helpText("The grouping categories are also the dependencies of the selected dataset"),
      conditionalPanel(condition="input.tabselected==1",
                       # Input: Selector for choosing grouping dimension ----
                       selectInput(inputId = "grouping",
                                   label = "Choose a grouping dimension:",
                                   choices = c("scenario","year_all","time") )
      ),
      
      uiOutput("ui_fill_inputs"),
      
      selectInput(inputId = "time",
                  label = "Choose a grouping dimension:",
                  choices = time, multiple = T )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  id = "tabselected",
                  tabPanel("Plot", value = 1, plotlyOutput("plot"),
                           textOutput("explanation"),
                           radioButtons(inputId = 'png_pdf', label = 'Select the graphic output format:', choices = list('png','pdf')),
                           downloadButton("downloadPlot", "Download Figure")
                  ),
                  
                  tabPanel("Table",value=3, 
                           downloadButton("downloadData", "Download"),
                           tableOutput("table")
                  )
      )
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output,session) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           'River flow' = raw_extraction$river_flow,
           'Canal flow' = raw_extraction$canal_flow, 
           'Electricity transmission' = raw_extraction$trans_flow
    )
  })
  observe({
    
    updateSelectInput(session, "time",
                      label = "Choose a time:",
                      choices = (unique( (datasetInput())$time ) ),
                      selected = 'year')
    
    updateSelectInput(session, "scenario",
                      label = "Choose a scenario:",
                      choices = (unique( (datasetInput())$scenario ) ),
                      selected = head(unique( (datasetInput())$scenario ))[1])
  })
  
  build_inputs <- function(choices) {
    output = tagList()
    for(i in choices){
      output[[i]] = tagList()
      output[[i]][[1]] = selectInput(inputId = paste0(i),
                                     label = paste0('Choose a ',i),
                                     choices =  unique(datasetInput() %>% select_('i')) ,multiple = TRUE)
    }
    return(output)
  }

  output$ui_fill_inputs <- renderUI({
    group_chs <- (datasetInput() %>% names() %>% as.data.frame() %>% filter(!. %in% c('node','year_all','value')))$.
    build_inputs('scenario')

  })
  
  output$plot <- renderPlotly({
    
    ocean_poly = data.frame(long = c(65,65,71,71), lat = c(26,20,20,26),
                            order = c(1,2,3,4), hole = FALSE , piece = 1,
                            group = 0.1, id = 0 ) #trip out ocean with other countries, does't work well
    
    #no transparency of filling, yes very light colors
    theme_map_flow = theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank()) 
    country_colors = c(AFG = "#A2A7D2", CHN = "#F4AA91", IND ="#FAD4A1", PAK = "#C0DDAE")
    
    plot_data <<- datasetInput() %>% 
      filter(scenario %in% input$scenario) %>% 
      #     the if condition to chekc that column is always there, otherwise repeat the scenario choice
      # have not found smarter solution
      filter({if("time" %in% names(.)) time %in% input$time else scenario %in% input$scenario} ) %>% 
      filter(year_all >= min(input$year) & year_all <= max(input$year)) %>% # homogenize the names of year/year all
      mutate(year_all = as.numeric(year_all)) %>% 
      # group_by_('node',input$grouping) %>% 
      # summarize(value = sum(value)) %>% 
      as.data.frame()
    
    # plot1 <<- ggplot() +
    #   geom_polygon(data = basin.tidy, aes(long,lat, group=group),color='black',fill = "#f5f5f2",size = 0.1) +
    #   geom_polygon(data = basin.tidy2, aes(long,lat, group=group,fill=value),alpha=0.8,color='black',size = 0.1) +
    #   coord_map("azequalarea") +
    #   facet_wrap(as.formula(paste("~", input$grouping)))+
    #   scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
    #   theme_map()
    
    if (input$dataset == 'River flow'){
      river_plot = plot_data %>% 
        group_by_("tec","scenario","year_all" ,"node_in", "node_out","x.in","y.in","x.out","y.out",'time') %>% 
        summarise(value = sum(value)) 
      
    plot1 <<- ggplot()+
      geom_polygon(data = ocean_poly,aes(long,lat,group = group),colour = 'lightblue',fill = 'lightblue')+
      geom_polygon(data = extra_node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50')+ # write country name. make country color, 
      geom_polygon(data = node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50',size = 0.5)+
      geom_segment(data = river_plot ,aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),
                 color = 'deepskyblue3', lineend = c('butt') )+
      facet_wrap(as.formula(paste("~", input$grouping)))+
      ggtitle(paste0('Annual river flows. Scenario: ') )+
      coord_cartesian(xlim = c(66,82), ylim = c(24,37), expand = TRUE,
                      default = FALSE, clip = "on")+
      scale_size(range = c(0.6, 5))+
      scale_fill_manual(values=country_colors)+
      theme_bw() + theme_map_flow
    
    } else if(input$dataset == 'Canal flow'){
    # canal flows
      canal_plot = plot_data %>% 
        group_by_("tec",'type' ,"scenario","year_all" ,"node_in", "node_out","x.in","y.in","x.out","y.out",'time') %>% 
        summarise(value = sum(value)) 
  
    plot1 <<- ggplot()+
      geom_polygon(data = ocean_poly,aes(long,lat,group = group),colour = 'lightblue',fill = 'lightblue')+
      geom_polygon(data = extra_node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50', size = 0.5)+ # write country name. make country color, 
      geom_polygon(data = node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50', size = 0.5)+
      geom_segment(data = canal_plot ,aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value,colour = type),
                 arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),
                 lineend = c('butt') )+
      facet_wrap(as.formula(paste("~", input$grouping)))+
      ggtitle(paste0('Annual canal flows') )+
      # geom_curve(data = canal_test_plot %>% filter(grepl('lined',tec)),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),
      #            color = 'deepskyblue4', arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),
      #            lineend = c('butt'), linejoin = c('round'),curvature = 0.1 )+
      coord_cartesian(xlim = c(66,82), ylim = c(24,37), expand = TRUE,
                      default = FALSE, clip = "on")+
      scale_size(range = c(0.6, 2))+
      scale_fill_manual(values=country_colors)+
      scale_color_manual(values = c(conv = 'brown3',lined = 'deepskyblue4'))+
      theme_bw()+theme_map_flow
    
    } else if (input$dataset == 'Electricity transmission'){
    # electricity transmission plot
      
      trans_plot = plot_data %>% 
        group_by_("tec" ,"scenario","mode","year_all" ,"node_in", "node_out","x.in","y.in","x.out","y.out",'time') %>% 
        summarise(value = sum(value))

    plot1 <<- ggplot()+
      geom_polygon(data = ocean_poly,aes(long,lat,group = group),colour = 'lightblue',fill = 'lightblue')+
      geom_polygon(data = extra_node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50',size = 0.1)+ # write country name. make country color, 
      geom_polygon(data = node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50',size = 0.1)+
      geom_segment(data = trans_plot%>% filter( mode == 1),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),
                 color = 'brown2', arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),
                 lineend = c('butt'), linejoin = c('round'))+
      geom_segment(data = trans_plot %>% filter(mode == 2),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),color = 'brown2',
                 arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),
                 lineend = c('butt'), linejoin = c('round') )+
      coord_cartesian(xlim = c(66,82), ylim = c(24,37), expand = TRUE,
                      default = FALSE, clip = "on")+
      facet_wrap(as.formula(paste("~", input$grouping)))+
      ggtitle(paste0('Annual electricity transmission flows. Scenario: ') )+
      scale_size(range = c(0.6, 2))+
      scale_fill_manual(values=country_colors)+
      theme_bw()+theme_map_flow
    }
    
    ggplotly(plot1)
  })
  
  output$explanation <- renderText({
    'The grouping category will be shown in the legend. When selecting indexes DIFFERENT than the GROUPING category, the values will be SUMMED UP in the plot over the selected elements'
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$dataset, input$png_pdf, sep = ".")
    },
    content = function(file) {
      if(input$png_pdf == 'png')
        png(file)
      else 
        pdf(file)
      
      print(plot1)
      dev.off()
    }
  )
  
  
  # Show the first "n" observations ----
  output$table <- renderTable({
    
    name_col <- datasetInput() %>% names()
    
    table_data <<- datasetInput() %>% 
      filter(scenario %in% input$scenario) %>% 
      #     the if condition to chekc that column is always there, otherwise repeat the scenario choice
      # have not found smarter solution
      filter({if("commodity" %in% names(.)) commodity %in% input$commodity else scenario %in% input$scenario} ) %>% 
      filter({if("level" %in% names(.)) level %in% input$level else scenario %in% input$scenario} ) %>% 
      filter({if("time" %in% names(.)) time %in% input$time else scenario %in% input$scenario} ) %>% 
      filter({if("tec" %in% names(.)) tec %in% input$tec else scenario %in% input$scenario}) %>%
      filter({if("type" %in% names(.)) type %in% input$type else scenario %in% input$scenario}) %>%
      filter(year_all >= min(input$year) & year_all <= max(input$year)) # homogenize the names of year/year all
    
    table_data
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(table_data, file, row.names = FALSE)
    }
  )
  
  theme_map <<- function(...) {
    #    theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
  }
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)