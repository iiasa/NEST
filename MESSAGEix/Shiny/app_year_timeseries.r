library(dplyr)
library(ggplot2)
library(shiny)

## IMPORTANT
# To display new variables, go and add them to datasetInput manually

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  
  titlePanel("Yearly Timeseries"),
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
                  choices = c('Energy production by source','Final energy','Water widthrawals by source','Final water use',
                              'Water for crops', 'Crop land use',
                              'Total cost of technology','Energy for water','Water for energy','water for Irrigation',
                              'Activity','Capacity',"New installed capacity",'Input',"Investment cost",'Output',"Demand",'Emission'
                              ) ),
      
      helpText("The grouping categories are also the dependencies of the selected dataset"),
      conditionalPanel(condition="input.tabselected==1",
                       # Input: Selector for choosing grouping dimension ----
                       selectInput(inputId = "grouping",
                                   label = "Choose a grouping dimension:",
                                   choices = c("tec","commodity","node") )# it's random, will be automatically updated
                      ),
      
      conditionalPanel(condition="input.tabselected==1",
                       # Input: Selector for choosing grouping dimension ----
                       selectInput(inputId = "facet",
                                   label = "Choose a multiplot dimension (Optional):",
                                   choices = c("tec","commodity","node") )# it's random, will be automatically updated
                      ),
      
      # uiOutput("ui_selected"),
      uiOutput("ui_fill_inputs")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  id = "tabselected",
                  tabPanel("Plot", value = 1, 
                           radioButtons(inputId = 'plot_type', label = 'Select the type of plot:', choices = list('line','stack bar')),
                           plotOutput("plot"),
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
           'Energy production by source' = raw_extraction$energy_prod_by_source,
           'Final energy' = raw_extraction$final_energy_by_use,
           'Water widthrawals by source' = raw_extraction$water_by_source,
           'Final water use' = raw_extraction$final_water_by_use,
           'Water for crops' = raw_extraction$water_for_crops,
           'Crop land use' = raw_extraction$area_by_crop,
           'Total cost of technology' = raw_extraction$cost_by_technology,
           'Energy for water' = raw_extraction$energy_for_water,
           'Water for energy' = raw_extraction$water_for_energy,
           'water for Irrigation' = raw_extraction$water_for_irrigation,
           'Activity' = raw_extraction$ACT,
           'Capacity' = raw_extraction$CAP,
           "New installed capacity" = raw_extraction$CAP_NEW,
           'Input' = raw_extraction$input,
           "Investment cost" = raw_extraction$inv_cost,
           'Output' = raw_extraction$output,
           "Demand" = raw_extraction$demand_fixed,
           'Emission' = raw_extraction$EMISS
           )
  })

  observe({

    group_chs <- (datasetInput() %>% names() %>% as.data.frame() %>% filter(!. %in% c('time','year_all','value')))$. 
    updateSelectInput(session, "grouping",
                      label = "Choose a grouping dimension:",
                      choices = group_chs )
    
    updateSelectInput(session, "facet",
                      label = "Choose a multiplot dimension (Optional):",
                      choices = group_chs,
                      selected = 'scenario')
    
    updateSelectInput(session, "node",
                      label = "Choose a node:",
                      choices = (unique( (datasetInput())$node ) ),
                      selected = 'Indus')
    
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
    group_chs <<- (datasetInput() %>% names() %>% as.data.frame() %>% filter(!. %in% c('time','year_all','value')))$.
    build_inputs(as.character(group_chs))
    
  })
  
  output$plot <- renderPlot({
    
    plot_data <- datasetInput() %>% 
      filter(scenario %in% input$scenario) %>% 
      #     the if condition to chekc that column is always there, otherwise repeat the scenario choice
      # have not found smarter solution
      filter({if ('node' %in% names(.)) node %in% input$node else scenario %in% input$scenario} ) %>% 
      filter({if("commodity" %in% names(.)) commodity %in% input$commodity else scenario %in% input$scenario} ) %>% 
      filter({if("level" %in% names(.)) level %in% input$level else scenario %in% input$scenario} ) %>% 
      filter({if("time" %in% names(.)) time == 'year' else scenario %in% input$scenario} ) %>% 
      filter({if("tec" %in% names(.)) tec %in% input$tec else scenario %in% input$scenario}) %>%
      filter({if("type" %in% names(.)) type %in% input$type else scenario %in% input$scenario}) %>%
      filter(year_all >= min(input$year) & year_all <= max(input$year)) %>% # homogenize the names of year/year all
      mutate(year_all = as.numeric(year_all)) %>% 
      group_by_('year_all',input$grouping,input$facet) %>% 
      summarize(value = sum(value)) %>% 
      as.data.frame()
    
    if(input$plot_type == 'line'){ 
    plot1 <<- ggplot(plot_data)+
      geom_line(aes_string(x = 'year_all', y = 'value', color = input$grouping),size = 2)+
      facet_wrap(as.formula(paste("~", input$facet)))+
      theme_bw() + ylim(0,max(plot_data$value))+
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 12))
    } else{
      plot1 <<- ggplot(plot_data)+
        geom_bar(aes_string(x = 'year_all', y = 'value', fill = input$grouping), stat = "identity", position = "stack")+ 
        facet_wrap(as.formula(paste("~", input$facet)))+
        theme_bw() +
        theme(legend.position = 'bottom',
              axis.text = element_text(size = 12))
    }
    plot1
  })
  
  output$explanation <- renderText({
    'The grouping category will be shown in the legend. When selecting indexes DIFFERENT than the GROUPING or MULTIPLOT category, the values will be SUMMED UP in the plot over the selected elements'
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
      filter({if ('node' %in% names(.)) node %in% input$node else scenario %in% input$scenario} ) %>% 
      filter({if("commodity" %in% names(.)) commodity %in% input$commodity else scenario %in% input$scenario} ) %>% 
      filter({if("level" %in% names(.)) level %in% input$level else scenario %in% input$scenario} ) %>% 
      filter({if("time" %in% names(.)) time == 'year' else scenario %in% input$scenario} ) %>% 
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
  

  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
