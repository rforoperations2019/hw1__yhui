library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
#load csv data
park <- read.csv("C:/Users/user/Desktop/HW1_Rshiny/hw1__yhui/homework1_park/park.csv")


# Define UI for application -----------

ui <- fluidPage(
  
  
  
  # Application title -----------------------------------------------
  
  titlePanel("Pittsburgh Parks"),
  
  
  
  # Sidebar layout with a input and output definitions --------------
  
  sidebarLayout(
    
    
    
    # Inputs: ------------------------
    
    sidebarPanel(
      
      
      
      # Select variable for y-axis in scatterplot----------------------------------
      
      selectInput(inputId = "y", 
                  
                  label = "Y-axis:",
                  
                  choices = c("Acreage" = "acreage", 
                              
                              "Square foot" = "sqft", 
                              
                              "Shape area" = "Shape__Area", 
                              
                              "Shape length" = "Shape__Length"), 
                  
                  selected = "Shape__Area"),
      
      
      
      # Select variable for x-axis in scatterplot----------------------------------
      
      selectInput(inputId = "x", 
                  
                  label = "X-axis:",
                  
                  choices = c("Acreage" = "acreage", 
                              
                              "Square foot" = "sqft", 
                              
                              "Shape area" = "Shape__Area", 
                              
                              "Shape length" = "Shape__Length"), 
                  
                  selected = "Shape__Length"),
      
      
      
      # Select variable for color of points in scatterplot -----------------------------------
      
      selectInput(inputId = "z", 
                  
                  label = "Color by:",
                  
                  choices = c("Type" = "type_", 
                              
                              "Sector" = "sector", 
                              
                              "Divname" = "divname"),
                  
                  selected = "sector"),
      
      
      
      # Set alpha level and point size for scatterplot ---------------------------------------------
      
      sliderInput(inputId = "alpha", 
                  
                  label = "Alpha:", 
                  
                  min = 0, max = 1, 
                  
                  value = 0.5),
      
      numericInput(inputId="size",
                   label="point size in scatterplot",
                   value=3,
                   min=1,max=5,step=0.1),
      #input color of barchart
      textInput(inputId="bar_color",label="input color for the barchart",value="blue"),
      
      
      
      # whether or not show data table ---------------------------------------------
      
      checkboxInput(inputId = "show_data",
                    
                    label = "Show data table",
                    
                    value = TRUE),
      
      
      #select the divname for plotting corresponding piechart and barchart
      
      selectInput(inputId="selected_division",
                  label="select divname for piechart and barchart:",
                  choices=c("Northern",
                            "Northeast",
                            "Schenley",
                            "Eastern",
                            "Southern",
                            "Western",
                            "State"),
                  selected="State"),
      
      #Enter the number of entries to be shown in data table
      
      numericInput(inputId = "n_samp", 
                   
                   label = "A random selection of n parks of this division shown in the table:", 
                   
                   min = 1, max = nrow(park), 
                   
                   value = 20)
      
      
      
    ),
    
    
    
    
    # Output --------------------------------------------------------
    
    mainPanel(
      
      
      
      
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId="barchart"),
      plotOutput(outputId = "piechart"),
      uiOutput(outputId="number"),
      
      DT::dataTableOutput(outputId = "parkstable"),
      
      #show download button to download data subset
      
      downloadButton(outputId="download_subset",label="Download the dataset recording the parks 
                               in the selected divname")
      
    )
    
  )
  
)



# Define server function 

server <- function(input, output,session) {
  
  park_subset<-reactive({
    req(input$selected_division) 
    filter(park,divname %in% input$selected_division)
  })
  
  # observer to update maximum possible number of entries in data table
  observe({
    
    updateNumericInput(session, 
                       
                       inputId = "n_samp",
                       
                       value = min(5, nrow(park_subset())),
                       
                       max = nrow(park_subset())
                       
    )
    
  })
  
  park_sample<-reactive({
    req(input$n_samp)
    sample_n(park_subset(),input$n_samp)
  })
  
  # Create scatterplot object 
  output$scatterplot <- renderPlot({
    
    ggplot(data = park, aes_string(x = input$x, y = input$y,
                                   
                                   color = input$z)) +
      
      geom_point(alpha = input$alpha, size=input$size) +
      
      labs(title="General scatterplot of all parks",
           x = toTitleCase(str_replace_all(input$x, "_", " ")),
           
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           
           color = toTitleCase(str_replace_all(input$z, "_", " ")))
    
  })
  
  
  #draw piechart
  
  output$piechart <- renderPlot({
    types<-park_subset()$type_
    counts<-table(types)
    pct<-round(counts/sum(counts),2)*100
    label<-paste(levels(types),pct,"%")
    pie_title<-paste("Park distribution by type in",input$selected_division)
    pie(counts,cex=0.5,main=pie_title,label=label)
  })
  #draw barchart  
  output$barchart <- renderPlot({
    types<-park_subset()$type_
    counts<-table(types)
    bar_title<-paste("Park frequency by type in",
                     input$selected_division)
    barplot(counts,main=bar_title,col=input$bar_color)
  })
  
  #show number of each type of park in the selected divname 
  output$number<-renderUI({
    types<-park_subset()$type_
    counts<-table(types)
    HTML(paste("There are",counts,levels(types),
               "parks in this division.<br>"))
  })
  
  # Print data table if checked -------------------------------------
  
  output$parkstable <- DT::renderDataTable(
    
    if(input$show_data){
      
      DT::datatable(data = park_sample()[ ,c(2:7,9,11,12,19,20)], 
                    
                    options = list(pageLength = 10), 
                    
                    rownames = FALSE)
      
    }
    
  )
  
  output$download_subset <- downloadHandler(
    
    filename = function() {
      paste(input$selected_division, ".csv")
    },
    content = function(file) {
      write.csv(park_subset(), file, row.names = FALSE)
    }
  )
  
}



# Run the application -----------------------------------------------

shinyApp(ui = ui, server = server)

