library(shiny)
library(tidyverse)
ornamental <- read_csv('data/ornamental_data.csv')

ui <-
  navbarPage(
    "Ornamental fish: what are the risks?",
    tabPanel("Explore taxonomic groups",
             fluidPage(
               # titlePanel("Explore taxonomic groups"),
               sidebarLayout(
                 sidebarPanel(
                   img(
                     src = "caw.png",
                     height = 100,
                     width = 400
                   ),
                   img(
                     src = "Colisa_lalia.jpg",
                     height = 250,
                     width = 400
                   ),
                   helpText("Select the taxonomic group, year and variable you want to explore"),
                   checkboxInput("logarithmicY", label = "Show response in log scale", TRUE),
                   selectInput(
                     "response",
                     label = "Variable",
                     choices = list("Quantity", "Frequency")
                   ),
                   selectInput(
                     "selects",
                     label = "Phylum",
                     choices = unique(ornamental$Phylum),
                     selected = "Chordata"
                   ),
                   selectInput(
                     "x_select",
                     label = "X-axis",
                     choices = names(ornamental)[7:9],
                     selected = "Order"
                   ),
                   sliderInput(
                     "year",
                     label = "Year",
                     min = 2015,
                     max = 2019,
                     value = c(2015, 2019),sep = ""
                   )
                 ),
                 
                 mainPanel(plotOutput(
                   "Plot", width = 'auto', height = "800px"
                 ))
               )
             )),
    tabPanel("Most abundant taxa",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 img(
                   src = "caw.png",
                   height = 100,
                   width = 400
                 ),
                 img(
                   src = "betta.jpg",
                   height = 250,
                   width = 400
                 ),
                 helpText("Explore the 20 nmost abundant taxa per taxonomic group"),
                 checkboxInput("logarithmicY2", label = "Show response in log scale", FALSE),
                 selectInput(
                   "response2",
                   label = "Variable",
                   choices = list("Quantity", "Frequency")
                 ),
                 selectInput(
                   "select_phylum",
                   label = "Phylum",
                   choices = unique(ornamental$Phylum),
                   selected = "Chordata"
                 )
               ),
               mainPanel(plotOutput(
                 "Plot2", width = 'auto', height = "800px"
               ))
             )))
  )


# Define server logic required to draw a histogram
server <-
  function(input, output) {
    output$Plot <- renderPlot({
      my_boxplot <-
        ornamental %>%
        filter(Phylum %in% input$selects) %>%
        filter(Year >= input$year[1] & Year <= input$year[2]) %>%
        # filter(Year %in% input$year)
        ggplot(aes(fill = Order)) +
        geom_boxplot(aes_string(y = input$response, x = input$x_select)) +
        coord_flip() +
        theme_bw(base_size = 18)
      
      if (input$logarithmicY)
        my_boxplot <- 
          my_boxplot + 
          scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                                                labels = scales::comma)
      
      return(my_boxplot)})
      
      ## Second page 
      output$Plot2 <- renderPlot({
        plot_top <-
          ornamental %>%
          gather(var, value, Quantity:Frequency) %>% 
          group_by(Taxa, var, Phylum, Order) %>% 
          summarise_at(vars(value), list(mean = mean, sd = sd, se = ~ sd / sqrt(n()))) %>%
          group_by(Phylum, var) %>%
          top_n(wt = mean, n = 20) %>% 
          filter(Phylum %in% input$select_phylum & var %in% input$response2) %>%
        
          ggplot(aes(x = fct_reorder(Taxa,.x =  mean), y = mean, fill = Order)) +
          geom_bar(position=position_dodge(), 
                   stat="identity",
                   colour="black") +      # Thinner lines
          geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                        size=.3,    # Thinner lines
                        width=.2,
                        position=position_dodge(.9)) +
          coord_flip() +
          theme_minimal(base_size = 18) +
          theme(axis.text.y = element_text(face = 'italic')) +
          # scale_fill_brewer(name = "Order", palette = 'Spectral') +
          labs(x = "Taxa", y = "")
        
        if (input$logarithmicY2)
          plot_top <- 
            plot_top +
            scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                          labels = scales::comma)
        
        
        return(plot_top)
      
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
