#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinyjs)

list_choices <-  unique(msleep$vore)
list_choices <- list_choices[!is.na(list_choices)]
names(list_choices) <- paste0(list_choices,"vore")

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
                 tabPanel("msleep",
                          
                          fluidPage(
                              sidebarLayout(sidebarPanel(
                                  selectInput("select", label = h3("Plot by type of alimentation"), 
                                              choices = list_choices,
                                              selected = 1)
                              ), mainPanel(
                                  h3("Plots"), 
                                  plotOutput(outputId = "hello", click = "plot_click"),
                                  tableOutput("info")
                              )
                              ))),
                 
                 tabPanel("Random generator",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          selectInput("dist", label = h3("Select the distribution"), 
                                                      choices = list(Normal="rnorm", Uniform="runif", Exponential="rexp"),
                                                      selected = 1),
                                          sliderInput("n_sample", label = h3("Number of samples"), min = 10, 
                                                      max = 100, value = 50),
                                          actionButton("goButton", "Go!"),
                                          fluidRow(
                                            h3(style = "margin-left: 20px; margin-bottom: 0px;", "Number of bins"),
                                            column(2,
                                                   div(style = "margin-top: 37px", checkboxInput("auto_bins", label = "auto", value = TRUE))
                                            ),
                                            column(10,
                                                   sliderInput("n_bins", label="", min = 1, max = 50, value = 30)
                                            )
                                          ),
                                      
                                        downloadButton("report", "Generate report"),
                                        ),
                                        # sidebarPanel
                                        mainPanel(tabsetPanel(type = "tabs",
                                                              tabPanel("Plot", plotOutput("histPlot")),
                                                              tabPanel("Summary", verbatimTextOutput("histSummary")),
                                                              tabPanel("Table", tableOutput("histTable"))
                                        )
                                        ) # mainPanel
                          ) # sidebarLayout
                 ), #  tabPanel
                 
                 tabPanel("References",
                          p(tags$button(class="btn btn-default", 
                                        `data-toggle`="collapse", 
                                        `data-target`="#hola",
                                        "References")),
                          div(class="collapse", id="hola",
                              div(class="card card-body",
                                  includeMarkdown("references.md")
                              ))
                 ), #  titlePanel
                 useShinyjs()
) # navbarPage

col_scale <- scale_colour_discrete(limits = list_choices)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$hello <- renderPlot({
        ggplot(msleep %>% filter(vore == input$select)
               , aes(bodywt, sleep_total, colour = vore)) +
            scale_x_log10() +
            col_scale +
            geom_point()
    })
    
    output$info <- renderTable({
      nearPoints(msleep 
                 %>% filter(vore == input$select) 
                 %>% select(name, bodywt,  sleep_total, sleep_rem, sleep_cycle ), 
                 input$plot_click, threshold = 10, maxpoints = 1,
                 addDist = TRUE)
    })
    
    samples <- reactive({
      dist <- eval(parse(text=paste(input$dist)))
      dist(input$n_sample)
    })
    
    observe(if(input$auto_bins) disable("n_bins") else enable("n_bins"))
    
    output$histPlot <- renderPlot(
      hist(samples(), main="Random Generation", 
           breaks = if(!input$auto_bins) {input$n_bins} else {"Sturges"})
    )
    output$histSummary <- renderPrint(summary(samples()))
    output$histTable <- renderTable(samples())
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          n_sample = isolate(input$n_sample), 
          dist = isolate(input$dist), 
          breaks = if(!isolate(input$auto_bins)) {isolate(input$n_bins)} else {"Sturges"}
        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
