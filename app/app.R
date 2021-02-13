# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
# devtools::install_github("jienagu/noteMD")
library(noteMD)
library(knitr)
library(rmarkdown)
# Define UI for application that draws a histogram
ui <- fluidPage( # /-/ START fluidPage() ----

  # Application title
  titlePanel("Example noteMD Report Download App"),

  # Sidebar with a slider input for number of bins
  sidebarLayout( # /--/ START sidebarLayout() ----
     
    sidebarPanel( # /---/ START sidebarPanel() ----
                  
                  
            sliderInput(inputId = "bins", label = "Number of bins:", min = 1, 
                        max = 50, value = 30
                        ),

            # **describe_download outputId** ----------------------------------
            downloadButton(outputId = "describe_download", 
                           label = "Download Report", class = "butt"
                           ), 
      
                           br(),
      
      tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
      
      radioButtons(inputId = "format", 
                   label = "Document format", 
                   choices = c("PDF", "Word"),
                   inline = TRUE)
      
      ), # /---/ END sidebarPanel() ----

    # **Show a plot of the generated distribution** ----
    mainPanel( # /---/ START mainPanel() ----
       
      plotOutput(outputId = "distPlot"),
      
      fluidRow( # /----/ START fluidRow() ----
         
         # column with text 
         column(width = 12,
          helpText("Note: Any comments made in the box will be printed if you download the summary report.")
         ),
         
         # column with textbox 
         column(width = 12,
          tags$textarea("Please using any **markdown** syntax!", 
                        id = "markdowninput", 
                        rows  = 3, 
                        style = "width:100%;")
          )
         
      ), # /----/ END fluidRow() ----
      
      # **Add preview for markdown** ----
      helpText("Preview:"),
      htmlOutput("htmlmarkdown")
      
    ) # /---/ END mainPanel() ----
    
  ) # /--/ END sidebarLayout() ----
  
) # /-/ END fluidPage() ----


# **Define server logic required to draw a histogram** ----
server <- function(input, output) { # /-/ START server function ----
  
  # /--/ START renderPlot ----
  output$distPlot <- renderPlot({ 
    # **generate bins based on input$bins from ui.R** ----
    x <- faithful[ , 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  }) # /--/ END renderPlot ----

  # /--/ START reactive ----
  output$htmlmarkdown <- reactive({ 
    note_in_html(input$markdowninput)
  }) # /--/ END reactive ----

  
  output$describe_download <- downloadHandler(
     
    # **create file name** ----
    filename = function() {
      paste("Summary", Sys.Date(), switch(
        input$format, PDF = ".pdf", Word = ".docx"
      ), sep = "")
    },
    # **(PDF) create content** ----
    content = function(file) {
      if (input$format == "PDF") {
         
        # **(PDF) start of progressing indicator** ----
        withProgress(
          message = "Download in progress",
          detail = "This may take a while...",
          value = 0,
          {
            for (i in 1:15) {
              incProgress(1 / 15)
              Sys.sleep(0.01)
            }
            # **(PDF) end of progression** ----
            
            src <- normalizePath("summary_report.Rmd")
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            file.copy(src, "summary_report.Rmd", overwrite = TRUE)

            library(rmarkdown)
            out <- render("summary_report.Rmd", pdf_document())
            file.rename(out, file)
          }
        )
        # **end of pdf content** ----
      } else {
         # **(DOCX) create content** ----
         # **(DOCX) start of progressing indicator** ----
        withProgress(
          message = "Download in progress",
          detail = "This may take a while...",
          value = 0,
          {
            for (i in 1:15) {
               # incremental progress 
              incProgress(1 / 15)
               # set the Sys.sleep time
              Sys.sleep(0.01)
            }
            # (DOCX) end of progression ----
            
            src <- normalizePath("summary_report_word.Rmd")
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, "summary_report_word.Rmd", overwrite = TRUE)

            library(rmarkdown)
            out <- render("summary_report_word.Rmd", word_document())
            file.rename(out, file)
          }
        )
      }
    }
  )
} # /-/ END server function ----

# Run the application
shinyApp(ui = ui, server = server)
