# Shiny app for viewing image data from URLs
# and updating associated values in a table

library(shiny) 
library(DT)

# example data file with image urls
# originally from inaturalist package
image_data <- read.csv("images.csv", stringsAsFactors = FALSE)
image_data <- image_data[,c("image_url", "id_please")]
names(image_data)[2] <- "keep"

#######################################################
### INTERFACE
#######################################################

in4 <- uiOutput("load")

in1 <-   radioButtons("truefalse", "Change values in 'keep' column here", 
                      choices=c("TRUE", "FALSE"),
                      selected = "TRUE", inline = FALSE)
in2 <-   actionButton("goButton", "Update Table")
in3 <-   downloadButton("download_data", label = "Download")

# # sidebar with buttons
# side1 <- sidebarPanel(in1, in2, in3)

out1 <- htmlOutput("image1")
out2 <- dataTableOutput("image_data", width = "30%")

# main panel with image and datatable
tab1 <- tabPanel("table", 
                 h3("Click on a row to see image"))

in5 <- textInput("save_file", "New file name:", value="updated_data.csv")
in6 <- actionButton("save", "Save updated data")

ui <- fluidPage(titlePanel("Image Viewer"),
                column(6, out2),
                column(6, in1, in2, in5, in6, br(),br(), out1))


#######################################################
### SERVER 
#######################################################
server <- function(input, output){

  output$load <- renderUI({
    choices <- list.files(pattern="*.csv")
    selectInput("input_file", "Select input file", choices)
  })
  
  # reactive dataframe object
  df <- eventReactive(input$goButton, {
     if(selected_row_id()>0 &&input$goButton>0){  
      image_data[selected_row_id(),"keep"] <<- input$truefalse
     }
    image_data
  }, ignoreNULL = FALSE)
  
  # selectable data table 
  output$image_data <- renderDataTable({
    df()
  }, selection = "single")

 
  # selected row ID from data table
  selected_row_id <- reactive({
    input$image_data_rows_selected
  })
  
  # reactive image url object
  image_url <- reactive({
    df[selected_row_id(), "image_url"]
    })
  
  # html string of image
  output$image1<-renderText({c('<img src="',image_data[input$image_data_rows_selected, "image_url"],'">')})
  
  # Save input$bins when click the button
  observeEvent(input$save, {
    write.csv(image_data, input$save_file, row.names = FALSE)
  })
  
  # this is a new line
  # download button
  output$download_data <- downloadHandler(
    filename = "updated_data.csv",
    content = function(file) {
      write.csv(df(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)
