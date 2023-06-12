library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        .container-fluid {
          padding-left: 0px !important;
          padding-right: 0px !important;
        }
        .header-panel {
          background-color: orange;
          color: black;
          display: flex;
          justify-content: space-between; /* Align content at each end */
          align-items: center;
          margin: 0;
        }
        .header-title {
          font-family: 'Nunito'; /* Change the font family */
          font-size: 24px; /* Change the font size */
          font-weight: bold; /* Change the font weight */
          margin: 0 auto; /* Center the title */
          padding-left: 20px; /* Add some left padding for further right alignment */
        }
        .search-bar {
          display: flex;
          align-items: center;
          position: relative;
          
        }
        .search-input {
          padding-left: 30px !important; /* Adjust the left padding to accommodate the search icon */
        }
        .search-icon {
          position: absolute;
          left: 10px; /* Adjust the position of the search icon as needed */
          top: 50%;
          transform: translateY(-50%);
        }
        .slider-input {
          margin-left: 20px; /* Add some space between the search bar and the slider input */
          margin-right: 20px; /* Add some space between the search bar and the slider input */
          width: 200px; /* Adjust the width as per your preference */
        }
        .image-container {
          margin-right: 0px;
          display: flex;
          flex-wrap: wrap;
          justify-content: center;
        }
        .image-container img {
          width: 290px; /* Adjust the image width as per your preference */
          margin: 5px; /* Add margin between images */
        }
        body {
          margin: auto; /* Remove default body margin */
        }
        "
      )
    )
  ),
  div(
    class = "header-panel",
    div(
      class = "header-title",
      "Image Discovery Hub"
    ),
    div(
      class = "search-bar",
      div(
        class = "search-icon",
        icon("search")
      ),
      tags$style(".search-bar input[type='text'] { padding-left: 30px !important; }"),
      textInput("search_term", "", value = "", placeholder = "Search here")
    ),
    div(
      class = "slider-input",
      sliderInput("num_images", "Select Number of Images :", value = 1, min = 1, max = 300)
    ),
    # To adjust search button slightly towards left     
    actionButton("search_button", "Search"),
    div(
      style = "margin-left: 10px; margin-right: 10px;",  # Adjust the left and right margin as per your preference
    )
    
  ),
  
  div(
    class = "image-container",
    uiOutput("image_output")
  )
)

server <- function(input, output) {
  observeEvent(input$search_button, {
    search_term <- input$search_term
    num_images <- input$num_images
    
    if (!is.null(search_term) && num_images > 0) {
      # Get a list of all image files in the www folder with full file paths
      image_files <- list.files("www", pattern = ".jpg|jpeg", full.names = TRUE)
      
      # Filter the image files based on the search term
      filtered_files <- image_files[sapply(image_files, function(file) grepl(tolower(search_term), file, ignore.case = TRUE))]
      
      # Limit the number of images to display
      display_files <- head(filtered_files, num_images)
      
      if (length(display_files) > 0) {
        # Generate the HTML code to display the images
        image_tags <- lapply(display_files, function(file) {
          # Adjust the file path by removing the "www/" prefix
          relative_path <- sub("^www/", "", file)
          tags$img(src = relative_path, width = "100%", height = "350px")  # Remove the width property to allow responsive sizing
        })
        
        # Wrap the image tags in a div
        div_tags <- tags$div(image_tags)
        
        # Update the image output
        output$image_output <- renderUI(div_tags)
      } else {
        # Display "Image not found!!!" message
        output$image_output <- renderText(HTML("<span style='font-weight:bold; font-size: 50px;'>Image not found!!!</span>"))
      }
    } else {
      output$image_output <- renderUI(NULL)
    }
  })
}

shinyApp(ui, server)

