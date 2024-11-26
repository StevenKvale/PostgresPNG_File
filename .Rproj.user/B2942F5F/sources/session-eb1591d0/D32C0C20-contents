# App to demonstrate that png files stored in a remote Postgres DB
# won't display if published to the shinyapps.io server and the png file is > 1K
# in size.
#
# A postgres DB png file of any size will display if the app
# is run locally.
#
# The same remove DB is being accessed when run locally or published.
#
# The only difference is the Postgres driver being used in the config.yml file.
# Local opering system is Windows 11.
# Local driver - 'PostgreSQL Unicode(x64)'
# Shinyapps.io driver - 'PostgreSQL'

library(shiny)
library(DBI)
library(RPostgres)

# Database connection function
connect_to_db <- function() {
  conn_args <- config::get("dataconnection")
  DBI::dbConnect(odbc::odbc(),
                 Driver = conn_args$driver,
                 Server = conn_args$server,
                 UID    = conn_args$uid,
                 PWD    = conn_args$pwd,
                 Port   = conn_args$port,
                 Database = conn_args$database
  )
}

# Function to get PNG data from database
get_png_from_db <- function(con, image_id) {
  query <<-  paste0("SELECT png_file FROM png_table WHERE id = ",image_id)
  con <- connect_to_db()
  result <<- dbGetQuery(con, query)
  
  dbDisconnect(con)
  if (nrow(result) > 0) {
    return(result$png_file[[1]])
  } else {
    return(NULL)
  }
}

# UI Definition
ui <- fluidPage(
  titlePanel("Image Handling Demo"),
  
  sidebarLayout(
    sidebarPanel(
      # Add a selector for different static images
      selectInput("staticImage", "Choose Static Image:",
                  choices = c("kitten.png","logo.png", "banner.png")),
      
      # Add image size controls
      sliderInput("imageWidth", "Image Width (px):",
                  min = 100, max = 800, value = 400),
      # Choose image ID from DB
      numericInput("image_id", "Image ID from Postgres DB (1 Tiny file .939K, 2 Kitten file 473K)", 1, min = 1)
    ),
    
    mainPanel(
      # Section for static images
      h3("Static Image Example"),
      # Direct reference to static image (notice no 'www' in path)
      tags$div(
        style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 20px;",
        uiOutput("staticImageOutput")
      ),
      
      # Section for DB images
      h3("DB Image Example"),
      # Container for dynamic image
      tags$div(
        style = "border: 1px solid #ddd; padding: 10px;",
        imageOutput("dbImage")
      )
    )
  )
) # End UI Definition

# Server Definition
server <- function(input, output, session) {
  # Render static image from WWW filder with reactive width
  output$staticImageOutput <- renderUI({
    req(input$staticImage)
    tags$img(
      src = input$staticImage,
      width = input$imageWidth,
      style = "max-width: 100%;",
      alt = "Static demo image"
    )
  })
  
  # Handle database images
  output$dbImage <- renderImage({
    png_data <- get_png_from_db(con, input$image_id)
    if (!is.null(png_data)) {
      temp_file <- tempfile(fileext = ".png")
      
      local_temp_path <- tempfile(paste0("TempFileName",input$image_id), tmpdir = tempdir(), fileext =".png")
      writeBin(png_data, local_temp_path)
      list(src = local_temp_path,
           contentType = "image/png",
           width = input$imageWidth,
           alt = "Image from PostgreSQL")
    } else {
      tags$p("Image not found")
    }
  }, deleteFile = FALSE)
  
} # server

# Run the app
shinyApp(ui = ui, server = server)