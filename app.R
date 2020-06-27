library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(rdrop2)
library(shinyalert)
library(shinyBS)
library(DT)

# Uncomment to create and save dropbox token
# token <- drop_auth()
# saveRDS(token, "dropbox-token.rds")

token <- readRDS("dropbox-token.rds")
drop_acc(dtoken = token)
outputDir <- "COVID-CatalogShiny/responses"
outputDirBckup <- "COVID-CatalogShiny/responsesBackup"
resourceTypes <- c(
  "Computational",
  "Contact tracing apps",
  "Literature",
  "Databases (epidemiological)",
  "Databases (image)",
  "Databases (sequencing)",
  "Databases (proteomics)",
  "Databases (clinical)",
  "Databases (chemical structure)",
  "Databases (other)"
)

######################################################################################
# Define global #
######################################################################################
fieldsMandatory <- c("email_submit", "resource_name_submit", "resource_url_submit",
                     "resource_description_submit")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


appCSS <- ".mandatory_star { color: red; }
   #error { color: red; }"

formCSS <- ".notbold{
    font-weight:normal
}"

fieldsAll <-  c("email_submit", "resource_name_submit", "resource_url_submit", "resources_description_submit", "notes_submit")

responsesDir <- file.path("COVID-resources-Shiny/responses")
responesesBackup <- file.path("COVID-resources-Shiny/responsesBackup")
epochTime <- function() {
  as.integer(Sys.time())
}


######################################################################################
# Define UI #
######################################################################################

ui <- fluidPage(
  shinyjs::useShinyjs(), 
  shinyjs::inlineCSS(appCSS), 
  shinyjs::inlineCSS(formCSS), 
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "my.css")
  ),
  
    tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: lightyellow !important;}')),
  
  navbarPage(
    title = "", id="main_panel",
    theme = shinythemes::shinytheme("cosmo"),
    ##first the table
    tabPanel( value="catalog",
              p("COVID Informatics Catalog"),
              
              sidebarLayout(
                # Sidebar panel for inputs ----
                wellPanel(
                  fluidRow(
                    column( 3, 
                            uiOutput("countryValue"))
                  
                ))
                ,
                # Main panel for displaying outputs ----
                div(DT::dataTableOutput("mytable1"), style = "font-size: 75%")
                
              )
    ), 
    tabPanel( value="submit",
              p("Submit a new resource"),
              div(

              ),
              fluidRow( 
                #column ( 6,
                sidebarLayout(
                  sidebarPanel(
                    tags$p(HTML("Submit your proposed resource using the <a href='https://docs.google.com/forms/d/e/1FAIpQLScTi6SP2i6bkrBJukrChRtmtt-LNfFvVTaZjSBmbnc7yxjTDQ/viewform', target='_blank'>submission form</a>. We will review your submission before adding it to the catalog." )),
                    width = 12),
                  mainPanel()
                  
                  )),
              DT::dataTableOutput("responsesTable"),
              shinyjs::hidden(
                div(
                  id = "thankyou_msg",
                  h3("Thanks, your response was submitted successfully!"),
                  actionLink("submit_another", "Submit another response")
                )
              ), 
              shinyjs::hidden(
                span(id = "submit_msg", "Submitting..."),
                div(id = "error",
                    div(br(), tags$b("Error: "), span(id = "error_msg"))
                )
              )
              
    ),
    tabPanel(value="main",
             title = p("Suggest a modification on the current catalog data"),
             fluidRow( 
               #column ( 6,
               sidebarLayout(
                 sidebarPanel(
                   tags$p(HTML("The Shiny App of this on-line catalog is automatically generated based on  the CSV file <a href='https://github.com/Ru-bixcube/COVID-resources-Shiny/blob/master/csv/tableData.csv', target='_blank'>\"tableData.csv\"</a> available at the GitHub repo: <a href='https://github.com/Ru-bixcube/COVID-resources-Shiny', target='_blank'>COVID-resources-Shiny</a>" )),
                   tags$p(HTML( "To propose any correction, please:")),
                   p(
                     HTML("<ol>
                                <li>Fork the GitHub repo <a href='https://github.com/Ru-bixcube/COVID-resources-Shiny', target='_blank'>COVID-resources-Shiny</a></li>
                                <li>Propose the changes in the CSV file <a href='https://github.com/Ru-bixcube/COVID-resources-Shiny/blob/master/csv/tableData.csv', target='_blank'>\"tableData.csv\"</a></li>
                                <li>Submit a pull request</li>
                                </ol>")
                    ),
                   width = 12
                   ),
                 mainPanel()
                 
               ))
    ), 
    tabPanel(value="main",
             title = p("About"),
             fluidRow( 
               #column ( 6,
               sidebarLayout(
                 sidebarPanel(
                   h3( "Welcome to the COVID Informatics Catalog!" ),
                   br(),
                   tags$p(HTML( "The objective of this Shiny App is to provide a dynamic online informatics catalog for COVID-19 resources. We welcome the community to correct and complete it." ) ),
                   br(),
                   tags$h5(HTML("<b>Inclusion Criteria:</b>")),
                   p(
                     HTML("<ol>
                                <li>Restricted to datasets, software, and related informatics resources relevant to the COVID-19 pandemic</li>
                                <li>The resource must be open source and/or publicly accessible (free of charge) through a website or via a way of collaboration with investigators</li>
                                <li>All the resources (data and website) have to be available in English</li>
                                </ol>")
                     
                     
                     
                     
                     
                   ),
                   tags$h5(HTML("<u>All three criteria must be met.</u>")),
                   br(),
                   tags$p(HTML( "<b>The COVID Informatics Catalog contains:</b>
                                        <li>Resource name (long name and acronym if any)</li>
                                        <li>Resource URL</li>
                                        <li>Resource type</li>
                                        <li>Resource description</li>
                                        <li>Additional notes</li>" ) ),
                   br(),
                   
                   width = 12
                 ),
                 mainPanel()
                 
                 # )
               ))
    )
  )
)

######################################################################################
# Define server #
######################################################################################

server <- function(input, output, session) {
  
  attr(input, "readonly") <- FALSE
  dataValues <- reactiveValues()
  biobanks <- read.delim( "./csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)
  biobanks$Name <- paste0("<a href='",  biobanks$URL, "' target='_blank'>", biobanks$Name, "</a>")
  biobanks <- biobanks[, (names(biobanks) != 'URL')]

  observeEvent(input$confirm0, {
    
    
    if( input$dataset == ""){
      biobanks <- read.delim( "./csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
    }else{
      biobanks <- read.delim( "./csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
      
      output$mytable1 <- DT::renderDataTable(DT::datatable({
        data <- biobanks
        if (input$dataset %in% data$Name) {
          data <- data[data$Name == input$dataset,]
          colnames(data) <- gsub("_", " ", colnames(data))
          data
        }else{
          
          # Show a modal when the button is pressed
          shinyalert("Oops!", "There is not any biobank/datase with that name in the catalog", type = "error")
        }
        
        
      }))
      
    }
    
    
  })
  
  output$countryValue <- renderUI({
    selectInput(inputId = "resource_type", 
                label = "Resource Type:", 
                choices =  c("All", resourceTypes)
    )
    
  })
  
  output$mytable1 <- DT::renderDataTable(
    DT::datatable({
    data <- biobanks
    if (input$resource_type != "All") {
      data <- data[tolower(data$Resource.Type) == tolower(input$resource_type),]
    }

    colnames(data) <- c("Name","Resource Type","Description","Notes")
    data
    
  },  filter = "top", escape = FALSE, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 30), callback = JS("
var tips = ['Resource name (long name and acronym if any)', 'Web URL', 'Resource type (Data/Computational/Supporting)', 'About the resource',
'E.g. genomics or clinical studies (for data resources)','Additional notes',
'Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}
")
)
  )

  
  
  
  observeEvent(input$confirm1, {
    
    
    updateTabsetPanel(session, "main_panel",
                      selected = "submission")
    
  })
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  
  formData <- reactive({
    dataSubmission <- sapply(fieldsAll, function(x) input[[x]])
    dataSubmission <- c(dataSubmission, timestamp = epochTime())
    dataSubmission$status <- "Validation Pending"
    dataSubmission <- t(dataSubmission)
    dataSubmission
  })

  saveData <- function(dataSubmission) {
    dataSubmission <- t(dataSubmission)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(dataSubmission))
    dataSubmission <- gsub(",", ";", dataSubmission)
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(dataSubmission, filePath, quote = TRUE, row.names = FALSE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
    drop_upload(filePath, path = outputDirBckup)

  }
  
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })   
  
  
  
  
}


# Create Shiny app ----
shinyApp(ui, server)