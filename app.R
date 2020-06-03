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

######################drop################################################################
# Define global #
######################################################################################
fieldsMandatory <- c("email_submit", "resource_name_submit", "resource_url_submit",
                     "resource_description_submit", "resource_nih_funded_submit")

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

fieldsAll <-  c("email_submit", "resource_name_submit", "resource_url_submit",
                "resources_description_submit", "resource_data_type_submit",
                "resource_nih_funded_submit", "notes_submit")

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
              p("COVID Informatics Resources Catalog"),
              
              sidebarLayout(
                # Sidebar panel for inputs ----
                wellPanel(
                  fluidRow(
                    column( 3, 
                            uiOutput("countryValue")),
                    column( 3, 
                            uiOutput("radioButtonsDiseaseType")),
                    column( 3, 
                            uiOutput("radioButtonsGenomicType")),
                    column( 3,
                            uiOutput("patientValue"))
                  
                ))
                ,
                # Main panel for displaying outputs ----
                div(DT::dataTableOutput("mytable1", width = 1700), style = "font-size: 75%")
                
              )
    ), 
    tabPanel( value="submit",
              p("Submit a new dataset"),
              div(
                id = "form",
                textInput("email_submit", labelMandatory(HTML("<b>Contributor e-mail</b>  <br/>  <span class='notbold'>(e.g., myemail@...)</span>")), ""),
                textInput("resource_name_submit", labelMandatory(HTML("<b>Resource name</b>  <br/>  <span class='notbold'>(e.g., ClinicalTrials.gov COVID-19 related studies)</span>")), ""),
                textInput("resource_url_submit", labelMandatory(HTML("<b>Resource URL</b>  <br/>  <span class='notbold'>(e.g., https://clinicaltrials.gov/ct2/results?cond=COVID-19)</span>")), ""),
                textInput("resource_description_submit", labelMandatory(HTML("<b>Resource description</b>  <br/>  <span class='notbold'>(e.g., NLM curated literature hub for COVID-19)</span>")), ""),
                textInput("resource_data_type_submit", HTML("<b>Resource data type (if dataset)</b>  <br/>  <span class='notbold'>(e.g., case studies, dashboards and visualization tools)</span>"), ""),
                textInput("resource_nih_funded_submit", labelMandatory(HTML("<b>NIH Funded?</b>  <br/>  <span class='notbold'>(yes/no)</span>")), ""),
                textInput("notes_submit", HTML("<b>Additional notes</b>  <br/>  <span class='notbold'>(additional information)</span>"), ""),
                
                actionButton("submit", "Submit", class = "btn-primary")
                
              ),
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
                   br(),
                   h3( "Thank you for your contribution to update and improve the COVID Informatics Resources Catalog!" ),
                   br(),
                   tags$p(HTML( "Your proposed changes will be reivewed in the following days")),
                   br(),
                   width = 12
                   ),
                 mainPanel(img(src = 'logo.png', align = "center", height="30px"))
                 
               ))
    ), 
    tabPanel(value="main",
             title = p("About"),
             fluidRow( 
               #column ( 6,
               sidebarLayout(
                 sidebarPanel(
                   h3( "Welcome to the COVID Informatics Resources Catalog Shiny App!" ),
                   br(),
                   tags$p(HTML( "The objective of this Shiny App is to provide a dynamic online dataset catalog. We welcome the community to correct and complete it." ) ),
                   tags$h5(HTML("<u>Inclusion Criteria</u>")),
                   p(
                     HTML("<ol>
                                <li>Over five hundred (500) human subjects</li>
                                <li>Contain both genotype and phenotype data of the same subjects</li>
                                <li>Include Whole Genome Sequencing (WGS) or Whole Exome Sequencing (WES) data as part of their genomic data content</li>
                                <li>Include at least one hundred (100) recorded phenotypic variables per subject</li>
                                <li>The dataset has to be accessible through a website or via a way of collaboration with investigators</li>
                                <li>All the resources (data and website) have to be available in English</li>
                                </ol>")
                     
                     
                     
                     
                     
                   ),
                   br(),
                   tags$h5(HTML("<u>All five criteria must be meet</u>")),
                   
                   tags$p(HTML( "The COVID Informatics Resources Catalog contains:
                                        <li>Dataset name (long name and acronym if any)</li>
                                        <li>Country (where does the research take place)</li>
                                        <li>Subject count with both genomic and clinical data</li>
                                        <li>Study design (e.g., cohort, prospective, longitudinal)</li>
                                        <li>Number of phenotypic variables per patient</li>
                                        <li>Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)</li>
                                        <li>Sample size (total number of genomic samples [e.g., # of WGS samples + # of WES samples])</li>
                                        <li>Molecular data type (e.g., SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- )</li>
                                        <li>Genomic Markerset (e.g., genotyping microarrays or on a technology basis)</li>
                                        <li>Disease/Focus (e.g., general or disease specific)</li>
                                        <li>Patients age in years</li>
                                        <li>Ancestry</li>
                                        <li>Consent groups present in the dataset (e.g., biomedical, disease-specific)</li>
                                        <li>Accession link to the dataset (link to the website or contact information to obtain data access)</li>
                                        <li>Link to clinical/genomic study</li>
                                        <li>Link to genomic study if different than the clinical one</li>
                                        <li>Pubmed identifier number to key study infrastructure publication</li>" ) ),
                   br(),
                   
                   width = 12
                 ),
                 mainPanel(img(src = 'logo.png', align = "center", height="30px")), 
                 
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
  biobanks <- read.delim( "https://raw.githubusercontent.com/Ru-bixcube/COVID-resources-Shiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

  observeEvent(input$confirm0, {
    
    
    if( input$dataset == ""){
      biobanks <- read.delim( "https://raw.githubusercontent.com/Ru-bixcube/COVID-resources-Shiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
    }else{
      biobanks <- read.delim( "https://raw.githubusercontent.com/Ru-bixcube/COVID-resources-Shiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

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
    selectInput(inputId = "studydesign", 
                label = "Study Design:", 
                choices =  c("All", unique(tolower(biobanks$Study.Design)))
    )
    
  })
  
  output$patientValue <-
    renderUI({
      sliderInput("patientValue", "Subject count with genomic and clinical data:",
                  min = min(biobanks$Subject, na.rm = TRUE),
                  max = max(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE),
                  value = c(min(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE), max = max(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE))
      )
    })
  
  
  output$radioButtonsDiseaseType <- renderUI({
    radioButtons("diseasetype", "Disease/Focus:",
                 c("All" = "all",
                   "General" = "general", 
                   "Disease specific" = "specific"
                 )
    )
    
  })
  
  output$radioButtonsGenomicType <- renderUI({
    radioButtons("genomictype", "Molecular Data Type:",
                 c("All" = "all",
                   "WGS" = "wgs", 
                   "WES" = "wes"
                 )
    )
    
  })
  
  output$mytable1 <- DT::renderDataTable(
    DT::datatable({
    data <- biobanks
    if (input$diseasetype != "all") {
      if (input$diseasetype == "general") {
        data <- data[data$Disease.Focus == "General" & !is.na(data$Disease.Focus),]
      }
      if (input$diseasetype != "general") {
        data <- data[data$Disease.Focus != "General"  & !is.na(data$Disease.Focus),]
      }
    }
    if (input$genomictype != "all") {
      if (input$genomictype == "wgs") {
        data <- data[ grep( "WGS", data$Molecular.Data.Type), ]
      }
      if (input$genomictype == "wes") {
        data <- data[ grep( "WES", data$Molecular.Data.Type), ]
      }
    }
    if (input$studydesign != "All") {
      data <- data[tolower(data$Study.Design) == tolower(input$studydesign),]
    }
    if( ! is.null(input$patientValue) ){
      data <- data[ as.numeric(as.character(data$Subject.Count)) >= input$patientValue[1] &
                      as.numeric(as.character(data$Subject.Count)) <= input$patientValue[2] , ]
    }

    colnames(data) <- c("Name","Country", "Subject Count with Genomic and Clinical Data","Study Design","Disease/Focus","Number Of Phenotypic Variables Per Patient",
                         "Phenotypic Data Type","Sample Size","Molecular Data Type","Genomic Markerset",
                         "Patients Age (yrs)","Ancestry","Consent","Accession Link to the Dataset","Link to Clinical And Genomic Study", "Link to Genomic Study (if different than the clinical)","PubMed ID","Notes")
    data
    
  },  filter = "top", escape = FALSE, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 30), callback = JS("
var tips = ['Dataset name (long name and acronym if any)', 'Country (where does the research take place)', 'Subject count with both genomic and clinical data',
'Study design (e.g., cohort, prospective, longitudinal)','Disease/Focus (e.g., general or disease specific)','Number Of Phenotypic Variables Per Patient',
'Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)',
'Sample size (total number of genomic samples [e.g., # of WGS samples + # of WES samples])','Molecular data type (e.g., SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- )','Genomic Markerset (e.g., genotyping microarrays or on a technology basis)',
'Patients Age in years','Ancestry','Consent groups present in the dataset (e.g., biomedical, disease-specific)','Accession link to the dataset (link to the website or contact information to obtain data access)', 
'Link to clinical/genomic study','Link to genomic study if different than the clinical one','Pubmed identifier number to the key study infrastructure publication/s','Notes'],
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