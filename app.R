## BiomaRt shiny application
##
## Created by Nisitha Jayatilleke
## Date: 30/05/2019
## Last updated: 30/05/2019

# Check for package installations
Bioconductor_list <- c("biomaRt", "mygene")
for(i in 1:length(Bioconductor_list)){
  if(!requireNamespace(Bioconductor_list[i], quietly = TRUE)){
    BiocManager::install(Bioconductor_list[i], update = FALSE)
  }
}

# Load libraries
library(shiny) #CRAN
library(DT) #CRAN
library(biomaRt) #Bioconductor
library(shinythemes) #CRAN
library(markdown) #CRAN
library(shinyjs) #CRAN
library(R.utils) #CRAN
library(knitr) #CRAN
library(mygene) #Bioconductor

# Knit markdown documents
knit("Instructions.Rmd", output = "Instructions.md")
knit("About.Rmd", output = "About.md")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Use shinyjs
  shinyjs::useShinyjs(),
  
  # Set shiny theme
  theme = shinytheme("cerulean"),
  
  # Set navbar 
  navbarPage(id = "tabs", title = "BiomaRt",
    tabPanel("Instructions",
      includeMarkdown("Instructions.md")
    ),
    tabPanel("biomaRt annotate",
      fluidRow(
        column(12, radioButtons(inputId = "mirrorChoice", label = h4("Select biomaRt mirror:"), choices = list("UK" = "UK", "Asia" = "Asia", "US West" = "US West", "US East" = "US East"), selected = "Asia"))
      ),
      fluidRow(
        column(12, radioButtons(inputId = "genomeChoice", label = h4("Select genome to annotate against:"), choices = list("Human" = "human", "Mouse" = "mouse")))
      ),
      fluidRow(
        column(12, textOutput("martLoaded"), tags$head(tags$style("#martLoaded{color: green; font-size: 20px; font-style: bold}")))
      ),
      fluidRow(
        column(12, textOutput("martError"), tags$head(tags$style("#martError{color: red; font-size: 20px; font-style: bold}")))
      ),
      fluidRow(
        column(12, uiOutput("geneFile"))
      ),
      fluidRow(
        column(6, uiOutput("listInput")),
        column(6, uiOutput("listType"))
      ),
      fluidRow(
        column(6, uiOutput("attributeCategory")),
        column(6, uiOutput("attributePage"))
      ),
      fluidRow(
        column(6, uiOutput("geneSums"))
      ),
      fluidRow(
        column(12,
          sidebarLayout(position = "left",
            mainPanel(
              fluidRow(
                column(12, h4(textOutput("attributeTableTitle")))
              ),
              fluidRow(
                column(12, dataTableOutput(outputId = "attributeTable"))
              )
            ),
            sidebarPanel(
              fluidRow(
                column(12, h4(textOutput("selectedAttributesTitle")))
              ),
              fluidRow(
                column(12, textOutput("selectedAttributes"))
              ),
              br(),
              fluidRow(
                column(12, actionButton(inputId = "clearAttributes", label = "Clear All"))
              )
            )
          )
        )
      ),
      fluidRow(
        column(6, actionButton(inputId = "submit", label = "Submit")),
        column(6, textOutput("queryError"), tags$head(tags$style("#martError{color: red; font-size: 20px; font-style: bold}")))
      ),
      br(),
      fluidRow(
        column(12, textOutput("biomartMessage"))
      ),
      br(),
      fluidRow(
        column(12, h4(textOutput("finalTableTitle")))
      ),
      fluidRow(
        column(12, dataTableOutput(outputId = "finalTable"))
      ),
      br(),
      fluidRow(
        column(12, downloadButton(outputId = "downloadData", label = "Download"))
      )
    ),
    navbarMenu("More",
      tabPanel("About",
        includeMarkdown("About.md")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Disable submission
  shinyjs::disable("submit")
  shinyjs::disable("downloadData")
  
  # Reactive values
  downloadExists <- reactiveValues(active = NULL)
  
  # Set global variables
  mart <- NULL
  attributeFullListIDs <- NULL
  attributeFullListDesc <- NULL
  
  # Set mart variable
  observeEvent(c(input$tabs, input$genomeChoice, input$mirrorChoice),{
    shinyjs::disable("mirrorChoice")
    shinyjs::disable("genomeChoice")
    shinyjs::disable("geneFile2")
    shinyjs::disable("submit")
    attributeFullListIDs <<- NULL
    attributeFullListDesc <<- NULL
    downloadExists$active <- NULL
    output$finalTableTitle <- NULL
    output$finalTable <- NULL
    host <- input$mirrorChoice
    if(host == "UK"){
      host <- "www.ensembl.org"
    } else if(host == "Asia"){
      host <- "asia.ensembl.org"
    } else if(host == "US West"){
      host <- "uswest.ensembl.org"
    } else if(host == "US East"){
      host <- "useast.ensembl.org"
    }
    if(input$genomeChoice == "human"){
      tryCatch(expr = {
        withTimeout(expr = {
          mart <<- useMart("ensembl", dataset = "hsapiens_gene_ensembl", host = host)
          output$martLoaded <- renderText(expr = paste(paste(input$mirrorChoice, "mirror", sep = " "), "Human mart loaded!", sep = ": "))
          output$martError <- NULL
          shinyjs::enable("mirrorChoice")
          shinyjs::enable("genomeChoice")
        }, timeout = 20, onTimeout = "error", substitute = FALSE)
      },
      TimeoutException = function(e){
        mart <<- NULL
        output$martLoaded <- NULL
        output$martError <- renderText(expr = "Mart loading failed! Try another mirror!")
        shinyjs::enable("mirrorChoice")
        shinyjs::enable("genomeChoice")
      },
      error = function(e){
        mart <<- NULL
        output$martLoaded <- NULL
        output$martError <- renderText(expr = "Mart loading failed! Try another mirror!")
        shinyjs::enable("mirrorChoice")
        shinyjs::enable("genomeChoice")
      })
    }
    if(input$genomeChoice == "mouse"){
      tryCatch(expr = {
        withTimeout(expr = {
          mart <<- useMart("ensembl", dataset = "mmusculus_gene_ensembl", host = host)
          output$martLoaded <- renderText(expr = paste(paste(input$mirrorChoice, "mirror", sep = " "), "Mouse mart loaded!", sep = ": "))
          output$martError <- NULL
          shinyjs::enable("mirrorChoice")
          shinyjs::enable("genomeChoice")
        }, timeout = 20, onTimeout = "error", substitute = FALSE)
      },
      TimeoutException = function(e){
        mart <<- NULL
        output$martLoaded <- NULL
        output$martError <- renderText(expr = "Mart loading failed! Try another mirror!")
        shinyjs::enable("mirrorChoice")
        shinyjs::enable("genomeChoice")
      },
      error = function(e){
        mart <<- NULL
        output$martLoaded <- NULL
        output$martError <- renderText(expr = "Mart loading failed! Try another mirror!")
        shinyjs::enable("mirrorChoice")
        shinyjs::enable("genomeChoice")
      })
    }
    if(!is.null(mart)){
      output$geneFile <- renderUI({fileInput(inputId = "geneFile2", label = h4("File to annotate:"))})
      shinyjs::enable("geneFile2")
    } else {
      output$geneFile <- renderUI({})
    }
  })
  
  # Set parameter choices 
  observeEvent(c(input$geneFile2),{
    inFile <- input[["geneFile2"]]
    if(is.null(inFile) | inFile$type != "text/plain" | is.null(mart)){
      attributeFullListIDs <<- NULL
      attributeFullListDesc <<- NULL
      output$listInput <- renderUI({})
      output$listType <- renderUI({})
      output$attributeTable <- NULL
      output$attributeTableTitle <- NULL
      output$selectedAttributesTitle <- NULL
      output$selectedAttributes <- NULL
      output$submit <- NULL
      output$queryError <- NULL
      output$geneSums <- NULL
      downloadExists$active <- NULL
      shinyjs::disable("submit")
      return(NULL)
    } else {
      attributeFullListIDs <<- NULL
      attributeFullListDesc <<- NULL
      output$selectedAttributesTitle <- NULL
      output$selectedAttributes <- NULL
      downloadExists$active <- NULL
      shinyjs::disable("submit")
      test <- read.table(inFile$datapath, header = TRUE, sep = "\t", stringsAsFactors = F)
      list.choices <- c("Row names", colnames(test))
      names(list.choices) <- list.choices
      list.choices <- as.list(list.choices)
      output$listInput <- renderUI({selectInput(inputId = "listInput2", label = h4("Select list input column:"), choices = list.choices)})
      data.choices <- list("Entrez ID (NCBI)" = "entrezgene", 
                           "Ensembl Gene ID (eg. ENSG....)" = "ensembl_gene_id",
                           "Ensembl Transcript ID (eg. ENST....)" = "ensembl_transcript_id",
                           "Ensembl Peptide ID (eg. ENSP....)" = "ensembl_peptide_id",
                           "Official Gene Symbol (HUGO)" = "external_gene_name",
                           "RefSeq ID (mRNA)" = "refseq_mrna",
                           "RefSeq ID (ncRNA)" = "refseq_ncrna",
                           "RefSeq ID (peptide)" = "refseq_peptide",
                           "UCSC ID" = "ucsc",
                           "miRBase ID" = "mirbase_id",
                           "Illumina MouseRef 8 Probe" = "illumina_mouseref_8",
                           "Illumina MouseWG 6 V1 Probe" = "illumina_mousewg_6_v1",
                           "Illumina MouseWG 6 v2 Probe" = "illumina_mousewg_6_v2",
                           "Affymetrix MG U74A Probe" = "affy_mg_u74a",
                           "Affymetrix MG U74Av2 Probe" = "affy_mg_u74av2",
                           "Affymetrix MG U74B Probe" = "affy_mg_u74b",
                           "Affymetrix MG U74Bv2 Probe" = "affy_mg_u74bv2",
                           "Affymetrix MG U74C Probe" = "affy_mg_u74c",
                           "Affymetrix MG U74Cv2 Probe" = "affy_mg_u74cv2",
                           "Affymetrix MOE430A Probe" = "affy_moe430a",
                           "Affymetrix MOE430B Probe" = "affy_moe430b",
                           "Affymetrix MoEx 1 0 st v1 Probe" = "affy_moex_1_0_st_v1",
                           "Affymetrix MoGene 1 0 st v1 Probe" = "affy_mogene_1_0_st_v1",
                           "Affymetrix MoGene 2 1 st v1 Probe" = "affy_mogene_2_1_st_v1",
                           "Affymetrix Mouse430A 2 Probe" = "affy_mouse430a_2",
                           "Affymetrix Mouse430 2 Probe" = "affy_mouse430_2",
                           "Affymetrix Mu11KsubA Probe" = "affy_mu11ksuba",
                           "Affymetrix Mu11KsubB Probe" = "affy_mu11ksubb",
                           "Agilent SurePrint G3 GE 8x60k Probe" = "",
                           "Agilent WholeGenome Probe" = "",
                           "Agilent WholeGenome 4x44k v1 Probe" = "",
                           "Agilent WholeGenome 4x44k v2 Probe" = ""
                           )
      output$listType <- renderUI({selectInput(inputId = "listType2", label = h4("Select input type:"), choices = data.choices)})
      output$attributeCategory <- renderUI({selectInput(inputId = "attributeCategory2", label = h4("Select attribute category filter:"), choices = list("All attributes" = "all", "IDs" = "ids"))})
      output$attributePage <- renderUI({selectInput(inputId = "attributePage2", label = h4("Select attribute page filter:"), choices = list("Feature page" = "feature_page", "Homologs" = "homologs", "Sequences" = "sequences", "SNP" = "snp", "Structure" = "structure"))})
      output$attributeTable <- renderDataTable(listAttributes(mart))
      output$attributeTableTitle <- renderText("Select biomaRt attributes to annotate:")
      output$geneSums <- renderUI({radioButtons(inputId = "includeMyGeneSummary", label = h4("Include gene summaries (MyGene):"), choices = list("Yes" = T, "No" = F), selected = F)})
      output$queryError <- NULL
    }
  })
  
  # Update attribute table
  observeEvent(c(input$attributeCategory2, input$attributePage2, input$clearAttributes),{
    if(!is.null(mart)){
      filter <- input$attributeCategory2
      pageFilter <- input$attributePage2
      fullTable <- listAttributes(mart)
      if(is.null(filter)){
        return(NULL)
      } else if(filter == "all"){
        fullTable <- fullTable[which(fullTable$page == input$attributePage2),]
        output$attributeTable <- renderDataTable(fullTable, selection = list(selected = which(fullTable[,1] %in% attributeFullListIDs)))
      } else if(filter == "ids"){
        fullTable <- fullTable[which(fullTable$page == input$attributePage2),]
        fullTable <- fullTable[grep(pattern = "_id", x = fullTable[,1]),]
        output$attributeTable <- renderDataTable(fullTable, selection = list(selected = which(fullTable[,1] %in% attributeFullListIDs)))
      }
    }
  })
  
  # Choose attributes
  observeEvent(c(input$attributeTable_rows_selected, input$geneFile2),{
    output$finalTableTitle <- NULL
    output$finalTable <- NULL
    shinyjs::disable("downloadData")
    filter <- input$attributeCategory2
    pageFilter <- input$attributePage2
    fullTable <- listAttributes(mart)
    if(is.null(filter)){
      return(NULL)
    }
    if(filter == "all"){
      attributeList <-  fullTable[which(fullTable$page == input$attributePage2),]
    } else if(filter == "ids"){
      attributeList <-  fullTable[which(fullTable$page == input$attributePage2),]
      attributeList <- attributeList[grep(pattern = "_id", x = attributeList[,1]),]
    }
    index = input$attributeTable_rows_selected
    attributeIDs <- attributeList[index,1]
    attributeDesc <- attributeList[index,2]
    if(length(attributeFullListIDs) >= 1){
      currentSelection <- which(attributeList[,1] %in% attributeFullListIDs)
    } else {
      currentSelection <- NULL
    }
    attributeFullListIDs <<- unique(c(attributeFullListIDs, attributeIDs))
    attributeFullListDesc <<- unique(c(attributeFullListDesc, attributeDesc))
    if(length(currentSelection) > length(attributeIDs)){
      rmID <- attributeList[which(attributeList[,1] %in% attributeFullListIDs),1][!(attributeList[currentSelection, 1] %in% attributeIDs)]
      rmDesc <- attributeList[which(attributeList[,1] %in% attributeFullListIDs),2][!(attributeList[currentSelection, 1] %in% attributeIDs)]
      attributeFullListIDs <<- attributeFullListIDs[-which(attributeFullListIDs == rmID)]
      attributeFullListDesc <<- attributeFullListDesc[-which(attributeFullListDesc == rmDesc)]
    }
    if(is.null(attributeFullListIDs) | length(attributeFullListIDs) == 0){
      shinyjs::disable("submit")
      output$selectedAttributesTitle <- renderText("Selected attributes:")
      output$selectedAttributes <- renderText("")
    } else if(!is.null(attributeFullListIDs)){
      shinyjs::enable("submit")
      output$selectedAttributesTitle <- renderText("Selected attributes:")
      output$selectedAttributes <- renderText(paste(attributeFullListDesc, collapse = ", "))
    }
  })
  
  # Clear attribute list
  observeEvent(c(input$clearAttributes),{
    attributeFullListIDs <<- NULL
    attributeFullListDesc <<- NULL
    output$selectedAttributesTitle <- renderText("Selected attributes:")
    output$selectedAttributes <- renderText("")
    shinyjs::disable("submit")
  })
  
  # Observe column selection and type input
  observeEvent(c(input$listInput2, input$listType2), {
    output$finalTableTitle <- NULL
    output$finalTable <- NULL
    shinyjs::disable("downloadData")
  })
  
  # Run biomaRt
  observeEvent(c(input$submit),{
    inFile <- input[["geneFile2"]]
    if(!is.null(inFile)){
      df <- read.table(inFile$datapath, header = TRUE, sep = "\t", stringsAsFactors = F)
      if(!is.null(attributeFullListIDs)){
        attributeList <- listAttributes(mart)
        inputValues <- input$listInput2
        if(inputValues == "Row names"){
          inputValues <- rownames(df)
        } else {
          inputValues <- df[,inputValues]
        }
        inputValues <- as.character(inputValues)
        filters <- input$listType2
        final_attributes <- c(filters, attributeFullListIDs)
        final_attributes <- unique(final_attributes)
        tryCatch(expr = {
          # withCallingHandlers({
          #   html(id = "biomartMessage", "")
          #   annotation <- getBM(attributes = final_attributes, filters = filters, values = inputValues, mart = mart, uniqueRows = TRUE)
          # },
          # message = function(m){
          #   html(id = "biomartMessage", html = m$message, add = FALSE)
          #   html(id = "biomartMessage", html = "Getting biomart annotation!", add = FALSE)
          # })
          html(id = "biomartMessage", "")
          html(id = "biomartMessage", html = "Getting biomart annotation!", add = FALSE)
          annotation <- getBM(attributes = final_attributes, filters = filters, values = inputValues, mart = mart, uniqueRows = TRUE)
          annotation <- as.data.frame(aggregate(as.formula(paste(". ~ ", filters, sep = "")), annotation, paste, collapse = ","))
          anno_total <- nrow(annotation)*ncol(annotation)
          for(i in 1:nrow(annotation)){
            for(j in 1:ncol(annotation)){
              html(id = "biomartMessage", html = paste("Checking duplicates:", i*j, "/", anno_total), add = FALSE)
              annotation[i,j] <- paste(unique(unlist(strsplit(annotation[i,j], ","))), collapse=", ")
            }
          }
          output$queryError <- renderText("")
          columnValues <- input$listInput2
          if(columnValues == "Row names"){
            df <- cbind(df, annotation[match(rownames(df), annotation[,filters]),])
          } else {
            df <- cbind(df, annotation[match(df[,columnValues], annotation[,filters]),])
          }
          if(input$includeMyGeneSummary){
            if(!("entrezgene_id" %in% colnames(df))){
              html(id = "biomartMessage", html = "Getting entrez IDs!", add = FALSE)
              entrez_ids <- getBM(attributes = c("entrezgene_id", filters), filters = filters, values = inputValues, mart = mart, uniqueRows = TRUE)
              if(columnValues == "Row names"){
                df <- cbind(df, entrez_ids[match(rownames(df), entrez_ids[,filters]),])
              } else {
                df <- cbind(df, entrez_ids[match(df[,columnValues], entrez_ids[,filters]),])
              }
            }
            df$geneSummaries <- c(rep("-", nrow(df)))
            for(i in 1:nrow(df)){
              html(id = "biomartMessage", html = paste("Getting gene summaries:", i, "/", nrow(df)), add = FALSE)
              if(!is.na(df[i,"entrezgene_id"])){
                summaryVal <- mygene::getGene(geneid = df[i,"entrezgene_id"], fields = "all")[[1]]$summary
              } else {
                summaryVal <- NULL
              }
              if(!is.null(summaryVal)){
                df$geneSummaries[i] <- summaryVal
              }
            }
          }
          output$downloadData <- downloadHandler(filename = function(){"annotated_df.txt"}, content = function(file){write.table(df, file, sep = "\t", quote = F)})
          output$finalTableTitle <- renderText("Annotated table:")
          output$finalTable <- renderDataTable(df)
          downloadExists$active <- TRUE
          shinyjs::enable("downloadData")
        },
        error = function(e){
          annotation <- NULL
          output$queryError <- renderText("Server query failed! Try another mirror!")
          return(NULL)
        })
      }
    } else {
      return(NULL)
    }
  })
  
  # Enable download button
  observe({
    if(is.null(downloadExists$active)){
      output$finalTableTitle <- NULL
      output$finalTable <- NULL
      shinyjs::disable("downloadData")
    } else if(downloadExists$active == TRUE){
      shinyjs::enable("downloadData")
    }
  })
  
  # Stop application command required for Inno deployment
  if(!interactive()){
    session$onSessionEnded(function(){
      stopApp()
      q("no")
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

