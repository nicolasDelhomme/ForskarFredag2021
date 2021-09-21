# Load packages
suppressPackageStartupMessages({
  library(here)
  library(shiny)
  library(shinythemes)
})

# data
colors <- c("yellow","green")
alleles <- c("ACGT","TCGA")
allele.df <- cbind(expand.grid(allele1=alleles,
                               allele2=alleles),
                   color=c("yellow","orange","orange","red"))
genes <- c("protoporphyrin ferrochelatase" = "18097764",
           "8-vinyl-reductase" = "7481639",
           "chlorophyll synthase" = "7466089")

pathway="pop00860"

# UI
ui <- navbarPage("Autumn's colorful secret",
                 tabPanel("Candidate selection",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("gene", "Select a candidate gene:",
                                           choices=genes,
                                           selected=character(0))
                            ),
                            mainPanel(
                              htmlOutput(outputId = "intro"),
                              imageOutput(outputId= "pathway")
                            ))
                 ),
                 tabPanel("Genotype determination",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("allele1", "Select the first sequence:", 
                                          choices=alleles),
                              hr(),
                              selectInput("allele2", "Select the second sequence:", 
                                          choices=alleles),
                              helpText("Do not hesitate to combine the sequences in a different order!")
                            ),
                          mainPanel(
                            htmlOutput(outputId = "intro2"),
                            imageOutput(outputId= "tree"),
                            htmlOutput(outputId = "result2")
                          ))
                          ),
                 navbarMenu("More",
                            tabPanel("Disclaimer",htmlOutput(outputId = "disclaimer")))
                 )


# Server
server <- function(input, output) {
  output$intro <- renderUI(HTML(
    here(paste(pathway,
               ifelse(is.null(input$gene),"pathview",input$gene),
               "png",sep="."))))
  
  output$pathway <- renderImage({
    list(src = here(paste(pathway,
                     ifelse(is.null(input$gene),"pathview",input$gene),
                     "png",sep=".")),
         contentType = 'image/png',
         width = 600,
         height = 800,
         alt = ifelse(input$gene==character(0),
                      "A view of the chlorophyll pathway",
                      sprintf("A view of the chlorophyll pathway, displaying the gene %s",input$gene)))
  }, deleteFile = FALSE)
  
  output$intro2 <- renderUI(HTML(
    paste("You have selected the cut site number",
          "<b>",input$cutSite,"</b>",
          "<br/>In the table below you can see where it starts and ends in the gene, as well as the recognised sequence (guide RNA)")))
  
  output$tree <- renderImage({
    list(src = paste("Trees",
                     allele.df[allele.df$allele1==input$allele1 & 
                                 allele.df$allele2==input$allele2
                               ,"color"],
                     "jpg",sep="."),
         contentType = 'image/jpeg',
         width = 400,
         height = 300,
         alt = paste("The resulting color of the tree"))
  }, deleteFile = FALSE)
  
  output$result2 <- renderUI(HTML(
    paste("You have selected the cut site number",
          "<b>",input$cutSite,"</b>",
          "<br/>In the table below you can see where it starts and ends in the gene, as well as the recognised sequence (guide RNA)")))
}

# Create Shiny object
shinyApp(ui = ui, server = server)