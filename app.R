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

gene.col <- colors[c(2,2,1)]

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
                              htmlOutput(outputId = "text"),
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
                              helpText("Do not hesitate to combine the sequences in any order!")
                            ),
                          mainPanel(
                            htmlOutput(outputId = "text2"),
                            imageOutput(outputId= "tree"),
                          ))
                          ),
                 navbarMenu("More",
                            tabPanel("Disclaimer",htmlOutput(outputId = "disclaimer")))
                 )


# Server
server <- function(input, output) {
  
  # Tab selection
  output$text <- renderUI(HTML(
    paste("This is a representation of the <b>Porphyrin and chlorophyll metabolism</b> pathway; 
          a visualisation of the cascade of enzymes and compounds that among other things
          will help produce the green pigment (chloropyll) in the leaves. If you select
      one of a candidate gene in the left panel, you will see what could be the resulting leaves' color.
      <br/>
      <hr/>",
    
    ifelse(is.null(input$gene),"Your task is to find the candidate gene that would result in the color the trees have in autumn.
      The selected gene will show up in the pathway below colored by the expected resulting leaf color.
      <br/>",
      sprintf("You have selected the gene: <b>%s</b>, which will result in a <b>%s</b> color of the leaves",
              names(genes)[genes==input$gene],gene.col[genes==input$gene]
    )),
    "<hr/>"
    )))
  
  output$pathway <- renderImage({
    list(src = here(paste0(pathway,
                          ifelse(is.null(input$gene),"",paste0(".",input$gene)),".png")),
         contentType = 'image/png',
         width = 600,
         height = 800,
         alt = ifelse(input$gene==character(0),
                      "A view of the chlorophyll pathway",
                      sprintf("A view of the chlorophyll pathway, displaying the gene %s",input$gene)))
  }, deleteFile = FALSE)
  
  # Tab genotype
  output$text2 <- renderUI(HTML({
    col <- allele.df[allele.df$allele1==input$allele1 & 
                       allele.df$allele2==input$allele2
                     ,"color"]
    paste("Like us humans, most trees are diploid; they have two copies of their genetic material.",
          "<br/>",
          "This means that every gene will exist in two copies, which explain why you got two extra sequences.",
          "<br/>",
          "<hr>",
          ifelse(input$allele1==alleles[1] & 
                   input$allele2==alleles[1],
                 paste("In the panel on the left, select the sequences you retrieved from the sequencer,",
                 "and reveal the leaf color that would be observed."),
                 paste(sprintf("The allelic combination selected would result in the %s leaf color.",col),
                       "<br/><hr/>",
                       "Note that it is frequent that the gene is present in two allelic version (the two different copies you obtained from the sequencer).",
                       "<br/><br/>",
                       paste("It is not uncommon that this results in a gradient of color as presented here,",
                             "where both alleles have an equal contribution to the phenotype - the color; we observe.),",
                             "Situations were one allele - one of the two copies; is stronger than the other, ",
                             "which are then called dominant and recessive, respectively) are more common.",
                             "This was actually revealed by Gregor Mendel, nicknamed the father of genetics, aleready in 1865!")
                       )),
          "<hr/>")}))
  
  output$tree <- renderImage({
    col <- allele.df[allele.df$allele1==input$allele1 & 
                allele.df$allele2==input$allele2
              ,"color"]
    list(src = here(paste("Trees",
                     col,
                     "jpeg",sep=".")),
         contentType = 'image/jpeg',
         width = 800,
         height = 600,
         alt = sprintf("The resulting color of the tree is %s.",col,"color"))
  }, deleteFile = FALSE)
  
  # Navbar menu
  output$disclaimer <- renderUI(HTML(
    paste(
      "<h3>Disclaimer</h3>",
      "<br/>The genes selected as an example here would not change the tree color as described.",
      "<br/>While this example is purely <b>fictional</b> and its aim <b>educational</b>",
      "the technology described <br/> exist and could be used as described.",
      "<br/>The genetic concepts described herein are otherwise accurate."
  )))
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)