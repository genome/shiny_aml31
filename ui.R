library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
    
  # Application title
  titlePanel("AML31 SNV benchmarking"),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(

    #fileInput('vcffile', 'Upload your VCF file',
    #          accept=c('.vcf', '.vcf.gz')),
    fileInput('tabfile', 'Upload your tab-delimited file'),
      br(),
      radioButtons("list", "Truth-set List:",
                   c("Platinum" = "plat",
                     "Gold" = "gold")),

      br(),
  
      conditionalPanel(condition="input.tabsetPanel==1",
        sliderInput("range", "VAF:", min = 0, max = 100, value = c(0,100))
      )
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Performance Venn", plotOutput("plot"), value="venn"), 
        tabPanel("Sensitivity Histogram", plotOutput("hist"), value="hist"), 
        #tabPanel("Caller Performance", verbatimTextOutput("summary"), value="callers"), 
        tabPanel("Uploaded Variants", dataTableOutput("uploaded"), value="uploadList"),
        tabPanel("Truth-set", dataTableOutput("truth"), value="truthList"),
        id = "tabsetPanel"
      )
    )
  )
))
