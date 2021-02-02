#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

ksource <- function(x, ...) {
    library(knitr)
    source(purl(x, output = tempfile()), ...)
}

## ksource("analysis.Rmd")
load("shiny_input.RData")

cell_types = setNames(c("Alveolar macrophages", "Eosinophils"),
                      c("macro", "eos"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Macrophages DEG multiple comparisons"),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
      #   sidebarPanel(
            selectInput(
                inputId = "deg_type",
                label = "DEG comparison:",
                choices = unname(contrast_names),
                selected = NULL,
                selectize = TRUE,
                width = NULL,
                 size = NULL
                        ),
    selectInput(
        inputId = "cell",
        label = "Cell type:",
        choices = unname(cell_types),
        selected = NULL,
        selectize = TRUE,
        width = NULL,
        size = NULL
    ),
            actionButton(inputId="run.glimma", label="Glimma plot")
            )
        # Show a plot of the generated distribution
        # mainPanel(
           # plotOutput("distPlot")
           htmlOutput("glimma")
        # )
#    )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
    
    observeEvent({
        input$deg_type
        input$cell}, 
        {
        test_name = names(which(contrast_names == input$deg_type))
        cell_type = names(which(cell_types == input$cell))
        
        tfit_object = get(paste0(cell_type, "_tfit"))
        test_outs_object = get(paste0(cell_type, "_test_outs"))
        lcpm_object = get(paste0(cell_type, "_lcpm"))
        groups = macro_dge_norm$samples$group
        glMDPlot(tfit_object, coef = test_name, status = test_outs_object, 
                 main = contrast_names[test_name], 
                 side.main = "external_gene_name", counts = lcpm_object, 
                 groups = groups, launch = TRUE)
        # glMDSPlot(dgeObj.norm(), labels=rownames(dgeObj.norm()$samples), 
        # groups=dgeObj.norm()$samples[,as.character(input$mds.grouping.feature)])
    })
    
    output$glimma <- renderUI({
        includeHTML("glimma-plots/MD-Plot.html")
        # includeHTML("../test_html.html")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
