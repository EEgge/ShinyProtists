#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(vegan)
library(here)
library(plotly)
library(ggpubr)

source(here("src", "shinyprot_nmdsplot.R"))
source(here("src", "mp_nmds.R"))

source("panels/panel_ui_ordinationCompareTaxgroups.R", local = TRUE)
source("panels/panel_ui_barplot.R", local = TRUE)
source("panels/panel_ui_upload.R", local = TRUE)

ui <- navbarPage(
    
    # App title ----
    "Shiny Happy Protists",
    
    # Tabulated
    tabPanel("Uploading Files", uploadpage),
    tabPanel("Proportional abundance", barpage),
    tabPanel("Ordination", ordpage)
)
# Define server logic to read selected file ----
server <- function(input, output, session) {

    #main reactive OTU/ASV table. 
    #subsets of taxonomic groups and/or samples can be selected in downstream panels.
    #various transformations can be compared in downstream ordination panels.
    otutab <- reactive({
        if (input$useexdata == TRUE) {
            table1 <- read.csv(here("data", "otutab.txt"), sep = "\t", header = T)
        } else {
        inFile <- input$file1   
        table1<-read.csv(inFile$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
        }
            return(table1)
        list(table1 = table1)
        
    })
    
    #Main reactive taxonomy table.
    
    taxonomy <- reactive({
        if (input$useexdata == TRUE) {
            table3 <- read.csv(here("data", "taxonomy.txt"), sep = "\t", header = T)
        } else {
        inFile <- input$file3
        table3 <- read.csv(inFile$datapath, header = input$header, 
                           sep = input$sepTax)
        }
        return(table3)
    })
    
    envdata <- reactive({
        if (input$useexdata == TRUE) {
            table2 <- read.csv(here("data", "envdata.txt"), sep = "\t", header = T)
        } else {
        inFile <- input$file2
        table2 <- read.csv(inFile$datapath, header = input$header, 
                           sep = input$sepEnv, quote = input$quote)
        return(table2)
        }
    })
    
    otutabtax <- reactive({
        otutabtax <- inner_join(otutab(), taxonomy(), by = "otuid")
        return(otutabtax)
    })
    
    #### Transform OTU table to proportions, join with taxonomy ####
    
    otutab_prop <- reactive({
        otutab_prop0 <- sweep(otutab()[,-1], 2, colSums(otutab()[,-1]), FUN = "/")
        otutab_prop1 <- otutab_prop0 %>% mutate(otuid = otutab()[,1]) 
        otutab_prop_tax <- inner_join(otutab_prop1, taxonomy(), by = "otuid")
        return(otutab_prop_tax)
    })
    # Barplot panel
    source("panels/panel_server_barplot.R", local = TRUE)
    
    #### Ordination analysis reactive #### 
    source("panels/panel_server_ordinationCompareTaxgroups.R", local = TRUE)
    
    
    

    output$envja <- renderText({
        input$env1
    })
    
    output$selectorSample <- renderUI({
        radioButtons("var", "Choose name:", as.list(colnames(otutab())))
    })
    
    output$selectorEnv1 <- renderUI({
        radioButtons("env1", "Choose var1:", as.list(colnames(envdata())))
    })
    
    output$selectorEnv2 <- renderUI({
        radioButtons("env2", "Choose var2:", as.list(colnames(envdata())))
    })
    
    output$selectorTaxlevel <- renderUI({
        radioButtons("taxlevel", "Choose taxonomic level:", as.list(colnames(taxonomy())))
    })
    
    output$selectorTaxgroup <- renderUI({
        taxlev <- input$taxlevel
        taxlev2 <- taxonomy() %>% select(taxlev)
        selectInput("taxgroup", "Choose taxonomic group:", taxgroups()$taxvec, multiple = TRUE)
    })
    
    output$selectorTaxlevel2 <- renderUI({
        radioButtons("taxlevel2", "Choose taxonomic level:", as.list(colnames(taxonomy())))
    })
    
    output$otutable <- renderTable({prop_tax()$prop_taxlev_piv_env
        
    })
    
    output$propplot <- renderPlot({prop_tax()$prop_taxlev_plot
        
    })
    
    output$taxtest <- renderTable({
        #taxlev <- input$taxlevel
        taxgroups()$taxja#<- taxonomy() %>% select(taxlev)
        #list(tax)
    })
    
    ####Input ordination ####
    output$selectorTaxlevel_ord <- renderUI({
        radioButtons("taxlevel_ord", "Choose taxonomic level:", as.list(colnames(taxonomy())))
    })
    
    output$selectorTaxgroup1_ord <- renderUI({
        selectInput("taxgroup", "Choose taxonomic group:", taxgroups()$taxvec, multiple = TRUE)
    })
    
    #### Output ordination ####
    output$ordination <- renderPlotly({
        ordination()$nmdsplotly
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)