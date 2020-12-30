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

source(here("src", "nmdsplot.R"))
source(here("src", "mp_nmds.R"))

# Define UI for data upload app ----
ui <- navbarPage(
    
    # App title ----
    "Shiny Happy Protists",
    
    # Tabulated
    tabPanel("Uploading Files",
    # Sidebar layout with input and output definitions ----
        sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose OTU table",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = "\t"),
            
            
            #### Environmental data file ####
            fileInput("file2", "Choose environmental data file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sepEnv", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = "\t"),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            #### Taxonomy ####
            
            fileInput("file3", "Choose taxonomy file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sepTax", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = "\t"),
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            textOutput("contents")
          
            
        )
        
    )
),
    tabPanel("Proportional abundance",
             sidebarPanel(
             uiOutput("selectorSample"),
             #uiOutput("selectorEnv"),
             uiOutput("selectorTaxlevel"),
             uiOutput("selectorTaxgroup"),
             uiOutput("selectorTaxlevel2"),
             #tableOutput("taxtest")
             checkboxInput("propselect", "Create proportions of selected groups", FALSE),
             uiOutput("selectorEnv1"),
             uiOutput("selectorEnv2")
             ),
             mainPanel(
                 plotOutput(outputId = "propplot"),
                 textOutput(outputId = "envja"),
                 tableOutput(outputId = "otutable")
             )
             
             
),
    tabPanel("Ordination - compare taxonomic groups",
             sidebarPanel(
                 uiOutput("selectorTaxlevel_ord"),
                 uiOutput("selectorTaxgroup1_ord"),
                 uiOutput("selectorTaxgroup2_ord")
             ),
                 mainPanel(
                     plotOutput(outputId = "")
                 )
             )
)
# Define server logic to read selected file ----
server <- function(input, output, session) {
    
    otutab <- reactive({
        inFile <- input$file1   
        table1<-read.csv(inFile$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
            return(table1)
        list(table1 = table1)
        
    })
    
    envdata <- reactive({
        inFile <- input$file2
        table2 <- read.csv(inFile$datapath, header = input$header, 
                           sep = input$sepEnv, quote = input$quote)
        return(table2)
    })
    
    taxonomy <- reactive({
        inFile <- input$file3
        table3 <- read.csv(inFile$datapath, header = input$header, 
                           sep = input$sepTax)
        return(table3)
    })
    
    otutabtax <- reactive({
        inFile1 <- input$file1
        inFile3 <- input$file3
        table1 <- read.csv(inFile1$datapath, header = input$header,
                           sep = input$sepOTU)
        table3 <- read.csv(inFile3$datapath, header = input$header, 
                           sep = input$sepTax)
        otutabtax <- inner_join(table1, table3, by = "otuid")
        return(otutabtax)
    })
    
    taxlevel <- reactive({
        taxlev <- input$taxlevel
        return(taxlev)
        #list(taxlev = taxlev)
    })
    
    taxgroups <- reactive({
        taxlev <- input$taxlevel
        taxvec <- taxonomy() %>% pull(taxlev) %>% levels()
        list(taxvec = taxvec)
    })
    
    #### Barplot of proportional abundance ####
    prop_tax <- reactive({
        taxlevel_barplot <- input$taxlevel
        # Transform to proportional abundance
        otutab_prop0 <- sweep(otutab()[,-1], 2, colSums(otutab()[,-1]), FUN = "/")
        otutab_prop1 <- otutab_prop0 %>% mutate(otuid = otutab()[,1]) 
        otutab_prop_tax <- inner_join(otutab_prop1, taxonomy(), by = "otuid")
        
        otutab_prop_tax_filter <- otutab_prop_tax %>% filter(.data[[input$taxlevel]] %in% .env$input$taxgroup)
        
        
        if (input$propselect == TRUE) {
            prop_taxlev_select0 <- otutab_prop_tax_filter %>%  select_if(is.numeric) %>% 
                sweep(., 2 , colSums(.), FUN = "/")
            prop_taxlev_select <- cbind.data.frame(otutab_prop_tax_filter %>% select_if(negate(is.numeric)), prop_taxlev_select0)
        } else {
            prop_taxlev_select <- otutab_prop_tax_filter
        }
        
        prop_taxlev_select_lev2 <- prop_taxlev_select %>% group_by(.data[[input$taxlevel2]]) %>% 
            summarise_if(is.numeric, sum)
        
       prop_taxlev_piv <- prop_taxlev_select_lev2 %>% pivot_longer(!.data[[input$taxlevel2]], names_to = "sample") 
       
       prop_taxlev_piv_env <- inner_join(prop_taxlev_piv, envdata(), by = "sample")
        
       prop_taxlev_plot <- ggplot(prop_taxlev_piv_env, aes(x=sample, y=value, fill = .data[[input$taxlevel2]])) + 
           geom_bar(stat = "identity", position = "stack")+
           #facet_wrap(month + station ~., scale = "free")
           facet_wrap(get(input$env1) + get(input$env2) ~., scale = "free_x") # Why does not .data[[input$env1]] work here?
       
        list(otutab_prop_tax = otutab_prop_tax, prop_taxlev_plot = prop_taxlev_plot, prop_taxlev_piv_env = prop_taxlev_piv_env)
    })
    
    #### Ordination analysis reactive ####
    
    ordination <- reactive({
        # Prepare OTU table for first taxonomic group #
        otutab_prop_tax_filter_list <- list()
        otutab_prop_tax_filter_list[1] <- prop_tax()$otutab_prop_tax %>% filter(.data[[input$taxlevel1_ord]] %in% .env$input$taxgroup_ord) #list of taxlevel_ord ?
        otutab_prop_tax_filter_list[2] <- prop_tax()$otutab_prop_tax %>% filter(.data[[input$taxlevel2_ord]] %in% .env$input$taxgroup_ord) #list of taxlevel_ord ?
        otutab_ord_list <- list()
        for (i in c(1,2)) {
        otutab_ord_list[i] <- otutab_prop_tax_filter_list[i] %>% select_if(is.numeric)
        
        if (input$propselect_ord == TRUE) {
            otutab_ord_prop <- otutab_ord %>% sweep(., 2 , colSums(.), FUN = "/")
        } else {
            otutab_ord_prop <- otutab_ord_prop
        }
        rownames(otutab_ord_prop) <- otutab_prop_tax_filter$otuid
        
        #### Run ordination ####
        nmds_tax <- mp_nmds(otutab_ord_prop)
        nmds_table <- data.frame(cbind(nmds_tax$points[,1], nmds_tax$points[,2], meta_sf))
        names(nmds_table)[c(1,2)] <- c("nmds_axis1", "nmds_axis2")
        
        ###Plotting NMDS
        plot_nmds <- nmdsplot(nmds_table)+
            xlim(min(nmds_table$nmds_axis1)-.1,max(nmds_table$nmds_axis1)+.1)+
            ylim(min(nmds_table$nmds_axis2)-.1,max(nmds_table$nmds_axis2)+.1)#+
        #geom_path(aes(x = V1, y = V2), data = ordih1)
        
        #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
        nmdsplotly <- ggplotly(plot_nmds, tooltip = "text")
        }
        
        list(nmdsplotly = nmdsplotly)
    })
    

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