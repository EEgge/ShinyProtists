##############################
#UI
##############################

output$bar_ui_xvar <- renderUI({
  selectInput()
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

output$selectorEnv1 <- renderUI({
  radioButtons("env1", "Choose var1:", as.list(colnames(envdata())))
})

output$selectorEnv2 <- renderUI({
  radioButtons("env2", "Choose var2:", as.list(colnames(envdata())))
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
