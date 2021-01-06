##########
#UI
##########

output$selectorTaxlevelOrd <- renderUI({
  radioButtons("taxlevel_ord", "Choose taxonomic level:", as.list(colnames(taxonomy())))
})

output$selectorTaxgroupsOrd <- renderUI({
  checkboxGroupInput("taxgroups_ord", "Choose taxonomic group:", taxgroups_ord()$taxvec)
})

output$selectorColor <- renderUI({
  radioButtons("color_aes", "Choose env. variable to color samples:", colnames(envdata()))
})

output$selectorShape <- renderUI({
  radioButtons("shape_aes", "Choose env. variable to shape samples:", colnames(envdata()))
})

output$sjekk <- renderTable({ 
  ordination()$ord_table
})

output$nmdsplot1 <- renderPlot({
  ordination()$nmdsplot
})
# 
# ##########
# #Server
# #########
taxgroups_ord <- reactive({
  taxlev <- input$taxlevel_ord
  taxvec <- taxonomy() %>% pull(taxlev) %>% levels()
  list(taxvec = taxvec)
})
# 

# ord_aes <- reactive({
#   color_aes <- envdata() %>% pull(input$color_aes)
#   shape_aes <- envdata() %>% pull(input$shape_aes)
#   list(color_aes = color_aes, shape_aes = shape_aes)
# })


ordination <- eventReactive(input$actionb_ord1, {
#   # Prepare OTU table for first taxonomic group #
  otutab_tax_filter_list <- list()
  otuid_list <- list()
  sjekk1 <- input$taxgroups_ord
  sjekk2 <- input$taxlevel_ord
  sjekk <- c(sjekk1, sjekk2)
  for (i in 1:length(input$taxgroups_ord)) {
  otutab_tax_filter_list[[i]] <- otutab_prop() %>% filter(.data[[input$taxlevel_ord]] %in% .env$input$taxgroups_ord[i]) %>% select_if(is.numeric)
  otuid_list[[i]] <- otutab_prop() %>% filter(.data[[input$taxlevel_ord]] %in% .env$input$taxgroups_ord[i]) %>% pull(otuid) #Not hard code 'otuid'
  rownames(otutab_tax_filter_list[[i]]) <- otuid_list[[i]]
  }
  
  ### Run ordination ####
  ordination_taxgroups_list <- list()
  for (i in 1:length(input$taxgroups_ord)) {
    tmp <- mp_nmds(otutab_tax_filter_list[[i]])
    ordination_taxgroups_list[[i]] <- cbind.data.frame(tmp$points[,1], tmp$points[,2])
      names(ordination_taxgroups_list[[i]])[c(1,2)] <- c("nmds_axis1", "nmds_axis2")
  }
  
      ###Plotting NMDS ####
  color_aes <- envdata() %>% pull(input$color_aes)
  shape_aes <- envdata() %>% pull(input$shape_aes)
  plot_nmds_list <- list()
  for (i in 1:length(input$taxgroups_ord)) {
      plot_nmds_list[[i]] <- nmdsplot(ordination_taxgroups_list[[i]], color_aes, shape_aes)+
        xlim(min(ordination_taxgroups_list[[i]]$nmds_axis1)-.1,max(ordination_taxgroups_list[[i]]$nmds_axis1)+.1)+
        ylim(min(ordination_taxgroups_list[[i]]$nmds_axis2)-.1,max(ordination_taxgroups_list[[i]]$nmds_axis2)+.1)#+
  }
  
  nmdsplots <- ggarrange(plotlist = plot_nmds_list, common.legend = TRUE, labels = input$taxgroups_ord)
  #### Select environmental variable for color aes ####
 
  
  #list of taxlevel_ord ?
#   otutab_prop_tax_filter_list[2] <- prop_tax()$otutab_prop_tax %>% filter(.data[[input$taxlevel2_ord]] %in% .env$input$taxgroup_ord) #list of taxlevel_ord ?
#   otutab_ord_list <- list()
#   for (i in c(1,2)) {
#     otutab_ord_list[i] <- otutab_prop_tax_filter_list[i] %>% select_if(is.numeric)
#     
#     if (input$propselect_ord == TRUE) {
#       otutab_ord_prop <- otutab_ord %>% sweep(., 2 , colSums(.), FUN = "/")
#     } else {
#       otutab_ord_prop <- otutab_ord_prop
#     }
#     rownames(otutab_ord_prop) <- otutab_prop_tax_filter$otuid
#     
#     
#     #geom_path(aes(x = V1, y = V2), data = ordih1)
#     
#     #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
#     nmdsplotly <- ggplotly(plot_nmds, tooltip = "text")
#   }
#   
#   list(nmdsplotly = nmdsplotly)
# return(sjekk)
 list(ord_table = otutab_tax_filter_list[[1]], nmdsplot = nmdsplots)
})