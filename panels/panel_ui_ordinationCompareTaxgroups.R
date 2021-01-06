sbp_ord = sidebarPanel(
  actionButton("actionb_ord1", "Start analyses", icon("signal")),
  h4('Select taxonomic level'),
  uiOutput("selectorTaxlevelOrd", inline = TRUE),
  uiOutput("selectorTaxgroupsOrd", inline = TRUE),
  uiOutput("selectorColor", inline = TRUE),
  uiOutput("selectorShape", inline = TRUE)
)

########
ordpage = fluidPage(
  headerPanel("Ordination"),
  sidebarLayout(
    sbp_ord,
  mainPanel(
    tableOutput(outputId = "sjekk"),
    plotOutput(outputId = "nmdsplot1")
  
)
)
)