
barpage = fluidPage( 
  sidebarPanel(
  #uiOutput("selectorSample"),
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
)