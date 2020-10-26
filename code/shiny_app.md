# Tutorial on how to reproduce and modify the Shiny App for the Deworming Open Policy Analysis

## Your setup to reproduce and modify the shiny app of the deworming OPA

- To run the shiny app (SA) you will need to install R and RStudio. The relevant file can be found by running 'ui.R'.

- Click 'Run App' located at the top of RStudio. This will open the SA.

- The SA has three tabs to explore
    - Main Policy Estimate, which describes the results. There is no interactive capabilities on this tab.
    - Key Assumptions, where you can adjust 'policy estimate', 'unit costs in new country', and 'prevelance in the new region'.
    - Main Assumptions, where you can adjust 'number of simulations' and 'policy estimate', as well as choose between 'Data' and 'Research'
        - Data
            - Gov Bonds (and SD)
            - Inflation (and SD) 
            - Exchange rate (and SD)
            - Costs of T (and SD)
            - Years of treatment in original study (and SD)
            - Years of treatment in new setting (and SD)
            - Costs adjustments (and SD)
        - Research
            - Take-up (and SD)
            - Take-up with no subsidy (and SD)
            - Lambda 1_1_new (and SD)
            - Lambda 1_2_new (and SD)
            - Lambda 1_3_new (and SD)
            - Prevalence in original study (and SD)

## Beginning

- The code begins with a list of all relevant libraries **(should I list them or is there no need?)**
- Then, it states the unit costs and the prevalences of 4 countries: India, Kenya, Nigeria, and Vietnam
- At the top of the SA, it says "Open Policy Analysis for Deworming Interventions: Open Output Component"
    - This is created using 'navbarPage'


## The tabs

- All three tabs are created using 'tabPanel'.
  - Then, use sidebarPanel, which creates the panel on the left side of the screen. For tab titled "Main Policy Estimate", this only consists of text and links.
    - 'fluidrow' creates rows, such as "Unit costs in new country" in "Key Assumptions".
  - 'mainPanel' creates the graph seen on the right

## The functions

- shinyUI -> creates a user interface
- fluidPage -> creates a page
- navbarPage -> creates a page with a navigation bar at the top
- tabPanel -> creates a tab panel, takes in a title (i.e. "Main Policy Estimate") and a value **(should I do this explanation for each function, where all inputs are listed?)**
- sidebarPanel -> creates a sidebar that has input controls
- tags$a -> creates a link
- mainPanel -> creates a main panel of output elements
- plotOutput -> creates a plot
- checkboxInput -> creates a checkbox used to specify logical values
- numericInput -> creates an input control for numeric values
- withMathJax -> loads in MathJax, a JavaScript library
- useShinyjs -> loads in Shinyjs, an r package
- helpText -> creates text to help explain some element of the SA
- selectInput -> creates a list of choices
- sliderInput -> creates a slider widget containing numbers in a range
- tabsetPanel -> creates a tabset containing elements from tabPanel
- uiOutput -> tells Shiny where the controls should be rendered

**What other topics should be covered in this readme?**
**Should I add a full explanation of 'server.R'?**



        
