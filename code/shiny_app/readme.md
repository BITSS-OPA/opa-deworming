# Tutorial on how to reproduce and modify the Shiny App for the Deworming Open Policy Analysis

## Your setup to reproduce and modify the UI of the deworming OPA

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

### Beginning

- The code begins with a list of all relevant libraries
- Then, it states the unit costs and the prevalences of 4 countries: India, Kenya, Nigeria, and Vietnam
- At the top of the SA, it says "Open Policy Analysis for Deworming Interventions: Open Output Component"
    - This is created using 'navbarPage'


### The tabs

- All three tabs are created using 'tabPanel'.
  - Then, use sidebarPanel, which creates the panel on the left side of the screen. For tab titled "Main Policy Estimate", this only consists of text and links.
    - 'fluidrow' creates rows, such as "Unit costs in new country" in "Key Assumptions".
  - 'mainPanel' creates the graph seen on the right

### The functions

- shinyUI -> creates a user interface
- fluidPage -> creates a page
- navbarPage -> creates a page with a navigation bar at the top
- tabPanel -> creates a tab panel, takes in a title (i.e. "Main Policy Estimate") and a value
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

## Your setup to reproduce and modify the server.r file of the deworming OPA

### The functions

- sim.data -> returns a random sample from some distributions
- as.numeric -> converts a factor to a numeric factor
- observeEvent -> **observer reference class object? not sure what that means, though**
- which -> returns the indices that are True
- paste -> links vectors together after to character
- ggplot -> data visualization package
    - geom_density -> computes and draws density estimate
    - geom_vline -> annotates the plot with vertical lines
    - xlim -> sets the min and max boundaries of the plot in the x-direction
    - guides -> the guide is set scale-by-scale for each scale
    - annotate- > adds small annotations, i.e. text labels
    - theme -> sets the theme of the plot by costumizing all non-data components

### Walkthrough

- 'reactive.data1' is an R expression that uses a widget input and outputs a value, and in this case the input is 'sim_data'.
- 'sim_data' sets variable names by turning the parameters used in ui.R to numeric factors
- The following section of 'server.R' is 'if' statements that are used to show which parameters to hide for different inputs of 'policy estimate'.
- Then, the code adds helps text to the SA depending on the input for 'policy estimate'.
- The next section defines a function that generates policy estimate plots
    - 'If' statements specify what texts to display and what 'selectInput' to call for each of the three different plots (Main, ka, all)
    - Then, the code constructs the plot for all three tabs
        - All have the same axes
        - All are density plots
        - All share the same plot structure

### How to add a new slider to the UI

- In order to add a slider, you will want to use the 'sliderInput' function in ui. R. 'sliderInput' is a widget that creates a slider across a range. It has five inputs: inputId, label, min, max, and value. Below is an example:

    `sliderInput("param3", label = "Inflation (\\( \\pi \\) ) = ", min = 0.001, max = 0.2, value = inflation_so)`

- Here, 'param3' is the inputID, 'Inflation (π) =' is the label, min is 0.001, max is 0.2, and the value is 'inflation_so'

- To add a new slider, you will first need to figure out what the inputID should be. 
    - First, you will need to add the inputID to the server file. Define a variable in the function 'sim.data1' in server.R. For the example above this would be "inflation_var2 = as.numeric(input$param3)." 
    - 'simdata1' is contained in the 'reactive.data1.' 
- Then, if you don't want the slider to appear for certain inputs of 'policy estimate', insert it in 'else if(input$policy_est == "What policy estimate
you don't want the slider to appear in"). 
- It goes in the 'list_hide' function within the else if. Once that has been added, that variable, in this case 'inflation_var2', should be added to 'simdata1' in all_analysis.R. This deals directly with the original analysis.

- If the slider you want to add is already in all_analysis.R, make sure to use the name defined in all_analysis, then add it to server.R, and then ui.R.

- To change the value, which is 'inflation.so' in the above example, modify it in the "Data" section of all_analysis.R.

## The chart below shows the way that the different components of the OPA interact with each other

**I have a chart, need permission to add photos I believe**



        
