
library(shiny)

shinyServer( function(input, output, session) {
  #Dynamic UI
  
  
  #Show/hide SDs code.
  onclick("toggleDataSDs",
          lapply(
            c("SD1", "SD2", "SD3", "SD4", "SD5", "SD6", "SD7",
              "SD8", "SD9", "SD10", "SD11", "SD12", "SD13", "SD14",
              "SD15", "SD16", "SD17", "SD20", "SD21", "SD39"), 
            toggle, anim=TRUE)
          )
  
  onclick("toggleResearchSDs",
          lapply(c("SD22", "SD23", "SD24", "SD25", "SD27", "SD28",
                   "SD29", "SD32", "SD36", "SD18", "SD35"), toggle, anim=TRUE))
  
  onclick("toggleGWSDs",
          lapply(c("SD33","SD34", "SD36", "SD37", "SD38", "SD19"), toggle, anim=TRUE))
  
  
# Generate reactive simulated data for plotting 

  reactive.data1<- reactive( {
    sim.data1(
      nsims = as.numeric(input$param1),                                                    
      gov_bonds_var2     = as.numeric(input$param2),                                           
      gov_bonds_var2_sd  = as.numeric(input$param2_1),                                      
      inflation_var2     = as.numeric(input$param3),                                           
      inflation_var2_sd  = as.numeric(input$param3_1),    
      gov_bonds_new_var2    = as.numeric(input$param2_new),       
      gov_bonds_new_var2_sd = as.numeric(input$param2_1_new),                                                            
      inflation_new_var2    = as.numeric(input$param3_new),                                
      inflation_new_var2_sd = as.numeric(input$param3_1_new), 
      wage_ag_var2 = as.numeric(input$param4),                                             
      wage_ag_var2_sd = as.numeric(input$param4_1),                                        
      wage_ww_var2 = as.numeric(input$param5),                                             
      wage_ww_var2_sd = as.numeric(input$param5_1),                                        
      profits_se_var2 = as.numeric(input$param6),                                          
      profits_se_var2_sd = as.numeric(input$param6_1),                                     
      hours_se_cond_var2 = as.numeric(input$param7),                                       
      hours_se_cond_var2_sd = as.numeric(input$param7_1),                                  
      hours_ag_var2 = as.numeric(input$param8),                                            
      hours_ag_var2_sd = as.numeric(input$param8_1),                                       
      hours_ww_var2 = as.numeric(input$param9),                                            
      hours_ww_var2_sd = as.numeric(input$param9_1),                                       
      hours_se_var2 = as.numeric(input$param10),                                           
      hours_se_var2_sd = as.numeric(input$param10_1),                                      
      ex_rate_var2 = as.numeric(input$param11),                                            
      ex_rate_var2_sd = as.numeric(input$param11_1),                                       
      growth_rate_var2 = as.numeric(input$param12),                                        
      growth_rate_var2_sd = as.numeric(input$param12_1),  
      coverage_var2 = as.numeric(input$param13), 
      coverage_var2_sd = as.numeric(input$param13_1),
      tax_var2 = as.numeric(input$param15),                                                 
      tax_var2_sd = as.numeric(input$param15_1),                                            
      unit_cost_local_var2 = as.numeric(input$param16),                                           
      unit_cost_local_var2_sd = as.numeric(input$param16_1), 
      unit_cost_local_new_var2 = as.numeric(input$param16_new),                                           
      unit_cost_local_new_var2_sd = as.numeric(input$param16_1_new), 
      years_of_treat_0_var2   = as.numeric(input$param17)        ,    
      years_of_treat_0_var2_sd= as.numeric(input$param17_1)     ,
      years_of_treat_t_var2   = as.numeric(input$param17_new)   ,    
      years_of_treat_t_var2_sd= as.numeric(input$param17_1_new)     ,
      lambda1_var2 = c(as.numeric(input$param18_1), as.numeric(input$param18_2)),                                          
      lambda1_var2_sd = c(as.numeric(input$param18_1_1), as.numeric(input$param18_2_1)),                                     
      lambda2_var2 = as.numeric(input$param19),                                             
      lambda2_var2_sd = as.numeric(input$param19_1),                                        
      q_full_var2 = as.numeric(input$param20),                                              
      q_full_var2_sd = as.numeric(input$param20_1),                                           
      coef_exp_var2 = c(as.numeric(input$param21_1), as.numeric(input$param21_2)),                      
      teach_sal_var2    = as.numeric(input$param22),                                             
      teach_sal_var2_sd = as.numeric(input$param22_1),                                        
      teach_ben_var2    = as.numeric(input$param23),                                             
      teach_ben_var2_sd = as.numeric(input$param23_1),  
      teach_sal_new_var2     = (50000 * 12 / 49.773),                   # change to match DD     
      teach_sal_new_var2_sd  = (50000 * 12 / 49.773) * 0.1,             # change to match DD
      teach_ben_new_var2     = 0,                                       # change to 0 to match DD
      teach_ben_new_var2_sd  = 0.000001  ,                              # change to 0.000001 to match DD      
      n_students_var2 = as.numeric(input$param24),                                            
      n_students_var2_sd = as.numeric(input$param24_1),                                       
      delta_ed_var2 = as.numeric(input$param26),                                              
      delta_ed_var2_sd = as.numeric(input$param26_1),                                             
      delta_ed_ext_var2 = as.numeric(input$param27),                                              
      delta_ed_ext_var2_sd = as.numeric(input$param27_1),                                               
      q_zero_var2 = as.numeric(input$param28),                                                
      q_zero_var2_sd = 0.001,                                            # change to 0.001 to match DD
      lambda1_new_var2 = as.numeric(input$param29_1),                   
      lambda1_new_var2_sd = as.numeric(input$param29_1_1),             
      prevalence_0_var2 = as.numeric(input$param30),    
      prevalence_0_var2_sd = as.numeric(input$param30_1), 
#     prevalence_r_var2 = as.numeric(input$param31),
      prevalence_r_var2 = 1,
      prevalence_r_var2_sd = as.numeric(input$param31_1),                                                                         
      counts_par_var2 = as.numeric(input$param32), 
      counts_par_var2_sd = as.numeric(input$param32_1),
      staff_time_var2 = as.numeric(input$param33), 
      staff_time_var2_sd = as.numeric(input$param33_1), 
      costs_par_var2 = as.numeric(input$param34), 
      costs_par_var2_sd = as.numeric(input$param34_1), 
      new_costs_var2 = as.numeric(input$param35),
      new_costs_var2_sd = as.numeric(input$param35_1),
      new_prev_r_var2 = as.numeric(input$param31),
      new_prev_r_var2_sd = as.numeric(input$param31_1),
      countries_var2 = list("india", "kenya", "nigeria", "vietnam")  # = input$param36  to make it interactive
      
      
    )
  } 
  )
  # Export Input Parameter Values
  output$downloadParams <- downloadHandler(
    filename = "export.txt",
    content = function(file) {
      inputList <- names(reactiveValuesToList(input))
      inputLabel <- inputMaster[inputList]
      exportVars <- paste0(inputLabel, " = ", sapply(inputList, function(inpt) input[[inpt]]))
      write(exportVars, file)
    })
  
  # Export All Assumption Plot
  output$downloadPlotAll <- downloadHandler(filename = function() {
    "plotAll.png"
  },
  content = function(file) {
    file.copy("./code/shiny_app/www/plotAll.png", file, overwrite=TRUE)
  })
  
  # Export Key Assumption Plot
  output$downloadPlotKA <- downloadHandler(filename = function() {
    "plotKA.png"
  },
  content = function(file) {
    file.copy("./code/shiny_app/www/plotKA.png", file, overwrite=TRUE)
  })
  
  # Sync cost variable for Key Assumptions and All Assumptions 
  
  observeEvent(
    input$param35_ka,
    updateSliderInput(session, "param35", value = input$param35_ka)
  )
  
  observeEvent(
    input$param35,
    updateSliderInput(session, "param35_ka", value = input$param35)
  )
  
  
    
  # Sync prevalence variable for Key Assumptions and All Assumptions 
  
  observeEvent(
    input$param31_ka,
    updateSliderInput(session, "param31", value = input$param31_ka)
  )

  observeEvent(
    input$param31,
    updateSliderInput(session, "param31_ka", value = input$param31)
  )

  # Sync length of treatment in new environment for Key Assumptions and All Assumptions 
  
  observeEvent(
    input$param17_new_ka,
    updateNumericInput(session, "param17_new", value = input$param17_new_ka)
    
  )
  
  observeEvent(
    input$param17_new,
    updateSliderInput(session, "param17_new_ka", value = input$param17_new)
  )
  
  # Reset all inputs from Key Assumption tab
  observeEvent(input$resetKA, {reset("KA")})
  
  # Reset all inputs from All Assumption tab
  observeEvent(input$resetAll, {reset("All")})
 
  # Show/hide components of each model 
  observeEvent(input$policy_est,{ 
    # all params
    list_master <- c(
      "param2",                                             #Data
      "param2_1",
      "param2_new",
      "param2_1_new",
      "param3",
      "param3_1",
      "param3_new",
      "param3_1_new",
      "param4",
      "param4_1",
      "param5",
      "param5_1",
      "param6",
      "param6_1",
      "param7",
      "param7_1",
      "param8",
      "param8_1",
      "param9",
      "param9_1",
      "param10",
      "param10_1",
      "param11",
      "param11_1",
      "param12",
      "param12_1",
      "param13",
      "param13_1",
      "param15",
      "param15_1",
      "param16",
      "param16_1",
      "param16_new",
      "param16_1_new",
      "param17",
      "param17_1",
      "param17_new",
      "param17_1_new",
      "param18_1",                                          #Research
      "param18_1_1",
      "param18_2",
      "param18_2_1",
      "param20",
      "param20_1",
      "param28",
      "param28_1",
      "param26",
      "param26_1",
      "param28", 
      "param28_1", 
      "param30",
      "param30_1",
      "param21_1",                                          #Guesswork
      "param21_2",
      "param22",
      "param22_1",
      "param23",
      "param23_1",
      "param24",
      "param24_1",
      "param31",
      "param31_1",
      "param32",
      "param32_1",
      "param34",
      "param34_1",
      "param19",
      "param19_1",
      "param27",
      "param27_1",
      "param29_1",
      "param29_1_1",
      "param33",
      "param33_1"
    )
    if (input$policy_est == "A1. Tax revenue") {
      # remove: counts adj, costs adj, lambda 2, delda ed w/ext, new lambdas, 
      # costs due to staff, 
      # new gov bonds, new inflation, new cost of teaching, 
      list_hide <- c("param32",
                     "param32_1",
                     "param34",
                     "param34_1", 
                     "param19",
                     "param19_1",
                     "param27",
                     "param27_1",
                     "param29_1",
                     "param29_1_1",
                     "param33",
                     "param33_1", 
                     "param2_new",
                     "param2_1_new",
                     "param3_new",
                     "param3_1_new",
                     "param16_new",
                     "param6_1_new", 
                     "param30", 
                     "param30_1", 
                     "param28",
                     "param28_1")
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
    } else if (input$policy_est == "A1. With externalities. Tax") {
      list_hide <- c("param32",
                     "param32_1",
                     "param34",
                     "param34_1", 
                     "param29_1",
                     "param29_1_1",
                     "param33",
                     "param33_1", 
                     "param2_new",
                     "param2_1_new",
                     "param3_new",
                     "param3_1_new",
                     "param16_new",
                     "param6_1_new", 
                     "param30", 
                     "param30_1", 
                     "param28",
                     "param28_1")
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
    } else if (input$policy_est == "A1. All income") {
      list_hide <- c("param32",
                     "param32_1",
                     "param34",
                     "param34_1", 
                     "param19",
                     "param19_1",
                     "param27",
                     "param27_1",
                     "param29_1",
                     "param29_1_1",
                     "param33",
                     "param33_1", 
                     "param15", 
                     "param15_1", 
                     "param2_new",
                     "param2_1_new",
                     "param3_new",
                     "param3_1_new",
                     "param16_new",
                     "param6_1_new", 
                     "param30", 
                     "param30_1", 
                     "param28",
                     "param28_1")
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
    } else if (input$policy_est == "A1. With ext. All income") {
      list_hide <- c("param32",
                     "param32_1",
                     "param34",
                     "param34_1", 
                     "param29_1",
                     "param29_1_1",
                     "param33",
                     "param33_1", 
                     "param2_new",
                     "param2_1_new",
                     "param3_new",
                     "param3_1_new",
                     "param16_new",
                     "param6_1_new", 
                     "param30", 
                     "param30_1", 
                     "param28",
                     "param28_1")
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
      
    } else if (input$policy_est == "A2. Tax") {
      list_hide <- c("param32",
                     "param32_1",
                     "param34",
                     "param34_1", 
                     "param19",
                     "param19_1",
                     "param27",
                     "param27_1",     
                     "param4",
                     "param4_1",
                     "param5",
                     "param5_1",
                     "param6",
                     "param6_1",
                     "param7",
                     "param7_1",
                     "param8",
                     "param8_1",
                     "param9",
                     "param9_1",
                     "param10",
                     "param10_1",
                     "param12",
                     "param12_1",
                     "param13",
                     "param13_1", 
                     "param18_1",                                         
                     "param18_1_1",
                     "param18_2",
                     "param18_2_1",
                     "param33",
                     "param33_1" , 
                     "param2",
                     "param2_1",
                     "param3",
                     "param3_1",
                     "param16",
                     "param6_1", 
                     "param30", 
                     "param30_1",
                     "param28",
                     "param28_1"
      )
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
    } else if (input$policy_est == "A2. All income") {
      list_hide <- c("param32",
                     "param32_1",
                     "param34",
                     "param34_1", 
                     "param19",
                     "param19_1",
                     "param27",
                     "param27_1",     
                     "param4",
                     "param4_1",
                     "param5",
                     "param5_1",
                     "param6",
                     "param6_1",
                     "param7",
                     "param7_1",
                     "param8",
                     "param8_1",
                     "param9",
                     "param9_1",
                     "param10",
                     "param10_1",
                     "param12",
                     "param12_1",
                     "param13",
                     "param13_1", 
                     "param18_1",                                         
                     "param18_1_1",
                     "param18_2",
                     "param18_2_1",
                     "param33",
                     "param33_1", 
                     "param15", 
                     "param15_1", 
                     "param2",
                     "param2_1",
                     "param3",
                     "param3_1",
                     "param16",
                     "param16_1", 
                     "param30", 
                     "param30_1", 
                     "param28",
                     "param28_1")
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
    } else if (input$policy_est == "A3. All income of A1") {
      list_hide <- c("param19",
                     "param19_1",
                     "param22",
                     "param22_1",
                     "param23",
                     "param23_1",
                     "param24",
                     "param24_1",
                     "param26",
                     "param26_1",
                     "param27",
                     "param27_1",
                     "param29_1",
                     "param29_1_1",
                     "param16", 
                     "param16_1", 
                     "param2_new",
                     "param2_1_new",
                     "param3_new",
                     "param3_1_new", 
                     "param28",
                     "param28_1",
                     "param34",
                     "param34_1")
      list_show <- list_master[ - which(list_master %in% list_hide)]
    } else if (input$policy_est == "A3. All income of A1, with ext.") {
      list_hide <- c("param22",
                     "param22_1",
                     "param23",
                     "param23_1",
                     "param24",
                     "param24_1",
                     "param26",
                     "param26_1",
                     "param27",
                     "param27_1",
                     "param29_1",
                     "param29_1_1",
                     "param16", 
                     "param16_1", 
                     "param2_new",
                     "param2_1_new",
                     "param3_new",
                     "param3_1_new", 
                     "param28",
                     "param28_1", 
                     "param34",
                     "param34_1",
                     "param16",
                     "param16_1")
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
    } else if (input$policy_est == "A3. All income of A2. Main Policy Estimate") {
      list_hide <- c("param19",
                     "param19_1",
                     "param21_1",
                     "param21_2",
                     "param22",
                     "param22_1",
                     "param23",
                     "param23_1",
                     "param24",
                     "param24_1",
                     "param26",
                     "param26_1",
                     "param27",
                     "param27_1",
                     "param16", 
                     "param16_1",
                     "param16_new",
                     "param16_new_sd",
                     "param4",
                     "param4_1",
                     "param5",
                     "param5_1",
                     "param6",
                     "param6_1",
                     "param7",
                     "param7_1",
                     "param8",
                     "param8_1",
                     "param9",
                     "param9_1",
                     "param10",
                     "param10_1",
                     "param12",
                     "param12_1",
                     "param13",
                     "param13_1", 
                     "param18_1",                                         
                     "param18_1_1",
                     "param18_2",
                     "param18_2_1",
                     "param15", 
                     "param15_1", 
                     "param2",
                     "param2_1",
                     "param3",
                     "param3_1", 
                     "param28",
                     "param28_1", 
                     "param34",
                     "param34_1", 
                     "param32", 
                     "param32_1", 
                     "param11", 
                     "param11_1", 
                     "param16_1_new")
      
      list_show <- list_master[ - which(list_master %in% list_hide)]
      
    } 
    sapply(list_hide, 
           function(x) hideElement(id = x) ) 
    sapply(list_show, 
           function(x) showElement(id = x) ) 
  })
  
  
  hideElement("show_eq")
  #observeEvent(input$run, {
  ################
  ###### Results/Viz
  ################
  output$eqns <- renderUI({
    #if (input$run == TRUE) {showElement("show_eq")}
    showElement("show_eq")
    if (input$show_eq == TRUE) {
      if (input$policy_est == "A1. Tax revenue" ) { 
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\tau \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2) 
                      \\right]
                            }_{\\text{net labor market gains}} - 
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      
                      }_{\\text{cost of deworming medication}}
            
            \\tag{1}
            \\end{equation}
            $$ \n See', a("Approach 1", href='https://rpubs.com/fhoces/547979', target = "_blank"), 'in the documentation component for more details'  ) 
        )
        
      } else if (input$policy_est ==  "A1. With externalities. Tax"){
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\tau \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}, \\lambda_{2}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2) 
                      \\right]
                            }_{\\text{net labor market gains}} - 
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      
                      }_{\\text{cost of deworming medication}}
            
            \\tag{2}
            \\end{equation}
            $$ \n See' , a("Approach 1", href="05_final_opa.html#21_Approach_1:_Baird_et_al_(2016)", target = "_blank"),  'in the documentation component for more details'  ) 
        )
      } else if (input$policy_est == "A1. All income"){
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2) 
                      \\right]
                            }_{\\text{net labor market gains}} - 
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      
                      }_{\\text{cost of deworming medication}}
            
            \\tag{3}
            \\end{equation}
            $$ \n See' , a("Approach 1", href="https://rpubs.com/fhoces/547979", target = "_blank"),  'in the documentation component for more details'  
          )
        )
      } else if (input$policy_est ==  "A1. With ext. All income"){
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}, \\lambda_{2}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2) 
                      \\right]
                            }_{\\text{net labor market gains}} - 
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      
                      }_{\\text{cost of deworming medication}}
            
            \\tag{4}
            \\end{equation}
            $$ \n See', a("Approach 1", href="https://rpubs.com/fhoces/547979", target = "_blank"),  'in the documentation component for more details'  )
        )
      } else if (input$policy_est == "A2. Tax"){
        withMathJax(helpText('$$
            \\begin{equation}
              NPV =  \\underbrace{
                 \\left[ \\tau \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\alpha^{pooled}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2) 
                      \\right]
                            }_{\\text{net labor market gains}} - 
                      \\underbrace{
                      \\left[\\sum_{t=0}^{1.4} \\left( \\frac{1}{1 + r}\\right)^{t} \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      \\right]
                      }_{\\text{cost of deworming medication}}
            
            \\tag{5}
            \\end{equation}
                                 $$ \n See' , a("Approach 2", href="https://rpubs.com/fhoces/547979", target = "_blank"),  'in the documentation component for more details'))
      } else if (input$policy_est == "A2. All income"){
        withMathJax(helpText('$$
            \\begin{equation}
              NPV =  \\underbrace{
                 \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\alpha^{pooled}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2) 
                      \\right]
                            }_{\\text{net labor market gains}} - 
                      \\underbrace{
                      \\left[\\sum_{t=0}^{1.4} \\left( \\frac{1}{1 + r}\\right)^{t} \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      \\right]
                      }_{\\text{cost of deworming medication}}
            
            \\tag{6}
            \\end{equation}
                                 $$ \n See' , a("Approach 2", href="https://rpubs.com/fhoces/547979", target = "_blank"),  ' in the documentation component for more details' ))
      } else if (input$policy_est == "A3. All income of A1"){
        withMathJax(helpText('$$   
            \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}) 
                     \\right]
                            }_{\\text{labor market gains}} - 
                      \\underbrace{
                       \\left[\\sum_{t=0}^{t_{treat}} \\left( \\frac{1}{1 + r}\\right)^{t}  Q(S_{2})\\sum_{i \\in Countries } \\omega_{i} c_{i}(\\delta_{g})\\
                      \\right]
                      }_{\\text{cost of deworming medication}}
            
            \\tag{7}
            \\end{equation}
             $$ \n See' , a("Approach 3", href="https://rpubs.com/fhoces/547979", target = "_blank"),  ' in the documentation component for more details'))
      } else if (input$policy_est == "A3. All income of A1, with ext."){
        withMathJax(helpText('$$
            \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}, \\lambda_{2}) 
                     \\right]
                            }_{\\text{labor market gains}} - 
                      \\underbrace{
                      Q(S_{2})\\sum_{i \\in Countries } \\omega_{i} c_{i}(\\delta_{g})\\
                      }_{\\text{cost of deworming medication}}
            
            \\tag{8}
            \\end{equation}
            $$ \n See' , a("Approach 3", href="https://rpubs.com/fhoces/547979", target = "_blank"),  ' approach 3 in the documentation component for more details'))
      } else if (input$policy_est == "A3. All income of A2. Main Policy Estimate"){
        withMathJax(helpText(
          '$$
            \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\alpha^{pooled}, \\eta_{new}, L_{new}) 
                     \\right]
                            }_{\\text{Benefits (B)}} - 
                      \\underbrace{
                       Q(S_{2})\\sum_{i \\in Countries } \\omega_{i} c_{i}(\\delta_{g})\\
                      }_{\\text{Costs (C)}}
            
            \\tag{9}
            \\end{equation}
              $$ \n See' , a("Approach 3", href="https://rpubs.com/fhoces/547979", target = "_blank"),  ' in the documentation component for more details'
        ))
      }
    } 
  })

  
  # Generate Plot with All Asumptions
  plotInputAll <- function(){
    npv_sim_all <- reactive.data1()
    plot1 <- generate_plot_f(npv_sim_all, input$policy_est, input$rescale)[[1]]
    
    position <- generate_plot_f(npv_sim_all, input$policy_est, input$rescale)[[2]]
    total_time <- generate_plot_f(npv_sim_all, input$policy_est, input$rescale)[[3]]
    plot1 <- plot1 + labs(y = NULL,
                          x = "Net Present Value (Benefits -  Costs)" ,
                          title = "Net Lifetime Income Effects of Deworming for Each Treated Children",
                          subtitle = paste0(policy_estimates_text[position], ". ",
                                            "N = ", input$param1, " simulations. Takes ",
                                            round(total_time, 1)," ",attributes(total_time)$units )  ) 
    
    
    
  }
  output$plot1 <- renderPlot({  
    ggsave("plotAll.png", plotInputAll(), path= "./code/shiny_app/www")
    print(plotInputAll())
  }, height = 600#, width = 750 
  )
  
  # Generate Plot with Key Assumptions
  plotInputKA <- function(){
    npv_sim_all <- reactive.data1()
    output_plot <- generate_plot_f(npv_sim_all, "A3. All income of A2. Main Policy Estimate", input$rescale)
    plot1 <- output_plot[[1]]
    
    position <- output_plot[[2]]
    
    plot1 <- plot1 + labs(y = NULL,
                          x = "Net Present Value (Benefits -  Costs)" ,
                          title = "Net Lifetime Income Effects of Deworming for Each Treated Children",
                          subtitle = paste0(policy_estimates_text[position], ". ")
    )
  }
  output$plot1_ka <- renderPlot({      
    ggsave("plotKA.png", plotInputKA(), path= "./code/shiny_app/www")
    print(plotInputKA())  
  }, height = 600 #, width = 750 
  )
  
  # Generate Main Policy Estimate Plot 
  output$plot1_main <- renderPlot({    
    npv_sim_all <- reactive.data1()
    output_plot <- generate_plot_f(npv_sim_all, "A3. All income of A2. Main Policy Estimate", input$rescale)
    plot1 <- output_plot[[1]]
    
    position <- output_plot[[2]]
   
    plot1 <- plot1 + labs(y = NULL,
           x = "Net Present Value (Benefits -  Costs)" ,
           title = "Net Lifetime Income Effects of Deworming for Each Treated Children",
           subtitle = "Distribution of the Net Present Value of Deworming Interventions"
           ) 
    print(plot1)  
  }, height = 600#, width = 750 
  )
  
  # Master List of Input ID & Label
  
  inputMaster <- c(# ---- Key Assumptions
                   "param35_ka" = "Yearly unit costs in new country (in $US)",
                   "param31_ka" = "Prevalence in new region",
                   "param17_new_ka" = "Length of treatment (years)",
                   "resetKA" = "Reset Button for Key Assumptions",
                   # ---- All Assumptions
                   "rescale" = "Rescale Checkbox Status",
                   "policy_est" = "Policy Estimate Assumption",
                   "param1" = "Number of simulations",
                   # ---- research tab
                   
                   "param18_1" = "Increase in number of hours worked due to treatment (Male)",
                   "param18_1_1" = "SD of Increase in number of hours worked due to treatment (Male)",
                   "param18_2" = "Increase in number of hours worked due to treatment (Female)",
                   "param18_2_1" = "SD of Increase in number of hours worked due to treatment (Female)",
                   "param29_1" = "Increase in yearly earnings (pooling 10, 15, 20 year follow-ups)",
                   "param29_1_1" = "SD of Increase in yearly earnings (pooling 10, 15, 20 year follow-ups)",
                   "param19" = "Increase in number of hours worked due to treatment (Externalities included)",
                   "param19_1" = "SD of Increase in number of hours worked due to treatment (Externalities included)",
                   "param30" = "Prevalence in original study",
                   "param30_1" = "SD of Prevalence in original study",
                   "param4" = "Agricultural Wages",
                   "param4_1" = "SD of Agricultural Wages",
                   "param5" = "Wages of a Wage Worker",
                   "param5_1" = "SD of Wages of a Wage Worker",
                   "param6" = "Average monthly self-employed profits (Profits SE)",
                   "param6_1" = "SD of Profits SE",
                   "param7" = "Weekly hours worked by self-employed workers(>10)",
                   "param7_1" = "SD of Weekly hours worked by self-employed workers(>10)",
                   "param8" = "Weekly hours worked by agricultural workers",
                   "param8_1" = "SD of Weekly hours worked by agricultural workers",
                   "param9" = "Weekly hours worked by wage earners",
                   "param9_1" = "SD of Weekly hours worked by wage earners", 
                   "param10" = "Weekly hours worked by self-employed workers (no condition)",
                   "param10_1" = "SD of Weekly hours worked by self-employed workers (no condition)",
                   "param21_1" = "Coefficients of Teacher Experience, Xp (beta_1)",
                   "param21_2" = "Coefficients of Teacher Experience Squared, Xp ^2 (beta_2)",
                   "param13" = "Coverage(R)",
                   "param13_1" = "SD of Coverage(R)",
                   "param20" = "Take up (Q(S_2))",
                   "param20_1" = "SD of Take up (Q(S_2))",
                   "param28" = "Take-up with no subsidy ( Q(S_1) )",
                   "param26" = "x * Delta{E}",
                   "param26_1" = "SD of x * Delta{E}",
                   "param27" = "x * Delta{E}  (ext)",
                   "param27_1" = "SD of x * Delta{E}  (ext)",
                   "param22" = "Teacher salary",
                   "param22_1" = "SD of Teacher salary",
                   "param23" = "Teacher Benefits",
                   "param23_1" = "SD of Teacher Benefits",
                   "param24" = "Students per teacher",
                   "param24_1" = "SD of Students per teacher",
                   "param17" = "Years of treatment in orginal study",
                   "param17_1" = "SD of Years of treatment in orginal study",
                   "param16" = "Costs of Treatment (local $)",
                   "param16_1" = "SD of Costs of Treatment (local $)",
                   "param16_new" = "Costs of Treatment (US $)", 
                   "param16_1_new" = "SD of Costs of Treatment (US $)",
                   "param34" = "Costs adjustment",
                   "param34_1" = "SD of Costs adjustment",
                   "param32" = "Counts adjustment",
                   "param32_1" = "SD of Counts adjustment",
                   #---- data tab
                   "param35" = "Yearly unit costs in new country (in $US)",
                   "param35_1" = "SD of Yearly unit costs in new country (in $US)",
                   "param11" = "Exchange rate",
                   "param11_1" = "SD of Exchange rate",
                   "param12" = "GDP Growth Rate",
                   "param12_1" = "SD of GDP Growth Rate",
                   "param2" = "Government Bonds",
                   "param2_1" = "SD of Government Bonds",
                   "param2_new" = "Government Bonds",
                   "param2_1_new" = "SD of Government Bonds",
                   "param3" = "Inflation Rate",
                   "param3_1" = "SD of Inflation Rate",
                   "param3_new" = "Inflation Rate",
                   "param3_1_new" = "SD of Inflation Rate",
                   "param15" = "Tax rate",
                   "param15_1" = "SD of Tax rate",
                   # ---- GW tab
                   "param31" = "Prevalence in new region",
                   "param31_1" = "SD of Prevalence in new region",
                   "param17_new" = "Years of Treatment in New Setting",
                   "param17_1_new" = "SD of Years of Treatment in New Setting",
                   "param33" = "Additional costs due to staff time",
                   "param33_1" = "SD of Additional costs due to staff time",
                   # ---- Buttons for All Assumption tab
                   "resetAll" = "Reset Button for All Assumptions",
                   "show_eq" = "Show Equation Checkbox Status"
                   
                   
                   
                   )
                  
  
})
